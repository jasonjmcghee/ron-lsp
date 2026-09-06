use crate::rust_analyzer::{RustAnalyzer, TypeInfo, TypeKind};
use crate::tree_sitter_parser;
use std::sync::Arc;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
    Position,
};

#[derive(Debug, PartialEq)]
enum CompletionContext {
    FieldName,  // Completing field names (e.g., after comma or opening paren)
    FieldValue, // Completing values after colon
    StructType, // Completing struct type name for nested types
}

/// Determine what we're completing based on cursor position using tree-sitter
fn get_completion_context(content: &str, position: Position) -> CompletionContext {
    use crate::ts_utils::{self, RonParser};

    let mut parser = RonParser::new();
    let tree = match parser.parse(content) {
        Some(t) => t,
        None => return CompletionContext::FieldName,
    };

    let node = match ts_utils::node_at_position(&tree, content, position) {
        Some(n) => n,
        None => return CompletionContext::FieldName,
    };

    // Check if we're inside a field node
    if let Some(field_node) = ts_utils::find_ancestor_by_kind(node, "field") {
        // Check if we're after the colon (in the value position)
        let field_name_node = field_node.child(0);
        if let (Some(field_name), Some(value_node)) =
            (field_name_node, ts_utils::field_value(&field_node))
        {
            // If cursor is after the field name, we're completing a value
            let name_end = field_name.end_position();
            if position.line > name_end.row as u32
                || (position.line == name_end.row as u32
                    && position.character > name_end.column as u32)
            {
                // Check if there's already some text (might be completing a type)
                if let Some(val_text) = ts_utils::node_text(&value_node, content) {
                    if val_text
                        .chars()
                        .all(|c| c.is_alphanumeric() || c == '_' || c == ':')
                    {
                        return CompletionContext::StructType;
                    }
                }

                return CompletionContext::FieldValue;
            }
        }
    }

    // Default to field name completion
    CompletionContext::FieldName
}

/// Find the type we're currently inside (e.g., if typing "User(" we return "User")
#[allow(dead_code)]
fn find_current_type_context(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();

    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;
    let before_cursor = &line[..col.min(line.len())];

    // Look for pattern like "TypeName(" right before cursor
    // This handles cases like: author: User(
    if let Some(paren_pos) = before_cursor.rfind('(') {
        let before_paren = before_cursor[..paren_pos].trim();

        // Extract the type name (last word before the paren)
        if let Some(word_start) = before_paren.rfind(|c: char| !c.is_alphanumeric() && c != '_') {
            let type_name = &before_paren[word_start + 1..];
            if !type_name.is_empty() && type_name.chars().next().unwrap().is_uppercase() {
                return Some(type_name.to_string());
            }
        } else if !before_paren.is_empty() && before_paren.chars().next().unwrap().is_uppercase() {
            return Some(before_paren.to_string());
        }
    }

    None
}

/// Generate completions for a given type (already navigated to the innermost type)
/// Type context navigation is now done in main.rs using Backend::navigate_to_innermost_type
pub async fn generate_completions_for_type(
    content: &str,
    position: Position,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<CompletionItem> {
    let effective_type = type_info;

    let context = get_completion_context(content, position);

    match context {
        CompletionContext::FieldName => {
            generate_field_completions(content, position, effective_type)
        }
        CompletionContext::FieldValue => {
            // Find the field we're completing the value for
            if let Some(field_name) = find_current_field(content, position) {
                let mut completions = generate_value_completions_for_field(
                    field_name,
                    effective_type,
                    analyzer.clone(),
                )
                .await;

                // Also add all workspace symbols as potential completions
                completions.extend(get_all_workspace_types(analyzer).await);

                completions
            } else {
                get_all_workspace_types(analyzer).await
            }
        }
        CompletionContext::StructType => {
            // Find the field type and provide struct completions
            if let Some(field_name) = find_current_field(content, position) {
                generate_type_completions_for_field(field_name, effective_type, analyzer).await
            } else {
                Vec::new()
            }
        }
    }
}

/// Get all types from the workspace as completion items
async fn get_all_workspace_types(analyzer: Arc<RustAnalyzer>) -> Vec<CompletionItem> {
    analyzer
        .get_all_types()
        .await
        .into_iter()
        .map(|type_info| create_type_completion(&type_info))
        .collect()
}

fn generate_field_completions(
    content: &str,
    position: Position,
    type_info: &TypeInfo,
) -> Vec<CompletionItem> {
    match &type_info.kind {
        TypeKind::Struct(fields) => {
            // Get fields already used in the RON file
            let used_fields = tree_sitter_parser::extract_fields_from_ron(content);

            // Generate completions for unused fields
            fields
                .iter()
                .filter(|field| !used_fields.contains(&field.name))
                .map(|field| {
                    let documentation = if let Some(docs) = &field.docs {
                        Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                "```rust\n{}: {}\n```\n\n{}",
                                field.name, field.type_name, docs
                            ),
                        }))
                    } else {
                        Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```rust\n{}: {}\n```", field.name, field.type_name),
                        }))
                    };

                    CompletionItem {
                        label: field.name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(field.type_name.clone()),
                        documentation,
                        insert_text: Some(format!("{}: ", field.name)),
                        ..Default::default()
                    }
                })
                .collect()
        }
        TypeKind::Enum(variants) => {
            // Check if we're inside a specific variant's fields
            if let Some(variant_name) =
                tree_sitter_parser::find_current_variant_context(content, position)
            {
                if let Some(variant) = variants.iter().find(|v| v.name == variant_name) {
                    // Complete the variant's fields
                    let used_fields = tree_sitter_parser::extract_fields_from_ron(content);
                    return variant
                        .fields
                        .iter()
                        .filter(|field| !used_fields.contains(&field.name))
                        .map(|field| {
                            let documentation = if let Some(docs) = &field.docs {
                                Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: format!(
                                        "```rust\n{}: {}\n```\n\n{}",
                                        field.name, field.type_name, docs
                                    ),
                                }))
                            } else {
                                Some(Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: format!(
                                        "```rust\n{}: {}\n```",
                                        field.name, field.type_name
                                    ),
                                }))
                            };

                            CompletionItem {
                                label: field.name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(field.type_name.clone()),
                                documentation,
                                insert_text: Some(format!("{}: ", field.name)),
                                ..Default::default()
                            }
                        })
                        .collect();
                }
            }

            // Otherwise, complete variant names
            // Generate completions for enum variants
            variants
                .iter()
                .map(|variant| {
                    let documentation = if let Some(docs) = &variant.docs {
                        Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```rust\n{}\n```\n\n{}", variant.name, docs),
                        }))
                    } else {
                        Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("```rust\n{}\n```", variant.name),
                        }))
                    };

                    let insert_text = if variant.fields.is_empty() {
                        variant.name.clone()
                    } else if variant
                        .fields
                        .iter()
                        .all(|f| f.name.chars().all(|c| c.is_numeric()))
                    {
                        // Tuple variant
                        format!("{}($0)", variant.name)
                    } else {
                        // Struct variant
                        format!("{}($0)", variant.name)
                    };

                    CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("Variant of {}", type_info.name)),
                        documentation,
                        insert_text: Some(insert_text),
                        ..Default::default()
                    }
                })
                .collect()
        }
    }
}

/// Find the field name for the current cursor position using tree-sitter
fn find_current_field(content: &str, position: Position) -> Option<String> {
    tree_sitter_parser::get_field_at_position(content, position)
}

/// Generate value completions for a specific field
async fn generate_value_completions_for_field(
    field_name: String,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<CompletionItem> {
    // Find the field in the type info
    if let TypeKind::Struct(fields) = &type_info.kind {
        if let Some(field) = fields.iter().find(|f| f.name == field_name) {
            return generate_value_completions_by_type(&field.type_name, analyzer).await;
        }
    }

    Vec::new()
}

/// Generate type completions for a field that expects a custom type
async fn generate_type_completions_for_field(
    field_name: String,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<CompletionItem> {
    // Find the field in the type info
    if let TypeKind::Struct(fields) = &type_info.kind {
        if let Some(field) = fields.iter().find(|f| f.name == field_name) {
            // Get the inner type if it's a generic
            let inner_type = extract_inner_type(&field.type_name);

            // Try to get type info for this type
            if let Some(nested_type) = analyzer.get_type_info(&inner_type).await {
                return vec![create_type_completion(&nested_type)];
            }
        }
    }

    Vec::new()
}

/// Create a completion item for a type (struct or enum)
fn create_type_completion(type_info: &TypeInfo) -> CompletionItem {
    let type_name = type_info.name.split("::").last().unwrap_or(&type_info.name);

    match &type_info.kind {
        TypeKind::Struct(fields) => {
            // Generate a snippet for the struct with all fields
            let field_snippets: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(i, f)| format!("    {}: ${{{}}}", f.name, i + 1))
                .collect();

            let snippet = if field_snippets.is_empty() {
                format!("{}()", type_name)
            } else {
                format!("{}(\n{},\n)", type_name, field_snippets.join(",\n"))
            };

            CompletionItem {
                label: type_name.to_string(),
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(format!("struct {}", type_info.name)),
                documentation: type_info.docs.as_ref().map(|docs| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: docs.clone(),
                    })
                }),
                insert_text: Some(snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        }
        TypeKind::Enum(_variants) => {
            // For enums, just provide the type name - variants will be suggested separately
            CompletionItem {
                label: type_name.to_string(),
                kind: Some(CompletionItemKind::ENUM),
                detail: Some(format!("enum {}", type_info.name)),
                documentation: type_info.docs.as_ref().map(|docs| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: docs.clone(),
                    })
                }),
                insert_text: Some(format!("{}($0)", type_name)),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        }
    }
}

/// Extract inner type from generics (e.g., Option<T> -> T, Vec<T> -> T)
fn extract_inner_type(type_string: &str) -> String {
    let clean = type_string.replace(" ", "");

    if let Some(start) = clean.find('<') {
        if let Some(end) = clean.rfind('>') {
            return clean[start + 1..end].to_string();
        }
    }

    type_string.to_string()
}

/// Generate value completions based on field type
async fn generate_value_completions_by_type(
    field_type: &str,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();

    // Clean up the type string (remove spaces)
    let clean_type = field_type.replace(" ", "");

    // First check if this is a custom type (struct or enum) in the workspace
    if let Some(type_info) = analyzer.get_type_info(field_type).await {
        match &type_info.kind {
            TypeKind::Enum(variants) => {
                // For enums, provide completions for each variant
                for variant in variants {
                    let completion = CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(format!("Variant of {}", type_info.name)),
                        documentation: variant.docs.as_ref().map(|docs| {
                            Documentation::MarkupContent(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: docs.clone(),
                            })
                        }),
                        insert_text: Some(variant.name.clone()),
                        ..Default::default()
                    };
                    completions.push(completion);
                }
                return completions;
            }
            TypeKind::Struct(_) => {
                // For structs, provide the type with snippet
                completions.push(create_type_completion(&type_info));
                return completions;
            }
        }
    }

    // Check for generic types and try to provide completions for the inner type
    if clean_type.starts_with("Option<") {
        let inner = extract_inner_type(&clean_type);
        if let Some(type_info) = analyzer.get_type_info(&inner).await {
            completions.push(CompletionItem {
                label: format!(
                    "Some({})",
                    type_info.name.split("::").last().unwrap_or(&type_info.name)
                ),
                kind: Some(CompletionItemKind::VALUE),
                detail: Some("Some variant with nested type".to_string()),
                insert_text: Some(format!(
                    "Some({}($0))",
                    type_info.name.split("::").last().unwrap_or(&type_info.name)
                )),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        } else {
            completions.push(CompletionItem {
                label: "Some()".to_string(),
                kind: Some(CompletionItemKind::VALUE),
                detail: Some("Some variant".to_string()),
                insert_text: Some("Some($0)".to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }
        completions.push(CompletionItem {
            label: "None".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("None variant".to_string()),
            insert_text: Some("None".to_string()),
            ..Default::default()
        });
        return completions;
    }

    // Handle primitive types
    if clean_type == "bool" {
        completions.push(CompletionItem {
            label: "true".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Boolean value".to_string()),
            insert_text: Some("true".to_string()),
            ..Default::default()
        });
        completions.push(CompletionItem {
            label: "false".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Boolean value".to_string()),
            insert_text: Some("false".to_string()),
            ..Default::default()
        });
    } else if clean_type.starts_with("Option<") {
        completions.push(CompletionItem {
            label: "Some()".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Some variant".to_string()),
            insert_text: Some("Some($0)".to_string()),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        });
        completions.push(CompletionItem {
            label: "None".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("None variant".to_string()),
            insert_text: Some("None".to_string()),
            ..Default::default()
        });
    } else if clean_type.starts_with("Vec<") || clean_type.starts_with("[") {
        completions.push(CompletionItem {
            label: "[]".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Empty vector/array".to_string()),
            insert_text: Some("[]".to_string()),
            ..Default::default()
        });
        completions.push(CompletionItem {
            label: "[...]".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Vector/array with elements".to_string()),
            insert_text: Some("[$0]".to_string()),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    } else if clean_type.starts_with("HashMap<") || clean_type.starts_with("BTreeMap<") {
        completions.push(CompletionItem {
            label: "{}".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Empty map".to_string()),
            insert_text: Some("{}".to_string()),
            ..Default::default()
        });
        completions.push(CompletionItem {
            label: "{...}".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("Map with entries".to_string()),
            insert_text: Some("{$0}".to_string()),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    } else if clean_type == "String" || clean_type == "&str" {
        completions.push(CompletionItem {
            label: "\"\"".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some("String value".to_string()),
            insert_text: Some("\"$0\"".to_string()),
            insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    } else if clean_type.starts_with("i")
        || clean_type.starts_with("u")
        || clean_type.starts_with("f")
    {
        // Numeric types (i8, i16, i32, i64, u8, u16, u32, u64, f32, f64)
        completions.push(CompletionItem {
            label: "0".to_string(),
            kind: Some(CompletionItemKind::VALUE),
            detail: Some(format!("{} value", field_type)),
            insert_text: Some("0".to_string()),
            ..Default::default()
        });
    }

    completions
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_analyzer::{EnumVariant, FieldInfo, TypeInfo, TypeKind};

    #[tokio::test]
    async fn test_enum_variant_field_completion() {
        // Create a mock enum with a struct variant
        let variant = EnumVariant {
            name: "StructVariant".to_string(),
            fields: vec![
                FieldInfo {
                    name: "field_a".to_string(),
                    type_name: "String".to_string(),
                    docs: Some("Field A documentation".to_string()),
                    line: Some(10),
                    column: Some(8),
                    has_default: false,
                },
                FieldInfo {
                    name: "field_b".to_string(),
                    type_name: "i32".to_string(),
                    docs: None,
                    line: Some(11),
                    column: Some(8),
                    has_default: false,
                },
            ],
            docs: Some("A struct variant".to_string()),
            line: Some(9),
            column: Some(4),
        };

        let type_info = TypeInfo {
            name: "MyEnum".to_string(),
            kind: TypeKind::Enum(vec![variant]),
            docs: None,
            source_file: None,
            line: Some(8),
            column: Some(0),
            has_default: false,
        };

        // Test content with a struct variant (RON uses parentheses)
        let content = "MyEnum::StructVariant(\n    \n)";
        let position = Position::new(1, 4); // Inside the parens

        let analyzer = std::sync::Arc::new(crate::rust_analyzer::RustAnalyzer::new());
        let completions =
            generate_completions_for_type(content, position, &type_info, analyzer).await;

        // Should complete with variant fields
        assert!(!completions.is_empty());
        let field_labels: Vec<String> = completions.iter().map(|c| c.label.clone()).collect();
        assert!(field_labels.contains(&"field_a".to_string()));
        assert!(field_labels.contains(&"field_b".to_string()));

        // Check that field_a has documentation
        let field_a = completions.iter().find(|c| c.label == "field_a").unwrap();
        assert!(field_a.documentation.is_some());
        if let Some(Documentation::MarkupContent(content)) = &field_a.documentation {
            assert!(content.value.contains("Field A documentation"));
        }
    }

    #[tokio::test]
    async fn test_enum_variant_completion() {
        // Create a mock enum with multiple variants
        let variant1 = EnumVariant {
            name: "UnitVariant".to_string(),
            fields: vec![],
            docs: Some("A unit variant".to_string()),
            line: Some(9),
            column: Some(4),
        };

        let variant2 = EnumVariant {
            name: "TupleVariant".to_string(),
            fields: vec![FieldInfo {
                name: "0".to_string(),
                type_name: "i32".to_string(),
                docs: None,
                line: None,
                column: None,
                has_default: false,
            }],
            docs: Some("A tuple variant".to_string()),
            line: Some(10),
            column: Some(4),
        };

        let type_info = TypeInfo {
            name: "MyEnum".to_string(),
            kind: TypeKind::Enum(vec![variant1, variant2]),
            docs: None,
            source_file: None,
            line: Some(8),
            column: Some(0),
            has_default: false,
        };

        // Test in FieldName context - should get variant completions
        let content = "";
        let position = Position::new(0, 0);

        let analyzer = std::sync::Arc::new(crate::rust_analyzer::RustAnalyzer::new());
        let completions =
            generate_completions_for_type(content, position, &type_info, analyzer).await;

        // Should complete with variant names
        assert!(!completions.is_empty());
        let variant_labels: Vec<String> = completions.iter().map(|c| c.label.clone()).collect();
        assert!(variant_labels.contains(&"UnitVariant".to_string()));
        assert!(variant_labels.contains(&"TupleVariant".to_string()));
    }
}
