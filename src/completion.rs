use crate::ron_parser;
use crate::rust_analyzer::{RustAnalyzer, TypeInfo, TypeKind};
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

/// Determine what we're completing based on cursor position
fn get_completion_context(content: &str, position: Position) -> CompletionContext {
    let lines: Vec<&str> = content.lines().collect();

    if position.line as usize >= lines.len() {
        return CompletionContext::FieldName;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;
    let before_cursor = &line[..col.min(line.len())];

    // Check if we're after a colon (completing a value)
    if let Some(last_colon) = before_cursor.rfind(':') {
        // Make sure there's no comma after the colon (which would mean we're on a new field)
        let after_colon = &before_cursor[last_colon + 1..];
        if !after_colon.contains(',') && !after_colon.trim().is_empty() {
            // If there's already some text after the colon, might be completing a type
            let trimmed = after_colon.trim();
            if trimmed
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == ':')
                && !trimmed.is_empty()
            {
                return CompletionContext::StructType;
            }
        }
        return CompletionContext::FieldValue;
    }

    // Default to field name completion
    CompletionContext::FieldName
}

/// Find the type we're currently inside (e.g., if typing "User(" we return "User")
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

pub async fn generate_completions(
    content: &str,
    position: Position,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<CompletionItem> {
    // Find nested type contexts at cursor position
    let contexts = ron_parser::find_type_context_at_position(content, position);

    // Navigate through contexts to find the innermost type
    let mut current_type_info = Some(type_info.clone());

    for context in contexts.iter().skip(1) {
        // Skip first since it's the top-level type we already have
        if let Some(info) = current_type_info {
            // Try to find the context type as a field's type
            if let Some(fields) = info.fields() {
                if let Some(field) = fields.iter().find(|f| f.type_name.contains(&context.type_name)) {
                    current_type_info = analyzer.get_type_info(&field.type_name).await;
                    continue;
                }
            }
            // Try as direct type lookup
            current_type_info = analyzer.get_type_info(&context.type_name).await;
        }
    }

    let effective_type = current_type_info.as_ref().unwrap_or(type_info);

    let context = get_completion_context(content, position);

    match context {
        CompletionContext::FieldName => generate_field_completions(content, effective_type),
        CompletionContext::FieldValue => {
            // Find the field we're completing the value for
            if let Some(field_name) = find_current_field(content, position) {
                let mut completions =
                    generate_value_completions_for_field(field_name, effective_type, analyzer.clone())
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

fn generate_field_completions(content: &str, type_info: &TypeInfo) -> Vec<CompletionItem> {
    match &type_info.kind {
        TypeKind::Struct(fields) => {
            // Get fields already used in the RON file
            let used_fields = ron_parser::extract_fields_from_ron(content);

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

/// Find the field name for the current cursor position
fn find_current_field(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();

    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;
    let before_cursor = &line[..col.min(line.len())];

    // Find the last colon before cursor
    if let Some(colon_pos) = before_cursor.rfind(':') {
        let before_colon = &before_cursor[..colon_pos].trim();

        // Extract field name
        if let Some(word_start) = before_colon.rfind(|c: char| !c.is_alphanumeric() && c != '_') {
            return Some(before_colon[word_start + 1..].to_string());
        } else {
            return Some(before_colon.to_string());
        }
    }

    None
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
