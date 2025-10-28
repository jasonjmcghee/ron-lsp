use crate::ron_parser;
use crate::rust_analyzer::{FieldInfo, RustAnalyzer, TypeInfo, TypeKind};
use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tower_lsp::Client;

/// Generate all code actions for a RON document
pub async fn generate_code_actions(
    content: &str,
    type_info: &TypeInfo,
    uri: &str,
    analyzer: Arc<RustAnalyzer>,
    client: &Client,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Add actions for making struct names explicit
    actions.extend(generate_explicit_type_actions(content, type_info, uri));

    // Add actions for missing fields
    match &type_info.kind {
        TypeKind::Struct(fields) => {
            actions.extend(generate_missing_field_actions(
                content, fields, type_info, uri,
            ));
            // Also check for nested enum variant fields
            actions.extend(generate_missing_variant_field_actions(content, type_info, uri, analyzer.clone(), client).await);
        }
        TypeKind::Enum(_) => {
            // For enums, generate_missing_field_actions handles variant fields internally
            actions.extend(generate_missing_field_actions(
                content, &[], type_info, uri,
            ));
            // Also check for nested enum variant fields within the enum's variants
            actions.extend(generate_missing_variant_field_actions(content, type_info, uri, analyzer.clone(), client).await);
        }
    }

    actions
}

/// Generate code actions for adding missing fields in nested enum variants
async fn generate_missing_variant_field_actions(
    content: &str,
    type_info: &TypeInfo,
    uri: &str,
    analyzer: Arc<RustAnalyzer>,
    client: &Client,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();
    let variant_locations = ron_parser::find_all_variant_field_locations(content);

    client.log_message(MessageType::INFO, format!("DEBUG code_actions: Found {} variant locations for type {}", variant_locations.len(), type_info.name)).await;
    for loc in &variant_locations {
        client.log_message(MessageType::INFO, format!(
            "  - Line {}: variant={}, containing_field={}, field_at_pos={:?}",
            loc.line_idx, loc.variant_name, loc.containing_field_name, loc.field_at_position
        )).await;
    }

    // Group locations by (containing_field_name, variant_name) to find which fields are used
    let mut variant_fields_map: std::collections::HashMap<(String, String), std::collections::HashSet<String>> =
        std::collections::HashMap::new();

    // Collect all fields used for each variant
    for location in &variant_locations {
        if let Some(ref field_at_pos) = location.field_at_position {
            let key = (location.containing_field_name.clone(), location.variant_name.clone());
            variant_fields_map.entry(key).or_insert_with(std::collections::HashSet::new).insert(field_at_pos.clone());
        }
    }

    // Now generate actions for each unique variant
    let mut seen_variants = std::collections::HashSet::new();
    for location in variant_locations {
        let key = (location.containing_field_name.clone(), location.variant_name.clone());
        if seen_variants.contains(&key) {
            continue;
        }
        seen_variants.insert(key.clone());

        // Need to navigate to the right type that contains this field
        // The containing_field_name might be a field in a struct, or it might be nested deeper

        // First try: if type_info is a struct, look for the field directly
        let target_type_info: Option<String> = if let Some(field) = type_info.find_field(&location.containing_field_name) {
            client.log_message(MessageType::INFO, format!("Found field {} in struct {}", location.containing_field_name, type_info.name)).await;
            Some(field.type_name.clone())
        } else {
            // Second try: if type_info is an enum, we need to find which variant contains the field
            // This means navigating through the structure
            client.log_message(MessageType::INFO, format!("Field {} not found directly, searching in enum variants", location.containing_field_name)).await;

            // Use the same navigation logic as goto_definition would
            // For now, check if containing_field_name is actually a nested type we can look up
            None
        };

        if target_type_info.is_none() {
            // Try to look up containing_field_name as a type name directly (for nested structs)
            // This handles cases like Post containing post_type field
            // We need to figure out what type contains the field with name containing_field_name
            client.log_message(MessageType::INFO, format!("Skipping variant location - couldn't resolve containing type for field {}", location.containing_field_name)).await;
            continue;
        }

        if let Some(field_type_name) = target_type_info {
            // Get the enum type for this field
            if let Some(field_type_info) = analyzer.get_type_info(&field_type_name).await {
                client.log_message(MessageType::INFO, format!("Got type info for {}", field_type_name)).await;
                // Find the variant definition
                if let Some(variant) = field_type_info.find_variant(&location.variant_name) {
                    client.log_message(MessageType::INFO, format!("Found variant {} with {} fields", location.variant_name, variant.fields.len())).await;
                    // Get fields used in this variant from our map
                    let used_fields = variant_fields_map.get(&key).cloned().unwrap_or_default();
                    client.log_message(MessageType::INFO, format!("Used fields: {:?}", used_fields)).await;

                    let missing_fields: Vec<_> = variant.fields
                        .iter()
                        .filter(|vfield| !used_fields.contains(&vfield.name))
                        .collect();

                    let required_missing: Vec<_> = missing_fields
                        .iter()
                        .filter(|&&vfield| !vfield.type_name.starts_with("Option") && !field_type_info.has_default)
                        .copied()
                        .collect();

                    client.log_message(MessageType::INFO, format!("Missing {} required fields", required_missing.len())).await;
                    if !required_missing.is_empty() {
                        if let Some(edit) = generate_field_insertions(&required_missing, content) {
                            let mut changes = std::collections::HashMap::new();
                            changes.insert(
                                tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                                vec![edit],
                            );

                            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                                title: format!(
                                    "Add {} required field{} to {}::{}",
                                    required_missing.len(),
                                    if required_missing.len() == 1 { "" } else { "s" },
                                    location.containing_field_name,
                                    location.variant_name
                                ),
                                kind: Some(CodeActionKind::QUICKFIX),
                                edit: Some(WorkspaceEdit {
                                    changes: Some(changes),
                                    ..Default::default()
                                }),
                                ..Default::default()
                            }));
                        }
                    }
                } else {
                    client.log_message(MessageType::INFO, format!("Variant {} not found in type {}", location.variant_name, field_type_name)).await;
                }
            } else {
                client.log_message(MessageType::INFO, format!("Could not get type info for {}", field_type_name)).await;
            }
        }
    }

    client.log_message(MessageType::INFO, format!("Generated {} variant field actions", actions.len())).await;
    actions
}

/// Generate code actions for making implicit struct names explicit
fn generate_explicit_type_actions(
    content: &str,
    type_info: &TypeInfo,
    uri: &str,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Check if the root level uses unnamed struct syntax
    if let Some(action) = create_explicit_root_type_action(content, type_info, uri) {
        actions.push(action);
    }

    // Check for nested unnamed structs in field values
    if let Some(fields) = type_info.fields() {
        for field in fields {
            if let Some(action) = create_explicit_field_type_action(content, field, uri) {
                actions.push(action);
            }
        }
    }

    actions
}

/// Generate code actions for adding missing fields
fn generate_missing_field_actions(
    content: &str,
    fields: &[FieldInfo],
    type_info: &TypeInfo,
    uri: &str,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Check if we're in an enum variant context
    if let TypeKind::Enum(variants) = &type_info.kind {
        // Try to detect which variant we're in
        if let Some(variant_name) = detect_current_variant_in_content(content) {
            if let Some(variant) = variants.iter().find(|v| v.name == variant_name) {
                // Generate actions for this variant's fields
                let ron_fields = ron_parser::extract_fields_from_ron(content);
                let all_missing: Vec<_> = variant.fields
                    .iter()
                    .filter(|field| !ron_fields.contains(&field.name))
                    .collect();

                let required_missing: Vec<_> = all_missing
                    .iter()
                    .filter(|&&field| !field.type_name.starts_with("Option") && !type_info.has_default)
                    .copied()
                    .collect();

                // Code action: Add all required fields for variant
                if !required_missing.is_empty() {
                    if let Some(edit) = generate_field_insertions(&required_missing, content) {
                        let mut changes = std::collections::HashMap::new();
                        changes.insert(
                            tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                            vec![edit],
                        );

                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: format!(
                                "Add {} required field{} to {}",
                                required_missing.len(),
                                if required_missing.len() == 1 { "" } else { "s" },
                                variant_name
                            ),
                            kind: Some(CodeActionKind::QUICKFIX),
                            edit: Some(WorkspaceEdit {
                                changes: Some(changes),
                                ..Default::default()
                            }),
                            ..Default::default()
                        }));
                    }
                }

                // Code action: Add all fields for variant
                if !all_missing.is_empty() {
                    if let Some(edit) = generate_field_insertions(&all_missing, content) {
                        let mut changes = std::collections::HashMap::new();
                        changes.insert(
                            tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                            vec![edit],
                        );

                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: format!(
                                "Add all {} missing field{} to {}",
                                all_missing.len(),
                                if all_missing.len() == 1 { "" } else { "s" },
                                variant_name
                            ),
                            kind: Some(CodeActionKind::QUICKFIX),
                            edit: Some(WorkspaceEdit {
                                changes: Some(changes),
                                ..Default::default()
                            }),
                            ..Default::default()
                        }));
                    }
                }

                return actions;
            }
        }
    }

    // Original struct logic
    let ron_fields = ron_parser::extract_fields_from_ron(content);

    // Find missing fields
    let all_missing: Vec<_> = fields
        .iter()
        .filter(|field| !ron_fields.contains(&field.name))
        .collect();

    // Find required missing fields (not Option<T> and no Default trait)
    let required_missing: Vec<_> = all_missing
        .iter()
        .filter(|&&field| !field.type_name.starts_with("Option") && !type_info.has_default)
        .copied()
        .collect();

    // Code action: Add all required fields
    if !required_missing.is_empty() {
        if let Some(edit) = generate_field_insertions(&required_missing, content) {
            let mut changes = std::collections::HashMap::new();
            changes.insert(
                tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                vec![edit],
            );

            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!(
                    "Add {} required field{}",
                    required_missing.len(),
                    if required_missing.len() == 1 { "" } else { "s" }
                ),
                kind: Some(CodeActionKind::QUICKFIX),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }
    }

    // Code action: Add all fields
    if !all_missing.is_empty() {
        if let Some(edit) = generate_field_insertions(&all_missing, content) {
            let mut changes = std::collections::HashMap::new();
            changes.insert(
                tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                vec![edit],
            );

            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!(
                    "Add all {} missing field{}",
                    all_missing.len(),
                    if all_missing.len() == 1 { "" } else { "s" }
                ),
                kind: Some(CodeActionKind::QUICKFIX),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }
    }

    actions
}

/// Create action to make root-level struct name explicit
/// Converts: `(field: value)` → `StructName(field: value)`
fn create_explicit_root_type_action(
    content: &str,
    type_info: &TypeInfo,
    uri: &str,
) -> Option<CodeActionOrCommand> {
    // Check if content starts with unnamed struct syntax
    for (line_num, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("/*") {
            continue;
        }

        // If it starts with '(' without a type name, offer to add one
        if trimmed.starts_with('(') {
            let type_name = type_info
                .name
                .split("::")
                .last()
                .unwrap_or(&type_info.name);

            // Find the position of '(' in the original line (accounting for leading whitespace)
            let leading_whitespace = line.len() - line.trim_start().len();
            let paren_pos = leading_whitespace;

            let mut changes = std::collections::HashMap::new();
            changes.insert(
                tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                vec![TextEdit {
                    range: Range::new(
                        Position::new(line_num as u32, paren_pos as u32),
                        Position::new(line_num as u32, paren_pos as u32),
                    ),
                    new_text: type_name.to_string(),
                }],
            );

            return Some(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Make struct name explicit: {}", type_name),
                kind: Some(CodeActionKind::REFACTOR),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }

        break; // Only check first non-comment line
    }

    None
}

/// Create action to make nested field type explicit
/// Converts: `foo: (value)` → `foo: TypeName(value)`
fn create_explicit_field_type_action(
    content: &str,
    field: &FieldInfo,
    uri: &str,
) -> Option<CodeActionOrCommand> {
    // Find the field in the content
    let field_pattern = format!("{}:", field.name);
    let field_pos = content.find(&field_pattern)?;

    // Find where the value starts (after colon)
    let after_field = &content[field_pos + field_pattern.len()..];
    let value_start_in_after = after_field.len() - after_field.trim_start().len();
    let value_str = after_field[value_start_in_after..].trim_start();

    // Check if the value starts with '(' (unnamed struct/tuple)
    if value_str.starts_with('(') {
        // Check if it's already explicitly typed by looking for TypeName(
        // We need to check if there's a type name before the paren
        let before_paren = &after_field[value_start_in_after..]
            .split('(')
            .next()
            .unwrap_or("");

        if before_paren.trim().is_empty() {
            // It's unnamed! Offer to add the type name
            let type_name = field
                .type_name
                .split("::")
                .last()
                .unwrap_or(&field.type_name)
                .replace(" ", "");

            // Strip Option< > and other wrappers
            let clean_type = if type_name.starts_with("Option<") && type_name.ends_with('>') {
                &type_name[7..type_name.len() - 1]
            } else {
                &type_name
            };

            // Calculate the byte offset of the opening paren in the entire content
            let paren_offset = field_pos + field_pattern.len() + value_start_in_after;

            // Convert byte offset to line/column position
            let before_paren_content = &content[..paren_offset];
            // Count newlines to get 0-indexed line number
            let paren_line_num = before_paren_content.matches('\n').count();
            let line_start_offset = before_paren_content.rfind('\n').map(|pos| pos + 1).unwrap_or(0);
            let paren_col = paren_offset - line_start_offset;

            let mut changes = std::collections::HashMap::new();
            changes.insert(
                tower_lsp::lsp_types::Url::parse(uri).unwrap(),
                vec![TextEdit {
                    range: Range::new(
                        Position::new(paren_line_num as u32, paren_col as u32),
                        Position::new(paren_line_num as u32, paren_col as u32),
                    ),
                    new_text: clean_type.to_string(),
                }],
            );

            return Some(CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Make field type explicit: {} {}", field.name, clean_type),
                kind: Some(CodeActionKind::REFACTOR),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            }));
        }
    }

    None
}

/// Generate text edits to insert missing fields
fn generate_field_insertions(missing_fields: &[&FieldInfo], content: &str) -> Option<TextEdit> {
    // Find the insertion point - right before the closing parenthesis
    let lines: Vec<&str> = content.lines().collect();

    // Find the last line with content (before the closing paren)
    let mut insert_line = 0;
    let mut insert_col = 0;
    let mut found_opening = false;

    for (line_num, line) in lines.iter().enumerate() {
        if line.contains('(') {
            found_opening = true;
        }
        if found_opening && line.contains(')') {
            insert_line = line_num;
            // Find the position of the closing paren
            insert_col = line.find(')').unwrap_or(0);
            break;
        }
    }

    if !found_opening {
        return None;
    }

    // Check if we need to add a comma before our new fields
    let needs_comma = if insert_line > 0 {
        let prev_line = lines[insert_line.saturating_sub(1)].trim();
        !prev_line.is_empty() && !prev_line.ends_with(',') && !prev_line.ends_with('(')
    } else {
        false
    };

    // Generate the field text
    let mut field_text = String::new();
    if needs_comma {
        field_text.push_str(",\n");
    } else if insert_line > 0 {
        field_text.push('\n');
    }

    // Detect indentation from existing content
    let indent = if let Some(line_with_field) = lines
        .iter()
        .find(|l| l.contains(':') && !l.trim().starts_with("/*"))
    {
        let trimmed = line_with_field.trim_start();
        &line_with_field[..line_with_field.len() - trimmed.len()]
    } else {
        "    " // default to 4 spaces
    };

    for (i, field) in missing_fields.iter().enumerate() {
        field_text.push_str(indent);
        field_text.push_str(&field.name);
        field_text.push_str(": ");
        field_text.push_str(&generate_default_value(&field.type_name));
        if i < missing_fields.len() - 1 {
            field_text.push(',');
        }
        field_text.push('\n');
    }

    Some(TextEdit {
        range: Range::new(
            Position::new(insert_line as u32, insert_col as u32),
            Position::new(insert_line as u32, insert_col as u32),
        ),
        new_text: field_text,
    })
}

/// Detect which enum variant we're currently inside based on the content
/// This scans for patterns like EnumName::VariantName( or EnumName::VariantName {
fn detect_current_variant_in_content(content: &str) -> Option<String> {
    // Look for pattern: EnumName::VariantName( or EnumName::VariantName {
    // RON uses parentheses for struct variants
    let re = regex::Regex::new(r"\w+::(\w+)\s*[\(\{]").ok()?;
    let caps = re.captures(content)?;
    Some(caps.get(1)?.as_str().to_string())
}

/// Generate a default value for a given Rust type
fn generate_default_value(type_name: &str) -> String {
    let clean = type_name.replace(" ", "");

    if clean.starts_with("Option") {
        "None".to_string()
    } else if clean == "bool" {
        "false".to_string()
    } else if clean.starts_with("Vec") || clean.starts_with("[") {
        "[]".to_string()
    } else if clean.starts_with("HashMap") || clean.starts_with("BTreeMap") {
        "{}".to_string()
    } else if clean == "String" || clean == "&str" || clean == "str" {
        "\"\"".to_string()
    } else if clean.chars().all(|c| c.is_numeric() || c == 'i' || c == 'u' || c == 'f') {
        // Numeric types
        "0".to_string()
    } else {
        // Custom type - use constructor notation with placeholder
        format!("{}()", clean)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_analyzer::TypeKind;

    #[test]
    fn test_explicit_root_type_same_line() {
        let content = "(id: 1, name: \"test\")";
        let type_info = TypeInfo {
            name: "User".to_string(),
            kind: TypeKind::Struct(vec![]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
            has_default: false,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_root_type_action(content, &type_info, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make struct name explicit: User");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, "User");
            assert_eq!(text_edits[0].range.start.line, 0);
            assert_eq!(text_edits[0].range.start.character, 0);
        }
    }

    #[test]
    fn test_explicit_root_type_next_line() {
        let content = "(\n    id: 1,\n    name: \"test\"\n)";
        let type_info = TypeInfo {
            name: "example::User".to_string(),
            kind: TypeKind::Struct(vec![]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
            has_default: false,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_root_type_action(content, &type_info, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make struct name explicit: User");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, "User");
            assert_eq!(text_edits[0].range.start.line, 0);
            assert_eq!(text_edits[0].range.start.character, 0);
        }
    }

    #[test]
    fn test_explicit_field_type_same_line() {
        let content = "User(author: (id: 1, name: \"test\"))";
        let field = FieldInfo {
            name: "author".to_string(),
            type_name: "Author".to_string(),
            docs: None,
            line: None,
            column: None,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_field_type_action(content, &field, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make field type explicit: author Author");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, "Author");
            // Should insert before the opening paren after "author: "
            assert_eq!(text_edits[0].range.start.line, 0);
            assert_eq!(text_edits[0].range.start.character, 13); // position of '(' after "author: "
        }
    }

    #[test]
    fn test_explicit_field_type_next_line() {
        let content = r#"Post(
    author: (
        id: 5,
        name: "Charlie",
        email: "charlie@example.com"
    )
)"#;
        let field = FieldInfo {
            name: "author".to_string(),
            type_name: "User".to_string(),
            docs: None,
            line: None,
            column: None,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_field_type_action(content, &field, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make field type explicit: author User");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, "User");
            // Should insert at the opening paren on line 1
            assert_eq!(text_edits[0].range.start.line, 1);
            assert_eq!(text_edits[0].range.start.character, 12); // position of '(' on second line
        }
    }

    #[test]
    fn test_explicit_field_type_with_option_wrapper() {
        let content = "User(author: (id: 1, name: \"test\"))";
        let field = FieldInfo {
            name: "author".to_string(),
            type_name: "Option<Author>".to_string(),
            docs: None,
            line: None,
            column: None,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_field_type_action(content, &field, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make field type explicit: author Author");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits[0].new_text, "Author"); // Should strip Option wrapper
        }
    }

    #[test]
    fn test_no_action_for_explicit_type() {
        let content = "User(id: 1, name: \"test\")";
        let type_info = TypeInfo {
            name: "User".to_string(),
            kind: TypeKind::Struct(vec![]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
            has_default: false,
        };
        let uri = "file:///test.ron";

        // Should not offer action since type is already explicit
        let action = create_explicit_root_type_action(content, &type_info, uri);
        assert!(action.is_none());
    }

    #[test]
    fn test_no_action_for_explicit_field_type() {
        let content = "User(author: Author(id: 1, name: \"test\"))";
        let field = FieldInfo {
            name: "author".to_string(),
            type_name: "Author".to_string(),
            docs: None,
            line: None,
            column: None,
        };
        let uri = "file:///test.ron";

        // Should not offer action since field type is already explicit
        let action = create_explicit_field_type_action(content, &field, uri);
        assert!(action.is_none());
    }

    #[test]
    fn test_real_world_with_comments_and_spacing() {
        let content = r#"Post(
    id: 123,
    title: "Mixed Syntax Example",
    content: "Demonstrating both explicit and unnamed struct syntax",

    // Explicit type name for author
    author: (
        id: 5,
        name: "Charlie",
        email: "charlie@example.com",
        age: 28,
        bio: None,
        is_active: true,
        roles: ["editor"],
    ),

    likes: 50,
    tags: ["example", "syntax"],
    published: true,
    post_type: Short,
)"#;
        let field = FieldInfo {
            name: "author".to_string(),
            type_name: "User".to_string(),
            docs: None,
            line: None,
            column: None,
        };
        let uri = "file:///test.ron";

        let action = create_explicit_field_type_action(content, &field, uri);
        assert!(action.is_some());

        if let Some(CodeActionOrCommand::CodeAction(action)) = action {
            assert_eq!(action.title, "Make field type explicit: author User");
            let edit = action.edit.unwrap();
            let changes = edit.changes.unwrap();
            let text_edits = changes.values().next().unwrap();
            assert_eq!(text_edits.len(), 1);
            assert_eq!(text_edits[0].new_text, "User");
            // The opening paren is on line 6 (0-indexed), after the comment
            assert_eq!(text_edits[0].range.start.line, 6);
            // The opening paren is at column 12 (after "    author: ")
            assert_eq!(text_edits[0].range.start.character, 12);

            println!("Line: {}, Character: {}",
                text_edits[0].range.start.line,
                text_edits[0].range.start.character);
        }
    }

    #[tokio::test]
    async fn test_enum_variant_missing_fields() {
        use crate::rust_analyzer::EnumVariant;

        let variant = EnumVariant {
            name: "StructVariant".to_string(),
            fields: vec![
                FieldInfo {
                    name: "field_a".to_string(),
                    type_name: "String".to_string(),
                    docs: None,
                    line: Some(10),
                    column: Some(8),
                },
                FieldInfo {
                    name: "field_b".to_string(),
                    type_name: "i32".to_string(),
                    docs: None,
                    line: Some(11),
                    column: Some(8),
                },
            ],
            docs: None,
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

        let content = "MyEnum::StructVariant(\n    field_a: \"test\"\n)";
        let uri = "file:///test.ron";

        // Create mock analyzer and client for the test
        use std::sync::Arc;
        use std::path::PathBuf;
        use crate::rust_analyzer::RustAnalyzer;
        use tower_lsp::Client;

        let analyzer = Arc::new(RustAnalyzer::new());
        let (service, _) = tower_lsp::LspService::new(|client| crate::Backend {
            client: client.clone(),
            documents: Default::default(),
            rust_analyzer: analyzer.clone(),
        });
        let client = service.inner().client.clone();

        let actions = generate_code_actions(content, &type_info, uri, analyzer, &client).await;

        // Should suggest adding missing field_b
        assert!(!actions.is_empty());

        let titles: Vec<String> = actions
            .iter()
            .filter_map(|a| {
                if let CodeActionOrCommand::CodeAction(action) = a {
                    Some(action.title.clone())
                } else {
                    None
                }
            })
            .collect();

        // Should have action mentioning the variant name
        assert!(titles.iter().any(|t| t.contains("StructVariant")));
        assert!(titles.iter().any(|t| t.contains("field")));
    }

    #[test]
    fn test_detect_current_variant_in_content() {
        let content = "MyEnum::StructVariant(\n    field_a: value\n)";
        let variant = detect_current_variant_in_content(content);
        assert_eq!(variant, Some("StructVariant".to_string()));
    }

    #[test]
    fn test_detect_current_variant_in_content_no_match() {
        let content = "MyStruct(\n    field_a: value\n)";
        let variant = detect_current_variant_in_content(content);
        assert!(variant.is_none());
    }
}
