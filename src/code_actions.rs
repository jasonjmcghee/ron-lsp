use crate::ron_parser;
use crate::rust_analyzer::{FieldInfo, TypeInfo};
use tower_lsp::lsp_types::*;

/// Generate all code actions for a RON document
pub fn generate_code_actions(
    content: &str,
    type_info: &TypeInfo,
    uri: &str,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Add actions for making struct names explicit
    actions.extend(generate_explicit_type_actions(content, type_info, uri));

    // Add actions for missing fields (only for structs)
    if let Some(fields) = type_info.fields() {
        actions.extend(generate_missing_field_actions(
            content, fields, type_info, uri,
        ));
    }

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
}
