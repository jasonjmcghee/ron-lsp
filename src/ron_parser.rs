use ron::Value;
use std::collections::HashSet;
use tower_lsp::lsp_types::Position;

/// Get the field name at a specific position in RON content
pub fn get_field_at_position(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();

    if position.line as usize >= lines.len() {
        return None;
    }

    let line = lines[position.line as usize];
    let col = position.character as usize;

    // Find field name before the colon
    // Pattern: field_name: value
    if let Some(colon_pos) = line[..col.min(line.len())].rfind(':') {
        let before_colon = &line[..colon_pos].trim();

        // Extract the last word before the colon
        if let Some(word_start) = before_colon.rfind(|c: char| !c.is_alphanumeric() && c != '_') {
            return Some(before_colon[word_start + 1..].to_string());
        } else {
            return Some(before_colon.to_string());
        }
    }

    None
}

/// Parse RON structure to get all field names present using proper RON parsing
pub fn extract_fields_from_ron(content: &str) -> Vec<String> {
    // Try to parse the RON content
    match ron::from_str::<Value>(content) {
        Ok(value) => extract_fields_from_value(&value),
        Err(_) => {
            // If parsing fails, return empty vec
            // The diagnostics will catch the syntax error
            Vec::new()
        }
    }
}

/// Extract field names from a RON Value recursively
fn extract_fields_from_value(value: &Value) -> Vec<String> {
    let mut fields = HashSet::new();

    match value {
        Value::Map(map) => {
            for (key, _) in map.iter() {
                if let Value::String(field_name) = key {
                    fields.insert(field_name.clone());
                }
            }
        }
        Value::Seq(seq) => {
            // For sequences, we don't have fields, but we might have nested structs
            for item in seq {
                fields.extend(extract_fields_from_value(item));
            }
        }
        _ => {}
    }

    fields.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_fields_from_struct() {
        let content = r#"MyStruct(
    name: "test",
    age: 30,
    items: [],
)"#;
        let fields = extract_fields_from_ron(content);
        let mut sorted_fields = fields.clone();
        sorted_fields.sort();
        assert_eq!(sorted_fields, vec!["age", "items", "name"]);
    }

    #[test]
    fn test_extract_fields_with_comment() {
        let content = r##"/* @[crate::models::Post] */

Post(
    id: 101,
    title: "Getting Started with Rust",
    content: "# Introduction",
    author_id: 1,
    likes: 42,
    tags: ["rust", "programming"],
    published: true,
)"##;
        let fields = extract_fields_from_ron(content);

        // Should not include any fields from the comment
        assert!(!fields.contains(&"@[crate".to_string()));
        assert!(!fields.contains(&"crate".to_string()));

        // Should include actual fields
        assert!(fields.contains(&"id".to_string()));
        assert!(fields.contains(&"title".to_string()));
        assert!(fields.contains(&"content".to_string()));
        assert!(fields.contains(&"author_id".to_string()));
        assert!(fields.contains(&"likes".to_string()));
        assert!(fields.contains(&"tags".to_string()));
        assert!(fields.contains(&"published".to_string()));
    }

    #[test]
    fn test_extract_fields_invalid_ron() {
        // Invalid RON should return empty vec
        let content = r#"This is not valid RON"#;
        let fields = extract_fields_from_ron(content);
        assert_eq!(fields, Vec::<String>::new());
    }

    #[test]
    fn test_extract_fields_nested_structs() {
        let content = r#"MyStruct(
    name: "test",
    config: Config(
        enabled: true,
        timeout: 30,
    ),
)"#;
        let fields = extract_fields_from_ron(content);

        // Should extract top-level fields
        assert!(fields.contains(&"name".to_string()));
        assert!(fields.contains(&"config".to_string()));

        // May also extract nested fields depending on implementation
        // This is acceptable behavior
    }

    #[test]
    fn test_get_field_at_position() {
        let content = r#"MyStruct(
    name: "test",
    age: 30,
)"#;
        // Position on "name" line, right after the colon (position includes the colon)
        let field = get_field_at_position(content, Position::new(1, 9));
        assert_eq!(field, Some("name".to_string()));

        // Position on "age" line, right after the colon
        let field = get_field_at_position(content, Position::new(2, 8));
        assert_eq!(field, Some("age".to_string()));

        // Position in the middle of the field name should not find it
        let field = get_field_at_position(content, Position::new(1, 5));
        assert_eq!(field, None);
    }

    #[test]
    fn test_get_field_at_position_no_colon() {
        let content = r#"MyStruct(
    name "test",
)"#;
        // Position on line without colon after field
        let field = get_field_at_position(content, Position::new(1, 5));
        assert_eq!(field, None);
    }

    #[test]
    fn test_get_field_at_position_on_comment() {
        let content = r#"/* @[crate::models::Post] */
Post(
    id: 101,
)"#;
        // Position on comment line after a colon
        let field = get_field_at_position(content, Position::new(0, 20));

        // This will extract from the comment because get_field_at_position
        // operates on individual lines without context
        // The actual validation layer should filter these out
        assert!(field.is_some());
    }
}
