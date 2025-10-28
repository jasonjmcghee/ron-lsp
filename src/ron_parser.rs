use ron::Value;
use std::collections::HashSet;
use tower_lsp::lsp_types::Position;

/// Information about a variant field location in RON content
#[derive(Debug, Clone)]
pub struct VariantFieldLocation {
    pub line_idx: usize,
    pub variant_name: String,
    pub containing_field_name: String,
    pub field_at_position: Option<String>,
}

/// Represents the nesting context at a cursor position
#[derive(Debug, Clone)]
pub struct TypeContext {
    /// The type name we're currently inside (e.g., "User", "Post")
    pub type_name: String,
    /// The line where this type context starts
    #[allow(dead_code)]
    pub start_line: usize,
}

/// Find the nested type context at a cursor position
/// Returns a stack of type contexts from outermost to innermost
pub fn find_type_context_at_position(content: &str, position: Position) -> Vec<TypeContext> {
    let lines: Vec<&str> = content.lines().collect();
    let mut contexts = Vec::new();

    // Track opening and closing delimiters with their types
    let mut stack: Vec<(String, usize)> = Vec::new(); // (type_name, line_number)

    for (line_idx, line) in lines.iter().enumerate() {
        if line_idx > position.line as usize {
            break;
        }

        let mut chars = line.chars().peekable();
        let mut col = 0;
        let mut in_string = false;
        let mut escape_next = false;

        while let Some(ch) = chars.next() {
            // Stop at cursor position on the target line
            if line_idx == position.line as usize && col >= position.character as usize {
                break;
            }

            col += 1;

            if escape_next {
                escape_next = false;
                continue;
            }

            match ch {
                '\\' if in_string => escape_next = true,
                '"' => in_string = !in_string,
                '(' if !in_string => {
                    // Look back to find the type name before this paren
                    let before = &line[..col - 1];
                    if let Some(type_name) = extract_type_name_before_paren(before) {
                        stack.push((type_name, line_idx));
                    }
                }
                ')' if !in_string => {
                    stack.pop();
                }
                _ => {}
            }
        }
    }

    // Convert stack to TypeContext vec
    for (type_name, start_line) in stack {
        contexts.push(TypeContext {
            type_name,
            start_line,
        });
    }

    contexts
}

/// Extract the type name before an opening paren
/// e.g., "author: User" -> Some("User")
/// e.g., "Post" -> Some("Post")
fn extract_type_name_before_paren(text: &str) -> Option<String> {
    let trimmed = text.trim_end();

    // Find the last word (which should be the type name)
    let chars: Vec<char> = trimmed.chars().collect();
    let mut end = chars.len();

    // Skip back while we have alphanumeric or underscore
    while end > 0 && (chars[end - 1].is_alphanumeric() || chars[end - 1] == '_') {
        end -= 1;
    }

    if end >= chars.len() {
        return None;
    }

    let type_name: String = chars[end..].iter().collect();

    // Only return if it looks like a type (starts with uppercase)
    if !type_name.is_empty() && type_name.chars().next().unwrap().is_uppercase() {
        Some(type_name)
    } else {
        None
    }
}

/// Get the field name at a specific position in RON content
/// Scan through content and find all variant field locations
/// This is used by both diagnostics and code actions to process enum variant fields
pub fn find_all_variant_field_locations(content: &str) -> Vec<VariantFieldLocation> {
    let mut locations = Vec::new();
    let lines: Vec<&str> = content.lines().collect();

    for (line_idx, _line) in lines.iter().enumerate() {
        let position = Position::new(line_idx as u32, 0);

        // Check if we're inside a variant at this position
        if let Some(variant_name) = find_current_variant_context(content, position) {
            // Find which field contains this variant
            if let Some(containing_field_name) = get_containing_field_context(content, position) {
                // Get the field at this position (might be None if we're not on a field line)
                let field_at_position = get_field_at_position(content, position);

                locations.push(VariantFieldLocation {
                    line_idx,
                    variant_name,
                    containing_field_name,
                    field_at_position,
                });
            }
        }
    }

    locations
}

/// This looks for "field_name:" on the CURRENT line before the cursor
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

/// Get the containing field context by scanning backwards
/// This is useful when you're nested inside a field's value
/// For example: "post_type: Detailed(\n    length: 1" - when on "length" line, returns "post_type"
pub fn get_containing_field_context(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    // Build content up to cursor
    let mut content_until_cursor = String::new();
    for (i, line) in lines.iter().enumerate() {
        if i < position.line as usize {
            content_until_cursor.push_str(line);
            content_until_cursor.push('\n');
        } else if i == position.line as usize {
            content_until_cursor.push_str(&line[..position.character.min(line.len() as u32) as usize]);
            break;
        }
    }

    // Look for the last field assignment before an opening paren/brace
    // Pattern: field_name: SomeType(   or   field_name: (
    let re = regex::Regex::new(r"(\w+)\s*:\s*[A-Z]?\w*\s*[\(\{][^\)\}]*$").ok()?;

    // Find the last match
    let mut last_field = None;
    for caps in re.captures_iter(&content_until_cursor) {
        if let Some(field) = caps.get(1) {
            last_field = Some(field.as_str().to_string());
        }
    }

    last_field
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

/// Find the current variant context in content (for detecting which variant we're inside)
/// This is useful for code actions and completions
/// This tracks parentheses depth to find which variant we're actually inside
pub fn find_current_variant_context(content: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = content.lines().collect();
    if position.line as usize >= lines.len() {
        return None;
    }

    // Build content up to cursor position
    let mut content_until_cursor = String::new();
    for (i, line) in lines.iter().enumerate() {
        if i < position.line as usize {
            content_until_cursor.push_str(line);
            content_until_cursor.push('\n');
        } else if i == position.line as usize {
            content_until_cursor.push_str(&line[..position.character.min(line.len() as u32) as usize]);
            break;
        }
    }

    // Track all variant openings with their depth and name
    // We want to find the innermost unclosed variant by matching parens
    let mut variant_stack: Vec<String> = Vec::new();
    let mut in_string = false;
    let mut escape_next = false;

    let chars: Vec<char> = content_until_cursor.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];

        if escape_next {
            escape_next = false;
            i += 1;
            continue;
        }

        match ch {
            '\\' if in_string => escape_next = true,
            '"' => in_string = !in_string,
            '(' | '{' if !in_string => {
                // Look backwards from this position to find the variant name
                // Could be "Type::Variant(" or just "Variant("
                let mut j = i;
                // Skip whitespace backwards
                while j > 0 && chars[j - 1].is_whitespace() {
                    j -= 1;
                }

                if j > 0 {
                    // Extract the word before the whitespace/paren
                    let word_end = j;
                    let mut word_start = j;

                    // Go back to find the start of the word
                    while word_start > 0 {
                        let prev_ch = chars[word_start - 1];
                        if prev_ch.is_alphanumeric() || prev_ch == '_' {
                            word_start -= 1;
                        } else if prev_ch == ':' && word_start >= 2 && chars[word_start - 2] == ':' {
                            // Handle :: syntax, but skip it for the variant name
                            break;
                        } else {
                            break;
                        }
                    }

                    if word_start < word_end {
                        let word: String = chars[word_start..word_end].iter().collect();
                        // Only consider it a variant if it starts with uppercase
                        if word.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            variant_stack.push(word);
                        }
                    }
                }
            }
            ')' | '}' if !in_string => {
                if !variant_stack.is_empty() {
                    variant_stack.pop();
                }
            }
            _ => {}
        }

        i += 1;
    }

    // Return the last (innermost) variant on the stack
    variant_stack.last().cloned()
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
    fn test_find_nested_type_context() {
        let content = r#"PostReference(Post(
    id: 42,
    author: User(
        name: "Alice",
    ),
))"#;
        // Position inside User
        let contexts = find_type_context_at_position(content, Position::new(3, 20));
        assert_eq!(contexts.len(), 3);
        assert_eq!(contexts[0].type_name, "PostReference");
        assert_eq!(contexts[1].type_name, "Post");
        assert_eq!(contexts[2].type_name, "User");

        // Position inside Post but not User
        let contexts = find_type_context_at_position(content, Position::new(1, 10));
        assert_eq!(contexts.len(), 2);
        assert_eq!(contexts[0].type_name, "PostReference");
        assert_eq!(contexts[1].type_name, "Post");

        // Position at top level
        let contexts = find_type_context_at_position(content, Position::new(0, 5));
        assert_eq!(contexts.len(), 0);
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

    #[test]
    fn test_find_current_variant_context() {
        // RON uses parentheses for struct variants
        let content = "MyEnum::StructVariant( field_a: value )";
        let position = Position::new(0, 25); // Right after the opening paren

        let variant = find_current_variant_context(content, position);
        assert_eq!(variant, Some("StructVariant".to_string()));
    }

    #[test]
    fn test_find_current_variant_context_lowercase() {
        // Lowercase names (like functions) should not match
        let content = "my_function(\n    field_a: value\n)";
        let position = Position::new(1, 10);

        let variant = find_current_variant_context(content, position);
        assert!(variant.is_none(), "Lowercase names should not match as variants");
    }

    #[test]
    fn test_enum_variants_ron_actual_file() {
        // Load the ACTUAL file content from enum_variants.ron
        let content = r#"/* @[crate::models::Message] */
// Complex nested enum variant with Post containing User
PostReference(
    Post(
        id: 42,
        title: "Enum Variants in RON",
        content: "Shows tuple variants, struct variants, and nesting",
        author: User(
            id: 1,
            name: "Alice",
            email: "alice@example.com",
            age: 30,
            bio: Some(
                "Rust developer",
            ),
            is_active: true,
            roles: [
                "admin",
            ],
            invalid_field: "should error",
        ),
        likes: 100,
        tags: [
            "rust",
            "ron",
        ],
        published: true,
        post_type: Detailed(
            length: 1,
        ),
    )
)"#;

        // Position on line 29, on the word "length" (line 28 in 0-indexed)
        let position = Position::new(28, 16);

        // Test 1: Can we find the variant?
        let variant = find_current_variant_context(content, position);
        println!("Variant found: {:?}", variant);
        assert_eq!(variant, Some("Detailed".to_string()), "Should find Detailed variant");

        // Test 2: Can we find the containing field?
        let field = get_containing_field_context(content, position);
        println!("Containing field found: {:?}", field);
        assert_eq!(field, Some("post_type".to_string()), "Should find post_type as containing field");
    }
}
