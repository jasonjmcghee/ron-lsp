/// Simple RON formatter that handles:
/// - Indentation based on (), [], {} nesting
/// - Whitespace normalization
/// - Comma placement
/// - Preserves comments
/// - Always multi-line for non-empty structures
pub fn format_ron(content: &str) -> String {
    // Extract type annotation if present
    let has_annotation = content.trim_start().starts_with("/*");
    let (annotation, ron_content) = if has_annotation {
        if let Some(end_idx) = content.find("*/") {
            let annotation = &content[..end_idx + 2];
            let rest = &content[end_idx + 2..];
            (Some(annotation.trim()), rest)
        } else {
            (None, content)
        }
    } else {
        (None, content)
    };

    let mut result = String::new();
    let mut indent_level = 0;
    let indent_str = "    "; // 4 spaces
    let mut chars = ron_content.chars().peekable();
    let mut in_string = false;
    let mut escape_next = false;
    let mut in_comment = false;
    let mut current_line = String::new();
    let mut at_line_start = true;
    let mut bracket_depth: usize = 0; // Track nesting depth to know if we're in multi-line mode

    while let Some(ch) = chars.next() {
        // Handle line comments
        if !in_string && ch == '/' && chars.peek() == Some(&'/') {
            if !current_line.trim().is_empty() {
                result.push_str(&indent_str.repeat(indent_level));
                result.push_str(current_line.trim());
                result.push('\n');
                current_line.clear();
            }

            // Add blank line before comment if it's on its own line
            // (at_line_start means we haven't accumulated content on this line yet)
            // But don't add if it's right after an opening bracket (first field)
            // or if the previous line was already a comment
            let prev_line_was_comment = result
                .trim_end()
                .lines()
                .last()
                .map(|line| line.trim_start().starts_with("//"))
                .unwrap_or(false);

            if at_line_start
                && !result.is_empty()
                && !result.ends_with("\n\n")
                && !result.ends_with("(\n")
                && !result.ends_with("[\n")
                && !result.ends_with("{\n")
                && !prev_line_was_comment
            {
                result.push('\n');
            }

            current_line.push(ch);
            in_comment = true;
            continue;
        }

        if in_comment {
            current_line.push(ch);
            if ch == '\n' {
                result.push_str(&indent_str.repeat(indent_level));
                result.push_str(current_line.trim());
                result.push('\n');
                current_line.clear();
                in_comment = false;
                at_line_start = true;
            }
            continue;
        }

        // Handle strings
        if in_string {
            current_line.push(ch);
            if escape_next {
                escape_next = false;
            } else if ch == '\\' {
                escape_next = true;
            } else if ch == '"' {
                in_string = false;
            }
            continue;
        }

        match ch {
            '"' => {
                in_string = true;
                current_line.push(ch);
            }
            '(' | '[' | '{' => {
                current_line.push(ch);

                // Check if empty (e.g., "()", "[]")
                let mut peek_chars = chars.clone();
                let mut is_empty = true;
                while let Some(&peek_ch) = peek_chars.peek() {
                    if peek_ch.is_whitespace() {
                        peek_chars.next();
                    } else {
                        is_empty = peek_ch == ')' || peek_ch == ']' || peek_ch == '}';
                        break;
                    }
                }

                if !is_empty {
                    // Multi-line: write current line and indent
                    result.push_str(&indent_str.repeat(indent_level));
                    result.push_str(current_line.trim());
                    result.push('\n');
                    current_line.clear();
                    indent_level += 1;
                    bracket_depth += 1;
                    at_line_start = true;
                }
                // If empty, stay on same line - current_line still has the opening bracket
            }
            ')' | ']' | '}' => {
                // Check if we're in multi-line mode (bracket_depth > 0)
                let is_multi_line = bracket_depth > 0;

                if !is_multi_line {
                    // Single-line mode (empty parens): just append closing bracket
                    current_line.push(ch);
                } else {
                    // Multi-line mode: flush any content, dedent, and write closing bracket
                    if !current_line.trim().is_empty() {
                        let line = current_line.trim();
                        result.push_str(&indent_str.repeat(indent_level));
                        result.push_str(line);
                        // Add trailing comma if not already there
                        if !line.ends_with(',') {
                            result.push(',');
                        }
                        result.push('\n');
                        current_line.clear();
                    }

                    // Dedent and write closing bracket
                    indent_level = indent_level.saturating_sub(1);
                    bracket_depth = bracket_depth.saturating_sub(1);
                    result.push_str(&indent_str.repeat(indent_level));
                    result.push(ch);

                    // Check if there's a trailing comma after the closing bracket in input
                    let mut peek_chars = chars.clone();
                    let mut found_comma = false;
                    while let Some(&peek_ch) = peek_chars.peek() {
                        if peek_ch == ',' {
                            found_comma = true;
                            break;
                        } else if !peek_ch.is_whitespace() {
                            break;
                        }
                        peek_chars.next();
                    }

                    if found_comma {
                        // Consume whitespace and comma from input
                        while let Some(&peek_ch) = chars.peek() {
                            if peek_ch == ',' {
                                chars.next();
                                result.push(',');
                                break;
                            } else if peek_ch.is_whitespace() {
                                chars.next();
                            } else {
                                break;
                            }
                        }
                    } else if bracket_depth > 0 {
                        // Still nested - check if next non-whitespace is closing bracket
                        let mut peek_chars2 = chars.clone();
                        let mut next_is_closing = false;
                        while let Some(&peek_ch) = peek_chars2.peek() {
                            if peek_ch.is_whitespace() {
                                peek_chars2.next();
                            } else {
                                next_is_closing =
                                    peek_ch == ')' || peek_ch == ']' || peek_ch == '}';
                                break;
                            }
                        }

                        // Only add trailing comma if NOT followed by closing bracket
                        // This avoids "Foo(Bar(...),)" syntax which is invalid in RON
                        if !next_is_closing {
                            result.push(',');
                        }
                    }

                    result.push('\n');
                    at_line_start = true;
                }
            }
            ',' => {
                current_line.push(ch);
                result.push_str(&indent_str.repeat(indent_level));
                result.push_str(current_line.trim());
                result.push('\n');
                current_line.clear();
                at_line_start = true;
            }
            '\n' | '\r' => {
                // Skip newlines, we'll add our own
                if !current_line.trim().is_empty() && !at_line_start {
                    current_line.push(' ');
                }
            }
            c if c.is_whitespace() => {
                if !current_line.trim().is_empty() && !current_line.ends_with(' ') {
                    current_line.push(' ');
                }
            }
            _ => {
                current_line.push(ch);
                at_line_start = false;
            }
        }
    }

    // Flush any remaining content
    if !current_line.trim().is_empty() {
        result.push_str(&indent_str.repeat(indent_level));
        result.push_str(current_line.trim());
        result.push('\n');
    }

    let formatted = result.trim_end().to_string();

    // Add back annotation if present
    if let Some(ann) = annotation {
        format!("{}\n{}", ann, formatted)
    } else {
        formatted
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_struct() {
        let input = "User(id: 1, name: \"Alice\")";
        let formatted = format_ron(input);
        println!("Formatted:\n{}", formatted);
        assert!(formatted.contains("User("));
        assert!(formatted.contains("    id: 1,"));
        assert!(formatted.contains("    name: \"Alice\","));
        assert!(formatted.contains(")"));
    }

    #[test]
    fn test_empty_parens() {
        let input = "Unit()";
        let formatted = format_ron(input);
        println!("Formatted: '{}'", formatted);
        // Empty parens stay on same line
        assert!(formatted.starts_with("Unit()"));
    }

    #[test]
    fn test_nested_struct() {
        let input = "Post(author: User(id: 1))";
        let formatted = format_ron(input);
        println!("Formatted:\n{}", formatted);
        assert!(formatted.contains("Post("));
        assert!(formatted.contains("    author: User("));
        assert!(formatted.contains("        id: 1,"));
        // Conservative: no trailing comma when followed by closing bracket (avoids invalid syntax)
        assert!(formatted.contains("    )"));
        assert!(formatted.trim().ends_with(")"));
    }

    #[test]
    fn test_array() {
        let input = r#"Config(roles: ["admin", "user"])"#;
        let formatted = format_ron(input);
        println!("Formatted:\n{}", formatted);
        assert!(formatted.contains("roles: ["));
        assert!(formatted.contains(r#"        "admin","#));
        assert!(formatted.contains(r#"        "user","#));
        // Array closing bracket should be followed by comma on next line
        assert!(formatted.contains("    ],") || formatted.contains("    ]\n)"));
    }
}
