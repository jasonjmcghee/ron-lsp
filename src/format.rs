/// Tree-sitter based RON formatter
/// This formatter uses the AST to properly handle formatting

use crate::{annotation_parser, ts_utils::{self, RonParser}};
use tree_sitter::Node;

/// Format RON content using tree-sitter AST
pub fn format_ron(content: &str) -> String {
    let indent_str = "    "; // 4 spaces

    // Build the formatted output
    let mut result = String::new();

    // Check for type annotation using the existing parser
    // (tree-sitter grammar has issues with /* @[...] */ vs block comments)
    if let Some(annotation) = annotation_parser::parse_type_annotation(content) {
        result.push_str("/* @[");
        result.push_str(&annotation);
        result.push_str("] */\n\n");
    }

    // Parse the RON content (without annotation) for formatting
    let ron_content = if content.trim_start().starts_with("/*") {
        // Skip past the type annotation
        if let Some(end_idx) = content.find("*/") {
            &content[end_idx + 2..]
        } else {
            content
        }
    } else {
        content
    };

    let mut parser = RonParser::new();
    let tree = match parser.parse(ron_content) {
        Some(t) => t,
        None => {
            // If parsing fails, return original content
            return content.to_string();
        }
    };

    // Format the main value
    if let Some(main_value) = ts_utils::find_main_value(&tree) {
        format_node(&main_value, ron_content, &mut result, 0, indent_str, false);
    }

    result.trim_end().to_string()
}

/// Format a single node recursively
fn format_node(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
    inline: bool,
) {
    let kind = node.kind();

    match kind {
        "struct" => format_struct(node, content, output, indent_level, indent_str, inline),
        "array" => format_array(node, content, output, indent_level, indent_str, inline),
        "map" => format_map(node, content, output, indent_level, indent_str, inline),
        "tuple" => format_tuple(node, content, output, indent_level, indent_str, inline),
        "field" => format_field(node, content, output, indent_level, indent_str),
        "string" | "integer" | "float" | "boolean" | "char" | "identifier" | "unit" => {
            // Leaf nodes - just output their text
            if let Some(text) = ts_utils::node_text(node, content) {
                output.push_str(text);
            }
        }
        _ => {
            // For other nodes, just output their text as-is
            if let Some(text) = ts_utils::node_text(node, content) {
                output.push_str(text);
            }
        }
    }
}

/// Format a struct node
fn format_struct(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
    _inline: bool,
) {
    // Get struct name if it exists
    if let Some(name) = ts_utils::struct_name(node, content) {
        output.push_str(name);
    }

    // Check if empty
    let is_empty = ts_utils::is_empty_structure(node, content);

    if is_empty {
        output.push_str("()");
        return;
    }

    output.push('(');

    // Get all fields and values to determine if this is a tuple-style or field-style struct
    let fields = ts_utils::struct_fields(node);
    let values = ts_utils::struct_values(node, content);

    // If we have fields, use field formatting
    if !fields.is_empty() {
        output.push('\n');

        for (i, field) in fields.iter().enumerate() {
            output.push_str(&indent_str.repeat(indent_level + 1));
            format_field(field, content, output, indent_level + 1, indent_str);

            // Add comma after each field
            if i < fields.len() - 1 {
                output.push(',');
            } else {
                // Optional trailing comma on last field
                output.push(',');
            }
            output.push('\n');
        }

        output.push_str(&indent_str.repeat(indent_level));
    } else if !values.is_empty() {
        // Tuple-style struct like Some("value") - these should stay inline if single element
        let should_inline = values.len() == 1;

        if !should_inline {
            output.push('\n');
        }

        for (i, child) in values.iter().enumerate() {
            if !should_inline {
                output.push_str(&indent_str.repeat(indent_level + 1));
            }
            format_node(child, content, output, indent_level + 1, indent_str, should_inline);

            if i < values.len() - 1 {
                output.push(',');
                if !should_inline {
                    output.push('\n');
                } else {
                    output.push(' ');
                }
            }
        }

        if !should_inline {
            output.push('\n');
            output.push_str(&indent_str.repeat(indent_level));
        }
    }

    output.push(')');
}

/// Format a field node
fn format_field(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
) {
    // Get field name
    if let Some(name) = ts_utils::field_name(node, content) {
        output.push_str(name);
        output.push_str(": ");
    }

    // Get field value
    if let Some(value) = ts_utils::field_value(node) {
        format_node(&value, content, output, indent_level, indent_str, false);
    }
}

/// Format an array node
fn format_array(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
    _inline: bool,
) {
    let is_empty = ts_utils::is_empty_structure(node, content);

    if is_empty {
        output.push_str("[]");
        return;
    }

    output.push('[');

    // Get all array elements (named children that aren't punctuation)
    let elements = ts_utils::named_children(node);

    if !elements.is_empty() {
        output.push('\n');

        for (i, element) in elements.iter().enumerate() {
            output.push_str(&indent_str.repeat(indent_level + 1));
            format_node(element, content, output, indent_level + 1, indent_str, false);

            if i < elements.len() - 1 {
                output.push(',');
            } else {
                output.push(',');
            }
            output.push('\n');
        }

        output.push_str(&indent_str.repeat(indent_level));
    }

    output.push(']');
}

/// Format a map node
fn format_map(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
    _inline: bool,
) {
    let is_empty = ts_utils::is_empty_structure(node, content);

    if is_empty {
        output.push_str("{}");
        return;
    }

    output.push('{');

    // Get all map entries
    let entries = ts_utils::children_by_kind(node, "map_entry");

    if !entries.is_empty() {
        output.push('\n');

        for (i, entry) in entries.iter().enumerate() {
            output.push_str(&indent_str.repeat(indent_level + 1));

            // Format map entry (key: value)
            let children = ts_utils::named_children(entry);
            if children.len() >= 2 {
                // Key
                format_node(&children[0], content, output, indent_level + 1, indent_str, false);
                output.push_str(": ");
                // Value
                format_node(&children[1], content, output, indent_level + 1, indent_str, false);
            }

            if i < entries.len() - 1 {
                output.push(',');
            } else {
                output.push(',');
            }
            output.push('\n');
        }

        output.push_str(&indent_str.repeat(indent_level));
    }

    output.push('}');
}

/// Format a tuple node
fn format_tuple(
    node: &Node,
    content: &str,
    output: &mut String,
    indent_level: usize,
    indent_str: &str,
    _inline: bool,
) {
    output.push('(');

    let elements = ts_utils::named_children(node);

    if !elements.is_empty() {
        output.push('\n');

        for (i, element) in elements.iter().enumerate() {
            output.push_str(&indent_str.repeat(indent_level + 1));
            format_node(element, content, output, indent_level + 1, indent_str, false);

            if i < elements.len() - 1 {
                output.push(',');
            } else {
                output.push(',');
            }
            output.push('\n');
        }

        output.push_str(&indent_str.repeat(indent_level));
    }

    output.push(')');
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
        assert_eq!(formatted, "Unit()");
    }

    #[test]
    fn test_nested_struct() {
        let input = "Post(author: User(id: 1))";
        let formatted = format_ron(input);
        println!("Formatted:\n{}", formatted);
        assert!(formatted.contains("Post("));
        assert!(formatted.contains("    author: User("));
        assert!(formatted.contains("        id: 1,"));
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
        assert!(formatted.contains("    ],") || formatted.contains("    ]\n)"));
    }

    #[test]
    fn test_with_type_annotation() {
        let input = "/* @[crate::User] */\nUser(id: 1)";
        let formatted = format_ron(input);
        println!("Formatted:\n{}", formatted);
        assert!(formatted.contains("/* @[crate::User] */"));
        assert!(formatted.contains("User("));
        assert!(formatted.contains("    id: 1,"));
    }
}
