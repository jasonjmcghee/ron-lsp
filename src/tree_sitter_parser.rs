use tower_lsp::lsp_types::Position;
use tree_sitter::{Node, Parser, Tree};

/// Wrapper around tree-sitter parser for RON files
pub struct RonParser {
    parser: Parser,
}

impl RonParser {
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_ron::language())
            .expect("Error loading RON language");
        Self { parser }
    }

    /// Parse RON content and return the tree
    pub fn parse(&mut self, content: &str) -> Option<Tree> {
        self.parser.parse(content, None)
    }
}

/// Represents the nesting context at a cursor position
#[derive(Debug, Clone)]
pub struct TypeContext {
    /// The type name we're currently inside (e.g., "User", "Post")
    pub type_name: String,
    /// The line where this type context starts
    pub start_line: usize,
}

/// Find the nested type context at a cursor position using tree-sitter
/// Returns a stack of type contexts from outermost to innermost
pub fn find_type_context_at_position(content: &str, position: Position) -> Vec<TypeContext> {
    let mut parser = RonParser::new();
    let tree = match parser.parse(content) {
        Some(t) => t,
        None => return Vec::new(),
    };

    let root = tree.root_node();
    let byte_offset = position_to_byte_offset(content, position);

    // Find the node at the cursor position
    let node = root.descendant_for_byte_range(byte_offset, byte_offset);

    let mut contexts = Vec::new();
    if let Some(mut current) = node {
        // Walk up the tree to collect all struct contexts
        loop {
            if current.kind() == "struct" {
                if let Some(type_name) = extract_struct_name(&current, content) {
                    let start_line = current.start_position().row;
                    contexts.push(TypeContext {
                        type_name,
                        start_line,
                    });
                }
            }

            match current.parent() {
                Some(parent) => current = parent,
                None => break,
            }
        }
    }

    // Reverse to get outermost to innermost
    contexts.reverse();
    contexts
}

/// Extract the struct/variant name from a struct node
fn extract_struct_name(node: &Node, content: &str) -> Option<String> {
    // A struct node's first child is typically the identifier (name)
    if let Some(first_child) = node.child(0) {
        if first_child.kind() == "identifier" {
            let name = first_child.utf8_text(content.as_bytes()).ok()?;
            return Some(name.to_string());
        }
    }
    None
}

/// Get the field name at a specific position in RON content using tree-sitter
pub fn get_field_at_position(content: &str, position: Position) -> Option<String> {
    let mut parser = RonParser::new();
    let tree = parser.parse(content)?;
    let root = tree.root_node();
    let byte_offset = position_to_byte_offset(content, position);

    // Find the node at the cursor position
    let node = root.descendant_for_byte_range(byte_offset, byte_offset)?;

    // Walk up to find a field node
    let mut current = node;
    loop {
        if current.kind() == "field" {
            // The first child of a field node is the identifier (field name)
            if let Some(field_name_node) = current.child(0) {
                if field_name_node.kind() == "identifier" {
                    let field_name = field_name_node.utf8_text(content.as_bytes()).ok()?;
                    return Some(field_name.to_string());
                }
            }
        }

        match current.parent() {
            Some(parent) => current = parent,
            None => break,
        }
    }

    None
}

/// Find the current variant context (enum variant name) at a position
pub fn find_current_variant_context(content: &str, position: Position) -> Option<String> {
    let mut parser = RonParser::new();
    let tree = parser.parse(content)?;
    let root = tree.root_node();
    let byte_offset = position_to_byte_offset(content, position);

    // Find the node at the cursor position
    let node = root.descendant_for_byte_range(byte_offset, byte_offset)?;

    // Walk up to find the innermost struct node with a name
    let mut current = node;
    loop {
        if current.kind() == "struct" {
            // Check if this struct has a name (making it a variant)
            if let Some(name) = extract_struct_name(&current, content) {
                // Make sure it's actually a variant by checking if it's uppercase
                if name.chars().next()?.is_uppercase() {
                    return Some(name);
                }
            }
        }

        match current.parent() {
            Some(parent) => current = parent,
            None => break,
        }
    }

    None
}

/// Get the containing field context by finding the parent field
/// For example: "post_type: Detailed(\n    length: 1" - when on "length" line, returns "post_type"
pub fn get_containing_field_context(content: &str, position: Position) -> Option<String> {
    let mut parser = RonParser::new();
    let tree = parser.parse(content)?;
    let root = tree.root_node();
    let byte_offset = position_to_byte_offset(content, position);

    // Find the node at the cursor position
    let node = root.descendant_for_byte_range(byte_offset, byte_offset)?;

    // Walk up the tree to find field nodes
    // We want to find the parent field that contains a struct which contains our current field
    let mut current = node;
    let mut found_current_field = false;

    loop {
        if current.kind() == "field" {
            if found_current_field {
                // This is the containing field - extract its name
                if let Some(field_name_node) = current.child(0) {
                    if field_name_node.kind() == "identifier" {
                        if let Ok(field_name) = field_name_node.utf8_text(content.as_bytes()) {
                            return Some(field_name.to_string());
                        }
                    }
                }
            } else {
                // This is the first field we found (the one we're in)
                found_current_field = true;
            }
        }

        match current.parent() {
            Some(parent) => current = parent,
            None => break,
        }
    }

    None
}

/// Helper to convert LSP Position to byte offset in content
fn position_to_byte_offset(content: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut current_line = 0;
    let mut current_col = 0;

    for ch in content.chars() {
        if current_line == position.line as usize && current_col == position.character as usize {
            return offset;
        }

        if ch == '\n' {
            current_line += 1;
            current_col = 0;
        } else {
            current_col += 1;
        }

        offset += ch.len_utf8();
    }

    offset
}

/// Information about a variant field location in RON content
#[derive(Debug, Clone)]
pub struct VariantFieldLocation {
    pub line_idx: usize,
    pub variant_name: String,
    pub containing_field_name: String,
    pub field_at_position: Option<String>,
}

/// Scan through content and find all variant field locations
pub fn find_all_variant_field_locations(content: &str) -> Vec<VariantFieldLocation> {
    let mut parser = RonParser::new();
    let tree = match parser.parse(content) {
        Some(t) => t,
        None => return Vec::new(),
    };

    let mut locations = Vec::new();
    let root = tree.root_node();

    // Walk the tree to find all field nodes that are inside struct variants
    visit_fields(&root, content, &mut locations);

    locations
}

/// Recursively visit nodes to find field locations inside variants
fn visit_fields(node: &Node, content: &str, locations: &mut Vec<VariantFieldLocation>) {
    // Check if this is a struct (potential variant)
    if node.kind() == "struct" {
        if let Some(variant_name) = extract_struct_name(node, content) {
            // This is a named struct, check if it's a variant (uppercase start)
            if variant_name.chars().next().map_or(false, |c| c.is_uppercase()) {
                // Look for the containing field by checking parent
                let containing_field_name = find_parent_field_name(node, content);

                // Now collect all fields inside this variant
                collect_fields_in_node(node, content, &variant_name, &containing_field_name, locations);
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        visit_fields(&child, content, locations);
    }
}

/// Find the parent field name of a node
fn find_parent_field_name(node: &Node, content: &str) -> Option<String> {
    let mut current = *node;
    loop {
        match current.parent() {
            Some(parent) => {
                if parent.kind() == "field" {
                    if let Some(field_name_node) = parent.child(0) {
                        if field_name_node.kind() == "identifier" {
                            if let Ok(name) = field_name_node.utf8_text(content.as_bytes()) {
                                return Some(name.to_string());
                            }
                        }
                    }
                }
                current = parent;
            }
            None => break,
        }
    }
    None
}

/// Collect all fields in a node
fn collect_fields_in_node(
    node: &Node,
    content: &str,
    variant_name: &str,
    containing_field_name: &Option<String>,
    locations: &mut Vec<VariantFieldLocation>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "field" {
            let line_idx = child.start_position().row;

            // Extract field name
            let field_at_position = if let Some(field_name_node) = child.child(0) {
                if field_name_node.kind() == "identifier" {
                    field_name_node.utf8_text(content.as_bytes()).ok()
                        .map(|s| s.to_string())
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(containing_field) = containing_field_name {
                locations.push(VariantFieldLocation {
                    line_idx,
                    variant_name: variant_name.to_string(),
                    containing_field_name: containing_field.clone(),
                    field_at_position,
                });
            }
        }

        // Recurse for nested structures
        collect_fields_in_node(&child, content, variant_name, containing_field_name, locations);
    }
}

/// Parse RON structure to get all field names present at the top level
pub fn extract_fields_from_ron(content: &str) -> Vec<String> {
    let mut parser = RonParser::new();
    let tree = match parser.parse(content) {
        Some(t) => t,
        None => return Vec::new(),
    };

    let mut fields = std::collections::HashSet::new();
    let root = tree.root_node();

    // Find the first struct node (the top-level value)
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "struct" {
            // Collect only direct child fields of this struct
            collect_direct_field_names(&child, content, &mut fields);
            break;
        }
    }

    fields.into_iter().collect()
}

/// Collect only direct child field names from a struct node (not recursively)
fn collect_direct_field_names(node: &Node, content: &str, fields: &mut std::collections::HashSet<String>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "field" {
            if let Some(field_name_node) = child.child(0) {
                if field_name_node.kind() == "identifier" {
                    if let Ok(field_name) = field_name_node.utf8_text(content.as_bytes()) {
                        fields.insert(field_name.to_string());
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_struct() {
        let content = r#"MyStruct(
    name: "test",
    age: 30,
)"#;
        let mut parser = RonParser::new();
        let tree = parser.parse(content);
        assert!(tree.is_some());
    }

    #[test]
    fn test_find_type_context_nested() {
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
    }

    #[test]
    fn test_get_field_at_position() {
        let content = r#"MyStruct(
    name: "test",
    age: 30,
)"#;
        // Position on "name" field
        let field = get_field_at_position(content, Position::new(1, 8));
        assert_eq!(field, Some("name".to_string()));
    }

    #[test]
    fn test_find_current_variant_context() {
        let content = r#"Detailed(
    length: 1,
)"#;
        let variant = find_current_variant_context(content, Position::new(1, 12));
        assert_eq!(variant, Some("Detailed".to_string()));
    }

    #[test]
    fn test_extract_fields_from_ron() {
        let content = r#"MyStruct(
    name: "test",
    age: 30,
    items: [],
)"#;
        let fields = extract_fields_from_ron(content);
        assert!(fields.contains(&"name".to_string()));
        assert!(fields.contains(&"age".to_string()));
        assert!(fields.contains(&"items".to_string()));
    }

    #[test]
    fn test_enum_variant_field_detection() {
        let content = r#"Post(
    id: 42,
    post_type: Detailed(
        length: 1,
    ),
)"#;
        let position = Position::new(3, 16);

        let variant = find_current_variant_context(content, position);
        assert_eq!(variant, Some("Detailed".to_string()));

        let containing_field = get_containing_field_context(content, position);
        assert_eq!(containing_field, Some("post_type".to_string()));
    }
}
