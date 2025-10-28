/// Tree-sitter utilities for RON LSP
/// This module provides high-level helpers for working with tree-sitter AST nodes

use tower_lsp::lsp_types::{Position, Range};
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

impl Default for RonParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert LSP Position to byte offset in content
pub fn position_to_byte_offset(content: &str, position: Position) -> usize {
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

/// Convert tree-sitter byte range to LSP Range
pub fn node_to_lsp_range(node: &Node) -> Range {
    let start_pos = node.start_position();
    let end_pos = node.end_position();

    Range {
        start: Position {
            line: start_pos.row as u32,
            character: start_pos.column as u32,
        },
        end: Position {
            line: end_pos.row as u32,
            character: end_pos.column as u32,
        },
    }
}

/// Convert tree-sitter Point to LSP Position
pub fn point_to_position(point: tree_sitter::Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

/// Get the text content of a node
pub fn node_text<'a>(node: &Node, content: &'a str) -> Option<&'a str> {
    node.utf8_text(content.as_bytes()).ok()
}

/// Find the deepest node at a given position
pub fn node_at_position<'a>(tree: &'a Tree, content: &str, position: Position) -> Option<Node<'a>> {
    let byte_offset = position_to_byte_offset(content, position);
    let root = tree.root_node();
    root.descendant_for_byte_range(byte_offset, byte_offset)
}

/// Find the first ancestor of a node with a given kind
pub fn find_ancestor_by_kind<'a>(mut node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    while let Some(parent) = node.parent() {
        if parent.kind() == kind {
            return Some(parent);
        }
        node = parent;
    }
    None
}

/// Find the first ancestor of a node matching any of the given kinds
pub fn find_ancestor_by_kinds<'a>(mut node: Node<'a>, kinds: &[&str]) -> Option<Node<'a>> {
    while let Some(parent) = node.parent() {
        if kinds.contains(&parent.kind()) {
            return Some(parent);
        }
        node = parent;
    }
    None
}

/// Get all children of a node with a specific kind
pub fn children_by_kind<'a>(node: &Node<'a>, kind: &str) -> Vec<Node<'a>> {
    let mut results = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == kind {
            results.push(child);
        }
    }
    results
}

/// Get the first child of a node with a specific kind
pub fn child_by_kind<'a>(node: &Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut cursor = node.walk();
    let result = node.children(&mut cursor).find(|child| child.kind() == kind);
    result
}

/// Get all named children of a node
pub fn named_children<'a>(node: &Node<'a>) -> Vec<Node<'a>> {
    let mut results = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        results.push(child);
    }
    results
}

/// Check if a node is of a specific kind
pub fn is_kind(node: &Node, kind: &str) -> bool {
    node.kind() == kind
}

/// Get the struct/variant name from a struct node
/// Returns None if it's an anonymous struct (no identifier child)
pub fn struct_name<'a>(node: &Node, content: &'a str) -> Option<&'a str> {
    if node.kind() != "struct" {
        return None;
    }

    // First child should be identifier if it's a named struct
    if let Some(first_child) = node.child(0) {
        if first_child.kind() == "identifier" {
            return node_text(&first_child, content);
        }
    }
    None
}

/// Get the field name from a field node
pub fn field_name<'a>(node: &Node, content: &'a str) -> Option<&'a str> {
    if node.kind() != "field" {
        return None;
    }

    // First child should be the identifier
    if let Some(first_child) = node.child(0) {
        if first_child.kind() == "identifier" {
            return node_text(&first_child, content);
        }
    }
    None
}

/// Get the value node from a field node
pub fn field_value<'a>(node: &Node<'a>) -> Option<Node<'a>> {
    if node.kind() != "field" {
        return None;
    }

    // Field structure: identifier ":" value
    // So value is typically the 3rd child (after identifier and colon)
    let mut cursor = node.walk();
    let children: Vec<_> = node.children(&mut cursor).collect();

    // Find the colon, then return the next node
    for (i, child) in children.iter().enumerate() {
        if child.kind() == ":" && i + 1 < children.len() {
            return Some(children[i + 1]);
        }
    }

    // Fallback: last child that's not a comma
    children.iter()
        .rev()
        .find(|c| c.is_named())
        .copied()
}

/// Find all struct nodes in the tree
pub fn find_all_structs(tree: &Tree) -> Vec<Node> {
    let mut structs = Vec::new();
    let root = tree.root_node();
    collect_nodes_by_kind(&root, "struct", &mut structs);
    structs
}

/// Recursively collect all nodes of a specific kind
fn collect_nodes_by_kind<'a>(node: &Node<'a>, kind: &str, results: &mut Vec<Node<'a>>) {
    if node.kind() == kind {
        results.push(*node);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_nodes_by_kind(&child, kind, results);
    }
}

/// Find all fields in a struct node (non-recursive, only direct children)
pub fn struct_fields<'a>(node: &Node<'a>) -> Vec<Node<'a>> {
    if node.kind() != "struct" {
        return Vec::new();
    }

    children_by_kind(node, "field")
}

/// Get all value children of a struct (for tuple-style structs), excluding the struct name identifier
pub fn struct_values<'a>(node: &Node<'a>, content: &str) -> Vec<Node<'a>> {
    if node.kind() != "struct" {
        return Vec::new();
    }

    let mut values = Vec::new();
    let mut cursor = node.walk();
    let has_name = struct_name(node, content).is_some();
    let mut skip_first_identifier = has_name;

    for child in node.children(&mut cursor) {
        // Skip the first identifier (struct name) and punctuation
        if child.is_named() && child.kind() != "field" {
            if skip_first_identifier && child.kind() == "identifier" {
                skip_first_identifier = false;
                continue;
            }
            values.push(child);
        }
    }

    values
}

/// Get the type annotation node from the tree
pub fn find_type_annotation(tree: &Tree) -> Option<Node> {
    let root = tree.root_node();
    child_by_kind(&root, "type_annotation")
}

/// Extract the type path from a type annotation node
pub fn type_annotation_path<'a>(node: &Node, content: &'a str) -> Option<String> {
    if node.kind() != "type_annotation" {
        return None;
    }

    // Find the type_path child
    if let Some(type_path) = child_by_kind(node, "type_path") {
        return node_text(&type_path, content).map(|s| s.to_string());
    }

    None
}

/// Find the main value node (the actual RON data, skipping annotation and comments)
pub fn find_main_value(tree: &Tree) -> Option<Node> {
    let root = tree.root_node();
    let mut cursor = root.walk();

    // Skip type_annotation, extensions, and comments - find the first actual value node
    for child in root.children(&mut cursor) {
        if child.is_named() {
            let kind = child.kind();
            if kind != "type_annotation"
                && kind != "extensions"
                && kind != "extension"
                && kind != "line_comment"
                && kind != "block_comment" {
                return Some(child);
            }
        }
    }

    None
}

/// Check if a node represents an empty structure (), [], or {}
pub fn is_empty_structure(node: &Node, _content: &str) -> bool {
    match node.kind() {
        "struct" | "array" | "map" | "tuple" => {
            // Check if it has any named children (fields, values, etc.)
            let mut cursor = node.walk();
            let has_children = node.named_children(&mut cursor).next().is_some();
            !has_children
        }
        _ => false
    }
}

/// Get the depth/nesting level of a node
pub fn node_depth(node: &Node) -> usize {
    let mut depth = 0;
    let mut current = *node;
    while let Some(parent) = current.parent() {
        depth += 1;
        current = parent;
    }
    depth
}

/// Check if position is inside a string node
pub fn is_inside_string(tree: &Tree, content: &str, position: Position) -> bool {
    if let Some(node) = node_at_position(tree, content, position) {
        let mut current = node;
        loop {
            if current.kind() == "string" {
                return true;
            }
            match current.parent() {
                Some(parent) => current = parent,
                None => break,
            }
        }
    }
    false
}

/// Check if position is inside a comment
pub fn is_inside_comment(tree: &Tree, content: &str, position: Position) -> bool {
    if let Some(node) = node_at_position(tree, content, position) {
        let mut current = node;
        loop {
            let kind = current.kind();
            if kind == "line_comment" || kind == "block_comment" {
                return true;
            }
            match current.parent() {
                Some(parent) => current = parent,
                None => break,
            }
        }
    }
    false
}

/// Information about a parsed enum variant
#[derive(Debug, Clone)]
pub struct ParsedEnumVariant {
    pub name: String,
    pub data: Option<String>,
    pub line: u32,
    pub col: u32,
}

/// Extract enum variant information from a struct node
/// Handles: UnitVariant, TupleVariant(data), StructVariant { fields }
pub fn extract_enum_variant(node: &Node, content: &str) -> Option<ParsedEnumVariant> {
    if node.kind() != "struct" && node.kind() != "identifier" {
        return None;
    }

    // Get variant name
    let name = if node.kind() == "identifier" {
        node_text(node, content)?.to_string()
    } else {
        struct_name(node, content)?.to_string()
    };

    let pos = node.start_position();
    let line = pos.row as u32;
    let col = pos.column as u32;

    // Get data if it's a struct variant
    let data = if node.kind() == "struct" {
        let fields = struct_fields(node);
        let values = struct_values(node, content);

        if !fields.is_empty() || !values.is_empty() {
            // Extract the content between parens
            let start = node.start_byte();
            let end = node.end_byte();
            let full_text = &content.as_bytes()[start..end];
            let text = std::str::from_utf8(full_text).ok()?;

            // Find content between first ( and last )
            if let Some(paren_start) = text.find('(') {
                if let Some(paren_end) = text.rfind(')') {
                    let inner = &text[paren_start + 1..paren_end];
                    Some(inner.to_string())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    Some(ParsedEnumVariant {
        name,
        data,
        line,
        col,
    })
}

/// Find all identifier nodes that could be enum variants (uppercase identifiers not in field position)
pub fn find_potential_variants<'a>(tree: &'a Tree, content: &str) -> Vec<Node<'a>> {
    let mut variants = Vec::new();
    let root = tree.root_node();
    collect_potential_variants(&root, content, &mut variants);
    variants
}

fn collect_potential_variants<'a>(node: &Node<'a>, content: &str, results: &mut Vec<Node<'a>>) {
    if node.kind() == "identifier" {
        if let Some(text) = node_text(node, content) {
            if text.chars().next().map_or(false, |c| c.is_uppercase()) {
                // Check if this is not a field name (field names are first child of field nodes)
                let is_field_name = node.parent()
                    .map(|p| p.kind() == "field" && p.child(0).map(|c| c.id()) == Some(node.id()))
                    .unwrap_or(false);

                if !is_field_name {
                    results.push(*node);
                }
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_potential_variants(&child, content, results);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_struct() {
        let mut parser = RonParser::new();
        let tree = parser.parse("User(id: 1, name: \"test\")");
        assert!(tree.is_some());
    }

    #[test]
    fn test_struct_name() {
        let mut parser = RonParser::new();
        let tree = parser.parse("User(id: 1)").unwrap();
        let root = tree.root_node();

        if let Some(struct_node) = child_by_kind(&root, "struct") {
            let name = struct_name(&struct_node, "User(id: 1)");
            assert_eq!(name, Some("User"));
        } else {
            panic!("No struct node found");
        }
    }

    #[test]
    fn test_struct_fields() {
        let mut parser = RonParser::new();
        let tree = parser.parse("User(id: 1, name: \"test\")").unwrap();
        let root = tree.root_node();

        if let Some(struct_node) = child_by_kind(&root, "struct") {
            let fields = struct_fields(&struct_node);
            assert_eq!(fields.len(), 2);
        }
    }

    #[test]
    fn test_field_name() {
        let content = "User(id: 1)";
        let mut parser = RonParser::new();
        let tree = parser.parse(content).unwrap();
        let root = tree.root_node();

        if let Some(struct_node) = child_by_kind(&root, "struct") {
            let fields = struct_fields(&struct_node);
            if let Some(field) = fields.first() {
                let name = field_name(field, content);
                assert_eq!(name, Some("id"));
            }
        }
    }

    #[test]
    fn test_struct_name_extraction() {
        let content = "User(id: 1)";
        let mut parser = RonParser::new();
        let tree = parser.parse(content).unwrap();

        if let Some(main_value) = find_main_value(&tree) {
            assert_eq!(main_value.kind(), "struct");
            if let Some(name) = struct_name(&main_value, content) {
                assert_eq!(name, "User");
            }
        } else {
            panic!("No main value found");
        }
    }

    #[test]
    fn test_empty_structure() {
        let mut parser = RonParser::new();
        let content = "Unit()";
        let tree = parser.parse(content).unwrap();

        if let Some(value) = find_main_value(&tree) {
            assert_eq!(value.kind(), "struct");
            let values = struct_values(&value, content);
            let fields = struct_fields(&value);
            assert_eq!(values.len(), 0);
            assert_eq!(fields.len(), 0);
        }
    }

    #[test]
    fn test_position_to_byte_offset() {
        let content = "abc\ndef";
        let offset = position_to_byte_offset(content, Position::new(1, 1));
        assert_eq!(offset, 5); // After "abc\nd"
    }

    #[test]
    fn test_field_value_extraction_nested() {
        let content = "Post(author: User(id: 1, name: \"Alice\"))";
        let mut parser = RonParser::new();
        let tree = parser.parse(content).unwrap();

        if let Some(main) = find_main_value(&tree) {
            let fields = struct_fields(&main);
            if let Some(author_field) = fields.first() {
                if let Some(value) = field_value(author_field) {
                    let value_text = node_text(&value, content).unwrap();
                    println!("Author field value: {}", value_text);
                    assert!(value_text.contains("User("));
                    assert!(value_text.contains("id: 1"));
                    assert!(value_text.contains("name: \"Alice\""));
                }
            }
        }
    }

    #[test]
    fn test_parse_double_colon_syntax() {
        let content = "MyEnum::StructVariant(\n    field_a: \"test\"\n)";
        let mut parser = RonParser::new();
        let tree = parser.parse(content);

        if let Some(tree) = tree {
            println!("Parsed successfully");
            let root = tree.root_node();
            println!("Root sexp: {}", root.to_sexp());

            if let Some(main) = find_main_value(&tree) {
                println!("Main value kind: {}", main.kind());
                println!("Main value child count: {}", main.child_count());

                let mut cursor = main.walk();
                for child in main.children(&mut cursor) {
                    println!("  Child: kind='{}' is_named={}", child.kind(), child.is_named());
                }

                if let Some(name) = struct_name(&main, content) {
                    println!("Struct name: {}", name);
                }
            }
        } else {
            panic!("Failed to parse");
        }
    }
}
