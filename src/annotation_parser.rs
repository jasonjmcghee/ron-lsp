use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    // Matches: /* @[path::to::Type] */
    static ref TYPE_ANNOTATION_REGEX: Regex =
        Regex::new(r"/\*\s*@\[([^\]]+)\]\s*\*/").unwrap();
}

/// Parses the type annotation from a RON file
/// Looks for: /* @[crate::models::MyStruct] */
pub fn parse_type_annotation(content: &str) -> Option<String> {
    // Only check the first few lines for performance
    let first_lines: String = content.lines().collect::<Vec<_>>().join("\n");

    TYPE_ANNOTATION_REGEX
        .captures(&first_lines)
        .and_then(|cap| cap.get(1))
        .map(|m| m.as_str().trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_type_annotation() {
        let content = r#"/* @[crate::models::MyStruct] */

MyStruct(
    name: "test",
)
"#;
        assert_eq!(
            parse_type_annotation(content),
            Some("crate::models::MyStruct".to_string())
        );
    }

    #[test]
    fn test_parse_type_annotation_with_spaces() {
        let content = r#"/*  @[ crate::models::MyStruct ]  */

MyStruct(
    name: "test",
)
"#;
        assert_eq!(
            parse_type_annotation(content),
            Some("crate::models::MyStruct".to_string())
        );
    }

    #[test]
    fn test_no_annotation() {
        let content = r#"MyStruct(
    name: "test",
)
"#;
        assert_eq!(parse_type_annotation(content), None);
    }
}
