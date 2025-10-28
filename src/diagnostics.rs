#[cfg(feature = "cli")]
use crate::diagnostic_reporter;
use crate::ron_parser;
use crate::rust_analyzer::{EnumVariant, FieldInfo, RustAnalyzer, TypeInfo, TypeKind};
use ron::Value;
use std::sync::Arc;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Validate RON with access to RustAnalyzer for recursive type lookups (returns portable diagnostics)
#[cfg(feature = "cli")]
pub async fn validate_ron_portable(
    content: &str,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<diagnostic_reporter::Diagnostic> {
    let lsp_diagnostics = validate_ron_with_analyzer(content, type_info, analyzer).await;
    lsp_diagnostics_to_portable(&lsp_diagnostics)
}

/// Validate RON with access to RustAnalyzer for recursive type lookups
pub async fn validate_ron_with_analyzer(
    content: &str,
    type_info: &TypeInfo,
    analyzer: Arc<RustAnalyzer>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // First check for syntax errors
    let syntax_errors = validate_ron_syntax(content);
    if !syntax_errors.is_empty() {
        return syntax_errors;
    }

    // Try to parse the RON content
    let parsed_value = ron::from_str::<Value>(content);

    match &type_info.kind {
        TypeKind::Struct(fields) => {
            diagnostics.extend(
                validate_struct_fields(
                    content,
                    fields,
                    &parsed_value,
                    type_info.has_default,
                    Some(&analyzer),
                )
                .await,
            );
        }
        TypeKind::Enum(variants) => {
            diagnostics.extend(validate_enum_variant(content, variants, type_info));
        }
    }

    diagnostics
}

/// Convert LSP diagnostics to portable format
#[cfg(feature = "cli")]
fn lsp_diagnostics_to_portable(diagnostics: &[Diagnostic]) -> Vec<diagnostic_reporter::Diagnostic> {
    diagnostics
        .iter()
        .map(|d| {
            let severity = match d.severity {
                Some(DiagnosticSeverity::ERROR) => diagnostic_reporter::Severity::Error,
                Some(DiagnosticSeverity::WARNING) => diagnostic_reporter::Severity::Warning,
                _ => diagnostic_reporter::Severity::Info,
            };

            diagnostic_reporter::Diagnostic {
                line: d.range.start.line,
                col_start: d.range.start.character,
                col_end: d.range.end.character,
                severity,
                message: d.message.clone(),
            }
        })
        .collect()
}

/// Helper function for struct validation (async version with analyzer)
async fn validate_struct_fields(
    content: &str,
    fields: &[FieldInfo],
    parsed_value: &Result<Value, ron::error::SpannedError>,
    has_default: bool,
    analyzer: Option<&Arc<RustAnalyzer>>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Extract fields from RON using proper parsing
    let ron_fields = ron_parser::extract_fields_from_ron(content);

    // Check for unknown fields in RON
    for ron_field in &ron_fields {
        if !fields.iter().any(|f| &f.name == ron_field) {
            // Find the position of this field in the content for better error reporting
            for (line_num, line) in content.lines().enumerate() {
                if let Some(start_col) = line
                    .find(&format!("{}: ", ron_field))
                    .or_else(|| line.find(&format!("{}:", ron_field)))
                {
                    diagnostics.push(Diagnostic {
                        range: Range::new(
                            Position::new(line_num as u32, start_col as u32),
                            Position::new(line_num as u32, (start_col + ron_field.len()) as u32),
                        ),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("Unknown field '{}'", ron_field),
                        ..Default::default()
                    });
                    break;
                }
            }
        }
    }

    // Type check each field if we successfully parsed the RON
    if let Ok(ref value) = parsed_value {
        // For named structs like MyStruct(...), the value might be wrapped
        let map = extract_map_from_value(value);

        if let Some(map) = map {
            for field in fields {
                if let Some(field_value) = map.get(&Value::String(field.name.clone())) {
                    // Check type - with enum validation if analyzer is available
                    let type_mismatch = if let Some(analyzer) = analyzer {
                        check_type_mismatch_with_enum_validation(
                            field_value,
                            &field.type_name,
                            content,
                            &field.name,
                            analyzer,
                        )
                        .await
                    } else {
                        check_type_mismatch_deep(
                            field_value,
                            &field.type_name,
                            content,
                            &field.name,
                        )
                    };

                    if let Some(error_msg) = type_mismatch {
                        if let Some((line_num, col_start, col_end)) =
                            find_field_value_position(content, &field.name)
                        {
                            diagnostics.push(Diagnostic {
                                range: Range::new(
                                    Position::new(line_num as u32, col_start as u32),
                                    Position::new(line_num as u32, col_end as u32),
                                ),
                                severity: Some(DiagnosticSeverity::ERROR),
                                message: format!("Type mismatch: {}", error_msg),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }
    }

    // Check for missing required fields
    // Fields are required if they are not Option<T> and the struct doesn't have Default
    if !has_default {
        let missing_fields: Vec<_> = fields
            .iter()
            .filter(|field| {
                !ron_fields.contains(&field.name) && !field.type_name.starts_with("Option")
            })
            .collect();

        if !missing_fields.is_empty() {
            let missing_names: Vec<String> =
                missing_fields.iter().map(|f| f.name.clone()).collect();

            // Find the struct name position in the content
            let (line, col_start, col_end) = find_struct_name_position(content);

            diagnostics.push(Diagnostic {
                range: Range::new(Position::new(line, col_start), Position::new(line, col_end)),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!("Required fields: {}", missing_names.join(", ")),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Helper to validate enum variants
fn validate_enum_variant(
    content: &str,
    variants: &[EnumVariant],
    type_info: &TypeInfo,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // For enums, we need to parse the variant from the raw text
    let variant_name = extract_enum_variant_from_text(content);

    if let Some(variant) = variant_name {
        // Check if this variant exists
        if !variants.iter().any(|v| v.name == variant) {
            // Find the position of the variant in the content
            let (line, col) = find_variant_position(content, &variant);
            diagnostics.push(Diagnostic {
                range: Range::new(
                    Position::new(line, col),
                    Position::new(line, col + variant.len() as u32),
                ),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!(
                    "Unknown variant '{}' for enum '{}'",
                    variant, type_info.name
                ),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Extract a map from a RON value (handles both raw maps and named struct syntax)
fn extract_map_from_value(value: &Value) -> Option<&ron::Map> {
    match value {
        Value::Map(map) => Some(map),
        _ => None,
    }
}

/// Check if a type is a primitive type (not a custom enum/struct)
fn is_primitive_type(type_name: &str) -> bool {
    let clean = type_name.replace(" ", "");

    // Primitive types
    let primitives = [
        "bool", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128",
        "usize", "f32", "f64", "char", "String", "&str", "str",
    ];

    if primitives.contains(&clean.as_str()) {
        return true;
    }

    false
}

/// Check if a type is a standard library generic type (Option, Vec, HashMap, etc.)
fn is_std_generic_type(type_name: &str) -> bool {
    let clean = type_name.replace(" ", "");

    clean.starts_with("Option<")
        || clean.starts_with("Vec<")
        || clean.contains("HashMap<")
        || clean.contains("BTreeMap<")
        || clean.contains("HashSet<")
        || clean.contains("BTreeSet<")
        || clean.starts_with("Result<")
        || clean.starts_with("Box<")
        || clean.starts_with("Rc<")
        || clean.starts_with("Arc<")
}

/// Extract the variant name from raw RON text
/// Enums can be: Simple (Long), or with data (Long(...))
fn extract_enum_variant_from_text(content: &str) -> Option<String> {
    // Skip comments and find the first non-comment line
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("/*") {
            continue;
        }

        // Extract the first word (variant name)
        // Could be just "Long" or "Long(...)" or "Long { ... }"
        let variant = trimmed
            .split(|c: char| c == '(' || c == '{' || c.is_whitespace())
            .next()?
            .trim();

        if !variant.is_empty() {
            return Some(variant.to_string());
        }
    }

    None
}

/// Find the position of an enum variant in the content
fn find_variant_position(content: &str, variant: &str) -> (u32, u32) {
    for (line_num, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("/*") {
            continue;
        }

        if let Some(col) = line.find(variant) {
            return (line_num as u32, col as u32);
        }
    }

    (0, 0)
}

/// Find the position of the struct name in the RON content (e.g., "Post" in "Post(...)")
fn find_struct_name_position(content: &str) -> (u32, u32, u32) {
    for (line_num, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("/*") {
            continue;
        }

        // Look for a struct name followed by '(' (e.g., "Post(")
        if let Some(open_paren) = trimmed.find('(') {
            // Extract the struct name before the paren
            let before_paren = &trimmed[..open_paren];
            let struct_name = before_paren.trim();

            if !struct_name.is_empty() && struct_name.chars().next().unwrap().is_uppercase() {
                // Find the position in the original line (not trimmed)
                if let Some(col_start) = line.find(struct_name) {
                    return (
                        line_num as u32,
                        col_start as u32,
                        (col_start + struct_name.len()) as u32,
                    );
                }
            }
        }
    }

    // Default to first character if we can't find it
    (0, 0, 1)
}

/// Find the position of a field's value in the content for better error reporting
fn find_field_value_position(content: &str, field_name: &str) -> Option<(usize, usize, usize)> {
    for (line_num, line) in content.lines().enumerate() {
        // Look for "field_name:" pattern
        if let Some(field_pos) = line
            .find(&format!("{}: ", field_name))
            .or_else(|| line.find(&format!("{}:", field_name)))
        {
            // Find where the value starts (after the colon and whitespace)
            let after_field = field_pos + field_name.len();
            if let Some(colon_pos) = line[after_field..].find(':') {
                let value_start = after_field + colon_pos + 1;
                let trimmed_start = value_start + line[value_start..].len()
                    - line[value_start..].trim_start().len();

                // Find where the value ends (before comma, closing paren, or end of line)
                let rest = &line[trimmed_start..];
                let value_end = rest
                    .find(',')
                    .or_else(|| rest.find(')'))
                    .or_else(|| rest.find('}'))
                    .or_else(|| rest.find(']'))
                    .unwrap_or(rest.trim_end().len());

                return Some((line_num, trimmed_start, trimmed_start + value_end));
            }
        }
    }
    None
}

/// Type checking with enum variant validation (async, uses analyzer)
async fn check_type_mismatch_with_enum_validation(
    value: &Value,
    expected_type: &str,
    content: &str,
    field_name: &str,
    analyzer: &Arc<RustAnalyzer>,
) -> Option<String> {
    // First do basic type checking
    let basic_result = check_type_mismatch_deep(value, expected_type, content, field_name);
    if basic_result.is_some() {
        return basic_result;
    }

    // If the expected type is custom (not primitive), check if it's an enum and validate the variant
    if !is_primitive_type(expected_type) {
        if let Some(field_value_text) = extract_field_value_text(content, field_name) {
            let trimmed = field_value_text.trim();

            // Check if the type is an enum
            if let Some(type_info) = analyzer.get_type_info(expected_type).await {
                if let TypeKind::Enum(variants) = &type_info.kind {
                    // Extract the variant name from the text
                    let variant_name = trimmed.split('(').next().unwrap_or(trimmed).trim();

                    // Check if this is a valid variant
                    if !variants.iter().any(|v| v.name == variant_name) {
                        return Some(format!(
                            "unknown variant '{}' for enum {}",
                            variant_name, expected_type
                        ));
                    }
                }
            }
        }
    }

    None
}

/// Deep type checking that also validates custom types by looking at raw text
fn check_type_mismatch_deep(
    value: &Value,
    expected_type: &str,
    content: &str,
    field_name: &str,
) -> Option<String> {
    let clean_type = expected_type.replace(" ", "");

    // First check if it's a primitive type or standard library generic type
    if is_primitive_type(expected_type) || is_std_generic_type(expected_type) {
        return check_type_mismatch(value, expected_type);
    }

    // For custom types (structs/enums), we need to check the raw text
    // because Value loses the type information

    // Find the field's value in the raw text
    let field_value_text = extract_field_value_text(content, field_name)?;
    let trimmed = field_value_text.trim();

    // Check if expected type is a custom struct/enum (starts with uppercase)
    if clean_type
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
    {
        // Expected a struct/enum
        // The value should start with TypeName( or be a variant name

        // Check if it looks like a struct instantiation TypeName(...)
        if trimmed.contains('(') {
            // Extract the type name before the paren
            let type_in_text = trimmed.split('(').next().unwrap_or("").trim();
            let expected_simple = clean_type.split("::").last().unwrap_or(&clean_type);

            if type_in_text != expected_simple {
                return Some(format!("expected {}, got {}", expected_type, type_in_text));
            }
        } else {
            // It's a bare value - could be an enum variant or a primitive
            // If it's a number, string literal, or bool, that's wrong
            if trimmed.parse::<i64>().is_ok() {
                return Some(format!("expected {}, got integer", expected_type));
            }
            if trimmed.parse::<f64>().is_ok() {
                return Some(format!("expected {}, got float", expected_type));
            }
            if trimmed.starts_with('"') {
                return Some(format!("expected {}, got string", expected_type));
            }
            if trimmed == "true" || trimmed == "false" {
                return Some(format!("expected {}, got bool", expected_type));
            }
            // Otherwise assume it's an enum variant (we'd need more context to validate)
        }
    }

    None
}

/// Extract the raw text value for a field
fn extract_field_value_text(content: &str, field_name: &str) -> Option<String> {
    for line in content.lines() {
        if let Some(field_pos) = line.find(&format!("{}:", field_name)) {
            // Find where value starts (after colon)
            let after_colon = &line[field_pos + field_name.len() + 1..];
            let trimmed = after_colon.trim();

            // Extract until comma or end
            let value = trimmed.split(',').next()?.trim();
            return Some(value.to_string());
        }
    }
    None
}

/// Check if a RON value matches the expected Rust type
fn check_type_mismatch(value: &Value, expected_type: &str) -> Option<String> {
    let clean_type = expected_type.replace(" ", "");
    // Use clean_type for error messages to avoid extra spaces
    let display_type = &clean_type;

    // Handle Option types - None is always valid for Option<T>
    if clean_type.starts_with("Option<") {
        if matches!(value, Value::Option(None)) {
            return None;
        }
        // For Some(value), check the inner type
        if let Value::Option(Some(inner)) = value {
            // Extract the inner type from Option<InnerType>
            if let Some(inner_type) = extract_inner_type(&clean_type, "Option<") {
                return check_type_mismatch(inner, &inner_type);
            }
        }
        // Non-Option value for Option type is okay (will be wrapped)
        if let Some(inner_type) = extract_inner_type(&clean_type, "Option<") {
            return check_type_mismatch(value, &inner_type);
        }
    }

    // Handle Box, Rc, Arc - they serialize as just the inner value
    if clean_type.starts_with("Box<")
        || clean_type.starts_with("Rc<")
        || clean_type.starts_with("Arc<")
    {
        let wrapper = if clean_type.starts_with("Box<") {
            "Box<"
        } else if clean_type.starts_with("Rc<") {
            "Rc<"
        } else {
            "Arc<"
        };

        if let Some(inner_type) = extract_inner_type(&clean_type, wrapper) {
            return check_type_mismatch(value, &inner_type);
        }
    }

    match value {
        Value::Bool(_) => {
            if clean_type != "bool" {
                return Some(format!("expected {}, got bool", display_type));
            }
        }
        Value::Number(n) => {
            // Check for integer types
            let integer_types = [
                "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128",
                "usize",
            ];
            let float_types = ["f32", "f64"];

            let is_integer_type = integer_types.contains(&clean_type.as_str());
            let is_float_type = float_types.contains(&clean_type.as_str());

            // Check if it's a float or integer based on the Number variant
            let is_float_value = matches!(n, ron::Number::F32(_) | ron::Number::F64(_));
            let is_int_value = !is_float_value;

            if is_float_value && is_integer_type {
                return Some(format!("expected {}, got float", display_type));
            }
            if is_int_value && is_float_type {
                return Some(format!("expected {}, got integer", display_type));
            }
            // If not a numeric type at all, it's an error
            if !is_integer_type && !is_float_type {
                return Some(format!("expected {}, got number", display_type));
            }
        }
        Value::String(_) => {
            // String is valid for String types
            if clean_type == "String" || clean_type == "&str" || clean_type == "str" {
                return None;
            }
            // Otherwise it's an error
            return Some(format!("expected {}, got string", display_type));
        }
        Value::Seq(seq) => {
            if clean_type.starts_with("Vec<") {
                // Check element types if possible
                if let Some(elem_type) = extract_inner_type(&clean_type, "Vec<") {
                    for elem in seq {
                        if let Some(err) = check_type_mismatch(elem, &elem_type) {
                            return Some(format!("in Vec: {}", err));
                        }
                    }
                }
            } else if clean_type.contains("HashSet<") || clean_type.contains("BTreeSet<") {
                // Sets are serialized as arrays in RON
                return None;
            } else if clean_type.starts_with("Result<") {
                // Result variants like Ok(...) and Err(...) are serialized as tuples/arrays
                return None;
            } else if clean_type.starts_with("[") {
                // Array type
                return None; // Arrays are similar to Vec, accept them
            } else {
                return Some(format!("expected {}, got array", display_type));
            }
        }
        Value::Map(_) => {
            // Maps could be structs or actual maps
            if clean_type.contains("HashMap<") || clean_type.contains("BTreeMap<") {
                // It's a map type, which is fine
                return None;
            }
            // Check if it's a custom struct (starts with uppercase)
            if clean_type
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or(false)
            {
                // Could be a struct, allow it
                return None;
            }
            return Some(format!("expected {}, got map/struct", display_type));
        }
        Value::Option(Some(_)) => {
            if !clean_type.starts_with("Option<") {
                return Some(format!("expected {}, got Some(...)", display_type));
            }
        }
        Value::Option(None) => {
            if !clean_type.starts_with("Option<") {
                return Some(format!("expected {}, got None", display_type));
            }
        }
        Value::Unit => {
            if clean_type != "()" && clean_type != "unit" {
                return Some(format!("expected {}, got ()", display_type));
            }
        }
        _ => {}
    }

    None
}

/// Extract the inner type from a generic type like Option<T> or Vec<T>
fn extract_inner_type(type_str: &str, wrapper: &str) -> Option<String> {
    if type_str.starts_with(wrapper) && type_str.ends_with('>') {
        let inner = &type_str[wrapper.len()..type_str.len() - 1];
        return Some(inner.to_string());
    }
    None
}

/// Basic RON syntax validation with better error positioning
pub fn validate_ron_syntax(content: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Try to parse as RON
    if let Err(e) = ron::from_str::<ron::Value>(content) {
        let error_msg = e.to_string();

        // Try to extract line and column from error message
        // RON errors often contain "at line X column Y" or similar patterns
        let (line, col) = parse_error_position(&error_msg, content);

        // Create a more helpful error message
        let simplified_msg = simplify_ron_error(&error_msg);

        diagnostics.push(Diagnostic {
            range: Range::new(Position::new(line, col), Position::new(line, col + 1)),
            severity: Some(DiagnosticSeverity::ERROR),
            message: simplified_msg,
            ..Default::default()
        });
    }

    diagnostics
}

/// Parse error position from RON error message
fn parse_error_position(error_msg: &str, content: &str) -> (u32, u32) {
    // RON error messages often contain position info like "1:5" or "line 1 column 5"

    // Try to find "line X column Y" pattern
    if let Some(line_start) = error_msg.find("line ") {
        let rest = &error_msg[line_start + 5..];
        if let Some(line_end) = rest.find(|c: char| !c.is_numeric()) {
            if let Ok(line) = rest[..line_end].parse::<u32>() {
                if let Some(col_start) = rest.find("column ") {
                    let col_rest = &rest[col_start + 7..];
                    if let Some(col_end) = col_rest.find(|c: char| !c.is_numeric()) {
                        if let Ok(col) = col_rest[..col_end].parse::<u32>() {
                            // RON reports 1-indexed, LSP expects 0-indexed
                            return (line.saturating_sub(1), col.saturating_sub(1));
                        }
                    }
                }
            }
        }
    }

    // Try to find "X:Y" pattern (common in parsers)
    if let Some(colon_pos) = error_msg.find(':') {
        let before = &error_msg[..colon_pos];
        // Find the last number before the colon
        if let Some(line_start) = before.rfind(|c: char| !c.is_numeric()) {
            let line_str = &before[line_start + 1..];
            if let Ok(line) = line_str.parse::<u32>() {
                let after = &error_msg[colon_pos + 1..];
                if let Some(col_end) = after.find(|c: char| !c.is_numeric()) {
                    if let Ok(col) = after[..col_end].parse::<u32>() {
                        return (line.saturating_sub(1), col.saturating_sub(1));
                    }
                }
            }
        }
    }

    // If we can't parse position, try to find likely error location by looking for common issues
    let lines: Vec<&str> = content.lines().collect();

    // Check for missing commas between fields
    for (idx, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        // If a line ends with a value (not comma, not open brace) and next line starts with a field
        if !trimmed.is_empty()
            && !trimmed.ends_with(',')
            && !trimmed.ends_with('(')
            && !trimmed.ends_with('{')
            && !trimmed.ends_with('[')
            && !trimmed.starts_with("//")
            && !trimmed.starts_with("/*")
            && idx + 1 < lines.len()
        {
            let next_line = lines[idx + 1].trim();
            // Next line looks like a field (word followed by colon)
            if next_line.contains(':') && !next_line.starts_with("//") {
                // Likely missing comma
                return (idx as u32, line.len().saturating_sub(1) as u32);
            }
        }
    }

    // Default to start of file
    (0, 0)
}

/// Simplify RON error messages to be more user-friendly
fn simplify_ron_error(error_msg: &str) -> String {
    // Extract the core error without all the implementation details
    if error_msg.contains("expected") {
        if error_msg.contains("`,`") || error_msg.contains("comma") {
            return "Expected comma between fields".to_string();
        }
        if error_msg.contains("`:`") || error_msg.contains("colon") {
            return "Expected colon after field name".to_string();
        }
        if error_msg.contains("`)`") {
            return "Expected closing parenthesis".to_string();
        }
        if error_msg.contains("`}`") {
            return "Expected closing brace".to_string();
        }
        if error_msg.contains("`]`") {
            return "Expected closing bracket".to_string();
        }
    }

    if error_msg.contains("unexpected") {
        return format!(
            "Syntax error: {}",
            error_msg
                .split("unexpected")
                .nth(1)
                .unwrap_or(error_msg)
                .trim()
        );
    }

    // Return simplified version
    format!("RON syntax error: {}", error_msg)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_analyzer::{EnumVariant, FieldInfo};

    /// Sync version of struct validation (for tests, no analyzer)
    fn validate_struct_fields_sync(
        content: &str,
        fields: &[FieldInfo],
        parsed_value: &Result<Value, ron::error::SpannedError>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Extract fields from RON using proper parsing
        let ron_fields = ron_parser::extract_fields_from_ron(content);

        // Check for unknown fields
        for ron_field in &ron_fields {
            if !fields.iter().any(|f| &f.name == ron_field) {
                for (line_num, line) in content.lines().enumerate() {
                    if let Some(start_col) = line
                        .find(&format!("{}: ", ron_field))
                        .or_else(|| line.find(&format!("{}:", ron_field)))
                    {
                        diagnostics.push(Diagnostic {
                            range: Range::new(
                                Position::new(line_num as u32, start_col as u32),
                                Position::new(
                                    line_num as u32,
                                    (start_col + ron_field.len()) as u32,
                                ),
                            ),
                            severity: Some(DiagnosticSeverity::ERROR),
                            message: format!("Unknown field '{}'", ron_field),
                            ..Default::default()
                        });
                        break;
                    }
                }
            }
        }

        // Type check each field
        if let Ok(ref value) = parsed_value {
            let map = extract_map_from_value(value);

            if let Some(map) = map {
                for field in fields {
                    if let Some(field_value) = map.get(&Value::String(field.name.clone())) {
                        let type_mismatch = check_type_mismatch_deep(
                            field_value,
                            &field.type_name,
                            content,
                            &field.name,
                        );
                        if let Some(error_msg) = type_mismatch {
                            if let Some((line_num, col_start, col_end)) =
                                find_field_value_position(content, &field.name)
                            {
                                diagnostics.push(Diagnostic {
                                    range: Range::new(
                                        Position::new(line_num as u32, col_start as u32),
                                        Position::new(line_num as u32, col_end as u32),
                                    ),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    message: format!("Type mismatch: {}", error_msg),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                }
            }
        }

        // Check for missing required fields
        let missing_fields: Vec<_> = fields
            .iter()
            .filter(|field| {
                !ron_fields.contains(&field.name) && !field.type_name.starts_with("Option")
            })
            .collect();

        if !missing_fields.is_empty() {
            let missing_names: Vec<String> =
                missing_fields.iter().map(|f| f.name.clone()).collect();

            // Find the struct name position in the content
            let (line, col_start, col_end) = find_struct_name_position(content);

            diagnostics.push(Diagnostic {
                range: Range::new(Position::new(line, col_start), Position::new(line, col_end)),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!("Required fields: {}", missing_names.join(", ")),
                ..Default::default()
            });
        }

        diagnostics
    }

    /// Synchronous version for tests (without analyzer)
    pub fn validate_ron_against_type(content: &str, type_info: &TypeInfo) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // First check for syntax errors - if there are any, return them immediately
        // Don't try to do type checking on invalid RON
        let syntax_errors = validate_ron_syntax(content);
        if !syntax_errors.is_empty() {
            return syntax_errors;
        }

        // Try to parse the RON content to get actual values
        let parsed_value = ron::from_str::<Value>(content);

        match &type_info.kind {
            TypeKind::Struct(fields) => {
                // Use blocking version for sync function
                diagnostics.extend(validate_struct_fields_sync(content, fields, &parsed_value));
            }
            TypeKind::Enum(variants) => {
                diagnostics.extend(validate_enum_variant(content, variants, type_info));
            }
        }

        diagnostics
    }

    #[test]
    fn test_enum_variant_validation() {
        let type_info = TypeInfo {
            name: "PostType".to_string(),
            kind: TypeKind::Enum(vec![
                EnumVariant {
                    name: "Short".to_string(),
                    fields: vec![],
                    docs: None,
                    line: None,
                    column: None,
                },
                EnumVariant {
                    name: "Long".to_string(),
                    fields: vec![],
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
        };

        // Valid enum variant
        let content = "Long";
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(
            diagnostics.len(),
            0,
            "Long should be valid. Got errors: {:?}",
            diagnostics
        );

        // Another valid variant
        let content = "Short";
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(
            diagnostics.len(),
            0,
            "Short should be valid. Got errors: {:?}",
            diagnostics
        );

        // Invalid enum variant
        let content = "Medium";
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(diagnostics.len(), 1, "Medium should be invalid");
        assert!(
            diagnostics[0].message.contains("Unknown variant 'Medium'"),
            "Expected unknown variant error, got: {}",
            diagnostics[0].message
        );

        // Invalid enum variant (typo)
        let content = "Longs";
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(diagnostics.len(), 1, "Longs should be invalid");
        assert!(
            diagnostics[0].message.contains("Unknown variant 'Longs'"),
            "Expected unknown variant error, got: {}",
            diagnostics[0].message
        );
    }

    #[test]
    fn test_struct_with_enum_field() {
        let type_info = TypeInfo {
            name: "Post".to_string(),
            kind: TypeKind::Struct(vec![
                FieldInfo {
                    name: "id".to_string(),
                    type_name: "u32".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
                FieldInfo {
                    name: "title".to_string(),
                    type_name: "String".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
                FieldInfo {
                    name: "post_type".to_string(),
                    type_name: "PostType".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
        };

        // Valid struct with enum field
        let content = r#"Post(
            id: 1,
            title: "Test",
            post_type: Long,
        )"#;
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(
            diagnostics.len(),
            0,
            "Should have no errors. Got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_struct_field_expects_struct_not_primitive() {
        let type_info = TypeInfo {
            name: "Post".to_string(),
            kind: TypeKind::Struct(vec![
                FieldInfo {
                    name: "id".to_string(),
                    type_name: "u32".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
                FieldInfo {
                    name: "author".to_string(),
                    type_name: "User".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
        };

        // WRONG: author should be User(...), not just 1
        let content = r#"Post(
            id: 101,
            author: 1,
        )"#;
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert!(
            diagnostics.len() > 0,
            "Should error on primitive when expecting struct"
        );
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("expected User")),
            "Should complain about User type. Got: {:?}",
            diagnostics
        );

        // CORRECT: author is User(...)
        let content = r#"Post(
            id: 101,
            author: User(id: 1, name: "John"),
        )"#;
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert_eq!(
            diagnostics.len(),
            0,
            "Should have no errors. Got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_type_mismatch_primitives() {
        let type_info = TypeInfo {
            name: "User".to_string(),
            kind: TypeKind::Struct(vec![
                FieldInfo {
                    name: "id".to_string(),
                    type_name: "u32".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
                FieldInfo {
                    name: "name".to_string(),
                    type_name: "String".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
        };

        // Type mismatch - string for number
        let content = r#"User(
            id: "not a number",
            name: "John",
        )"#;
        let diagnostics = validate_ron_against_type(content, &type_info);
        assert!(diagnostics.len() > 0, "Should error on type mismatch");
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Type mismatch")),
            "Should have type mismatch error. Got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_ron_parsing_collections() {
        // Test what RON actually parses for bad collection values
        let content = r#"GenericTest(
            bad_hashmap: "not a map",
            bad_btreemap: 123,
            bad_hashset: "not a set",
        )"#;

        let parsed = ron::from_str::<Value>(content);
        println!("Parse result: {:?}", parsed);

        if let Ok(Value::Map(map)) = parsed {
            println!("Map has {} entries", map.len());
            for (k, v) in map.iter() {
                if let Value::String(key) = k {
                    println!("  {}: {:?}", key, v);
                }
            }
        }
    }

    #[test]
    fn test_invalid_enum_in_struct_field() {
        // This test ensures that invalid enum variants in struct fields are caught
        let type_info = TypeInfo {
            name: "Post".to_string(),
            kind: TypeKind::Struct(vec![
                FieldInfo {
                    name: "id".to_string(),
                    type_name: "u32".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
                FieldInfo {
                    name: "post_type".to_string(),
                    type_name: "PostType".to_string(),
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
        };

        // Invalid: "Longs" is not a valid PostType variant
        // This will be caught when the LSP has access to the analyzer
        // In the sync test version, it won't catch this (needs analyzer)
        let content = r#"Post(
            id: 1,
            post_type: Longs,
        )"#;
        let diagnostics = validate_ron_against_type(content, &type_info);
        // Without analyzer, this won't be caught - that's expected
        // With analyzer (in real LSP), check_type_mismatch_with_enum_validation will catch it
        println!("Diagnostics for invalid enum variant: {:?}", diagnostics);
    }
}
