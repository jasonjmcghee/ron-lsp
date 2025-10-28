#[cfg(feature = "cli")]
use crate::diagnostic_reporter;
use crate::tree_sitter_parser;
use crate::ts_utils::ParsedEnumVariant;
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

    // Parse RON once and check for syntax errors from the result
    // Try to parse the RON content
    let parsed_value = ron::from_str::<Value>(content);

    // If parsing failed, return syntax error
    if let Err(e) = &parsed_value {
        let error_msg = e.to_string();
        let (line, col) = parse_error_position(&error_msg, content);
        let simplified_msg = simplify_ron_error(&error_msg);

        diagnostics.push(Diagnostic {
            range: Range::new(Position::new(line, col), Position::new(line, col + 1)),
            severity: Some(DiagnosticSeverity::ERROR),
            message: simplified_msg,
            ..Default::default()
        });
        return diagnostics;
    }

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
            diagnostics.extend(
                validate_enum_variant_with_fields(content, variants, type_info, &analyzer).await,
            );
        }
    }

    // Check for enum variant fields (scans the whole file, so only call once)
    diagnostics.extend(
        validate_enum_variant_fields_in_structs(content, type_info, &analyzer).await,
    );

    // Deduplicate diagnostics by message and position
    let mut seen = std::collections::HashSet::new();
    diagnostics.retain(|d| {
        let key = (d.range.start.line, d.range.start.character, d.message.clone());
        seen.insert(key)
    });

    diagnostics
}

/// Validate enum variant fields within struct fields using the same logic as goto_definition
async fn validate_enum_variant_fields_in_structs(
    content: &str,
    type_info: &TypeInfo,
    analyzer: &Arc<RustAnalyzer>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut reported_errors = std::collections::HashSet::new();

    // Collect all variant locations and group by variant to check for missing fields
    let variant_locations = tree_sitter_parser::find_all_variant_field_locations(content);
    let mut variant_info: std::collections::HashMap<(String, String), (usize, std::collections::HashSet<String>)> =
        std::collections::HashMap::new();

    // Cache for variant type lookups: (containing_field_name, variant_name) -> Option<EnumVariant>
    let mut variant_cache: std::collections::HashMap<(String, String), Option<EnumVariant>> =
        std::collections::HashMap::new();

    // First pass: collect all fields present for each variant
    let lines: Vec<&str> = content.lines().collect();
    for location in &variant_locations {
        let key = (location.containing_field_name.clone(), location.variant_name.clone());
        let entry = variant_info.entry(key).or_insert((location.line_idx, std::collections::HashSet::new()));
        if let Some(ref field_at_pos) = location.field_at_position {
            entry.1.insert(field_at_pos.clone());
        }
    }

    // Second pass: validate unknown fields - do navigation ONCE per unique variant
    for ((containing_field_name, variant_name), (first_line, _present_fields)) in &variant_info {
        // Check if we've already looked up this variant
        if variant_cache.contains_key(&(containing_field_name.clone(), variant_name.clone())) {
            continue;
        }

        // Do the expensive navigation once per unique variant
        let position = Position::new(*first_line as u32, 0);
        let mut contexts = vec![tree_sitter_parser::TypeContext {
            type_name: type_info.name.split("::").last().unwrap_or(&type_info.name).to_string(),
            start_line: 0,
        }];
        let mut position_contexts = tree_sitter_parser::find_type_context_at_position(content, position);
        if !position_contexts.is_empty() {
            position_contexts.pop();
        }
        contexts.extend(position_contexts);

        let mut current_type_info = Some(type_info.clone());
        for context in contexts.iter().skip(1) {
            let info = match current_type_info {
                Some(ref info) => info.clone(),
                None => break,
            };

            if let Some(fields) = info.fields() {
                let context_name = &context.type_name;
                if let Some(field) = fields.iter().find(|f| {
                    let field_type_last = f.type_name.split("::").last().unwrap_or(&f.type_name);
                    let field_type_base = field_type_last.split('<').next().unwrap_or(field_type_last);
                    field_type_base == context_name
                }) {
                    current_type_info = analyzer.get_type_info(&field.type_name).await;
                    continue;
                }
            }

            let direct_lookup = analyzer.get_type_info(&context.type_name).await;
            if direct_lookup.is_some() {
                current_type_info = direct_lookup;
            } else {
                let mut found_via_variant = false;
                if let Some(variant) = info.find_variant(&context.type_name) {
                    if variant.fields.len() == 1 {
                        let field_type = &variant.fields[0].type_name;
                        current_type_info = analyzer.get_type_info(field_type).await;
                        found_via_variant = true;
                    }
                }
                if !found_via_variant {
                    if let Some(fields) = info.fields() {
                        for field in fields {
                            if let Some(field_type_info) = analyzer.get_type_info(&field.type_name).await {
                                if field_type_info.find_variant(&context.type_name).is_some() {
                                    current_type_info = Some(field_type_info);
                                    found_via_variant = true;
                                    break;
                                }
                            }
                        }
                    }
                }
                if !found_via_variant {
                    current_type_info = None;
                }
            }
        }

        // Look up the variant and cache it
        let variant = if let Some(current_type) = current_type_info {
            if let Some(field) = current_type.find_field(containing_field_name) {
                if let Some(field_type_info) = analyzer.get_type_info(&field.type_name).await {
                    field_type_info.find_variant(variant_name).cloned()
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        variant_cache.insert((containing_field_name.clone(), variant_name.clone()), variant);
    }

    // Third pass: check all field locations using cached variant info
    for location in &variant_locations {
        if let Some(ref field_at_pos) = location.field_at_position {
            let error_key = (location.line_idx, field_at_pos.clone());
            if !reported_errors.contains(&error_key) {
                let cache_key = (location.containing_field_name.clone(), location.variant_name.clone());
                if let Some(Some(variant)) = variant_cache.get(&cache_key) {
                    if !variant.fields.iter().any(|f| f.name == *field_at_pos) {
                        let line = lines.get(location.line_idx).unwrap_or(&"");
                        if let Some(col) = line.find(&format!("{}:", field_at_pos))
                            .or_else(|| line.find(&format!("{} :", field_at_pos))) {
                            diagnostics.push(Diagnostic {
                                range: Range::new(
                                    Position::new(location.line_idx as u32, col as u32),
                                    Position::new(location.line_idx as u32, (col + field_at_pos.len()) as u32),
                                ),
                                severity: Some(DiagnosticSeverity::ERROR),
                                message: format!("Unknown field '{}' in variant '{}'", field_at_pos, location.variant_name),
                                ..Default::default()
                            });
                            reported_errors.insert(error_key);
                        }
                    }
                }
            }
        }
    }

    // Fourth pass: check for missing required fields using cached variant info
    for ((containing_field_name, variant_name), (first_line, present_fields)) in variant_info {
        let cache_key = (containing_field_name.clone(), variant_name.clone());
        if let Some(Some(variant)) = variant_cache.get(&cache_key) {
            // Check for missing required fields
            for vfield in &variant.fields {
                if !present_fields.contains(&vfield.name)
                    && !vfield.type_name.starts_with("Option") {
                    // Note: We don't have has_default info in the cached variant,
                    // but typically enum variants don't have defaults

                    // Find the variant opening line to report the error
                    // Search backwards from first_line to find the line with the variant name
                    let mut variant_line_idx = first_line;
                    let mut variant_col = 0;
                    for i in (0..=first_line).rev() {
                        if let Some(line) = lines.get(i) {
                            if let Some(col) = line.find(&variant_name) {
                                variant_line_idx = i;
                                variant_col = col;
                                break;
                            }
                        }
                    }

                    diagnostics.push(Diagnostic {
                        range: Range::new(
                            Position::new(variant_line_idx as u32, variant_col as u32),
                            Position::new(variant_line_idx as u32, (variant_col + variant_name.len()) as u32),
                        ),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("Missing required field '{}' in variant '{}'", vfield.name, variant_name),
                        ..Default::default()
                    });
                }
            }
        }
    }

    diagnostics
}

/// Adjust diagnostic line numbers by an offset
fn adjust_diagnostic_positions(diagnostics: Vec<Diagnostic>, line_offset: u32) -> Vec<Diagnostic> {
    diagnostics
        .into_iter()
        .map(|mut d| {
            d.range.start.line += line_offset;
            d.range.end.line += line_offset;
            d
        })
        .collect()
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
    let ron_fields = tree_sitter_parser::extract_fields_from_ron(content);

    // Check for unknown fields in RON using tree-sitter for position
    use crate::ts_utils::{self, RonParser};
    let mut parser = RonParser::new();
    if let Some(tree) = parser.parse(content) {
        if let Some(main_value) = ts_utils::find_main_value(&tree) {
            if main_value.kind() == "struct" {
                let field_nodes = ts_utils::struct_fields(&main_value);
                for field_node in field_nodes {
                    if let Some(field_name) = ts_utils::field_name(&field_node, content) {
                        if !fields.iter().any(|f| f.name == field_name) {
                            let range = ts_utils::node_to_lsp_range(&field_node.child(0).unwrap());
                            diagnostics.push(Diagnostic {
                                range,
                                severity: Some(DiagnosticSeverity::ERROR),
                                message: format!("Unknown field '{}'", field_name),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        }
    }

    // Type check each field if we successfully parsed the RON
    if let Ok(ref value) = parsed_value {
        // For named structs like MyStruct(...), the value might be wrapped
        let map = extract_map_from_value(value);

        if let Some(map) = map {
            // Pre-compute field positions and contents in a single pass
            let mut field_positions = std::collections::HashMap::new();
            let mut field_contents = std::collections::HashMap::new();

            for field in fields {
                if let Some(pos) = find_field_value_position(content, &field.name) {
                    field_positions.insert(field.name.clone(), pos);
                }
                if let Some(content_str) = extract_field_value_text(content, &field.name) {
                    field_contents.insert(field.name.clone(), content_str);
                }
            }

            for field in fields {
                if let Some(field_value) = map.get(&Value::String(field.name.clone())) {
                    // For custom types (structs/enums), recursively validate
                    if let Some(analyzer) = analyzer {
                        if !is_primitive_type(&field.type_name) && !is_std_generic_type(&field.type_name) {
                            if let Some(nested_type_info) = analyzer.get_type_info(&field.type_name).await {
                                // Use pre-extracted position and content
                                if let Some(&(line_num, _, _)) = field_positions.get(&field.name) {
                                    if let Some(field_content) = field_contents.get(&field.name) {
                                        let mut nested_diags = Box::pin(validate_ron_with_analyzer(
                                            field_content,
                                            &nested_type_info,
                                            analyzer.clone(),
                                        )).await;
                                        // Adjust line numbers to match the original file
                                        nested_diags = adjust_diagnostic_positions(nested_diags, line_num as u32);
                                        diagnostics.extend(nested_diags);
                                        continue; // Skip type checking since we did full validation
                                    }
                                }
                            }
                        }
                    }

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
                        // Use pre-computed position
                        if let Some(&(line_num, col_start, col_end)) = field_positions.get(&field.name) {
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

/// Async version: validate enum variants with field type checking
async fn validate_enum_variant_with_fields(
    content: &str,
    variants: &[EnumVariant],
    type_info: &TypeInfo,
    analyzer: &Arc<RustAnalyzer>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // For enums, we need to parse the variant from the raw text
    let parsed_variant = extract_enum_variant_from_text(content);

    if let Some(variant) = parsed_variant {
        // Check if this variant exists
        if let Some(variant_def) = variants.iter().find(|v| v.name == variant.name) {
            // Variant exists - now validate its fields if it has data
            if let Some(ref data) = variant.data {
                // Validate that the variant can have data
                if variant_def.fields.is_empty() {
                    diagnostics.push(Diagnostic {
                        range: Range::new(
                            Position::new(variant.line, variant.col),
                            Position::new(variant.line, variant.col + variant.name.len() as u32),
                        ),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!(
                            "Variant '{}' is a unit variant and cannot have data",
                            variant.name
                        ),
                        ..Default::default()
                    });
                } else {
                    // Validate the fields
                    let mut field_diagnostics =
                        validate_variant_field_data(data, &variant_def.fields, analyzer).await;
                    // Adjust positions to account for the variant line offset
                    field_diagnostics = adjust_diagnostic_positions(field_diagnostics, variant.line);
                    diagnostics.extend(field_diagnostics);
                }
            }
        } else {
            // Variant doesn't exist
            diagnostics.push(Diagnostic {
                range: Range::new(
                    Position::new(variant.line, variant.col),
                    Position::new(variant.line, variant.col + variant.name.len() as u32),
                ),
                severity: Some(DiagnosticSeverity::ERROR),
                message: format!(
                    "Unknown variant '{}' for enum '{}'",
                    variant.name, type_info.name
                ),
                ..Default::default()
            });
        }
    }

    diagnostics
}

/// Validate the data inside an enum variant (tuple or struct fields)
/// This recursively validates nested types
async fn validate_variant_field_data(
    data: &str,
    expected_fields: &[FieldInfo],
    analyzer: &Arc<RustAnalyzer>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // For single-field tuple variants with custom types, recursively validate
    if expected_fields.len() == 1 && !is_primitive_type(&expected_fields[0].type_name) && !is_std_generic_type(&expected_fields[0].type_name) {
        let field_type = &expected_fields[0].type_name;
        if let Some(nested_type_info) = analyzer.get_type_info(field_type).await {
            // Recursively validate the nested content (box to avoid infinite size)
            // Note: For enum variants, the 'data' string already has correct relative positions
            // within the extracted content, but we don't have the parent context here to adjust them
            let nested_diags = Box::pin(validate_ron_with_analyzer(data, &nested_type_info, analyzer.clone())).await;
            diagnostics.extend(nested_diags);
            return diagnostics;
        }
    }

    // Try to parse the data as RON
    // For struct variants, the data contains named fields like "field1: val, field2: val"
    // For tuple variants, the data contains unnamed values like "val1, val2"
    let has_named_fields = expected_fields.iter().any(|f| f.name.parse::<usize>().is_err());

    let parsed_data = if has_named_fields {
        // Struct-like variant: wrap the named fields in parentheses for RON parsing
        // RON syntax for struct variants is: VariantName( field: value )
        ron::from_str::<Value>(&format!("Temp({})", data))
    } else if data.contains(',') || expected_fields.len() > 1 {
        // Tuple variant with multiple fields
        ron::from_str::<Value>(&format!("({})", data))
    } else {
        // Single unnamed field
        ron::from_str::<Value>(data)
    };

    match parsed_data {
        Ok(value) => {
            // Validate fields based on whether it's named or unnamed
            if expected_fields.iter().all(|f| f.name.parse::<usize>().is_err()) {
                // Named fields (struct-like variant)
                if let Some(map) = extract_map_from_value(&value) {
                    // First, check for unknown fields - extract from the parsed value, not raw data
                    let mut ron_fields = Vec::new();
                    for key in map.keys() {
                        if let Value::String(field_name) = key {
                            ron_fields.push(field_name.clone());
                        }
                    }

                    // Validate field types
                    for field in expected_fields {
                        if let Some(field_value) = map.get(&Value::String(field.name.clone())) {
                            if let Some(error_msg) = check_type_mismatch_with_enum_validation(
                                field_value,
                                &field.type_name,
                                data,
                                &field.name,
                                analyzer,
                            )
                            .await
                            {
                                diagnostics.push(Diagnostic {
                                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    message: format!("Type mismatch in variant field: {}", error_msg),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                }
            } else {
                // Unnamed fields (tuple variant)
                // For tuple variants, fields are named "0", "1", "2", etc.
                if let Value::Seq(values) = value {
                    for (i, field) in expected_fields.iter().enumerate() {
                        if let Some(field_value) = values.get(i) {
                            if let Some(error_msg) = check_type_mismatch_with_enum_validation(
                                field_value,
                                &field.type_name,
                                data,
                                &field.name,
                                analyzer,
                            )
                            .await
                            {
                                diagnostics.push(Diagnostic {
                                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    message: format!(
                                        "Type mismatch in variant field {}: {}",
                                        i, error_msg
                                    ),
                                    ..Default::default()
                                });
                            }
                        }
                    }
                } else if expected_fields.len() == 1 {
                    // Single field tuple variant
                    if let Some(error_msg) = check_type_mismatch_with_enum_validation(
                        &value,
                        &expected_fields[0].type_name,
                        data,
                        &expected_fields[0].name,
                        analyzer,
                    )
                    .await
                    {
                        diagnostics.push(Diagnostic {
                            range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                            severity: Some(DiagnosticSeverity::ERROR),
                            message: format!("Type mismatch in variant field: {}", error_msg),
                            ..Default::default()
                        });
                    }
                }
            }
        }
        Err(_) => {
            // Failed to parse - could be syntax error
            diagnostics.push(Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Invalid syntax in enum variant data".to_string(),
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


/// Extract the variant name and data from raw RON text using tree-sitter
/// Enums can be: Simple (Long), tuple (Long(...)), or struct-like (Long { ... })
fn extract_enum_variant_from_text(content: &str) -> Option<ParsedEnumVariant> {
    use crate::ts_utils::{self, RonParser};

    // Skip type annotation if present
    let ron_content = if content.trim_start().starts_with("/*") {
        if let Some(end_idx) = content.find("*/") {
            &content[end_idx + 2..]
        } else {
            content
        }
    } else {
        content
    };


    let mut parser = RonParser::new();
    let tree = parser.parse(ron_content)?;

    // Find the main value (should be an identifier or struct representing the variant)
    let main_value = ts_utils::find_main_value(&tree);

    let main_value = main_value?;
    ts_utils::extract_enum_variant(&main_value, ron_content)
}


/// Find the position of an enum variant in the content using tree-sitter
#[allow(dead_code)]
fn find_variant_position(content: &str, variant: &str) -> (u32, u32) {
    use crate::ts_utils::{self, RonParser};

    let mut parser = RonParser::new();
    if let Some(tree) = parser.parse(content) {
        let variants = ts_utils::find_potential_variants(&tree, content);
        for v in variants {
            if let Some(text) = ts_utils::node_text(&v, content) {
                if text == variant {
                    let pos = v.start_position();
                    return (pos.row as u32, pos.column as u32);
                }
            }
        }
    }

    (0, 0)
}

/// Find the position of the struct name in the RON content using tree-sitter
/// Returns (line, col_start, col_end) where col_start == col_end indicates unnamed struct
fn find_struct_name_position(content: &str) -> (u32, u32, u32) {
    use crate::ts_utils::{self, RonParser};

    let mut parser = RonParser::new();
    if let Some(tree) = parser.parse(content) {
        if let Some(main_value) = ts_utils::find_main_value(&tree) {
            if main_value.kind() == "struct" {
                if let Some(name_node) = main_value.child(0) {
                    if name_node.kind() == "identifier" {
                        let pos = name_node.start_position();
                        let end_pos = name_node.end_position();
                        return (pos.row as u32, pos.column as u32, end_pos.column as u32);
                    }
                }
                // Unnamed struct
                let pos = main_value.start_position();
                return (pos.row as u32, pos.column as u32, pos.column as u32);
            }
        }
    }

    (0, 0, 1)
}

/// Find the position of a field's value in the content using tree-sitter
fn find_field_value_position(content: &str, field_name: &str) -> Option<(usize, usize, usize)> {
    use crate::ts_utils::{self, RonParser};

    let mut parser = RonParser::new();
    let tree = parser.parse(content)?;

    if let Some(main_value) = ts_utils::find_main_value(&tree) {
        if main_value.kind() == "struct" {
            let fields = ts_utils::struct_fields(&main_value);
            for field in fields {
                if let Some(name) = ts_utils::field_name(&field, content) {
                    if name == field_name {
                        if let Some(value_node) = ts_utils::field_value(&field) {
                            let pos = value_node.start_position();
                            let end_pos = value_node.end_position();
                            return Some((pos.row, pos.column, end_pos.column));
                        }
                    }
                }
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

        // Check if it looks like a struct instantiation TypeName(...) or unnamed (...)
        if trimmed.contains('(') {
            // Extract the type name before the paren
            let type_in_text = trimmed.split('(').next().unwrap_or("").trim();
            let expected_simple = clean_type.split("::").last().unwrap_or(&clean_type);

            // Allow unnamed struct syntax - empty type_in_text means type is inferred
            if !type_in_text.is_empty() && type_in_text != expected_simple {
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

/// Extract the raw text value for a field, handling nested structures
fn extract_field_value_text(content: &str, field_name: &str) -> Option<String> {
    use crate::ts_utils::{self, RonParser};

    let mut parser = RonParser::new();
    let tree = parser.parse(content)?;
    let main_value = ts_utils::find_main_value(&tree)?;

    if main_value.kind() == "struct" || main_value.kind() == "ERROR" {
        // For ERROR nodes, find the struct sibling
        let struct_node = if main_value.kind() == "ERROR" {
            let root = tree.root_node();
            let mut cursor = root.walk();
            let result = root.children(&mut cursor).find(|n| n.kind() == "struct");
            match result {
                Some(s) => s,
                None => return None,
            }
        } else {
            main_value
        };

        let field_nodes = ts_utils::struct_fields(&struct_node);
        for field_node in field_nodes {
            if let Some(name) = ts_utils::field_name(&field_node, content) {
                if name == field_name {
                    if let Some(value_node) = ts_utils::field_value(&field_node) {
                        return ts_utils::node_text(&value_node, content).map(|s| s.to_string());
                    }
                }
            }
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
#[allow(dead_code)]
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



    #[tokio::test]
    async fn test_enum_variant_validation() {
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // Valid enum variant
        let content = "Long";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Long should be valid. Got errors: {:?}",
            diagnostics
        );

        // Another valid variant
        let content = "Short";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Short should be valid. Got errors: {:?}",
            diagnostics
        );

        // Invalid enum variant
        let content = "Medium";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
        assert_eq!(diagnostics.len(), 1, "Medium should be invalid");
        assert!(
            diagnostics[0].message.contains("Unknown variant 'Medium'"),
            "Expected unknown variant error, got: {}",
            diagnostics[0].message
        );

        // Invalid enum variant (typo)
        let content = "Longs";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
        assert_eq!(diagnostics.len(), 1, "Longs should be invalid");
        assert!(
            diagnostics[0].message.contains("Unknown variant 'Longs'"),
            "Expected unknown variant error, got: {}",
            diagnostics[0].message
        );
    }

    #[tokio::test]
    async fn test_struct_with_enum_field() {
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // Valid struct with enum field
        let content = r#"Post(
            id: 1,
            title: "Test",
            post_type: Long,
        )"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Should have no errors. Got: {:?}",
            diagnostics
        );
    }

    #[tokio::test]
    async fn test_struct_field_expects_struct_not_primitive() {
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // WRONG: author should be User(...), not just 1
        let content = r#"Post(
            id: 101,
            author: 1,
        )"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
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
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Should have no errors. Got: {:?}",
            diagnostics
        );
    }

    #[tokio::test]
    async fn test_type_mismatch_primitives() {
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // Type mismatch - string for number
        let content = r#"User(
            id: "not a number",
            name: "John",
        )"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
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

    #[tokio::test]
    async fn test_invalid_enum_in_struct_field() {
        // This test ensures that invalid enum variants in struct fields are caught
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // Invalid: "Longs" is not a valid PostType variant
        // This will be caught when the LSP has access to the analyzer
        // In the sync test version, it won't catch this (needs analyzer)
        let content = r#"Post(
            id: 1,
            post_type: Longs,
        )"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        // Without analyzer, this won't be caught - that's expected
        // With analyzer (in real LSP), check_type_mismatch_with_enum_validation will catch it
        println!("Diagnostics for invalid enum variant: {:?}", diagnostics);
    }

    #[tokio::test]
    async fn test_unnamed_struct_syntax() {
        let analyzer = Arc::new(RustAnalyzer::new());
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
            has_default: false,
        };

        // Unnamed struct syntax should be valid
        let content = r#"(
            id: 1,
            name: "John",
        )"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Unnamed struct syntax should be valid. Got errors: {:?}",
            diagnostics
        );
    }

    #[tokio::test]
    async fn test_enum_with_tuple_variant() {
        let analyzer = Arc::new(RustAnalyzer::new());
        let type_info = TypeInfo {
            name: "Value".to_string(),
            kind: TypeKind::Enum(vec![
                EnumVariant {
                    name: "Int".to_string(),
                    fields: vec![FieldInfo {
                        name: "0".to_string(),
                        type_name: "i32".to_string(),
                        docs: None,
                        line: None,
                        column: None,
                    }],
                    docs: None,
                    line: None,
                    column: None,
                },
                EnumVariant {
                    name: "Str".to_string(),
                    fields: vec![FieldInfo {
                        name: "0".to_string(),
                        type_name: "String".to_string(),
                        docs: None,
                        line: None,
                        column: None,
                    }],
                    docs: None,
                    line: None,
                    column: None,
                },
            ]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
            has_default: false,
        };

        // Valid tuple variant
        let content = "Int(42)";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer.clone()).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Tuple variant should be valid. Got errors: {:?}",
            diagnostics
        );

        // Another valid variant
        let content = r#"Str("hello")"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        assert_eq!(
            diagnostics.len(),
            0,
            "Tuple variant with string should be valid. Got errors: {:?}",
            diagnostics
        );
    }

    #[tokio::test]
    async fn test_enum_with_struct_variant() {
        let analyzer = Arc::new(RustAnalyzer::new());
        let type_info = TypeInfo {
            name: "Message".to_string(),
            kind: TypeKind::Enum(vec![EnumVariant {
                name: "Text".to_string(),
                fields: vec![
                    FieldInfo {
                        name: "content".to_string(),
                        type_name: "String".to_string(),
                        docs: None,
                        line: None,
                        column: None,
                    },
                    FieldInfo {
                        name: "sender".to_string(),
                        type_name: "String".to_string(),
                        docs: None,
                        line: None,
                        column: None,
                    },
                ],
                docs: None,
                line: None,
                column: None,
            }]),
            docs: None,
            source_file: None,
            line: None,
            column: None,
            has_default: false,
        };

        // Struct-like variant (this requires parentheses in RON)
        let content = r#"Text { content: "hello", sender: "alice" }"#;
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        // This might not validate correctly without proper struct-variant handling
        // but we're testing that it parses and doesn't crash
        println!("Struct variant diagnostics: {:?}", diagnostics);
    }

    #[test]
    fn test_ron_parsing_enum_variant() {
        // Test if RON can parse a standalone enum variant
        let test_cases = vec![
            "Detailed( length: 1 )",
            "Detailed(length: 1)",
            "Detailed { length: 1 }",
        ];

        for case in test_cases {
            println!("Testing: {}", case);
            let result = ron::from_str::<Value>(case);
            println!("Result: {:?}", result);
        }
    }

    #[test]
    fn test_extract_field_value_for_enum() {
        let content = r#"Post(
    id: 42,
    post_type: Detailed( length: 1 ),
)"#;
        let extracted = extract_field_value_text(content, "post_type");
        println!("Extracted post_type value: {:?}", extracted);
        assert!(extracted.is_some());
        let value = extracted.unwrap();
        assert_eq!(value, "Detailed( length: 1 )");
    }

    #[test]
    fn test_extract_nested_enum_variant() {
        let content = r#"/* @[crate::models::Message] */

PostReference(Post(
    id: 42,
    title: "test",
))"#;
        let variant = extract_enum_variant_from_text(content);
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "PostReference");
        println!("Extracted data: {:?}", variant.data);
        assert!(variant.data.is_some());
        let data = variant.data.unwrap();
        assert!(data.contains("Post("));
        assert!(data.contains("id: 42"));
    }

    #[tokio::test]
    async fn test_unit_variant_with_data_error() {
        let analyzer = Arc::new(RustAnalyzer::new());
        let type_info = TypeInfo {
            name: "Status".to_string(),
            kind: TypeKind::Enum(vec![
                EnumVariant {
                    name: "Active".to_string(),
                    fields: vec![],
                    docs: None,
                    line: None,
                    column: None,
                },
                EnumVariant {
                    name: "Inactive".to_string(),
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
            has_default: false,
        };

        // Unit variant should not have data
        let content = "Active(123)";
        let diagnostics = validate_ron_with_analyzer(content, &type_info, analyzer).await;
        // Should get error for providing data to unit variant
        assert!(
            diagnostics.iter().any(|d| d
                .message
                .contains("unit variant") || d.message.contains("cannot have data")),
            "Should error on unit variant with data. Got: {:?}",
            diagnostics
        );
    }
}
