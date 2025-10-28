// Tree-sitter grammar for RON (Rusty Object Notation)
// Reference: https://github.com/ron-rs/ron/blob/master/docs/grammar.md

module.exports = grammar({
  name: 'ron',

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  rules: {
    source_file: $ => seq(
      optional($.extensions),
      optional($.type_annotation),
      $._value,
    ),

    // Extensions like #![enable(implicit_some)]
    extensions: $ => repeat1($.extension),

    extension: $ => seq(
      '#!',
      '[',
      $.identifier,
      optional(seq('(', commaSep($.identifier), ')')),
      ']',
    ),

    // Type annotation like /* @[crate::models::Post] */
    type_annotation: $ => seq(
      '/*',
      '@',
      '[',
      $.type_path,
      ']',
      '*/',
    ),

    type_path: $ => sep1('::', choice($.identifier, 'crate', 'super', 'self')),

    _value: $ => choice(
      $.unit,
      $.boolean,
      $.integer,
      $.float,
      $.char,
      $.string,
      $.array,
      $.map,
      $.tuple,
      $.struct,
      $.identifier, // For unit enum variants or bare identifiers
    ),

    // Unit type ()
    unit: $ => '()',

    // Structs and variants
    // Named tuple struct: StructName(val1, val2)
    // Named struct: StructName(field: val1, field2: val2) or (field: val1)
    struct: $ => choice(
      // Named struct/variant with parentheses (either named fields or tuple-like)
      seq(
        $.identifier,
        '(',
        commaSep(choice($.field, $._value)),
        optional(','),
        ')',
      ),
      // Anonymous struct with only named fields: (field: value, ...)
      prec(1, seq(
        '(',
        $.field,
        repeat(seq(',', choice($.field, $._value))),
        optional(','),
        ')',
      )),
    ),

    field: $ => seq(
      $.identifier,
      ':',
      $._value,
    ),

    // Tuples - must not have any named fields, just values
    // Disambiguated from single-value parens by requiring multiple values
    tuple: $ => prec(-1, seq(
      '(',
      $._value,
      repeat1(seq(',', $._value)),
      optional(','),
      ')',
    )),

    // Arrays
    array: $ => seq(
      '[',
      commaSep($._value),
      optional(','),
      ']',
    ),

    // Maps
    map: $ => seq(
      '{',
      commaSep($.map_entry),
      optional(','),
      '}',
    ),

    map_entry: $ => seq(
      $._value,
      ':',
      $._value,
    ),

    // Identifiers
    identifier: $ => /[_\p{XID_Start}][_\p{XID_Continue}]*/,

    // Numbers
    integer: $ => token(choice(
      seq(optional('-'), /[0-9][0-9_]*/),
      seq(optional('-'), /0x[0-9a-fA-F_]+/),
      seq(optional('-'), /0b[01_]+/),
      seq(optional('-'), /0o[0-7_]+/),
    )),

    float: $ => token(seq(
      optional('-'),
      choice(
        // 123.456
        seq(/[0-9][0-9_]*/, '.', /[0-9_]*/),
        // 123e10 or 123.456e10
        seq(/[0-9][0-9_]*/, optional(seq('.', /[0-9_]*/)), /[eE][+-]?[0-9_]+/),
      ),
    )),

    // Strings
    string: $ => choice(
      $._quoted_string,
      $.raw_string,
    ),

    _quoted_string: $ => seq(
      '"',
      repeat(choice(
        token.immediate(prec(1, /[^"\\]+/)),
        $.escape_sequence,
      )),
      '"',
    ),

    raw_string: $ => token(seq(
      'r',
      repeat('#'),
      '"',
      repeat(/./),
      '"',
      repeat('#'),
    )),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^xu]/,
        /u[0-9a-fA-F]{4}/,
        /u\{[0-9a-fA-F]+\}/,
        /x[0-9a-fA-F]{2}/,
      ),
    )),

    // Characters
    char: $ => token(seq(
      optional('b'),
      '\'',
      choice(
        seq('\\', choice(
          /[^xu]/,
          /u[0-9a-fA-F]{4}/,
          /u\{[0-9a-fA-F]+\}/,
          /x[0-9a-fA-F]{2}/,
        )),
        /[^'\\]/,
      ),
      '\'',
    )),

    // Booleans
    boolean: $ => choice('true', 'false'),

    // Comments
    line_comment: $ => token(seq('//', /.*/)),

    block_comment: $ => token(seq(
      '/*',
      repeat(choice(
        /[^*]/,
        /\*[^/]/,
      )),
      '*/',
    )),
  }
});

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

function sep1(separator, rule) {
  return seq(rule, repeat(seq(separator, rule)));
}
