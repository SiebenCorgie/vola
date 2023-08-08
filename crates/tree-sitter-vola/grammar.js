module.exports = grammar({
  name: 'vola',

  word: $ => $.identifier,

  rules: {

    //Any definition of anything in the file.
    source_file: $ => repeat($._definition),

    //This level will sort out the domain in which we are
    _definition: $ => choice(
      $.prim_definition,
      $.op_definition,
      $.field_definition
    ),

    // prim some_name([params...]){[block]}
    prim_definition: $ => seq(
      'prim',
      $.identifier,
      $.parameter_list,
      $.block
    ),

    //op some_op<[prims]>([params]){[block]}
    op_definition: $ => seq(
      'op',
      $.identifier,
      $.prim_list,
      $.parameter_list,
      $.block
    ),

    //field some_field([params]){[block]}
    field_definition: $ => seq(
      'field',
      $.identifier,
      $.parameter_list,
      $.block
    ),


    //Generic primitive list
    prim_list: $ => seq(
      '<',
      $.identifier,
      '>'
    ),

    //Generic parameter list
    parameter_list: $ => seq(
      '(',
      repeat($.typed_identifier),
      ')'
    ),

    //Any typed identifier with an optional type hint
    typed_identifier: $ => seq(
      $.identifier,
      optional(
        seq(
          ':',
          $._type
        )
      )
    ),


    block: $ => seq(
      '{',
      repeat($._statement),
      //by definition we must end with an unbound statement
      $.unbound_statement,
      '}'
    ),

    _statement: $ => choice(
      $.let_statement,
      $.unbound_statement,
      $.at_rewrite_statement
      // TODO: other kinds of statements
    ),

    unbound_statement: $ => seq(
      $._expression
    ),

    let_statement: $ => seq(
      'let',
      $.typed_identifier,
      '=',
      $._expression,
      ';'
    ),

    at_rewrite_statement: $ => seq(
      '@',
      '=',
      $._expression,
      ';'
    ),


    _expression: $ => choice(
      $.identifier,
      $.number,
      $.keyword,
      $.scoped_expression,
      $.function_call_expr,
      $.unary_expression,
      $.binary_expression
      // TODO: other kinds of expressions
    ),

    function_call_expr: $ => seq(
      $.identifier,
      '(',
      repeat(seq($._expression, ',')), //for >1 arguments
      optional($._expression), // for 0-th and/or 1st
      ')'
    ),

    scoped_expression: $ => seq(
      '{',
      $._expression,
      '}'
    ),

    unary_expression: $ => prec(3, choice(
      seq('-', $._expression),
      seq('!', $._expression),
      //TODO more?
    )),

    binary_expression: $ => choice(
      prec.left(2, seq($._expression, '/', $._expression)),
      prec.left(2, seq($._expression, '*', $._expression)),
      prec.left(1, seq($._expression, '+', $._expression)),
      prec.left(1, seq($._expression, '-', $._expression)),
      // ...
    ),

    _type: $ => choice(
      $.scalarty,
      $.vecty,
      $.matty
    ),

    scalarty: $ => 'scalar',
    vecty: $ => seq('vec', $.number),
    matty: $ => seq('mat', $.number, 'x', $.number),

    identifier: $ => /[a-z]+/,

    number: $ => /\d+/,

    keyword: $ => choice(
      $.kw_at,
      //TODO other keywords?
    ),

    kw_at: $ => '@',
  }
});
