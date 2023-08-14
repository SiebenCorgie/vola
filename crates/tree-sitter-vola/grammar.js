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
      $.arit_block
    ),

    //op some_op<[prims]>([params]){[block]}
    op_definition: $ => seq(
      'op',
      $.identifier,
      $.prim_list,
      $.parameter_list,
      $.comb_block
    ),

    //field some_field([params]){[block]}
    field_definition: $ => seq(
      'field',
      $.identifier,
      $.parameter_list,
      $.combi_block
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


    combi_block: $ => seq(
      '{',
      repeat($._combi_stmt),
      //By definition, a combinatorical block needs to end with a identifier of the primitive that is being returned.
      $.identifier,
      '}'
    ),

    _combi_stmt: $ => choice(
      //either defines a primitive,
      $.define_prim,
      //combines primitives
      $.op_prim,
      //calculate something
      $.let_arit
    ),


    // looks like
    //
    // prim x = some_prim(a, b, c);
    //
    define_prim: $ => seq(
      $.kw_prim,
      $.identifier,
      '=',
      $.call,
      ';'
    ),

    //looks like
    //
    // some_op<x, y>(a,b);
    //


    $call: $ => seq(
      $identifier,
      '(',
      repeat(identifier),
      ')',
    ),

    arit_block: $ => seq(
      '{',
      repeat($.arit_stmt),

      '}'
    ),

    _arti_stmt: $ => choice(
      $.let_arit_stmt,
      $.unbound_arit_stmt,
    ),


    //Artimetic statement that bind some expression to a identivier.
    let_arit_stmt: $ => seq(
      'let',
      $.typed_identifier,
      '=',
      $._arit_expression,
      ';'
    ),


    unbound_arit_stmt: $ => seq(
      $._expression
    ),

    //Whenever we change from the combinatoric to the "lower" arithmetical level.
    _arithmetic_stmt: $ => choice(

    ),

    _scoped_stmts: $ => seq(
      '{',
      repeat($_arithmetic_stmt),
      '}',
      ';'
    )

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
      $.scalar
    ),

    scalar: $ => seq(
      's',
    ),

    vec: $ => seq(
      'vec',
      option($.number)
      'x',
      $.number,
    ),

    mat: $ => seq(
      'mat',
      $.number
      'x',
      $.number,
    ),

    identifier: $ => /[a-z]+/,

    number: $ => /\d+/,

    keyword: $ => choice(
      $.kw_at,
      $.kw_prim,
      //TODO other keywords?
    ),

    kw_at: $ => '@',
    kw_prim: $ => 'prim'
  }
});
