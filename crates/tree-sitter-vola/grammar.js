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
      repeat1($.identifier),
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

    //A block of some domain. Per definition we must always end with an primitive
    //expression
    block: $ => seq(
      '{',
      repeat($._stmt),
      $.prim_expr,
      '}'
    ),

    //Single statement. Either transforms a primitive (for instance attribute change),
    //defines a prim,
    //or calculates something arithmetic.
    _stmt: $ => choice(
      $.let_stmt,
      $.def_prim,
      $.set_prim,
      $.set_at,
    ),

// Primitive expressions
//=================

    //An expression that defines an prim somehow.
    //this is either a op-tree on primitives, or a primitive itself
    prim_expr: $ => choice(
      //name of a primitive
      $.identifier,
      $.optree,
    ),

    //expression that defines a tree of ops on leaf primitives.
    //possible tree of primitive expressions
    optree: $ => seq(
      //op's identifier
      $.identifier,
      '<',
      //the primitives that are being modified, possibly nested
      repeat($.prim_expr),
      '>',
      '(',
      //list of arithmetic arguments
      $._art_list,
      ')',
    ),

    //Statement that defines a primitive
    def_prim: $ => seq(
      $.kw_prim,
      $.identifier,
      //possibly initialize
      optional(seq(
        '=',
        $.identifier,
        '(',
        $._art_list,
        ')',
      )),
      ';'
    ),

    //Statement that sets an attribute of some primitive
    set_prim: $ => seq(
      $.identifier,
      '.',
      optional(choice(
        $.kw_at,
        $.identifier
      )),
      '=',
      //whatever is written on that attribute
      $._art_expr,
      ';'
    ),

    set_at: $ => seq(
      $.kw_at,
      '=',
      $._art_expr,
      ';'
    ),

// Arithmetic statements
//=================


    //Arithmetic let statement.
    //something like
    // let a = 4+5;
    let_stmt: $ => seq(
      'let',
      $.typed_identifier,
      '=',
      $._art_expr,
      ';'
    ),

    //either a direct arithmetic statement, or a
    //scoped one
    _art_expr: $ => choice(
      //Opens a new scope
      $.unary_expr,
      $.scoped_expr,
      $.binary_expr,
      $.call_expr,
      //returns something from before,
      $.identifier,
      $.float,
      $.kw_at,
    ),


    scoped_expr: $ => seq(
      '{',
      repeat($.let_stmt),
      $._art_expr,
      '}'
    ),

    unary_expr: $ => prec(3, choice(
      seq('-', $._art_expr),
      seq('!', $._art_expr),
      //TODO more?
    )),

    binary_expr: $ => choice(
      prec.left(2, seq($._art_expr, '/', $._art_expr)),
      prec.left(2, seq($._art_expr, '*', $._art_expr)),
      prec.left(1, seq($._art_expr, '+', $._art_expr)),
      prec.left(1, seq($._art_expr, '-', $._art_expr)),
      // ...
    ),

    call_expr: $ => seq(
      $.identifier,
      '(',
      optional($._art_list),
      ')'
    ),

    _art_list: $ => choice(

      //single expr
      $._art_expr,
      // list of 1..n (ident,) + last without ,
      seq(
        repeat(
          seq(
            $._art_expr,
            ','
          )
        ),
        $._art_expr,
      )
    ),


//Types and keywords
//=================

    _type: $ => choice(
      $.scalar,
      $.vec,
      $.mat,
    ),

    scalar: $ => seq(
      's',
    ),

    vec: $ => seq(
      'vec',
      $.number,
    ),

    mat: $ => seq(
      'mat',
      $.number,
      'x',
      $.number,
    ),

    identifier: $ => /[a-z_]+/,

    float: $ => seq(
      $.number,
      optional(seq(
        '.',
        $.number
      ))
    ),

    number: $ => /\d+/,

    keyword: $ => choice(
      $.kw_at,
      $.kw_prim,
      //TODO other keywords?
    ),

    kw_at: $ => '@',
    kw_prim: $ => 'def'
  }
});
