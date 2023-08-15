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
      $.field_definition,
      $.comment,
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
      $._ident_iter,
      '>'
    ),

    _ident_iter: $ => choice(
      $.identifier,
      seq(
        $.identifier,
        ',',
        $._ident_iter
      )
    ),

    //Generic parameter list
    parameter_list: $ => seq(
      '(',
      optional($._param_iter),
      ')'
    ),


    _param_iter: $ => choice(
      $.typed_identifier,
      seq(
        $.typed_identifier,
        ',',
        $._param_iter,
      )
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
      $.assignment_stmt,
      $.comment,
    ),

// Primitive expressions
//=================

    //An expression that defines an prim somehow.
    //this is either a op-tree on primitives, or a primitive itself
    prim_expr: $ => choice(
      //name of a primitive
      $.identifier,
      //adhoc creation
      $.call_expr,
      //Sub tree
      $.optree,
    ),

    //expression that defines a tree of ops on leaf primitives.
    //possible tree of primitive expressions
    optree: $ => seq(
      //op's identifier
      $.identifier,
      '<',
      //the primitives that are being modified, possibly nested.
      //Must be at least 1, since there are no ops on no primitives
      $._prim_list,
      '>',
      '(',
      //list of arithmetic arguments
      $._art_list,
      ')',
    ),


    _prim_list: $ => choice(
      $.prim_expr,
      seq(
        $.prim_expr,
        ',',
        $._prim_list
      ),
    ),


    //Statement that defines a primitive
    def_prim: $ => seq(
      $.kw_prim,
      $.identifier,
      //possibly initialize
      optional(
        seq(
        '=',
          $.prim_expr,
      )),
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
      $._sub_art_expr,
      //returns something from before,
      $.identifier,
      $.arg_access,
      $.float,
      $.kw_at,
      $.list,
    ),

    //Sub expression
    _sub_art_expr: $ => seq(
      '(',
      $._art_expr,
      ')'
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
      prec.left(1, seq($._art_expr, '%', $._art_expr)),
      // ...
    ),


    assignment_stmt: $ => choice(
      seq($.assignee, '=', $._art_expr, ';'),
      seq($.assignee, '+=', $._art_expr, ';'),
      seq($.assignee, '-=', $._art_expr, ';'),
      seq($.assignee, '*=', $._art_expr, ';'),
      seq($.assignee, '/=', $._art_expr, ';'),
    ),

    assignee: $ => choice(
      seq($.identifier, '.', $.identifier),
      seq($.kw_at),
      seq($.identifier, '.', $.kw_at),
    ),

    call_expr: $ => seq(
      $.identifier,
      '(',
      optional($._art_list),
      ')'
    ),


    _art_list: $ => choice(
      $._art_expr,
      seq(
        $._art_expr,
        ',',
        $._art_list
      )
    ),

    arg_access: $ => seq(
      $.identifier,
      '.',
      choice(
        $.identifier,
        $.kw_at
      )
    ),

//Types and keywords
//=================


    //any list
    list: $ => seq(
      '[',
      optional($._list_iter),
      ']'
    ),

    _list_iter: $ => choice(
      $._list_item,
      seq(
        $._list_item,
        ',',
        //rest of the list
        $._list_iter
      )
    ),

    _list_item: $ => choice(
      $.list,
      $.float,
      $.identifier,
      $.kw_at,
    ),


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
    kw_prim: $ => 'def',

//All about comments
//=================

    comment: $ => token(seq(
      '//', /.*/
    )),
  }
});
