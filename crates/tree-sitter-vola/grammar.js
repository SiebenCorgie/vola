module.exports = grammar({
  name: 'vola',
  rules: {

    source_file: $ => repeat(
      choice(
        $.comment,
        $.df_alias,
        $.alge_decl,
        $.field_decl
      )
    ),

//Field DSL
//=============================================

    field_decl: $ => seq(
      "field",
      $.identifier,
      "(",
      optional($.arg_list),
      ")",
      "{",
      $.subtree,
      "}",
    ),

    subtree: $ => choice(
      $.csg_unary,
      $.csg_binary,
      $.csg_prim,
    ),

    csg_unary: $ => seq(
      $.identifier,
      "(",
      optional($.param_list),
      ")",
      "{",
      $.subtree,
      "}"
    ),

    csg_binary: $ => seq(
      $.identifier,
      "(",
      optional($.param_list),
      ")",
      "{",
      $.subtree,
      "}",
      "{",
      $.subtree,
      "}"
    ),


    csg_prim: $ => seq(
      $.identifier,
      "(",
      optional($.param_list),
      ")",
    ),


    param_list: $ => seq(
      repeat(seq(
        $.alge_expr,
        ","
      )),
      $.alge_expr
    ),


//Alge DSL
//=============================================
    

    //Some type of algebraic decleration.
    // We have `alge`, for simple algebraic declerations,
    // `prim` for a DF declartion,
    // and `op` for a (DF, DF, ...) -> DF operation.
    alge_decl: $ => seq(
      "alge",
      $.identifier,
      "(",
      optional($.arg_list),
      ")",
      "{",
      $.alge_region,
      "}",
    ),

    op_decl: $ => seq(
      "op",
      $.identifier,
      seq(
        "<",
        $.df_list,
        ">",
      ),
      "(",
      optional($.arg_list),
      ")",
      optional(seq(
        "->",
        $.t_df
      )),
      "{",
      $.alge_region,
      "}",
    ),

    prim_decl: $ => seq(
      "prim",
      $.identifier,
      "(",
      optional($.arg_list),
      ")",
      optional(seq(
        "->",
        $.t_df
      )),
      "{",
      $.alge_region,
      "}",
    ),

    arg_list: $ => seq(
      repeat(seq(
        $.typed_arg,
        ","
      )),
      $.typed_arg,
    ),

    df_list: $ => seq(
      repeat(seq(
        $.t_df,
        ','
      )),
      $.t_df
    ),

    typed_arg: $ => seq(
      $.identifier,
      ":",
      $.alge_type,
    ),

    alge_region: $ => seq(
      repeat($._alge_stmt),
      $.alge_expr,
    ),

    _alge_stmt: $ => choice(
      $.let_stmt,
      $.assign_stmt,
      $.comment
    ),

    let_stmt: $ => seq(
      "let",
      $.identifier,
      "=",
      $.alge_expr
    ),

    assign_stmt: $ => seq(
      $.identifier,
      ".",
      $.identifier,
      "=",
      $.alge_expr
    ),

    alge_expr: $ => choice(
      $.unary_expr,
      $.binary_expr,
      $.eval_expr,
      prec(6, $.alge_type_constructor),
      prec(5, seq('(', $.alge_expr, ')')),
      $.literal,
      $.identifier,
    ),

    eval_expr: $ => seq(
      "eval",
      $.identifier,
      "(",
      optional($.ident_list),
      ")"
    ),

    ident_list: $ => seq(
      repeat(seq(
        $.identifier,
        ","
      )),
      $.identifier,
    ),


    unary_expr: $ => prec.left(4,
      seq(
        $.alge_unary_op,
        $.alge_expr
      ),
    ),

    binary_expr: $ => prec.left(3,
      seq(
        $.alge_expr,
        $.alge_binary_op,
        $.alge_expr
      ),
    ),

    alge_unary_op: $ => choice(
      '-', '!'
    ),

    alge_binary_op: $ => choice(
      '/', '*', '+', '-', '%'
    ),

    type: $ => choice(
      $.alge_type,
      $.t_df,
    ),

    alge_type: $ => choice(
      $.t_scalar,
      $.t_vec,
      $.t_mat,
    ),

    alge_type_constructor: $ => seq(
      $.alge_type,
      "(",
      optional($.param_list),
      ")"
    ),

//Common defs
//=============================================

    df_alias: $ => seq(
      'type',
      $.identifier,
      '=',
      $.t_df,
    ),

    t_df: $ => choice(
      //an identifier set df type.
      $.identifier,
      seq('DF', '<', $.alge_type, '->', $.alge_type, '>'),
    ),

    t_scalar: $ => seq(
      's',
    ),

    t_vec: $ => seq(
      'vec',
      $.digit,
    ),

    t_mat: $ => seq(
      'mat',
      $.digit,
      'x',
      $.digit,
    ),


    literal: $ => choice(
      $.integer_literal,
      $.float_literal
    ),

    integer_literal: $ => $.digit,

    float_literal: $ => seq(
      $.digit,
      '.',
      $.digit
    ),

    digit: $ => /[0-9][0-9_]*/,
    identifier: $ => /([a-zA-Z$][a-zA-Z_]*)/,
    //identifier: _ => /(r#)?[_\p{XID_Start}][_\p{XID_Continue}]*/,

//All about comments
//=================

    comment: $ => token(seq(
      '//', /.*/
    )),
  }
});
