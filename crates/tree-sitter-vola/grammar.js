
//Shamelessly taken from the RUST precedence rules. But we don't use all
const PREC = {
  call: 15,
  field: 14,
  try: 13,
  unary: 12,
  cast: 11,
  multiplicative: 10,
  additive: 9,
  shift: 8,
  bitand: 7,
  bitxor: 6,
  bitor: 5,
  comparative: 4,
  and: 3,
  or: 2,
  range: 1,
  assign: 0,
  closure: -1,
};

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
      field("field_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      "{",
      $._subtree,
      "}",
    ),

    _subtree: $ => choice(
      $.csg_unary,
      $.csg_binary,
      $.csg_prim,
    ),

    csg_unary: $ => seq(
      $.identifier,
      "(",
      optional($._param_list),
      ")",
      "{",
      $._subtree,
      "}"
    ),

    csg_binary: $ => seq(
      $.identifier,
      "(",
      optional($._param_list),
      ")",
      "{",
      $._subtree,
      "}",
      "{",
      $._subtree,
      "}"
    ),

    csg_prim: $ => seq(
      $.identifier,
      "(",
      optional($._param_list),
      ")",
    ),

    _param_list: $ => seq(
      repeat(seq(
        field("parameter", $.alge_expr),
        ","
      )),
      field("parameter", $.alge_expr)
    ),


//Alge DSL
//=============================================
    

    //Some type of algebraic decleration.
    // We have `alge`, for simple algebraic declerations,
    // `prim` for a DF declartion,
    // and `op` for a (DF, DF, ...) -> DF operation.
    alge_decl: $ => seq(
      "alge",
      field("alge_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      "{",
      $._alge_region,
      "}",
    ),

    op_decl: $ => seq(
      "op",
      field("op_name", $.identifier),
      seq(
        "<",
        $._df_list,
        ">",
      ),
      "(",
      optional($._arg_list),
      ")",
      optional(seq(
        "->",
        $.t_df
      )),
      "{",
      $._alge_region,
      "}",
    ),

    prim_decl: $ => seq(
      "prim",
      field("prim_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      optional(seq(
        "->",
        $.t_df
      )),
      "{",
      $._alge_region,
      "}",
    ),

    _arg_list: $ => seq(
      repeat(seq(
        $.typed_arg,
        ","
      )),
      $.typed_arg,
    ),

    _df_list: $ => seq(
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

    _alge_region: $ => seq(
      repeat(choice($._alge_stmt, $.comment)),
      $.alge_expr,
    ),

    _alge_stmt: $ => seq(
      choice(
        $.let_stmt,
        $.assign_stmt,
      ),
      ";"
    ),

    let_stmt: $ => seq(
      "let",
      $.identifier,
      "=",
      $.alge_expr,
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
      $.alge_type_constructor,
      seq('(', $.alge_expr, ')'),
      $._literal,
      $.identifier,
    ),

    eval_expr: $ => seq(
      "eval",
      $.identifier,
      "(",
      optional($._param_list),
      ")"
    ),

    unary_expr: $ => prec.left(PREC.unary, choice(
        seq("-", $.alge_expr),
        seq("!", $.alge_expr),
    )),

    binary_expr: $ => choice(
      prec.left(PREC.multiplicative, seq($.alge_expr, "/", $.alge_expr)),
      prec.left(PREC.multiplicative, seq($.alge_expr, "*", $.alge_expr)),
      prec.left(PREC.additive, seq($.alge_expr, "+", $.alge_expr)),
      prec.left(PREC.additive, seq($.alge_expr, "-", $.alge_expr)),
      prec.left(PREC.additive, seq($.alge_expr, "%", $.alge_expr)),
    ),

    _type: $ => choice(
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
      optional($._param_list),
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


    _literal: $ => choice(
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
