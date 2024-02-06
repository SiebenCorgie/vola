
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
        $.field_decl,
        $.field_export,
        $.def_concept,
        $.def_entity,
        $.def_operation,
        $.impl_block,
        $.ct_attrib,
      )
    ),

//the #[some(stuff)] syntax
    ct_attrib: $ => seq(
      "#",
      "[",
      $.fn_call,
      "]"
    ),


//Field DSL
//=============================================

    field_decl: $ => seq(
      "define",
      field("field_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      "{",
      repeat($._csg_stmt),
      //field decl must end on subtree
      $._subtree,
      "}",
    ),


    field_export: $ => seq(
      "export",
      field("field_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      "{",
      repeat($._csg_stmt),
      //export must end on access description
      $._access_desc,
      "}",
    ),

    _csg_stmt: $ => seq(
      choice(
        $.let_stmt,
        $.csg_binding,
      ),
      ";"
    ),

    csg_binding: $ => seq(
      "csg",
      field("csg_ident", $.identifier),
      "=",
      $._subtree
    ),

    _access_desc: $ => choice(
      seq(
        "(",
        repeat(seq(
          $.access_decl,
          ","
        )),
        $.access_decl,
        ")"
      ),
      //single access without tuple decl
      $.access_decl
    ),

    access_decl: $ => seq(
      field("field_name", $.identifier),
      ".",
      field("concept_access", $.fn_call),
    ),

    _subtree: $ => choice(
      $.csg_unary,
      $.csg_binary,
      $.fn_call,
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


//Alge DSL
//=============================================

// entity // concept // operation

    def_entity: $ => seq(
      "entity",
      field("entity_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      ";",
    ),


    def_concept: $ => seq(
      "concept",
      field("concept_name", $.identifier),
      ":",
      optional(field("concept_arg", $.alge_type)),
      "->",
      field("concept_result", $.alge_type),
      ";",
    ),


    def_operation: $ => seq(
      "operation",
      field("operation_name", $.identifier),
      "(",
      optional($._arg_list),
      ")",
      ";",
    ),

//impl syntax

    impl_block: $ => seq(
      "impl",
      field("op_or_entity", $.identifier),
      optional(seq(
        "<",
        field("operand", $._ident_list),
        ">"
      )),
      "for",
      field("concept", $.identifier),
      optional(seq(
        "(",
        $._ident_list,
        ")",
      )),
      "{",
      $.block,
      "}",
    ),

    _arg_list: $ => seq(
      repeat(seq(
        $.typed_arg,
        ","
      )),
      $.typed_arg,
    ),

    typed_arg: $ => seq(
      $.identifier,
      ":",
      $.alge_type,
    ),


    block: $ => seq(
      repeat(
        $._alge_stmt
      ),
      //must end on alge expr
      $.alge_expr
    ),

    _ident_list: $ => seq(
      repeat(seq(
        $.identifier,
        ","
      )),
      $.identifier
    ),

    _alge_stmt: $ => seq(
      choice(
        $.let_stmt,
        $.assign_stmt,
        $.dead_eval_stmt,
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
      "=",
      $.alge_expr
    ),

    dead_eval_stmt: $ => $.eval_expr,



    //TODO: add sum and mul loops?

    alge_expr: $ => prec.left(choice(
      $.unary_expr,
      $.binary_expr,
      $.eval_expr,
      seq('(', $.alge_expr, ')'),
      $._literal,
      $.identifier,
      $.fn_call,
      $.list,
    )),

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

    fn_call: $ => seq(
      field("function_name", $.identifier),
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

    _type: $ => choice(
      $.alge_type,
      "CSG",
    ),

    alge_type: $ => choice(
      $.t_scalar,
      $.t_vec,
      $.t_mat,
      $.t_tensor,
    ),


//Common defs
//=============================================

    list: $ => seq(
      "[",
      repeat(seq(
        $.alge_expr,
        ","
      )),
      $.alge_expr,
      "]"
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

    t_tensor: $ => seq(
      "tensor",
      "<",
      $._digit_list,
      ">"
    ),

    _digit_list: $ => seq(
      repeat(seq(
        $.digit,
        ","
      )),
      $.digit
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
    //identifier: $ => /([a-zA-Z$][a-zA-Z_]*)/,
    identifier: _ => /[_\p{XID_Start}][_\p{XID_Continue}]*/,
//All about comments
//=================

    comment: $ => token(seq(
      '//', /.*/
    )),
  }
});
