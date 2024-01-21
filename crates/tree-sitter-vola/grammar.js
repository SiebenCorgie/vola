module.exports = grammar({
  name: 'vola',
  rules: {

    source_file: $ => `todo`,


    unary_expr: $ => prec(4, choice(
      seq('-', $._alge_expr),
      seq('!', $._alge_expr),
      //TODO more?
    )),

    binary_expr: $ => choice(
      prec.left(3, seq($._alge_expr, '/', $._alge_expr)),
      prec.left(3, seq($._alge_expr, '*', $._alge_expr)),
      prec.left(2, seq($._alge_expr, '+', $._alge_expr)),
      prec.left(2, seq($._alge_expr, '-', $._alge_expr)),
      prec.left(2, seq($._alge_expr, '%', $._alge_expr)),
      // ...
    ),


    assignment_stmt: $ => choice(
      seq($.assignee, '=', $._alge_expr, ';'),
      seq($.assignee, '+=', $._alge_expr, ';'),
      seq($.assignee, '-=', $._alge_expr, ';'),
      seq($.assignee, '*=', $._alge_expr, ';'),
      seq($.assignee, '/=', $._alge_expr, ';'),
    ),

    assignee: $ => choice(
      seq($.identifier, '.', $.identifier),
      seq($.kw_at),
      seq($.identifier, '.', $.kw_at),
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
      $.digit,
    ),

    mat: $ => seq(
      'mat',
      $.digit,
      'x',
      $.digit,
    ),

    identifier: $ => /[a-z_]+/,

    float: $ => seq(
      $.digit,
      optional(
        seq(
          '.',
          $.digit,
        )
      ),
    ),

    digit: $ => /\d+/,


//All about comments
//=================

    comment: $ => token(seq(
      '//', /.*/
    )),
  }
});
