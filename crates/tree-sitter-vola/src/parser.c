#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 120
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 66
#define ALIAS_COUNT 0
#define TOKEN_COUNT 34
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 10
#define PRODUCTION_ID_COUNT 8

enum {
  anon_sym_field = 1,
  anon_sym_LPAREN = 2,
  anon_sym_RPAREN = 3,
  anon_sym_LBRACE = 4,
  anon_sym_RBRACE = 5,
  anon_sym_COMMA = 6,
  anon_sym_alge = 7,
  anon_sym_op = 8,
  anon_sym_LT = 9,
  anon_sym_GT = 10,
  anon_sym_DASH_GT = 11,
  anon_sym_prim = 12,
  anon_sym_COLON = 13,
  anon_sym_SEMI = 14,
  anon_sym_let = 15,
  anon_sym_EQ = 16,
  anon_sym_DOT = 17,
  anon_sym_eval = 18,
  anon_sym_DASH = 19,
  anon_sym_BANG = 20,
  anon_sym_SLASH = 21,
  anon_sym_STAR = 22,
  anon_sym_PLUS = 23,
  anon_sym_PERCENT = 24,
  anon_sym_type = 25,
  anon_sym_DF = 26,
  anon_sym_s = 27,
  anon_sym_vec = 28,
  anon_sym_mat = 29,
  anon_sym_x = 30,
  sym_digit = 31,
  sym_identifier = 32,
  sym_comment = 33,
  sym_source_file = 34,
  sym_field_decl = 35,
  sym__subtree = 36,
  sym_csg_unary = 37,
  sym_csg_binary = 38,
  sym_csg_prim = 39,
  sym__param_list = 40,
  sym_alge_decl = 41,
  sym__arg_list = 42,
  sym_typed_arg = 43,
  sym__alge_region = 44,
  sym__alge_stmt = 45,
  sym_let_stmt = 46,
  sym_assign_stmt = 47,
  sym_alge_expr = 48,
  sym_eval_expr = 49,
  sym_unary_expr = 50,
  sym_binary_expr = 51,
  sym_alge_type = 52,
  sym_alge_type_constructor = 53,
  sym_df_alias = 54,
  sym_t_df = 55,
  sym_t_scalar = 56,
  sym_t_vec = 57,
  sym_t_mat = 58,
  sym__literal = 59,
  sym_integer_literal = 60,
  sym_float_literal = 61,
  aux_sym_source_file_repeat1 = 62,
  aux_sym__param_list_repeat1 = 63,
  aux_sym__arg_list_repeat1 = 64,
  aux_sym__alge_region_repeat1 = 65,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_field] = "field",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_COMMA] = ",",
  [anon_sym_alge] = "alge",
  [anon_sym_op] = "op",
  [anon_sym_LT] = "<",
  [anon_sym_GT] = ">",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_prim] = "prim",
  [anon_sym_COLON] = ":",
  [anon_sym_SEMI] = ";",
  [anon_sym_let] = "let",
  [anon_sym_EQ] = "=",
  [anon_sym_DOT] = ".",
  [anon_sym_eval] = "eval",
  [anon_sym_DASH] = "-",
  [anon_sym_BANG] = "!",
  [anon_sym_SLASH] = "/",
  [anon_sym_STAR] = "*",
  [anon_sym_PLUS] = "+",
  [anon_sym_PERCENT] = "%",
  [anon_sym_type] = "type",
  [anon_sym_DF] = "DF",
  [anon_sym_s] = "s",
  [anon_sym_vec] = "vec",
  [anon_sym_mat] = "mat",
  [anon_sym_x] = "x",
  [sym_digit] = "digit",
  [sym_identifier] = "identifier",
  [sym_comment] = "comment",
  [sym_source_file] = "source_file",
  [sym_field_decl] = "field_decl",
  [sym__subtree] = "_subtree",
  [sym_csg_unary] = "csg_unary",
  [sym_csg_binary] = "csg_binary",
  [sym_csg_prim] = "csg_prim",
  [sym__param_list] = "_param_list",
  [sym_alge_decl] = "alge_decl",
  [sym__arg_list] = "_arg_list",
  [sym_typed_arg] = "typed_arg",
  [sym__alge_region] = "_alge_region",
  [sym__alge_stmt] = "_alge_stmt",
  [sym_let_stmt] = "let_stmt",
  [sym_assign_stmt] = "assign_stmt",
  [sym_alge_expr] = "alge_expr",
  [sym_eval_expr] = "eval_expr",
  [sym_unary_expr] = "unary_expr",
  [sym_binary_expr] = "binary_expr",
  [sym_alge_type] = "alge_type",
  [sym_alge_type_constructor] = "alge_type_constructor",
  [sym_df_alias] = "df_alias",
  [sym_t_df] = "t_df",
  [sym_t_scalar] = "t_scalar",
  [sym_t_vec] = "t_vec",
  [sym_t_mat] = "t_mat",
  [sym__literal] = "_literal",
  [sym_integer_literal] = "integer_literal",
  [sym_float_literal] = "float_literal",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym__param_list_repeat1] = "_param_list_repeat1",
  [aux_sym__arg_list_repeat1] = "_arg_list_repeat1",
  [aux_sym__alge_region_repeat1] = "_alge_region_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_field] = anon_sym_field,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_alge] = anon_sym_alge,
  [anon_sym_op] = anon_sym_op,
  [anon_sym_LT] = anon_sym_LT,
  [anon_sym_GT] = anon_sym_GT,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_prim] = anon_sym_prim,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_eval] = anon_sym_eval,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_PERCENT] = anon_sym_PERCENT,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_DF] = anon_sym_DF,
  [anon_sym_s] = anon_sym_s,
  [anon_sym_vec] = anon_sym_vec,
  [anon_sym_mat] = anon_sym_mat,
  [anon_sym_x] = anon_sym_x,
  [sym_digit] = sym_digit,
  [sym_identifier] = sym_identifier,
  [sym_comment] = sym_comment,
  [sym_source_file] = sym_source_file,
  [sym_field_decl] = sym_field_decl,
  [sym__subtree] = sym__subtree,
  [sym_csg_unary] = sym_csg_unary,
  [sym_csg_binary] = sym_csg_binary,
  [sym_csg_prim] = sym_csg_prim,
  [sym__param_list] = sym__param_list,
  [sym_alge_decl] = sym_alge_decl,
  [sym__arg_list] = sym__arg_list,
  [sym_typed_arg] = sym_typed_arg,
  [sym__alge_region] = sym__alge_region,
  [sym__alge_stmt] = sym__alge_stmt,
  [sym_let_stmt] = sym_let_stmt,
  [sym_assign_stmt] = sym_assign_stmt,
  [sym_alge_expr] = sym_alge_expr,
  [sym_eval_expr] = sym_eval_expr,
  [sym_unary_expr] = sym_unary_expr,
  [sym_binary_expr] = sym_binary_expr,
  [sym_alge_type] = sym_alge_type,
  [sym_alge_type_constructor] = sym_alge_type_constructor,
  [sym_df_alias] = sym_df_alias,
  [sym_t_df] = sym_t_df,
  [sym_t_scalar] = sym_t_scalar,
  [sym_t_vec] = sym_t_vec,
  [sym_t_mat] = sym_t_mat,
  [sym__literal] = sym__literal,
  [sym_integer_literal] = sym_integer_literal,
  [sym_float_literal] = sym_float_literal,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym__param_list_repeat1] = aux_sym__param_list_repeat1,
  [aux_sym__arg_list_repeat1] = aux_sym__arg_list_repeat1,
  [aux_sym__alge_region_repeat1] = aux_sym__alge_region_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_field] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_alge] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_op] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_prim] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_eval] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PERCENT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_s] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_vec] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_mat] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_x] = {
    .visible = true,
    .named = false,
  },
  [sym_digit] = {
    .visible = true,
    .named = true,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_field_decl] = {
    .visible = true,
    .named = true,
  },
  [sym__subtree] = {
    .visible = false,
    .named = true,
  },
  [sym_csg_unary] = {
    .visible = true,
    .named = true,
  },
  [sym_csg_binary] = {
    .visible = true,
    .named = true,
  },
  [sym_csg_prim] = {
    .visible = true,
    .named = true,
  },
  [sym__param_list] = {
    .visible = false,
    .named = true,
  },
  [sym_alge_decl] = {
    .visible = true,
    .named = true,
  },
  [sym__arg_list] = {
    .visible = false,
    .named = true,
  },
  [sym_typed_arg] = {
    .visible = true,
    .named = true,
  },
  [sym__alge_region] = {
    .visible = false,
    .named = true,
  },
  [sym__alge_stmt] = {
    .visible = false,
    .named = true,
  },
  [sym_let_stmt] = {
    .visible = true,
    .named = true,
  },
  [sym_assign_stmt] = {
    .visible = true,
    .named = true,
  },
  [sym_alge_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_eval_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_binary_expr] = {
    .visible = true,
    .named = true,
  },
  [sym_alge_type] = {
    .visible = true,
    .named = true,
  },
  [sym_alge_type_constructor] = {
    .visible = true,
    .named = true,
  },
  [sym_df_alias] = {
    .visible = true,
    .named = true,
  },
  [sym_t_df] = {
    .visible = true,
    .named = true,
  },
  [sym_t_scalar] = {
    .visible = true,
    .named = true,
  },
  [sym_t_vec] = {
    .visible = true,
    .named = true,
  },
  [sym_t_mat] = {
    .visible = true,
    .named = true,
  },
  [sym__literal] = {
    .visible = false,
    .named = true,
  },
  [sym_integer_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_float_literal] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__param_list_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__arg_list_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__alge_region_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_alge_name = 1,
  field_field_name = 2,
  field_parameter = 3,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_alge_name] = "alge_name",
  [field_field_name] = "field_name",
  [field_parameter] = "parameter",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 1},
  [3] = {.index = 2, .length = 1},
  [4] = {.index = 3, .length = 1},
  [5] = {.index = 4, .length = 2},
  [6] = {.index = 6, .length = 2},
  [7] = {.index = 8, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_field_name, 1},
  [1] =
    {field_alge_name, 1},
  [2] =
    {field_parameter, 0},
  [3] =
    {field_parameter, 2, .inherited = true},
  [4] =
    {field_parameter, 0, .inherited = true},
    {field_parameter, 1},
  [6] =
    {field_parameter, 0, .inherited = true},
    {field_parameter, 1, .inherited = true},
  [8] =
    {field_parameter, 3, .inherited = true},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 48,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 69,
  [70] = 70,
  [71] = 71,
  [72] = 72,
  [73] = 73,
  [74] = 74,
  [75] = 75,
  [76] = 76,
  [77] = 77,
  [78] = 78,
  [79] = 79,
  [80] = 80,
  [81] = 81,
  [82] = 82,
  [83] = 83,
  [84] = 84,
  [85] = 85,
  [86] = 86,
  [87] = 87,
  [88] = 88,
  [89] = 89,
  [90] = 90,
  [91] = 91,
  [92] = 92,
  [93] = 93,
  [94] = 94,
  [95] = 95,
  [96] = 96,
  [97] = 97,
  [98] = 98,
  [99] = 99,
  [100] = 100,
  [101] = 101,
  [102] = 102,
  [103] = 103,
  [104] = 104,
  [105] = 105,
  [106] = 106,
  [107] = 107,
  [108] = 108,
  [109] = 109,
  [110] = 110,
  [111] = 111,
  [112] = 112,
  [113] = 113,
  [114] = 114,
  [115] = 115,
  [116] = 116,
  [117] = 117,
  [118] = 118,
  [119] = 119,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(32);
      if (lookahead == '!') ADVANCE(55);
      if (lookahead == '%') ADVANCE(60);
      if (lookahead == '(') ADVANCE(34);
      if (lookahead == ')') ADVANCE(35);
      if (lookahead == '*') ADVANCE(58);
      if (lookahead == '+') ADVANCE(59);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == '-') ADVANCE(54);
      if (lookahead == '.') ADVANCE(50);
      if (lookahead == '/') ADVANCE(57);
      if (lookahead == ':') ADVANCE(45);
      if (lookahead == ';') ADVANCE(46);
      if (lookahead == '<') ADVANCE(41);
      if (lookahead == '=') ADVANCE(49);
      if (lookahead == '>') ADVANCE(42);
      if (lookahead == 'D') ADVANCE(7);
      if (lookahead == 'a') ADVANCE(20);
      if (lookahead == 'e') ADVANCE(29);
      if (lookahead == 'f') ADVANCE(19);
      if (lookahead == 'l') ADVANCE(12);
      if (lookahead == 'm') ADVANCE(9);
      if (lookahead == 'o') ADVANCE(24);
      if (lookahead == 'p') ADVANCE(26);
      if (lookahead == 's') ADVANCE(64);
      if (lookahead == 't') ADVANCE(30);
      if (lookahead == 'v') ADVANCE(13);
      if (lookahead == 'x') ADVANCE(70);
      if (lookahead == '{') ADVANCE(36);
      if (lookahead == '}') ADVANCE(37);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(71);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(55);
      if (lookahead == '%') ADVANCE(60);
      if (lookahead == '(') ADVANCE(34);
      if (lookahead == ')') ADVANCE(35);
      if (lookahead == '*') ADVANCE(58);
      if (lookahead == '+') ADVANCE(59);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == '-') ADVANCE(53);
      if (lookahead == '.') ADVANCE(50);
      if (lookahead == '/') ADVANCE(56);
      if (lookahead == ';') ADVANCE(46);
      if (lookahead == 'e') ADVANCE(81);
      if (lookahead == 'm') ADVANCE(74);
      if (lookahead == 's') ADVANCE(65);
      if (lookahead == 'v') ADVANCE(77);
      if (lookahead == '}') ADVANCE(37);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(71);
      if (lookahead == '$' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(55);
      if (lookahead == '(') ADVANCE(34);
      if (lookahead == '-') ADVANCE(53);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == 'e') ADVANCE(81);
      if (lookahead == 'l') ADVANCE(76);
      if (lookahead == 'm') ADVANCE(74);
      if (lookahead == 's') ADVANCE(65);
      if (lookahead == 'v') ADVANCE(77);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(71);
      if (lookahead == '$' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 3:
      if (lookahead == ')') ADVANCE(35);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (lookahead == '$' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 4:
      if (lookahead == '/') ADVANCE(83);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(43);
      END_STATE();
    case 6:
      if (lookahead == 'D') ADVANCE(72);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(6)
      if (lookahead == '$' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 7:
      if (lookahead == 'F') ADVANCE(62);
      END_STATE();
    case 8:
      if (lookahead == 'a') ADVANCE(21);
      END_STATE();
    case 9:
      if (lookahead == 'a') ADVANCE(28);
      END_STATE();
    case 10:
      if (lookahead == 'c') ADVANCE(66);
      END_STATE();
    case 11:
      if (lookahead == 'd') ADVANCE(33);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(27);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(10);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(39);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(61);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(22);
      END_STATE();
    case 17:
      if (lookahead == 'g') ADVANCE(14);
      END_STATE();
    case 18:
      if (lookahead == 'i') ADVANCE(23);
      END_STATE();
    case 19:
      if (lookahead == 'i') ADVANCE(16);
      END_STATE();
    case 20:
      if (lookahead == 'l') ADVANCE(17);
      END_STATE();
    case 21:
      if (lookahead == 'l') ADVANCE(51);
      END_STATE();
    case 22:
      if (lookahead == 'l') ADVANCE(11);
      END_STATE();
    case 23:
      if (lookahead == 'm') ADVANCE(44);
      END_STATE();
    case 24:
      if (lookahead == 'p') ADVANCE(40);
      END_STATE();
    case 25:
      if (lookahead == 'p') ADVANCE(15);
      END_STATE();
    case 26:
      if (lookahead == 'r') ADVANCE(18);
      END_STATE();
    case 27:
      if (lookahead == 't') ADVANCE(47);
      END_STATE();
    case 28:
      if (lookahead == 't') ADVANCE(68);
      END_STATE();
    case 29:
      if (lookahead == 'v') ADVANCE(8);
      END_STATE();
    case 30:
      if (lookahead == 'y') ADVANCE(25);
      END_STATE();
    case 31:
      if (eof) ADVANCE(32);
      if (lookahead == '(') ADVANCE(34);
      if (lookahead == ')') ADVANCE(35);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '/') ADVANCE(4);
      if (lookahead == '>') ADVANCE(42);
      if (lookahead == 'a') ADVANCE(20);
      if (lookahead == 'f') ADVANCE(19);
      if (lookahead == 't') ADVANCE(30);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(31)
      END_STATE();
    case 32:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_field);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_alge);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_op);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_LT);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_GT);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_prim);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_let);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_eval);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_eval);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(43);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_SLASH);
      if (lookahead == '/') ADVANCE(83);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_DF);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_DF);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_s);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_s);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_vec);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_vec);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_mat);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_mat);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_x);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_digit);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(71);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'F') ADVANCE(63);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(78);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(80);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(67);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(79);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(75);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(52);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(48);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(69);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'v') ADVANCE(73);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(sym_identifier);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(82);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(83);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 31},
  [2] = {.lex_state = 2},
  [3] = {.lex_state = 2},
  [4] = {.lex_state = 2},
  [5] = {.lex_state = 1},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 1},
  [12] = {.lex_state = 1},
  [13] = {.lex_state = 1},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 2},
  [17] = {.lex_state = 2},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 31},
  [23] = {.lex_state = 1},
  [24] = {.lex_state = 1},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 1},
  [27] = {.lex_state = 1},
  [28] = {.lex_state = 1},
  [29] = {.lex_state = 31},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 1},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 1},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 1},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 1},
  [38] = {.lex_state = 1},
  [39] = {.lex_state = 1},
  [40] = {.lex_state = 1},
  [41] = {.lex_state = 1},
  [42] = {.lex_state = 1},
  [43] = {.lex_state = 1},
  [44] = {.lex_state = 3},
  [45] = {.lex_state = 3},
  [46] = {.lex_state = 3},
  [47] = {.lex_state = 31},
  [48] = {.lex_state = 3},
  [49] = {.lex_state = 3},
  [50] = {.lex_state = 31},
  [51] = {.lex_state = 31},
  [52] = {.lex_state = 3},
  [53] = {.lex_state = 3},
  [54] = {.lex_state = 31},
  [55] = {.lex_state = 31},
  [56] = {.lex_state = 3},
  [57] = {.lex_state = 31},
  [58] = {.lex_state = 31},
  [59] = {.lex_state = 31},
  [60] = {.lex_state = 31},
  [61] = {.lex_state = 31},
  [62] = {.lex_state = 31},
  [63] = {.lex_state = 6},
  [64] = {.lex_state = 3},
  [65] = {.lex_state = 3},
  [66] = {.lex_state = 0},
  [67] = {.lex_state = 0},
  [68] = {.lex_state = 0},
  [69] = {.lex_state = 0},
  [70] = {.lex_state = 0},
  [71] = {.lex_state = 0},
  [72] = {.lex_state = 0},
  [73] = {.lex_state = 0},
  [74] = {.lex_state = 0},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 0},
  [77] = {.lex_state = 0},
  [78] = {.lex_state = 3},
  [79] = {.lex_state = 0},
  [80] = {.lex_state = 0},
  [81] = {.lex_state = 0},
  [82] = {.lex_state = 3},
  [83] = {.lex_state = 31},
  [84] = {.lex_state = 0},
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 0},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 3},
  [89] = {.lex_state = 3},
  [90] = {.lex_state = 0},
  [91] = {.lex_state = 0},
  [92] = {.lex_state = 0},
  [93] = {.lex_state = 0},
  [94] = {.lex_state = 0},
  [95] = {.lex_state = 0},
  [96] = {.lex_state = 0},
  [97] = {.lex_state = 0},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 3},
  [100] = {.lex_state = 0},
  [101] = {.lex_state = 0},
  [102] = {.lex_state = 0},
  [103] = {.lex_state = 0},
  [104] = {.lex_state = 0},
  [105] = {.lex_state = 0},
  [106] = {.lex_state = 0},
  [107] = {.lex_state = 0},
  [108] = {.lex_state = 0},
  [109] = {.lex_state = 0},
  [110] = {.lex_state = 0},
  [111] = {.lex_state = 0},
  [112] = {.lex_state = 0},
  [113] = {.lex_state = 0},
  [114] = {.lex_state = 3},
  [115] = {.lex_state = 0},
  [116] = {.lex_state = 3},
  [117] = {.lex_state = 0},
  [118] = {.lex_state = 0},
  [119] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_field] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_alge] = ACTIONS(1),
    [anon_sym_op] = ACTIONS(1),
    [anon_sym_LT] = ACTIONS(1),
    [anon_sym_GT] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_prim] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_eval] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_PERCENT] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_DF] = ACTIONS(1),
    [anon_sym_s] = ACTIONS(1),
    [anon_sym_vec] = ACTIONS(1),
    [anon_sym_mat] = ACTIONS(1),
    [anon_sym_x] = ACTIONS(1),
    [sym_digit] = ACTIONS(1),
    [sym_comment] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(113),
    [sym_field_decl] = STATE(22),
    [sym_alge_decl] = STATE(22),
    [sym_df_alias] = STATE(22),
    [aux_sym_source_file_repeat1] = STATE(22),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_field] = ACTIONS(5),
    [anon_sym_alge] = ACTIONS(7),
    [anon_sym_type] = ACTIONS(9),
    [sym_comment] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 17,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(29), 1,
      sym_identifier,
    ACTIONS(31), 1,
      sym_comment,
    STATE(40), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    STATE(87), 1,
      sym__alge_region,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(4), 2,
      sym__alge_stmt,
      aux_sym__alge_region_repeat1,
    STATE(85), 2,
      sym_let_stmt,
      sym_assign_stmt,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [63] = 17,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(29), 1,
      sym_identifier,
    ACTIONS(31), 1,
      sym_comment,
    STATE(40), 1,
      sym_alge_expr,
    STATE(74), 1,
      sym__alge_region,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(4), 2,
      sym__alge_stmt,
      aux_sym__alge_region_repeat1,
    STATE(85), 2,
      sym_let_stmt,
      sym_assign_stmt,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [126] = 16,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(29), 1,
      sym_identifier,
    ACTIONS(33), 1,
      sym_comment,
    STATE(43), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(16), 2,
      sym__alge_stmt,
      aux_sym__alge_region_repeat1,
    STATE(85), 2,
      sym_let_stmt,
      sym_assign_stmt,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [186] = 15,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(35), 1,
      anon_sym_RPAREN,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(8), 1,
      aux_sym__param_list_repeat1,
    STATE(35), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    STATE(102), 1,
      sym__param_list,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [241] = 15,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(39), 1,
      anon_sym_RPAREN,
    STATE(8), 1,
      aux_sym__param_list_repeat1,
    STATE(35), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    STATE(90), 1,
      sym__param_list,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [296] = 15,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(41), 1,
      anon_sym_RPAREN,
    STATE(8), 1,
      aux_sym__param_list_repeat1,
    STATE(35), 1,
      sym_alge_expr,
    STATE(77), 1,
      sym__param_list,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [351] = 13,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(9), 1,
      aux_sym__param_list_repeat1,
    STATE(33), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [400] = 13,
    ACTIONS(43), 1,
      anon_sym_LPAREN,
    ACTIONS(46), 1,
      anon_sym_eval,
    ACTIONS(52), 1,
      anon_sym_s,
    ACTIONS(55), 1,
      anon_sym_vec,
    ACTIONS(58), 1,
      anon_sym_mat,
    ACTIONS(61), 1,
      sym_digit,
    ACTIONS(64), 1,
      sym_identifier,
    STATE(9), 1,
      aux_sym__param_list_repeat1,
    STATE(41), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(49), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [449] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(42), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [495] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(26), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [541] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(38), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [587] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(39), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [633] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(25), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [679] = 12,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_eval,
    ACTIONS(21), 1,
      anon_sym_s,
    ACTIONS(23), 1,
      anon_sym_vec,
    ACTIONS(25), 1,
      anon_sym_mat,
    ACTIONS(27), 1,
      sym_digit,
    ACTIONS(37), 1,
      sym_identifier,
    STATE(23), 1,
      sym_alge_expr,
    STATE(84), 1,
      sym_alge_type,
    ACTIONS(19), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
    STATE(31), 7,
      sym_eval_expr,
      sym_unary_expr,
      sym_binary_expr,
      sym_alge_type_constructor,
      sym__literal,
      sym_integer_literal,
      sym_float_literal,
  [725] = 7,
    ACTIONS(69), 1,
      anon_sym_let,
    ACTIONS(74), 1,
      sym_identifier,
    ACTIONS(77), 1,
      sym_comment,
    STATE(16), 2,
      sym__alge_stmt,
      aux_sym__alge_region_repeat1,
    STATE(85), 2,
      sym_let_stmt,
      sym_assign_stmt,
    ACTIONS(67), 4,
      anon_sym_LPAREN,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_digit,
    ACTIONS(72), 4,
      anon_sym_eval,
      anon_sym_s,
      anon_sym_vec,
      anon_sym_mat,
  [755] = 2,
    ACTIONS(80), 5,
      anon_sym_LPAREN,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_digit,
      sym_comment,
    ACTIONS(82), 6,
      anon_sym_let,
      anon_sym_eval,
      anon_sym_s,
      anon_sym_vec,
      anon_sym_mat,
      sym_identifier,
  [771] = 2,
    ACTIONS(86), 1,
      anon_sym_DOT,
    ACTIONS(84), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [786] = 2,
    ACTIONS(88), 4,
      anon_sym_LPAREN,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_digit,
    ACTIONS(90), 5,
      anon_sym_eval,
      anon_sym_s,
      anon_sym_vec,
      anon_sym_mat,
      sym_identifier,
  [800] = 1,
    ACTIONS(92), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [812] = 1,
    ACTIONS(94), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [824] = 6,
    ACTIONS(5), 1,
      anon_sym_field,
    ACTIONS(7), 1,
      anon_sym_alge,
    ACTIONS(9), 1,
      anon_sym_type,
    ACTIONS(96), 1,
      ts_builtin_sym_end,
    ACTIONS(98), 1,
      sym_comment,
    STATE(29), 4,
      sym_field_decl,
      sym_alge_decl,
      sym_df_alias,
      aux_sym_source_file_repeat1,
  [846] = 2,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(100), 7,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [860] = 1,
    ACTIONS(104), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [872] = 1,
    ACTIONS(100), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [884] = 1,
    ACTIONS(106), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [896] = 1,
    ACTIONS(108), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [908] = 1,
    ACTIONS(110), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [920] = 6,
    ACTIONS(112), 1,
      ts_builtin_sym_end,
    ACTIONS(114), 1,
      anon_sym_field,
    ACTIONS(117), 1,
      anon_sym_alge,
    ACTIONS(120), 1,
      anon_sym_type,
    ACTIONS(123), 1,
      sym_comment,
    STATE(29), 4,
      sym_field_decl,
      sym_alge_decl,
      sym_df_alias,
      aux_sym_source_file_repeat1,
  [942] = 1,
    ACTIONS(126), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [954] = 1,
    ACTIONS(128), 9,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_COMMA,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [966] = 5,
    ACTIONS(130), 1,
      anon_sym_s,
    ACTIONS(132), 1,
      anon_sym_vec,
    ACTIONS(134), 1,
      anon_sym_mat,
    STATE(83), 1,
      sym_alge_type,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
  [984] = 4,
    ACTIONS(136), 1,
      anon_sym_RPAREN,
    ACTIONS(138), 1,
      anon_sym_COMMA,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1000] = 5,
    ACTIONS(130), 1,
      anon_sym_s,
    ACTIONS(132), 1,
      anon_sym_vec,
    ACTIONS(134), 1,
      anon_sym_mat,
    STATE(67), 1,
      sym_alge_type,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
  [1018] = 4,
    ACTIONS(138), 1,
      anon_sym_COMMA,
    ACTIONS(142), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1034] = 5,
    ACTIONS(130), 1,
      anon_sym_s,
    ACTIONS(132), 1,
      anon_sym_vec,
    ACTIONS(134), 1,
      anon_sym_mat,
    STATE(93), 1,
      sym_alge_type,
    STATE(47), 3,
      sym_t_scalar,
      sym_t_vec,
      sym_t_mat,
  [1052] = 2,
    ACTIONS(144), 1,
      anon_sym_DOT,
    ACTIONS(128), 6,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1064] = 3,
    ACTIONS(146), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1077] = 3,
    ACTIONS(148), 1,
      anon_sym_SEMI,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1090] = 3,
    ACTIONS(150), 1,
      anon_sym_RBRACE,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1103] = 3,
    ACTIONS(138), 1,
      anon_sym_COMMA,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1116] = 3,
    ACTIONS(152), 1,
      anon_sym_SEMI,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1129] = 3,
    ACTIONS(154), 1,
      anon_sym_RBRACE,
    ACTIONS(102), 2,
      anon_sym_SLASH,
      anon_sym_STAR,
    ACTIONS(140), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_PERCENT,
  [1142] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(106), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1152] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(112), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1162] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(115), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1172] = 1,
    ACTIONS(158), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_GT,
      anon_sym_DASH_GT,
  [1180] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(75), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1190] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(118), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1200] = 1,
    ACTIONS(160), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1208] = 1,
    ACTIONS(162), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_GT,
      anon_sym_DASH_GT,
  [1216] = 5,
    ACTIONS(164), 1,
      anon_sym_RPAREN,
    ACTIONS(166), 1,
      sym_identifier,
    STATE(64), 1,
      aux_sym__arg_list_repeat1,
    STATE(66), 1,
      sym_typed_arg,
    STATE(105), 1,
      sym__arg_list,
  [1232] = 5,
    ACTIONS(166), 1,
      sym_identifier,
    ACTIONS(168), 1,
      anon_sym_RPAREN,
    STATE(64), 1,
      aux_sym__arg_list_repeat1,
    STATE(66), 1,
      sym_typed_arg,
    STATE(103), 1,
      sym__arg_list,
  [1248] = 1,
    ACTIONS(170), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1256] = 1,
    ACTIONS(172), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_GT,
      anon_sym_DASH_GT,
  [1264] = 2,
    ACTIONS(156), 1,
      sym_identifier,
    STATE(96), 4,
      sym__subtree,
      sym_csg_unary,
      sym_csg_binary,
      sym_csg_prim,
  [1274] = 1,
    ACTIONS(174), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1282] = 1,
    ACTIONS(176), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_GT,
      anon_sym_DASH_GT,
  [1290] = 1,
    ACTIONS(178), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1298] = 1,
    ACTIONS(180), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1306] = 1,
    ACTIONS(182), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1314] = 1,
    ACTIONS(184), 5,
      ts_builtin_sym_end,
      anon_sym_field,
      anon_sym_alge,
      anon_sym_type,
      sym_comment,
  [1322] = 3,
    ACTIONS(186), 1,
      anon_sym_DF,
    ACTIONS(188), 1,
      sym_identifier,
    STATE(59), 1,
      sym_t_df,
  [1332] = 3,
    ACTIONS(166), 1,
      sym_identifier,
    STATE(65), 1,
      aux_sym__arg_list_repeat1,
    STATE(68), 1,
      sym_typed_arg,
  [1342] = 3,
    ACTIONS(190), 1,
      sym_identifier,
    STATE(65), 1,
      aux_sym__arg_list_repeat1,
    STATE(92), 1,
      sym_typed_arg,
  [1352] = 2,
    ACTIONS(193), 1,
      anon_sym_RPAREN,
    ACTIONS(195), 1,
      anon_sym_COMMA,
  [1359] = 1,
    ACTIONS(197), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1364] = 2,
    ACTIONS(195), 1,
      anon_sym_COMMA,
    ACTIONS(199), 1,
      anon_sym_RPAREN,
  [1371] = 2,
    ACTIONS(201), 1,
      anon_sym_LBRACE,
    ACTIONS(203), 1,
      anon_sym_RBRACE,
  [1378] = 2,
    ACTIONS(205), 1,
      anon_sym_LBRACE,
    ACTIONS(207), 1,
      anon_sym_RBRACE,
  [1385] = 2,
    ACTIONS(209), 1,
      anon_sym_LBRACE,
    ACTIONS(211), 1,
      anon_sym_RBRACE,
  [1392] = 2,
    ACTIONS(213), 1,
      anon_sym_LBRACE,
    ACTIONS(215), 1,
      anon_sym_RBRACE,
  [1399] = 1,
    ACTIONS(217), 1,
      anon_sym_x,
  [1403] = 1,
    ACTIONS(219), 1,
      anon_sym_RBRACE,
  [1407] = 1,
    ACTIONS(221), 1,
      anon_sym_RBRACE,
  [1411] = 1,
    ACTIONS(223), 1,
      sym_digit,
  [1415] = 1,
    ACTIONS(225), 1,
      anon_sym_RPAREN,
  [1419] = 1,
    ACTIONS(227), 1,
      sym_identifier,
  [1423] = 1,
    ACTIONS(229), 1,
      anon_sym_EQ,
  [1427] = 1,
    ACTIONS(231), 1,
      sym_digit,
  [1431] = 1,
    ACTIONS(233), 1,
      anon_sym_LPAREN,
  [1435] = 1,
    ACTIONS(235), 1,
      sym_identifier,
  [1439] = 1,
    ACTIONS(237), 1,
      anon_sym_DASH_GT,
  [1443] = 1,
    ACTIONS(239), 1,
      anon_sym_LPAREN,
  [1447] = 1,
    ACTIONS(241), 1,
      anon_sym_SEMI,
  [1451] = 1,
    ACTIONS(243), 1,
      anon_sym_EQ,
  [1455] = 1,
    ACTIONS(245), 1,
      anon_sym_RBRACE,
  [1459] = 1,
    ACTIONS(247), 1,
      sym_identifier,
  [1463] = 1,
    ACTIONS(249), 1,
      sym_identifier,
  [1467] = 1,
    ACTIONS(251), 1,
      anon_sym_RPAREN,
  [1471] = 1,
    ACTIONS(144), 1,
      anon_sym_DOT,
  [1475] = 1,
    ACTIONS(195), 1,
      anon_sym_COMMA,
  [1479] = 1,
    ACTIONS(253), 1,
      anon_sym_GT,
  [1483] = 1,
    ACTIONS(255), 1,
      sym_digit,
  [1487] = 1,
    ACTIONS(257), 1,
      sym_digit,
  [1491] = 1,
    ACTIONS(259), 1,
      anon_sym_RBRACE,
  [1495] = 1,
    ACTIONS(261), 1,
      anon_sym_LPAREN,
  [1499] = 1,
    ACTIONS(263), 1,
      anon_sym_LBRACE,
  [1503] = 1,
    ACTIONS(265), 1,
      sym_identifier,
  [1507] = 1,
    ACTIONS(267), 1,
      anon_sym_LBRACE,
  [1511] = 1,
    ACTIONS(269), 1,
      anon_sym_LT,
  [1515] = 1,
    ACTIONS(271), 1,
      anon_sym_RPAREN,
  [1519] = 1,
    ACTIONS(273), 1,
      anon_sym_RPAREN,
  [1523] = 1,
    ACTIONS(275), 1,
      anon_sym_LBRACE,
  [1527] = 1,
    ACTIONS(277), 1,
      anon_sym_RPAREN,
  [1531] = 1,
    ACTIONS(279), 1,
      anon_sym_RBRACE,
  [1535] = 1,
    ACTIONS(281), 1,
      anon_sym_COLON,
  [1539] = 1,
    ACTIONS(283), 1,
      anon_sym_LBRACE,
  [1543] = 1,
    ACTIONS(285), 1,
      anon_sym_EQ,
  [1547] = 1,
    ACTIONS(287), 1,
      anon_sym_LPAREN,
  [1551] = 1,
    ACTIONS(289), 1,
      anon_sym_LPAREN,
  [1555] = 1,
    ACTIONS(291), 1,
      anon_sym_RBRACE,
  [1559] = 1,
    ACTIONS(293), 1,
      ts_builtin_sym_end,
  [1563] = 1,
    ACTIONS(295), 1,
      sym_identifier,
  [1567] = 1,
    ACTIONS(297), 1,
      anon_sym_RBRACE,
  [1571] = 1,
    ACTIONS(299), 1,
      sym_identifier,
  [1575] = 1,
    ACTIONS(301), 1,
      anon_sym_RBRACE,
  [1579] = 1,
    ACTIONS(303), 1,
      anon_sym_RBRACE,
  [1583] = 1,
    ACTIONS(305), 1,
      anon_sym_RBRACE,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 63,
  [SMALL_STATE(4)] = 126,
  [SMALL_STATE(5)] = 186,
  [SMALL_STATE(6)] = 241,
  [SMALL_STATE(7)] = 296,
  [SMALL_STATE(8)] = 351,
  [SMALL_STATE(9)] = 400,
  [SMALL_STATE(10)] = 449,
  [SMALL_STATE(11)] = 495,
  [SMALL_STATE(12)] = 541,
  [SMALL_STATE(13)] = 587,
  [SMALL_STATE(14)] = 633,
  [SMALL_STATE(15)] = 679,
  [SMALL_STATE(16)] = 725,
  [SMALL_STATE(17)] = 755,
  [SMALL_STATE(18)] = 771,
  [SMALL_STATE(19)] = 786,
  [SMALL_STATE(20)] = 800,
  [SMALL_STATE(21)] = 812,
  [SMALL_STATE(22)] = 824,
  [SMALL_STATE(23)] = 846,
  [SMALL_STATE(24)] = 860,
  [SMALL_STATE(25)] = 872,
  [SMALL_STATE(26)] = 884,
  [SMALL_STATE(27)] = 896,
  [SMALL_STATE(28)] = 908,
  [SMALL_STATE(29)] = 920,
  [SMALL_STATE(30)] = 942,
  [SMALL_STATE(31)] = 954,
  [SMALL_STATE(32)] = 966,
  [SMALL_STATE(33)] = 984,
  [SMALL_STATE(34)] = 1000,
  [SMALL_STATE(35)] = 1018,
  [SMALL_STATE(36)] = 1034,
  [SMALL_STATE(37)] = 1052,
  [SMALL_STATE(38)] = 1064,
  [SMALL_STATE(39)] = 1077,
  [SMALL_STATE(40)] = 1090,
  [SMALL_STATE(41)] = 1103,
  [SMALL_STATE(42)] = 1116,
  [SMALL_STATE(43)] = 1129,
  [SMALL_STATE(44)] = 1142,
  [SMALL_STATE(45)] = 1152,
  [SMALL_STATE(46)] = 1162,
  [SMALL_STATE(47)] = 1172,
  [SMALL_STATE(48)] = 1180,
  [SMALL_STATE(49)] = 1190,
  [SMALL_STATE(50)] = 1200,
  [SMALL_STATE(51)] = 1208,
  [SMALL_STATE(52)] = 1216,
  [SMALL_STATE(53)] = 1232,
  [SMALL_STATE(54)] = 1248,
  [SMALL_STATE(55)] = 1256,
  [SMALL_STATE(56)] = 1264,
  [SMALL_STATE(57)] = 1274,
  [SMALL_STATE(58)] = 1282,
  [SMALL_STATE(59)] = 1290,
  [SMALL_STATE(60)] = 1298,
  [SMALL_STATE(61)] = 1306,
  [SMALL_STATE(62)] = 1314,
  [SMALL_STATE(63)] = 1322,
  [SMALL_STATE(64)] = 1332,
  [SMALL_STATE(65)] = 1342,
  [SMALL_STATE(66)] = 1352,
  [SMALL_STATE(67)] = 1359,
  [SMALL_STATE(68)] = 1364,
  [SMALL_STATE(69)] = 1371,
  [SMALL_STATE(70)] = 1378,
  [SMALL_STATE(71)] = 1385,
  [SMALL_STATE(72)] = 1392,
  [SMALL_STATE(73)] = 1399,
  [SMALL_STATE(74)] = 1403,
  [SMALL_STATE(75)] = 1407,
  [SMALL_STATE(76)] = 1411,
  [SMALL_STATE(77)] = 1415,
  [SMALL_STATE(78)] = 1419,
  [SMALL_STATE(79)] = 1423,
  [SMALL_STATE(80)] = 1427,
  [SMALL_STATE(81)] = 1431,
  [SMALL_STATE(82)] = 1435,
  [SMALL_STATE(83)] = 1439,
  [SMALL_STATE(84)] = 1443,
  [SMALL_STATE(85)] = 1447,
  [SMALL_STATE(86)] = 1451,
  [SMALL_STATE(87)] = 1455,
  [SMALL_STATE(88)] = 1459,
  [SMALL_STATE(89)] = 1463,
  [SMALL_STATE(90)] = 1467,
  [SMALL_STATE(91)] = 1471,
  [SMALL_STATE(92)] = 1475,
  [SMALL_STATE(93)] = 1479,
  [SMALL_STATE(94)] = 1483,
  [SMALL_STATE(95)] = 1487,
  [SMALL_STATE(96)] = 1491,
  [SMALL_STATE(97)] = 1495,
  [SMALL_STATE(98)] = 1499,
  [SMALL_STATE(99)] = 1503,
  [SMALL_STATE(100)] = 1507,
  [SMALL_STATE(101)] = 1511,
  [SMALL_STATE(102)] = 1515,
  [SMALL_STATE(103)] = 1519,
  [SMALL_STATE(104)] = 1523,
  [SMALL_STATE(105)] = 1527,
  [SMALL_STATE(106)] = 1531,
  [SMALL_STATE(107)] = 1535,
  [SMALL_STATE(108)] = 1539,
  [SMALL_STATE(109)] = 1543,
  [SMALL_STATE(110)] = 1547,
  [SMALL_STATE(111)] = 1551,
  [SMALL_STATE(112)] = 1555,
  [SMALL_STATE(113)] = 1559,
  [SMALL_STATE(114)] = 1563,
  [SMALL_STATE(115)] = 1567,
  [SMALL_STATE(116)] = 1571,
  [SMALL_STATE(117)] = 1575,
  [SMALL_STATE(118)] = 1579,
  [SMALL_STATE(119)] = 1583,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(116),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(114),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(89),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(88),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(51),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(95),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(94),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [43] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(12),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(88),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(11),
  [52] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(51),
  [55] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(95),
  [58] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(94),
  [61] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(18),
  [64] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 6), SHIFT_REPEAT(31),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__alge_region_repeat1, 2),
  [69] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__alge_region_repeat1, 2), SHIFT_REPEAT(89),
  [72] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__alge_region_repeat1, 2),
  [74] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__alge_region_repeat1, 2), SHIFT_REPEAT(91),
  [77] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__alge_region_repeat1, 2), SHIFT_REPEAT(16),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__alge_stmt, 2),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__alge_stmt, 2),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_integer_literal, 1),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 3),
  [90] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__param_list_repeat1, 2, .production_id = 3),
  [92] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_float_literal, 3),
  [94] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_eval_expr, 4),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expr, 3),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_expr, 3),
  [106] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expr, 2),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_type_constructor, 4, .production_id = 4),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_type_constructor, 3),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [114] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(82),
  [117] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(116),
  [120] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(114),
  [123] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(29),
  [126] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_eval_expr, 5, .production_id = 7),
  [128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_expr, 1),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(95),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__param_list, 2, .production_id = 5),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [142] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__param_list, 1, .production_id = 3),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_stmt, 4),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__alge_region, 1),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_assign_stmt, 5),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__alge_region, 2),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_type, 1),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field_decl, 8, .production_id = 1),
  [162] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_t_scalar, 1),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(108),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(104),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_decl, 8, .production_id = 2),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_t_mat, 4),
  [174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field_decl, 7, .production_id = 1),
  [176] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_t_vec, 2),
  [178] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_df_alias, 4),
  [180] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_t_df, 1),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_alge_decl, 7, .production_id = 2),
  [184] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_t_df, 6),
  [186] = {.entry = {.count = 1, .reusable = false}}, SHIFT(101),
  [188] = {.entry = {.count = 1, .reusable = false}}, SHIFT(60),
  [190] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__arg_list_repeat1, 2), SHIFT_REPEAT(107),
  [193] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__arg_list, 1),
  [195] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [197] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typed_arg, 3),
  [199] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__arg_list, 2),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [203] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_unary, 6),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [207] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_prim, 3),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [211] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_prim, 4, .production_id = 4),
  [213] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [215] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_unary, 7, .production_id = 4),
  [217] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [219] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [221] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [223] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [225] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [227] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [229] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [231] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [233] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [235] = {.entry = {.count = 1, .reusable = true}}, SHIFT(111),
  [237] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [239] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [241] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [243] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [245] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [247] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [249] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [251] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [253] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [255] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [257] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [259] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [261] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [263] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [265] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__arg_list_repeat1, 2),
  [267] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [269] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [271] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [273] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [275] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [277] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [279] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [281] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [283] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [285] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [287] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [289] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [291] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [293] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [295] = {.entry = {.count = 1, .reusable = true}}, SHIFT(109),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(117),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(110),
  [301] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_binary, 9),
  [303] = {.entry = {.count = 1, .reusable = true}}, SHIFT(119),
  [305] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_csg_binary, 10, .production_id = 4),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_vola(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
