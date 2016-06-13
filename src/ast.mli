(********************************************************************************)
(*  Ast.mli
    Copyright (c) 2010-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Defines the AST for parsed CSS stylesheets.
*)

(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(********************************************************************************)
(** {2 Top-level statements}                                                    *)
(********************************************************************************)

type t = statement list

and statement =
    [ `Atrule of prefix option * atrule
    | `Rule of rule
    | `Vardecl of variable * vardecl
    ]

and atrule =
    [ `Charset of string
    | `Import of source * media_query list option
    | `Media of media_query list * rule list
    | `Page of pseudo_page option * declaration list
    | `Fontface of declaration list
    | `Keyframes of string * keyframe_block list
    ]

and prefix = string

and source =
    [ `String of string
    | `Uri of string
    ]

and media_query =
    [ `Typed of media_type * media_expression list option
    | `Untyped of media_expression list
    ]

and media_type = [ `Only | `Not ] option * string

and media_expression = media_feature * sentence option

and media_feature = string

and pseudo_page = string

and keyframe_block = keyframe_sel * declaration list

and keyframe_sel =
    [ `Ident of string
    | `Calc of calc
    ]

and rule = selector list * declaration list

and variable = Lexing.position * string

and vardecl =
    [ `Expr of expression
    | `Mixin of declaration list
    ]


(********************************************************************************)
(** {2 Selectors}                                                               *)
(********************************************************************************)

and selector = simplesel * (combinator * simplesel) list

and simplesel =
    [ `Explicit of element * qualifier list
    | `Generic of qualifier * qualifier list
    ]

and combinator =
    [ `Descendant
    | `General_sibling
    | `Adjacent_sibling
    | `Child
    ]

and element =
    [ `Tag of string
    | `Universal
    ]

and qualifier =
    [ `Id of string
    | `Class of string
    | `Attr of string * attr
    | `Pseudo_class of string
    | `Pseudo_element of string
    | `Sel_func of string * func
    ]

and func =
    [ `Qualified of qualifier list
    | `Quantity of quantity
    | `Nth of string
    ]

and attr =
    [ `Attr_exists
    | `Attr_equals of string
    | `Attr_includes of string
    | `Attr_dashmatch of string
    | `Attr_prefix of string
    | `Attr_suffix of string
    | `Attr_substring of string
    ]


(********************************************************************************)
(** {2 Declarations}                                                            *)
(********************************************************************************)

and declaration =
    [ `Property of property * expression * important
    | `Varref of variable
    ]

and property = string

and important = bool

and expression = sentence list

and sentence = term list

and term =
    [ `Calc of calc
    | `String of string
    | `Ident of string
    | `Uri of string
    | `Hash of string
    | `Urange of string
    | `Term_func of string * expression
    | `Slash
    ]

and calc =
    [ `Varref of variable
    | `Quantity of quantity
    | `Sum of Lexing.position * calc * calc
    | `Sub of Lexing.position * calc * calc
    | `Mul of Lexing.position * calc * calc
    | `Div of Lexing.position * calc * calc
    ]

and quantity = float * string option

