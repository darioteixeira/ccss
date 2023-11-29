open Parser


(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception Error of string


(********************************************************************************)
(** {1 Regular expressions}                                                     *)
(********************************************************************************)

let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let hexa = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let space = [%sedlex.regexp? ' ' | '\t' | '\n']
let trailing = [%sedlex.regexp? upper | lower | digit | '-' | '_']
let ident = [%sedlex.regexp? (lower | '-'), Star trailing]
let variable = [%sedlex.regexp? upper, Star trailing]
let hashed = [%sedlex.regexp? '#', Plus trailing]
let urange_num = [%sedlex.regexp? (Plus hexa, Star '?') | Plus '?']
let urange = [%sedlex.regexp? ('u' | 'U'), '+', urange_num, Opt ('-', urange_num)]
let number = [%sedlex.regexp? Opt ('-' | '+'), Plus digit, Opt ('.', Plus digit)]
let units = [%sedlex.regexp? (Plus lower) | '%']
let slc = [%sedlex.regexp? "//", Plus (Compl '\n')]
let nth = [%sedlex.regexp? Opt ('-' | '+'), Plus digit, 'n', ('-' | '+'), Plus digit]
let prefix = [%sedlex.regexp? '-', Plus lower, '-']


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

(********************************************************************************)
(** {2 Auxiliary functions}                                                     *)
(********************************************************************************)

let add_lines nlines lexbuf =
    let adder acc el = if el = 10 then acc+1 else acc in
    let lexeme = Sedlexing.lexeme lexbuf
    in nlines + (Array.fold_left adder 0 lexeme)


let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf left ((Sedlexing.lexeme_length lexbuf) - left - right)


let ltrim_lexbuf lexbuf =
    trim_lexbuf ~left:1 lexbuf


let rtrim_lexbuf lexbuf =
    trim_lexbuf ~right:1 lexbuf


let parse_quantity =
    let digits = Re.(rep1 (rg '0' '9')) in
    let alphas = Re.(rep1 (alt [rg 'a' 'z'; rg 'A' 'Z'])) in
    let rex = Re.(compile (seq [group (seq [opt (set "+-"); digits; opt (seq [char '.'; digits])]); opt (group (alt [char '%'; alphas]))])) in
    fun lexbuf ->
        let groups = Re.exec rex (Sedlexing.Utf8.lexeme lexbuf) in
        let number = Re.get groups 1 in
        let units =
            if Re.test groups 2
            then Some (Re.get groups 2)
            else None in
        (float_of_string number, units)


let parse_prefixed_atrule =
    let rex = Re.(compile (seq [bos; char '@'; opt (seq [char '-'; group (rep1 (rg 'a' 'z')); char '-'])])) in
    fun lexbuf ->
        let groups = Re.exec rex (Sedlexing.Utf8.lexeme lexbuf) in
        if Re.test groups 1
        then Some (Re.get groups 1)
        else None


(********************************************************************************)
(** {2 Lexers}                                                                  *)
(********************************************************************************)

let rec main_scanner nlines lexbuf = match%sedlex lexbuf with
    | "url("                                   -> (nlines, URI)
    | urange                                   -> (nlines, URANGE (Sedlexing.Utf8.lexeme lexbuf))
    | ident, '('                               -> (nlines, TERM_FUNC (rtrim_lexbuf lexbuf))
    | ':', ident, '('                          -> (nlines, SEL_FUNC (trim_lexbuf ~right:1 ~left:1 lexbuf))
    | nth                                      -> (nlines, NTH (Sedlexing.Utf8.lexeme lexbuf))
    | number, Opt units                        -> (nlines, QUANTITY (parse_quantity lexbuf))
    | Star space, "only", Star space           -> (nlines, ONLY)
    | Star space, "not", Star space            -> (nlines, NOT)
    | Star space, "and", Star space            -> (nlines, AND)
    | ident                                    -> (nlines, IDENT (Sedlexing.Utf8.lexeme lexbuf))
    | variable                                 -> (nlines, VAR (Sedlexing.Utf8.lexeme lexbuf))
    | hashed                                   -> (nlines, HASH (ltrim_lexbuf lexbuf))
    | "@", Opt prefix, "charset", Plus space   -> (add_lines nlines lexbuf, CHARSET (parse_prefixed_atrule lexbuf))
    | "@", Opt prefix, "import", Plus space    -> (add_lines nlines lexbuf, IMPORT (parse_prefixed_atrule lexbuf))
    | "@", Opt prefix, "media", Plus space     -> (add_lines nlines lexbuf, MEDIA (parse_prefixed_atrule lexbuf))
    | "@", Opt prefix, "page", Plus space      -> (add_lines nlines lexbuf, PAGE (parse_prefixed_atrule lexbuf))
    | "@", Opt prefix, "font-face", Plus space -> (add_lines nlines lexbuf, FONTFACE (parse_prefixed_atrule lexbuf))
    | "@", Opt prefix, "keyframes", Plus space -> (add_lines nlines lexbuf, KEYFRAMES (parse_prefixed_atrule lexbuf))
    | Star space, "!important", Star space     -> (nlines, IMPORTANT)
    | Star space, "/*"                         -> comment_scanner nlines lexbuf
    | Star space, slc, Star space              -> main_scanner (add_lines nlines lexbuf) lexbuf
    | "="                                      -> (nlines, ATTR_EQUALS)
    | "~="                                     -> (nlines, ATTR_INCLUDES)
    | "|="                                     -> (nlines, ATTR_DASHMATCH)
    | "^="                                     -> (nlines, ATTR_PREFIX)
    | "$="                                     -> (nlines, ATTR_SUFFIX)
    | "*="                                     -> (nlines, ATTR_SUBSTRING)
    | Star space, "::", Star space             -> (add_lines nlines lexbuf, DOUBLE_COLON)
    | Star space, '*', Star space              -> (add_lines nlines lexbuf, ASTERISK)
    | Star space, 247, Star space              -> (add_lines nlines lexbuf, QUOTIENT)  (* 247 is the decimal Unicode codepoint for the division sign *)
    | Star space, '/', Star space              -> (add_lines nlines lexbuf, SLASH)
    | Star space, '+', Star space              -> (add_lines nlines lexbuf, PLUS)
    | Plus space, '-', Plus space              -> (add_lines nlines lexbuf, MINUS)
    | Star space, '~', Star space              -> (add_lines nlines lexbuf, TILDE)
    | Star space, '>', Star space              -> (add_lines nlines lexbuf, GT)
    | Star space, '{', Star space              -> (add_lines nlines lexbuf, OPEN_CURLY)
    | Star space, '}', Star space              -> (add_lines nlines lexbuf, CLOSE_CURLY)
    | Star space, ';', Star space              -> (add_lines nlines lexbuf, SEMICOLON)
    | Star space, ':', Star space              -> (add_lines nlines lexbuf, COLON)
    | Star space, ',', Star space              -> (add_lines nlines lexbuf, COMMA)
    | Star space, '(', Star space              -> (add_lines nlines lexbuf, OPEN_ROUND)
    | Star space, ')', Star space              -> (add_lines nlines lexbuf, CLOSE_ROUND)
    | '.'                                      -> (nlines, PERIOD)
    | '['                                      -> (nlines, OPEN_SQUARE)
    | ']'                                      -> (nlines, CLOSE_SQUARE)
    | '\''                                     -> single_string_scanner nlines "" lexbuf
    | '"'                                      -> double_string_scanner nlines "" lexbuf
    | space                                    -> (add_lines nlines lexbuf, S)
    | eof                                      -> (nlines, EOF)
    | any                                      -> raise (Error (Sedlexing.Utf8.lexeme lexbuf))
    | _                                        -> assert false


and single_string_scanner nlines accum lexbuf = match%sedlex lexbuf with
    | '\'' -> (nlines, STRING accum)
    | any  -> single_string_scanner (add_lines nlines lexbuf) (accum ^ (Sedlexing.Utf8.lexeme lexbuf)) lexbuf
    | _    -> assert false


and double_string_scanner nlines accum lexbuf = match%sedlex lexbuf with
    | '"' -> (nlines, STRING accum)
    | any -> double_string_scanner (add_lines nlines lexbuf) (accum ^ (Sedlexing.Utf8.lexeme lexbuf)) lexbuf
    | _   -> assert false


and comment_scanner nlines lexbuf = match%sedlex lexbuf with
    | "*/", Star space -> main_scanner (add_lines nlines lexbuf) lexbuf
    | any              -> comment_scanner (add_lines nlines lexbuf) lexbuf
    | _                -> assert false

