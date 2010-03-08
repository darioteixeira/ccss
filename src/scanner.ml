(********************************************************************************)
(*	Scanner.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Parser


(********************************************************************************)
(**	{1 Regular expressions}							*)
(********************************************************************************)

let regexp alpha = ['a'-'z']
let regexp digit = ['0'-'9']
let regexp hexa = ['0'-'9' 'a'-'f']
let regexp space = [' ' '\t' '\n']
let regexp ident = ['a'-'z']['a'-'z' '0'-'9' '-']*
let regexp hashed = ['a'-'z' '0'-'9' '-']+
let regexp number = ('-' | '+')? digit+ ('.' digit+)?
let regexp units = alpha+ | '%'
let regexp slc = "//" [^ '\n']+
let regexp nth = [^ ')']+ ')'


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Auxiliary functions}							*)
(********************************************************************************)

let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
	Ulexing.utf8_sub_lexeme lexbuf left ((Ulexing.lexeme_length lexbuf) - left - right)


let ltrim_lexbuf lexbuf =
	trim_lexbuf ~left:1 lexbuf


let rtrim_lexbuf lexbuf =
	trim_lexbuf ~right:1 lexbuf


let parse_quantity =
	let rex = Pcre.regexp "(?<integer>(\\+|-)?[0-9]+)(\\.(?<decimals>[0-9]+))?(?<units>%|[a-z]+)?"
	in fun lexbuf ->
		let subs = Pcre.exec ~rex (Ulexing.utf8_lexeme lexbuf) in
		let integer = Pcre.get_named_substring rex "integer" subs
		and decimals = try Pcre.get_named_substring rex "decimals" subs with Not_found -> ""
		and units = try Some (Pcre.get_named_substring rex "units" subs) with Not_found -> None in
		let divident = Num.num_of_string (integer ^ decimals)
		and divisor = Num.num_of_string ("1" ^ (String.make (String.length decimals) '0')) in
		let number = Num.div_num divident divisor
		in (number, units)


(********************************************************************************)
(**	{2 Lexers}								*)
(********************************************************************************)

let rec main_scanner = lexer
	| "url("			-> URI
	| ident '('			-> TERM_FUNC (rtrim_lexbuf lexbuf)
	| number units?			-> QUANTITY (parse_quantity lexbuf)
	| ident				-> IDENT (Ulexing.utf8_lexeme lexbuf)
	| ":nth-child(" nth		-> NTH_FUNC (ltrim_lexbuf lexbuf)
	| ":nth-last-child(" nth	-> NTH_FUNC (ltrim_lexbuf lexbuf)
	| ":nth-of-type(" nth		-> NTH_FUNC (ltrim_lexbuf lexbuf)
	| ":nth-last-of-type(" nth	-> NTH_FUNC (ltrim_lexbuf lexbuf)
	| ':' ident '('			-> QUALIFIER_FUNC (trim_lexbuf ~right:1 ~left:1 lexbuf)
	| '#' hashed			-> HASH (ltrim_lexbuf lexbuf)
	| "@charset" space+		-> CHARSET
	| "@import" space+		-> IMPORT
	| "@media" space+		-> MEDIA
	| "@page" space+		-> PAGE
	| "@font-face" space+		-> FONTFACE
	| space* "/*"			-> comment_scanner lexbuf
	| space* slc space*		-> main_scanner lexbuf
	| "="				-> ATTR_EQUALS
	| "~="				-> ATTR_INCLUDES
	| "|="				-> ATTR_DASHMATCH
	| "^="				-> ATTR_PREFIX
	| "$="				-> ATTR_SUFFIX
	| "*="				-> ATTR_SUBSTRING
	| space* "**" space*		-> MUL
	| space* "%%" space*		-> DIV
	| space* "++" space*		-> SUM
	| space* "--" space*		-> SUB
	| space* "::" space*		-> DOUBLE_COLON
	| space* '*' space*		-> ASTERISK
	| space* '/' space*		-> SLASH
	| space* '+' space*		-> PLUS
	| space* '~' space*		-> TILDE
	| space* '>' space*		-> GT
	| space* '{' space*		-> OPEN_CURLY
	| space* '}' space*		-> CLOSE_CURLY
	| space* ';' space*		-> SEMICOLON
	| space* ':' space*		-> COLON
	| space* ',' space*		-> COMMA
	| space* '(' space*		-> OPEN_ROUND
	| space* ')' space*		-> CLOSE_ROUND
	| space* '$'			-> DOLLAR
	| '.'				-> PERIOD
	| '['				-> OPEN_SQUARE
	| ']'				-> CLOSE_SQUARE
	| '!'				-> EXCLAMATION
	| '\''				-> STRING (single_string_scanner "" lexbuf)
	| '"'				-> STRING (double_string_scanner "" lexbuf)
	| space				-> S
	| eof				-> EOF


and single_string_scanner accum = lexer
	| '\''				-> accum
	| '\\' _			-> single_string_scanner (accum ^ (Ulexing.utf8_sub_lexeme lexbuf 1 1)) lexbuf
	| _				-> single_string_scanner (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


and double_string_scanner accum = lexer
	| '"'				-> accum
	| '\\' _			-> double_string_scanner (accum ^ (Ulexing.utf8_sub_lexeme lexbuf 1 1)) lexbuf
	| _				-> double_string_scanner (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


and comment_scanner = lexer
	| "*/" space*			-> main_scanner lexbuf
	| _				-> comment_scanner lexbuf

