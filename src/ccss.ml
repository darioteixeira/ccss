(********************************************************************************)
(*	Ccss.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lexing


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Syntax_error of Lexing.position


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let menhir_with_ulex menhir_parser lexbuf =
	let position = ref
		{
		pos_fname = "";
		pos_lnum = 1;
		pos_bol = 0;
		pos_cnum = 0;
		} in
	let count_lines ante_position =
		let new_cnum = Ulexing.lexeme_end lexbuf in
		let adder acc el = if el = 10 then acc+1 else acc in
		let target = Array.sub (Ulexing.get_buf lexbuf) ante_position.pos_cnum (new_cnum - ante_position.pos_cnum) in
		let new_lnum = ante_position.pos_lnum + (Array.fold_left adder 0 target)
		in	{
			ante_position with
			pos_lnum = new_lnum;
			pos_cnum = new_cnum;
			} in
	let lexer_maker () =
		let ante_position = !position in
		let token = Scanner.main_scanner lexbuf in
		position := count_lines ante_position;
		let post_position = !position
		in (token, ante_position, post_position) in
	let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
	in try
		revised_parser lexer_maker
	with
		Parser.Error -> raise (Syntax_error !position)


let () =
	try
		let lexbuf = Ulexing.from_utf8_channel stdin in
		let css = menhir_with_ulex Parser.stylesheet lexbuf in
		let out = Printer.sprint css
		in print_string out
	with
		| Syntax_error pos ->
			Printf.eprintf "Syntax error on line %d\n" pos.pos_lnum
		| Printer.Variable_redeclared (pos, id) ->
			Printf.eprintf "Attempt to redefine variable '%s' in line %d\n" id pos.pos_lnum
		| Printer.Variable_undeclared (pos, id) ->
			Printf.eprintf "Variable '%s' referenced in line %d has not been declared\n" id pos.pos_lnum
		| Printer.Invalid_arithmetic (pos, op) ->
			Printf.eprintf "Invalid arithmetic in line %d: attempt to %s with non-numeric expression\n" pos.pos_lnum op
		| Printer.Invalid_units (pos, op, u1, u2) ->
			Printf.eprintf "Invalid use of units in line %d: attempt to %s '%s' and '%s'\n" pos.pos_lnum op u1 u2

