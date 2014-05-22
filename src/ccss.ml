(********************************************************************************)
(*	Ccss.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lexing
open Parser


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Scanning_error of Lexing.position * string
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
	let lexer_maker () =
		let ante_position = !position in
		let (nlines, token) = Scanner.main_scanner 0 lexbuf in
		let () = position := {!position with pos_lnum = !position.pos_lnum + nlines;} in
		let post_position = !position
		in (token, ante_position, post_position) in
	let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
	in try
		revised_parser lexer_maker
	with
		| Scanner.Error x -> raise (Scanning_error (!position, x))
		| Parser.Error	  -> raise (Syntax_error !position)

let die s =
	output_string stderr s;
	exit 1

let () =
	let convert = Options.parse ()
	in try
		let lexbuf = Ulexing.from_utf8_channel stdin in
		let css = menhir_with_ulex Parser.stylesheet lexbuf in
		let out = Printer.sprint convert css
		in print_string out
	with
		| Scanning_error (pos, x) ->
			Printf.sprintf "Scanning error on line %d: cannot interpret '%s'.\n" pos.pos_lnum x |> die
		| Syntax_error pos ->
			Printf.sprintf "Syntax error on line %d.\n" pos.pos_lnum |> die
		| Printer.Variable_redeclared (pos, id) ->
			Printf.sprintf "Attempt to redefine variable '%s' in line %d.\n" id pos.pos_lnum |> die
		| Printer.Variable_undeclared (pos, id) ->
			Printf.sprintf "Variable '%s' referenced in line %d has not been declared.\n" id pos.pos_lnum |> die
		| Printer.Expected_mixin_over_expression (pos, id) ->
			Printf.sprintf "In line %d, variable '%s' refers to an expression, but a mixin was expected in this context.\n" pos.pos_lnum id |> die
		| Printer.Expected_expression_over_mixin (pos, id) ->
			Printf.sprintf "In line %d, variable '%s' refers to a mixin, but an expression was expected in this context.\n" pos.pos_lnum id |> die
		| Printer.Invalid_arithmetic (pos, op) ->
			Printf.sprintf "Invalid arithmetic in line %d: attempt to %s with non-numeric expression.\n" pos.pos_lnum op |> die
		| Printer.Invalid_units (pos, op, u1, u2) ->
			let error = Printf.sprintf "Invalid use of units in line %d: attempt to %s %s and %s.\n" pos.pos_lnum op u1 u2 in
			let error = if not convert then Printf.sprintf "%s\nHint: the '--convert' option may be used to attempt unit conversion, where applicable.\n" error else error in
			die error

