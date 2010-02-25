(********************************************************************************)
(*	Ccss.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lexing

let position =
	{
	pos_fname = "";
	pos_lnum = 0;
	pos_bol = 0;
	pos_cnum = 0;
	}


let menhir_with_ulex menhir_parser lexbuf =
	let lexer_maker () =
		let ante_position = position in
		let token = Scanner.main_scanner lexbuf in
		let post_position = position
		in (token, ante_position, post_position) in
	let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
	in revised_parser lexer_maker


let () =
	let lexbuf = Ulexing.from_utf8_channel stdin in
	let css = menhir_with_ulex Parser.stylesheet lexbuf in
	let out = Printer.sprint css
	in print_string out

