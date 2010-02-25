(********************************************************************************)
(*	Printer.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf

exception Bad_units


let sprint_list ?(termin = "") ?(sep = termin) f xs =
	let rec concat = function
		| []	   -> ""
		| [x]	   -> x ^ termin
		| hd :: tl -> hd ^ sep ^ (concat tl)
	in concat (List.map f xs)


let sprint stylesheet =

	let rec sprint_stylesheet (maybe_charset, statements) =
		let str1 = match maybe_charset with
			| Some charset -> sprintf "@charset \"%s\"" charset
			| None	       -> ""
		and str2 = sprint_list ~termin:"\n\n" sprint_statement statements
		in str1 ^ str2

	and sprint_statement = function
		| `Import (source, maybe_media) ->
			sprintf "@import %s%s;"
				(sprint_source source)
				(match maybe_media with None -> "" | Some media -> " " ^ (sprint_media media))
		| `Media (media, rules) ->
			sprintf "@media %s\n{\n%s}"
				(sprint_media media)
				(sprint_list ~termin:"\n" sprint_rule rules)
		| `Page (pseudo_page, declarations) ->
			sprintf "@page %s\n\t{\n%s\t}"
				(match pseudo_page with None -> "" | Some x -> ":" ^ x)
				(sprint_list ~termin:"\n" sprint_declaration declarations)
		| `Fontface declarations ->
			sprintf "@font-face\n\t{\n%s\t}"
				(sprint_list ~termin:"\n" sprint_declaration declarations)
		| `Rule rule ->
			sprint_rule rule

	and sprint_source = function
		| `String s -> sprintf "\"%s\"" s
		| `Uri s    -> sprintf "url(\"%s\")" s

	and sprint_media media =
		sprint_list ~sep:", " (fun x -> x) media

	and sprint_rule (selectors, declarations) =
		sprintf "%s\n\t{\n%s\t}" (sprint_list ~sep:", " sprint_selector selectors) (sprint_list ~termin:"\n" sprint_declaration declarations)

	and sprint_selector (simplesel, combinations) =
		(sprint_simplesel simplesel) ^ (sprint_list sprint_combination combinations)

	and sprint_simplesel = function
		| `Explicit (el, qs) -> (sprint_element el) ^ (sprint_list sprint_qualifier qs)
		| `Generic (hd, tl)  -> (sprint_list sprint_qualifier (hd :: tl))

	and sprint_combination (combinator, simplesel) =
		(sprint_combinator combinator) ^ (sprint_simplesel simplesel)

	and sprint_combinator = function
		| `Descendant	    -> " "
		| `General_sibling  -> " ~ "
		| `Adjacent_sibling -> " + "
		| `Child	    -> " > "

	and sprint_element = function
		| `Tag s     -> s
		| `Universal -> "*"

	and sprint_qualifier = function
		| `Id str			  -> "#" ^ str
		| `Class str			  -> "." ^ str
		| `Attr (name, value)		  -> sprintf "[%s%s]" name (sprint_attr value)
		| `Pseudo_class str		  -> ":" ^ str
		| `Pseudo_element str		  -> "::" ^ str
		| `Qualifier_func (f, qualifiers) -> sprintf ":%s(%s)" f (sprint_list sprint_qualifier qualifiers)
		| `Nth_func str			  -> ":" ^ str

	and sprint_attr = function
		| `Attr_exists	    -> ""
		| `Attr_equals v    -> "=" ^ "\"" ^ v ^ "\""
		| `Attr_includes v  -> "~=" ^ "\"" ^ v ^ "\""
		| `Attr_dashmatch v -> "|=" ^ "\"" ^ v ^ "\""
		| `Attr_prefix v    -> "^=" ^ "\"" ^ v ^ "\""
		| `Attr_suffix v    -> "$=" ^ "\"" ^ v ^ "\""
		| `Attr_substring v -> "*=" ^ "\"" ^ v ^ "\""

	and sprint_declaration (property, expression, important) =
		sprintf "\t%s: %s%s;" property (sprint_expression expression) (if important then " !important" else "")

	and sprint_expression expression =
		sprint_list ~sep:", " sprint_sentence expression

	and sprint_sentence sentence =
		sprint_list ~sep:" " sprint_term sentence

	and sprint_term = function
		| `Calc calc		-> sprint_calc calc
		| `String str		-> sprintf "\"%s\"" str
		| `Ident str		-> str
		| `Uri str		-> sprintf "url(\"%s\")" str
		| `Hash str		-> "#" ^ str
		| `Term_func (f, expr)	-> sprintf "%s(%s)" f (sprint_expression expr)

	and sprint_calc calc =
		let (num, units) = expand_calc calc
		in (sprintf "%.4g" (Num.float_of_num num)) ^ (match units with Some s -> s | None -> "")

	and expand_calc = function
		| `Quantity (num, units) -> (num, units)
		| `Sum (c1, c2)		 -> perform_calc Num.add_num false c1 c2
		| `Sub (c1, c2)		 -> perform_calc Num.sub_num false c1 c2
		| `Mul (c1, c2)		 -> perform_calc Num.mult_num true c1 c2
		| `Div (c1, c2)		 -> perform_calc Num.div_num true c1 c2

	and perform_calc op allow_scaling x y =
		let (num1, units1) = (expand_calc x)
		and (num2, units2) = (expand_calc y) in
		let units = match (units1, units2) with
			| (u1, u2) when u1 = u2		    -> units1
			| (Some u, None) when allow_scaling -> Some u
			| (None, Some u) when allow_scaling -> Some u
			| _				    -> raise Bad_units
		in (op num1 num2, units)

	in sprint_stylesheet stylesheet

