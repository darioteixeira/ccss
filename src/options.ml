(********************************************************************************)
(*  Options.ml
    Copyright (c) 2010-2022 Dario Teixeira <dario.teixeira@nleyten.com>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License
    version 2.1 as published by the Free Software Foundation, with the
    special exception on linking described in the LICENSE file.
*)
(********************************************************************************)

open BatOptParse


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let options = OptParser.make ()

let convert_opt = StdOpt.store_true ()


let () =
    let general = OptParser.add_group options "General options"
    in OptParser.add options ~group:general ~short_name:'c' ~long_name:"convert" ~help:"Attempt unit conversion" convert_opt


let parse () = match OptParser.parse_argv options with
    | hd::tl ->
        OptParser.usage options ();
        OptParser.error options "Error: invalid usage";
        raise Exit
    | [] ->
        Opt.get convert_opt

