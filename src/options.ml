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

