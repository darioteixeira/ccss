module String = BatString


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let execute source () =
    let ch = open_in source in
    let source_contents = BatPervasives.input_all ch in
    close_in ch;
    let (in_ch, out_ch) = Unix.open_process "ccss --convert" in
    output_string out_ch source_contents;
    flush out_ch;
    close_out out_ch;
    let target_contents = BatPervasives.input_all in_ch in
    let _ = Unix.close_process (in_ch, out_ch) in
    let correct = String.slice ~last:(-4) source ^ "css" in
    let ch = open_in correct in
    let correct_contents = BatPervasives.input_all ch in
    close_in ch;
    Alcotest.(check string) source correct_contents target_contents

let build_test () =
    let process filename =
        let ch = open_in filename in
        let line = input_line ch in
        close_in ch;
        (filename, String.slice ~first:3 line, `Quick, execute filename) in
    let hnd = Unix.opendir "." in
    let rec loop accum = match Unix.readdir hnd with
        | filename ->
            if String.ends_with filename ".ccss"
            then loop (process filename :: accum)
            else loop accum
        | exception End_of_file ->
            Unix.closedir hnd;
            accum in
    loop [] |> List.sort Pervasives.compare |> List.map (fun (_, d, q, f) -> (d, q, f))

let tests =
    [
    ("CCSS testset", build_test ());
    ]

let () =
    Alcotest.run "CCSS tests" tests

