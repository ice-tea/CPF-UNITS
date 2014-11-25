open Cpfannotation

let interactive = ref true
let verbose = ref true

let argspecs = [
    ("-n", Arg.Clear interactive,
           "Runs in non-interactive mode.\n"); 
]

let ast s = 
    Cpfparse.cpf_annotation Cpflex.cpftoken (Lexing.from_string (s ^ ";;"))


let rec prompt () = 
    try
        if !interactive then (print_string "> "; flush stdout); 
        let lexbuf       = Lexing.from_channel stdin in 
        let ast          = Cpfparse.cpf_annotation Cpflex.cpftoken lexbuf in
        begin
          print_string (Cpfannotation.show_cpf_annotation ast) ;
          print_newline () ;
          prompt ()
        end
    with
      | Failure s           -> (print_string (s ^ "\n"); prompt ())
      | Parsing.Parse_error -> (print_string "Parse error\n"; prompt ())
      | Cpflex.CpfEndInput    -> if !interactive then print_string "\nBye.\n"

let _ = 
    let istoplevel = 
        try 
            Str.search_forward (Str.regexp "cpf\\.top$") Sys.argv.(0) 0; 
            true 
        with _ -> false
    in
    if not istoplevel then
        begin
            Arg.parse argspecs (fun x -> ()) "Cpf usage:";
            if !interactive then print_string "Welcome to cpf.\n"; 
            prompt ()
        end
    else
        print_string "Cpf: Exiting into OCaml...\n"
