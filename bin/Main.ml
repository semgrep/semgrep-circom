(* Generated by ocaml-tree-sitter. *)

open Tree_sitter_circom

let () =
  Tree_sitter_run.Main.run
    ~lang:"circom"
    ~parse_source_file:Parse.parse_source_file
    ~parse_input_tree:Parse.parse_input_tree
    ~dump_tree:Boilerplate.dump_tree