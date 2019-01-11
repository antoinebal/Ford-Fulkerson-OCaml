open Graph
open Printf
       
type path = string

(* Format of text files: lines of the form 
 *
 *  v id               (node with the given identifier)
 *  e label id1 id2    (arc with the given (string) label. Goes from node id1 to node id2.)
 *
 *)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "=== Graph file ===\n\n" ;

  (* Write all nodes *)
  v_iter graph (fun id _ -> fprintf ff "v %s\n" id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "e \"%s\" %s %s\n" lbl id id2) out) ;
  
  fprintf ff "\n=== End of graph ===\n" ;
  
  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "v %s" (fun id -> add_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e \"%s@\" %s %s" (fun label id1 id2 -> add_arc graph id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
             | 'v' -> read_node graph line
             | 'e' -> read_arc graph line
             | _ -> graph
      in                 
      loop graph2        
    with End_of_file -> graph
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph


    
(*cette fonction est utilisée pour les test unitaires : 
elle récupère les valeurs de flow max attendues pour comparer avec le résultat de notre algorithme, 
situées après des balises 'r' dans 
les fichiers des graphes*)
let from_file2 path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop x =
    try
      let line = input_line infile in
      let x2 =
        (* Ignore empty lines *)
        if line = "" then x

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
             |'r' ->
               (try Scanf.sscanf line "r %d" (fun x -> x)
		with e ->
		  Printf.printf "Cannot read result in line - %s:\n%s\n" (Printexc.to_string e) line ;
		  failwith "from_file")
             |_ -> x
      in                 
      loop x2        
    with End_of_file -> x
  in

  let result = loop 0 in
  
  close_in infile ;
  result




(* écrit un graphe dans le format compéhensible par graphviz.
 * de la forme : énumération de 
Noeud départ -> noeud arrivée [ label = "label de l'arc" ]; *)
let export gr cv chemin =
  let rec succToString idNDepart l acu =
    match l with
    |[] -> acu
    |(i,lab)::reste ->
      succToString idNDepart reste (acu^idNDepart^" -> "^i^" [ label = \""^(cv lab)^"\" ];\n")
  in
  let f acu nCourant lSuivants =
    acu^(succToString nCourant lSuivants "")
  in
  let leFond = v_fold gr f "" in
  let ff = open_out chemin in
  fprintf ff
	  "digraph finite_state_machine {
	   rankdir=LR;
	   size=\"8,5\"
	   node [shape = circle];\n%s}" leFond;
  close_out ff;

  
