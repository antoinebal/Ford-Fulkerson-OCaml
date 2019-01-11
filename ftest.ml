open Graph
open FordFulk

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
  let graphInt = map graph (fun x -> int_of_string x) in

  Printf.printf "----------ALGORITHME-DE-FORD-FULKERSON----------\n\n\n%!";
  
  let flow = fordFulkersonInt graphInt source sink in
  
  Printf.printf "Le graphe de flow maximal du graphe %s de %s vers %s a été calculé avec l'algorithme de Ford-Fulkerson.\n%!" infile source sink;
  Printf.printf "Le flow maximal est de %d.\n\n\n%!" (flowMax flow source sink (-) (+) 0);

  (* Rewrite the graph that has been read. *)
  let () =
    Gfile.export flow (fun x -> string_of_int x) outfile
  in
  ();
  Printf.printf "Le graphe de flow a été importé en format dot dans %s.\n%!" outfile;
  Printf.printf "------------------------------------------------\n";
  

