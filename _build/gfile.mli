(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> string graph

(*cette fonction est utilisée pour les test unitaires : 
elle récupère les valeurs de flow max attendues pour comparer avec le résultat de notre algorithme, 
situées après des balises 'r' dans 
les fichiers des graphes*)
val from_file2: path -> int

(* Similarly, we write only a string graph.
 * Use Graph.map if necessary to prepare the input graph. *)
val write_file: path -> string graph -> unit


(* écrit un graphe dans le format compéhensible par graphviz.
 * de la forme : énumération de 
Noeud départ -> noeud arrivée [ label = "label de l'arc" ];
doit prendre une fonction 'a -> String qui convertit les labels
des arcs en string *)
val export: 'a graph -> ('a -> string) -> path -> unit


