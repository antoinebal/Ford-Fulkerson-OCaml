open Graph
open FordFulk

(* ce module est dédié aux tests unitaires. Deux seront effectués : 
   > Vérification pour chaque noeud que flow entrant flow rentrant
   > Calcul du flow maximal et comparaison avec des graphes références 
   (on est également informé si flow source = flow puits dans ce dernier test*)

(*Pour utiliser ce module, donner le path vers le dossier contenant les graphes à tester
au processus. Les nom des fichiers doit être sp, avec s id de la source et p du puits. Si l'on veut connait la valeur
du flow max et que l'on veut comparer avec le résultat de l'algorithme, il faut mettre une balise "r 'flow_max'"
n'importe où dans le fichier (cf fichiers dans dossierTest) *)


       
type  testGraph =
    {graphe: (int graph) ;
     source: id ;
     puits: id ;
    }

let () =

  if Array.length Sys.argv <> 2 then
    begin
      Printf.printf "\nUsage: %s dossier_graphes_à_tester\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  (* on récupère le dossier de graphes à tester *)
  let dossierTest = Sys.argv.(1) in
  Printf.printf "Nom dossier : %s \n%!" dossierTest;

  (*le nom des fichier doit être sp, avec s id de la source et p du puits *)
  let listeFiles = Array.to_list (Sys.readdir dossierTest) in
  Printf.printf "Dossier lu\n";
  Printf.printf "Graphes à tester :  \n%!";
  List.iter (fun x ->  Printf.printf "%s\n" x) listeFiles;

  (* on se déplace dans le dossier test *)
  Sys.chdir dossierTest;

  (*fonction pour construire une liste de testGraph à partir de la liste des fichiers *)
  let rec buildListTestGraph lF acu =
    match lF with
    |[] -> acu
    |nomFichier::reste ->
      if (String.length nomFichier) <> 2
      then failwith "Nom fichier incorrect"
      else
        let tGr =
          {graphe=map (Gfile.from_file nomFichier) (fun x -> int_of_string x) ;
           source=String.make 1 (String.get nomFichier 0);
           puits=String.make 1 (String.get nomFichier 1)}
        in
        buildListTestGraph reste (acu@[tGr])
  in

  (* fonction pour construire la liste des résultats attendus à partir de la liste des fichiers *)
  let rec buildListResAttendus lF lT acu =
    match (lF, lT) with
    |([], []) -> acu
    |((nomFichier::reste1), (t::reste2)) ->
      buildListResAttendus reste1
			   reste2
			   (acu@[Gfile.from_file2 nomFichier])
    |_ -> failwith "listes pas de même taille"
  in

  (*on construit une première liste, contenant les graphes initiaux *)
  let listeTestGr = buildListTestGraph listeFiles [] in

  (* on construit la liste des résultats attendus *)
  let lTest = buildListResAttendus listeFiles listeTestGr [] in

  (*on construit la liste des testGraph contenant les graphes de flow max *)
  let listeTestGr2 =
    List.map (fun t ->
              {graphe=(fordFulkersonInt (t.graphe) (t.source) (t.puits)) ;
               source=(t.source) ; puits=(t.puits)}) listeTestGr
  in

  Printf.printf "\n***************************DEBUT DES TESTS UNITAIRES***************************\n%!";

  (* on vérifie que pour chaque noeud on a : flow In = flow Out *)
  Printf.printf "\n-------VERIFICATION DE FLOW ENTRANT = FLOW SORTANT POUR CHAQUE NOEUD-------\n%!";
  let rec testFlowInOut lTGr =
    let f t id =
      if (id=t.puits)||(id=t.source)
      then ()
      else
        let diff = (differenceFlowInOut (t.graphe) id (-) (+) 0) in
        match diff with
        |0 -> Printf.printf "GOOD %s ENTRANT=SORTANT\n%!" id
        |_ -> Printf.printf "BAD %s ENTRANT!=SORTANT %d\n%!" id diff
    in

    let rec testUnitIO tGr firstCall premierId=
      match getGraph (tGr.graphe) with
      |[] -> Printf.printf "\n%!"
      |(id, out)::reste ->
        if (id=premierId)&&(not firstCall)
        then Printf.printf "\n%!"
        else
          match firstCall with
          |true ->
            (f tGr id);
            testUnitIO {graphe=(buildGraph (reste@[(id, out)])) ;
                        source=(tGr.source) ;
                        puits=(tGr.puits)} false id
          |false ->
            (f tGr id);
            testUnitIO {graphe=(buildGraph (reste@[(id, out)])) ;
                        source=(tGr.source) ;
                        puits=(tGr.puits)} false premierId
    in

    match lTGr with
    |[] -> Printf.printf "*********FIN*********";
    |t::reste ->
      Printf.printf "**TEST POUR GRAPHE %s%s**\n" (t.source) (t.puits); 
      testUnitIO t true ""; 
      testFlowInOut reste
  in


  testFlowInOut listeTestGr2;



  Printf.printf "\n\n-------VERIFICATION DE LA REGLE FLOW TOTAL CALCULE = FLOW TOTAL ATTENDU-------\n%!";

  
  let rec compareFlowTotaux l =
    let fMax t exp =
      try
	(flowMaxInt (t.graphe) (t.source) (t.puits))=exp
      with
      |Graph_error e -> Printf.printf "BAD : FLOW SOURCE != FLOW PUITS \n%!"; false
    in
    let f t exp = match fMax t exp with
      |true -> Printf.printf "GOOD : FLOW MAX COMPUTED IS CORRECT \n\n%!"
      |false -> Printf.printf "BAD : FLOW MAX COMPUTED IS NOT CORRECT\n\n%!"
    in
    match l with
    |[] -> Printf.printf "***************FIN***************\n%!"
    |(t, exp)::reste ->
      Printf.printf "**TEST POUR GRAPHE %s%s**\n" (t.source) (t.puits); 
      f t exp;
      compareFlowTotaux reste
  in



  compareFlowTotaux (List.combine listeTestGr2 lTest);
