open Graph

(* fonction pour enlever tous les arcs du graphes
   pour lesquels f est vraie *)
let filtreGraphe (gr: 'a graph) f =
  let rec filtreArcs l acu =
    match l with
    |[] -> acu
    |(id, lab)::reste ->
      if f lab
      then filtreArcs reste acu
      else filtreArcs reste (acu@[(id,lab)])
  in
  let rec filtreGrapheAcu lGraphe acu =
    match lGraphe with
    |[] -> acu
    |(id, outArcs)::reste ->
      filtreGrapheAcu  reste (acu@[(id, (filtreArcs (getLOut outArcs) []))])
  in
  buildGraph (filtreGrapheAcu (getGraph gr) [])


(* fonction utile pour retourner un arc sortant et le noeud qui y est lié parmi
   la liste listeArcs. Le noeud distant ne doit pas appartenir à chemin ou à sansIssue.
   Cette fonction est utilise pour trouve un noeud voisin d'un autre lorsque l'on cherche
   une chemin dans le graphe.
   >Params : listeArcs (a out_arcs)
   chemin : (id list)
   sansIssue : liste des noeud débouchant sur des culs-de-sac (gérée par chercheChemin)
   >Valeur retournée : (a' option (id, 'a (label))) du noeud choisi et de l'arc y menant*)
let rec retournePiste listeArcs chemin sansIssue =
  match listeArcs with
  |[] -> None
  |(x_id, lab)::reste ->
    if (List.exists (fun x -> x=x_id) chemin)||(List.exists (fun x -> x=x_id) sansIssue)
						 (* si le noeud est dans chemin ou sansIssue, on en cherche un autre *)
    then retournePiste reste chemin sansIssue
    else Some (x_id, lab)
	      


(* chercheChemin essaie de chercher un chemin entre le noeuds 
   >Params : gr : ('a graph) graphe d'entrée
   idSource, idPuits : (id) noeuds de départ et d'arrivée 
   valMax : ('a) utilisé pour initialiser la valeur du plus petit label d'arc, afin de calculer le label minimal.
   >Valeur retournée : (id list) des noeuds constituant le chemin*)
let chercheChemin (gr: 'a graph) idSource idPuits valMax = 
  let min a b = if a < b then a else b in
  let rec chercheCheminAcu (gr: 'a graph) noeudCourant acu delta sansIssue =
    (* on cherche tout d'abord s'il existe un arc entre noeudCourant et puits *)
    match find_arc gr noeudCourant idPuits with
    |Some label -> Some ((acu@[idPuits]), (min delta label))
    |None -> 
      (* si pas de tel arc, alors on cherche un autre chemin *)
      match retournePiste (getLOut (out_arcs gr noeudCourant)) acu sansIssue with
      |None ->
	(* si pas de piste trouvée, on lève une exception si le noeud courant
	   n'est pas la source, on retourne None sinon *)
	if noeudCourant = idSource
	then None
	else raise Not_found
      |Some (x_id, lab) ->
	(* si on trouve une piste, on appelle la fonction depuis le noeud trouvé *)
	try chercheCheminAcu gr x_id (acu@[x_id]) (min delta lab) sansIssue
	with
	(* si cet appel lève une exception, on essaie de trouver une autre piste
	   en excluant le cul-de-sac *)
	|Not_found -> chercheCheminAcu gr noeudCourant acu delta (sansIssue@[x_id])
  in
  chercheCheminAcu gr idSource [idSource] valMax []


(* dans gr, updateGraph modifie les arcs reliant les noeuds constituant
 le chemin. 
 >Params : chemin : (id list)
 delta : ('a), valeur minimale des labels du chemin, calculé dans chercherChemin
  filtre : ('a -> bool), sert à enlever les arcs nuls du graphe entre deux appels récursifs.
 dec et inc : ('a -> 'a -> 'a), servent à décrémenter et incrémenter les labels. 
 >Valeur retournée : ('a graph) nouvelle version du graphe*)
let rec updateGraph (gr: 'a graph) chemin delta filtre dec inc =
  match chemin with
  |[] -> raise  (Graph_error "Chemin vide")
  |_::[] -> gr
  |idX::(idY::reste) ->
    (* on modifie les arcs du chemin *)
    match find_arc gr idX idY with
    (* si l'arc n'est pas trouvé on lève une exception :
	   il devrait exister car il fait partie du chemin
	   trouvé par chercheChemin *)
    |None -> raise (Graph_error "Arc dans le chemin mais pas dans le graphe")
    |Some lab ->
      match find_arc gr idY idX with
      (* on crée les arcs. Si l'arc inverse existe on incrémente son label,
	 sinon on le crée avec delta pour label. Dans tous les cas, on
	 décrémente le label de l'arc dans le sens du chemin*)
      |None ->
	updateGraph (filtreGraphe (add_arc (add_arc gr idX idY (dec lab delta)) idY idX delta) filtre) (idY::reste) delta filtre dec inc
      |Some lab2 ->
	updateGraph (filtreGraphe (add_arc (add_arc gr idX idY (dec lab delta)) idY idX (inc lab2 delta)) filtre) (idY::reste) delta filtre dec inc
		    

(* calculeGrapheEcart applique l'algorithme de ford-fulkerson sur gr
   entre les noeuds idSource et idPuits. Il travaille sur le graphe d'écart, le modifie jusqu'à
   ce qu'il n'y est plus de chemin.	
   >Param : gr : ('a graph) graphe d'entrée.On considère que les labels sur les arcs
   de gr sont leur capacité. 
   idSource, idPuits : (id) noeuds de départ et d'arrivée
   dec et inc : ('a -> 'a -> 'a) , servent à décrémenter et incrémenter les labels. (donné en argument à updateGraph)
   filtre : ('a -> bool), sert à enlever les arcs nuls du graphe entre deux appels récursifs. (donné en argument à updateGraph)
   >Valeur retournée : ('a graph)  graphe d'écart ne contenant plus de chemin entre idSource
   et idPuits.*) 
let rec calculeGrapheEcart (gr: 'a graph) idSource idPuits valMax dec inc filtre = 
  match chercheChemin gr idSource idPuits valMax with
  (* quand il n'y a plus de chemin, l'algo est terminé *)
  |None -> gr
  (* impossible de trouver un chemin à un élément
     car il devrait au moins contenir la source et le puits *)
  |Some ((_::[]), _) -> raise (Graph_error "Le chemin ne contient qu'un élément")
  |Some (chemin, delta) -> calculeGrapheEcart (updateGraph gr chemin delta filtre dec inc) idSource idPuits valMax dec inc filtre



(* calcule |(somme labels arc sortant) - (somme labels arc entrant)| pour le noeud id*)
let differenceFlowInOut (gr: 'a graph) id soustraction somme zero=
  let flowEntrant = List.fold_left
		      (fun x y ->
		       match y with
		       |(id, lab) -> somme x lab) zero (in_arcs gr id) in

  let flowSortant = List.fold_left
		      (fun x y ->
		       match y with
		       |(id, lab) -> somme x lab) zero (out_arcs gr id) in
  let excedent = soustraction flowSortant flowEntrant in

  if excedent < zero
  then soustraction zero excedent
  else excedent




(* cette fonction reconstitue le graphe initial avec les bonnes valeurs de flows,
 à partir du résultat de calculeGrapheEcart, soit à partir du graphe d'écart
 >Params : gr : ('a graph), graphe initial.
 grEcart : ('a graph) graphe d'écart calculé par ff
 soustraction : ('a -> 'a -> 'a) pour soustraire deux labels entre eux
 somme : ('a -> 'a -> 'a)
 >Valeur retournée : ('a graph), avec le flow maximal *)
let graphFlowFF (gr: 'a graph) (grEcart: 'a graph) soustraction somme zero =
  (*listeOverFlows réunit tous les arcs tels que flow > capacité, ils seront changés à la fin *)
  let rec reconstitueArcs gr idX outArcs=
    match outArcs with
    |[] -> gr
    |(idY, lab)::reste ->
      (* on vérifie si l'arc reliant idX et idY existe dans le graphe d'écart calculé avec calculeGrapheEcart *)
      match find_arc grEcart idX idY with
      (* s'il n'existe pas, le flow de cet arc est sa capacité *)  
      |None -> reconstitueArcs (add_arc gr idX idY lab) idX reste
      (* s'il existe : *)
      |Some labEcart ->
	(* le flow de cet arc est sa capacité - labEcart *)
	reconstitueArcs (add_arc gr idX idY (soustraction lab labEcart)) idX reste
  in
  let rec reconstitueAcu lGraph acu = 
    match lGraph with
    |[] -> acu
    |(idX, outArcs)::reste -> reconstitueAcu reste (reconstitueArcs acu idX outArcs) 
  in
  reconstitueAcu (getGraph gr) gr


(*version polymorphique du calcul de graphe de flow maximal de gr en appliquant l'algorithme de
  ford fulkerson
  >Params : gr : ('a graph), graphe initial.
  idSource : id
  idPuits : id
  soustraction : ('a -> 'a -> 'a) pour soustraire deux labels entre eux
  somme : ('a -> 'a -> 'a)
  zero : 'a, élément nul de l'ensemble des éléments 'a
  filtre : ('a -> bool), fonction utile pour enlever des arcs du graphes (les arcs nuls typiquement)
  >Valeur retournée : ('a graph), avec le flow maximal *)
let fordFulkerson (gr: 'a graph) idSource idPuits soustraction somme zero valMax filtre = 
  let ecart = calculeGrapheEcart gr idSource idPuits valMax soustraction somme filtre in
  graphFlowFF gr ecart soustraction somme zero


(*calcule le graphe de flow maximal de gr en appliquant l'algorithme de
  ford fulkerson pour les graphes ayant des labels int.
  Cette fonction a pour but de faciliter l'utilisation du module à l'extérieur
  avec car elle demande moins d'arguments comme elle est non polymorphique*)
let fordFulkersonInt (gr: int graph) idSource idPuits =
  let nonNul x = x = 0 in
  fordFulkerson gr idSource idPuits (-) (+) 0 83647 nonNul


(* calcule le flow maximum d'un graphe, lève une exception Graph_error
   si le flow sortant du noeud source est différent du flow rentrant
   dans le noeud puits
   >Params : gr : ('a graph)
   idSource : id
   idPuits : id
   soustraction : ('a -> 'a -> 'a) pour soustraire deux labels entre eux
   somme : ('a -> 'a -> 'a)
   zero : 'a, élément nul de l'ensemble des éléments 'a
   >Valeur retournée : 'a flow maximal *)
let flowMax (gr: 'a graph) idSource idPuits soustraction somme zero =
  let flowSource = differenceFlowInOut gr idSource soustraction somme zero in
  let flowPuits = differenceFlowInOut gr idPuits soustraction somme zero in
  if flowSource <> flowPuits
  then raise (Graph_error "Flow source et flow puits différents.")
  else flowSource


(* version non polymorphique (int) de la fonction ci-dessus *)
let flowMaxInt (gr: int graph) idSource idPuits =
  let flowSource = differenceFlowInOut gr idSource (-) (+) 0 in
  let flowPuits = differenceFlowInOut gr idPuits (-) (+) 0 in
  if flowSource <> flowPuits
  then raise (Graph_error "Flow source et flow puits différents.")
  else flowSource


