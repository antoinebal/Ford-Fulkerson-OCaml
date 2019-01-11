open Graph



(* fonction pour enlever tous les arcs du graphes pour lesquels f est vraie *)
val filtreGraphe: 'a graph -> ('a -> bool) -> 'a graph


(* fonction utile pour retourner un arc sortant et le noeud qui y est lié parmi
   la liste listeArcs. Le noeud distant ne doit pas appartenir à chemin ou à sansIssue.
   Cette fonction est utilisée pour trouver un noeud voisin d'un autre lorsque l'on cherche
   une chemin dans le graphe.
   >Params : listeArcs (a out_arcs)
   chemin : (id list)
   sansIssue : liste des noeud débouchant sur des culs-de-sac (gérée par chercheChemin)
   >Valeur retournée : (a' option (id, 'a (label))) du noeud choisi et de l'arc y menant*)
val retournePiste: (id * 'a) list -> id list -> id list -> (id * 'a) option

								     
(* chercheChemin essaie de chercher un chemin entre les noeuds source et puits 
   >Params : gr : ('a graph) graphe d'entrée
   idSource, idPuits : (id) noeuds de départ et d'arrivée 
   valMax : ('a) utilisé pour initialiser la valeur du plus petit label d'arc, afin de calculer le label minimal.
   >Valeur retournée : (id list) des noeuds constituant le chemin*)
val chercheChemin: 'a graph -> id -> id -> 'a -> (id list * 'a) option


(* dans gr, updateGraph modifie les arcs reliant les noeuds constituant
 le chemin. 
 >Params : chemin : (id list)
 delta : ('a), valeur minimale des labels du chemin, calculé dans chercherChemin
 filtre : ('a -> bool), sert à enlever les arcs nuls du graphe entre deux appels récursifs.
 dec et inc : ('a -> 'a -> 'a), servent à décrémenter et incrémenter les labels. 
 >Valeur retournée : ('a graph) nouvelle version du graphe*)
val updateGraph: 'a graph -> id list -> 'a -> ('a -> bool) ->  ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a graph

													  
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
val calculeGrapheEcart: 'a graph -> id -> id -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> bool)  -> 'a graph


(* calcule |(somme labels arc sortant) - (somme labels arc entrant)| pour le noeud id*)
val differenceFlowInOut: 'a graph -> id -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a
											   
											   
(* cette fonction reconstitue le graphe initial avec les bonnes valeurs de flows,
   à partir du résultat de calculeGrapheEcart, soit à partir du graphe d'écart 
   >Params : gr : ('a graph), graphe initial.
   grEcart : ('a graph) graphe d'écart calculé par calculeGrapheEcart
   soustraction : ('a -> 'a -> 'a) pour soustraire deux labels entre eux
   >Valeur retournée : ('a graph), avec le flow maximal *)
val graphFlowFF: 'a graph -> 'a graph -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a graph

											  
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
val fordFulkerson: 'a graph -> id -> id -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> ('a -> bool) -> 'a graph


(*calcule le graphe de flow maximal de gr en appliquant l'algorithme de
  ford fulkerson pour les graphes ayant des labels int.
  Cette fonction a pour but de faciliter l'utilisation du module à l'extérieur
  avec car elle demande moins d'arguments comme elle est non polymorphique*)
val fordFulkersonInt: int graph -> id -> id -> int graph

						   
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
val flowMax: 'a graph -> id -> id -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a

										     
(* version non polymorphique (int) de la fonction ci-dessus *)
val flowMaxInt: int graph -> id -> id -> int

