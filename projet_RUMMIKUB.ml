(*POJET – LE JEU DU RUMMIKUB*)
(*Q1 : Implémenter les fonctions spécifiées dans la section 3.1*)
type nat = int ;; (* nat >0 *)
type 'a multielement = 'a * nat ;;
type 'a multiensemble =
	|  Vide
	|  C of 'a multielement * 'a multiensemble ;;
(*1.Cardinalité d’un multi-ensemble :*)
let rec cardinal (s : 'a multiensemble) : int =
	match s with
	 | Vide -> 0
	 | C((x,m),suite) -> cardinal suite + m ;;
(*pour calculer le nombre total d’occurrences des éléments de ens, on fait les additions pour tous les nombres.*)
(*2. Nombre d’occurrences d’un élément :*)
 let rec nbocc ( elt : 'a  ) ( ens : 'a multiensemble ) : int =
	match ens with
	 | Vide -> 0
	 | C((x,m),suite) -> if x == elt then m + nbocc elt suite else nbocc elt suite ;;
(*pour calculer le nombre d’occurrences de elt dans ens, on utilise une boucle ‘if’ pour deux cas différents. Si x est égale a elt, on additionne. Sinon on ne fait rien *)
(*3. Appartenance d’un élément à un multi-ensemble :*)
 let rec appartient ( elt :'a ) ( ens : 'a multiensemble) : bool =
	match ens with
	 | Vide -> false
	 | C((x,m),suite) -> if x == elt then true else appartient elt suite  ;;
(*pour confirmer si elt ∈ mens, on utilise un filtrage s’il est dedans, on renvoie ‘true’.*)
(*4. Inclusion de deux multi-ensembles :*)
 let rec inclus ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : bool =
	match ens1 with
 	 | Vide -> true
	 | C((x,m),suite) -> if appartient x ens2 && (nbocc x ens1 <= nbocc x ens2 ) then inclus suite ens2 else false ;;
(*pour confirmer si ens1 ⊆ ens2 , si et seulement si chaque multiélément dans ens1 appartient dans ens2 et le nombre d’occurrence de ce multiélément dans ens1 est inférieur ou égale a ce lui dans ens2.*)
(*5. Ajout d’un multi-élément à un multi-ensemble*)
let rec ajoute (elt : 'a multielement) ( ens : 'a multiensemble ) : 'a multiensemble =
	let (x,m) = elt in
match ens with
| Vide -> C(elt,ens)
| C((y,n),suite) ->  if y==x then C((x,m+n),suite) else C((y,n), ajoute elt suite) ;;
(*pour ajouter un multiélément et éviter de le répéter, on utilise une boucle ‘if’, s’il est dans ens, on fait seulement une addition de deux nombres, sinon on ajoute un nouveau multiélément à la fin.*)
(*6. Suppression d’un multi-élément d’un multi-ensemble :*)
let rec supprime ( elt : 'a multielement ) ( ens : 'a multiensemble ) : 'a multiensemble =
	let (x,n)=elt in
	match ens with
		| Vide -> ens
		| C((y,m),suite) -> if ( y == x && n < m ) then C((y,m-n),suite) else if ( y ==x && n >= m ) then suite else C((y,m), supprime elt suite ) ;;
(*pour supprimer n occurrences de l’élément x du multi-ensemble ens, on a trois cas différents. Premier cas si elt  appartient dans ens et n est inferieur a m, on fait seulement une soustraction (m-n). Deuxième cas si elt  appartient dans ens mais n est supérieur ou égale a m, donc on directement saute ce multiélément, on le ne compte pas. Le dernier cas si elt n’appartient pas dans ens, on ne fait rien.  *)
(*7. Égalité de deux multi-ensembles :*)
let egaux ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : bool =
	if ( cardinal ens1 == cardinal ens2 && inclus ens1 ens2 && inclus ens2 ens1) then true else false
(*pour confirmer si 𝑒𝑛𝑠1 et 𝑒𝑛𝑠2 ont les mêmes multiéléments, on utilise les fonctions qu’on a défini avant. Si le nombre total d’occurrences des éléments de ens1 et ens2 sont égaux et puis ens1 appartient dans ens2 et ens2 appartient dans ens1. Donc on peut  dire que ens1 et ens2 ont les même multiéléments. *)
(*8. Intersection de deux multi-ensembles :*)
let rec intersection ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : 'a multiensemble =
	match ens1 with
		| Vide -> Vide
		| C((x,m),suite) -> if appartient x ens1 then
								If m >= nbocc x ens2 then ( ajoute (x , nbocc x ens2) ( intersection suite ens2 ))
						   	else ( ajoute (x , m) ( intersection suite ens2 ))
					    else  ( intersection suite ens2 ) ;;
(*on utilise un filtrage pour sélectionner les multiéléments en commun. Pour chaque elt dans ens1, s’il appartient dans ens2, on utilise la fonction ‘ajoute’ pour le mettre dans le ens qu’on va renvoyer. Sinon on ne le met pas, on continue ce filtrage.*)
(*9. Différence de deux multi-ensembles :*)
let rec difference ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : 'a multiensemble =
	match ens1 with
		| Vide -> Vide
		| C((x,m),suite) -> if appartient x ens2 == false then  (ajoute (x,m) difference (suite ens2)) else difference suite ens2 ;;
(*dans cette fonction, pour chaque elt dans ens1, s’il n’appartient pas à ens2, on utilise la fonction ‘ajoute’ pour le mettre dans le ens qu’on va renvoyer. Sinon on ne le met pas, on continue ce filtrage.*)
(*10. Obtention aléatoire d’un élément d’un multi-ensemble :*)
let rec ieme ( n : int ) ( ens : 'a multiensemble ) : 'a =
match ens with
	| Vide -> failwith "erreur"
	| C((x,m),suite) -> if n > m then ieme (n-m) suite else x
let un_dans ( ens : 'a multiensemble ) : 'a =
	let n = Random.int ( cardinal ens) in ieme n ens ;;
(*tout d’abord, on définit une fonction ‘ieme’ pour renvoyer le ie élément de ens, et puis on peut utiliser ‘random.int’ pour avoir un élément choisi aléatoirement dans le multi-ensemble ens.*)
(*Q2. Réusiner le code de la question précédente en ré-implémentant le type multiensemble grâce aux listes natives d’Ocaml :*)
type 'a multielement = 'a * int ;;
type 'a multiensemble = 'a multielement list ;;

(*1.	Cardinalité d’un multi-ensemble :*)
let rec cardinal ( ens : 'a multiensemble ) : int =
		match ens with
		| [] -> 0
		| (x,m) ::suite -> m + cardinal suite ;;

(*2.	Nombre d’occurrences d’un élément :*)
let rec nbocc ( elt : 'a  ) ( ens : 'a multiensemble ) : int =
	match ens with
	 | [] -> 0
	 | (x,m)::suite -> if x == elt then m + nbocc elt suite else nbocc elt suite;;

(*3.	Appartenance d’un élément à un multi-ensemble :*)
let rec appartient ( elt : 'a ) ( ens : 'a multiensemble) : bool =
	match ens with
	 | [] -> false
	 | (x,m)::suite -> if x == elt then true else appartient elt suite  ;;
(*4.	Inclusion de deux multi-ensembles :*)
let rec inclus ( ens1 : ‘a multiensemble ) ( ens2 : ‘a multiensemble ) : bool =
	match ens1 with
 	 | [] -> true
	 | (x,m)::suite -> if  appartient x ens2 && (nbocc x ens1 <= nbocc x ens2 ) then inclus suite ens2 else false ;;

(*5.	Ajout d’un multi-élément à un multi-ensemble :*)
let rec ajoute (elt : 'a multielement) ( ens : 'a multiensemble ) : 'a multiensemble =
	let (x,m) = elt in
match ens with
 | [] -> elt::ens
 | (y,n)::suite ->  if y==x then (x,m+n)::suite else (y,n)::ajoute elt suite ;;

(*6.	Suppression d’un multi-élément d’un multi-ensemble :*)
let rec supprime ( elt : 'a multielement ) ( ens : 'a multiensemble ) : 'a multiensemble =
	let (x,m)=elt in
	match ens with
		| [] -> ens
		| (y,n)::suite -> if ( y == x && m <= n ) then (y,n-m)::suite else if ( y ==x && m > n ) then suite else (y,n)::supprime elt suite ;;


(*7.	Égalité de deux multi-ensembles :*)
let egaux ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : bool =
if ( cardinal ens1 == cardinal ens2 && inclus ens1 ens2 && inclus ens2 ens1) then true else false

(*8.	Intersection de deux multi-ensembles :*)
let rec intersection ( ens1 : ‘a multiensemble ) ( ens2 : ‘a multiensemble ) : ‘a multiensemble =
	match ens1 with
		| [] -> []
		| (x,m)::suite -> if appartient x ens1 then
								if m >= nbocc x ens2 then ( ajoute (x , nbocc x ens2) ( intersection suite ens2 ))else ( ajoute (x , m) ( intersection suite ens2 ))
					    else  ( intersection suite ens2 ) ;;
(*9.	Différence de deux multi-ensembles :*)
let rec difference ( ens1 : 'a multiensemble ) ( ens2 : 'a multiensemble ) : 'a multiensemble =
	match ens1 with
		| [] -> []
		| (x,m)::suite -> if appartient x ens2 == false then  (ajoute (x,m) difference (suite ens2)) else difference suite ens2 ;;


(*10.	Obtention aléatoire d’un élément d’un multi-ensemble :*)
let rec ieme ( n : int ) ( ens : 'a multiensemble ) : 'a =
match ens with
	 | [] -> failwith "erreur"
	 | (x,m)::suite -> if n > m then ieme (n-m) suite else x
let un_dans ( ens : 'a multiensemble ) : 'a =
	let n = Random.int ( cardinal ens) in ieme n ens ;;

