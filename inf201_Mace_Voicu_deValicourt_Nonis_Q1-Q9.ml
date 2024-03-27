(* -----------------------------------------------------------------------------
   inf201_Mace_Stefan_deValicourt_Nonis_Q1-Q9.ml : cr Q1 Q9 projet
   Erwan Mace <Erwan.Mace@etu.univ-grenoble-alpes.fr>
   David Voicu <David.Voicu@etu.univ-grenoble-alpes.fr>
   Amaury de Valicourt <Amaury.De-Valicourt@etu.univ-grenoble-alpes.fr>
   Dario Nonis <Dario.Nonis@etu.univ-grenoble-alpes.fr>
   -------------------------------------------------------------------------- *)

type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = defaut;;

(*Q1 : 
1) Correspond à la base sud (la notre)
2) Correspond à la base nord (la base de l'adversaire)
3) Correspond à la base nord ouest
4) Correspond a la pointe nored du plateau
5) Correspond au point le plus a gauche de la base sud
6) Forme un triangle qui englobe tout le plateau et qui exclus les base sud, nord est et nord ouest
*)
(*A MODIFIER en Q2*)
let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
  -dim<=j && j<= dim && -dim <= k && k <= dim;;           

(*A MODIFIER en Q3
Choix de definir l'étoile par l'union de trois grand losange
*)
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool = 
  (-2*dim <= i && i <= 2*dim) || (-2*dim <= j && j <= 2*dim) || (-2*dim <= k && k <= 2*dim);;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

(*Q4*)
let tourner_case (m:int) (c:case) : case = 
	let i,j,k = c in
		match m mod 6 with
		|0 -> (i,j,k)
		|1 | -5 -> (-k,-i,-j)
		|2 | -4 -> (j,k,i)
		|3 | -3 -> (-i,-j,-k)
		|4 | -2 -> (k,i,j)
		|5 | -1 -> (-j,-k,-i)
[@@warning "-8"]  
;;


(*Q5*)
let translate (c:case) (v:vecteur) : case = 
	let c1,c2,c3 = c and v1,v2,v3 = v in
		(c1+v1,c2+v2,c3+v3);;
(*Q6*)
let diff_case (c1:case) (c2:case) : vecteur = 
	let i1,j1,k1 = c1 and i2,j2,k2 = c2 in
		(i1-i2,j1-j2,k1-k2);;
(*Q7*)
let sont_cases_voisines (c1:case) (c2:case) : bool = 
	let vect_diff = diff_case c1 c2 in
		let i,j,k = vect_diff in
			(abs(i) = 1 && abs(j) = 1) || (abs(i) = 1 && abs(k) = 1) || (abs(j) = 1 && abs(k) = 1);;


(*Q8*)
let calcul_pivot (c1:case) (c2:case) : case option = 
	let i1,j1,k1 = c1 and i2,j2,k2 = c2 in
		if i1=i2 && abs(j1 mod 2) = abs(j2 mod 2) && abs(k1 mod 2) = abs(k2 mod 2) then
			Some (i1,(j1+j2)/2,(k1+k2)/2)
		else if j1=j2 && abs(i1 mod 2) = abs(i2 mod 2) && abs(k1 mod 2) = abs(k2 mod 2) then
			Some ((i1+i2)/2,j1,(k1+k2)/2)
		else if k1=k2 && abs(i1 mod 2) = abs(i2 mod 2) && abs(j1 mod 2) = abs(j2 mod 2) then
			Some ((i1+i2)/2,(j1+j2)/2,k1)
		else 
			None;;

(*Q9*)
let vec_et_dist (c1:case) (c2:case):vecteur*int = 
  let i,j,k = diff_case c1 c2 in
  let d = max (max i j) k in
  ((i/d,j/d,k/d),d);;

(*Q10*)
let tourner_liste(s:'a list):'a list=
  match s with
  |[] -> []
  |pr::fin -> fin@[pr];;

let rec der_liste(s:'a list):'a=
  match s with
  |[pr] -> pr
  |pr::fin -> der_liste fin;;

(*Q11*)
type case = int*int*int;;
let rec remplir_segment(m:int)(c:case):case list=
  let i,j,k = c in
  match m with
  |m when m<=0 -> []
  |_ -> [c]@remplir_segment (m-1) (i,j+1,k-1)
;;


let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;


(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)
