(* -----------------------------------------------------------------------------
   inf201_Mace_Voicu_deValicourt_Nonis.ml : cr projet
   Erwan Mace <Erwan.Mace@etu.univ-grenoble-alpes.fr>              
   David Voicu <David.Voicu@etu.univ-grenoble-alpes.fr>                 \
   Amaury de Valicourt <Amaury.De-Valicourt@etu.univ-grenoble-alpes.fr>  > Groupe IMA 4/Groupe TP O et N
   Dario Nonis <Dario.Nonis@etu.univ-grenoble-alpes.fr>                 /
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

let rec associe (a:'a) (l:('a*'b) list) (defaut:'b) :'b = 
  match l with
    |[]->defaut
    |(case,col)::fin -> if case = a then
                          col
                        else
                          associe a fin defaut;;

(*Q1 : 
1) Correspond à la base sud (la notre)
2) Correspond à la base nord (la base de l'adversaire)
3) Correspond à la base nord ouest
4) Correspond a la pointe nored du plateau
5) Correspond au point le plus a gauche de la base sud
6) Forme un triangle qui englobe tout le plateau et qui exclus les base sud, nord est et nord ouest
*)
(*A MODIFIER en Q2*)
let est_dans_losange ((i,j,k):case) (dim:dimension) : bool =
  -dim<=j && j<= dim && -dim <= k && k <= dim;;           

(*A MODIFIER en Q3
Choix de definir l'étoile par l'union de trois grand losange
*)
let est_dans_etoile ((i,j,k):case) (dim:dimension) : bool = 
  (est_dans_losange (i,j,k) dim) || (est_dans_losange (k,i,j) dim) || (est_dans_losange (j,k,i) dim);;

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
[@@warning "-8"]  (*Içi on traite bien tout les cas donc pas de problème*)
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
	let i,j,k = diff_case c1 c2 in
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
let vec_et_dist (c1:case) (c2:case) : vecteur*int = 
  let i,j,k = diff_case c1 c2 in
  let d = max (max (abs (i)) (abs (j))) (abs (k)) in 
  if d = 0 then 
    (0,0,0),0
  else
    ((i/d,j/d,k/d),d);;

(*Q10*)
let tourner_liste (s:'a list) : 'a list =
  match s with
  |[] -> []
  |pr::fin -> fin @ [pr];;

let rec der_liste (s:'a list) : 'a =
  match s with
  |[pr] -> pr
  |pr::fin -> der_liste fin
[@@warning "-8"]
;;

(*Q11*)
let rec remplir_segment (m:int) (c:case) : case list =
  let i,j,k = c in
    match m with
    |m when m<=0 -> []
    |_ -> [c] @ remplir_segment (m-1) (i,j+1,k-1)
;;

(*Q12*)
let rec remplir_triangle_bas (m:int) (c:case) : case list =
  let i,j,k = c in
    match m with
    |m when m<=0 -> []
    |_ -> remplir_segment m c @ remplir_triangle_bas (m-1) (i-1,j+1,k)
;;

(*Q13*)
let rec remplir_triangle_haut (m:int) (c:case) : case list =
  let i,j,k = c in
  match m with
  |m when m<=0 -> []
  |_ -> remplir_segment m c @ remplir_triangle_haut (m-1) (i+1,j,k-1)
;;

(*Q14*)
let rec colorie (coul:couleur) (lc:case list) : case_coloree list =
  match lc with
  |[] -> []
  |pr::fin -> [pr,coul] @ colorie coul fin
;;

(*Q15 - fonction auxilière pour calculer le nb de joueur (longueur de la liste couleur)*)
let rec len (s:'a list) : int= 
  match s with
  |[] -> 0
  |pr::fin -> 1 + len fin
;;

(*fonction auxilière pour tourner toutes les cases d'une liste*)
let rec tourner_liste_case (m:int) (s:case_coloree list) : case_coloree list =
  match s with
  |[] -> []
  |(case,col)::fin -> [(tourner_case m case,col)] @ tourner_liste_case m fin
;;

let tourner_config (case_col,coul,dim:configuration) : configuration =
  let m = 6/(len coul) in 
    tourner_liste_case m case_col,tourner_liste coul,dim
;;

(*Q16*)
(*Fonction auxiliaire qui permet d'obtenir une liste de toute les case d'une config initiale*)
let rec coord_case (n:int) (ljoueurs:couleur list) (dim:dimension) : case_coloree list =
  match ljoueurs with
  |[] -> []
  |pr::fin -> tourner_liste_case (-1*6/n) (colorie pr ( remplir_triangle_bas dim (-dim-1,1,dim))@coord_case n fin dim)
;;
(*Fonction auxiliaire pour permettre d'effacer le dernier element d'une liste*)
let rec effacer_dernier (l:'a list) : 'a list =
  match l with
  |[pr] -> []
  |t::q -> t::(effacer_dernier q)
[@@warning "-8"]
;;

let remplir_init (ljoueurs:couleur list) (dim:dimension) : configuration =
  tourner_config (coord_case (len ljoueurs) ljoueurs dim,[der_liste ljoueurs]@(effacer_dernier ljoueurs),dim)
;;(*Pour la liste des joueur on doit deplacer le dernier élément au debut de la liste pour que l'ordre correspond avec celui du plateau*)

(*Q17*)
let quelle_couleur (c:case) ((l_case_color,l_col,dim):configuration) : couleur=
  associe c l_case_color Libre;;

(*Q18*)
let rec supprime_dans_config (c:case) (conf:configuration) : configuration =
  match conf with
  |([],l_col,dim) ->([],l_col,dim) 
  |((p,col)::fin,l_couleurs,dim) -> let l_points,l_couleurs,dim = supprime_dans_config c (fin,l_couleurs,dim) in
                                      if c = p then
                                        l_points,l_couleurs,dim
                                      else
                                        [p,col] @ l_points,l_couleurs,dim
;;
(*Question dans le désordre pour le bon fonctionnement du programme*)
(*Q22*)
(*Petite fonction auxiliaire qui renvoi true si une case est libre (pour simplifier les fonctions suivante)*)
let case_libre (c:case) (conf:configuration):bool=
  quelle_couleur c conf = Libre;;

let rec est_libre_seg (x1,y1,z1:case) (x2,y2,z2:case) (conf:configuration):bool=
  let (u,v,w),dist = vec_et_dist (x1,y1,z1) (x2,y2,z2) in 
  match (x2,y2,z2),dist with
  |_,1 -> true
  |_,_ -> case_libre (x2+u,y2+v,z2+w) conf && est_libre_seg (x1,y1,z1) (x2+u,y2+v,z2+w) (conf);;

(*Q23*)
let est_saut (x1,y1,z1 : case) (x2,y2,z2 : case) (l,li,dim : configuration) = 
  let (u,v,w),d = vec_et_dist (x1,y1,z1) (x2,y2,z2) in 
  (d = 2) && (est_dans_losange (x2,y2,z2) dim ) && (not (case_libre (x2+u,y2+v,z2+w) (l,li,dim))) && (case_libre (x2,y2,z2) (l,li,dim)) && (est_dans_losange (x1,y1,z1) dim);;

(*Q24*)
let rec est_saut_multiple (l : case list) (conf : configuration):bool =
  match l with 
  |[a] -> est_case a
  |(x1,y1,z1)::(x2,y2,z2)::fin -> let (u,v,w),d = vec_et_dist (x1,y1,z1) (x2,y2,z2)  in 
      est_case (x1,y1,z1) && est_case (x2,y2,z2) && (d = 2) && (not (case_libre (x2+u,y2+v,z2+w) conf)) && (case_libre (x2,y2,z2) conf) && 
      est_saut_multiple ((x2,y2,z2)::fin) conf
[@@warning "-8"]
;;
(*Le match ici est non exhaustive car on ne traite pas le cas d'une liste vide mais c'est voulu*)

(*Q19*)
let est_coup_valide (lcase,pr::fin,dim:configuration) (c:coup) : bool =
  match c with
  |Du(c1,c2) -> est_case c1 && est_case c2 && sont_cases_voisines c1 c2 && quelle_couleur c1 (lcase,pr::fin,dim) = pr && quelle_couleur c2 (lcase,pr::fin,dim) = Libre && est_dans_losange c2 dim
  |Sm(l) -> est_saut_multiple l (lcase,pr::fin,dim) && est_dans_losange (der_liste l) dim
[@@warning "-8"]
;;

(*Q20*)
(*Cette fonction renvoie un couple avec la première et la dernière case d'un coup peu import le type du coup*)
let case_deb_fin (cp:coup) : case*case =
  match cp with
  |Du(c1,c2) -> c1,c2
  |Sm(l) -> List.hd l, der_liste l
;;

let rec appliquer_coup (lcase,prl::finl,dim:configuration) (cp:coup) : configuration =
  let c1,c2 = case_deb_fin cp in
  match lcase with
  |(c,cou)::fin when c=c1 -> [c2,cou]@fin,[prl]@finl,dim
  |pr::fin -> let lcase2,ljoueur2,dim2 = appliquer_coup (fin,prl::finl,dim)(Du(c1,c2)) in [pr]@lcase2,ljoueur2,dim2
[@@warning "-8"]
;; 

(*Q21*)
let mettre_a_jour_configuration (lcase,ljoueur,dim:configuration) (cou:coup) : configuration =
  match est_coup_valide (lcase,ljoueur,dim) cou with
  |true -> tourner_config(appliquer_coup (lcase,ljoueur,dim) cou)
  |false -> failwith "Ce coup n'est pas valide, le joueur doir rejouer"
;;

(*Q26*)
let score (lcase,ljoueur,dim:configuration) : int =
  List.fold_left (fun acc ((i,j,k),cou) -> if cou = (List.hd ljoueur) then acc + i else acc) 0 lcase
;;

(* Renvoie une liste des entiers de 1 à n *)
let rec liste_facteurs (n:int) : int list =
  match n with
  |1 -> [1]
  |x -> liste_facteurs (x-1) @ [x]
;;

(* Renvoie le n-ième élément de la liste l *)
let rec n_ieme_elem (n:int) (l:int list) : int =
  match n with
  |1 -> List.hd l
  |x -> n_ieme_elem (x-1) (List.tl l)
;;

let score_gagnant (d:dimension) : int =
  let l_fact = liste_facteurs (d) in
  List.fold_right (fun x acc -> acc + x*(d+(n_ieme_elem x (List.rev l_fact)))) l_fact 0
;;

(*Q27*)
let gagne (lcase,ljoueur,dim:configuration) : bool =
  score (lcase,ljoueur,dim) = score_gagnant dim
;;

(*Q28*)
let verif_coup_list (conf:configuration) (lcoups:coup list) : bool =
  fst(List.fold_left (fun acc coup -> let verite,config = acc in 
                       verite && est_coup_valide config coup, if est_coup_valide config coup then
                                                                mettre_a_jour_configuration config coup
                                                              else
                                                                config)
      (true,conf) lcoups)
;;


let est_partie (config:configuration) (c_l:coup list) : couleur =
  if verif_coup_list config c_l then 
    fst(List.fold_left (fun acc coup -> let coul,(lcase,ljoueur,dim) = acc in 
                         let lcase_c,ljoueur_c,dim_c = mettre_a_jour_configuration (lcase,ljoueur,dim) coup in
                         if (gagne (lcase_c,ljoueur_c,dim_c)) then
                           (List.hd ljoueur_c),(lcase_c,ljoueur_c,dim_c)
                         else
                           (coul,(lcase_c,ljoueur_c,dim_c)))
          (Libre,config) c_l)
  
  else
    failwith "La liste de coups n'est pas valide"
;;


let couleur2string (coul:couleur) : string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M "
;;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;

let affiche (config:configuration) : unit =
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
    affiche_aux (2*dim+1)
;;

(*Voici une liste de test réalisé pour garantir le bon fonctionnement du programme*)

(*let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

let conf_2=remplir_init [Rouge;Vert;Bleu] 3;;
score conf_reggae;;


let conf_test1 = remplir_init [Vert;Jaune;Rouge;Noir;Bleu;Marron] 3;;
affiche conf_test1;;

let conf_test2 = remplir_init [Vert;Jaune;Rouge;] 3;;
affiche conf_test2;;

let conf_test3 = remplir_init [Vert;Jaune] 3;;
affiche conf_test3;;

let conf_test4 = remplir_init [Vert] 3;;
affiche conf_test4;;


let conf3 = remplir_init [Rouge;Vert;Bleu] 1;;
affiche conf3;;

let conf3_c1 = mettre_a_jour_configuration conf3 (Du((-2,1,1),(-1,1,0)));;
affiche conf3_c1;;

let conf3_c2 = mettre_a_jour_configuration conf3_c1 (Du((-2,1,1),(-1,1,0)));;
affiche conf3_c2;;

let conf3_c3 = mettre_a_jour_configuration conf3_c2 (Du((-2,1,1),(-1,0,1)));;
affiche conf3_c3;;

let conf3_c4 = mettre_a_jour_configuration conf3_c3 (Du((-1,1,0),(0,0,0)));;
affiche conf3_c4;; 

let conf3_c5 = mettre_a_jour_configuration conf3_c4 (Du((-1,1,0),(0,1,-1)));;
affiche conf3_c5;;

let conf3_c6 = mettre_a_jour_configuration conf3_c5 (Du((-1,0,1),(0,-1,1)));;
affiche conf3_c6;;

let conf3_c7 = mettre_a_jour_configuration conf3_c6 (Du((0,0,0),(1,0,-1)));;
affiche conf3_c7;;

let conf3_c8 = mettre_a_jour_configuration conf3_c7 (Du((0,1,-1),(0,0,0)));;
affiche conf3_c8;;

let conf3_c9 = mettre_a_jour_configuration conf3_c8 (Du((0,-1,1),(1,-1,0)));;
affiche conf3_c9;;

let conf3_c10 = mettre_a_jour_configuration conf3_c9 (Du((1,0,-1),(2,-1,-1)));;
affiche conf3_c10;;

let conf3_c11 = mettre_a_jour_configuration conf3_c10 (Du((0,0,0),(1,-1,0)));;
affiche conf3_c11;;

let conf3_c12 = mettre_a_jour_configuration conf3_c11 (Du((1,-1,0),(2,-1,-1)));;
affiche conf3_c12;;

gagne conf3_c12;; (* renvoie true*)

let coup_liste_conf3 = [Du ((-2, 1, 1), (-1, 1, 0));
                        Du((-2,1,1),(-1,1,0));
                        Du((-2,1,1),(-1,0,1));
                        Du((-1,1,0),(0,0,0));
                        Du((-1,1,0),(0,1,-1));
                        Du((-1,0,1),(0,-1,1));
                        Du((0,0,0),(1,0,-1));
                        Du((0,1,-1),(0,0,0));
                        Du((0,-1,1),(1,-1,0));
                        Du((1,0,-1),(2,-1,-1));
                        Du((0,0,0),(1,-1,0));
                        Du((1,-1,0),(2,-1,-1))];; 
                          
verif_coup_list conf3 coup_liste_conf3;; 
est_partie conf3 coup_liste_conf3;; 

let conf_test_multiples = remplir_init [Rouge;Bleu;Vert] 3;;
affiche conf_test_multiples;;

let conf_test_multiples_c1 = mettre_a_jour_configuration conf_test_multiples (Sm[(-5,2,3);(-3,2,1)]);;
(* est_saut_multiple [(-5,2,3);(-3,2,1)] conf_test_multiples;; *)
affiche conf_test_multiples_c1;;

let conf_test_multiples_c1_2t = tourner_config (tourner_config conf_test_multiples_c1);;
affiche conf_test_multiples_c1_2t;;

let conf_test_multiples_c2 = mettre_a_jour_configuration conf_test_multiples_c1_2t (Sm[(-5,3,2);(-3,3,0);(-3,1,2)]);;
affiche conf_test_multiples_c2;;



(* Le coup ci-dessous n'est pas valide, on recoit un message de l'interpréteur nous l'indiquant *)
let conf_test_multiples_erreur = mettre_a_jour_configuration conf_test_multiples_c2 (Sm[(-5,3,2);(-3,1,2);(-1,1,0)]);;

let conf_test_multiples_c3 = mettre_a_jour_configuration conf_test_multiples_c2 (Sm[(-5,2,3);(-3,0,3)]);;
let conf_test_multiples_c3_2t = tourner_config (tourner_config conf_test_multiples_c3);;
(* Sortie du losange : ne fonctionne pas *)
let conf_test_multiples_hore_losange = mettre_a_jour_configuration conf_test_multiples_c3_2t (Sm[(-5,3,2);(-3,1,2);(-3,4,-1)]);;




(* let sauts = (Sm[(-5,3,2);(-3,3,0);(-3,1,2)]);; *)
est_saut_multiple [(-5,3,2);(-3,3,0);(-3,1,2)] conf_test_multiples_c1_2t;;
*)
