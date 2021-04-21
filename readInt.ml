let readInt l = 

	(*
	* la fonction parc parcourt la liste UNE fois afin de faire son travail: lire un entier
	* Dans cette tâche, elle aura l'aide de 5 paramètres:
	*
	* - l1 -> Type: Liste; l1 est la liste qui est donnée en paramètre de readInt
	*
	* - intStockage: Entier; intStockage est un entier que j'utilise pour stocker le résultat du calcul, on l'initialise à 0
	*
	* - booVide: Entier (bool); booVide est un entier que j'utilise comme un bool: c'est à dire que je le met à 1 quand je suis sur la première case et à 0 quand je suis sur une autre. Je le mets à 1 après chaque signe
	*
	* - booSigne: Entier (bool); booSigne est un entier que j'utilise comme un bool: c'est à dire que je l'initialise à 0 puis, quand il y a un signe sur la case, je le mets à 1 afin d'éviter que deux signes s'enchaînent.
	*)

  let rec parc l1 intStockage booVide booSigne = match l1 with
		
		(* Dans le if qui suit, j'ai décidé que le signe doit forcément être le premier caractère car dans readInt, nous cherchons bien à lire un entier, pas à calculer *)
    |e::l when e = '-' -> if booSigne = 0 && booVide = 1 then 
          parc [] (intStockage - ((parc l 0 1 1)*10)) 0 0
        else failwith "error"

          
    |e::l when e = '+' -> if booSigne = 0 && booVide = 1 then parc [] (intStockage + ((parc l 0 1 1)*10)) 0 0
								else failwith "error"
          
		(*
		* Bienvenue dans la partie où le gros de la magie opère.
		* Ici, on 'stocke' dans intStockage chaque nombre que l'on multiplie par 10.
* Ainsi, avec cette entrée ['1';'2';'3'], nous avons au départ 0, puis (1+0) * 10 puis (2+10)*10 puis (120+3) * 10 soit 1230. Le 0 en trop sera retiré plus tard. Cela permet de ne parcourir la liste qu'une seule fois!
                                                                    *)

    |e::l when e = '0' -> parc l ((0+intStockage)*10) 0 0
    
    |e::l when e = '1' -> parc l ((1+intStockage)*10) 0 0

    |e::l when e = '2' -> parc l ((2+intStockage)*10) 0 0

    |e::l when e = '3' -> parc l ((3+intStockage)*10) 0 0

    |e::l when e = '4' -> parc l ((4+intStockage)*10) 0 0

    |e::l when e = '5' -> parc l ((5+intStockage)*10) 0 0

    |e::l when e = '6' -> parc l ((6+intStockage)*10) 0 0

    |e::l when e = '7' -> parc l ((7+intStockage)*10) 0 0
    
    |e::l when e = '8' -> parc l ((8+intStockage)*10) 0 0
    
    |e::l when e = '9' -> parc l ((9+intStockage)*10) 0 0
    
    |e::l -> 0
                                                                                            
		(* La liste vidée, on teste si:
		* - la liste est vide de base.
		*
		* Le /10 est là pour retirer le 0 en trop vu précédemment.
*)

    |[] -> if booVide = 1 then 
          failwith "error" 
        else 
          intStockage / 10
  in parc l 0 1 0 ;;

