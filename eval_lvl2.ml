
let eval l =  

	(*
	* la fonction parc parcourt la liste UNE fois afin de faire son travail: calculer
	* Dans cette tâche, elle aura l'aide de 5 paramètres:
	*
	* - l1 -> Type: Liste; l1 est la liste qui est donnée en paramètre d'eval.
	*
	* - intStockage: Entier; intStockage est un entier que j'utilise pour stocker le résultat du calcul, on l'initialise à 0
	*
	* - booVide: Entier (bool); booVide est un entier que j'utilise comme un bool: c'est à dire que je le met à 1 quand je suis sur la première case et à 0 quand je suis sur une autre. Je le mets à 1 après chaque signe.
	*
	* - booModeNegative: Entier (bool); booModeNegative est aussi un entier que j'utilise comme bool: ici, il prendra pour valeur de base 0 et changera en 1 lorsque le premier caractère est '-', son utilité sera précisé plus tard.
	*
	* - booSignePrecedent: Entier (bool); booSignePrecedent permet de savoir s'il y a un signe à la position précédente (un signe peut-être: '*', '-', '+', '/'.)
	*)
	
  let rec parc l1 intStockage booVide booModeNegative booSignePrecedent = match l1 with

		(* 
		*On entre dans les signes, j'ai distingué deux types de signe: les signes ('+' et '-') pouvant s'enchaîner et ceux ne pouvant pas ('*' et '/'). Par exemple, ['1';'*';'-';'2'] est possible et retourne -2 alors que ['1';'*';'*';'2'] ne l'est pas et retourne 'error'.  
		*)
		
    |e::l when e = '-' -> if booModeNegative = 0 || booSignePrecedent = 1 then
          parc [] (intStockage - ((parc l 0 1 1 1)*10)) 0 0 0
        else 
          parc [] (intStockage + ((parc l 0 1 0 1)*10)) 0 0 0 

		(* /!\ On voit que le booModeNegative a été activé dans l'appel de parc précédent!! Alarme!
		* En effet, j'ai mis du temps à trouver mais j'utilise un bool pour activer le 'mode négative'. Si je n'avais pas fait ça, mon programme aurait pris ['-';'1';'2';'-';'1';'2'] et aurait retourné 0 car il le lit comme suit:
		* -(12-12) donc -(0) donc 0.
		* Le 'mode négative' permet de faire en échange: -(12 + 12) donc -(24) donc -24! 
*)

    |e::l when e = '+' -> if booModeNegative = 0 || booSignePrecedent = 1 then 
          parc [] (intStockage + ((parc l 0 1 booModeNegative 1)*10)) 0 0 0
        else 
							  	parc [] (intStockage - ((parc l 0 1 booModeNegative 1)*10)) 0 0 0
          
		(* Dans le if suivant, on cherche à voir si les signes '*' ou '/' n'ont aucun précédent.
		* -> ['1';'*';'1';'2'] a un précédent et retourne 12 alors que ['*';'1';'2'] n'en a pas et retourne "error"...
*)

    |e::l when e = '*' -> if booVide = 1 then failwith "error" 
        else parc [] (intStockage * (parc l 0 1 booModeNegative 1)) 0 0 0
 
          
    |e::l when e = '/' -> if booVide = 1 then failwith "error" 
        else parc [] (intStockage / (parc l 0 1 booModeNegative 1)) 0 0 0

		(*
		* C'est toujours ici où le gros de la magie opère.
		* Ici, on 'stocke' dans intStockage chaque nombre que l'on multiplie par 10.
		* Ainsi, avec cette entrée ['1';'2';'3'], nous avons au départ 0, puis (1+0) * 10 puis (2+10)*10 puis (120+3) * 10 soit 1230. Le 0 en trop sera retiré plus tard. Cela permet de ne parcourir la liste qu'une seule fois!
*)

    |e::l when e = '0' -> parc l ((0+intStockage)*10) 0 booModeNegative 0
    
    |e::l when e = '1' -> parc l ((1+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '2' -> parc l ((2+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '3' -> parc l ((3+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '4' -> parc l ((4+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '5' -> parc l ((5+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '6' -> parc l ((6+intStockage)*10) 0 booModeNegative 0

    |e::l when e = '7' -> parc l ((7+intStockage)*10) 0 booModeNegative 0
    
    |e::l when e = '8' -> parc l ((8+intStockage)*10) 0 booModeNegative 0
    
    |e::l when e = '9' -> parc l ((9+intStockage)*10) 0 booModeNegative 0
    
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
                                                                                              
  in parc l 0 1 0 0;;