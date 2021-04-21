
let eval l =  

  let rec parc l1 intStockage booVide booModeNegative booSignePrecedent booParenthese = match l1 with

	
	|e::l when e = '(' -> parc l intStockage 0 0 0 (booParenthese + 1)
	
	|e::l when e = ')' -> if booParenthese < 1 then failwith "error" else parc l intStockage 0 0 0 (booParenthese - 1)
	
    |e::l when e = '-' -> if booModeNegative = 0 || booSignePrecedent = 1 then
          parc [] (intStockage - ((parc l 0 1 1 1 booParenthese)*10)) 0 0 0 0
        else 
          parc [] (intStockage + ((parc l 0 1 0 1 booParenthese)*10)) 0 0 0 0


    |e::l when e = '+' -> if booModeNegative = 0 || booSignePrecedent = 1 then 
          parc [] (intStockage + ((parc l 0 1 booModeNegative 1 booParenthese)*10)) 0 0 0 0
        else 
							  	parc [] (intStockage - ((parc l 0 1 booModeNegative 1 booParenthese)*10)) 0 0 0 0
         

    |e::l when e = '*' -> if booVide = 1 then failwith "error" 
        else parc [] (intStockage * (parc l 0 1 booModeNegative 1 booParenthese)) 0 0 0 0
 
          
    |e::l when e = '/' -> if booVide = 1 then failwith "error" 
        else parc [] (intStockage / (parc l 0 1 booModeNegative 1 booParenthese)) 0 0 0 0


    |e::l when e = '0' -> parc l ((0+intStockage)*10) 0 booModeNegative 0 booParenthese
    
    |e::l when e = '1' -> parc l ((1+intStockage)*10) 0 booModeNegative 0 booParenthese

    |e::l when e = '2' -> parc l ((2+intStockage)*10) 0 booModeNegative 0 booParenthese

    |e::l when e = '3' -> parc l ((3+intStockage)*10) 0 booModeNegative 0 booParenthese

    |e::l when e = '4' -> parc l ((4+intStockage)*10) 0 booModeNegative 0 booParenthese

    |e::l when e = '5' -> parc l ((5+intStockage)*10) 0 booModeNegative 0 booParenthese

    |e::l when e = '6' -> parc l ((6+intStockage)*10) 0 booModeNegative 0 booParenthese 

    |e::l when e = '7' -> parc l ((7+intStockage)*10) 0 booModeNegative 0 booParenthese
    
    |e::l when e = '8' -> parc l ((8+intStockage)*10) 0 booModeNegative 0 booParenthese
    
    |e::l when e = '9' -> parc l ((9+intStockage)*10) 0 booModeNegative 0 booParenthese
    
    |e::l -> 0
                                                                                             
    |[] -> if booVide = 1 || booParenthese <> 0 then 
          failwith "error" 
        else
          intStockage / 10 
                                                                                              
  in parc l 0 1 0 0 0;;
  
  
 





