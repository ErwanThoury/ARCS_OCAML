(* Attention
   If I give a prototype / head of a function, define an error case or a signature, this HAS to be respected.
   If it is not respected I will not give any partial credit!
*)

(* Polynomial *)
(* Polynomials of degree n have the general form P(x) = a0 + a1*x + a2*x^2 + ...  + an*x^n
   These can be represented as a list of length n+1 holding the coefficients like
   [a0, a1, a2, ..., an]
   However in many applications it often happens that many of coefficients are 0 and storing all of them is a
   waste of memory space. Therefore they "compacted" to list of pairs holding degree and coefficient like
   [(0, a0), (2,a2), (n, an)] [So pairs of int*float]
*)

(* 1 Eval : Write a function that evaluates a polynomial given as such a list of pairs at a given value*)
(* Signature int*float list -> float *)
(* Bonus (If you have no clear idea how to tackle this when reading the quesion, skip it and proceed to the next question)
   : Do it with the "minimal" number of multiplications.
     There are two means of reducing them: propagating intermediate results (somewhat tricky) and fast exponentiation and a
     mix of them (Warning to mix them can become very tricky) *)

(*Signature: int*float list -> float -> float *)
let eval l x =
  let rec pow nombre puiss = match puiss with
    | puiss1 when puiss1 > 1 -> nombre *. (pow nombre (puiss1 - 1))
    | 1 -> nombre
    | 0 -> 1.
    | puiss -> failwith "blop"
  in
  let rec recu li base stock = match li with
    | e::li1 -> let puiss2, fac = e in recu li1 base (stock +. (fac *. (pow base puiss2))) 
    | [] -> stock
(* One or two auxiliary recursive functions
   One of them having the POSSIBLE signature int*float list -> float -> float -> float or int*float list -> float -> float -> int -> float *)
  in match l with
  | [e] -> let puiss, fac = e in fac *. (pow x puiss) (* A case where you can directly give the solution *)
  | l -> recu l x 0.(* Actual call *);;
(* Example 
   eval [(4,1.)] 2. -> 1.*2.^4 -> 16.
   eval [(2,2.); (4,1.)] 2. -> 2.*(2.^2) + 1.*(2.^4) -> 24.
*)

(* Test *)
let r = eval [] 1.1 in
if not ((-0.001) < r && r < 0.001) then
  print_endline "Faux";
let r = eval [(0,1.)] 1.1 in
if not (0.999 < r && r < 1.001) then
  print_endline "Faux";
let r = eval [(0,1.); (4,2.)] 2. in
if not (32.999 < r && r < 33.001) then
  print_endline "Faux";;

(* 2 Add : Write a function that adds a monomial (given as pair (k,ak)) to a given polynomial
           Attention 1: this monomial can or can not be present in the given polynomial
           Attention 2: This list of monomial is sorted with respected to the exponent, keep this order*)

(* Hint : You can get inspiration from searching an element in a sorted list *)

(* Example 
   add [(4,1.)] (4,2.) -> 1.*x^4 + 2.*x^4 -> 3.*x^4 -> [(4,3.)]
   add [(0,1.1), (4,2.)] (3,2.33) -> 1.1*x^0 + 2.*x^4 + 2.33*x^3 -> 1.1*x^0 + 2.33*x^3 + 2.*x^4 -> [(0,1.1); (3,2.33); (4,2.)]
   add [(0,1.1), (3,2.)] (3,2.33) -> ... -> [(0,1.1); (3,4.33)]
*)

(* Signature : int*float list -> int*float -> int*float list *)
let add l m =
  let rec recu lis tup = let puiss, fac = tup in match lis with 
    |[] -> [tup]
    |(puissLis, facLis)::lis1 when puissLis = puiss -> (puiss, (fac +. facLis))::lis1
    |(puissLis, facLis)::lis1 when puissLis > puiss -> tup::lis
    |e::lis1 -> e::(recu lis1 tup)
    
(* Recursive auxilliary function with 4 cases. signature int*float list -> int*float list *)
  in match l with
  | [] -> [m]
  | l -> recu l m;;

(* Test *)
if ( (add [] (0,1.)) <> [(0,1.)] ) then
  print_endline "FAUX";
if ( (add [(1,1.)] (0,1.)) <> [(0,1.); (1,1.)] ) then
  print_endline "FAUX";
if ( (add [(0,1.);(2,1.)] (1,1.)) <> [(0,1.); (1,1.); (2,1.)] ) then
  print_endline "FAUX";;
