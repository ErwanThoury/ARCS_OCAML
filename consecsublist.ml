(* Attention
If I give a prototype / head of a function, define an error case or a signature, this HAS to be respected.
If it is not respected I will not give any partial credit!
*)

(* Part 1 : consecutive sublist*)
(* Find the index in a list of integers at which the longest array
of consecutive integers starts. Ex. [1; 2; 3] is a consecutive subarray, [1; 2; 4] is not
     Your function has to return the starting index AND the length of the subarray
      If there are multiple such indices, there is no restriction onto which of them
has to be returned
    Example 1:
         [0; 2; 1; 2; 3; 6; 9; 0]
Your function has to return the pair (2, 3)
    Example 2:
                       [1; 1; 1; 1] your function has to return (0,1) or (1,1) or (2,1) or (3,1)
*)

open Char;;
(* helper *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) ( ((code s.[i]) - 48) :: l) in
  exp (String.length s - 1) []
(* Helper function: Get the length of the consecutive subarray starting at the head of the given list *)

(* Signature: int list -> int *)
let conArrLen l =
  let rec recu li comp prec = match li with
    | e::li1 when e = (prec + 1) -> recu li1 (comp + 1) e
    | [] -> comp
    | e::li1 when e <> (prec + 1) -> comp
(* Recursive auxiliary function with signature int list -> int -> int -> int
   Hint: matching with 3 cases on first argument *)
  in match l with
  | [] -> failwith "error"
  | e::l1 -> recu l1 1 e;;
               

(* Test *)
if conArrLen [1; 2] <> 2 then
  print_endline "Faux";
if conArrLen [1; 0] <> 1 then
  print_endline "Faux";
if conArrLen [11; 12; 13; 14; 0] <> 4 then
  print_endline "Faux";;

(* Signature: int list -> int*int *)
let maxConArrLen l =
  let rec recu li iS iEC lon = match li with
    | e::li1 when (conArrLen li) > lon -> recu li1 iEC (iEC + 1) (conArrLen li) 
    | [] -> (iS, lon)
    | e::li1 -> recu li1 iS (iEC + 1) lon
   (* Recursive auxiliary function
   POSSIBLE signature (may differ) : int list -> int -> int -> int -> int*int *)
  in match l with
  | [] -> failwith "error"(* Error on empty list *)
  | e::l1 -> recu l 0 0 0;;(* Actual call *)

(* Test *)
if maxConArrLen [1; 2] <> (0,2) then
  print_endline "Faux";
if (maxConArrLen [1; 0] <> (0,1) && maxConArrLen [1; 0] <> (1,1)) then
  print_endline "Faux";
if maxConArrLen [11; 12; 13; 14; 0] <> (0,4) then
  print_endline "Faux";
if maxConArrLen [1; 11; 12; 13; 14; 0] <> (1,4) then
  print_endline "Faux";;