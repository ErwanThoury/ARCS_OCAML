(*
returns the concatenation of l1 and l2
    Look it up, we have already done this.
                                       Make sure you understand what is happening.
                                                                          You will need the same concept later on
                                     *)
let rec concatenate l1 l2 = match l1 with
  | [] -> l2
  | e::l -> e::(concatenate l l2);;
(*
Helper function:
Appends a list l1 n times to the original list lorig
signature:
int -> 'alpha list -> 'alpha list -> 'alpha list
Number of repetitions -> list to repeat and concat -> original list -> result

The main idea is to use n as a counter in a local recursive function
The local function only needs a counter c (int)
and the current list list_c. It has to return a list
Note that this is generic and not limited to chars
*)
let repeater n l1 lorig =
  let rec repeaterImpl_ c list_c = match c with
    | 0 -> [](*special case*)
    | c -> concatenate list_c (repeaterImpl_ (c-1) list_c)(*some recursive call using concatenate*)
  in match n with 
  | n when n < 0 -> failwith "error"(* Faulty call leading to error *) 
  | n when n >= 0 -> concatenate (repeaterImpl_ n l1) lorig;;(* Some actual call *)


(*Some tests*)
if (repeater 1 [1;2] [1;2;3]) <> [1;2;1;2;3] then
  print_endline "Failed 1 [1;2] [1;2;3]";;

if (repeater 2 [1;2] [1;2;3]) <> [1;2;1;2;1;2;3] then
  print_endline "Failed 2 [1;2] [1;2;3]";;

if (repeater 0 [1;2] [1;2;3]) <> [1;2;3] then
  print_endline "Failed 0 [1;2] [1;2;3]";;

(*
We have a list of char as for the python version.
So a block of digits representing a number followed by a block of chars
representing the substring to be repeated.
As we are in Ocaml we have to swap "iterative features" versus "recursive features"

The main idea is the following:
Define the helper functions
1) readInt l
signature: char list -> int*char list
Function: Takes a list that starts with a digit
"Consumes" the list until a non-digit char is
returns the tuple int computed * rest of the list (which in our case cannot be empty)
If you struggle with this, make a first version which only works for single digit integers
to pass the first tests
EX
['9'; 'a'; 'b'; 'c'; '4'; 'a'] -> '9', ['a'; 'b'; 'c'; '4'; 'a']

2) isNumeric c
signature: char -> bool
  Function: True if c is a numeric char

                         3) getSubString l
  signature: char list -> char list*char list
                            Function: Takes a list of chars that starts with a non-digit.
                                                                                     Read the list and construct the substring until the next digit is found OR until the list is EMPTY
    Return the substring * rest of list. rest of list is either empty or starts with digit char
  EX
  ['a'; 'b'; 'c'; '4'; 'a'] -> ['a'; 'b'; 'c']*['4'; 'a']
                                 Note: This is where you can reuse the concept of how concatenate works

    Use these helper functions in a local recursive function
to determine the number of times a substring has to be repeated and what that substring is
    before performing a recursive call.
                                    Be reminded that you can construct "local variables" like
let myint,mysubstringlist = readInt current_list in
    (*some other expression*)
*)


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let readInt l =
  let rec readIntImpl_ current_list n = match current_list with
    | e::l when e == '1' -> readIntImpl_ l ((1+n) * 10)
    | e::l when e == '2' -> readIntImpl_ l ((2+n) * 10)
    | e::l when e == '3' -> readIntImpl_ l ((3+n) * 10)
    | e::l when e == '4' -> readIntImpl_ l ((4+n) * 10)
    | e::l when e == '5' -> readIntImpl_ l ((5+n) * 10)
    | e::l when e == '6' -> readIntImpl_ l ((6+n) * 10)
    | e::l when e == '7' -> readIntImpl_ l ((7+n) * 10)
    | e::l when e == '8' -> readIntImpl_ l ((8+n) * 10)
    | e::l when e == '9' -> readIntImpl_ l ((9+n) * 10)
    | e::l when e == '0' -> readIntImpl_ l ((0+n) * 10)
    | e::l -> ((n/10), current_list) (* Some final case *)
    | [] -> failwith "error"
  in match l with
  | [] -> failwith "error" 
  | l -> readIntImpl_ l 0;;
      
let isNumeric c = 
  if '0' <= c && c <= '9' then true else false;;(* I will leave that to you *)

let getSubString l =
  let rec reverse l lr = match l with
    | [] -> lr 
    | e::l1 -> reverse l1 (e::lr) in  
  let rec getSubStringImpl_ l resteL= match l with
    | e::l1 when (isNumeric e) == true -> ((reverse  resteL []), l)(* final case 1 *)
    | [] -> ((reverse resteL []), [])
    |  e::l1 when (isNumeric e) == false -> getSubStringImpl_ l1 (e::resteL) 
    | _ -> [],[]
  in match l with
  | e::l1 when (isNumeric e) == true -> failwith "error" (* some error *)
  | l -> getSubStringImpl_ l [];; (* actual call *)

let decompress l =
  let rec decompressImpl_ current_list =
    let myInt, mySubstring = readInt current_list in let subUn, subDeux = getSubString mySubstring in
    match current_list with
    | l when subDeux = [] -> repeater myInt subUn [](* final case *)
    | l -> repeater myInt subUn (decompressImpl_ subDeux)   
    | [] -> failwith "error" 

  in match l with
  | [] -> [](* empty list handle *)
  | e::l1 when (isNumeric e) == true -> decompressImpl_ l(* Actual call *)
  | e::l1 when (isNumeric e) == false -> failwith "error";;(* some error *)


(*test*)
if (decompress (explode "1abc")) <> (explode "abc") then
  print_endline "Failed 1abc";;
if (decompress (explode "3abc")) <> (explode "abcabcabc") then
  print_endline "Failed 3abc";;
if (decompress (explode "3abc")) <> (repeater 3 (explode "abc") []) then
  print_endline "Failed 3abc";;
if (decompress (explode "3abc4edf")) <> (repeater 3 (explode "abc") (repeater 4 (explode "edf") [])) then
  print_endline "Failed 3abc4edf";;
if (decompress (explode "31abc94edf")) <> (repeater 31 (explode "abc") (repeater 94 (explode "edf") [])) then
  print_endline "Failed 31abc94edf";;




