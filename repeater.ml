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