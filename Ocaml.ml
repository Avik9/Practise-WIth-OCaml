(* PART 1: RECURSION *)

(* 1.1.1: pow *)
let rec pow n x = 
  if n = 0 then 1 (* If the *)
  else x * pow (n - 1) x;;

(* 1.1.2: float_pow *)
let rec float_pow n x = 
  if n = 0 then 1.
  else x *. float_pow (n - 1) x;;

(* 1.2: rev *)
let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list;;

(* 1.3: compress *)
let rec compress = function
  | x :: (y :: _ as z) -> if x = y then compress z else x :: compress z
  | smaller -> smaller;;

(* 1.4: cluster *)
let rev_list l =
  let rec rev_com com = function
    | [] -> com
    | hd::tl -> rev_com (hd::com) tl
  in rev_com [] l

let cluster list =
  let rec aux not_modified com = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: not_modified) :: com
    | x :: (y :: _ as t) ->
      if x = y then aux (x :: not_modified) com t
      else aux [] ((x :: not_modified) :: com) t  in
  rev_list (aux [] [] list);;

(* 1.5: slice *)
let slice list i k = 
  let rec add n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: add (n-1) t
  in
  let rec remove n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else remove (n-1) t
  in
  add (k - i) (remove i list);;

(* Part 2: HIGHER-ORDER FUNCTIONS *)

(* 2.1: composition *)
let composition f g x = f (g x);;

(* 2.2: equiv_on *)
open List;;

let equiv_on fn gn l = 
  Array.for_all (fun x -> (fn x) == (gn x)) l

(* 2.3: pairwisefilter *)
let rec pairwisefilter pred l = 
  match l with
  | [] -> []
  | hd::snd::tl -> let new_tl = pairwisefilter pred tl in
           (pred hd snd) :: new_tl    
  | hd::[] -> [hd];;

(* 2.4: polynomial *)
let rec polynomial l = fun n ->
 match l with
 | [] -> 0
 | (c, p) :: tl -> let rest = polynomial tl in
     let x = (float_of_int n) ** (float_of_int p) |> int_of_float in
     (c * x) + (rest n);;

(* Part 3: DATA TYPES *)

(* 3.1: truth_table *)
type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec evaluate a value_of_a b value_of_b = function
  | Lit x -> if x = a then value_of_a
             else value_of_b
  | Not e -> not(evaluate a value_of_a b value_of_b e)
  | And(e1, e2) -> evaluate a value_of_a b value_of_b e1 && evaluate a value_of_a b value_of_b e2
  | Or(e1, e2) -> evaluate a value_of_a b value_of_b e1 || evaluate a value_of_a b value_of_b e2

let truth_table a b expression =
  [(true, true, evaluate a true b true expression);
   (true, false, evaluate a true b false expression);
   (false, true, evaluate a false b true expression);
   (false, false, evaluate a false b false expression)];

(* 3.2: binary_tree *)

type binary_tree = 
  | Empty
  | Node of int * binary_tree * binary_tree

(* 3.3: tree2str *)

let rec tree2str = function
    | Empty -> ""
    | Node(num, l, r) ->
       let data = string_of_int num in
       match l, r with
       | Empty, Empty -> data
       | _, _ -> data ^ "(" ^ (tree2str l)
                 ^ "," ^ (tree2str r) ^ ")";;