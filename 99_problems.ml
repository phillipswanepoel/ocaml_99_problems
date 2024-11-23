let rec last lst =
  match lst with
  | [] -> None
  | [h] -> Some h
  | _ :: t -> last t
let rec last_two lst =
  match lst with
  | [] | [_] -> None
  | [y ; z] -> Some (y, z)
  | _ :: t -> last_two t

let rec nth_mine lst n =
  match lst with
  | [] -> None
  | d :: t -> if n = 0 then Some d else nth_mine t (n-1)

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n

(*these two are equivalent, function is short for (fun x -> x )*)
let len lst =
  let rec aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t
  in aux 0 lst

let len lst =
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | _ :: t -> aux (acc+1) t
  in aux 0 lst

let rev lst =
  let rec aux res = function
    | [] -> res
    | h :: t -> aux (h :: res) t
  in aux [] lst

let is_palindrome lst =
  lst = rev lst

(* Flatten list *)
type 'a node =
| One of 'a
| Many of 'a node list

(*  This variant (type constructor ) has two variants:
    1. One: a value of any type
    2. Many: a list of nodes (each element of list can be one or many) *)

let rec flatten list =
  let rec aux acc list =
    match list with
    | []     -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in (aux [] list) |> rev
(* flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
string list = ["a"; "b"; "c"; "d"; "e"] *)

(* elimate consecutive duplicates*)
(* compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
let rec compress list =
  let rec aux acc list =
    match list with
    | [] -> acc
    | h :: t ->
      match acc with
      | [] -> aux (h :: acc) t
      | y :: z -> if h = y then aux acc t else aux (h :: acc) t
    in (aux [] list) |> rev

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;

(*
(* Pack consecutive duplicates into sublists*)

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]

*)

let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (a :: current) acc t
       else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list);;

(*

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

*)
let encode list =
  let rec aux count = function
    | [] -> []
    | [x] -> (count+1, x) :: []
    | a :: (b :: _ as t) ->
      if a = b then aux (count+1) t
      else (count+1, a) :: aux 0 t
  in aux 0 list

type 'a rle =
| One of 'a
| Many of int * 'a

let r_encode list =
  let rec aux = function
    | [] -> []
    | (count, element) :: t ->
      if count = 1
      then One element :: aux t
      else Many (count, element)  :: aux t
  in aux (encode list)

(*

#  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

*)

let n_times_c n c =
  let rec aux acc n c =
    if n > 0 then aux (c :: acc) (n-1) c
    else acc
  in aux [] n c

let rec decode = function
  | [] -> []
  | One value :: t -> value :: decode t
  | Many (count, value) :: t -> (n_times_c value count) @ decode t

(*

# duplicate ["a"; "b"; "c"; "c"; "d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

*)

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t

(*

# drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

*)

let drop list n =
  let rec aux count = function
    | [] -> []
    | h :: t ->
      if count = 1 then aux n t
      else h :: aux (count - 1) t
  in aux n list

(*

# split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a"; "b"; "c"; "d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])

*)

(* imagine you could program inside the pattern match: | [n_times_c] or | [n * _ ]*)

let split list n =
  let rec aux count acc = function
  | [] -> (acc, [])
  | h :: t ->
    if count < n
    then aux (count+1) (h :: acc) t
    else (acc, t)
  in aux 0 [] list


(*

# slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]

*)

let slice list i j =
  let rec aux c = function
  | [] -> []
  | h :: t ->
    if (c < i)
    then aux (c+1) t
    else if (c >= i && c <= j)
    then h :: aux (c+1) t
    else []
  in aux 0 list

(* exercise solution:
let slice list i k =
   let rec take n = function
     | [] -> []
     | h :: t -> if n = 0 then [] else h :: take (n - 1) t
   in
   let rec drop n = function
     | [] -> []
     | h :: t as l -> if n = 0 then l else drop (n - 1) t
   in
   take (k - i + 1) (drop i list);;
*)

(*

  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
  - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

*)

let rotate list r =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n-1) t
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n-1) t
  in
  drop (r mod len list) list @ take (r mod len list) list

(*

  # remove_at 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "c"; "d"]

*)

let remove_at list at = 
  let rec aux count = function
  | [] -> []
  | h :: t -> 
    if count = at
    then aux (count+1) t
    else h :: aux (count+1) t
  in aux 0 list

(*

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]

*)

let insert_at element pos list =
  let rec aux count = function
  | [] -> if pos >= count then element :: [] else []
  | h :: t ->
    if count = pos
    then element :: h :: aux (count+1) t
    else h :: aux (count+1) t
  in aux 0 list
  
(*

# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]

*)
let range a b =
  let rec aux a b =
    if a > b then [] else a :: aux (a + 1) b
  in
    if a > b then List.rev (aux b a) else aux a b

(*
# rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
- : string list = ["e"; "c"; "g"]
*)

let rec rand_select lst n =
  let r = Random.int (len lst) in
  if n > 0
  then (nth lst r) :: rand_select (remove_at lst r) (n-1)
  else []

(*
Draw N different random numbers from the set 1..M.
# lotto_select 6 49;;
- : int list = [20; 28; 45; 16; 24; 38]
*)

let lotto_select n m =
  rand_select (range 1 m) n

(*
# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
- : string list = ["c"; "d"; "f"; "e"; "b"; "a"]
*)

let rec permutation lst =
  if len lst > 0 then
  let r = Random.int (len lst) in
    (nth lst r) :: permutation (remove_at lst r) 
  else []
   

   



