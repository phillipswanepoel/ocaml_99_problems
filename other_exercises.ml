let double x = 2 * x
let square x = x * x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square


(* hint: investigate square $ 2 + 2 vs. square 2 + 2. *)
let ( $ ) f x = f x

(*
What does the following operator do?
let ( @@ ) f g x = x |> g |> f
Hint: investigate String.length @@ string_of_int applied to 1, 10, 100, etc.
*)

let ( @@ ) f g x = x |> g |> f

(*
Generalize twice to a function repeat, such that repeat f n x applies f to x a total of n times. That is,

    repeat f 0 x yields x

    repeat f 1 x yields f x

    repeat f 2 x yields f (f x) (which is the same as twice f x)

    repeat f 3 x yields f (f (f x))
*)

let rec repeat f c x =
  if c > 0 then f (repeat f (c-1) x)
  else x

(*
  Use fold_left to write a function product_left that computes the product of a list of floats. 
  The product of the empty list is 1.0. Hint: recall how we implemented sum in just one line of code in lecture.

  Use fold_right to write a function product_right that computes the product of a list of floats. Same hint applies.
*)

let product_left lst = List.fold_left ( *. ) 1.0 lst

let product_right lst = List.fold_right ( *. ) lst 1.0

(*
  Write a function sum_cube_odd n that computes the sum of the cubes of all the odd numbers between 0 and n inclusive. 
  Do not write any new recursive functions. 
  Instead, use the functionals map, fold, and filter, and the ( -- ) operator (defined in the discussion of pipelining).
*)

let rec ( -- ) x y = 
  if y < x then []
  else x :: (x+1) -- y

let cube n = n*n*n
let sum lst = List.fold_left ( + ) 0 lst
let is_odd a = if a mod 2 != 0 then true else false
let sum_cube_odd n =
  0 -- n 
  |> List.filter is_odd 
  |> List.map cube 
  |> List.fold_left ( + ) 0

(*
 Consider writing a function exists: ('a -> bool) -> 'a list -> bool, such that exists p [a1; ...; an]
 returns whether at least one element of the list satisfies the predicate p. 
 That is, it evaluates the same as (p a1) || (p a2) || ... || (p an). 
 When applied to an empty list, it evaluates to false.

Write three solutions to this problem, as we did above:

    exists_rec, which must be a recursive function that does not use the List module,

    exists_fold, which uses either List.fold_left or List.fold_right,
    but not any other List module functions nor the rec keyword, and

    exists_lib, which uses any combination of List module functions other than fold_left or fold_right,
    and does not use the rec keyword.
*)


let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

let exists_fold p lst = List.fold_left (fun acc x -> acc || p x) false lst

let exists_lib p lst = 
  let any = List.map p lst |> List.filter (fun x -> x) |> List.length in
  if any > 0 then true else false

let exists_lib = List.exists

(*

Write a function which, given a list of numbers representing debits, deducts them from an account balance,
and finally returns the remaining amount in the balance.
Write three versions: fold_left, fold_right, and a direct recursive implementation.

*)

let acct_bal_left total lst =
  total - List.fold_left ( + ) total lst

let acct_bal_right total lst =
  total - List.fold_right ( + ) lst total

let acct_bal_rec total lst = 
  let rec aux acc = function
  | [] -> total - acc
  | h :: t -> aux (acc + h) t
in aux 0 lst

(*
Here is an uncurried version of List.nth:

let uncurried_nth (lst, n) = List.nth lst n

In a similar way, write uncurried versions of these library functions:
    List.append
    Char.compare
    Stdlib.max
*)

let uncurried_append (lst0, lst1) = List.append lst0 lst1
let uncurried_compare (c0, c1) = Char.compare c0 c1
let uncurried_max (a, b) = Stdlib.max a b

(*
Show how to replace any expression of the form List.map f (List.map g lst) 
with an equivalent expression that calls List.map only once.
*)

let ( @@ ) f g x = x |> g |> f
let list_composition2 f g lst = List.map (f @@ g) lst


(*
Write functions that perform the following computations. 
Each function that you write should use one of List.fold, List.map or List.filter. 
To choose which of those to use, think about what the computation is doing: combining, transforming, or filtering elements.

  Find those elements of a list of strings whose length is strictly greater than 3.

  Add 1.0 to every element of a list of floats.

  Given a list of strings strs and another string sep, produce the string that contains every element of strs separated by sep.
  For example, given inputs ["hi";"bye"] and ",", produce "hi,bye", 
  being sure not to produce an extra comma either at the beginning or end of the result string.
*)

let len_less_3 lst = List.filter (fun s -> String.length s < 3) lst
let add_1 lst = List.map (fun f -> f +. 1.0) lst

let join_with_sep strings sep = 
  match strings with
  | [] -> ""
  | h :: t ->
    List.fold_left (fun acc s -> acc ^ sep ^ s) h t

(*
Exercise: association list keys [★★★]

Recall that an association list is an implementation of a dictionary
in terms of a list of pairs, in which we treat the first component of 
each pair as a key and the second component as a value.

Write a function keys: ('a * 'b) list -> 'a list 
that returns a list of the unique keys in an association list. 
Since they must be unique, no value should appear more than once in the output list. 
The order of values output does not matter. How compact and efficient can you make your solution? 
Can you do it in one line and linearithmic space and time? Hint: List.sort_uniq.
*)
let keys lst = 
  lst 
  |> List.map (fun (x , y) -> x) 
  |> List.sort_uniq (Stdlib.compare)

let keys lst = 
  lst
  |> List.rev_map fst
  |> List.sort_uniq (Stdlib.compare)

(*
A valid matrix is an int list list that has 
at least one row, 
at least one column, 
and every column has the same number of rows. 
There are many values of type int list list that are invalid:
    []
    [[1; 2]; [3]]
Implement a function is_valid_matrix
*)
