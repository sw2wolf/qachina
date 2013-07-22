#load "unix.cma";; (* for sleep and gettimeofday; not needed for the signals stuff per se *)
 
let start = Unix.gettimeofday ();;
 
Sys.set_signal Sys.sigint
  (Sys.Signal_handle (fun _signum ->
                        Printf.printf "Ran for %f seconds.\n"
                          (Unix.gettimeofday () -. start);
                        exit 0));;
 
let rec loop n =
  Printf.printf "%d\n%!" n;
  Unix.sleep 1;
  loop (n + 1)
in
  loop 1;;
(**)
#load "nums.cma";;
open Num;;
 
for candidate = 2 to 1 lsl 19 do
  let sum = ref (num_of_int 1 // num_of_int candidate) in
  for factor = 2 to truncate (sqrt (float candidate)) do
    if candidate mod factor = 0 then
      sum := !sum +/ num_of_int 1 // num_of_int factor
                  +/ num_of_int 1 // num_of_int (candidate / factor)
  done;
  if is_integer_num !sum then
    Printf.printf "Sum of recipr. factors of %d = %d exactly %s\n%!"
        candidate (int_of_num !sum) (if int_of_num !sum = 1 then "perfect!" else "")
done;;

(*Delimited overloading can be used to make the arithmetic expressions more readable: *)

let () =
  for candidate = 2 to 1 lsl 19 do
    let sum = ref Num.(1 / of_int candidate) in
    for factor = 2 to truncate (sqrt (float candidate)) do
      if candidate mod factor = 0 then
        sum := Num.(!sum + 1 / of_int factor + of_int factor / of_int candidate)
    done;
    if Num.is_integer_num !sum then
      Printf.printf "Sum of recipr. factors of %d = %d exactly %s\n%!"
        candidate Num.(to_int !sum) (if Num.(!sum = 1) then "perfect!" else "")
  done
(**)
# #load "str.cma";;
# let replace str occ by =
    Str.global_replace (Str.regexp_string occ) by str
  ;;
val replace : string -> string -> string -> string = <fun>
# replace "The white dog let out a single, loud bark." "white" "black" ;;
- : string = "The black dog let out a single, loud bark."
(**)
# module IntSet = Set.Make(struct type t = int let compare = compare end);; (* Create a module for our type of set *)
# IntSet.empty;; (* Empty set. A set is an abstract type that will not display in the interpreter *)
- : IntSet.t = <abstr>
# IntSet.elements (IntSet.empty);; (* Get the previous set into a list *)
- : IntSet.elt list = []
# let from_list lst = List.fold_right IntSet.add lst IntSet.empty;; (* Convenience function for constructing a set from a list *)
val from_list : IntSet.elt list -> IntSet.t = <fun>
# let s1 = from_list [1;2;3;4;3];;
val s1 : IntSet.t = <abstr>
# IntSet.elements s1;;
- : IntSet.elt list = [1; 2; 3; 4]
# let s2 = from_list [3;4;5;6];;
val s2 : IntSet.t = <abstr>
# IntSet.elements s2;;
- : IntSet.elt list = [3; 4; 5; 6]
# IntSet.elements (IntSet.union s1 s2);; (* Union *)
- : IntSet.elt list = [1; 2; 3; 4; 5; 6]
# IntSet.elements (IntSet.inter s1 s2);; (* Intersection *)
- : IntSet.elt list = [3; 4]
# IntSet.elements (IntSet.diff s1 s2);; (* Difference *)
- : IntSet.elt list = [1; 2]
# IntSet.subset s1 s1;; (* Subset *)
- : bool = true
# IntSet.subset (from_list [3;1]) s1;;
- : bool = true
# IntSet.equal (from_list [3;2;4;1]) s1;; (* Equality *)
- : bool = true
# IntSet.equal s1 s2;;
- : bool = false
# IntSet.mem 2 s1;; (* Membership *)
- : bool = true
# IntSet.mem 10 s1;;
- : bool = false
# IntSet.cardinal s1;; (* Cardinality *)
- : int = 4
# IntSet.elements (IntSet.add 99 s1);; (* Create a new set by inserting *)
- : IntSet.elt list = [1; 2; 3; 4; 99]
# IntSet.elements (IntSet.remove 3 s1);; (* Create a new set by deleting *)
- : IntSet.elt list = [1; 2; 4]
(**)
