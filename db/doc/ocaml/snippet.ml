
(**)
(* define a type “person” that is a particular subset of
   a compound type called Record *)
type person = {mutable name:string; mutable age:int };;

let mj = {name="Mary Jane"; age=19 };;

(* extracting a field *)
print_string mj.name;                   (* prints “Mary Jane” *)

(* changing a field's value *)
mj.name <- "Mary Johnson";

print_string mj.name;                   (* prints “Mary Johnson” *)

(**)
print_string "α β λ ≤ ≥ ≠ ⊂ ℚ ℝ ℂ ∑ ↔ ⇔ ◀▶▲▼\n";;

(**)
(* Use FFI and build your own. *)

(* The most direct solution is to write your own FFI and call a memmove in C. To ensure that this is not misused for array types that may have references, the types should be fixed to the concrete types that this is allowed for. *)
CAMLprim value _array_blit64(value src, value dst,
                             value src_pos, value dst_pos,
                             value len)
{
  CAMLparam5(src, dst, src_pos, dst_pos, len);
  int isrc_pos = Int_val(src_pos);
  int idst_pos = Int_val(dst_pos);
  int ilen = Int_val(len);

  int idst_len = Wosize_val(dst);
  int isrc_len = Wosize_val(src);

  // ensure we are dealing with 64-bit values
  assert(sizeof(value) == 8);

  //ensure that we are within bounds
  assert(ilen >=0);
  assert(isrc_pos >= 0 && isrc_pos + ilen <= isrc_len);
  assert(idst_pos >= 0 && idst_pos + ilen <= idst_len);

  memmove(String_val(dst) + idst_pos * sizeof(value),
          String_val(src) + isrc_pos * sizeof(value),
          ilen * sizeof(value));

  CAMLreturn (Val_unit);
}

ml/mli file bindings:
external int_array_blit :
  src:int array -> dst:int array ->
  src_pos:int -> dst_pos:int ->
  len:int -> unit
    = "_array_blit64"

external float_array_blit :
  src:float array -> dst:float array ->
  src_pos:int -> dst_pos:int ->
  len:int -> unit
    = "_array_blit64"

CAMLprim value caml_rdtsc( )
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return Val_int( ((unsigned long long)lo) | ( ((unsigned long long)hi)<<32 ));
}

(**)
open Core.Std
open Async.Std
open Parallel.Std
 
let worker h =
  Pipe.iter_without_pushback (Hub.listen_simple h) ~f:(fun (id, `Ping) ->
    Hub.send h id `Pong;
  >>| fun () -> `Done
 
let main () =
  Parallel.spawn ~where:Parallel.random worker >>> fun (c, _res) ->
  let rec loop () =
    Channel.write c `Ping;
    Channel.read c >>> fun `Pong ->
    Clock.after (sec 1.) >>> loop
  in
  loop ();
  Clock.after (sec 60.) >>> fun () -> Shutdown.shutdown 0
 
let () =
  Parallel.init ~cluster:
    {Cluster.master_machine = Unix.gethostname ();
     worker_machines = ["host0"; "host1"]} ();
  main ();
  never_returns (Scheduler.go ())

(**)
(*********************************************************************
 * This program alters an .exe file to make it use the "windows subsystem"
 * instead of the "console subsystem".  In other words, when Windows runs
 * the program, it will not create a console for it.
 *)

exception Invalid_file_format

let input_word ic =
    let lo = input_byte ic in
    let hi = input_byte ic in
    (hi lsl 8) + lo

let find_pe_header ic =
    seek_in ic 0x3C;
    let peheader = input_word ic in
    seek_in ic peheader;
    if input_char ic <> 'P' then
        raise Invalid_file_format;
    if input_char ic <> 'E' then
        raise Invalid_file_format;
    peheader

let find_optional_header ic =
    let peheader = find_pe_header ic in
    let coffheader = peheader + 4 in
    seek_in ic (coffheader + 16);
    let optsize = input_word ic in
    if optsize < 96 then
        raise Invalid_file_format;
    let optheader = coffheader + 20 in
    seek_in ic optheader;
    let magic = input_word ic in
    if magic <> 0x010B && magic <> 0x020B then
        raise Invalid_file_format;
    optheader

let change ic oc =
    let optheader = find_optional_header ic in
    seek_out oc (optheader + 64);
    for i = 1 to 4 do
        output_byte oc 0
    done;
    output_byte oc 2

let main () =
    if Array.length Sys.argv <> 2 then
      begin
        print_endline "Alters a Win32 executable file to use the Windows subsystem.";
        print_endline ("Usage: mkwinapp <filename>");
        exit 1
      end;
    let filename = Sys.argv.(1) in
    let f = Unix.openfile filename [Unix.O_RDWR] 0 in
    let ic = Unix.in_channel_of_descr f and oc = Unix.out_channel_of_descr f in
    change ic oc

let _ = main ()

(**)
open Core.Std
open Core_extended.Std
open Async.Std
 
let exec ~prog ~args ?working_dir () = 
  Process.run ~prog ~args ?working_dir () 
  >>= function 
  | Error _ -> return None
  | Ok res -> 
    match String.strip res with 
    | "" -> return (Some [])
    | src -> return (Some (String.split ~on:'\n' src))
 
(**)
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
(* default values *)
let somebool = ref false
let somestr = ref ""
let someint = ref 0
 
let usage = "usage: " ^ Sys.argv.(0) ^ " [-b] [-s string] [-d int]"
 
let speclist = [
    ("-b", Arg.Unit   (fun () -> somebool := true), ": set somebool to true");
    ("-s", Arg.String (fun s -> somestr := s),      ": what follows -s sets some string");
    ("-d", Arg.Int    (fun d -> someint := d),      ": some int parameter");
  ]
 
let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
 
  Printf.printf " %b %d '%s'\n" !somebool !someint !somestr;

(**)
(*appop.ml*)
open Camlp4.PreCast.Syntax
EXTEND Gram
    expr: BEFORE "apply"
        [ "applOp" RIGHTA [ f = expr; "$"; x = expr -> <:expr<$f$ $x$>> ]];
END

(* The above code extends the default OCaml grammar Gram by adding a new rule under the expr entry. This rule extends the expr (expression) entry by inserting the rule at the supplied precedence level – before application; *)
(* is named applOp (an arbitrarily chosen name); *)
(* is right-associative (RIGHTA); *)
(* rewrites f $ x as f x for all expressions (exprs) f, x. *)

(* To make use of this extension it must first be compiled – here with the help of ocamlfind, *)
(* $ ocamlfind ocamlc -linkpkg -syntax camlp4o \ *)
(*     -package camlp4.extend -package camlp4.quotations -c appop.ml *)

(* and passed to the -pp switch of ocamlc during batch compilation of dollar.ml, *)
(* $ ocamlc -pp "camlp4o ./appop.cmo" dollar.ml -o dollar *)
(* $ ./dollar *)

(* To see what’s going on under the hood, we can pre-process dollar.ml and output the (source) result to stdout, *)
(* $ camlp4o ./appop.cmo dollar.ml *)

let sum = List.fold_left ( + ) 0
let res xs = string_of_int (succ (sum xs))
let () = print_endline (res [ 1; 2; 3 ])

(* Finally, the extension can also be used interactively from the toplevel, *)
# #use "topfind";;
# #camlp4o;;
/opt/godi/lib/ocaml/std-lib/dynlink.cma: loaded
/opt/godi/lib/ocaml/std-lib/camlp4: added to search path
/opt/godi/lib/ocaml/std-lib/camlp4/camlp4o.cma: loaded
    Camlp4 Parsing version 3.12.1
# #load "appop.cmo";;
# let sum = List.fold_left (+) 0;;
val sum : int list -> int = <fun>
# string_of_int $ succ $ sum [1; 2; 3];;
- : string = "7"

(**)
* Simple OCaml Web Server *) 

open Unix 

(* Port to listen on *) 
let port = 9090 

let listen_sock = socket PF_INET SOCK_STREAM 0 

(* Sends the gives string to the given socket *) 
(* Unix.file_descr -> string -> unit *) 
let send_string sock str = 
  let len = String.length str in 
  let _ = send sock str 0 len [] in 
  ()

(* Main loop *) 
let rec do_listen () = 
  let (client_sock, _) = accept listen_sock in 
  send_string client_sock "HTTP/1.1 200/OK\nContent-type: text/html\n\n"; 
  send_string client_sock "<h1>Hello World</h1>"; 
  close client_sock; 
  do_listen () 

(* Listen on port *) 
let _ = 
  bind listen_sock (ADDR_INET (inet_addr_of_string "0.0.0.0", port)); 
  listen listen_sock 8; 
  do_listen ()

(**)
type expr =
  | EAdd of expr * expr
  | EApply of expr * expr
  | EEqual of expr * expr
  | EIf of expr * expr * expr
  | EInt of int
  | ELetRec of string * string * expr * expr
  | EMul of expr * expr
  | EVar of string

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * (string * value) list * expr;;

(* We use the Genlex module to create a lexer: *)
#load "camlp4o.cma";;
        Camlp4 Parsing version 3.09.2

open Genlex;;
let keywords =
   ["("; ")"; "+"; "-"; "=";
    "if"; "then"; "else";
    "let"; "rec"; "in"];;

(* val lex : char Stream.t -> Genlex.token Stream.t = <fun> *)
let lex stream =
   let rec aux = parser
      | [< 'Int n when n<0; t=aux >] -> [< 'Kwd "-"; 'Int(-n); t >]
      | [< 'h; t=aux >] -> [< 'h; t >]
      | [< >] -> [< >] in
    aux(make_lexer keywords stream);;

(* Note that we use a function to map negative integer tokens back into a minus sign followed by a positive integer. *)

(* Parser *)

(* Expressions are parsed using four mutually recursive functions for each of the four precedences: *)
let rec parse_atom = parser
  | [< 'Int n >] -> EInt n
  | [< 'Ident v >] -> EVar v
  | [< 'Kwd "("; e=parse_expr; 'Kwd ")" >] -> e

and parse_apply = parser
  | [< e1=parse_atom; stream >] ->
      (parser
         | [< e2=parse_atom >] -> EApply(e1, e2)
         | [< e2=parse_apply >] -> begin match e2 with
             | EApply(e2, e3) -> EApply(EApply(e1, e2), e3)
             | e2 -> EApply(e1, e2)
             end
         | [< >] -> e1) stream

and parse_arith = parser
  | [< e1=parse_apply; stream >] ->
      (parser
         | [< 'Kwd "+"; e2=parse_arith >] -> EAdd(e1, e2)
         | [< 'Kwd "-"; e2=parse_arith >] -> EAdd(e1, EMul(EInt(-1), e2))
         | [< >] -> e1) stream

and parse_expr : 'a Stream.t -> expr = parser
  | [< e1=parse_arith; stream >] ->
      (parser
         | [< 'Kwd "="; e2=parse_expr >] -> EEqual(e1, e2)
         | [< >] -> e1) stream
  | [< 'Kwd "if"; p=parse_expr; 'Kwd "then"; t=parse_expr;
         'Kwd "else"; f=parse_expr >] ->
        EIf(p, t, f)
  | [< 'Kwd "let"; 'Kwd "rec"; 'Ident f; 'Ident x; 'Kwd "="; body=parse_expr;
         'Kwd "in"; rest=parse_expr >] ->
        ELetRec(f, x, body, rest);;
(* val parse_atom : Genlex.token Stream.t -> expr = <fun> *)
(* val parse_apply : Genlex.token Stream.t -> expr = <fun> *)
(* val parse_arith : Genlex.token Stream.t -> expr = <fun> *)
(* val parse_expr : Genlex.token Stream.t -> expr = <fun> *)

(* Parsed expressions can then be given to the evaluator. *)
(* Evaluator *)

(* The evaluator translates expressions into values in the context of variable bindings (a string -> value association list called vars in this case): *)
let int = function VInt n -> n | _ -> invalid_arg "int";;
let bool = function VBool b -> b | _ -> invalid_arg "bool";;
let rec eval vars = function
  | EApply(func, arg) -> begin match eval vars func, eval vars arg with
  | VClosure(var, vars, body), arg -> eval ((var, arg) :: vars) body
  | _ -> invalid_arg "Attempt to apply a non-function value"
    end
  | EAdd(e1, e2) -> VInt (int(eval vars e1) + int(eval vars e2))
  | EMul(e1, e2) -> VInt (int(eval vars e1) * int(eval vars e2))
  | EEqual(e1, e2) -> VBool (eval vars e1 = eval vars e2)
  | EIf(p, t, f) -> eval vars (if bool (eval vars p) then t else f)
  | EInt i -> VInt i
  | ELetRec(var, arg, body, rest) ->
        let rec vars = (var, VClosure(arg, vars, body)) :: vars in
        eval vars rest
  | EVar s -> List.assoc s vars;;
(* val eval : (string * value) list -> expr -> value = <fun> *)

(* The treatment of closures in the target languages is interesting. Function applications cause the body of the applied closure to be evaluated in the context of the environment which it captured and its argument. Function definitions bind a variable to a new closure that captures the current environment (the current variable bindings). *)
(* Example *)

(* We can see the interpreter in action by feeding it an example program. The following program computes the 30th Fibonacci number using only the constructs provided by our language: *)
let program = "let rec fib n = if n=0 then 0 else if n=1 then 1 else fib(n - 1) + fib(n - 2) in fib 30";;

(* Applying the lexer and parser gives the program represented as an abstract syntax tree: *)
let ast = parse_expr(lex(Stream.of_string program));;
val ast : expr =
  ELetRec ("fib", "n",
   EIf (EEqual (EVar "n", EInt 0), EInt 0,
    EIf (EEqual (EVar "n", EInt 1), EInt 1,
     EAdd (EApply (EVar "fib", EAdd (EVar "n", EMul (EInt (-1), EInt 1))),
      EApply (EVar "fib", EAdd (EVar "n", EMul (EInt (-1), EInt 2)))))),
   EApply (EVar "fib", EInt 30))

(* Evaluating the abstract syntax tree gives the result of the program as a value of the type value: *)
# eval [] ast;;
- : value = VInt 832040

(* The result can be verified by evaluating the same program in the OCaml top-level: *)
# let rec fib n =
    if n=0 then 0 else
      if n=1 then 1 else
        fib(n - 1) + fib(n - 2) in
  fib 30;;
- : int = 832040
