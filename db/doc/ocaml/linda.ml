(*
 *
 * Copyright (c) 2009-, 
 *  Shuchang Zhou    <zhoushuchang@ict.ac.cn>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
open List
open Printf

(** general *)
let ( *@) f g = fun x -> f (g x)
let (@$) f x = f x
let (|>) x f = f x

let addRefList x rl = rl := x:: !rl
let between a x b = a<=x && x<=b
let cons x y = x:: y
let flip f x y = f y x
let const a b = a
let funPower n f x =
  let rec work i x =
    if i<=0 then x else work (i-1) (f x) in
  assert (n >=0);
  work n x

let funPowers n f x =
  let rec work i acc x =
    if i<0 then List.rev acc  else work (i-1) (x::acc) (f x) in
  assert (n >=0);
  work n [] x

let id x = x
let fraction p n = int_of_float (p *. float n)
let pair a b = (a, b)
let triple a b c = a,b,c
let tmap f (a, b) = (f a, f b)
let tmap3 f (a, b, c) = (f a, f b, f c)
let tmap4 f (a, b, c, d) = (f a, f b, f c, f d)
let tmap5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)
let tmap6 f (a, b, c, d, e, f') = (f a, f b, f c, f d, f e, f f')
let tmap7 f (a, b, c, d, e, f', g) = (f a, f b, f c, f d, f e, f f', f g)

let isTrue = id
let isFalse = not
let int_of_bool (b: bool) : int = Obj.magic b
let bool_of_int i = i!= 0
let string_of_char c = String.make 1 c
let char_of_string s = s.[0]
let question b e e2 = if b then e else e2
let even x = x land 1 = 0
let odd x = x land 1 = 1
let round f = int_of_float (floor (f +.0.5))

module Logic = struct
  let both p p' = fun x -> p x && p' x
  let either p p' = fun x -> p x || p' x
  let neither p p' = both (not*@p) (not*@p') 
end

module type PARAM = sig
  type t
  val p : t
end

module type PARAM2 = sig
  type t
  val p : t
  type t2
  val p2 : t2
end

module type PARAM3 = sig
  type t
  val p : t
  type t2
  val p2 : t2
  type t3
  val p3 : t3
end

module type ORD =sig
  type t
  val eq : t -> t -> bool
  val ge : t -> t -> bool
  val le : t -> t -> bool
end
module Ord (O: ORD) = struct
  let eq = O.eq
  let ge = O.ge
  let le = O.le
  let neq a a' = not (eq a a')
  let gt a a' = ge a a' && neq a a'
  let lt a a' = gt a a' && neq a a'
  let compare a a' =
    if eq a a' then 0 else
      if lt a a' then -1 else
        1
  let max a a' = if gt a a' then a else a'
  let min a a' = if gt a a' then a' else a
end

module Comparison (O:Set.OrderedType) = struct
  module O2 = Ord(struct
    type t = O.t
      let eq t t' = O.compare t t' =0
      let ge t t' = O.compare t t' >=0
      let le t t' = O.compare t t' <=0
  end)
  include O2
end

module type ORDLTEQ = sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
end  
   
module OrdLtEq(O:ORDLTEQ) = struct
  module O2 = Ord(struct
    type t = O.t
    let eq = O.eq
    let le t t' = O.eq t t' || O.lt t t'
    let ge t t' = not (O.lt t t')
  end)
  include O2 
end

module type COLLECTION = sig
    type 'a t
  val null : 'a t -> bool
  val head : 'a t -> 'a
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val zipWith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val iter : ('a -> unit) -> 'a t -> unit
    val length : 'a t -> int
    val nth : 'a t -> int -> 'a
    val sortNew : ('a -> 'a -> int) -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
end

module HomPair = struct
  type 'a t = 'a * 'a
  let null (_,_) = false
  let head = fst
  let to_list (a,b) = [a;b]
  let foldl f i (a,b) = f (f i a) b
  let foldr f i (a,b) = f a (f b i)
  let zipWith f (a,b) (a',b') = (f a a',f b b')
  let map = tmap
  let iter f (a,b) = f a;f b
  let length (a,b) = 2
  let nth (a,b) = function
    | 0 -> a
    | 1 -> b
    | _ -> failwith "nth"
  let sortNew cmp (a,b) = match cmp a b with
    | 1 -> (b,a) 
    | _ -> (a,b)
  let rev (a,b) = (b,a)
end

module HomTriple = struct
  type 'a t = 'a * 'a * 'a
  let to_list (a,b,c) = [a;b;c]
  let foldl f i (a,b,c) = f (f (f i a) b) c
  let foldr f i (a,b,c) = f c (f a (f b i))
  let zipWith f (a,b,c) (a',b',c') = (f a a',f b b',f c c')
  let map = tmap3
  let iter f (a,b,c) = f a;f b;f c
  let length (a,b,c) = 3
  let nth (a,b,c) = function
    | 0 -> a
    | 1 -> b
    | 2 -> c
    | _ -> failwith "nth"
  let sortNew cmp (a,b,c) = match List.sort cmp [a;b;c] with
    | [a';b';c'] -> (a',b',c')
    | _ -> assert false
  let rev (a,b,c) = (c,b,a)
    let hd (a,_,_) = a
    let last (_,_,a) = a
    let middle (_,a,_) = a
end

let time f =
  let t1 = Sys.time () in
  f ();
  Sys.time () -. t1
(* printf "\ntime: %f\n" @$ Sys.time() -. t1 *)
let timen n f =
  let ot = Sys.time () in
  for i = 0 to n - 1 do
    f ()
  done;
  Sys.time () -. ot

module ExpCounter = struct
    type t = int
    let create () = 
        let i = ref 0 in
        let i' = ref 0 in
        (fun () ->
          incr i';
          if !i' >= 1 lsl !i then begin
            i' := 0;
            incr i
           end 
          ), (fun () -> !i,!i')
end

let array n f =
  let a = Array.init n (fun i -> lazy (f i)) in 
  fun i -> Lazy.force a.(i)

(* print_intlist @$ map (fun _ -> Random.int 3) (range 0 10);              *)
(* print_intlist @$ map (fun _ -> randInt 3) (range 0 10);                 *)

(** pretty print *)
module type SHOW = sig
  type t
  val show : t -> string
end
module Show_list(S: SHOW) = struct
  type t = S.t list
  let show ?(brackets ="[","]") ?(sep =";") l =
    fst brackets ^ ((String.concat sep) *@ (map S.show)) l ^ snd brackets
end
module Show_pair(S: SHOW)(S2: SHOW) = struct
  type t = S.t * S2.t
  let show ?(brackets ="(",")") ?(sep =",") (a, b) =
    fst brackets ^ S.show a ^ sep ^ S2.show b ^ snd brackets
end
module Show_triple(S: SHOW)(S2: SHOW)(S3: SHOW) = struct
  type t = S.t * S2.t * S3.t
  let show ?(brackets ="(",")") ?(sep =",") (a, b, c) =
    fst brackets ^ S.show a ^ sep ^ S2.show b ^ sep ^ S3.show c ^ snd brackets
end
module Show_array(S: SHOW) = struct
  type t = S.t array
  module SS = Show_list(S)
  let show ?(brackets ="[|","|]") ?(sep =";") l =
    SS.show ~brackets ~sep @$ Array.to_list l
end
module Show_matrix(S: SHOW) = struct
  module SS = Show_array(S)
  let show ?(brackets ="[|","|]") ?(sep =";") a =
    fst brackets ^ ((String.concat sep) *@ (Array.to_list *@Array.map SS.show)) a ^ snd brackets
end
module Show_listlist(S: SHOW) = struct
  module SS = Show_matrix(S)
  let show ?(brackets ="[","]") ?(sep =";") l =
    SS.show ~brackets ~sep @$ Array.of_list (map Array.of_list l)
end

let show_list ?(brackets="[","]") ?(sep =";") show l =
  fst brackets ^ ((String.concat sep) *@ (map show)) l ^ snd brackets
let show_pair ?(brackets="(",")") ?(sep =",") showA showB (a, b) = 
  fst brackets ^ showA a ^ "," ^ showB b ^ snd brackets
let show_triple ?(brackets="(",")") ?(sep =",") showA showB showC (a, b, c) = 
  fst brackets ^ showA a ^ "," ^ showB b ^ "," ^ showC c ^ snd brackets
let show_stringpair ?(brackets="(",")") ?(sep =",") p =
  show_pair ~brackets ~sep id id p

let show_stringtriple ?(brackets="(",")") ?(sep =",") p =
  show_triple ~brackets ~sep id id id p  
  
let show_stringlist ?(brackets="[","]") ?(sep =";") = show_list ~sep id

let show_array ?(brackets="[|","|]") ?(sep =";") show a =
  fst brackets ^ ((String.concat sep) *@ (map show)) (Array.to_list a) ^ snd brackets

let show_stringarray ?(brackets="[|","|]") ?(sep =";") = show_array ~sep id
let show_matrix ?(brackets="[|","|]") ?(sep =";") show ma =
  String.concat "\n" @$ Array.to_list @$ Array.map (fun a -> show_array ~brackets ~sep show a) ma
let show_listlist ?(brackets="[","]") ?(sep =";") show ll =
  String.concat "\n" @$ Array.to_list @$ Array.map (fun a -> show_array ~brackets ~sep show a) @$
    Array.of_list (List.map (Array.of_list) ll)
 
(** numeric *)
let pi = 3.141592653589793
let angleToRadian angle = float angle /.180. *. pi
let radianToAngle ra = int_of_float @$ ra /. pi *. 180.

let neg a = - a
let ratio a b = float a /. float b
let closer a b c =
  abs_float (a -.b) < abs_float (b -.c)
let fabs = abs_float

let powBy one mult a b =
  let rec pow' b =
    if b = 0 then one else
      let half = pow' (b / 2) in
      mult (if even b then one else a) (mult half half) in
  assert (b >= 0);
  pow' b

let pow a b = powBy 1 ( * ) a b
let powf a b = powBy 1. ( *.) a b
let rem a b = (a + b) mod b

let logp p n =
  assert (n > 0);
  let rec work a n =
    if n > 1 then work (a + 1) (n / p) else if n = 1 then a else a - 1 in
  work 0 n
let log2 = logp 2
let rec until p f x = if p x then x else until p f (f x)
let asTypeOf : 'a -> 'a -> 'a = const
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let curry3 f x y z = f (x,y,z)
let uncurry3 f (x,y,z) = f x y z
let curry4 f x y z w = f (x,y,z,w)
let uncurry4 f (x,y,z,w) = f x y z w
let curry5 f x y z w v = f (x,y,z,w,v)
let uncurry5 f (x,y,z,w,v) = f x y z w v

let signum x =
  if x > 0 then 1 else
  if x = 0 then 0 else
    - 1

let binaryToGray l =
  let rec work = function
    | [] -> []
    | [x] -> [x]
    | x:: x':: xs -> (if x'= 1 then 1 - x else x) :: work (x':: xs) in
  rev @$ work (rev l)

let grayToBinary l =
  let rec work s = function
    | [] -> []
    | x:: xs ->
        let s' = (s + x) mod 2 in
        (if s'= 1 then 1 - x else x) :: work s' xs in
  rev @$ work (List.fold_left (+) 0 l) @$ rev l

let toBinary n =
  let rec work n = if n <= 1 then [n] else n mod 2 :: work (n / 2) in
  assert (n >= 0);
  rev @$ work n

let ofBinary l =
  let rec work acc = function
    | [] -> acc
    | x:: xs -> work (acc * 2 + x) xs in
  work 0 l

let ( ***) f g = fun (x, y) -> (f x, g y)
let (&&&) f g = fun x -> (f x, g x)
let first f (x, y) = (f x, y)
let second g (x, y) = (x, g y)
(* let finally fend f x= try f x with _ -> fend () *)

module Either = struct
  type ('a,'b) t = Left of 'a | Right of 'b
  
  let either f g = function
    | Left a -> f a
    | Right b -> g b
  let rec lefts = function
    | [] -> []
    | Left a:: xs -> a:: lefts xs
    | _:: xs -> lefts xs
  let rec rights = function
    | [] -> []
    | Right b:: xs -> b:: rights xs
    | _:: xs -> rights xs
  let partitionEithers l =
    let rec loop s s2 = function
      | [] -> List.rev s, List.rev s2
      | Left a:: xs -> loop (a:: s) s2 xs
      | Right b:: xs -> loop s (b:: s2) xs in
    loop [] [] l
  
end
module EitherMonad = struct
  open Either
  let fail x = Left x
  let bind m k = match m with
    | Left a -> Left a
    | Right b -> k b
  let return x = Right x
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap f = function
    | Left a -> Left a
    | Right b -> Right (f b)
  let mzero = Left ""
  let mplus x y = match x, y with
    | Left _, b -> b
    | a, _ -> a
  let msum l = List.fold_right mplus l mzero
  let guard flg = if flg then return () else mzero
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m1 m2 =
    m1 >>= fun x1 ->
        m2 >>= fun x2 ->
            return @$ f x1 x2
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return @$ f x
            
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])            
end
(** Maybe *)
let maybefind f h x =
  try Some (f h x)
  with Not_found -> None

let maybe x f = function
    None -> x
  | Some y -> f y
let isSome = function
    Some _ -> true
  | _ -> false
let isNone x = x == None
let fromSome = function
    Some x -> x
  | _ -> failwith "fromSome"
let fromMaybe x = function
    Some y -> y
  | None -> x
let listToMaybe = function
    [] -> None
  | x:: _ -> Some x
let maybeToList = function
    None -> []
  | Some x -> [x]
let catMaybes l = List.map fromSome (List.filter isSome l)
let mapMaybe f l = catMaybes (List.map f l)
let equalOptionsBy eq a b = match a, b with
  | None, None -> true
  | Some x, Some x' -> eq x x'
  | _ -> false

let divMaybe a b = if a mod b = 0 then Some (a / b) else None

module MaybeMonad = struct
  let fail _ = None 
  let bind m k = match m with
      None -> None
    | Some x -> k x
  let return x = Some x
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap f = function
    | None -> None
    | Some x -> Some (f x)
  let mzero = None
  let mplus x y = match x, y with
    | None, b -> b
    | a, _ -> a
  (* let msum l = List.fold_right mplus l mzero *)
  let guard flg = if flg then return () else mzero
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
  let liftM3 f m m' m'' =
    m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return @$ f x
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])
  let replicateM n m =
    let replicate n e =
        let rec loop stk i = if i = 0 then stk else loop (e:: stk) (i - 1) in
        loop [] n in 
    sequence (replicate n m)
  
  let msum l =
    let rec loop = function
      | [] -> mzero
      | Some x:: xs -> return x
      | None:: xs -> loop xs in
    loop l
  let msumRmap l f =
    let rec loop = function
      | [] -> mzero
      | x:: xs -> let x' = f x in if isSome x' then x' else loop xs in
    loop l

end

module ExtChar = struct
  include Char
  let isAscii c = c < '\x80'
  let isLatin1 c = c <= '\xff'
  let isBlank c = c == ' ' || c == '\t' || c == '\n'
  let isLower c = 'a' <= c && c <= 'z'
  let isUpper c = 'A' <= c && c <= 'Z'
  let toLower c =
    if isUpper c then   
        char_of_int (int_of_char c - int_of_char 'A' + int_of_char 'a')
    else c
  let toUpper c =
    if isLower c then   
        char_of_int (int_of_char c - int_of_char 'a' + int_of_char 'A')
    else c
  let isDigit c = '0' <= c && c <= '9'
  let isOctDigit c = '0' <= c && c <= '7'
  let isHexDigit c = between 'a' c 'f' || between 'A' c 'F' || isDigit c
  let isAlpha = Logic.either isLower isUpper
  let isAlphaNum = Logic.either isAlpha isDigit
  let digitToInt c = 
    if isDigit c then int_of_char c - int_of_char '0' else
      if between 'a' c 'f' then int_of_char c - int_of_char 'a'+10 else
        if between 'A' c 'F' then int_of_char c - int_of_char 'A'+10 else
          failwith "digitToInt"
end

(** Built on Hashtbl, with unique binding for each key*)
module HashMap = struct
  open Hashtbl
  type ('a,'b) t = ('a,'b) Hashtbl.t
  let create = create
  let clear = clear
  let copy = copy
  let get = find
  let mem = mem
  let remove = remove
  let set = replace
  let iter = iter
  let riter a f = iter f a
  let fold = fold
  let length = length
  let zipWith f a a' =
    let a'' = copy a' in
    iter (fun k v -> set a'' k (maybe v (f v) (maybefind find a'' k))) a;
    a''
  let to_list a =
    let r = ref [] in
    riter a (fun k v -> r:= (k, v):: !r);
    !r
  let of_list l : ('a,'b) t =
    let r = create 3 in
    List.iter (fun (k, v) -> set r k v) l;
    r
  let compose h h' =
    let h'' = create (length h) in
    iter (fun k v -> set h'' k (get h' v)) h;
    h''
  let keys h =
    let l = ref [] in
    iter (fun k _ -> addRefList k l) h;
    !l
  let values h =
    let h' = Hashtbl.create 3 in
    iter (fun _ v -> Hashtbl.add h' v ()) h;
    let l = ref [] in
    Hashtbl.iter (fun k _ -> addRefList k l) h';
    !l
        
  let show show_key show_val a =
    show_list (show_pair show_key show_val) (to_list a)
  let rev h =
      let h' = create 3 in
      Hashtbl.iter (fun k v -> set h' v k) h;
      h'
  module Make (H: HashedType) = struct
    module M = Hashtbl.Make(H)
    include M
    let create = create
    let clear = clear
    let copy = copy
    let get = find
    let mem = mem
    let remove = remove
    let set = replace
    let iter = iter
    let riter a f = iter f a
    let fold = fold
    let length = length
    let zipWith f a a' =
      let a'' = copy a' in
      iter (fun k v -> set a'' k (maybe v (f v) (maybefind find a'' k))) a;
      a''
    let to_list a =
      let r = ref [] in
      riter a (fun k v -> r:= (k, v):: !r);
      !r
    (* let of_list l : ('a,'b) t = let r = create 3 in List.iter (fun (k,v) -> *)
    (* set r k v) l; r                                                         *)
    let compose = compose
    let show show_key show_val a =
      show_list (show_pair show_key show_val) (to_list a)
    let rev = rev
  end
end

module Subarray = struct
  type 'a subarray = int * 'a array
  type 'a t = 'a subarray
  let make i a = (i, a)
  let of_array a = make 0 a
  let hd ((i, a):'a subarray) = a.(i)
  let tl ((i, a):'a subarray) : 'a subarray = (i + 1, a)
  let null (i, a) = i >= Array.length a
  let empty = (0,[||])
  let length ((i, a):'a t) = Array.length a - i
  let rec foldl f i sa =
    if null sa then i else foldl f (f i (hd sa)) (tl sa)
  let rec foldr f i sa =
    if null sa then i else f (hd sa) (foldr f i (tl sa))
  let of_list l : 'a t = make 0 (Array.of_list l)
  let to_list ((i, a):'a t) =
    let acc = ref [] in
    for j = i to Array.length a - 1 do
      acc := a.(j) :: !acc
    done;
    List.rev !acc
  let select j ((i, a):'a t) = (i + j, a)
  let rev ((i, a):'a t) =
    let acc = ref [] in
    for j = i to Array.length a - 1 do
      acc := a.(j) :: !acc
    done;
    of_list !acc
  
  let get (i, a) j = a.(i + j)
  let set (i, a) j v = a.(i + j) <- v
  let zipWith f (sa:'a t) (sa':'a t) : 'a t =
    let n = min (length sa) (length sa') in
    make 0 @$ Array.init n (fun i -> f (get sa i) (get sa' i))
  let full (i, a) = i = 0
  let notFull (sa:'a t) = not @$ full sa
  let cons e (i, a) : 'a t =
    a.(i - 1) <- e;
    (i - 1, a)
  
  let map f ((i, a):'a t) :'b t = make i (Array.init (Array.length a) (fun i -> f a.(i)))
  let iter f ((j, a):'a t) =
    for i = j to Array.length a - 1 do f a.(i) done
  let zipWithAssign f ((i, a):'a t) ((i', a'):'b t) =
    for j = i to Array.length a - 1 do
      a.(j) <- f a.(j) a'.(j - i + i')
    done
  (* let cons e (i,a) = let rec scanl f i sa = if null sa then i else let  *)
  (* foldl f a = let sa = of_array a in foldl                              *)
  let all f (sa:'a t) = foldl (&&) true @$ map f sa
  let drop n ((i, a):'a t) = make (i + n) a
  let rec tile n sa =
    if null sa then [] else sa :: tile n (drop n sa)
  
  (** generalized from semi tensor product proposed by Cheng Daizhan*)
  let semiZipWith f sa sa' =
    if length sa mod length sa' = 0 then
      let n = length sa' in
      List.map (fun sa -> zipWith f sa sa') (tile n sa)
    else failwith "semiZipWith"
  (* else if length sa' mod length sa = 0 then *)
end
module Submatrix = struct
  open Array
  open Subarray
  type 'a t = 'a subarray subarray
  let of_matrix (ma:'a array array) :'a t =
    of_array @$ (Array.map of_array ma)
  let map f sma = map (map f) sma
  let select i j (sma:'a t) =
    select i @$ Subarray.map (select j) sma
  let null (sma:'a t) = null sma || all null sma
  let get sma i j = get (get sma i) j
  let set (k, sa) i j v = set sa.(i + k) j v
  let hd (sma:'a t) = get sma 0 0
end
module ListMonad = struct
  open List
  let bind l f = concat(map f l)
  let return x = [x]
  let (>>=) = bind
  let (>>) l l2 = bind l (fun _ -> l2)
  let join m = m >>= id
  let fmap = map
  let mzero = []
  let mplus = (@)
  let msum l = fold_right mplus l mzero
  let guard flg = if flg then return () else mzero
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
  let liftM3 f m m' m'' =
    m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
  let ap fm m =
    fm >>= fun f ->
        m >>= fun x ->
            return @$ f x
  
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    fold_right k ms (return [])
(*  let replicateM n m = sequence (replicate n m)*)
end
module ExtList = struct
  open List
  exception ListFound
    type 'a t = 'a list
  let to_list = id
  let head = hd
  let null l = l ==[]
  let rmap l f = map f l
  let foldl = fold_left
  let foldr f a l = fold_right f l a
  let foldl1 f l = foldl f (hd l) (tl l)
  let foldr1 f l = foldr f (hd l) (tl l)
  let foreach l f = iter f l
  let foreach2 l l2 f = iter2 f l l2
  let mapi f l =
    let rec work i acc = function
      | [] -> rev acc
      | x:: xs -> work (i + 1) (f i x::acc) xs in
    work 0 [] l
  let iteri f l =
    ignore @$ mapi f l
    let sortNew = sort
  let liftCompare cmp l l' =
    let rec work l l' = match l,l' with
      | [],[] -> 0
      | _,[] -> 1
      | [],_ -> -1
      | x::xs,y::ys ->
        let r = cmp x y in 
        if r =0 then work xs ys else r in
    work l l'  
  let liftEqual eq l l' =
    let rec work l l' = match l,l' with
      | [],[] -> true
      | _,[]
      | [],_ -> false
      | x::xs,y::ys -> eq x y && work xs ys in
    work l l'
  let rec maybeDeleteBy eq x = function
  | [] -> None
  | y:: ys -> if eq x y then Some ys else maybeDeleteBy eq x ys

    let rec allEqualsToBy eq x = function
      | [] -> true
      | y:: ys -> eq x y && allEqualsToBy eq x ys
    let allEqualsBy eq = function
      | [] -> true
      | x:: xs -> allEqualsToBy eq x xs
    let allEqualsTo x l = allEqualsToBy (=) x l
    let allEquals l = allEqualsBy (=) l

  let isPalindrome l = l = List.rev l
  let replicate n e =
    let rec loop stk i = if i = 0 then stk else loop (e:: stk) (i - 1) in
    loop [] n
  open MaybeMonad
  (** no key assigned two values *)
  let rec toMap =
    let rec loop stk k v = function
      | [] -> return (rev stk)
      | (k', v'):: xs when k = k'->
          if v = v' then loop stk k v xs else None
      | x:: xs -> loop (x:: stk) k v xs in
    function
    | [] -> return []
    | (k, v):: xs ->
        loop [] k v xs >>= fun l ->
            toMap l >>= fun l' ->
                return @$ (k, v):: l'
  
  let findFirst f l =
    let rec loop = function
      | [] -> None
      | x:: xs -> begin match f x with
            | Some y -> Some y
            | None -> loop xs end in
    loop l
  (* pick the one satisfying [f] and return the rest. The [rest] may still *)
  (* contain the picked element if there are duplicates in [l]             *)
  let pick f l =
    let rec loop rest l = match l with
      | [] -> None
      | x:: xs -> begin match f x with
            | Some y -> Some (y, rev rest@xs)
            | None -> loop (x:: rest) xs
          end in
    loop [] l
  
  (* pick the n-th element, return it and the rest *)
  let pick2 n l = 
    let rec work i pre l =
      if i = 0 then hd l, (List.rev pre)@(tl l) else
        work (i - 1) (hd l:: pre) (tl l) in
    assert (n >= 0);
    work n [] l
  open ListMonad
    let pairWith f l l' =
        l >>= fun v ->
            l' >>= fun v' ->
                return @$ f v v'
    
    let rec combinations = function
        | [] -> [[]]
        | x::xs ->
        combinations xs >>= fun l ->
          mplus (return l) (return (cons x l))
    
    let combine n l = combinations (replicate n l)
    let picks l =
        let rec work pre acc = function
            | [] -> acc
            | x::xs -> work (x::pre) ((pre,x,xs)::acc) xs in
        work [] [] l
    let rec select n l =
        if n=0 then [[],l] else
            picks l >>= fun (pre,x,post) ->
                select (n-1) post >>= fun (l',rest) ->
                    return (x::l',pre@rest)
    
  let selections sizes l =
    assert (length l >= fold_left (+) 0 sizes);
    let rec work acc l = function
      | [] -> [List.rev acc]
      | x ::xs ->
        select x l >>= fun (l',l'') ->
          work (l'::acc) l'' xs in
    work [] l sizes 
  let pickNth n l =
    let rec work acc i = function
      | [] -> None
      | x :: xs -> 
        if i =n then Some (x,rev acc @ xs) else
          work (x::acc) (succ i) xs in
    work [] 0 l
  let random l = 
    try Some (nth l (Random.int (length l))) with Failure _ -> None
  let pickRandom l =
    pickNth (Random.int (length l)) l 
    
  let assocBy eq key l = findFirst (fun (k, v) -> if eq key k then Some v else None) l
  let equalBy eq a b =
    let rec loop a b = match a, b with
      | [],[] -> true
      | [], _ -> false
      | _,[] -> false
      | x:: xs, y:: ys -> eq x y && loop xs ys in
    loop a b
  let commonBy eq l l' =
    let rec work acc l l' = match l,l' with
      | _,[]
      | [],_ -> rev acc,(l,l')
      | x::xs,y::ys -> 
        if not (eq x y) then rev acc,(l,l') else
          work (x::acc) xs ys in
    work [] l l'
  let common l l' = commonBy (=) l l'
  (** Haskellish utility functions *)
  
  let any p l =
    try
      iter (fun x -> if p x then raise ListFound) l; false
    with ListFound -> true
  let all p = not *@ any (not *@p)
  let rec trimLast = function
    | [] -> failwith "extList.trimLast"
    | [x] -> []
    | x:: xs -> x:: trimLast xs
  let rec last = function
    | [] -> failwith "extList.last"
    | [x] -> x
    | x:: xs -> last xs
  let rec inits l = if null l then [] else l :: inits (trimLast l)
  let rec tails l = if null l then [] else l :: tails (tl l)
  let rec inits' l = if null l then [[]] else l :: inits'(trimLast l)
  let rec tails' l = if null l then [[]] else l :: tails' (tl l)
  let rec intersperse sep l = match l with
      [] -> []
    | [x] -> [x]
    | x:: xs -> x:: sep:: intersperse sep xs
  let intercalate xs xss = concat (intersperse xs xss)
  
  let sortBy = fast_sort
  let elemBy eq x l = any (eq x) l
  let memBy = elemBy
  
  let lookup x l =
    try Some (assoc x l)
    with Not_found -> None
  let rec permutations =
    let rec insert x = function
      | [] -> [[x]]
      | (y:: ys) as l -> (x:: l) :: map (cons y) (insert x ys) in
    function
    | [] -> [[]]
    | x:: xs -> concat (map (insert x) (permutations xs))
  
  let rec nonEmptySubsequences =
    let f x ys r = ys :: (x:: ys) :: r in
    function
      [] -> []
    | x:: xs -> [x] :: fold_right (f x) (nonEmptySubsequences xs) []
  let subsequences xs = [] :: nonEmptySubsequences xs
  
  let concatMap f = concat *@ map f
  let rec scanl f i = function
      [] -> [i]
    | x:: xs -> i:: scanl f (f i x) xs
  let scanl1 f l = scanl f (hd l) (tl l)
(*  let scanr1 f l = rev (scanl1 f (rev l))*)
  (* let mapAccumL f acc l = *)
  
  let rec takeNIterate n f i = if n <= 0 then [] else i:: takeNIterate (n - 1) f (f i)
  (* let rec repeat n f i = if n<=0 then i else repeat (n-1) f (f i) *)
  let rec insertBy cmp x = function
      [] -> [x]
    | y:: ys' as ys -> if cmp x y > 0 then y:: insertBy cmp x ys' else x:: ys
  let insert e ls = insertBy compare e ls

  let rec unfoldr f b = match f b with
      None -> []
    | Some (x, b') -> x:: unfoldr f b'
  
  let takeDrop n l=
    let rec work i acc = function
      | [] -> List.rev acc,[]
      | x::xs -> if i<n then work (i+1) (x::acc) xs else List.rev acc,x::xs in
    work 0 [] l
  let take n l = fst @$ takeDrop n l
  let drop n l = snd @$ takeDrop n l
  
  let lastn n = rev *@take n *@rev
  
  let startsWith hay ~needle =
    take (length needle) hay = needle
  let endsWith hay ~needle =
    startsWith (rev hay) (rev needle)
  
  let rotate n l =
    let n' = n mod (length l) in
    let l',l'' = takeDrop n' l in
    l'' @ l'
  
  let rec deltaBy f = function
    | x:: x2:: xs -> f x2 x :: deltaBy f (x2:: xs)
    | _ -> []
  (** tile the list into blocks of at most [n] long *)
  let tile n l =
    let rec loop stk = function
      | [] -> rev stk
      | l -> loop (take n l:: stk) (drop n l) in
    loop [] l
  let span f l =
        let rec work acc = function
            | [] -> List.rev acc, []
            | x ::xs -> if f x then work (x::acc) xs else List.rev acc, x::xs in
        work [] l
  let break f l = span (not*@f) l
    let takeWhile f l = fst (span f l)
    let dropWhile f l = snd (span f l)
  let switch f l =
    let l',l'' = break f l in l'' @ l'
  let addPack pack l =
    if null pack then l else List.rev pack::l  
  let packBy startNew l =
        let rec work acc pack = function
            | [] -> List.rev (addPack pack acc) 
            | x:: xs ->
                if startNew x then work (addPack pack acc) [x] xs
                else work acc (x::pack) xs
        in work [] [] l
    let switchBy switch l =
      let rec work acc pack = function
        | [] -> List.rev (addPack pack acc)
        | x:: xs ->
            if switch x then
              work (addPack (x:: pack) acc) [] xs
            else work acc (x:: pack) xs
      in work [] [] l
    let isolate start finish l =
        filter (not*@null) @$
            concatMap (fun l -> switchBy finish l) @$ packBy start l    
    let groupBy eq l =
        let rec work acc = function
            | [] -> List.rev acc
            | x::xs ->
                let l',l'' = span (eq x) xs in 
                work ((x::l')::acc) l'' in
        work [] l
  let group l = groupBy (=) l

  let transpose l =
    let rec loop acc l =
        if all null l then rev acc else 
        loop (map hd l:: acc) (map tl l) in
    loop [] l
  (* if null l then [] else let ls = tile (length (hd l)) (concat l) in    *)
  (* map (Array.to_list) @$ Array.to_list (ExtArray.transpose              *)
  (* (Array.of_list (map Array.of_list ls)))                               *)
  
  let rec stripPrefixBy eq xs ys = match xs, ys with
      [], _ -> Some ys
    | x:: xs, y:: ys when eq x y -> stripPrefixBy eq xs ys
    | _, _ -> None
  let findIndex f l =
    let rec loop i = function
        [] -> None
      | x:: xs -> if f x then Some i else loop (i + 1) xs in
    loop 0 l
  let findIndices f l =
    let rec loop i stk = function
        [] -> rev stk
      | x:: xs -> loop (i + 1) ((if f x then [i] else [])@stk) xs in
    loop 0 [] l
  let rec isPrefixOfBy eq l l2 = match l, l2 with
      [], _ -> true
    | _,[] -> false
    | x:: xs, y:: ys -> eq x y && isPrefixOfBy eq xs ys
  let isPrefixOf l l2 = isPrefixOfBy (=) l l2
  let isSuffixOfBy eq l l2 = isPrefixOfBy eq (rev l) (rev l2)
  let isSuffixOf l l2 = isSuffixOfBy (=) l l2
  let rec deleteBy eq x = function
      [] -> []
    | y:: ys -> (if eq x y then [] else [y]) @ deleteBy eq x ys
  let delete x l = deleteBy (=) x l
  let rec nubBy eq = function
      [] -> []
    | x:: xs -> x:: nubBy eq (deleteBy eq x xs)
  let nub l = nubBy (=) l
  let rec hasNoDupesBy eq = function
    | [] -> true
    | x:: xs -> not (elemBy eq x xs) && hasNoDupesBy eq xs
  
  (** [uniq] only removes consecutive duplicates*)
  let rec uniqBy eq = function
    | [] -> []
    | x:: xs -> x:: uniqBy eq (dropWhile (eq x) xs)
  let uniq l = uniqBy (=) l
  let unionBy eq xs ys = xs @ fold_left (flip (deleteBy eq)) (nubBy eq ys) xs
  let union l l2 = unionBy (=) l l2
  let intersectBy eq l l2 = concat @$ rmap l2 @$ fun i ->
        if any (eq i) l then [i] else []
  let intersect l l2 = intersectBy (=) l l2
  let maximumBy cmp l = fold_left (fun a x -> if cmp a x < 0 then x else a) (hd l) (tl l)
  let maximum l = maximumBy compare l
  let minimumBy cmp l = maximumBy (fun x y -> - cmp x y) l
  let minimum l = minimumBy compare l
  let sortByMap cmp f l =
    map snd @$ sort (fun a a' -> cmp (fst a) (fst a')) (map (fun e -> (f e, e)) l)
  let maximumByMap cmp f l =
    snd @$ maximumBy (fun (v,_) (v',_) -> cmp v v') (map (fun e -> (f e, e)) l)
  let minimumByMap cmp f l =
    snd @$ minimumBy (fun (v,_) (v',_) -> cmp v v') (map (fun e -> (f e, e)) l)
  let isSortedBy cmp l = 
    let rec work = function
      | [_]
      | [] -> true
      | x::x'::xs -> cmp x x' <=0 && work (x'::xs) in
    work l
  let rec mergeSortedBy ?(noDup=true) cmp l l' =
        let rec work acc l l' = 
        match l, l' with
    | [] ,[] -> rev acc
    | l,[] -> rev acc @ l
    | [],l' -> rev acc @ l'
    | x:: xs, y:: ys ->
            let r = cmp x y in
            if noDup then
                if r=0 then work (x::acc) xs ys else
                    if r<0 then work (x::acc) xs l' else
                        work (y::acc) l ys 
            else
        if r<=0 then work (x::acc) xs l' else work (y::acc) l ys in
        work [] l l' 
  
  (** no failing List.split *)
  let unzip l =
    let rec loop a b = function [] -> List.rev a, List.rev b
      | (x, y):: xs -> loop (x:: a) (y:: b) xs in
    loop [] [] l
  
  let rec zipWith f l l2 = match l, l2 with
    | [], _
    | _,[] -> []
    | x:: xs, y:: ys -> f x y:: zipWith f xs ys
  
  (** no failing List.combine *)
  let zip l l2 = zipWith (fun a b -> (a, b)) l l2
      
  (* type system = module Sublist = struct open List type 'a t = (int*'a)  *)
  (* list (* translate from equation system *) type of_system sys end      *)
  (* (*module Matrix = struct *) (* type 'a t = 'a list list*) (* let        *)
  (* of_matrix m = *) (*end *) *)

  let splitBy f l = 
    let pack r = if null r then [] else [r] in
    let rec work acc = function 
      | [] -> pack (rev acc)
      | x::xs -> if f x then pack (rev acc) @ work [] xs else work (x::acc) xs in
    work [] l 
  
  (** produces the list [a..b-1] *)
  let range a b =
    if a>=b then [] else
    Array.to_list @$ Array.init (b - a) (fun i -> a + i)
    let init n f =
        Array.to_list @$ Array.init n f
  let range2 a b inc =
    let ar = Array.make ((b - a) / inc + int_of_bool ((b - a) mod inc <> 0)) 0 in
    let v = ref a in
    for i = 0 to Array.length ar - 1 do
      ar.(i) <- !v;
      v := !v + inc;
    done;
    Array.to_list ar
  let exprange a b e =
    let rec work acc a =
      if a >= b then List.rev acc else work (a:: acc) (a * e) in
    assert (a <> 0 && e > 1);
    work [] a
  
  let exprangef a b e =
    let b' = float b in
    let rec work acc a =
      if a >= b' then List.rev acc else work (int_of_float a:: acc) (a *.e) in
    assert (a <> 0 && fabs e > 1.);
    uniq @$ work [] (float a)

  let pairTable l =
    map (fun e -> map (pair e) l) l 

  let rec pairs = function
    | [] -> []
    | x::xs ->
      map (pair x) xs @ pairs xs

    let shrink f l = zipWith f l (tl l)
    let classifyBy eq l =
      let rec work acc = function
        | [] -> List.rev acc
        | x::xs -> work ((x::filter (eq x) xs)::acc) xs in
      work [] l
                
    type 'a people =
    | Vip of 'a * 'a people
    | Crowd of 'a list

let venturiBy ?(noDup=true) cmp l =
    let of_list = function
        | [] -> Crowd []
        | x:: xs -> Vip(x, Crowd xs) in
    let rec to_list = function
        | Vip(x, xs) -> x:: to_list xs
        | Crowd l -> l
    in
    let merge l l' = mergeSortedBy ~noDup cmp l l' in
    let rec merge' p p' = match p, p' with
        | Crowd [], Vip(y, yt) -> Vip(y, yt)
        | Vip(x, xt), ys -> Vip(x, merge' xt ys)
        | Crowd xs, Crowd ys -> Crowd (merge xs ys)
        | Crowd(x:: xt), Vip(y, yt) ->
            if noDup then
                let r = cmp x y in
                if r=0 then Vip(x,merge' (Crowd xt) yt) else
                    if r<0 then Vip(x,merge'(Crowd xt) p') else
                        Vip(y,merge' p yt)
            else
                if cmp x y <= 0 then Vip(x, merge' (Crowd xt) p') else Vip(y, merge' p yt) in
    to_list @$ foldl1 merge' (map of_list l)
    open Array
    let longestCommonSubsequenceBy eq zero len l l' =
      let n = List.length l in
      let al = of_list l in
      let maxBy f e e' = if compare (f e) (f e') <=0 then e' else e in
      let aux (i,e) (i',l) = i+i',e::l in
      let rec work a a' = function
        | [] -> List.tl @$ List.rev @$ snd @$ maximumByMap compare fst (to_list a)
        | x::xs ->
          iteri (fun i _ ->
            a'.(i) <- (if eq al.(i) x then aux (len al.(i),al.(i)) else id) @$
            (if i>=1 then maxBy fst a'.(i-1) @$ maxBy fst a.(i-1) a.(i) else a.(i))
             ) a';
          work a' a xs in
      work (init n (fun _ -> 0,[zero])) (init n (fun _ -> 0,[zero])) l'

  include List
end

module AssocList = struct
  open ExtList
  let assoc = assoc
  let mem k l = try ignore (assoc k l);true with Not_found -> false
  let zipWith f l l' =
    (map (fun (k,v) -> k,if mem k l' then f v (assoc k l') else v) l) @ filter (not*@flip mem l*@fst) l'
    let elements assocList = 
      let l = ref [] in
      List.iter (fun (a,b) -> addRefList a l;addRefList b l) assocList;
      !l    
  end

module ExtArray = struct
  open Array
  open MaybeMonad
  exception ArrayFound
  type 'a t = 'a array
  let init_matrix m n f = init m (fun i -> init n (fun j -> f i j))
  let foreach l f = iter f l
  let foreachi l f = iteri f l
  let null l = l == [||]
  let hd l = l.(0)
  let head = hd
  let rmap l f = map f l
  let foldl = fold_left
  let foldr f a l = fold_right f l a
  let foldl1 f a =
    let n = length a in
    let it = ref a.(0) in
    for i = 1 to n - 1 do
      it := f !it a.(i)
    done;
    !it
  let foldr1 f a =
    let n = length a in
    let it = ref a.(n - 1) in
    for i = n - 2 downto 0 do
      it := f a.(i) !it
    done;
    !it
  let iter2 f l l2 =
    let n = max (length l) (length l2) in
    for i = 0 to n - 1 do
      f l.(i) l2.(i)
    done
  let foreach l f = iter f l
  let foreach2 l l2 f = iter2 f l l2
  let sortNew cmp a = 
        let a' = copy a in
        sort cmp a';
        a'
    let nth a n = a.(n)
  let medianBy cmp a =
    (sortNew cmp a).(length a/2) 
  let memBy eq e a =
  let rec work i =
    i >= length a || (not (eq e a.(i)) && work (succ i)) in
  not (work 0)
  let mem e a = memBy (=) e a
  let memq e a = memBy (==) e a
    
  (** produces the array [a..b-1] *)
  let range a b =
    Array.init (b - a) (fun i -> a + i)
  let range2 a b inc =
    let ar = Array.make ((b - a) / inc + int_of_bool ((b - a) mod inc <> 0)) 0 in
    let v = ref a in
    for i = 0 to Array.length ar - 1 do
      ar.(i) <- !v;
      v := !v + inc;
    done;
    ar
  
  let allEqualsTo x l =
    try foreach l (fun e -> if e <> x then raise ArrayFound); true
    with ArrayFound -> false
  let allEquals a = null a || allEqualsTo (hd a) a
  
  let findFirstAux f l =
    let n = length l in
    let rec loop i =
      if i = n then None else
        let r = f l.(i) in
        if isSome r then Some (fromSome r, i) else
          loop (i + 1) in
    loop 0
  
  let findFirst f l = findFirstAux f l >>= fun(r, _) -> return r
  let findIndex f l = findFirstAux (fun x -> if f x then Some () else None) l >>= fun(_, i) -> return i
  
  let filterAux f l =
    let n = length l in
    let rec loop stk i =
      if i >= n then List.rev stk else
        loop ((if f l.(i) then [(l.(i), i)] else [])@stk) (i + 1) in
    loop [] 0
  let filter f l = of_list @$ List.map fst @$ filterAux f l
  let filterIndices f l = List.map snd @$ filterAux f l
  
  let equalArrayBy eq a b =
    let na = length a in
    let nb = length b in
    na = nb &&
    let rec loop i =
      if i = na then true else
        eq a.(i) b.(i) && loop (i + 1) in
    loop 0
  
  let any p l =
    try
      iter (fun x -> if p x then raise ArrayFound) l; false
    with ArrayFound -> true
  let all p = not *@ any (not *@p)
  let last l = l.(length l - 1)
  let intersperse sep l =
    let n = length l in
    if n <= 1 then l else
      init (2 * n - 1) (fun i -> if even i then l.(i / 2) else sep)
  (* let intercalate *)
  let sortBy = fast_sort
  let elemBy eq x l = any (eq x) l
  let rec scanlRegion f i l s e =
    let cur = ref i in
    for i = s to e do
      cur := f !cur l.(i);
      l.(i) <- !cur
    done;
    l
  let scanl f i l = scanlRegion f i l 0 (length l - 1)
  let scanl1 f l = scanlRegion f (hd l) l 1 (length l - 1)
  let rec takeNIterate n f i =
    let cur = ref i in
    init n (fun i -> let v = !cur in if i < n - 1 then cur := f !cur ; v)
  let replicate n e = make n e
  (* let unfoldr f b = of_list @$ ExtList.unfoldr f b *)
  
  (* fresh array *) 
  let takeDrop n a =
    if n >= length a then copy a,[||] else
      if n<=0 then [||],copy a else
        sub a 0 n,sub a n (length a-n)

  let rec take n l = sub l 0 (min n (length l))
  let rec drop n l =
    let n' = length l - n in
    if n'< 0 then [||] else sub l n n'

    let span f a =
        let i = ref 0 in
        match findFirst (fun e -> if not (f e) then Some !i else begin incr i;None end) a with
            | Some i -> take i a,drop i a
            | None -> copy a,[||]

    let break f l = span (not*@f) l
    let takeWhile f l = fst (span f l)
    let dropWhile f l = snd (span f l)
    
  let lastn n l = drop (length l - n) l
  let pad zero n t = 
        init n (fun i -> if i < (length t) then t.(i) else zero)
  let rec deltaBy f l = init (length l - 1) (fun i -> l.(i + 1) - l.(i))
  (** tile the array into blocks of at most [n] long *)

  let tile n a =
    let rec work acc a =
      let a' = take n a in
      if a' = [||] then List.rev acc else work (a'::acc) (drop n a) in
    of_list @$ work [] a 
  
  let transpose a =
    if a ==[||] then [||] else
      let m = length a in
      let n = length a.(0) in
      let a' = init n (fun i -> init m (fun j -> a.(j).(i))) in
      a'
  
  let maximumBy cmp l = foldl (fun a b -> if cmp a b > 0 then a else b) (hd l) l
  let maximum l = maximumBy compare l
  let minimumBy cmp l = maximumBy (fun x y -> - cmp x y) l
  let minimum l = minimumBy compare l
  let sortByMap cmp f l =
    let l' = (map (fun e -> (f e, e)) l) in
    sort (fun a a' -> cmp (fst a) (fst a')) l';
    map snd @$ l'
  let maximumByMap cmp f l =
    snd @$ maximumBy (fun (v,_) (v',_) -> cmp v v') (map (fun e -> (f e, e)) l)
  let minimumByMap cmp f l =
    snd @$ minimumBy (fun (v,_) (v',_) -> cmp v v') (map (fun e -> (f e, e)) l)
  let isSortedBy cmp a =
    let rec work i =
      i > length a-2 || cmp a.(i) a.(i+1) <=0 && work (i+1) in
    work 0
  let mergeSortedBy cmp a a' =
    let n, n' = length a, length a' in
    if n = 0 then copy a' else
    if n'= 0 then copy a else
      let a'' = make (n + n') a.(0) in
      let rec work i i' =
        if i >= n then blit a' i' a'' (i + i') (n'- i') else
        if i'>= n' then blit a i a'' (i + i') (n - i) else
        if cmp a.(i) a'.(i') <= 0 then begin
          a''.(i + i') <- a.(i);
          work (i + 1) i'
        end else begin
          a''.(i + i') <- a'.(i');
          work i (i'+ 1)
        end in
      work 0 0;
      a''
  
  let zipWith f a a' =
    let n = min (length a) (length a') in
    init n (fun i -> f a.(i) a'.(i))
  let zip a a' = zipWith (fun a b -> (a, b)) a a'
  let unzip a = init (length a) (fun i -> fst a.(i)), init (length a) (fun i -> snd a.(i))
    let swap a i j =
        let t = a.(i) in
        a.(i) <- a.(j);
        a.(j) <- t
  let rev a' =
    let a = copy a' in
    let n = length a in
    for i =0 to n/2-1 do
      swap a i (n-1-i)
    done;
    a
  module Matrix = struct
  type 'a t = 'a array array 
  let map f t = map (map f) t
  let mapij f t =
    mapi (fun i -> mapi (fun j e -> f i j e)) t
  let dims t = 
    let m = length t in
    m,if m=0 then 0 else length t.(0)
  let fold f r t =
    foldr (fun a r -> foldr (fun a r -> f a r) r a) r t
  let zipWith f ma ma' =
    zipWith (zipWith f) ma ma'
  let init m n f =
    init m (fun i -> init n (fun j -> f i j)) 
  end
    include Array
end

module HashSet = struct
  let hash = Hashtbl.hash
  type 'a t =
    { mutable size: int;                        (* number of elements *)
      mutable data: 'a bucketlist array } (* the buckets *)
  
  and 'a bucketlist =
      Empty
    | Cons of 'a * 'a bucketlist
  
  let create initial_size =
    let s = min (max 1 initial_size) Sys.max_array_length in
    { size = 0; data = Array.make s Empty }
  
  let clear h =
    for i = 0 to Array.length h.data - 1 do
      h.data.(i) <- Empty
    done;
    h.size <- 0
  
  let copy h =
    { size = h.size;
      data = Array.copy h.data }
  
  let length h = h.size
  
  let resize hashfun tbl =
    let odata = tbl.data in
    let osize = Array.length odata in
    let nsize = min (2 * osize + 1) Sys.max_array_length in
    if nsize <> osize then begin
      let ndata = Array.create nsize Empty in
      let rec insert_bucket = function
          Empty -> ()
        | Cons(key, rest) ->
            insert_bucket rest; (* preserve original order of elements *)
            let nidx = (hashfun key) mod nsize in
            ndata.(nidx) <- Cons(key, ndata.(nidx)) in
      for i = 0 to osize - 1 do
        insert_bucket odata.(i)
      done;
      tbl.data <- ndata;
    end
  
  let add h key =
    let i = (hash key) mod (Array.length h.data) in
    let bucket = Cons(key, h.data.(i)) in
    h.data.(i) <- bucket;
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h
  
  let remove h key =
    let rec remove_bucket = function
        Empty ->
          Empty
      | Cons(k, next) ->
          if compare k key = 0
          then begin h.size <- pred h.size; next end
          else Cons(k, remove_bucket next) in
    let i = (hash key) mod (Array.length h.data) in
    h.data.(i) <- remove_bucket h.data.(i)
  
  let mem h key =
    let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(k, rest) ->
          compare k key = 0 || mem_in_bucket rest in
    mem_in_bucket h.data.((hash key) mod (Array.length h.data))
  
  let iter f h =
    let rec do_bucket = function
        Empty ->
          ()
      | Cons(k, rest) ->
          f k; do_bucket rest in
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done
  
  let fold f h init =
    let rec do_bucket b accu =
      match b with
        Empty ->
          accu
      | Cons(k, rest) ->
          do_bucket rest (f k accu) in
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    !accu
  let any f h =
    let rec do_bucket b =
      match b with
        Empty ->
          false
      | Cons(k, rest) ->
                f k || do_bucket rest in
    let d = h.data in
        ExtArray.any do_bucket d
    let all f h = any (not*@f) h
  let to_list h =
    let l = ref [] in
    iter (fun k -> addRefList k l) h;
    !l
  let of_list l =
    let h = create 3 in
    List.iter (fun k -> add h k) l;
    h
    let includes h h' =
        all (fun e -> mem h e) h'
    let equals h h' = includes h h' && includes h' h 
  module type S =
  sig
    type key
    type t
    val create: int -> t
    val clear: t -> unit
    val copy: t -> t
    (* val add: 'a t -> key -> 'a -> unit *)
    val add: t -> key -> unit
    val remove: t -> key -> unit
    (* val find: 'a t -> key -> 'a val find_all: 'a t -> key -> 'a list    *)
    (* val replace : 'a t -> key -> 'a -> unit                             *)
    val mem : t -> key -> bool
    val iter: (key -> unit) -> t -> unit
    val fold: (key -> 'b -> 'b) -> t -> 'b -> 'b
    val length: t -> int
    val to_list: t -> key list
    val of_list: key list -> t
        val any : (key -> bool) -> t -> bool
        val all : (key -> bool) -> t -> bool
        val includes : t -> t -> bool
        val equals : t -> t -> bool
  end
  module Make(H: Hashtbl.HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    (* type 'a hashtbl = (key, 'a) t type 'a t = 'a hashtbl type 'a        *)
    (* hashset = 'a t type 'a hashset = 'a t type 'a t = 'a hashset type t *)
    (* = H.t t                                                             *)
    type hashset = H.t t
    type t = hashset
    let create = create
    let clear = clear
    let copy = copy
    
    let safehash key = (H.hash key) land max_int
    
    let add h key =
      let i = (safehash key) mod (Array.length h.data) in
      let bucket = Cons(key, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize safehash h
    
    let remove h key =
      let rec remove_bucket = function
          Empty ->
            Empty
        | Cons(k, next) ->
            if H.equal k key
            then begin h.size <- pred h.size; next end
            else Cons(k, remove_bucket next) in
      let i = (safehash key) mod (Array.length h.data) in
      h.data.(i) <- remove_bucket h.data.(i)
    let mem h key =
      let rec mem_in_bucket = function
        | Empty ->
            false
        | Cons(k, rest) ->
            H.equal k key || mem_in_bucket rest in
      mem_in_bucket h.data.((safehash key) mod (Array.length h.data))
    
    let iter = iter
    let fold = fold
    let length = length
    let of_list = of_list
    let to_list = to_list
        let any = any
        let all = all
        let includes = includes
        let equals = equals
  end
  
end

module ExtHashtbl = struct
    open Hashtbl
    let keys h =
        let hs = HashSet.create 3 in
        iter (fun k _ -> HashSet.add hs k) h;
        HashSet.to_list hs
    let values h =
        let hs = HashSet.create 3 in
        iter (fun _ v -> HashSet.add hs v) h;
        HashSet.to_list hs
  let of_list l =
    let h = Hashtbl.create 3 in
    List.iter (fun (a,b) -> Hashtbl.add h a b) l;
    h
    let to_list h =
      let l = ref [] in
      Hashtbl.iter (fun k v -> addRefList (k,v) l) h;
      !l    
    include Hashtbl
end

(** k samples from [a,b] *)
let sampleInt (a, b) k =
  let n = b - a + 1 in
  let h = HashSet.create k in
  if k >= n then Array.to_list (Array.init n (fun i -> a + i)) else begin
    let flipped = k > n / 2 in
    let cnt = ref 0 in
    let k' = if flipped then n - k else k in
    while !cnt < k' do
      let j = a+Random.int n in
      if not @$ HashSet.mem h j then begin
        HashSet.add h j;
        incr cnt
      end
    done;
    if flipped then
      let l = ref [] in
      for i = a to b do
        if not @$ HashSet.mem h i then addRefList i l
      done;
      !l
    else
      HashSet.to_list h
  end
let sample pop k =
  Array.of_list @$ List.map (fun i -> pop.(i)) (sampleInt (0, Array.length pop - 1) k)

let randSample f l =
  let rec work acc l = match l with
    | [] -> acc
    | x:: xs ->
    (* printf "%f\n" (Random.float 1.); *)
        if Random.float 1. < f then work (x:: acc) xs else work acc xs in
  (* Random.init (int_of_float @$ (Sys.time ()) *. 1000.); *)
  Random.self_init ();
  work [] l

(** A random number in [0,a), no consecutive duplicates *)
let randInt =
  let lastRandInt = ref (Random.int 33) in
  fun a ->
      let r = ref (Random.int a) in
      while !r = !lastRandInt do
        r := Random.int a
      done;
      lastRandInt := !r;
      !r


module FindOpt = struct
  open ExtList
  let findOptAll f l = (maximumByMap compare f l), List.length l
  let findOptRand p f l =
    (* let l' = ensureNotNull (randSample p l) l in *)
    let a = Array.of_list l in
    let l' = Array.to_list @$ sample a (max 1 @$ int_of_float (p *. float (Array.length a))) in
    (* printf "%s\n" @$ show_list string_of_int l'; *)
    findOptAll f l'
  exception BudgetExceeded
  let findOptUniPeakInterval n f a b =
    let h = Hashtbl.create 3 in
    let best = ref a in
    let cnt = ref 0 in
    let g i =
      if not (Hashtbl.mem h i) then begin
        if !cnt >= n then raise BudgetExceeded;
        (* printf "eval %d\n" i; *)
        Hashtbl.replace h i (f i);
        incr cnt
      end;
      Hashtbl.find h i in
    let rec work a b =
      let h a b =
        let it = float (b - a) in
        a + int_of_float (0.382 *. it), a + int_of_float (0.618 *. it) in
      if abs(a - b) < 3 then
        fst (findOptAll g (range a (b + 1)))
      else begin
        let a', b' = h a b in
        (* printf "%d %d\n" a b; *)
        if abs (a - b) <= 1 then !best else
        if g a' > g b' then begin
          best := a';
          work a b'
        end else begin
          best := b';
          work a' b
        end
      end in
    let v = try work a b with BudgetExceeded -> !best in
    v,!cnt
end


module StateMonad
(*  :sig                                                                      *)
(*    type ('a, 'b) t = 'a -> 'b * 'a                                        *)
(*    val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t              *)
(*    val return : 'a -> ('b, 'a) t                                          *)
(*    val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t           *)
(*    val ( >> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t                    *)
(*    val fmap : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t                      *)
(*    val liftM : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t                     *)
(*    val liftM2 : ('a -> 'b -> 'c) -> ('d, 'a) t -> ('d, 'b) t -> ('d, 'c) t*)
(*    val liftM3 :                                                           *)
(*      ('a -> 'b -> 'c -> 'd) ->                                            *)
(*      ('e, 'a) t -> ('e, 'b) t -> ('e, 'c) t -> ('e, 'd) t                 *)
(*    val join : ('a, ('a, 'b) t) t -> ('a, 'b) t                            *)
(*    val sequence : ('a, 'b) t list -> ('a, 'b list) t                      *)
(*    val sequence_ : ('a, 'b) t list -> ('a, unit) t                        *)
(*    val replicateM : int -> ('a, 'b) t -> ('a, 'b list) t                  *)
(*    val put : 'a -> ('a, unit) t                                           *)
(*    val get : ('a, 'a) t                                                   *)
(*    val modify : ('a -> 'a) -> ('a, unit) t                                *)
(*    val gets : ('a -> 'b) -> ('a, 'b) t                                    *)
(*    val runState : ('a, 'b) t -> 'a -> 'b * 'a                             *)
(*    val evalState : ('a, 'b) t -> 'a -> 'b                                 *)
(*    val execState : ('a, 'b) t -> 'a -> 'a                                 *)
(*  end                                                                      *)
= struct
  open ExtList
  type ('s,'a) t = 's -> 'a *'s
  let bind (m: ('s,'a) t) (k: 'a -> ('s,'b) t) : ('s,'b) t =
    fun s ->
        let (a', s') = m s in
        let (a'', s'') = k a' s' in
        (a'', s'')
  let return (x:'a) : ('s,'a) t = fun s -> (x, s)
  let (>>=) = bind
  let (>>) m m2 = m >>= fun _ -> m2
  let fmap (f:'a ->'b) (m: ('s,'a) t) : ('s,'b) t =
    fun s ->
        let (a', s') = m s in
        (f a', s')
  let liftM f m = m >>= fun x -> return @$ f x
  let liftM2 f m m' = m >>= fun x -> m' >>= fun x' -> return @$ f x x'
  let liftM3 f m m' m'' =
    m >>= fun x -> m' >>= fun x' -> m'' >>= fun x'' -> return @$ f x x' x''
  let join m = m >>= id
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              return (x:: xs) in
    List.fold_right k ms (return [])
  let sequence_ ms = sequence ms >> return ()
  let replicateM n m = sequence (replicate n m)
  let put (s:'a) : ('a, unit) t = fun _ -> ((), s)
  let get : ('a,'a) t = fun s -> (s, s)
  let modify f = get >>= fun s -> put (f s)
  let gets f = get >>= fun s -> return (f s)
  let runState (m : ('a,'b) t) (s:'a) = m s
  let evalState m s = fst (runState m s)
  let execState m s = snd (runState m s)
end

(** tree *)
module TreeF(C:COLLECTION) = struct
  type 'a t = Branch of 'a * 'a t C.t | Leaf
    let empty = Leaf
    let make e l = Branch(e,l)
  let rec map f = function
    | Leaf -> Leaf
    | Branch(e, l) -> Branch(f e, C.map (map f) l)
    let rec zipWith f t t' = match t,t' with
        | Leaf,_ -> Leaf
        | _,Leaf -> Leaf
        | Branch(e,l),Branch(e',l') -> Branch(f e e',C.zipWith (zipWith f) l l')
    let mapi f t =
        let rec work i = function
            | Leaf -> Leaf
            | Branch(e,l) -> Branch(f i e,C.map (work (succ i)) l)
        in
        work 0 t
    let map f d = mapi (const f) d
    let iteri f t =
            let rec work i = function
                | Leaf -> ()
                | Branch(e,l) -> f i e;C.iter (work (succ i)) l in
            work 0 t
    let iter f d = iteri (const f) d

  let unfold f i =
    let rec work i = match f i with
      | Some (e, l) -> Branch (e, C.map work l)
      | None -> Leaf in
    work i
  let rec scanl f i = function
    | Leaf -> Leaf
    | Branch(e, l) ->
        let i' = f i e in
        Branch(i', C.map (scanl f i') l)
  let rec unscanl f i = function
    | Leaf -> Leaf
    | Branch(e, l) ->
        let l' = C.map (unscanl f e) l in
        Branch(f i e, l')
  
  let rec fold_pre f g i = function
    | Leaf -> i
    | Branch (e, l) ->
        let i' = f i e in
        g i e (C.map (fold_pre f g i') l)
  let rec fold_post f i = function
    | Leaf -> i
    | Branch (e, l) ->
        f e (C.map (fold_post f i) l)
  let length tr = fold_post (fun _ l -> 1 + C.foldl (+) 0 l) 0 tr
  let rec leaves = function
    | Leaf -> 1
    | Branch (_,l) -> C.foldl (+) 0 @$ C.map leaves l
  let rec flatten ?(reorder=cons) = function
    | Leaf -> []
    | Branch(e,l) -> reorder e (List.concat @$ C.to_list (C.map (flatten ~reorder) l))  
  let nth n t =
    if n<0 then [] else
      let rec work i = function
        | Leaf -> []
        | Branch(e,l) -> 
          if i=n then [e] else
            if i>n then List.concat @$ C.to_list @$ C.map (work (pred i)) l else
              [] in
      work 0 t 
  (* fold_pre (fun i _ -> i) (fun i e l -> i + 1 + List.fold_left (+) 0 l) 0 *)
  (* tr                                                                      *)
(*  let show show =                                                               *)
(*    fold_pre (fun a b -> a^"  ") (fun i e l ->                                  *)
(*            i^ "<" ^ show e ^ String.concat ",\n" (List.map ((^) i) l) ^ ">") ""*)
end
module Tree = struct 
  module T = TreeF(ExtList)
  include T
  let show show t =
    let b = Buffer.create 3 in
    let print t =
      let rec print_node pref t =
        match t with
          | Branch(e,sons) -> begin
            let s = show e in  
(*        let (s, sons) = decomp t in*)
(*        pp_print_string fmt s;*)
        Buffer.add_string b s;
        if sons <> [] then 
          let w = String.length s in
          let pref' = pref ^ String.make (w + 1) ' ' in
          match sons with
            | [t'] -> 
              Buffer.add_string b "---"; 
              print_node (pref' ^ "  ") t'
            | _ -> Buffer.add_string b "-"; print_sons pref' "+-" sons
          end 
          | Leaf -> ()
      and print_sons pref start = function
        | [] ->  
            assert false
        | [s] ->  
            Buffer.add_string b "`-"; print_node (pref ^ "  ") s
        | s :: sons -> 
            Buffer.add_string b start; print_node (pref ^ "| ") s;
            Buffer.add_string b "\n"; Buffer.add_string b pref;
            print_sons pref "|-" sons
  
      in 
      print_node "" t in
    print t; 
    Buffer.contents b 
  
end

module BinTree = struct 
  module T = TreeF(HomPair)
  include T
  open T
  let mirror = function
    | Leaf -> Leaf
    | Branch(e,p) -> Branch (e,HomPair.rev p)
  let isSymmetric t = t = mirror t
end

module Algebra = struct
  let dumb a = failwith "dumb"
  let dumb2 a a' = failwith "dumb2"
    
  module type R = sig
    type t
    val eq : t -> t -> bool
    val ge : t -> t -> bool
    val le : t -> t -> bool
    val isInfinite : t -> bool
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val recip : t -> t
    
    (** integer division*)
    val quot : t -> t -> t
    val rem : t -> t -> t
    val abs : t -> t
    val compare : t -> t -> int
    val show : t -> string
    val read : string -> t
    val of_int : int -> t
    val to_int : t -> int
    val to_float : t -> float
  end
  module type RING = sig
          type t
          val isInfinite : t -> bool
          val add : t -> t -> t
          val sub : t -> t -> t
          val mul : t -> t -> t
          val recip : t -> t
          val quot : t -> t -> t
          val rem : t -> t -> t
          val abs : t -> t
          val compare : t -> t -> int
          val show : t -> string
          val read : string -> t
          val of_int : int -> t
          val to_int : t -> int
          val to_float : t -> float
          val zero : t
          val one : t
          val isZero : t -> bool
          val div : t -> t -> t
          val neg : t -> t
          val mone : t
          val two : t
          val succ : t -> t
          val pred : t -> t
          val sqr : t -> t
          val cube : t -> t
          val eq : t -> t -> bool
          val ge : t -> t -> bool
          val le : t -> t -> bool
          val neq : t -> t -> bool
          val gt : t -> t -> bool
          val lt : t -> t -> bool
          val max : t -> t -> t
          val min : t -> t -> t
          val gez : t -> bool
          val lez : t -> bool
          val gtz : t -> bool
          val ltz : t -> bool
          val null : t -> bool
          val positive : t -> t
          val gcd : t -> t -> t
          val lcm : t -> t -> t
          val pow : t -> int -> t
          val factorial : int -> t
          val even : t -> bool
          val odd : t -> bool
          val isPrimeTrialDivision : t -> bool
          val coprime : t -> t -> bool    
  end 
  module Ring (R: R) 
  = struct
    (* open R *)
    include R
    let zero = of_int 0
    let one = of_int 1
    let isZero r = R.eq zero r
    let div a a' = mul a (recip a')
    let neg a = sub zero a
    let mone = neg one
    let two = add one one
    let succ a = add one a
    let pred a = add mone a
    let sqr x = mul x x
    let cube x = mul x (sqr x)
    module O = Ord(R)
    include O
    let gez a = ge a zero
    let lez a = le a zero
    let gtz a = gt a zero
    let ltz a = lt a zero
    let sign t = 
      if gtz t then 1 else
        if ltz t then -1 else 0 
    let null a = eq a zero
    let positive a = if lt a zero then zero else a
    let rec gcd a b =
      if eq b zero then a else gcd b (rem a b)
    let lcm a b = div (mul a b) (gcd a b)
    let pow x n =
      let rec pow' x n =
        if n = 0 then one else
        if even n then pow' (sqr x) (n / 2)
        else mul x (pow' (sqr x) (n / 2)) in
      if n >= 0 then pow' x n else
        div one (pow' x (- n))
    let rec factorial n = if n <= 1 then one else mul (of_int n) (factorial (n - 1))
    let even a = rem a two = zero
    let odd a = not (even a)
    let isPrimeTrialDivision a =
      gt a one &&
      let lim = (R.of_int*@int_of_float*@sqrt*@R.to_float) a in
      let rec work i =
        gt i lim || neq (R.rem a i) zero && work (succ i) in
      work two
    let coprime a b = eq (gcd a b) one
    (* least multiple of p at least n *)
    let rec multipleGe p n = 
      if ge n zero then mul p @$ div (pred @$ add n p ) p else neg (multipleLe p (neg n))
    and multipleLe p n = if ge n zero then sub n @$ rem n p else neg (multipleGe p (neg n))
  end
  module Counter(R:R) = struct
    module R = Ring(R)
    type t = {
      inc : unit -> unit;
      get : unit -> R.t 
    }
    let create () : t = 
        let cnt = ref R.zero in
        { inc = (fun () -> cnt := R.succ !cnt) ; get = fun () -> !cnt }
    end

  module RingCollection (R: R) (C: COLLECTION)
(*  :sig                                                *)
(*            val sum : R.t C.t -> R.t                  *)
(*            val product : R.t C.t -> R.t              *)
(*            val average : R.t C.t -> R.t              *)
(*            val variance : R.t C.t -> R.t             *)
(*            val dotProduct : R.t C.t -> R.t C.t -> R.t*)
(*            val formInt : ?radix:int -> R.t C.t -> R.t*)
(*            val median : R.t C.t -> R.t               *)
(*  end                                                 *)
  = struct
    module R = Ring(R)
    open R
    let sum l = C.foldl add zero l
    let product l = C.foldl mul one l
    let average l = div (sum l) (of_int (C.length l))
    let variance l =
      let x = average l in
      div (sum (C.map (fun e -> sqr (sub e x)) l)) (of_int (C.length l - 1))
    let dotProduct l l' = sum @$ C.zipWith mul l l'
    let formInt ?(radix=10) l = 
      C.foldl (fun s a -> R.add a @$ R.mul (R.of_int radix) s) R.zero l 
    let median l = C.nth (C.sortNew compare l) (C.length l / 2)
    let show ?(brackets="[","]") ?(sep =",") l =
      fst brackets ^ String.concat sep (C.to_list @$ C.map R.show l) ^ snd brackets
    let print ?(newline = true) ?(brackets="[","]") ?(sep =",") l =
        print_string (fst brackets);
        C.foldl (fun _ e -> print_string (R.show e);print_string sep) () l;
        print_string (snd brackets);
        if newline then print_newline ()
  end
  
  (* in fact not a ring, as don't have proper div *)
  module Int = Ring (struct
      type t = int
      let eq = (==)
      let ge = (>=)
      let le = (<=)
      let isInfinite _ = false
      let add = (+)
      let sub = (-)
      let mul = ( * )
      let quot = (/)
      let recip = dumb
      let rem = (mod)
      let abs = abs
      let compare = compare
      let show = string_of_int
      let read = int_of_string
      let of_int = id
      let to_int = id
      let to_float = float_of_int
    end)
  module BigInt = Ring (struct
      open Big_int
      type t = big_int
      let eq = eq_big_int
      let ge = ge_big_int
      let le = le_big_int
      let isInfinite _ = false
      let add = add_big_int
      let sub = sub_big_int
      let mul = mult_big_int
      let recip = dumb
      let quot = div_big_int
      let rem = mod_big_int
      let abs = abs_big_int
      let compare = compare
      let show = string_of_big_int
      let read = big_int_of_string
      let of_int = big_int_of_int
      let to_int = int_of_big_int
      let to_float = float_of_big_int
    end)
  
  module Long = Ring (struct
      open Int64
      include Int64
      let eq = (==)
      let ge = (>=)
      let le = (<=)
      let isInfinite _ = false
      let quot = div
      let recip = dumb
      let show = to_string
      let read = of_string
    end)
  
  module Rational = Ring(struct
      open Ratio
      include Ratio
      type t = ratio
      let eq = eq_ratio
      let ge = ge_ratio
      let le = le_ratio
      let isInfinite = null_denominator
      let add = add_ratio
      let sub = sub_ratio
      let mul = mult_ratio
      let recip = inverse_ratio
      let quot = dumb2
      let rem = dumb2
      let abs = abs_ratio
      let compare = compare
      let show = string_of_ratio
      let read = ratio_of_string
      let of_int = ratio_of_int
      let to_int = int_of_ratio
      let to_float = float_of_ratio
    end)
  
  module NumRational = Ring (struct
      open Num
      include Num
      type t = num
      let eq = eq_num
      let ge = ge_num
      let le = le_num
      let isInfinite = Ratio.null_denominator*@ratio_of_num
      let add = add_num
      let sub = sub_num
      let mul = mult_num
      let recip = div_num (num_of_int 1)
      let quot = quo_num
      let rem = mod_num
      let abs = abs_num
      let compare = compare
      let show = string_of_num
      let read = num_of_string
      let of_int = num_of_int
      let to_int = int_of_num
      let to_float = float_of_num
    end)
  module Float = Ring (struct
      type t = float
      let eq = (==)
      let ge = (>=)
      let le = (<=)
      let isInfinite f = f = infinity || f = neg_infinity
      let add = (+.)
      let sub = (-.)
      let mul = ( *.)
      let recip a = 1. /. a
      let quot a b = failwith "quot"
      let rem a b = failwith "rem"
      let abs f = if f > 0. then f else -.f
      let compare = compare
      let show = string_of_float
      let read = float_of_string
      let of_int = float_of_int
      let to_int = int_of_float
      let to_float = id
    end)
  module CN = Ring(struct
      include Complex
      let eq = (=)
      let ge = dumb2
      let le = dumb2
      let isInfinite cn =
        let isInfinite f = f = infinity || f = neg_infinity in
        isInfinite cn.re || isInfinite cn.im
      let quot = dumb2
      let rem = dumb2
      let recip = inv
      let abs c = { re = norm c; im = 0.}
      let compare = compare
      let show (c: t) = string_of_float c.re ^ ":+" ^ string_of_float c.im
      let read s = { re = float_of_string s; im = 0.}
      let of_int i = { re = float_of_int i; im = 0.}
      let to_int = dumb
      let to_float c = c.re
    end)
  module ComplexNumber = struct
    open Complex
    include CN
    let of_pair (a, b) = { re = a; im = b }
  end
  
  module ModInt (R: R) (P: PARAM with type t = int ) :RING = struct
    module R = Ring(R)
    include R
(*    type t = R.t*)
    let lift op = fun a a' -> R.rem (op a a') (R.of_int P.p)
    let add = lift R.add
    let sub = lift R.sub
    let mul = lift R.mul
    let div = lift R.div
    let quot = lift R.quot
    let rem = lift R.rem
    let rep a = R.rem a (R.of_int P.p)
    let abs = rep *@R.abs
    let compare a a' = R.compare (rep a) (rep a')
    let show = R.show *@ rep
    let read = rep *@R.read
    let of_int = rep *@R.of_int
  end
  
  module ModInt2 = ModInt (Int) (struct
      type t = int
      let p = 2
    end)
  
  module FloatArray = RingCollection(Float)(ExtArray)
  module FloatList = RingCollection(Float)(ExtList)
  module IntList = RingCollection(Int)(ExtList)
  module IntArray = RingCollection(Int)(ExtArray)
  
  (* only one letter X *)
  module LPolyX(R: R) = struct
    open List
    (* open ExtList *)
    module R = Ring(R)
    exception DivZero
    type t = (int * R.t) list
    let of_list l =
      let rec loop = function
        | [] -> []
        | (i, k):: (i', k'):: xs when i = i'-> loop @$ (i, R.add k k'):: xs
        | (i, k):: xs when R.eq k R.zero -> loop xs
        | (i, k):: xs -> (i, k):: loop xs in
      loop @$ sort (flip compare) l
    let merge f l l' =
      let rec loop l l' = match l, l' with
        | [], _ -> l'
        | _,[] -> l
        | (i, k):: xs, (i', k'):: xs' ->
            if i > i' then (i, k) :: loop xs l' else
            if i < i' then (i', k') :: loop l xs' else
              (i, f k k') :: loop xs xs' in
      loop l l'
    let add l l' = merge R.add l l'
    let sub l l' = merge R.sub l l'
    let scaleX i l =
      if i = 0 then l else
        map (fun (i', k) -> (i'+ i, k)) l
    let scale (i, k) l =
      if R.eq k R.zero then [] else
      if R.eq k R.one then scaleX i l else
        map (fun (i', k') -> (i'+ i, R.mul k k')) l
    let scaleK k l = scale (0, k) l
    let rank = function
      | [] -> 0
      | (i, k):: xs -> i
    
    let quotHeadTerm (i, k) (i', k') = (i - i', R.quot k k')
    let rec quotRem l l' =
      if rank l'= 0 then raise DivZero else
      if rank l < rank l' then [], l else
        let lam = quotHeadTerm (hd l) (hd l') in
        let q, r = quotRem (sub l @$ scale lam l') l' in
        lam:: q, r
    let quot l l' = fst (quotRem l l')
    let rem l l' = snd (quotRem l l')
    let mul l l' =
      let h = HashMap.create 3 in
      iter (fun (i, k) ->
              iter (fun (i', k') ->
                      let i'' = i + i' in
                      HashMap.set h i'' @$ maybe R.zero id @$ maybefind HashMap.get h i'') l') l;
      of_list @$ HashMap.to_list h
    let div = dumb2
    let of_int i = if i = 0 then [] else [0, R.of_int i]
    let compare = compare
    let read = dumb
    let show a = show_list (show_pair string_of_int R.show) @$ a
  end
  module HPolyX(R: R) = struct
    open HashMap
    module R = Ring(R)
    type t = (int, R.t) HashMap.t
    let add a a' = zipWith R.add
    let sub a a' = zipWith R.sub
    let mul a a' =
      let a'' = create 3 in
      (riter a @$ fun k v ->
            riter a' @$ fun k' v' ->
                let k'' = k + k' in
                set a'' k'' (R.add (R.mul v v') @$ maybe R.zero id (maybefind get a'' k'')));
      a''
    let div = dumb2
    let scale k a = riter a @$ fun k v -> set a k (R.mul k v)
    module L = LPolyX(R)
    let quotRem a a' =
      let l, l' = tmap (L.of_list *@to_list) (a, a') in
      tmap of_list @$ L.quotRem l l'
    let quot a a' = fst (quotRem a a')
    let rem a a' = snd (quotRem a a')
    let of_int i =
      let a = create 3 in
      if i <> 0 then set a 0 (R.of_int i);
      a
    let compare = compare
    let show a = show_list (show_pair string_of_int R.show) @$ sort compare (to_list a)
  end
module type POSITION =
        sig
          type t
          type elt
          val eq : t -> t -> bool
          val neq : t -> t -> bool
          val unit : t
          val zipWith :
            (elt -> elt -> elt) -> t -> t -> t
          val add : t -> t -> t
          val sub : t -> t -> t
          val isMultiple : t -> t -> bool
          val show : t -> string
          val of_elt : elt -> t
          val scale : elt -> t -> t
          val within : t -> t -> bool
          val map : (elt -> elt) -> t -> t  
end

module Pair(R: R) = struct
    module R = Ring(R)
    open Printf
    type elt = R.t
    type t = R.t * R.t
    let make = pair
    let eq (x,y) (x',y') = R.eq x x' && R.eq y y'
    let neq p p' = not (eq p p')
    let map = HomPair.map
    let zipWith = HomPair.zipWith
    let add,sub = tmap zipWith (R.add,R.sub) 
    let mul (x,y) (x',y') = make (R.sub (R.mul x x') (R.mul y y')) (R.add (R.mul x y') (R.mul x' y))
    let unit = (R.one, R.zero)
    
    let isMultiple (x,y) (x',y') =
      if R.isZero x then R.isZero x' else
        R.eq (R.mul x y') @$ R.mul x' y  
(*    let sign (x,y) = compare R.zero x,compare R.zero y *)
    let distance p p' =
      let (x,y) = map R.sqr @$ sub p p' in
      sqrt @$ R.to_float @$ R.add x y
    let scale k p = map (R.mul k) p
    let show p = show_pair R.show R.show p
    let of_elt e = make e e
    let within p p' =
      fst p <= fst p' && snd p <= snd p'
      
    let left (x, y) = (R.pred x , y)
    let right (x, y) = (R.succ x, y)
    let up (x, y) = (x, R.pred y)
    let down (x, y) = (x, R.succ y)
    let upright = up *@ right
    let upleft = up *@ left
    let downright = down *@ right
    let downleft = down *@ left
    
    let iter_neighbours4 f p =
      f (left p);
      f (right p);
      f (up p);
      f (down p)
    let iter_neighbours8 f p =
      iter_neighbours4 f p;
      f (upright p);
      f (upleft p);
      f (downright p);
      f (downleft p)
    let neighbours4 p = [left p; right p; up p; down p]
    let neighbours8 p = [upleft p; upright p; downright p; downleft p] @ neighbours4 p
  end
  
module Triple(R:R) = struct
  module R = Ring(R)
    type t = R.t * R.t * R.t
    type elt = R.t
  let make = triple
    let eq (x,y,z) (x',y',z') = R.eq x x' && R.eq y y' && R.eq z z'
  let neq p p' = not (eq p p')
  let map = HomTriple.map
  let zipWith = HomTriple.zipWith
    let add,sub = tmap zipWith (R.add,R.sub) 
  let isMultiple (x,y,z) (x',y',z') =
    let module P = Pair(R) in
    P.isMultiple (x,y) (x',y') && P.isMultiple (y,z) (y',z')
    let show p = show_triple R.show R.show R.show p
  let unit = make R.one R.zero R.zero
    let of_elt i = make i i i
    let scale k p = map (R.mul k) p
    let within (x,y,z) (x',y',z') = 
        not (R.ge x x') && not (R.ge y y') && not (R.ge z z')
  let distance p p' =
      let (x,y,z) = map R.sqr @$ sub p p' in
      sqrt @$ R.to_float @$ R.add (R.add x y) z
end
(*module FloatPair = Pair(Float)*)
  
  module FloatPair = struct
    module P = Pair(Float)
    include P
    let of_radian r =
      (cos r, sin r)
    let of_angle = of_radian *@angleToRadian
    let rotate radian p =
      mul p (of_radian radian)
    let rotate_angle angle p = rotate (angleToRadian angle) p
    let make_poly n r =
      let rad = 2. *. pi /. float n in
      funPowers (n - 1) (rotate rad) (scale r unit)
  end
module IntPair = Pair(Int)
module FloatTriple = Triple(Float)
module IntTriple = Triple(Int)
        
  module type GROUP = sig
    type t
    val eq : t -> t -> bool
    val op : t -> t -> t
    val inv : t -> t
    val zero : t
    val show : t -> string
  end
  
  (** element of an abelian group without rules *)
  module type XY = sig
    type t
    val compare : t -> t -> int
    val inv : t -> t
  end
  
  module XYProd (XY: XY) = struct
    module S = Set.Make(XY)
    type t = S.t
    let compare = S.compare
    let mulE (e: XY.t) s =
      if S.mem e s then s else
        let e' = XY.inv e in
        if S.mem e' s then S.remove e' s else
          S.add e s
    let mul (s: t) (s': t) : t = S.fold (fun e s -> mulE e s) s s'
    
    let recip (s: t) : t = S.fold (fun e s -> mulE (XY.inv e) s) S.empty s
    let div (s: t) (s': t) : t = mul s (recip s')
    let iter = S.iter
    let singleton = S.singleton
    
    let op = mul
    let zero = S.empty
    let inv = recip
    let eq a a' = compare a a'= 0
  end
  
  (** Persisitent *)
  module GroupVector (R: R) (G: GROUP) = struct
    module R = Ring(R)
    (* open HashMap *)
    module H = HashMap.Make(struct
        type t = G.t
        let equal = G.eq
        let hash = Hashtbl.hash
      end)
    open H
    type t = R.t H.t
    
    let removeZero a =
      let a' = create 3 in
      iter (fun k v -> if not (R.null v) then set a' k v) a;
      a'
    let zipWith f (a: t) (a': t) =
      let a'' = create 3 in
      iter (fun k v ->
              set a'' k (f v @$ maybe R.zero id (maybefind get a' k))) a;
      iter (fun k v ->
              if not (mem a k) then set a'' k v) a';
      removeZero a''
    
    let mul a a' : t =
      let a'' = create 3 in
      (riter a @$ fun k v ->
            riter a' @$ fun k' v' ->
                let k'' = G.op k k' in
                set a'' k'' (R.add (R.mul v v') @$ maybe R.zero id (maybefind get a'' k'')));
      removeZero a''
    let add (a: t) (a': t) = zipWith R.add a a'
    let sub (a: t) (a': t) = zipWith R.sub a a'
    let scale k' a =
      let a' = create 3 in
      iter (fun k v -> set a' k (R.mul k' v)) a;
      a'
    let length = length
    (* a,a' <> zero *)
    let isLinear a a' =
      length a = length a' &&
      try
        let l = ref [] in
        iter (fun k v -> addRefList (R.div (H.find a' k) v) l) a;
        ExtList.allEquals !l
      with Not_found -> false
    let of_list l =
      let a = create 3 in
      List.iter (fun (k, v) ->
              set a k @$ R.add v @$ maybe R.zero id (maybefind get a k)) l;
      removeZero a
    let to_list a =
      let l = ref [] in
      iter (fun k v -> addRefList (v, k) l) a;
      !l
    let show a =
      show_list (show_pair R.show G.show) @$ sort Pervasives.compare @$ to_list a
    (* let recip a : t= riter a (fun k v -> set a (P.inv k) v);a let div a *)
    (* a' = mul a (recip a') let quot (a:t) (a':t) =                       *)
  end
  
  (* module XYProdSum (R:R) (XY:XY) = struct module G = XYProd(XY) module  *)
  (* M = GroupVector(R)(G) include M end                                   *)
  open ExtArray
  
  module Matrix(R: R) = struct
    module R = Ring(R)
    open MaybeMonad
    exception WrongDimension
    type t = R.t array array
    let checkDim b = if not b then raise WrongDimension
    let dims ma = (length ma, length ma.(0))
    let checkSquare ma =
      let (m, n) = dims ma in checkDim (m = n)
    let fold_col f a ma j =
      let r = ref a in
      for i = 0 to snd (dims ma) - 1 do
        r:= f i !r ma.(i).(j)
      done;
      !r
    
    let findFirstColAux f ma j =
      let n = snd (dims ma) in
      let rec loop i =
        if i = n then None else
          let r = f ma.(i).(j) in
          if isSome r then Some (fromSome r, i) else
            loop (i + 1) in
      loop 0
    let findFirstCol f ma j = findFirstColAux f ma j >>= fun(r, _) -> return r
    let findIndexCol f ma j = findFirstColAux f ma j >>= fun(_, i) -> return i
    
    let swapRow = swap
    let swapCol ma j j' =
      let m, n = dims ma in
      for i = 0 to n - 1 do
        let t = ma.(i).(j) in
        ma.(i).(j) <- ma.(i).(j');
        ma.(i).(j') <- t
      done
    
    let add ma ma' = zipWith (zipWith R.add) ma ma'
    let sub ma ma' = zipWith (zipWith R.sub) ma ma'
    let show (ma: t) =
      String.concat "\n" @$ to_list @$ map (show_list R.show *@ to_list) ma
    let transpose = transpose
    let mul ma ma' =
      let dotProduct ma i ma' j =
        let s = ref R.zero in
        for k = 0 to length ma.(i) - 1 do
          s := R.add !s (R.mul ma.(i).(k) ma'.(k).(j))
        done;
        !s in
      let (m, n) = dims ma in
      let (m', n') = dims ma' in
      checkDim (n = m');
      init_matrix m n' (fun i j -> dotProduct ma i ma' j)
    let of_matrix m : t = m
    
    let rec fold_diag f i ma =
      let rec work i sma =
        if Submatrix.null sma then i else work (f i @$ Submatrix.hd sma) (Submatrix.select 1 1 sma) in
      work i (Submatrix.of_matrix ma)
    
    let gaussianElim ma =
      (* zero out head of [r] *)
      let schimdtRow r' r =
        let k = R.div (Subarray.hd r) (Subarray.hd r') in
        Subarray.zipWithAssign R.sub r @$ Subarray.map (R.mul k) r' in
      let rec work sma =
        if not @$ Submatrix.null sma then begin
          let row = Subarray.hd sma in
          Subarray.iter (schimdtRow row) (Subarray.tl sma);
          work (Submatrix.select 1 1 sma)
        end in
      work @$ Submatrix.of_matrix ma;
      ma
    let det ma =
      let ma' = gaussianElim ma in
      fold_diag R.mul R.one ma'
    let tr ma =
      let ma' = gaussianElim ma in
      fold_diag R.add R.zero ma'
    let eye n = init_matrix n n @$ fun i j -> if i = j then R.one else R.zero
    let diag a =
      let n = length a in
      init_matrix n n @$ fun i j -> if i = j then a.(i) else R.zero
    let row_vector a = [| a |]
    let col_vector a = init_matrix (length a) 1 (fun i j -> a.(i))
    
    (* horizontally join two matrices *)
    let jux ma ma' =
      let m, n = dims ma in
      let m', n' = dims ma' in
      checkDim (m = m');
      init_matrix m (n + n') (fun i j -> if j < n then ma.(i).(j) else ma'.(i).(j - n))
    let inverse ma =
      checkSquare ma;
      let n = fst @$ dims ma in
      let ma = jux ma (eye (Array.length ma)) in
      for i = 0 to n - 1 do
      (* print_endline @$ show_matrix R.show ma; *)
        let mi = ref i in
        let mv = ref (R.abs ma.(i).(i)) in
        for j = i + 1 to n - 1 do
          let v = R.abs ma.(j).(i) in
          if v > !mv then begin
            mv := v;
            mi := j
          end
        done;
        if !mi <> i then begin
          let t = ma.(!mi) in
          ma.(!mi) <- ma.(i);
          ma.(i) <- t
        end;
        let k = R.div R.one ma.(i).(i) in
        for j = i to 2 * n - 1 do
          ma.(i).(j) <- R.mul ma.(i).(j) k
        done;
        for k = 0 to n - 1 do
          if k <> i then
            let kk = R.neg ma.(k).(i) in
            for j = i to 2 * n - 1 do
              ma.(k).(j) <- R.add ma.(k).(j) @$ R.mul kk ma.(i).(j)
            done
        done;
      done;
      Array.init n (fun i -> Array.init n (fun j -> ma.(i).(j + n)))
    let recip = inverse
    let to_float = dumb
    let of_int = dumb
    let read = dumb
    let compare a a'= dumb2 a a'
    let abs = dumb
    let rem a a' = dumb2 a a'
    let quot a a' = dumb2 a a'
    let show ma : string = show_matrix R.show ma
  end
  
  module FloatMatrix = Matrix(Float)
  (* let invMatrix = FloatMatrix.inv let mulMatrix = FloatMatrix.mul *)

end

open Algebra
let standardDeviation l = sqrt (Algebra.FloatList.variance l) 

module ExtString = struct
  open String
  let trim s =
      let n = length s in
      if s.[n-1] = '\n' || s.[n-1] = '\r' then String.sub s 0 (n-1) else s
  let eq = (=)
  let drop n s = sub s (min (length s) n) (max 0 (length s - n))
  let take n s = sub s 0 (min (length s) n)
  let tl s = sub s 1 (length s - 1)
  let hd s = get s 0
  let init s = sub s 0 (length s - 1)
  let last s = get s (length s - 1)
  let null s = s =""
  let show = id
  let subStartsWith ~hay:s o s' =
    let n = length s' in
    let rec work i =
      i >= n || unsafe_get s (o + i) == unsafe_get s' i && work (i + 1) in
    o + n <= length s && work 0
  let isSubStringOf ~needle hay =
    ExtList.any (fun i -> subStartsWith hay i needle) (ExtList.range 0 (1+length hay - length needle))

  let to_list s =
    let l = ref [] in
    for i = 0 to length s - 1 do
      addRefList (unsafe_get s i) l
    done;
    List.rev !l
  let of_list l =
    let sb = Buffer.create 3 in
    List.iter (fun c -> Buffer.add_char sb c) l;
    Buffer.contents sb
  let takeWhile f s =
    of_list @$ ExtList.takeWhile f @$ to_list s
  let dropWhile f s =
    of_list @$ ExtList.dropWhile f @$ to_list s
  let rev = of_list *@List.rev *@to_list
  
  let quoteWith ~quote s = quote ^ s ^ quote
  let quote s = quoteWith "\"" s
  let countChar c s = List.length @$ List.filter ((=) c) @$ to_list s
  let split s ~needle:delim =
    let step = length delim in
    let lim = length s - step in
    let rec work acc i pack =
      if i > lim then (List.rev (to_list (sub s i (length s - i))) @ pack) :: acc
      else if subStartsWith s i delim then work (pack:: acc) (i + step) [] else
        work acc (i + 1) (unsafe_get s i:: pack) in
    List.filter (fun s -> s <> "") @$ rev_map (of_list *@List.rev) @$ work [] 0 []
  let splitBy f s =
    List.map of_list @$ List.filter ((<>) []) @$ List.map (ExtList.dropWhile f) @$ 
        ExtList.packBy f @$ to_list s
  let splitChar s c =
      let rec work i acc =
        try
          let i' = String.index_from s i c in
          work (i'+1) (String.sub s i (i'-i)::acc)
        with Not_found ->
            List.rev (String.sub s i (String.length s-i) :: acc) in
      work 0 []    
  let splitBlank s = splitBy ExtChar.isBlank s
    let find ~needle ~hay s e =
        let rec work i =
            if i>= e then None else
                if subStartsWith hay i needle then Some i else work (i+1) in
        work s
(*  (* substitute first occurence of s' in s with s'' *)*)
(*  let substitute_first s s' s'' =                     *)
  let guard b s = if b then s else ""
    let init n f =
        let s = make n '0' in
        for i = 0 to length s -1 do
            set s i (f i)
        done
    let mapi f s =
        init (length s) (fun i -> f i (get s i))
    let map f d = mapi (const f) d
    let iteri f d =
        for i = 0 to length d-1 do
            f d.[i]
            done
    let iter f d = iteri (const f) d
  let rec foldl f i' s =
        let cur = ref i' in
        for i = 0 to length s -1 do
            cur := f !cur (get s i)
        done;
        !cur
    let rec foldr f i' s =
        let cur = ref i' in
        let lim = length s -1 in
        for i = 0 to lim do
            cur := f (get s (lim - i)) !cur 
        done;
        !cur
    let null s = length s =0
    let nth s n = get s n
    let zipWith f s s' =
        init (min (length s) (length s')) (fun i -> f (get s i) (get s' i))
  let lines s = splitChar s '\n'
  let words s = splitChar s ' '
(*  let words s = List.map of_list @$ ExtList.packBy (fun c -> ExtChar.isBlank c) @$ to_list s*)
(*  let words s = split ~needle:" " s*)

    let startsWith ~needle:s' s =
      String.length s >= String.length s' && String.sub s 0 (String.length s') = s'
    let endsWith ~needle:s' s =
      String.length s >= String.length s' && String.sub s (String.length s - String.length s') (String.length s') = s'
    
  open ExtChar
    
    let isBlankString s =
      let lim = String.length s in
      let rec work i = i >= lim || isBlank (String.get s i) && work (i + 1) in
      work 0
                
    let lstrip s =
      let lim = String.length s in
      let rec work i =
        if i < lim && isBlank (String.get s i) then work (i + 1) else i in
      let i = work 0 in
      String.sub s i (String.length s - i)  
      
    let rstrip s =
      let lim = String.length s in
      let rec work i =
        if i >= 0 && isBlank (String.get s i) then work (i - 1) else i in
      let i = work (lim - 1) in
      String.sub s 0 (i + 1)
    
    let strip = rstrip *@ lstrip
    let replace ~hay:s ~old:s' ~new_:s'' =
      let l = String.length s in
      let l' = String.length s' in
      let sb = Buffer.create 33 in
      let i = ref 0 in
      while !i < l do
        if l - !i >= l' && String.sub s !i l' = s' then begin
          Buffer.add_string sb s'';
          i := !i + l'
        end else begin
          Buffer.add_char sb @$ String.get s !i;
          incr i
        end
      done;
      Buffer.contents sb
  let substitute ~old:s' ~new_:s'' s = replace ~hay:s ~old:s' ~new_:s''
    let diffStr s s' =
      if startsWith s s' then begin
        String.sub s (String.length s') (String.length s - String.length s')
      end else ""
  let token needle hay = startsWith ~needle (lstrip hay)
  let nthField n hay = List.nth (splitBlank hay) n
  
  let cleanLines s =
    List.filter (not*@null) @$ List.map strip @$ lines s
  let splitToPair separator s =
    let n = index s separator in
    sub s 0 n,sub s (n+1) (length s-n-1)
  let show = id
    include String
end

module ExtUnix = struct
  exception GotSignal of int
  let withTimeout (secs: float) (* Seconds for timeout *)
      (handler: int -> 'b)
  (* What to do if we have a timeout. The * argument passed is the signal    *)
  (* number * received.                                                      *)
      (f: unit -> 'b) (* The function to run *)
  : 'b =
    let oldHandler =
      Sys.signal Sys.sigalrm
        (Sys.Signal_handle
          (fun i ->
            (* ignore (E.log "Got signal %d\n" i); *)
                raise (GotSignal i)))
    in
    let reset_sigalrm () =
      ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_value = 0.0;
              Unix.it_interval = 0.0;});
      Sys.set_signal Sys.sigalrm oldHandler;
    in
    ignore (Unix.setitimer Unix.ITIMER_REAL
          { Unix.it_value = secs;
            Unix.it_interval = 0.0;});
    (* ignore (Unix.alarm 2); *)
    try
      let res = f () in
      reset_sigalrm ();
      res
    with exc -> begin
          reset_sigalrm ();
          (* ignore (E.log "Got an exception\n"); *)
          match exc with
            GotSignal i ->
              handler i
          | _ -> raise exc
        end
  let timeout secs f = withTimeout secs ignore f
  let rtime f =
      let t = Unix.time () in
      f ();
      Unix.time () -. t  
  let readAll ic =
    let l = ref [] in
    try
      while true do
        addRefList (input_line ic) l
      done;
      failwith "readAll"
    with End_of_file -> String.concat "\n" (List.rev !l)
  let readFile fn = readAll (open_in fn)
  let writeFile ~filename:fn s =
    let oc = open_out fn in
    output_string oc s;
    close_out oc
  let execFull cmd =
    let a = Array.of_list @$ ExtString.splitBlank cmd in
    let ic, oc, ec = Unix.open_process_full a.(0) (ExtArray.drop 1 a) in
(*    if oc <> stdout then close_out oc;*)
    let out,err = readAll ic,readAll ec in
(*    if ic <> stdin then close_in ic;*)
(*    if ec <> stdin then close_in ec;*)
    Unix.close_process_full (ic,oc,ec), (out,err)
  let exec cmd =
    let a,b = snd (execFull cmd) in a ^ "\n" ^ b
  open MaybeMonad

  let months = [|"Jan.";"Feb.";"Mar.";"Apr.";"May";"Jun.";"Jul.";"Aug.";"Sep.";"Oct.";"Nov.";"Dec." |]
  let ppTime ?(local = true) f =
    let tm = (if local then Unix.localtime else Unix.gmtime) f in
    Printf.sprintf "%02d:%02d:%02d, %s%d,%d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec months.(tm.Unix.tm_mon) tm.Unix.tm_mday (tm.Unix.tm_year + 1900)
    let joinPath a' b' =
      let a, b = tmap ExtString.strip (a', b') in
      if ExtString.isBlankString a || ExtString.startsWith b "/" || ExtString.endsWith a "/" then a ^ b else a ^ "/" ^ b
    let subdirs path =
      if Sys.file_exists path && Sys.is_directory path then
        Array.map (joinPath path) @$ Sys.readdir path else [||]
    let list_dir dir =
      (* Tree.fold_pre (fun i e -> i ^ "/" ^ e) (fun i e l -> ) *)
      Tree.unfold (fun i -> if Sys.file_exists i then Some (i, Array.to_list @$ subdirs i) else None) dir   
end

module Bit = struct
  let get n i =
    assert (i<Sys.word_size-1);
    n land (1 lsl i) <> 0
  let set n i = 
    assert (i<Sys.word_size-1);
    n lor (1 lsl i)
  let allSet m n =
(*    assert (n<=Sys.word_size-1);*)
    if m>=n then 0 else
      let f = if n<= Sys.word_size-1 then (1 lsl (n-m))-1 else -1 in
      f lsl m
  let getPart n' m n =
    (n' land (allSet m n)) lsr m
  let setPart n' m n k =
    let k' = getPart n' m n in
    (n' lxor (k' lsl m)) lor (k lsl m)
  let togglePart n' m n =
    let k = getPart n' m n in
    setPart n' m n (lnot k)

  let toggle n i = n lxor (1 lsl i)
  let unset n i = n land (lnot (1 lsl i))
  let crossover2 n n' =
    let m = Random.bits () in
    (n land m) lor (n' land (lnot m))
end
module BitSet = struct
  open Array
  (* open ExtArray *)
  type t = {
    d : int array;
    bits : int
  }
  let w = Sys.word_size - 2
  let create n v =
    assert (n >= 0);
    { d = make (if n mod w = 0 then n / w else (n + w) / w) (if v then max_int else 0);
      bits = n }
  let of_array a = { d = a; bits = w * length a }
  let of_bool_array a =
    { d = map (ofBinary*@List.rev*@to_list) @$ ExtArray.tile w @$ map int_of_bool a; bits = length a}
  let to_array bs = bs.d
(*  let length bs = length bs.d*)
  let bits bs = bs.bits
  let get bs n = Bit.get bs.d.(n / w) (n mod w)
  let to_bool_array bs =
    init bs.bits (fun i -> get bs i)   
  let lift f = fun bs n -> bs.d.(n / w) <- f bs.d.(n / w) (n mod w)
  let set, unset, toggle = tmap3 lift (Bit.set, Bit.unset, Bit.toggle)
  let crossover2 bs bs' =
    { bs with d = init (length bs.d) (fun i -> Bit.crossover2 bs.d.(i) bs'.d.(i)) }
  let crossover a = 
    let bs = of_bool_array @$ init a.(0).bits (fun i -> 
          get (a.(Random.int (Array.length a))) i) in
    {bs with bits = a.(0).bits}
  let copy bs = { bs with d = copy bs.d }
  let random n =
    assert (n >= 0);
    { d = init (if n mod w = 0 then n / w else (n + w) / w) (fun i -> Random.bits ());
      bits = n }

  let iteri f bs =
    let n = bits bs in
    for i = 0 to length bs.d - 1 do
      let lim = min (i * w + w) n - (i * w) - 1 in
      for j = 0 to lim do
        f (i * w + j) (Bit.get bs.d.(i) j)
      done
    done
  let iter f bs = iteri (fun i -> f) bs
  let map f bs =
    let bs' = copy bs in
    let n = bits bs in
    for i = 0 to length bs.d - 1 do
      let s = ref 0 in
      let k = ref 1 in
      let v = ref bs.d.(i) in
      let lim = min (i * w + w) n - (i * w) - 1 in
      for j = 0 to lim do
        s := !s + !k * int_of_bool (f (!v mod 2 != 0));
        v := !v / 2;
        k := !k * 2
      done;
      bs'.d.(i) <- !s
    done;
    bs'
  let lift2 f = fun bs bs' ->
        Array.mapi (fun i e -> f bs.d.(i) e) bs'.d, min bs.bits bs'.bits
  let inter, union, sym_diff = tmap3 lift2 ((land), (lor), (lxor))
  let flip bs = map not bs
  

  let set bs n = bs.d.(n / w) <- Bit.set bs.d.(n / w) (n mod w)
  let unset bs n = bs.d.(n / w) <- Bit.unset bs.d.(n / w) (n mod w)
  let toggle bs n = bs.d.(n / w) <- Bit.toggle bs.d.(n / w) (n mod w)
  let show bs = show_array string_of_bool (to_bool_array bs) 
end

(*(* [n] bit (less than [Sys.word_size - 1]) digit set of [len] long *)    *)
(*class digitSet n len =                                                   *)
(*    let w = Sys.word_size - 1 in                                         *)
(*    object (self)                                                        *)
(*    val d =                                                              *)
(*        assert (n<=w);                                                   *)
(*        Array.make ((n*len+w-1)/w) 0                                     *)
(*    method digits = len                                                  *)
(*    method get i' =                                                      *)
(*        let i = i'*n in                                                  *)
(*        if ((i+n-1)/w) > i/w then                                        *)
(*            let m = i/w *w +w -i in                                      *)
(*            (Bit.getPart d.(i/w) (i mod w) (i mod w + n)) lor            *)
(*            ((Bit.getPart d.(i/w+1) 0 ((i+n-w) mod w)) lsl m)            *)
(*        else                                                             *)
(*            Bit.getPart d.(i/w) (i mod w) (i mod w + n)                  *)
(*    method set i' k' =                                                   *)
(*        let k = k' land (Bit.allSet 0 n) in                              *)
(*        let i = i' *n in                                                 *)
(*        if ((i+n-1)/w) > i/w then begin                                  *)
(*            let m = i/w *w +w -i in                                      *)
(*            d.(i/w) <- Bit.setPart d.(i/w) (i mod w) (i mod w + n ) k;   *)
(*            d.(i/w+1) <- Bit.setPart d.(i/w+1) 0 (i mod w + n) (k lsr m);*)
(*        end else                                                         *)
(*            d.(i/w) <- Bit.setPart d.(i/w) (i mod w) (i mod w + n) k     *)
(*    method copy = {<>}                                                   *)
(*    method mapi f =                                                      *)
(*        let d' = self#copy in                                            *)
(*            for i =0 to self#digits do                                   *)
(*                d'#set i (f i (d'#get i))                                *)
(*            done;                                                        *)
(*            d'                                                           *)
(*    method map f = self#mapi (fun i -> f)                                *)
(*    method iteri f = ignore (self#mapi f)                                *)
(*    method iter f = ignore (self#map f)                                  *)
(*    method to_array =                                                    *)
(*        Array.init len (self#get)                                        *)
(*    method show = show_intarray @$ self#to_array                         *)
(*end                                                                      *)

(* [n] bit (less than [Sys.word_size - 1]) digit set of [len] long *)
module DigitSet = struct
    type t = {
        n : int; (* bits *)
        d : int array;
        digits : int
    }
    let create n len =
        let w = Sys.word_size - 1 in
        assert (n <= w);
        { n = n ; d = Array.make ((n * len + w - 1) / w) 0; digits = len }
    let digits d = d.digits
    let get d' i' =
        let d = d'.d in
        let w = Sys.word_size - 1 in
        let n = d'.n in
        let i = i'* n in
        if ((i + n - 1) / w) > i / w then
            let m = i / w * w + w - i in
            (Bit.getPart d.(i / w) (i mod w) (i mod w + n)) lor
            ((Bit.getPart d.(i / w + 1) 0 ((i + n - w) mod w)) lsl m)
        else
            Bit.getPart d.(i / w) (i mod w) (i mod w + n)
    let set d' i' k' =
        let d = d'.d in
        let w = Sys.word_size - 1 in
        let n = d'.n in
        let k = k' land (Bit.allSet 0 n) in
        let i = i' * n in
        if ((i + n - 1) / w) > i / w then begin
            let m = i / w * w + w - i in
            d.(i / w) <- Bit.setPart d.(i / w) (i mod w) (i mod w + n ) k;
            d.(i / w + 1) <- Bit.setPart d.(i / w + 1) 0 (i mod w + n) (k lsr m);
        end else
            d.(i / w) <- Bit.setPart d.(i / w) (i mod w) (i mod w + n) k
    let copy d = { d with d = Array.copy d.d }
    let mapi f d =
        let d' = copy d in
        for i = 0 to digits d do
            set d' i (f i (get d' i))
        done;
        d'
    let map f d = mapi (fun i -> f) d
    let iteri f d = ignore (mapi f d)
    let iter f d = ignore (map f d)
    let to_array d = Array.init d.digits (get d)
    let show d = IntArray.show @$ to_array d
end
let shuffle p a =
  let n = Array.length a in
  let n' = fraction p n in
  let bs = BitSet.create n true in
  for i = 0 to n' do
    BitSet.unset bs (Random.int n);
  done;
  let l = ref [] in
  let l' = ref [] in
  BitSet.iteri (fun i v -> addRefList a.(i) (if v then l else l')) bs;
  Array.of_list @$ List.rev @$ (!l @ !l')

module Permutation = struct
  type t = (int, int) HashMap.t
  let follow h a =
    Array.init (Array.length a) (fun i -> a.(HashMap.get h i))
  let create n : t =
    let h = HashMap.create 3 in
    for i = 0 to n - 1 do
      HashMap.set h i i
    done;
    h
  let of_array a : t =
    let h = HashMap.create 3 in
    Array.iteri (fun i v -> HashMap.set h i v) @$ a;
    h
  let to_array h =
    let n = HashMap.length h in
    Array.init n (fun i -> HashMap.get h i)
  let of_list l = of_array @$ Array.of_list l
  let to_list h = Array.to_list @$ to_array h
  let length = HashMap.length
  let random n =
    let pick2 n l =
      let rec work i pre l =
        if i = 0 then hd l, pre@(tl l) else
          work (i - 1) (hd l:: pre) (tl l) in
      assert (n >= 0);
      work n [] l in
    let rec work i acc rem =
      if i <= 0 then acc else
        let e, rem' = pick2 (Random.int i) rem in
        work (i - 1) (e:: acc) rem' in
    of_array @$ Array.of_list @$ work n [] (Array.to_list (Array.init n id))
  let randomize a =
    follow (random (Array.length a)) a
  let crossover2 h h' : t =
    (* let options used l = filter (fun i -> not @$ HashSet.mem used i) @$ nub *)
    (* l in                                                                    *)
    let l = (Array.to_list @$ Array.init (HashMap.length h) id) in
    let options used l =
      let l' = match l with
        | [] -> []
        | [x] -> [x]
        | [x; x'] -> if x = x' then [x] else [x; x']
        | _ -> failwith "options" in
      List.filter (fun i -> not @$ HashSet.mem used i) l' in
    let h'' = HashMap.create 3 in
    let postponed = ref [] in
    let used = HashSet.create 3 in
    iter (fun i ->
            match options used [HashMap.get h i; HashMap.get h' i] with
            | [] -> addRefList i postponed
            | [v; v'] ->
                let v = (if Random.int 2 = 0 then v else v') in
                HashSet.add used v;
                HashMap.set h'' i v
            | [v] ->
                HashSet.add used v;
                HashMap.set h'' i v
            | _ -> assert false
      ) l;
    let notUsed = Array.of_list @$ filter (not *@HashSet.mem used) l in
    let a = Array.of_list !postponed in
    ignore @$ Array.iteri (fun i b -> HashMap.set h'' a.(i) b) @$ randomize notUsed;
    h''
  
  let show h = IntArray.show (to_array h)
end

(*class ['a] dynArray = object (self)                                            *)
(*    val a = ref [||]                                                           *)
(*    val cap = ref 0                                                            *)
(*    method enlargeToAtLeast n =                                                *)
(*        let f = ref (float !cap) in                                            *)
(*        while int_of_float (!f) < n do                                         *)
(*            f := !f *. 1.5 +. 1.                                               *)
(*        done;                                                                  *)
(*        let n' = int_of_float !f in                                            *)
(*        a := Array.init n' (fun i -> if i < !cap then !a.(i) else Obj.magic 0);*)
(*        cap := n'                                                              *)
(*    method set i (k:'a) =                                                      *)
(*        if i>= !cap then self#enlargeToAtLeast (i+1);                          *)
(*        Array.unsafe_set !a i k                                                *)
(*    method get i : 'a =                                                        *)
(*        if i>= !cap then self#enlargeToAtLeast (i+1);                          *)
(*        Array.unsafe_get !a i                                                  *)
(*    method to_array = !a                                                       *)
(*    method to_list = Array.to_list self#to_array                               *)
(*    method cap = !cap                                                          *)
(*end                                                                            *)

module FIFO =struct
    type 'a t = {
        mutable cap : int;
        mutable idx : int;
        mutable d : 'a array
    }
    let empty () = {
            cap = 0;
            idx = 0;
            d = [||]
        }
    let push d da=
        if da.idx >= da.cap then begin
            let m = da.cap + 1 in
            da.d <- Array.init (da.cap+m) (fun i -> if i < da.cap then da.d.(i) else Obj.magic 0);
            da.cap <- da.cap + m;
        end;
        da.d.(da.idx) <- d;
        da.idx <- da.idx + 1
    let get da i = da.d.(i) 
    let to_array da = da.d
    let length da = da.idx
end
module FIFO2 = struct
    type 'a t = {
            mutable i : int;
            mutable i' : int;
            mutable d : 'a FIFO.t
        }
    let empty () = {
            i = 0;
            i' = 0;
            d = FIFO.empty ()
        }
    let push d da =
        let i,i' = da.i,da.i' in
        let n = 1 lsl da.i in
        if i' =0 then FIFO.push (Array.init n (fun i -> Obj.magic 0)) da.d;
        (FIFO.get da.d (i)).(i') <- d;
        da.i' <- da.i' + 1;
        if da.i' >= n then begin
            da.i' <- 0;
            da.i <- da.i + 1
        end
(*      printf "%d,%d\n" da.i da.i'*)
    let to_array da =
        let back (i,i') =
            if i' =0 then i-1,1 lsl (i-1) -1 else i,i'-1 in
        let n = 1 lsl da.i -1 + da.i' in
        let res = Array.make n (Obj.magic 0) in
        let a = FIFO.to_array da.d in
        let idx = ref 0 in
        let ii,ii' = back (da.i,da.i') in
        for i = 0 to ii do
            let lim = if i = ii then ii' else 1 lsl i -1 in
(*          printf "res(%d,)\n" i;*)
            for i' = 0 to lim do 
(*              printf "res(,%d)\n" i' ;*)
                res.(!idx) <- a.(i).(i');
                incr idx
            done
        done;
        res
end
module Float = Algebra.Float
module Int = Algebra.Int
module IntList = Algebra.IntList
module FloatList = Algebra.FloatList
let sum = IntList.sum
let product = IntList.product
let average = FloatList.average

module Train = struct
    open ExtList
    type 'a train = 'a Subarray.subarray list
    type 'a t = 'a train
    let hd tr = Subarray.get (List.hd tr) 0
    let tl tr = (Subarray.tl (List.hd tr)) :: List.tl tr
    let len = 4
    let carrige e : 'a Subarray.subarray = Subarray.make (len - 1) (Array.make len e)
    
    let cons e tr = if Subarray.full (List.hd tr) then carrige e :: tr else
            Subarray.cons e (List.hd tr) :: List.tl tr
    let rec of_list = function
        | [] -> []
        | l ->
                let l' = List.rev @$ take len l in
                (List.fold_left (flip Subarray.cons) (carrige (List.hd l')) (List.tl l'))
                :: of_list (drop len l)
    let to_list tr = concatMap Subarray.to_list tr
    let show tr = show_list string_of_int (to_list tr)
    let join (tr:'a t) (tr':'a t) :'a t = tr @ tr'
    let length tr = Algebra.IntList.sum (List.map Subarray.length tr)
    let map f tr = List.map (Subarray.map f) tr
    let null tr = all Subarray.null tr
    let rec foldl f i sa =
        if null sa then i else foldl f (f i (hd sa)) (tl sa)
    let rec foldr f i sa =
        if null sa then i else f (hd sa) (foldr f i (tl sa))
    (* let insert i tr = *)
    let rev tr = List.map Subarray.rev (List.rev tr)
end

(* The idea of QuickCheck attributes to Koen Claessen and John Hughes. The *)
(* Original O'Caml adaption is from                                                  *)
(* http://brierwooddesign.com/2009/1/16/ocaml-quickcheck-translating-quickcheck-from-haskell-type-classes-to-ocaml-modules *)
(* Just included here for convenience, but with modifications.                                      *)
module QuickCheck = struct
  module Random = struct
    include Random
    let int n = int (max n 1)
    let char : char -> char =
      fun lim ->
          let l = Char.code lim in
          let i = int l in
          Char.chr i
    let int_range : int * int -> int =
      fun (lo, hi) ->
          lo + int (hi - lo)
    let int32_range : Int32.t * Int32.t -> Int32.t =
      fun (lo, hi) ->
          Int32.add lo (int32 (Int32.sub hi lo))
    let int64_range : Int64.t * Int64.t -> Int64.t =
      fun (lo, hi) ->
          Int64.add lo (int64 (Int64.sub hi lo))
    let nativeint_range : Nativeint.t * Nativeint.t -> Nativeint.t =
      fun (lo, hi) ->
          Nativeint.add lo (nativeint (Nativeint.sub hi lo))
    let float_range : float * float -> float =
      fun (lo, hi) ->
          lo +. float (hi -. lo)
    let char_range : char * char -> char =
      fun (lo, hi) ->
          let lo' = Char.code lo and hi' = Char.code hi in
          let i = int_range (lo', hi') in
          Char.chr i
  end
  
  module List = struct
    include List
    let rec span : ('a -> bool) -> 'a list -> 'a list * 'a list =
      fun p -> function
            [] -> [],[]
          | x:: xs when p x ->
              let ys, zs = span p xs in
              (x:: ys, zs)
          | xs -> [], xs
    
    let rec groupBy : ('a -> 'a -> bool) -> 'a list -> 'a list list =
      fun p -> function
            [] -> []
          | x:: xs ->
              let ys, zs = span (p x) xs in
              (x:: ys) :: groupBy p zs
    
    let group xs = groupBy (=) xs
  end
  
  type 'a gen = Gen of (int -> 'a)
  type pretty_str = Format.formatter -> unit -> unit
  
  module type PSHOW = sig
    type t
    val show : t -> pretty_str
  end
  
  module type SHOW = sig
    type t
    val show : t -> string
  end
  
  module Show(P: PSHOW) = struct
    open Buffer
    open Format
    type t = P.t
    let show : t -> string =
      fun x ->
          let f _ =
            let str = contents stdbuf in
            clear stdbuf;
            str
          in
          clear stdbuf;
          kfprintf f str_formatter "@[%a@]@?" (P.show x) ()
  end
  
  module PShow_list(Elt: PSHOW) = struct
    type t = Elt.t list
    let show : t -> pretty_str =
      fun xs fmt () ->
          let pp = Format.fprintf in
          match List.map Elt.show xs with
            [] -> pp fmt "[]"
          | a1:: an ->
              let pprest f =
                List.iter (fun e -> pp f ";@ %a" e ())
              in
              pp fmt "[%a%a]" a1 () pprest an
  end
  module PShow_array(Elt: PSHOW) = struct
    type t = Elt.t array
    let show : t -> pretty_str =
      fun xs' fmt () ->
          let xs = Array.to_list xs' in
          let pp = Format.fprintf in
          match List.map Elt.show xs with
            [] -> pp fmt "[||]"
          | a1:: an ->
              let pprest f =
                List.iter (fun e -> pp f ";@ %a" e ())
              in
              pp fmt "[|%a%a|]" a1 () pprest an
  end  
  module PShow_pair(Elt: PSHOW)(Elt2: PSHOW) = struct
    type t = Elt.t * Elt2.t
    let show : t -> pretty_str =
      fun (a,b) fmt () ->
        Format.fprintf fmt "(%a,%a)" (Elt.show a) () (Elt2.show b) ()
  end
  module PShow_triple(Elt: PSHOW)(Elt2: PSHOW)(Elt3: PSHOW) = struct
    type t = Elt.t * Elt2.t * Elt3.t 
    let show : t -> pretty_str =
      fun (a,b,c) fmt () ->
        Format.fprintf fmt "(%a,%a,%a)" (Elt.show a) () (Elt2.show b) () (Elt3.show c) ()
  end  
  module PShow_char = struct
    type t = char
    let show : t -> pretty_str =
      fun c fmt () ->
          Format.fprintf fmt "%C" c
  end
  
  module PShow_int = struct
    type t = int
    let show : t -> pretty_str =
      fun c fmt () ->
          Format.fprintf fmt "%d" c
  end
  
  (* generator functions *)
  
  let sized : (int -> 'a gen) -> 'a gen =
    fun f -> Gen (fun n ->
              let Gen m = f n in
              m n)
  
  let resize : int -> 'a gen -> 'a gen =
    fun n (Gen m) -> Gen (fun _ -> m n)
  
  let promote : ('a -> 'b gen) -> ('a -> 'b) gen =
    fun f ->
        Gen (fun n ->
              fun a ->
                  let Gen m = f a in
                  m n)
  
  let variant : int -> 'a gen -> 'a gen =
    fun _v (Gen m) -> Gen (fun n -> m n)
  
  let generate : int -> 'a gen -> 'a =
    fun n (Gen m) ->
        let size = Random.int n in
        m size
  
  let map_gen : ('a -> 'b) -> 'a gen -> 'b gen =
    fun f (Gen m) ->
        Gen (fun n ->
              let v = m n in
              f v)
  
  let ret_gen : 'a -> 'a gen =
    fun a -> Gen (fun _n -> a)
  
  let (>>=) : 'a gen -> ('a -> 'b gen) -> 'b gen =
    fun (Gen m) k ->
        Gen (fun n ->
              let v = m n in
              let Gen m' = k v in
              m' n)
  
  let lift_gen : ('a -> 'b) -> 'a -> 'b gen =
    fun f -> fun a -> Gen (fun _ -> f a)
  
  let choose_int = lift_gen Random.int_range
  let choose_int0 = lift_gen Random.int
  let choose_char = lift_gen Random.char_range
  let choose_float = lift_gen Random.float_range
  
  let elements : 'a list -> 'a gen =
    fun xs ->
        map_gen (List.nth xs)
          (choose_int0 (List.length xs))
  
  let vector : 'a gen -> int -> 'a list gen =
    fun (Gen gelt) l ->
        Gen (fun n ->
              let rec gen acc = function
                  0 -> acc
                | l -> gen (gelt n :: acc) (l - 1)
              in gen [] l)
  let array : 'a gen -> int -> 'a array gen =
    fun (Gen gelt) l ->
        Gen (fun n ->
              let rec gen acc = function
                  0 -> acc
                | l -> gen (gelt n :: acc) (l - 1)
              in Array.of_list @$ gen [] l)  
  let oneof : 'a gen list -> 'a gen =
    fun gens -> elements gens >>= fun x -> x
  
  module type ARBITRARY = sig
    type t
    val arbitrary : t gen
  end
  
  module Arbitrary_unit = struct
    type t = unit
    let arbitrary = ret_gen ()
  end
  
  module Arbitrary_bool = struct
    type t = bool
    let arbitrary = elements [true; false]
  end
  
  module Arbitrary_char = struct
    type t = char
    let arbitrary =
      choose_int (32, 255) >>= fun c ->
          ret_gen (Char.chr c)
  end
  
  module Arbitrary_int = struct
    type t = int
    let arbitrary = sized (fun n -> choose_int (- n, n))
  end
  
  module Arbitrary_float = struct
    type t = float
    let arbitrary =
      Arbitrary_int.arbitrary >>= fun a ->
          Arbitrary_int.arbitrary >>= fun b ->
              sized choose_int0 >>= fun c ->
                  ret_gen
                    (float a +. (float b /. (float c +. 1.)))
  end
  
  module Arbitrary_pair(Fst: ARBITRARY)(Snd: ARBITRARY) = struct
    type t = Fst.t * Snd.t
    let arbitrary =
      Fst.arbitrary >>= fun v1 ->
          Snd.arbitrary >>= fun v2 ->
              ret_gen (v1, v2)
  end
  
  module Arbitrary_triple(Fst: ARBITRARY)(Snd: ARBITRARY)(Trd: ARBITRARY) = struct
    type t = Fst.t * Snd.t * Trd.t
    let arbitrary =
      Fst.arbitrary >>= fun v1 ->
          Snd.arbitrary >>= fun v2 ->
              Trd.arbitrary >>= fun v3 ->
                  ret_gen (v1, v2, v3)
  end
  
  module Arbitrary_list(Elt: ARBITRARY) = struct
    type t = Elt.t list
    let arbitrary =
      sized choose_int0 >>= vector Elt.arbitrary
  end
  module Arbitrary_array(Elt: ARBITRARY) = struct
    type t = Elt.t array
    let arbitrary = 
      sized choose_int0 >>= array Elt.arbitrary
  end 
  (*********** testable ************)
  
  type result = {
    ok : bool option;
    stamp : string list;
    arguments : pretty_str list;
  }
  
  type property = Prop of result gen
  
  module type TESTABLE = sig
    type t
    val property : t -> property
  end
  
  let nothing : result = { ok = None; stamp =[]; arguments =[]}
  
  let result : result -> property =
    fun res -> Prop (ret_gen res)
  
  module Testable_unit = struct
    type t = unit
    let property () = result nothing
  end
  
  module Testable_bool = struct
    type t = bool
    let property b = result { nothing with ok = Some b }
  end
  
  module Testable_result = struct
    type t = result
    let property r = result r
  end
  
  module Testable_property = struct
    type t = property
    let property p = p
  end
  
  module Evaluate(T: TESTABLE) = struct
    let evaluate : T.t -> result gen =
      fun a ->
          let Prop gen = T.property a in
          gen
  end
  
  module ForAll(S: PSHOW)(T: TESTABLE) = struct
    module E = Evaluate(T)
    let forAll : S.t gen -> (S.t -> T.t) -> property =
      fun gen body ->
          let argument a res =
            { res with arguments = S.show a :: res.arguments }
          in
          Prop
          (gen >>= fun a ->
                E.evaluate (body a) >>= fun res ->
                    ret_gen (argument a res))
  end
  
  module Testable_fun
      (A: ARBITRARY)
      (S: PSHOW with type t = A.t)
      (T: TESTABLE) =
  struct
    module F = ForAll(S)(T)
    type t = A.t -> T.t
    let property : t -> property =
      fun f ->
          F.forAll A.arbitrary f
  end
  
  module Implies(T: TESTABLE) = struct
    let (==>) : bool -> T.t -> property =
      fun b a ->
          if b
          then T.property a
          else Testable_unit.property ()
  end
  
  module Label(T: TESTABLE) = struct
    module E = Evaluate(T)
    let label : string -> T.t -> property =
      fun s a ->
          let add r = { r with stamp = s :: r.stamp } in
          let a' = E.evaluate a in
          Prop (map_gen add a')
  end
  
  module Classify(T: TESTABLE) = struct
    module L = Label(T)
    let classify : bool -> string -> T.t -> property =
      function
        true -> L.label
      | false -> fun _ -> T.property
    let trivial : bool -> T.t -> property =
      fun b -> classify b "trivial"
  end
  
  module Collect(S: SHOW)(T: TESTABLE) = struct
    module L = Label(T)
    let collect : S.t -> T.t -> property =
      fun v -> L.label (S.show v)
  end
  
  type config = {
    maxTest : int;
    maxFail : int;
    size : int -> int;
    every : Format.formatter -> int * pretty_str list -> unit;
  }
  
  let quick = {
    maxTest = 100;
    maxFail = 1000;
    size = (fun n -> 3 + n / 2);
    every = (fun _ (_, _) -> ())
  }
  
  let verbose = {
    quick with
    every = begin fun f (n, args) ->
            let pargs fmt l =
              List.iter (fun a -> Format.fprintf fmt "@ %a" a ()) l
            in
            Format.fprintf f "@[%d:@[<hov 2>%a@]@]@." n pargs args
      end
  }
  
  let done_ : string -> int -> string list list -> unit =
    fun mesg ntest stamps ->
        let percentage n m =
          Format.sprintf "%2d%%" ((100 * n) / m)
        in
        let entry (n, xs) =
          Format.sprintf "%s %s" (percentage n ntest) (String.concat ", " xs)
        in
        let pairLength = function
            (xs:: _) as xss -> (List.length xss, xs)
          | [] -> assert false
        in
        let display = function
            [] -> ".\n"
          | [x] -> Format.sprintf " (%s).\n" x
          | xs ->
              String.concat "\n" ("." :: List.map (Format.sprintf "%s.") xs)
        in
        let not_null = function [] -> false | _ -> true in
        let table =
          display
            (List.map entry
                (List.rev
                    (List.sort compare
                        (List.map pairLength
                            (List.group
                                (List.sort compare
                                    (List.filter not_null
                                        stamps)))))))
        in
        Format.printf "%s %d tests%s" mesg ntest table
  
  let rec tests : config -> result gen -> int -> int -> string list list -> unit =
    fun config gen ntest nfail stamps ->
        if ntest = config.maxTest
        then done_ "OK, passed" ntest stamps
        else if nfail = config.maxFail
        then done_ "Arguments exhausted after" nfail stamps
        else begin
          let result = generate (config.size ntest) gen in
          let () =
            Format.printf "@[%a@]@?" config.every (ntest, result.arguments)
          in
          match result.ok with
            None ->
              tests config gen ntest (nfail + 1) stamps
          | Some true ->
              tests config gen (ntest + 1) nfail (result.stamp :: stamps)
          | Some false ->
              let p f = function
                  [] -> ()
                | h:: t ->
                    h f ();
                    List.iter (fun s -> Format.fprintf f "@ %a" s ()) t
              in
              Format.printf "@[<2>Falsifiable, after %d tests:@ %a@]@."
                ntest p result.arguments
        end
  
  module Check(T: TESTABLE) = struct
    module E = Evaluate(T)
    let check : config -> T.t -> unit =
      fun config a ->
          tests config (E.evaluate a) 0 0 []
    let test = check quick
    let quickCheck = test
    let verboseCheck = check verbose
  end
  
  module Arbitrary_intlist = Arbitrary_list(Arbitrary_int)
    module Arbitrary_intarray = Arbitrary_array(Arbitrary_int)
    module Arbitrary_intpair = Arbitrary_pair(Arbitrary_int)(Arbitrary_int)
  module Arbitrary_int_intlist_pair = Arbitrary_pair(Arbitrary_int)(Arbitrary_intlist)
    module Arbitrary_inttriple = Arbitrary_triple(Arbitrary_int)(Arbitrary_int)(Arbitrary_int)
    module PShow_intlist = PShow_list(PShow_int)
    module PShow_intarray = PShow_array(PShow_int)
    module PShow_intpair = PShow_pair(PShow_int)(PShow_int)
  module PShow_int_intlist_pair = PShow_pair(PShow_int)(PShow_intlist)
    module PShow_inttriple = PShow_triple(PShow_int)(PShow_int)(PShow_int)
    module Testable_intlist_to_bool = Testable_fun  (Arbitrary_intlist) (PShow_intlist) (Testable_bool)
    module Testable_intarray_to_bool = Testable_fun  (Arbitrary_intarray) (PShow_intarray) (Testable_bool)
    module Testable_int_to_bool = Testable_fun  (Arbitrary_int) (PShow_int) (Testable_bool)
    module Testable_intpair_to_bool = Testable_fun  (Arbitrary_intpair) (PShow_intpair) (Testable_bool)
  module Testable_int_intlist_pair_to_bool = 
    Testable_fun  (Arbitrary_int_intlist_pair) (PShow_int_intlist_pair) (Testable_bool)
    module Testable_inttriple_to_bool = Testable_fun  (Arbitrary_inttriple) (PShow_inttriple) (Testable_bool)

  (* (set (make-local-variable 'flymake-ocaml-build-file) "Makefile") *)
  
end

module Parray = struct
    (**************************************************************************)
    (*                                                                        *)
    (*  Copyright (C) Jean-Christophe Filliatre                               *)
    (*                                                                        *)
    (*  This software is free software; you can redistribute it and/or        *)
    (*  modify it under the terms of the GNU Library General Public           *)
    (*  License version 2.1, with the special exception on linking            *)
    (*  described in file LICENSE.                                            *)
    (*                                                                        *)
    (*  This software is distributed in the hope that it will be useful,      *)
    (*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
    (*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
    (*                                                                        *)
    (**************************************************************************)
    
    (* Persistent arrays implemented using Backer's trick.
    
       A persistent array is a usual array (node Array) or a change into 
       another persistent array (node Diff). Invariant: any persistent array is a
       (possibly empty) linked list of Diff nodes ending on an Array node.
    
       As soon as we try to access a Diff, we reverse the linked list to move
       the Array node to the position we are accessing; this is achieved with
       the reroot function.
    *)
    
    type 'a t = 'a data ref
    and 'a data =
      | Array of 'a array 
      | Diff of int * 'a * 'a t
          
    let create n v = ref (Array (Array.create n v))
    let make = create
    
    let init n f = ref (Array (Array.init n f))
    
    (* reroot t ensures that t becomes an Array node *)
    let rec reroot t = match !t with
      | Array _ -> ()
      | Diff (i, v, t') -> 
          reroot t';
          begin match !t' with
        | Array a as n ->
            let v' = a.(i) in
            a.(i) <- v;
            t := n;
            t' := Diff (i, v', t)
        | Diff _ -> assert false
          end
      
    (* we rewrite it using CPS to avoid a possible stack overflow *)
    let rec rerootk t k = match !t with
      | Array _ -> k ()
      | Diff (i, v, t') -> 
          rerootk t' (fun () -> begin match !t' with
               | Array a as n ->
                   let v' = a.(i) in
                   a.(i) <- v;
                   t := n;
                   t' := Diff (i, v', t)
               | Diff _ -> assert false end; k())
    
    let reroot t = rerootk t (fun () -> ())
    
    let rec get t i = match !t with
      | Array a -> 
          a.(i)
      | Diff _ -> 
          reroot t; 
          begin match !t with Array a -> a.(i) | Diff _ -> assert false end
          
    let set t i v = 
      reroot t;
      match !t with
      | Array a as n ->
          let old = a.(i) in
          if old == v then
        t
          else begin
        a.(i) <- v;
        let res = ref n in
        t := Diff (i, old, res);
        res
          end
      | Diff _ ->
          assert false
    
    (* wrappers to apply an impure function from Array to a persistent array *)
    let impure f t =
      reroot t;
      match !t with Array a -> f a | Diff _ -> assert false
    
    let length t = impure Array.length t
    
    let to_list t = impure Array.to_list t
    
    let iter f t = impure (Array.iter f) t
    let iteri f t = impure (Array.iteri f) t
    
    let fold_left f acc t = impure (Array.fold_left f acc) t
    let fold_right f t acc = impure (fun a -> Array.fold_right f a acc) t
end
module PUnionFind = struct
  module Pa = Parray
    (**************************************************************************)
    (*                                                                        *)
    (*  Copyright (C) Jean-Christophe Filliatre                               *)
    (*                                                                        *)
    (**************************************************************************)
  
    (* Tarjan's algorithm *)
    
    type t = {
      mutable father: int Pa.t; (* mutable to allow path compression *)
      c:int Pa.t; (* ranks *)
    }
    
    let create n =
      { c = Pa.create n 0;
        father = Pa.init n (fun i -> i) }
    
    let rec find_aux f i =
      let fi = Pa.get f i in
      if fi == i then
        f, i
      else
        let f, r = find_aux f fi in
        let f = Pa.set f i r in
        f, r
    
    let find h x =
      let f,rx = find_aux h.father x in h.father <- f; rx
    
    let union h x y =
      let rx = find h x in
      let ry = find h y in
      if rx != ry then begin
        let rxc = Pa.get h.c rx in
        let ryc = Pa.get h.c ry in
        if rxc > ryc then
          { h with father = Pa.set h.father ry rx }
        else if rxc < ryc then
          { h with father = Pa.set h.father rx ry }
        else
          { c = Pa.set h.c rx (rxc + 1);
      father = Pa.set h.father ry rx }
      end else
        h
end  

module UnionFind = struct
(*
 *
 * Copyright (c) 2001-2002, 
 *  John Kodumal        <jkodumal@eecs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
exception Bad_find

type 'a urefC =
    Ecr of 'a * int
  | Link of 'a uref
and 'a uref = 'a urefC ref

let rec find p = 
  match !p with
    | Ecr _ -> p
    | Link p' ->
    let p'' = find p' 
    in p := Link p''; p''

let uref x = ref (Ecr(x,0))

let equal p p'= (find p == find p')

let deref p = 
  match ! (find p) with 
    | Ecr (x,_) -> x
    | _ -> raise Bad_find

let update p x = 
  let p' = find p 
  in
    match !p' with
      | Ecr (_,rank) -> p' := Ecr(x,rank)
      | _ -> raise Bad_find
      
let unifyWith f p q = 
  let p',q' = find p, find q in
    match (!p',!q') with 
      | (Ecr(px,pr),Ecr(qx,qr)) ->
    let x = f px qx in
      if (p' == q') then
        p' := Ecr(x,pr)
      else if pr == qr then
        (q' := Ecr(x,qr+1); p' := Link q')
      else if pr < qr then
        (q' := Ecr(x,qr); p' := Link q')
      else (* pr > qr *)
        (p' := Ecr(x,pr); q' := Link p')
      | _ -> raise Bad_find
      
let union p q = 
  let p',q' = find p, find q in
    match (!p',!q') with 
      | (Ecr(px,pr),Ecr(qx,qr)) ->
      if (p' == q') then 
        ()
      else if pr == qr then
        (q' := Ecr(qx, qr+1); p' := Link q')
      else if pr < qr then
        p' := Link q'
      else (* pr > qr *)
        q' := Link p'
      | _ -> raise Bad_find  
  end  

module Digit = struct
    module BI = Algebra.BigInt
    (* the list contatins digits in reverse order *)
    type t = int list * int
    let toBigInt (l,n') =
        let n = BI.of_int n' in
        let rec work a = function
            | [] -> a
            | x::xs -> work (BI.add (BI.mul n a) (BI.of_int x)) xs in
        work BI.zero l
    let ofBigInt n' bi =
        let n = BI.of_int n' in
        let rec work bi acc =
            if BI.eq bi BI.zero then acc else
                work (BI.quot bi n) (BI.to_int (BI.rem bi n) :: acc) in
        work bi [],n'
    let show (l,n) =
        Printf.sprintf "(%d,%s)" n @$ 
            IntList.show @$ List.rev l 
end

module type STATE = sig
    type t
    type action
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val next : t -> (t*action) list
    val final : t -> bool
end  

module Search(S:STATE) = struct
    exception Found of S.action list
    exception Stop of S.t
    module T = Weak.Make(S)
    module M = Map.Make(S)
    let dfs s =
        let visited = T.create 3 in
        let rec work s acc =
            T.add visited s;
            if S.final s then raise (Found acc) else
                iter (fun (s',a) -> work s' (a::acc)) (filter (not*@T.mem visited*@fst) @$ S.next s) in
        try work s [];None
        with Found acc -> Some acc
    let bfs s =
          let trace s h =
                let rec work s acc =
                    try
                        let (s',a) = HashMap.get h s in 
                        work (s') (a::acc) 
                    with Not_found -> acc in
                work s [] in
        let h = HashMap.create 3 in
        let q = Queue.create () in
        Queue.push s q;
        try
            while true do
                let s = Queue.pop q in
                if S.final s then raise (Stop s) else
                    iter (fun (s',a) -> Queue.add s' q;HashMap.set h s' (s,a)) (S.next s)
            done;
            None
        with Stop s -> Some (trace s h)
            | Queue.Empty -> None
end

module Dlist = struct
    open ExtList
    type 'a t = 'a list * 'a list
    let cons d (l,l') = l,d::l'
    let shift (l,l') =
        if l'==[] then None else Some (hd l'::l,tl l')
    let rev_shift (l,l') = 
        if l==[] then None else Some (tl l,hd l::l')
    let cur (l,l') = 
        if l'==[] then None else Some (hd l')
    let rest (l,l') = if l'==[] then None else Some (l,tl l')
    let hd (l,l') = 
        if l==[] then 
            if l'==[] then None else Some(hd l') 
        else Some (last l)
    let tl (l,l') = 
        if l==[] then 
            if l'==[] then None else Some(l,tl l') 
        else Some (trimLast l,l')
    let of_list l : 'a t = [],l
    let to_list (l,l') = List.rev l @ l'
    let normalize (l,l') = [],List.rev l@l'
    let equal t t' = to_list t = to_list t'
    let rev (l,l') = l',l
    let append (l,l') (l2,l2') = l,l'@List.rev l2 @ l2'
    let length (l,l') = length l + length l'
end
module ExtSet_
:
  sig
    module Make :
      functor (O : Set.OrderedType) ->
        sig
          type elt = O.t
          type t = Set.Make(O).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val max_elt : t -> elt
          val choose : t -> elt
          val split : elt -> t -> t * bool * t
          type set = t
          val foldl : ('a -> elt -> 'a) -> 'a -> t -> 'a
          val foldr : (elt -> 'a -> 'a) -> 'a -> t -> 'a
          val map : (elt -> elt) -> t -> t
          val to_list : t -> elt list
          val of_list : elt list -> t
          val singleton : elt -> t
        end
  end
 = struct
  module Make(O: Set.OrderedType) = struct
    module S_ = Set.Make(O)
    include S_
    type set = t
(*    module SS = Set.Make(struct type t = set let compare = S.compare end)*)
    let foldl f i s =
      fold (flip f) s i
    let foldr f i s = fold f s i
    let map f s =
      foldr (fun k s -> add (f k) s) empty s
    let to_list s =
      let l = ref [] in
      !(fold (fun k l -> addRefList k l; l) s l)
    let of_list l =
      List.fold_left (fun s e -> add e s) empty l
    let singleton e =
      of_list [e]
(*    let concat ss =          *)
(*      SS.fold union ss empty *) 
  end
end

module ExtSet
:
  sig
    module Make :
      functor (O : Set.OrderedType) ->
        sig
          type elt = O.t
          type t = Set.Make(O).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val max_elt : t -> elt
          val choose : t -> elt
          val split : elt -> t -> t * bool * t
          type set = t
          val foldl : ('a -> elt -> 'a) -> 'a -> t -> 'a
          val foldr : (elt -> 'a -> 'a) -> 'a -> t -> 'a
          val map : (elt -> elt) -> t -> t
          val to_list : t -> elt list
          val of_list : elt list -> t
          val singleton : elt -> t
          module Set :
            sig
              type elt = set
              type t
              val empty : t
              val is_empty : t -> bool
              val mem : elt -> t -> bool
              val add : elt -> t -> t
              val remove : elt -> t -> t
              val union : t -> t -> t
              val inter : t -> t -> t
              val diff : t -> t -> t
              val compare : t -> t -> int
              val equal : t -> t -> bool
              val subset : t -> t -> bool
              val iter : (elt -> unit) -> t -> unit
              val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
              val for_all : (elt -> bool) -> t -> bool
              val exists : (elt -> bool) -> t -> bool
              val filter : (elt -> bool) -> t -> t
              val partition : (elt -> bool) -> t -> t * t
              val cardinal : t -> int
              val elements : t -> elt list
              val min_elt : t -> elt
              val max_elt : t -> elt
              val choose : t -> elt
              val split : elt -> t -> t * bool * t
              type set = t
              val foldl : ('a -> elt -> 'a) -> 'a -> t -> 'a
              val foldr : (elt -> 'a -> 'a) -> 'a -> t -> 'a
              val map : (elt -> elt) -> t -> t
              val to_list : t -> elt list
              val of_list : elt list -> t
              val singleton : elt -> t
            end
          val concat : Set.t -> t
          val concatMap : (elt -> t) -> t -> t
          val fixpoint : (elt -> t) -> t -> t
        end
  end 
= struct
  module Make(O: Set.OrderedType) = struct
    module S = ExtSet_.Make(O)
    include S
    module Set = ExtSet_.Make(struct type t = set let compare = S.compare end)
    let concat ss =
      Set.fold union ss empty
    let concatMap f s =
      foldr (fun e s -> union (f e) s) empty s
    let fixpoint f s =
      let rec work set s =
        let s' = diff (concatMap f s) set in
        if s' = empty then set else
          work (union s' set) s' in
      work empty s
    end
  end
  
module IntSet = ExtSet.Make(struct type t = int let compare = compare end)
module IntListSet = ExtSet.Make(struct type t = int list let compare = compare end)  
  
module ExtMap = struct
  module Make(Ord: Map.OrderedType) = struct
    module M = Map.Make(Ord)
    include M
    module S = ExtSet.Make(Ord)
    let swap i j m =
      try
        let t = find i m in
        add j t @$ add i (find j m) m
      with Not_found -> failwith "swap"
    let foldli f a l =
      let i = ref 0 in
      List.fold_left (fun a e -> let a' = f !i a e in incr i; a') a l
    let to_list m =
      let l = ref [] in
      iter (fun k v -> addRefList (k, v) l) m;
      !l
    let of_list l = 
      List.fold_left (fun m (a,b) -> add a b m) M.empty l
    let zipWith f m m' = mapi (fun i v -> f v (find i m')) m
    let get = find
    let set = add
    let keys m = S.to_list @$ M.fold (fun k _ s -> S.add k s) m S.empty
    let values m = S.to_list @$ M.fold (fun _ a s -> S.add a s) m S.empty
  end
end
module IntMap = ExtMap.Make(struct type t = int let compare = compare end)
module ListMap
= struct
  module Make(Ord: Map.OrderedType)
  : sig
    type 'a t = 'a list ExtMap.Make(Ord).t
    type key = ExtMap.Make(Ord).key
    val empty : 'a t
    val is_empty : 'a t -> bool
    val removeKey : key -> 'a t -> 'a t
    val remove : key -> 'a -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    (* val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val get : key -> 'a t -> 'a list
    val add : key -> 'a -> 'a t -> 'a t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end
  
  = struct
    open ExtList
    module MM = ExtMap.Make(Ord)
    (* include MM *)
    type 'a t = 'a list MM.t
    type key = MM.key
    let empty = MM.empty
    let is_empty = MM.is_empty
    let removeKey = MM.remove
    let mem = MM.mem
    let iter f m = MM.iter (fun k -> List.iter (f k)) m
    let map f m = MM.map (List.map f) m
    (* let mapi = MM.mapi *)
    let fold f m r = MM.fold (fun k l r -> List.fold_right (f k) l r) m r
    let compare f m m' = MM.compare (liftCompare f) m m'
    let equal f m m' = MM.equal (liftEqual f) m m'
    let findlist f a b = try f a b with Not_found -> []
    let get k m = findlist MM.get k m
    let add k a m = MM.add k (nub @$ a:: get k m) m
    let remove k a m = MM.add k (ExtList.delete a (get k m)) m
    let keys = MM.keys 
    let values t =
      let l = ref [] in
      !(fold (fun _ v l -> addRefList v l;l) t l)
  end
end

module Heap = struct
  (**************************************************************************)
  (*                                                                        *)
  (*  Ocamlgraph: a generic graph library for OCaml                         *)
  (*  Copyright (C) 2004-2008                                               *)
  (*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
  (*                                                                        *)
  (*  This software is free software; you can redistribute it and/or        *)
  (*  modify it under the terms of the GNU Library General Public           *)
  (*  License version 2.1, with the special exception on linking            *)
  (*  described in file LICENSE.                                            *)
  (*                                                                        *)
  (*  This software is distributed in the hope that it will be useful,      *)
  (*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
  (*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
  (*                                                                        *)
  (**************************************************************************)
  
  (* $Id:$ *)
  
  module type Ordered = sig
    type t
    val compare : t -> t -> int
  end
  
  exception EmptyHeap
  
  (* s Imperative implementation *)
  
  module Imperative(X : Ordered) = struct
    
    (* The heap is encoded in the array [data], where elements are stored from *)
    (* [0] to [size - 1]. From an element stored at [i], the left (resp.       *)
    (* right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]).           *)
    
    type t = { mutable size : int; mutable data : X.t array }
    
    (* When [create n] is called, we cannot allocate the array, since there is *)
    (* no known value of type [X.t]; we'll wait for the first addition to do   *)
    (* it, and we remember this situation with a negative size.                *)
    
    let create n =
      if n <= 0 then invalid_arg "create";
      { size = - n; data = [||] }
    
    let is_empty h = h.size <= 0
    
    (* [resize] doubles the size of [data] *)
    
    let resize h =
      let n = h.size in
      assert (n > 0);
      let n' = 2 * n in
      let d = h.data in
      let d' = Array.create n' d.(0) in
      Array.blit d 0 d' 0 n;
      h.data <- d'
    
    let add h x =
      (* first addition: we allocate the array *)
      if h.size < 0 then begin
        h.data <- Array.create (- h.size) x; h.size <- 0
      end;
      let n = h.size in
      (* resizing if needed *)
      if n == Array.length h.data then resize h;
      let d = h.data in
      (* moving [x] up in the heap *)
      let rec moveup i =
        let fi = (i - 1) / 2 in
        if i > 0 && X.compare d.(fi) x < 0 then begin
          d.(i) <- d.(fi);
          moveup fi
        end else
          d.(i) <- x
      in
      moveup n;
      h.size <- n + 1
    
    let maximum h =
      if h.size <= 0 then raise EmptyHeap;
      h.data.(0)
    
    let remove h =
      if h.size <= 0 then raise EmptyHeap;
      let n = h.size - 1 in
      h.size <- n;
      let d = h.data in
      let x = d.(n) in
      (* moving [x] down in the heap *)
      let rec movedown i =
        let j = 2 * i + 1 in
        if j < n then
          let j =
            let j' = j + 1 in
            if j' < n && X.compare d.(j') d.(j) > 0 then j' else j
          in
          if X.compare d.(j) x > 0 then begin
            d.(i) <- d.(j);
            movedown j
          end else
            d.(i) <- x
        else
          d.(i) <- x
      in
      movedown 0
    
    let pop_maximum h = let m = maximum h in remove h; m
    
    let iter f h =
      let d = h.data in
      for i = 0 to h.size - 1 do f d.(i) done
    
    let fold f h x0 =
      let n = h.size in
      let d = h.data in
      let rec foldrec x i =
        if i >= n then x else foldrec (f d.(i) x) (succ i)
      in
      foldrec x0 0
    let size h = h.size 
  end
  
  (* s Functional implementation *)
  
  module type FunctionalSig = sig
    type elt
    type t
    val empty : t
    val add : elt -> t -> t
    val maximum : t -> elt
    val remove : t -> t
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
  
  module Functional(X : Ordered) = struct
    
    (* Heaps are encoded as complete binary trees, i.e., binary trees which    *)
    (* are full expect, may be, on the bottom level where it is filled from    *)
    (* the left. These trees also enjoy the heap property, namely the value of *)
    (* any node is greater or equal than those of its left and right subtrees. *)
    (* There are 4 kinds of complete binary trees, denoted by 4 constructors:  *)
    (* [FFF] for a full binary tree (and thus 2 full subtrees); [PPF] for a    *)
    (* partial tree with a partial left subtree and a full right subtree;      *)
    (* [PFF] for a partial tree with a full left subtree and a full right      *)
    (* subtree (but of different heights); and [PFP] for a partial tree with a *)
    (* full left subtree and a partial right subtree.                          *)
    
    type elt = X.t
    
    type t =
      | Empty
      | FFF of t * X.t * t (* full    (full,    full) *)
      | PPF of t * X.t * t (* partial (partial, full) *)
      | PFF of t * X.t * t (* partial (full,    full) *)
      | PFP of t * X.t * t (* partial (full,    partial) *)
    
    let empty = Empty
    
    (* smart constructors for insertion *)
    let p_f l x r = match l with
      | Empty | FFF _ -> PFF (l, x, r)
      | _ -> PPF (l, x, r)
    
    let pf_ l x = function
      | Empty | FFF _ as r -> FFF (l, x, r)
      | r -> PFP (l, x, r)
    
    let rec add x = function
      | Empty ->
          FFF (Empty, x, Empty)
      (* insertion to the left *)
      | FFF (l, y, r) | PPF (l, y, r) ->
          if X.compare x y > 0 then p_f (add y l) x r else p_f (add x l) y r
      (* insertion to the right *)
      | PFF (l, y, r) | PFP (l, y, r) ->
          if X.compare x y > 0 then pf_ l x (add y r) else pf_ l y (add x r)
    
    let maximum = function
      | Empty -> raise EmptyHeap
      | FFF (_, x, _) | PPF (_, x, _) | PFF (_, x, _) | PFP (_, x, _) -> x
    
    (* smart constructors for removal; note that they are different from the   *)
    (* ones for insertion!                                                     *)
    let p_f l x r = match l with
      | Empty | FFF _ -> FFF (l, x, r)
      | _ -> PPF (l, x, r)
    
    let pf_ l x = function
      | Empty | FFF _ as r -> PFF (l, x, r)
      | r -> PFP (l, x, r)
    
    let rec remove = function
      | Empty ->
          raise EmptyHeap
      | FFF (Empty, _, Empty) ->
          Empty
      | PFF (l, _, Empty) ->
          l
      (* remove on the left *)
      | PPF (l, x, r) | PFF (l, x, r) ->
          let xl = maximum l in
          let xr = maximum r in
          let l' = remove l in
          if X.compare xl xr >= 0 then
            p_f l' xl r
          else
            p_f l' xr (add xl (remove r))
      (* remove on the right *)
      | FFF (l, x, r) | PFP (l, x, r) ->
          let xl = maximum l in
          let xr = maximum r in
          let r' = remove r in
          if X.compare xl xr > 0 then
            pf_ (add xr (remove l)) xl r'
          else
            pf_ l xr r'
    
    let rec iter f = function
      | Empty ->
          ()
      | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) ->
          iter f l; f x; iter f r
    
    let rec fold f h x0 = match h with
      | Empty ->
          x0
      | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) ->
          fold f l (fold f r (f x x0))
    
  end
  
end

module Weaktbl = struct
  (***********************************************************************)
  (*                                                                     *)
  (*                            Weaktbl                                  *)
  (*                                                                     *)
  (*             (C) 2007 by Zheng Li (li@pps.jussieu.fr)                *)
  (*                                                                     *)
  (*  This program is free software; you can redistribute it and/or      *)
  (*  modify it under the terms of the GNU Lesser General Public         *)
  (*  License version 2.1 as published by the Free Software Foundation,  *)
  (*  with the special exception on linking described in file LICENSE.   *)
  (*                                                                     *)
  (*  This program is distributed in the hope that it will be useful,    *)
  (*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
  (*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
  (*  GNU Library General Public License for more details.               *)
  (*                                                                     *)
  (***********************************************************************)
  
  (* weak stack, for ordering purpose *)
  module Stack = struct
    type 'a t = { mutable data:'a Weak.t; mutable length: int; mutable cursor: int }
    let create n =
      let len = min n (Sys.max_array_length - 1) in
      { data = Weak.create len; length = len; cursor = 0 }
    let iter f s =
      for i = s.cursor - 1 downto 0 do
        match Weak.get s.data i with Some x -> f x | _ -> ()
      done
    let length s = (* resize by the way, since it's invoked by push *)
      let flag = ref false and pt = ref 0 in
      for i = 0 to s.cursor - 1 do
        match Weak.get s.data i with
        | Some x as d -> if !flag then Weak.set s.data !pt d; incr pt
        | None -> flag := true
      done;
      s.cursor <- !pt; s.cursor
    let copy s =
      let s' = create s.length in
      Weak.blit s.data 0 s'.data 0 s.cursor; s'.cursor <- s.cursor; s'
    let rec push x s =
      if s.cursor < s.length then
        (Weak.set s.data s.cursor (Some x); s.cursor <- s.cursor + 1)
      else
        let len = length s in
        if len >= s.length / 3 && len < s.length * 2 / 3 then push x s else
          let len' = min (len * 3 / 2 + 2) (Sys.max_array_length - 1) in
          if len' = len then failwith "Weaktbl.Stack.push: stack cannnot grow"
          else
            let data' = Weak.create len' in
            Weak.blit s.data 0 data' 0 s.cursor;
            s.data <- data'; s.length <- len'; push x s
    let rec pop s =
      if s.cursor <= 0 then raise Not_found;
      s.cursor <- s.cursor - 1;
      match Weak.get s.data s.cursor with Some x -> x | None -> pop s
    let rec top s =
      if s.cursor <= 0 then raise Not_found;
      match Weak.get s.data (s.cursor - 1) with
      | Some x -> x | None -> s.cursor <- s.cursor - 1; top s
    let is_empty s = (* stop as earlier as we can *)
      try iter (fun _ -> raise Not_found) s; true with Not_found -> false
  end
  
  open Obj (* Recover polymorphism from standard monomorphic (Weak)Hashtbl *)
  module Make (H: Hashtbl.HashedType) : Hashtbl.S with type key = H.t = struct
    type box = H.t Weak.t
    let enbox k = let w = Weak.create 1 in Weak.set w 0 (Some k); w
    let unbox bk = Weak.get bk 0
    type bind = box * t
    let bind_new k v = enbox k, repr v
    type cls = bind Stack.t
    let cls_new bd = let cls = Stack.create 1 in Stack.push bd cls; cls
    let dummy k = cls_new (bind_new k ())
    let rec top_bind cls =
      let (bk, v) as bind = Stack.top cls in
      match unbox bk with
      | Some k -> k, (obj v) | _ -> assert (bind == Stack.pop cls); top_bind cls
    let top_key cls = fst (top_bind cls) and top_value cls = snd (top_bind cls)
    let all_bind cls =
      let l = ref [] in
      let f (bk, v) = match unbox bk with
        | Some k -> l := (k, obj v) :: !l | _ -> () in
      Stack.iter f cls; List.rev !l
    let all_key cls = List.map fst (all_bind cls)
    and all_value cls = List.map snd (all_bind cls)
    module HX = struct
      type t = cls
      let hash x = try H.hash (top_key x) with Not_found -> 0
      let equal x y = try H.equal (top_key x) (top_key y) with Not_found -> false
    end
    module W = Weak.Make(HX)
    type key = H.t and 'a t = W.t
    let create = W.create and clear = W.clear
    let find_all tbl key =
      try all_value (W.find tbl (dummy key)) with Not_found -> []
    let rec find tbl key = top_value (W.find tbl (dummy key))
    let add tbl key data =
      let bd = bind_new key data in
      let cls =
        try let c = W.find tbl (dummy key) in Stack.push bd c; c
        with Not_found -> let c = cls_new bd in W.add tbl c; c in
      let final _ = ignore bd; ignore cls in
      try Gc.finalise final key
      with Invalid_argument _ -> Gc.finalise final bd; Gc.finalise final cls
    let remove tbl key =
      try ignore (Stack.pop (W.find tbl (dummy key))) with Not_found -> ()
    let replace tbl key data = remove tbl key; add tbl key data
    let mem tbl key = try ignore (find tbl key); true with Not_found -> false
    let iter f tbl =
      let f' (bk, v) = match unbox bk with Some k -> f k (obj v) | None -> () in
      W.iter (Stack.iter f') tbl
    let fold f tbl accu =
      let r = ref accu in
      let f' k v = r := f k v !r in
      iter f' tbl; !r
    let length tbl = W.fold (fun cls -> (+) (Stack.length cls)) tbl 0
    let copy tbl =
      let tbl'= W.create (W.count tbl * 3 / 2 + 2) in
      W.iter (fun cls -> W.add tbl' (Stack.copy cls)) tbl; tbl'
  end
  
  module StdHash = Make
    (struct
      type t = Obj.t let equal x y = (compare x y) = 0 let hash = Hashtbl.hash
    end)
  open StdHash
  type ('a,'b) t = 'b StdHash.t
  let create = create and clear = clear and copy = copy and length = length
  let add tbl k = add tbl (repr k)
  let remove tbl k = remove tbl (repr k)
  let find tbl k = find tbl (repr k)
  let find_all tbl k = find_all tbl (repr k)
  let replace tbl k = replace tbl (repr k)
  let mem tbl k = mem tbl (repr k)
  let iter f = iter (fun k d -> f (obj k) d)
  let fold f = fold (fun k d a -> f (obj k) d a)
end


module Cache = struct
  module T = Weaktbl
  let memo f =
    let t = T.create 3 in
    fun i ->
        try T.find t i with Not_found ->
            let r = f i in
            T.add t i r;
            r
  let memo_rec f =
    let t = T.create 3 in
    let rec g i =
      try T.find t i with Not_found ->
        let r = f g i in
        T.add t i r;
        r in
     g
    
end

module Toys = struct
  let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)
  let rec fibcps n k = if n <= 1 then k 1 else
      fibcps (n - 1) @$ fun x ->
          fibcps (n - 2) @$ fun x' -> k (x + x')
  let rec fix f x = f (fix f) x
  
  let facFix f = function
      0 -> 1
    | x -> x * f (x - 1)
  let fibFix f = function
    | 0 -> 1
    | 1 -> 1
    | x -> f (x-1) + f (x-2)

  let fib_memo = Cache.memo_rec fibFix
(*  let _ = (fix fact) 5 (* evaluates to "120" *)*)
end

module Table = struct
  type 'a element = Atom of 'a | Table of 'a t
  and 'a t = 'a element list
  let rec show sh t = show_list (showE sh) t
  and showE sh = function
      | Atom e -> sh e
      | Table t -> show sh t
  let atom x = Atom x
  let consT l t = Table (map atom l) :: t
  let cons x t = atom x :: t
  let empty = []
  let flatten t =
      let rec work acc = function
          | [] -> List.rev acc
          | Table l :: xs -> work (List.rev l@acc) xs
          | x :: xs -> work (x::acc) xs in
      work [] t
end

(*Okasaki's Random Access Lists by Matias Giovannini*)
module Vector : sig
  type 'a t
  val nil : 'a t
  val cons : 'a -> 'a t -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val length : 'a t -> int
  val index : int -> 'a t -> 'a
  val update : int -> 'a -> 'a t -> 'a t
end = struct
  type 'a t = Nil | Zero of ('a * 'a) t | One  of 'a * ('a * 'a) t

  let nil = Nil

  type cons = { cons : 'a . 'a -> 'a t -> 'a t }
  let cons =
    let rec cons = { cons = fun x l -> match l with
    | Nil         -> One (x, Nil)
    | Zero    ps  -> One (x, ps)
    | One (y, ps) -> Zero (cons.cons (x, y) ps)
    } in cons.cons

  type uncons = { uncons : 'a . 'a t -> 'a * 'a t }
  let uncons =
    let rec uncons = { uncons = function
    | Nil          -> failwith "uncons"
    | One (x, Nil) -> (x, Nil)
    | One (x, ps ) -> (x, Zero ps)
    | Zero    ps   ->
      let ((x, y), ps) = uncons.uncons ps in (x, One (y, ps))
    } in uncons.uncons

  let head l = fst (uncons l)
  and tail l = snd (uncons l)

  type length = { length : 'a . 'a t -> int }
  let length v =
    let rec length = { length = function
    | Nil         -> 0
    | One (_, ps) -> 1 + length.length (Zero ps)
    | Zero    ps  -> 2 * length.length ps
    } in length.length v

  type index = { index : 'a . int -> 'a t -> 'a }
  let index n =
    let rec index = { index = fun n l -> match l with
    | Nil                    -> failwith "index"
    | One (x, ps) when n = 0 -> x
    | One (x, ps)            -> index.index (n - 1) (Zero ps)
    | Zero    ps             ->
      let (l, r) = index.index (n / 2) ps in
      if n mod 2 = 0 then l else r
    } in index.index n

  type update = { update : 'a . int -> ('a -> 'a) -> 'a t -> 'a t }
  let update n e =
    let rec update = { update = fun n f l -> match l with
    | Nil                    -> failwith "update"
    | One (x, ps) when n = 0 -> One (f x, ps)
    | One (x, ps)            -> cons x (update.update (pred n) f (Zero ps))
    | Zero ps                ->
      let g (x, y) = if n mod 2 = 0 then (f x, y) else (x, f y) in
      Zero (update.update (n / 2) g ps)
    } in update.update n (fun _ -> e)
end

module Command = struct
  let concat a b =
    if ExtString.isBlankString a then b else
    if ExtString.isBlankString b then a else a ^ " && " ^ b
  let (<+>) = concat
end

module Loongson = struct
  module Compile = struct
    open Command
    let mylgcc2 = "loongcc -O3 -Wb,-WOPT:use_ld_st_offset=1 -static -loongson2f -Wb,-CG:float_use_madd -CG:use_loongson2e_multdivmod "
    let mylgcc = mylgcc2 <+> "-ipa"
    let stgcc = "mips64el-st-linux-gnu-gcc -O3 -march=loongson2f -mtune=loongson2f -static"
  end
end

module MultiSet = struct
  module Make(S: Set.OrderedType) = struct
    module M = Map.Make(S)
    type t = int M.t
    let empty = M.empty
    let count k t = try M.find k t with Not_found -> 0
    let mem k t = count k t > 0
    let add k t = M.add k (succ @$ count k t) t
    let remove k t =
      if count k t < 1 then raise Not_found else
        M.add k (pred (count k t)) t
    let iter f t =
      M.iter (fun k v -> List.iter (fun _ -> f k) @$ ExtList.range 0 v) t
    let fold f l t =
      M.fold (fun k v l -> f k v l) t l
    let to_list t =
      let l = ref [] in
      !(fold (fun k v l -> l := ExtList.replicate v k @ !l; l) l t)
    let of_list l =
      List.fold_left (flip add) empty l
    let map f t =
      let m = ref M.empty in
      M.iter (fun k v -> m := M.add (f k) v !m) t;
      !m
  end
end

module LazyList 
(*  :sig                                        *)
(*    type 'a stream                            *)
(*    type 'a t                                 *)
(*    val mfail : 'a t                          *)
(*    val return : 'a -> 'a t                   *)
(*    val mplus : 'a t -> 'a t -> 'a t          *)
(*    val bind : 'a t -> ('a -> 'b t) -> 'b t   *)
(*    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t*)
(*    val guard : bool -> unit t                *)
(*    val take : int -> 'a t -> 'a list         *)
(*    val drop : int -> 'a t -> 'a t            *)
(*    val enum : int -> int -> int t            *)
(*(*    val numb : unit -> int stream*)         *)
(*  end                                         *)
= struct
  (* test non-determinism monad, the simplest possible implementation *)
  type 'a stream =
    | Nil
    | Cons of 'a * (unit -> 'a stream)
    | InC of (unit -> 'a stream)
  type 'a t = unit -> 'a stream
  module Monad = struct
  let mfail = fun () -> Nil
  let return a = fun () -> Cons (a, mfail)
  
  let mzero = fun () -> Nil
  
  (* actually, interleave: a fair disjunction with breadth-first search *)
  let rec mplusNonDet a b = fun () -> match a () with
        | Nil -> InC b
        | InC a -> (match b () with
              | Nil -> InC a
              | InC b -> InC (mplusNonDet a b)
              | Cons (b1, b2) -> Cons (b1, (mplusNonDet a b2)))
        | Cons (a1, a2) -> Cons (a1, (mplusNonDet b a2))
  let rec mplus a b = fun () -> match a () with
    | Nil -> InC b
    | Cons(a1,a2) -> Cons(a1,mplus a2 b)
    | InC a -> InC (mplus a b)

  let rec bind m f = fun () -> match m () with
        | Nil -> mfail ()
        | InC a -> InC (bind a f)
        | Cons (a, b) -> mplus (f a) (bind b f) ()
  
  let (>>=) = bind
  let guard be = if be then return () else mfail  
  end
  open Monad
  let empty = mzero
  let singleton e = fun () -> Cons(e,empty)
  let null m = match m () with
    | Nil -> true
    | _ -> false
  let cons x m = fun () -> Cons(x,m)
  let rec filter f m = match m () with
    | Nil -> fun () -> Nil
    | InC a -> filter f a
    | Cons(a,b) ->
      fun () -> 
        if f a then Cons(a,(filter f b)) else InC (filter f b) 
  let rec map f m = match m () with
    | Nil -> fun () -> Nil
    | InC a -> map f a
    | Cons(a,b) -> fun () -> Cons(f a,map f b) 
  let rec take n m = if n <= 0 then [] else
      match m () with
      | Nil -> []
      | InC a -> take n a
      | Cons (a, b) -> (a:: take (n - 1) b)
  let rec drop n m = if n<=0 then m else
    match m () with
      | Nil -> fun () -> Nil
      | InC a -> drop n a
      | Cons(_,b) -> drop (n-1) b 
  let rec headTail m = match m () with
    | Nil -> failwith "headTail"
    | InC a -> headTail a
    | Cons(a,b) -> a,b

  let head m = fst (headTail m)
  let tail m = snd (headTail m)
  let rec enum i inc = mplus (return i) (fun () -> InC (enum (i+inc) inc))
  let rec to_list m = match m () with
    | Nil -> []
    | InC a -> to_list a
    | Cons(a,b) -> a::to_list b
  let rec of_list = function
      | [] -> fun () -> Nil
      | x::xs -> fun () -> Cons(x,of_list xs) 
  let rec cycle l =
    mplus (of_list l) (fun () -> InC (cycle l))
  let repeat e = cycle [e]
  let funPowers f i =
    let rec work i =
      mplus (return i) (fun () -> InC (work (f i))) in
    work i 
  let rev s = of_list @$ List.rev @$ to_list s
  let span f s =
    let rec work acc s =
      if null s then rev acc,s else
           let x,xs = headTail s in
           if f x then work (fun () -> Cons(x,acc)) xs else rev acc,s in
    work empty s
  let takeWhile f s = fst @$ span f s
  let dropWhile f s = snd @$ span f s
  let rec force m = match m () with
    | Nil -> Nil
    | Cons (a,b) -> Cons(a,b)
    | InC a -> force a
  let mergeSortedBy cmp m m' =
    let rec work m m' =  match m, m' with
    | Nil,Nil -> fun () -> Nil
    | a, Nil
    | Nil, a -> fun () -> a
    | InC a,_ -> work (a ()) m'
    | _,InC a' -> work m (a' ())
    | Cons(a,b),Cons(a',b') ->
      let r = cmp a a' in
      if r<0 then fun () -> Cons (a,work (InC b) m') else
        if r>0 then fun () -> Cons (a',work m (InC b')) else
          fun () -> Cons (a,work (InC b) (InC b')) in
    work (m ()) (m' ())
(*  let foldl f i m =*)
  let scanl f i m =
    let rec work i m = 
        match m () with
        | Nil -> return i
        | InC a -> work i a
      | Cons(a,b) -> 
        fun () -> Cons (i,work (f i a) b) in
    work i m 
  let scanl1 f m =
    let x,xs = headTail m in
    scanl f x xs
  let diffSortedBy cmp s s' =
    let rec work s s' =
        if null s then mzero else
          if null s' then s else
          let x,xs = headTail s in
          let y,ys = headTail s' in
          let r = cmp x y in
        if r=0 then work xs ys else
          if r<0 then fun () -> Cons(x,work xs s') else
            work s ys in
    work s s' 
   

  let prepend l s =
    let rec work = function
      | [] -> s
      | x::xs -> fun () -> Cons(x,(work xs)) in
    work l
  let rec concat s =
    if null s then mzero else
      let x,xs = headTail s in
      prepend x (fun () -> InC (concat xs))
  let concatMap f s =
    concat @$ map f s   
     
    let wheel =
      let seed = [2; 3; 5; 7; 11; 13; 17] in
      let m = product seed in
      let trim p l = List.filter (fun x -> x mod p <> 0) l in
      let ws = ExtList.foldr trim (ExtList.range 1 (m + 1)) seed in
      concatMap (fun i -> List.map ((+) i) ws) @$ map (( * ) m) (enum 0 1)
   
  (* merging a list of sorted lists, with their heads ascending*)
(*  let venturi mm =*)
    
(*  let qsort s =                                                                                      *)
(*    if null s then mzero else                                                                        *)
(*      let x,xs = headTail s in                                                                       *)
(*      mplus (mplus (filter (flip (<) x) xs) (of_list [x])) (fun () -> InC (filter (flip (>) x) xs))  *)
  module Venturi = struct
      type 'a people =
        | Vip of 'a * 'a people
        | Crowd of 'a t
    
      let venturiBy ?(noDup = true) cmp l =
        let of_lazylist s =
            if null s then Crowd mzero else
                let x, xs = headTail s in
                Vip(x, Crowd xs) in 
        let rec to_lazylist = function
            | Vip(x, xs) -> fun () -> Cons(x, to_lazylist xs)
            | Crowd l -> l in
        let merge l l' = mergeSortedBy cmp l l' in
        let rec merge' p p' = match p, p' with
            | Vip(x, xt), ys -> Vip(x, merge' xt ys)
            | Crowd xs, Crowd ys -> Crowd (merge xs ys)
            | Crowd xs, Vip(y, yt) ->
                    if null xs then Vip(y, yt) else
                        let x, xt = headTail xs in
                        if noDup then
                            let r = cmp x y in
                            if r = 0 then Vip (x, merge'(Crowd xt) yt) else
                            if r < 0 then Vip(x, merge' (Crowd xt) p') else
                                Vip(y, merge' p yt)
                        else
                        if cmp x y <= 0 then Vip(x, merge' (Crowd xt) p') else
                            Vip(y, merge' p yt)
        in
        to_lazylist @$ ExtList.foldl1 merge' (List.map of_lazylist l)
    end
    module Ventur2 = struct
      type 'a people =
        | Vip of 'a * (unit -> 'a people)
        | Crowd of 'a t
      
      let venturiBy ?(noDup = true) cmp l =
        let of_lazylist s =
          if null s then Crowd mzero else
            let x, xs = headTail s in
            Vip(x, fun () -> Crowd xs) in
        let rec to_lazylist = function
          | Vip(x, xs) -> fun () -> Cons(x, to_lazylist @$ xs ())
          | Crowd l -> l in
        let merge l l' = mergeSortedBy cmp l l' in
        (* let rec merge' p p' = match p, p' with | Vip(x, xt), ys -> Vip(x, fun   *)
        (* () -> merge' (xt ()) ys) | Crowd xs, Crowd ys -> Crowd (merge xs ys) |  *)
        (* Crowd xs, Vip(y, yt) -> if null xs then Vip(y, yt) else let x, xt =     *)
        (* headTail xs in if noDup then let r = cmp x y in if r = 0 then Vip (x,   *)
        (* fun () -> merge'(Crowd xt) (yt ())) else if r < 0 then Vip(x, fun () -> *)
        (* merge' (Crowd xt) p') else Vip(y, fun () -> merge' p (yt ())) else if   *)
        (* cmp x y <= 0 then Vip(x, fun () -> merge' (Crowd xt) p') else Vip(y,    *)
        (* fun () -> merge' p (yt ())) in                                          *)
        let rec mergeOneOther p ps = match p with
          | Vip(x, xt) -> Vip(x, fun () -> mergeOneOther (xt ()) ps)
          | Crowd xs ->
              let p' = mergeAll ps in
              match p' with
              | Crowd ys -> Crowd (merge xs ys)
              | Vip (y, yt) ->
                  let x, xt = headTail xs in
                  if noDup then
                    let r = cmp x y in
                    if r = 0 then Vip (x, fun () -> merge'(Crowd xt) (yt ())) else
                    if r < 0 then Vip(x, fun () -> merge' (Crowd xt) p') else
                      Vip(y, fun () -> merge' p (yt ()))
                  else
                  if cmp x y <= 0 then Vip(x, fun () -> merge' (Crowd xt) p') else
                    Vip(y, fun () -> merge' p (yt ()))
        and mergeAll (ps: unit -> 'a people stream) : 'a people =
          let p, ps' = headTail ps in
          mergeOneOther p ps'
        and merge' p p' = mergeAll (of_list [p; p']) in
        to_lazylist @$ mergeAll (map of_lazylist l)
          
    end
  let ofFile fn =
      let fdesc = Unix.openfile fn [Unix.O_RDONLY] 0o400 in
      let a = Bigarray.Array1.map_file fdesc Bigarray.char Bigarray.c_layout false (-1) in
      let n = Bigarray.Array1.dim a in
      let rec work i =
        if i>=n then empty else
          fun () -> Cons (Bigarray.Array1.get a i, work (succ i)) in
      work 0  
    
    let packBy startNew l =
        let addPack pack s =
          if pack==[] then s else fun () -> Cons(List.rev pack, s) in
        let rec work pack s =
              if null s then addPack pack empty else
                let x,xs = headTail s in
                    if startNew x then
                      addPack pack @$ fun () -> InC(work [x] xs) 
                    else work (x::pack) xs
        in work [] l 
    
  let tokenize separators blanks l =
    let isSeparator x = HashSet.mem (HashSet.of_list separators) x in
    let isBlank x = HashSet.mem (HashSet.of_list blanks) x in
    let addPack pack s =
      if pack==[] then s else fun () -> Cons(List.rev pack, s) in
    let rec work pack s =
          if null s then addPack pack empty else
            let x,xs = headTail s in
                if isSeparator x then
                  addPack pack @$ addPack [x] @$ fun () -> InC(work [] xs)
                else if isBlank x then  
                  addPack pack @$ fun () -> InC(work [] xs)
                else work (x::pack) xs
    in map (ExtString.of_list) @$ work [] l    
end

module LazyListMonad = LazyList.Monad


module PartialOrder = struct
  open ExtList
  open MaybeMonad
  module M = ListMap.Make(struct
      type t = int
      let compare = compare
    end) 
  type partialOrder = int M.t
  type t = partialOrder
  
  let findlist f a b = try f a b with Not_found -> []
  let of_list l =
    foldr (fun (a, b) m -> M.add a b m) M.empty l
  let to_list t =
    let l = ref [] in
    !(M.fold (fun k a l -> addRefList (k, a) l; l) t l)
  let rev t =
    of_list (map HomPair.rev (to_list t))
  let elements s = nub @$ M.values s @ M.keys s
  let topoSort t l =
    let rec work acc preds l =
      if null l then return @$ List.rev acc else
        findFirst (fun k -> if M.get k preds = [] then return k else mzero) l >>= fun rt ->
            work (rt:: acc) (foldr (fun k m -> M.remove k rt m) preds (M.get rt t)) (delete rt l) in
    work [] (rev t) l
  let succs k t = M.get k t
  
  let show t =
    show_list (show_pair string_of_int string_of_int) (to_list t)
  module TransitiveClosure
  : sig
    type t
    val ofPartialOrder : partialOrder -> t option
    val toPartialOrder : t -> partialOrder
    val show : t -> string
  end
  = struct
    type t = partialOrder
    let ofPartialOrder t =
      topoSort t (elements t) >>= fun l ->
          return @$ foldr (fun k m ->
                  foldr (fun k' m -> foldr (M.add k) m (succs k' m)) m (succs k m)) t l
    let toPartialOrder = id
    let show = show
  end
    
    let distance l l' =
      let posmap l = IntMap.of_list @$ List.rev @$ mapi pair l in 
      let pm,pm' = tmap posmap (l,l') in
      let n = length l in
      IntList.sum @$ map (fun i -> Algebra.Int.sqr (IntMap.find i pm - IntMap.get i pm')) (range 0 n)

  let random tc l =
      let t = TransitiveClosure.toPartialOrder tc in
      let rec work acc preds l =
        if null l then return @$ List.rev acc else
          ExtList.random @$ filter (fun k -> M.get k preds = []) l >>= fun rt ->
            work (rt::acc) (foldr (fun k m -> M.remove k rt m) preds (succs rt t)) (delete rt l) in
      work [] (rev t) l
  let mutate tc p l =
    let t = TransitiveClosure.toPartialOrder tc in
    let rec work acc preds l =
      if null l then return @$ List.rev acc else
        let rt = if Random.float 1.0 <p then 
          fromSome @$ ExtList.random @$ filter (fun k -> M.get k preds = []) l else hd l in
        work (rt::acc) (foldr (fun k m -> M.remove k rt m) preds (succs rt t)) (delete rt l) in
      work [] (rev t) l
(*  open ListMonad                                                                         *)
(*  let legalPermutations t l =                                                            *)
(*    let rec work preds l =                                                               *)
(*      if null l then [[]] else                                                           *)
(*        filter (fun k -> M.get k preds = []) l >>= fun rt ->                             *)
(*            map (cons rt) @$                                                             *)
(*            work (foldr (fun k m -> M.remove k rt m) preds (succs rt t)) (delete rt l) in*)
(*    work (rev t) l                                                                       *)

(*  open LazyList*)
  open LazyListMonad
  let legalPermutations t l =
    let rec work preds l =
      if ExtList.null l then return mzero else
        LazyList.filter (fun k -> M.get k preds = []) (LazyList.of_list l) >>= fun rt ->
            LazyList.map (LazyList.cons rt) @$
                work (foldr (fun k m -> M.remove k rt m) preds (succs rt t)) (delete rt l) in
    LazyList.map LazyList.to_list @$ work (rev t) l
    
  open Array
  let crossover a =
    let m = length a in
    let n = List.length a.(0) in
    let rec work i acc a =
      if i = n then List.rev acc else
        let e = List.hd (a.(Random.int m)) in
        work (i+1) (e::acc) (map (ExtList.delete e) a) in
    work 0 [] a
  let crossover2 t t' =
    crossover [|t;t'|]
end

module IO
: sig
  type 'a t = unit -> 'a
  val run : 'a t -> 'a
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t
  val sequence : 'a t list -> 'a list t
  val replicateM : int -> 'a t -> 'a list t
(*  val printf : ('a, out_channel, unit) format -> 'a t*)
(*  val return : 'a -> 'a t*)
end
= struct
  type 'a t = unit -> 'a
  let run f = f ()
  let bind m k = fun () -> k (run m) ()
  let (>>=) = bind
  let (>>) m m' = m >>= fun _ -> m' 
(*  let return a = fun () -> a*)
  let sequence ms =
    let k m m' =
      m >>= fun x ->
          m' >>= fun xs ->
              fun () -> (x:: xs) in
    List.fold_right k ms (fun () -> [])
  let replicateM n m =
    let replicate n e =
      let rec loop stk i = if i = 0 then stk else loop (e:: stk) (i - 1) in
      loop [] n in
    sequence (replicate n m)
(* let printf fmt = fun () -> Printf.printf fmt*)
end
module IntCounter = Counter(Int)

module TLattice = struct
    type 'a t =
      | Top
      | Val of 'a
      | Bottom
  let join ?(eq = (=)) ?(select = fst) t t' = match t, t' with
    | _, Top
    | Top, _ -> Top
    | Bottom, x
    | x, Bottom -> x
    | Val e, Val e' -> if eq e e' then Val (select (e, e')) else Top 
  let meet ?(eq = (=)) ?(select = fst) t t' = match t, t' with
    | Bottom, _
    | _, Bottom -> Bottom
    | Top, x
    | x, Top -> x
    | Val e, Val e' -> if eq e e' then Val (select (e, e')) else Bottom
  let make e = Val e
  let bottom = Bottom
  let top = Top
end

module type SHOWEQ = sig
  type t
  val show : t -> string
  val eq : t -> t -> bool
  end
  
module Parsec
(*:sig                                   *)
(*  type 'a t                            *)
(*  val mplus : 'a t -> 'a t -> 'a t     *)
(*  val fmap : ('a -> 'b) -> 'a t -> 'b t*)
(*(*  val mfail : 'a t*)                 *)
(*  end                                  *)
= struct
  open Either
  open EitherMonad
  open StateMonad
  type token = string
  type state = token list
  type 'a result = (state, 'a) Either.t 
  type 'a t = (state, 'a result) StateMonad.t
   
  let (<|>) p p' s =
    let a,s' = p s in
      match a with 
        | Left _ -> p' s
        | Right _ -> a,s'
  let fmap f p =
    StateMonad.fmap (function Left s -> Left s | Right x -> Right (f x)) p

  let mplus = (<|>)
  let mfail l =
    runState (get >>= fun s ->
      return (Left s)) l  
  let rec zeroOrMore p =
    oneOrMore p <|> return (Right [])
  and oneOrMore p =
    p >>= fun a -> match a with
      | Left s -> mfail
      | Right _ ->
          zeroOrMore p >>= fun es ->
            return (EitherMonad.liftM2 cons a es)
  let rec zeroOrMoreWithSep pSep p =
    oneOrMoreWithSep pSep p <|> return (Right [])
  and oneOrMoreWithSep pSep p =
    p >>= fun a -> match a with
      | Left s -> mfail
      | Right e ->
        pSep >>= fun a' ->
          match a' with
            | Left _ -> return (Right [e])
            | Right _ ->
              oneOrMoreWithSep pSep p >>= fun es ->
                return (EitherMonad.liftM2 cons a es)
    let zeroOrOne p =
      fmap (fun x -> Some x) p <|> StateMonad.return (Right None)
  let pThen f p p' =
    p >>= fun a ->
    match a with
      | Left _ ->
        mfail
      | Right e ->
        p' >>= fun a' ->
        match a' with
          | Left _ ->
            mfail
          | Right e' -> return (Right (f e e'))
  let pThen3 f p p' p'' =
    pThen (fun f x -> f x) (pThen f p p') p''
  let pThen4 f p p' p'' p''' =
    pThen (fun f x -> f x) (pThen3 f p p' p'') p'''
  let pThen5 f p p' p'' p''' p''''=
    pThen (fun f x -> f x) (pThen4 f p p' p'' p''') p''''
  let pThen6 f p p' p'' p''' p'''' p'''''=
    pThen (fun f x -> f x) (pThen5 f p p' p'' p''' p'''') p'''''
  let pThen7 f p p' p'' p''' p'''' p''''' p''''''=
    pThen (fun f x -> f x) (pThen6 f p p' p'' p''' p'''' p''''') p''''''
  let pThen8 f p p' p'' p''' p'''' p''''' p'''''' p'''''''=
    pThen (fun f x -> f x) (pThen7 f p p' p'' p''' p'''' p''''' p'''''') p'''''''
  let pThen9 f p p' p'' p''' p'''' p''''' p'''''' p''''''' p''''''''=
    pThen (fun f x -> f x) (pThen8 f p p' p'' p''' p'''' p''''' p'''''' p''''''') p''''''''        
  let pThen10 f p p' p'' p''' p'''' p''''' p'''''' p''''''' p'''''''' p'''''''''=
    pThen (fun f x -> f x) (pThen9 f p p' p'' p''' p'''' p''''' p'''''' p''''''' p'''''''') p'''''''''    
  let rec sequence l = match l with
    | [] -> return (Right [])
    | p::ps ->
      pThen cons p (sequence ps)
            
end

module ParsecList = struct
  open Either
  open EitherMonad
  open StateMonad
  include Parsec
  let pSat f =
    get >>= fun s -> match s with
    | [] ->
      mfail
    | x::xs ->
      if f x then begin
        put xs >>= fun _ ->
          return (Right x)
        end
      else 
        mfail
  let pNothing s =
    (get >>= fun s -> match s with
      | [] -> return (Right [])
      | x::xs -> mfail) s
  let pAny l = pSat (const true) l
  let pLit lit = pSat ((=) lit)      
  let pNumber = pSat (fun s -> try ignore (int_of_string s);true with _ -> false)
  end
module ParsecLazyList = struct
  open LazyList  
  open Either
  open EitherMonad
  open StateMonad
  include Parsec
  let pNothing s =
    (get >>= fun s ->
      if null s then return (Right empty) else mfail) s  
  let pSat f =
    get >>= fun s ->
      if null s then mfail else
        let x,xs = headTail s in
        if f x then
          put xs >>= fun _ ->
            return (Right x)
        else mfail
  let pAny l = pSat (const true) l
  let pLit lit = pSat ((=) lit)      
  let pNumber = pSat (fun s -> try ignore (int_of_string s);true with _ -> false)
          
  end    
  
let toOrderList l =
  let h = HashMap.create 3 in
  let cnt = ref 0 in
  let add e = try ignore (HashMap.get h e) with Not_found -> HashMap.set h e !cnt;incr cnt in
  List.iter (fun (a,b) -> add a;add b) l;
  let s = HashSet.create 3 in
  List.iter (fun (a,b) -> HashSet.add s (HashMap.get h a,HashMap.get h b)) l;
  HashSet.to_list s,HashMap.rev h  
  
let transitiveClosure l =
  let h = Hashtbl.create 3 in
  let add (a,b) =
    HashMap.set h a @$ IntSet.add b (try HashMap.get h a with Not_found-> IntSet.empty) in   
  List.iter add l; 
  let changed = ref true in
  while !changed do
    changed := false;
    Hashtbl.iter (fun k v ->
      let k_succs = HashMap.get h k in
      let s = IntSet.diff v k_succs in
      if not @$ IntSet.is_empty s then begin
        IntSet.iter (fun v -> add (k,v)) s;
        changed := true
      end) h
    done;
  h
  
