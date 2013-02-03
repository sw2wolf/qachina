open Printf
open Unix

(**
    Helper Functions
*)
(* compose functions *)
let compose f g = fun x -> f(g(x)) ;;
let (|>) x f = f x ;;
let ( |- ) f g x = g (f x);;
let flip f x y = f y x;;

let getenv s = try Sys.getenv s with Not_found -> "" ;;
let sh cmd = Sys.command cmd ;;
let q () = exit 0 ;;

let rec seq a b = if a>b then [] else a :: seq (a+1) b;;
let (--) i j = 
    let rec aux n acc =
        if n < i then acc else aux (n-1) (n :: acc)
            in aux j []
;;

let words line = Str.split (Str.regexp " +") line ;;
let lines str = Str.split (Str.regexp "\n") str ;;

let init l = l |> List.rev |> List.tl |> List.rev ;;
let md5 str = Digest.to_hex(Digest.string str) ;;

(**
    let db = Sqlite3.db_open "/media/D/www/qachina/db/qachina.db";;
    Sqlite3.db_close db;
*)
let qachina () = 
    Thread.create (fun _ -> Sys.command "cd /media/D/www/qachina; ./start.bat") ()

;;
(*
let sum x = let num = ref x in for i = 0 to 999999 do num := !num + i done; !num;;
printf "Sum(4) takes %f seconds.\n" (time_it sum 4);;
*)
let time_it action arg =
(* 计算函数运行时间 *)
    let start_time = Sys.time () in
    ignore (action arg);
    let finish_time = Sys.time () in
        finish_time -. start_time
;;

let is_leap_year ~year =
(*是否闰年*)
    if (year mod 100) = 0
    then (year mod 400) = 0
    else (year mod 4) = 0
;;

let pi = 4. *. atan 1. ;;
let e = exp 1. ;;

let random_gaussian () =
  1. +. sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. pi *. Random.float 1.)
;;

let sd ~word = Sys.command ("sdcv -n " ^ word) ;;

(**
    Stock Exchange
*)
let sxf = 0.0015 ;; (* 手续费 *)
let yhs = 0.001 ;;  (* 印花费 *)
let ghf = 1.0 ;;    (* 过户费 *)

let winG qty pb ps =
(* 算股票盈利 *)
    let obtain = (float_of_int qty) *. ps *. (1.0 -. sxf -. yhs) in
    let cost = (float_of_int qty) *. pb *. (1.0 +. sxf) +. 2.0 *. ghf in
    printf "You win: %.2f\n" (obtain -. cost)
;;

let winQ qty pb ps = 
(* 算权证盈利 *)
    let obtain = (float_of_int qty) *. ps *. (1.0 -. sxf) in
    let cost = (float_of_int qty) *. pb *. (1.0 +. sxf) +. 2.0 *. ghf in
    printf "You win: %.2f\n" (obtain -. cost)
;;

let stopLoss qty pb lossRate = 
(* 止损 *)
    let cost = (float_of_int qty) *. pb *. (1.0 +. sxf) in
    printf "Stop Loss at: %.2f\n" (pb -. (cost *. lossRate) /. (float_of_int qty));
    printf "You'll lose: %.2f\n" (cost *. lossRate)
;;

let div618 p1 p2 = 
(* 黄金分割 *)
    let ratio = [0.; 0.191; 0.236; 0.382; 0.5; 0.618; 0.809; 1.] in
    let price r = if p1 <= p2 then (p1 +. (p2 -. p1) *. r) else (p1 -. (p1 -. p2) *. r) in
    if p1 <= p2 then
        List.map (fun r -> printf "---%.3f   %.2f---\n" r (price r)) (List.rev ratio)
    else
        List.map (fun r -> printf "---%.3f   %.2f---\n" r (price r)) ratio
;;


(**
    SSQ lottery
*)
let hit_num_file = "ssqHitNum.txt";;

let writeFile ~filename:fn s =
    let oc = open_out fn in
    output_string oc s;
    close_out oc
;;

let appendFile fn s =
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 fn in
    output_string oc s;
    close_out oc
;;

let withFile fn handle = 
    let rec iter_lines fh =
        handle (input_line fh) ;
        iter_lines fh
    in
    let fh = open_in fn in
    try
        iter_lines fh
    with e ->
        (close_in fh)
;;

let good_red () =
    let a = Array.make 33 0 in
    let bump i = a.(i-1) <- a.(i-1) + 1 in
    withFile hit_num_file (fun line ->
        List.iter bump (List.map int_of_string (List.tl (words line) |> init))
    );
    let b = Array.mapi (fun i freq -> (i+1,freq)) a in
        Array.sort (fun (i1,f1) (i2,f2) -> compare f2 f1) b;
    Array.to_list(Array.map (fun x -> fst x) (Array.sub b 0 18))
;;

let sortLst l = List.sort (fun x y -> compare x y) l ;;
let set_diff s1 s2 = List.filter (fun x -> not (List.mem x s2)) s1 ;;
let lst2str l = List.fold_left (fun acc x -> acc ^ string_of_int x ^ " ") "" l ;;
let str2lst s = List.map int_of_string (words s) ;;
 
let pick_num from count = 
    assert (List.length from >= count);
    let rec pick l c acc =
        if c = 0 then sortLst acc
        else (
            let one = List.nth l (Random.int (List.length l)) in
            pick (List.filter (fun x -> x <> one) l) (c-1) (one :: acc)
        ) in
    pick from count []
;;

let win_ssq count noRed noBlue =
   (* let gr = good_red() in *)
    let noRedLst = str2lst noRed in
    let noBlueLst = str2lst noBlue in
    (* let yes_red = set_diff gr noRedLst in *)
    let yes_red = set_diff (1--33) noRedLst in
    (* let no_red = set_diff (set_diff (1--33) gr) noRedLst in *)
    let ok_blue = pick_num (set_diff (1--16) noBlueLst) count in
    let result = ref "" in
    assert (count >= 1) ;
    Random.self_init ();
    for i = 1 to count do
        result := !result ^ lst2str (sortLst 
           (* (pick_num yes_red 5 @ pick_num no_red 1) *)
           (pick_num yes_red 6)
          @ [List.nth ok_blue (i-1)]) ^ "\n";
    done;
    print_endline !result;
    writeFile "ssqNum.txt" !result
;;
(*
let hit_desc red blue = 
    match (red,blue) with
    |(6,1) -> "First"
    |(6,0) -> "Second"
    |(5,1) -> "Third(3000)"
    |(5,0) | (4,1) -> "Fourth(200)"
    |(4,0) | (3,1) -> "Fifth(10)"
    |(_,1) -> "Sixth(5)"
    |_ -> "X"
;;*)

let hit_desc  = function
    |(6,1) -> "First"
    |(6,0) -> "Second"
    |(5,1) -> "Third(3000)"
    |(5,0) | (4,1) -> "Fourth(200)"
    |(4,0) | (3,1) -> "Fifth(10)"
    |(_,1) -> "Sixth(5)"
    |_ -> "X"
;;

exception NumExist ;;

let hit_ssq no hitNum =
    let hitNumLst = str2lst hitNum in
    let hitRed redNo = List.fold_left (fun acc x -> if List.mem x (init hitNumLst) then acc+1 else acc) 0 redNo in
    let gr = good_red() in
    let hasNo = ref false in
    withFile hit_num_file (fun line ->
        if List.hd (words line) = no then (hasNo := true; raise NumExist)
    );
    if not !hasNo then appendFile hit_num_file (no ^ " " ^ hitNum ^ "\n");
    printf "Good Red Hit:%d of %d\n" (hitRed gr) (List.length gr);
    printf "------ result ------\n";
    withFile "ssqNum.txt" (fun line ->
        let numLst = str2lst line in
        assert (List.length numLst = 7);
        let hitR = hitRed (init numLst) in
        let hitB = if List.nth numLst 6 = List.nth hitNumLst 6 then 1 else 0 in
        printf "%s\t(%d:%d)\t%s\n" (lst2str numLst) hitR hitB (hit_desc (hitR, hitB))
    )
;;

let his () = sh ("tail " ^ hit_num_file) ;;

let sd word = sh ("sdcv -n " ^ word) ;;
