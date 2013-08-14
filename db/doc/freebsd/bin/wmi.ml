#!/usr/bin/env ocaml
#use "topfind";;

Sys.set_signal Sys.sigint
  (Sys.Signal_handle (fun _signum -> exit 0));;

print_string "----------------------\n";;
print_string "1:stumpwm-ccl with MPD\n";;
print_string "2:stumpwm-clisp with MPD\n";;
print_string "3:xmonad\n";;
print_string "4:dwm\n";;
print_string "9:console\n";;
print_string "----------------------\n";;

match read_line() with
  | "1" -> Sys.command "xinit ccl &";
  | "2" -> Sys.command "xinit clisp &";
  | "3" -> Sys.command "xinit xmonad &";
  | "4" -> Sys.command "xinit dwm &";
  | "9" -> 0;
  | _ -> Sys.command "xinit &";
  

exit 0;;
