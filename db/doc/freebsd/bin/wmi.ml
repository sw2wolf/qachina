#! /bin/sh
# (*
exec ocaml "$0" "$@"
*)
use "topfind";;
#require "unix";;

open Unix;;

print_string "----------------------\n";;
print_string "1:stumpwm-clisp with MPD\n";;
print_string "2:xmonad\n";;
print_string "3:dwm\n";;
print_string "9:console\n";;
print_string "----------------------\n";;

match read_line() with
| "1" -> Sys.command "xinit clisp &";
| "2" -> Sys.command "xinit xmonad &";
| "3" -> Sys.command "xinit dwm &";
| "9" -> exit 0 ;
| _ -> Sys.command "xinit &";

exit 0;;
