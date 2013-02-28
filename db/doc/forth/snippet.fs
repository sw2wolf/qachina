5 0 DO I . LOOP

Will print
0 1 2 3 4

The way this works is:
DO moves the index (0) and the control (5) over to the loop stack.
I copies the top of the loop stack to the data stack.
LOOP increments the index (top of loop stack). If the index is less than the control (one below the top of loop stack), then it reruns the commands from DO back to LOOP. If the index is >=, then it pops the index and control from the loop stack, and control resumes as normal.

\

variable rnd
 
: randoms ( n -- )
  s" /dev/urandom" r/o open-file throw
  swap 0 do
    dup rnd 1 cells rot read-file throw drop
    rnd @ .
  loop
  close-file throw ;

\
include unix/socket.fs
 
s" localhost" 80 open-socket
dup s\" GET / HTTP/1.0\n\n" rot write-socket
dup pad 8092 read-socket  type
close-socket


\
: stack ( size -- )
  create here cell+ ,  cells allot ;
 
: push ( n st -- ) tuck @ !  cell swap +! ;
: pop ( st -- n ) -cell over +!  @ @ ;
: empty? ( st -- ? ) dup @ - cell+ 0= ;
 
10 stack st
 
1 st push
2 st push
3 st push
st empty? .  \ 0 (false)
st pop . st pop . st pop .  \ 3 2 1
st empty? .  \ -1 (true)

\
: newline? ( ch -- flag )
    case
        10 of true endof
        13 of true endof
        false swap
	endcase ;

\
: factorial  ( n -- n! )  recursive
    dup 1 >  if dup 1- factorial  *  then
;

: test 0  begin  dup .  1+  dup 5 =  until ; \ 0 1 2 3 4

: test 7 2  do  9 .  loop ; \ 9 9 9 9 9


