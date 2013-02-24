/*  （本程序翻译自一德文网站）
* 本程序需要swi-prolog运行，因为使用了swi-prolog的一个扩展库readln.pl, 此文件
  可以在library目录下找到，如果需要在别的环境下运行，请自己实现readln的功能。
  
   这是一个pascal语言的编译、解释器，只实现了
   [while, do, begin, end, write, program, if, then, else]
   等关键字，不能自定义函数，没有局部变量。
   原德文程序中没有关于Exp1=Exp2的解释以及编译部分，我自己完善了解释器，但
   由于我对汇编语言不熟悉，没有对编译器进行修改。
   
   本程序各部分功能如下：
   Scanner进行词法分析
   Parser把词法分析的结果分析成语法树
   Interpreter解释Parser分析的语法树
   Compiler把语法树编译成汇编
   
   此程序能解释的Pascal 程序例：
program Primzahl;
begin
  write(2);
  bis:=100;
  Zahl:=3;
  while Zahl<bis do begin
    Teilbar:=0;
    Faktor1:=3;
    while Faktor1<Zahl/2 do begin
      Faktor2:=3;
      while Faktor2<Zahl/2 do begin
        if Zahl=Faktor1*Faktor2 then begin
          Teilbar:=1
        end;
        Faktor2:=Faktor2+2;
      end;
      Faktor1:=Faktor1+2
    end;
    if Teilbar=0 then begin
      write(Zahl)
    end;
    Zahl:=Zahl+2
  end
end.
%--------------------
program Fakultaet;
begin
  n:= 7;
  i:= 1;
  Fak:= 1;
  while i < n do begin
    i:= i + 1;
    Fak:= Fak * i
  end;
  write(Fak)
end.
%-------------------
program Potenzerrechnen;
begin
  i:=0;
  n:=30;
  Potenz:=1;
  while i<n do begin
    Potenz:=Potenz*n;
    i:=i+1;
  end;
end.
%--------------------
将上面的一段程序保存为test.pas，并且放在本程序同一目录下，然后调用：
interpret_programm('test.pas').
即可运行
   
*/

/* Scanner */
%------------------------------------------------------------------------------
init_scanner(File):-
  see(File),
  readln(Input, _, ".", "0123456789", lowercase),
  seen,
  scan(Input, Symbols),
  retractall(symbols(_)), asserta(symbols(Symbols)).

symbol(Symbol):-
  symbols([Symbol|_]).

next_symbol:-
  retract(symbols([_|Symbols])),
  asserta(symbols(Symbols)).

reserved(X):-
  member(X, [while, do, begin, end, write, program, if, then, else]).

/* Symbole bilden */

scan([Symbol1, Symbol2|Symbols1], [Symbol3|Symbols3]):-
  buildup(Symbol1, Symbol2, Symbol3), !,
  scan(Symbols1, Symbols3).
scan([Symbol1|Symbols1], [Symbol2|Symbols2]):-
  pack(Symbol1, Symbol2), !,
  scan(Symbols1, Symbols2).
scan([], []).

buildup(<, =, <=).
buildup(<, >, <>).
buildup(>, =, >=).
buildup(:, =, :=).

pack(Symbol, bez(Symbol)):-
  name(Symbol, [K|_]),
  is_alpha(K),
  \+ reserved(Symbol).
pack(Symbol, Symbol).


/* Parser fuer mini-PASCAL */
%------------------------------------------------------------------------------
parse_programm(File, ParsingTree):-
  init_scanner(File),
  write('scanner ok'),nl,
  parse_programm(ParsingTree), !.

parse_programm(Instructions):-
  parse_symbol(program),
  parse_variable(_Name),
  parse_symbol(;),
  parse_Instruction(begin(Instructions)),
  parse_symbol(.).

parse_Instructions([Instruction|Instructions]):-
  parse_Instruction(Instruction),
  (parse_symbol(;) ->   /* if .. then .. else .. */
     parse_Instructions(Instructions);
     Instructions = []).

parse_Instruction(assignment(Variable, Expression)):-
  parse_variable(Variable), !,
  parse_symbol(:=),
  parse_Expression(Expression).

parse_Instruction(while(Expression, Instruction)):-
  parse_symbol(while), !,
  parse_Expression(Expression),
  parse_symbol(do),
  parse_Instruction(Instruction).

parse_Instruction(if(Expression, Instruction1, Instruction2)):-
  parse_symbol(if), !,
  parse_Expression(Expression),
  parse_symbol(then),
  parse_Instruction(Instruction1),
  (parse_symbol(else) ->
     parse_Instruction(Instruction2);
     Instruction2 = nil).

parse_Instruction(begin(Instructions)):-
  parse_symbol(begin), !,
  parse_Instructions(Instructions),
  parse_symbol(end).

parse_Instruction(write(Expression)):-
  parse_symbol(write), !,
  parse_symbol('('),
  parse_Expression(Expression),
  parse_symbol(')').

parse_Instruction(nil).

parse_Expression(Expression):-
  parse_moresimply_Expression(Expression1),
  parse_Expression(Expression1, Expression).
parse_Expression(Expression1, Expression1 < Expression2):-
  parse_symbol(<), !,
  parse_moresimply_Expression(Expression2).
parse_Expression(Expression1, Expression1 = Expression2):-
  parse_symbol(=), !,
  parse_moresimply_Expression(Expression2).
parse_Expression(Expression, Expression).

parse_moresimply_Expression(Moresimply_Expression):-
  parse_term(Term),
  parse_moresimply_Expression(Term, Moresimply_Expression).
parse_moresimply_Expression(Term1, Moresimply_Expression):-
  parse_symbol(+), !,
  parse_term(Term2),
  parse_moresimply_Expression(Term1 + Term2, Moresimply_Expression).
parse_moresimply_Expression(Term1, Moresimply_Expression):-
  parse_symbol(-), !,
  parse_term(Term2),
  parse_moresimply_Expression(Term1 - Term2, Moresimply_Expression).
parse_moresimply_Expression(Moresimply_Expression, Moresimply_Expression).

parse_term(Term):-
  parse_factor(Factor),
  parse_term(Factor, Term).
parse_term(Factor1, Term):-
  parse_symbol(*), !,
  parse_factor(Factor2),
  parse_term(Factor1 * Factor2, Term).
parse_term(Factor1, Term):-
  parse_symbol(/), !,
  parse_factor(Factor2),
  parse_term(Factor1 / Factor2, Term).
parse_term(Term, Term).

parse_factor(Factor):-
  parse_symbol('('),
  parse_Expression(Factor),
  parse_symbol(')').

parse_factor(Factor):-
  parse_symbol(+),
  parse_factor(Factor).

parse_factor(- Factor):-
  parse_symbol(-),
  parse_factor(Factor).

parse_factor(Factor):-
  parse_number(Factor).

parse_factor(Variable):-
  parse_variable(Variable).

parse_number(Number):-
  symbol(Number),
  integer(Number),
  next_symbol.

parse_variable(Designator):-
  symbol(bez(Designator)),
  next_symbol.

parse_symbol(Symbol):-
  symbol(Symbol),
  next_symbol.


/* Interpreter for mini-PASCAL */
%------------------------------------------------------------------------------
interpret_programm(File):-
  parse_programm(File, Parsetree),
  write(ok),nl,
  retractall(memory(_, _)),
  interpret_instructions(Parsetree), !,
  listing(memory).
  
interpret_instructions([Instruction|Instructions]):-
  interpret_instruction(Instruction),
  interpret_instructions(Instructions).
interpret_instructions([]).
  
interpret_instruction(assignment(Variable, Expression)):-
  interpret_Expression(Expression, Value),
  retractall(memory(Variable, _)),
  asserta(memory(Variable, Value)).

interpret_instruction(while(Expression, Instruction)):-
  interpret_Expression(Expression, true), !,
  interpret_instruction(Instruction),
  interpret_instruction(while(Expression, Instruction)).
interpret_instruction(while(_,_)).
   
interpret_instruction(if(Expression, Instruction1, _instruction2)):-
  interpret_Expression(Expression, true), !,
  interpret_instruction(Instruction1).
interpret_instruction(if(_Expression, _instruction1, Instruction2)):-
  interpret_instruction(Instruction2).

interpret_instruction(begin(Instructions)):-
  interpret_instructions(Instructions).
  
interpret_instruction(write(Expression)):-
  interpret_Expression(Expression, Value),
  write(Value), nl.

interpret_instruction(nil).

interpret_Expression(Expression1 < Expression2, true):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value1 < Value2,!.
interpret_Expression(_Expression1 < _Expression2, false).

interpret_Expression(Expression1 = Expression2, true):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value1 = Value2,!.
interpret_Expression(_Expression1 = _Expression2, false).

interpret_Expression(Expression1 > Expression2, Value):-
    interpret_Expression(Expression2 < Expression1, Value).

interpret_Expression(Expression1 + Expression2, Value):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value is Value1 + Value2.
  
interpret_Expression(Expression1 - Expression2, Value):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value is Value1 - Value2.
  
interpret_Expression(Expression1 * Expression2, Value):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value is Value1 * Value2.
   
interpret_Expression(Expression1 / Expression2, Value):-
  interpret_Expression(Expression1, Value1),
  interpret_Expression(Expression2, Value2),
  Value is Value1 / Value2.
  
interpret_Expression(- Expression, Value):-
  interpret_Expression(Expression, Value1), !,
  Value is - Value1.
  
interpret_Expression(Number, Number):-
  integer(Number), !.
  
interpret_Expression(Variable, Value):-
  memory(Variable, Value), !.

interpret_Expression(false, false).

interpret_Expression(Variable, _):-
  atom(Variable),
  write('Run time error: Access to undefined variable'),
  write(Variable), nl, fail.
 
 
 /* Compiler for mini-Pascal */
%-----------------------------------------------------------------------------
compile_programm(File):-
  parse_programm(File, Parsetree),
  retractall(mark(_)),
  asserta(mark(1)),
  compile_Instructions(Parsetree).

give_mark(mark):-
  retract(mark(mark)),
  mark2 is mark + 1,
  asserta(mark(mark2)).

compile_Instructions([Instruction|Instructions]):-
  compile_Instruction(Instruction), !,
  compile_Instructions(Instructions).
compile_Instructions([]).

compile_Instruction(assignment(Variable, Expression)):-
  compile_Expression(Expression),
  writenl('mov  ', Variable, ', ax').

compile_Instruction(while(Expression, Instruction)):-
  give_mark(mark1),
  write_mark(mark1),
  compile_Expression(Expression),
  writenl('cmp  ax, True'),
  give_mark(mark2),
  writenl('jnz  M', mark2),
  compile_Instruction(Instruction),
  writenl('jmp  M', mark1),
  write_mark(mark2).

compile_Instruction(if(Expression, Instruction1, Instruction2)):-
  compile_Expression(Expression),
  give_mark(mark1),
  writenl('cmp  ax, True'),
  writenl('jnz  M', mark1),
  compile_Instruction(Instruction1),
  give_mark(mark2),
  writenl('jmp  M', mark2),
  write_mark(mark1),
  compile_Instruction(Instruction2),
  write_mark(mark2).

compile_Instruction(begin(Instructions)):-
  compile_Instructions(Instructions).

compile_Instruction(write(Expression)):-
  compile_Expression(Expression),
  writenl('out  ax').

compile_Instruction(nil).

compile_Expression(Expression1 < Expression2):-
  compile_Expression(Expression1),
  writenl('push ax'),
  compile_Expression(Expression2),
  writenl('pop  cx'),
  writenl('cmp  ax, cx'),
  writenl('mov  ax, True'),
  give_mark(mark),
  writenl('jl   M', mark),
  writenl('mov  ax, False'),
  write_mark(mark).

compile_Expression(Expression1 + Expression2):-
  compile_Expression(Expression2),
  writenl('push ax'),
  compile_Expression(Expression1),
  writenl('pop  cx'),
  writenl('add  ax, cx').

compile_Expression(Expression1 - Expression2):-
  compile_Expression(Expression2),
  writenl('push ax'),
  compile_Expression(Expression1),
  writenl('pop  cx'),
  writenl('sub  ax, cx').

compile_Expression(Expression1 * Expression2):-
  compile_Expression(Expression2),
  writenl('push ax'),
  compile_Expression(Expression1),
  writenl('pop  cx'),
  writenl('imul ax, cx').

compile_Expression(Expression1 / Expression2):-
  compile_Expression(Expression2),
  writenl('push ax'),
  compile_Expression(Expression1),
  writenl('pop  cx'),
  writenl('idiv ax, cx').

compile_Expression(- Expression):-
  compile_Expression(Expression), !,
  writenl('neg  ax').

compile_Expression(Variable):-
  writenl('mov  ax, ', Variable).

writenl(Exp):-
  tab(4), write(Exp), nl.

writenl(Exp1, Exp2):-
  tab(4), write(Exp1), write(Exp2), nl.

writenl(Exp1, Exp2, Exp3):-
  tab(4), write(Exp1), write(Exp2), write(Exp3), nl.

write_mark(mark):-
  write('M'), write(mark), write(':'), nl.
