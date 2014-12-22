%输入Start开始游戏，初始化9个格子，以-代表空格，X代表玩家，O代表电脑。
start:-
  initializecells(9,'-',R,[]), loop(R).
  
%重覆一直做玩家输入、电脑输入、玩家输入……
loop(R) :-
  repeat, write('Now the status is:'),nl,
  length(R,Num), printcells(R,Num),
  write('Please choose your cell(Enter a number between 1-9, Enter h or help):'),nl,
  read(Input), need_help(Input,R), writecell(Input,'X',R,R1,[]),
  not(game_is_over(R1)),
  write('Then I choose '),
  best_strategy(I,'O',R1),
  write(I), writecell(I,'O',R1,R2,[]),nl,
  not(game_is_over(R2)), !, loop(R2).
loop(_).

%当玩家输入h时，电脑会帮其计算出走每步的得分，建议最优选择。
need_help(Input,M) :- Input = h -> best_strategy(I,'X',M), write('For you the best strategy is to choose '), write(I), nl, fail ; true.


%判断游戏是否结束。两种情况代表游戏结束：一是有玩家连成一条直线，二是9个格子皆被占满。
game_is_over(M) :- gameover(M), nl, printcells(M,9), write('Game Over.'), nl,nl, abort.
gameover(M) :- in_line(S,M),(S = 'O'; S = 'X'), write('The Winner is '), (S = 'O' -> write('ME!') ; write('YOU!')), nl, !.
gameover(M) :- countif(M,'-',N), N = 0, write('No Winner!'), nl, !.

%玩家或电脑选择一个格子时，由此句来执行。
writecell(I,S,M) --> {nth1(I,M,'-'), replace(M,I,S,After_write)}, After_write.

%电脑模拟两人互动的过程。以下3句分别代表3个原则：
% 1. 若没格子了，以-当传回值，代表没输没赢。
% 2. 若选择某个格子，可以连成一直线，那必须选择该格子，并且传回X或O代表谁赢了。
% 3. 如果前两项皆不成立，则穷举所有的可能走法。
play_to_end(I,S,Winner,M):- countif(M,'-',N), N = 0, Winner = '-',!.
play_to_end(I,S,Winner,M):- will_in_line(I,S,M), Winner = S,!.
play_to_end(I,S,Winner,M):- writecell(I,S,M,M1,[]),rival(S,Sr),play_to_end(I1,Sr,Winner,M1).

%对某一个走法，算出其得分。当然，算分方式见仁见智了。
strategy_score(I,S,Score,M):-
  bagof(Winner, play_to_end(I,S,Winner,M), Set),
  countif(Set,S,Win), countif(Set,'-',Draw),length(Set,All), Score is round((Win + 0.1 * Draw)/ All * 10000) / 100,
  (S = 'X' -> write('If you choose '),write(I),write(', the score is '), write(Score), write('.'), nl ; true ).

%选出得分最高的策略
best_strategy(I,S,V) :-
  bagof([I,Score],strategy_score(I,S,Score,V),Result),
  nthlv2(2,Result,S1), max_list(S1,Max), nth1(Index,S1,Max),
  nthlv2(1,Result,I1), nth1(Index,I1,I).

%传回对手的标志
rival(S,Sr):- S = 'O' -> Sr = 'X' ; Sr = 'O'.

%判断是否排成一直线
will_in_line(I,S,M):- writecell(I,S,M,M1,[]), in_line(S,M1).

in_line(S,M):- M = [S,S,S,_,_,_,_,_,_],!.
in_line(S,M):- M = [_,_,_,S,S,S,_,_,_],!.
in_line(S,M):- M = [_,_,_,_,_,_,S,S,S],!.
in_line(S,M):- M = [S,_,_,S,_,_,S,_,_],!.
in_line(S,M):- M = [_,S,_,_,S,_,_,S,_],!.
in_line(S,M):- M = [_,_,S,_,_,S,_,_,S],!.
in_line(S,M):- M = [S,_,_,_,S,_,_,_,S],!.
in_line(S,M):- M = [_,_,S,_,S,_,S,_,_],!.

% 印出目前的状态，空的格子以-代替。
printcells(M,Num):-
  Num1 is 10 - Num, Num1 =< 9,
  ( nth1(Num1,M,S), write(S), mod(Num1,3) =:=0 , nl , fail;
    Num2 is Num -1, printcells(M,Num2)
  ),!.
printcells(_,_).

%以符号S初始化Num个格子
initializecells(Num,S) --> {Num >0}, [S], {Num1 is Num -1}, initializecells(Num1,S),!.
initializecells(NUm,S) --> [].

% 计算List(L)中某原子(S)的个数(N)
countif(L,S,N):- bagof(I,nth1(I,L,S),R), length(R,N),!.
countif(L,S,N):- N = 0.

% nthlv2用来取出第二层List的某一项，比如想取出[[a,t,m],[k,e,y],[p,a,s]]中每个List的第3项，
% 则令 A = [[a,t,m],[k,e,y],[p,a,s]], 使用nthlv2(3,A,B), 则 传回 B = [m,y,s]。
listlv2(A,L,S):- nth1(_,L,L2),nth1(A,L2,S).
nthlv2(A,L,R) :- bagof(S,listlv2(A,L,S),R).

%将List中的第Index项以某个原子(With)替换掉，并将结果输出至ListOut
replace(List, Index, With, ListOut) :-
  Idx is Index - 1, length(Before,Idx),
  append(Before, [_Discard|Rest], List),
  append(Before, [With|Rest], ListOut).
