%% coding: utf-8 
-module(user_default).
-author('***@163.com').

-compile(nowarn_unused_function).
-compile(export_all).
%-export([winG/3, winQ/3, div618/2, stopLoss/3, sd/1, sh/1, qachina/0]).
%-export([help/0, r/0]).

-compile([native, {hipe, [o3]}]).
%% -compile({inline,[pi/0]}).

%% pi() -> 3.1416.

%--------------------------------------------------
%% -opaque u16int() :: 0..(1 bsl 16 - 1). 
%% -export_type([u16int/0]). 

%% -record(headerrec, {message_id :: u16int()}). 
%% -opaque headerrec() :: #headerrec{}. 
%% -export_type([headerrec/0]).

%--------------------------------------------------
%-type diskinfo() :: {non_neg_integer(), non_neg_integer()}.
%-spec diskspace(nonempty_string()) -> {'ok', diskinfo()} | {'error', term()}.
%--------------------------------------------------

-include("records.hrl").

-import(calendar, [date_to_gregorian_days/3, gregorian_days_to_date/1, day_of_the_week/1]).

-ifdef(Debug).
-define(DEBUG(Fmt, Args), io:format(Fmt, Args)).  
-else.
-define(DEBUG(Fmt, Args), no_debug).  
-endif.

get_meta() -> 
	user_default:module_info().

help()->
    shell_default:help(),
    io:format("** user extended commands **~n",[]),
    io:format("r() -- compile and load all modified modules on all nodes\n",[]).
    
r()->
    make:all([netload]).

sd(Word) -> sh("sdcv -n " ++ Word).
	
sh(Cmd) -> 
    Res = string:tokens(os:cmd(Cmd),"\n"),
    lists:foreach(
        fun(X)->io:format("~ts~n", [unicode:characters_to_list(erlang:iolist_to_binary(X))]) end,
        Res).

qachina() ->
	ok = application:start(yaws).

%---------------------------------------------------------------------
%       stock
%---------------------------------------------------------------------

-define(SXF, 0.0015). %手续费
-define(YHS, 0.001).  %印花费
-define(GHF, 1.0).    %过户费

-define(RATIO,[0.0, 0.191, 0.236, 0.382, 0.5, 0.618, 0.809, 1.0]).

%计算股票盈利
-spec winG(integer(), float(), float()) -> float .
winG(Qty,Pb,Ps) -> 
	io:format("Win:~.2f~n", [Qty * Ps * (1 -?SXF - ?YHS) - 2 * ?GHF - Qty * Pb * (1 + ?SXF)]).

%算权证盈利
winQ(Qty,Pb,Ps) ->
	io:format("Win:~.2f~n", [Qty * Ps * (1 - ?SXF) - 2 * ?GHF - Qty * Pb * (1 + ?SXF)]).

%止损价
stopLoss(Qty,Pb,LossRate) ->
    T = Qty * Pb * (1 + ?SXF),
    io:format("Stop Loss at: ~.2f~n",[Pb - (T * LossRate) / Qty]),
    io:format("Lost Money: ~.2f~n",[T*LossRate]) .

div618(P1, P2) ->
    P = fun(R) ->
        if P1 =< P2 -> P1 + (P2 - P1) * R;
        true -> P1 - (P1 -P2) * R
        end
    end,
    if P1 =< P2 ->
        lists:foreach(fun(R)-> io:format("---~.3f  ~.2f---\n",[R,P(R)]) end, lists:reverse(?RATIO));
    true ->
        lists:foreach(fun(R)-> io:format("---~.3f  ~.2f---\n",[R,P(R)]) end, ?RATIO)
    end.


%---------------------------------------------------------------------
%       lottery
%---------------------------------------------------------------------

%            产生福彩双色球号码
%   Count:   注数
%   NoRed:   不要的红色球号列表
%   NoBlue:  不要的蓝色号列表
%---------------------------------------------------------------------
win_ssq(Count, NoRed, NoBlue) ->
    put(result,undefined),
    put(random_seed, seed()),
	NoRedLst = str2ints(NoRed),
    pick_ssq_nums(
        Count,
	    good_red() -- NoRedLst,
        lists:seq(1,33) -- NoRedLst,
        pick_num(Count, lists:seq(1,16)--str2ints(NoBlue), [])
    ),
    file:write_file(ssqNum(), get(result)).

pick_ssq_nums(0, _, _, _) -> ok;
pick_ssq_nums(Count, GRed, YesRed, OkBlue) ->
	if
		Count == 1 ->
			Red6 = lists:sort( pick_num(6, GRed, []) );
	    true ->
			Red6 = lists:sort( pick_num(6, YesRed, []) )
	end,
    Result = lists:append(Red6, [lists:nth(Count,OkBlue)]),
    ResStr = lists:append(string:strip(
                lists:foldl(fun(X,Acc) -> Acc ++ integer_to_list(X) ++ " " end, "", Result)), "\n"),
    io:format("~p~n", [lists:takewhile(fun(X)->X /= $\n end, ResStr)]),
    case get(result) of
        undefined ->
            put(result, ResStr);
        _ ->
            put(result, get(result) ++ ResStr)
    end,
    pick_ssq_nums(Count-1, GRed, YesRed, OkBlue).

good_red() ->
    {ok,Bin} = file:read_file(ssqHitNum()),
    NumLst = string:tokens(binary_to_list(Bin), "\n"),
    T = ets:new(tmp, [public,ordered_set]),
    lists:foreach(fun(I)-> ets:insert(T, {I,0}) end, lists:seq(1,33)),
    lists:foreach(
        fun(Num) ->
            Ns =  str2ints(string:sub_string(Num,7)),
            lists:foreach(
                fun(N) ->
                    [{_,Cnt}] = ets:lookup(T,N),
                    ets:insert(T,{N,Cnt+1})
                end, lists:sublist(Ns,6))
        end, NumLst),
    Tmp = lists:sublist(lists:keysort(2, ets:tab2list(T)), 13, 21),
    lists:sort( lists:map(fun({K,_})->K end, Tmp) ).
    
%检查是否中奖
%    HitNo:  中奖号
%hit_ssq(NoStr, HitNo) ->
hit_ssq(NoStr,HitNo) ->
    {ok,HitBin} = file:read_file(ssqHitNum()),
    HasNoStr = string:str(binary_to_list(HitBin),NoStr),
    if HasNoStr == 0 ->
        HitNoStr = NoStr ++ "\s" ++ HitNo ++ "\n",
        {ok,H} = file:open(ssqHitNum(),[append]),
        file:write(H,HitNoStr),
        file:close(H);
    true -> ok
    end,
    HitNoLst = str2ints(HitNo),
    HitRed = lists:sublist(HitNoLst, 6),
    HitBlue = lists:sublist(HitNoLst, 7, 1),

    {GoodHit,_} = hit_check({good_red(), HitRed}, {[], HitBlue}),
    io:format("Good Red Hit:~w~n",[GoodHit]),

    {ok,Bin} = file:read_file(ssqNum()),
    NumLst = string:tokens(binary_to_list(Bin), "\n"),
    io:format("No\tFirst\tSecond\tResult~n"),
    io:format("--------------------------------------------------------------~n"),
    lists:foreach(
        fun(Num) ->
            T1 = str2ints(Num),
            NumRed = lists:sublist(T1, 6),
            NumBlue = lists:sublist(T1, 7, 1),
            {HitRedNum, HitBlueNum} = hit_check({NumRed,HitRed}, {NumBlue,HitBlue}),
            Desc = get_ssq_result(HitRedNum, HitBlueNum),
            io:format("~w\t~w\t~w\t~s~n",[T1,HitRedNum,HitBlueNum,Desc])
        end, NumLst).

get_ssq_result(RedNum, BlueNum) ->
    case {RedNum, BlueNum} of
        {6,1} -> "1st";
        {6,0} -> "2nd";
        {5,1} -> "3rd(3000)";
        {5,0} -> "4th(200)";
        {4,1} -> "4th(200)";
        {4,0} -> "5th(10)";
        {3,1} -> "5th(10)";
        {_,1} -> "6th(5)";
        _ -> "X"
    end.

ssqNum() -> "/media/D/qachina/db/doc/money/ssqNum.txt".
	%filename:dirname(code:which('user_default')) ++ "/ssqNum.txt".

ssqHitNum() -> "/media/D/qachina/db/doc/money/ssqHitNum.txt".
	%filename:dirname(code:which('user_default')) ++ "/ssqHitNum.txt".

%convert a "1 2 3 4 5 " similar string to integer list
str2ints(Str) ->
    lists:map(fun(X)->list_to_integer(X) end, string:tokens(Str,"\s")).

%% pretty good seed, but non portable
%   <<A:32, B:32, C:32>> = crypto:rand_bytes(12)
seed() ->
    case (catch list_to_binary(
           os:cmd("dd if=/dev/urandom bs=12 count=1 2>/dev/null"))) of
        <<X:32, Y:32, Z:32>> ->
            {X, Y, Z};
        _ ->
            {_,_,X} = erlang:now(),
            {H,M,S} = time(),
            H1 = H * X rem 32767,
            M1 = M * X rem 32767,
            S1 = S * X rem 32767,
            {H1,M1,S1}
    end.

pick_num(0, _, Acc) -> Acc;
pick_num(N, From, Acc) ->
    OneNum = lists:nth(random:uniform(length(From)), From),
    %timer:sleep(1000),
	receive
    after 500 -> ok %% in milliseconds
	end,
    pick_num(N-1, From--[OneNum], [OneNum | Acc]).

hit_check({NumFst, HitFst}, {NumSnd, HitSnd}) ->
    HitFstNum = lists:foldl(fun(X, Acc) ->
                                case lists:member(X, HitFst) of
                                    true -> Acc + 1;
                                    false -> Acc
                                end
                            end, 0, NumFst),
    HitSndNum = lists:foldl(fun(X, Acc) ->
                                case lists:member(X, HitSnd) of
                                    true -> Acc + 1;
                                    false -> Acc
                                end
                            end, 0, NumSnd),
    {HitFstNum, HitSndNum}.

his() -> sh("tail " ++ ssqHitNum()).

%-----------------------------------------------------------------
