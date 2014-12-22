%% -define(NO_TRADE_DAYS,[{2011,1,1},{2011,1,2},{2011,1,3},        %元旦
%%                        {2011,2,2},{2011,2,3},{2011,2,4},{2011,2,5},{2011,2,6},
%%                        {2011,2,7},{2011,2,8},                   %春节
%%                        {2011,4,3},{2011,4,4},{2011,4,5},        %清明
%%                        {2011,4,30},{2011,5,1},{2011,5,2},       %劳动
%%                        {2011,6,4},{2011,6,5},{2011,6,6},        %端午
%%                        {2011,9,10},{2011,9,11},{2011,9,12},     %中秋
%%                        {2011,10,1}, {2011,10,2}, {2011,10,3},{2011,10,4},{2011,10,5}, %国庆
%%                        {2011,10,6},{2011,10,7}
%%                       ]).
%
%   what's the date from the given trading day passing some trading days ?
%   date window: 7, 13
%
%% date_by_nday({Y,M,D}, NTday) when NTday > 0 ->
%%     do_date_by_nday({Y,M,D}, NTday).

%% do_date_by_nday({Y,M,D}, NTday) when NTday >=1 ->
%%     case is_trade_day({Y,M,D}) of
%%         true ->
%%             if NTday == 1 -> {Y,M,D};
%%                 true -> do_date_by_nday(next_day({Y,M,D}), NTday-1)
%%             end;
%%         _    -> do_date_by_nday(next_day({Y,M,D}), NTday)
%%     end.

%% trade_days({Y1,M1,D1}, {Y2,M2,D2}) ->
%%     Day1 = date_to_gregorian_days(Y1, M1, D1+1),
%%     Day2 = date_to_gregorian_days(Y2, M2, D2),
%%     F = fun(E, Acc) ->
%%         {Y, M, D} = gregorian_days_to_date(E),
%%         case is_trade_day({Y, M, D}) of
%%             true -> [{Y, M, D} | Acc] ;
%%             _ -> Acc
%%         end
%%     end,
%%     Days = lists:reverse( lists:foldl(F, [], lists:seq(Day1, Day2)) ),
%%     Days.

%% is_trade_day({Y,M,D}) ->
%%     DOW = day_of_the_week({Y,M,D}),
%%     if DOW == 6 orelse DOW == 7 -> false;
%%     true -> 
%%         case lists:member({Y,M,D}, ?NO_TRADE_DAYS) of
%%             true -> false;
%%             _ -> true
%%         end
%%     end.

%% next_day({Y, M, D}) ->
%%     Day1 = date_to_gregorian_days(Y, M, D),
%%     gregorian_days_to_date(Day1 + 1).