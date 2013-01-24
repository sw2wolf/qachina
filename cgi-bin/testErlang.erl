#!/usr/local/bin/escript
-export([main/1]).

%% External API

main(_) ->
    ensure(),
    io:format("~n"),
    io:format("welcome to erlang world~n").
    
%% Internal API

ensure() ->
    code:add_patha(filename:dirname(escript:script_name())).
