%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
-record(client, {
	state = wait :: wait | request | response | response_body,
	opts = [] :: [any()],
	socket = undefined :: undefined | inet:socket(),
	transport = undefined :: module(),
	timeout = 5000 :: timeout(), %% @todo Configurable.
	buffer = <<>> :: binary(),
	connection = keepalive :: keepalive | close,
	version = 'HTTP/1.1' :: cowboy:http_version(),
	response_body = undefined :: undefined | non_neg_integer()
}).
%%%%%%%%%%%%%%%%%%
-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown} | {normal, timeout} | {error, atom()}.
%%%%%%%%%%%%%%%%%%
#!/bin/sh
erl +K true +P 10240000 -sname testserver -pa ebin -pa deps/*/ebin -s htmlfilesimple\
-eval "io:format(\"Server start with port 8000 Success!~n\")."
%%%%%%%%%%%%%%%%%%
-module(snippet).
-export([max/1]).
 
max(N) ->
	Max = erlang:system_info(process_limit),
	io:format("Maxmium allowed process is ~p ~n", [Max]),
	statistics(runtime),
	statistics(wall_clock),
	L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),
	lists:foreach(fun(Pid) -> Pid ! die end, L),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 /N,
	io:format("Process spawn time=~p (~p) microseconds ~n", [U1, U2]).
 
wait() ->
	receive
		die -> void
	end.
 
for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
%%%%%%%%%%%%%%%%%%
源代码安装
#wget https://elearning.erlang-solutions.com/binaries/sources/otp_src_R16B.tar.gz
#tar xvf otp_src_R16B.tar.gz
cd otp_src_R16B
./configure --prefix=/usr/local/erlang --enable-hipe --enable-threads --enable-smp-support --enable-kernel-poll
make
make install

添加到环境变量中(/etc/profile)
export ERL_HOME=/usr/local/erlang export PATH=$ERL_HOME/bin:$PATH

%%%%%%%%%%%%%%%%%%
写好的erlang程序要放在服务器上跑，总不能一直终端打开erl的吧？ 应该是让erlang程序运行得像deamon那样，需要时候再连接上去操作。

erl启动使用  -detached 参数启动

这个参数可以让erl节点脱离了终端。

例如 ：   $erl -sname dp -detached

如何链接：

     erl -sname dp2 -remsh dp@dp0304

如何退出：！！！超级要注意啊～～不能直接关掉退出

   ctrl +g 进入JCL模式

   输入  q  回车
%%%%%%%%%%%%%%%%%%

dbg:tracer(), dbg:p(all, [call, timestamp]).
dbg:tpl(yaws_server, aloop, []).

%Then hit the system with one client request. You should see some trace details on your Erlang console, including timestamps for the call in question. That function is recursive and it includes pretty much all of the server processing per request, so it's a quick way to help narrow down where the time is going.

To stop the tracing: dbg:stop_clear().

%%%%%%%%%%%%%%%%%%

Root = filename:absname_join(filename:dirname(?FILE), ".."),
application:set_env(mnesia, dir, filename:join(Root, "db")).

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
            false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.
