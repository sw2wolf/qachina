
%%%
WorkerPid = proc_lib:spawn(?MODULE, cgi_worker,
                            [self(), Arg, ExeFN, Scriptfilename, PI, ExtraEnv, SC]),
...

cgi_worker(Parent, Arg, Exefilename, Scriptfilename, Pathinfo, ExtraEnv,SC) ->
    Env = build_env(Arg, Scriptfilename, Pathinfo, ExtraEnv,SC),
    ?Debug("~p~n", [Env]),
    CGIPort = open_port({spawn, Exefilename},
                        [{env, Env},
                         {cd, pathof(Scriptfilename)},
                         exit_status,
                         binary]),
...

%%%
-record(g, {key, value}). 
ets:new(g, [public, named_table, {keypos, #g.key}]). 
ets:insert_new(g, #g{user_id, 0}). 

Uid = ets:update_counter(g, user_id). 
ets:insert_new(users, #user{id=Uid, name="Somebody"}.

%%%
dbg:tracer(),dbg:p(all, [call]),dbg:tpl(heart, [{'_', [], [{return_trace}]}]). 
{ok,[{matched,nonode@nohost,22},{saved,1}]}
4> erlang:whereis(heart).
(<0.4.0>) 

%%%
listen(Port, N) ->
    Opts = [{active, false},
            binary,
            {backlog, 256},
            {packet, http_bin},
            {raw,6,9,<<1:32/native>>}, %defer accept
            %%{delay_send,true},
            %%{nodelay,true},
            {reuseaddr, true}],
 
    {ok, S} = gen_tcp:listen(Port, Opts),
    Spawn = fun(I) ->    
                    register(list_to_atom("acceptor_" ++ integer_to_list(I)),
                             spawn_opt(?MODULE, accept, [S, I], [link, {scheduler, I}]))
            end,
    lists:foreach(Spawn, lists:seq(1, N)).

%%%
main() ->
    random:seed(now()),
    io:format("24 Game~n"),
    play().
 
play() ->
    io:format("Generating 4 digits...~n"),
    Digts = [random:uniform(X) || X <- [9,9,9,9]],
    io:format("Your digits\t~w~n", [Digts]),
    read_eval(Digts),
    play().
 
read_eval(Digits) ->
    Exp = string:strip(io:get_line(standard_io, "Your expression: "), both, $\n),
    case {correct_nums(Exp, Digits), eval(Exp)} of
        {ok, X} when X == 24 -> io:format("You Win!~n");
        {ok, X} -> io:format("You Lose with ~p!~n",[X]);
        {List, _} -> io:format("The following numbers are wrong: ~p~n", [List])
    end.
 
correct_nums(Exp, Digits) ->
    case re:run(Exp, "([0-9]+)", [global, {capture, all_but_first, list}]) of
        nomatch ->
            "No number entered";
        {match, IntLs} ->
            case [X || [X] <- IntLs, not lists:member(list_to_integer(X), Digits)] of
                [] -> ok;
                L -> L
            end
    end.
 
eval(Exp) ->
    {X, _} = eval(re:replace(Exp, "\\s", "", [{return, list},global]),
                  0),
    X.
 
eval([], Val) ->
    {Val,[]};
eval([$(|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$)|Rest], Val) ->
    {Val, Rest};
eval([$[|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$]|Rest], Val) ->
    {Val, Rest};
eval([$+|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val + NewOperand);
eval([$-|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val - NewOperand);
eval([$*|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val * NewOperand);
eval([$/|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val / NewOperand);
eval([X|Rest], 0) when X >= $1, X =< $9 ->
    eval(Rest, X-$0).
 

The evaluator uses a simple infix scheme that doesn't care about operator precedence, but does support brackets and parentheses alike. Thus, ((9+1)*2)+2+2 is evaluated as: 
9 + 1 = 10
10 * 2 = 20
2 + 2 = 4
20 + 4

Example: 
1> c(g24).    
{ok,g24}
2> g24:main().
24 Game
Generating 4 digits...
Your digits     [7,4,6,8]
Your expression: 6*4
You Win!
Generating 4 digits...
Your digits     [4,1,5,8]
Your expression: 6*4
The following numbers are wrong: ["6"]
Generating 4 digits...
Your digits     [8,5,8,2]
Your expression: 2*([8/5]*2)
You Lose with 6.4!
Generating 4 digits...
Your digits     [7,4,8,1]

%%%
io:format("<<~s>>~n", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= <<255,16>> ]]).
bin_to_hex_list(Bin) when is_binary(Bin) ->
  lists:flatten([integer_to_list(X,16) || <<X>> <= Bin]).
%
-module(snippet).

-compile([native, {hipe, [o3]}]).

-export([bin_to_hex/1]).

bin_to_hex(B) when is_binary(B) ->
  bin_to_hex(B, <<>>).

-define(H(X), (hex(X)):16).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(Bin, Acc) when byte_size(Bin) band 7 =:= 0 ->
  bin_to_hex_(Bin, Acc);
bin_to_hex(<<X:8, Rest/binary>>, Acc) ->
  bin_to_hex(Rest, <<Acc/binary, ?H(X)>>).

bin_to_hex_(<<>>, Acc) -> Acc;
bin_to_hex_(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acc) ->
  bin_to_hex_(
    Rest,
    <<Acc/binary,
      ?H(A), ?H(B), ?H(C), ?H(D), ?H(E), ?H(F), ?H(G), ?H(H)>>).

-compile({inline, [hex/1]}).

hex(X) ->
  element(
    X+1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
          16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044,
          16#3045, 16#3046, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
          16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3141, 16#3142,
          16#3143, 16#3144, 16#3145, 16#3146, 16#3230, 16#3231, 16#3232,
          16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
          16#3241, 16#3242, 16#3243, 16#3244, 16#3245, 16#3246, 16#3330,
          16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
          16#3338, 16#3339, 16#3341, 16#3342, 16#3343, 16#3344, 16#3345,
          16#3346, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
          16#3436, 16#3437, 16#3438, 16#3439, 16#3441, 16#3442, 16#3443,
          16#3444, 16#3445, 16#3446, 16#3530, 16#3531, 16#3532, 16#3533,
          16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3541,
          16#3542, 16#3543, 16#3544, 16#3545, 16#3546, 16#3630, 16#3631,
          16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
          16#3639, 16#3641, 16#3642, 16#3643, 16#3644, 16#3645, 16#3646,
          16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
          16#3737, 16#3738, 16#3739, 16#3741, 16#3742, 16#3743, 16#3744,
          16#3745, 16#3746, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
          16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3841, 16#3842,
          16#3843, 16#3844, 16#3845, 16#3846, 16#3930, 16#3931, 16#3932,
          16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
          16#3941, 16#3942, 16#3943, 16#3944, 16#3945, 16#3946, 16#4130,
          16#4131, 16#4132, 16#4133, 16#4134, 16#4135, 16#4136, 16#4137,
          16#4138, 16#4139, 16#4141, 16#4142, 16#4143, 16#4144, 16#4145,
          16#4146, 16#4230, 16#4231, 16#4232, 16#4233, 16#4234, 16#4235,
          16#4236, 16#4237, 16#4238, 16#4239, 16#4241, 16#4242, 16#4243,
          16#4244, 16#4245, 16#4246, 16#4330, 16#4331, 16#4332, 16#4333,
          16#4334, 16#4335, 16#4336, 16#4337, 16#4338, 16#4339, 16#4341,
          16#4342, 16#4343, 16#4344, 16#4345, 16#4346, 16#4430, 16#4431,
          16#4432, 16#4433, 16#4434, 16#4435, 16#4436, 16#4437, 16#4438,
          16#4439, 16#4441, 16#4442, 16#4443, 16#4444, 16#4445, 16#4446,
          16#4530, 16#4531, 16#4532, 16#4533, 16#4534, 16#4535, 16#4536,
          16#4537, 16#4538, 16#4539, 16#4541, 16#4542, 16#4543, 16#4544,
          16#4545, 16#4546, 16#4630, 16#4631, 16#4632, 16#4633, 16#4634,
          16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642,
          16#4643, 16#4644, 16#4645, 16#4646}).
%%%
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose
%%%
特性1： 
On the third line (or second line depending on the presence of the Emacs directive), it is possible to give arguments to the emulator, such as

%%! -smp enable -sname factorial -mnesia debug verbose

Such an argument line must start with %%! and the rest of the line will interpreted as arguments to the emulator.

特性2： 
 -mode(compile).

这个选项是在escript.erl这个模块处理的。默认情况下 escript是被解释执行的，如果你的脚本很复杂，那么效率估计会是瓶颈。这种情况下， 你可以通过这个选项来让escript来先编译你的模块成opcode, 在vm里面运行。

特性3：
 -d 选项 用来调试script的
 -c 编译执行
 -i 解释执行
 -s 只检查不执行
 root@nd-desktop:~#　escript -d ./factorial 10
 我们就可以看到 调试界面如下图

%%%
1) << <<(string:to_lower(C))/utf8>> || <<C/utf8>> <= Binary >>
2) unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(Binary)))

%%%
-type http_version() :: 'HTTP/1.1' | 'HTTP/1.0'.
-export_type([http_version/0]).

%%%
原始的or和and是不带”短路运算”操作的，而orelse和andalso是带短路运算操作的。

Express1 and Express2
Express1 andalso Express2

如果Express1 为假，and会继续判断Express2，然后整体判定为假，而andalso”短路”操作，直接判定整个表达式为假，从效率上来说，andalso会高一些

%%%
-record(g, {key, value}). 
ets:new(g, [public, named_table, {keypos, #g.key}]). 
ets:insert_new(g, #g{user_id, 0}). 

Uid = ets:update_counter(g, user_id). 
ets:insert_new(users, #user{id=Uid, name="Somebody"}.
%%%
[register(list_to_atom("pid" ++ integer_to_list(X)), spawn(fun() -> myFun() end)) || X <- lists:seq(1,10)].
code:which('user_default').

%%%
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

%%%
-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown} | {normal, timeout} | {error, atom()}.

%%%
#!/bin/sh
$erl +K true +P 10240000 -sname testserver -pa ebin -pa deps/*/ebin -s htmlfilesimple\
-eval "io:format(\"Server start with port 8000 Success!~n\")."

$erl -pa $MD/erlang -noshell -eval 'user_default:div618(1,2)' -s init stop

$erl -mnesia dir '"/home/sw2wolf/erlang/data"'
%%%
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
%%%
源代码安装
#wget https://elearning.erlang-solutions.com/binaries/sources/otp_src_R16B.tar.gz
#tar xvf otp_src_R16B.tar.gz
cd otp_src_R16B
./configure --prefix=/usr/local/erlang --enable-hipe --enable-threads --enable-smp-support --enable-kernel-poll
make
make install

添加到环境变量中(/etc/profile)
export ERL_HOME=/usr/local/erlang export PATH=$ERL_HOME/bin:$PATH

%%%
写好的erlang程序要放在服务器上跑，总不能一直终端打开erl的吧？ 应该是让erlang程序运行得像deamon那样，需要时候再连接上去操作。

erl启动使用  -detached 参数启动

这个参数可以让erl节点脱离了终端。

例如 ：   $erl -sname dp -detached

如何链接：

     $erl -sname dp2 -remsh dp@dp0304

如何退出：！！！超级要注意啊～～不能直接关掉退出

   ctrl +g 进入JCL模式

   输入  q  回车
%%%

dbg:tracer(), dbg:p(all, [call, timestamp]).
dbg:tpl(yaws_server, aloop, []).

dbg:tpl(Module, '_', []) is a good starting point

%Then hit the system with one client request. You should see some trace details on your Erlang console, including timestamps for the call in question. That function is recursive and it includes pretty much all of the server processing per request, so it's a quick way to help narrow down where the time is going.

To stop the tracing: dbg:stop_clear().

%%%

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

%%%
#! /usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -detached -pa /home/hakan/erl_libs/eflex/ebin

main([]) ->
    eflex:start();
main(["--help"]) ->
    AppDir = get_app_dir(),
    File = filename:join([AppDir, "README"]),
    case file:read_file(File) of
	{ok, Bin} ->
	    io:format("~s\n", [binary_to_list(Bin)]);
	{error, Reason} ->
	    io:format("~s: ~s\n", [File, file:format_error(Reason)]),
	    halt(3)
    end;
main(_) ->
    usage().

usage() ->
    FullName = escript:script_name(),
    BaseName = filename:basename(FullName),
    io:format("usage: ~s [--help]\n", [BaseName]),
    halt(1).

get_app_dir() ->
    case code:lib_dir(eflex) of
	{error, Reason} ->
	    io:format("Illegal application directory: ~p\n", [Reason]),
	    halt(2);
	AppDir ->
	    AppDir
    end.
