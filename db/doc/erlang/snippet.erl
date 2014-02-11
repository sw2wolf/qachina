
%%%%%%
Erlang集群节点集合的启动
一、配置SSH客户端：无需密码的连接 
1. SSH客户端RSA key授权 
i). 生成SSH RSA key： 
ssh-keygen -t rsa 
全回车取缺省，将在~/.ssh目录下将生成id_rsa和id_rsa.pub俩文件 

ii) 将生产的公共RSA key拷贝到目标机器上： 
a).将id_rsa.pub文件拷贝到目标机器上： 
scp ~/.ssh/id_rsa.pub userid@ssh2_server:id_rsa.pub 

b). ssh登陆目标机器，在目标机器的~/.ssh目录下生成文件内容： 
cat id_rsa.pub >>$HOME/.ssh/authorized_keys 
删掉id_rsa.pub 

以上两步命令可以用一条命令实现： 
ssh-copy-id ssh2-server 

本质上都是把master上生成的id_rsa.pub文件拷贝到slave机器./ssh目录下，同时改名为authorized_keys 
这一步完成后，从master机器上登录slave机器直接 
ssh slave 
就行了，不用输入密码。 

同样的，slave到master也得这样：不用输入密码直接ssh 

集群内所有的机器相互之间都要这样实现无密码输入就能直接ssh 

2. 在启动master节点的那个机器上，启动SSH-agent软件并添加你的用户ID 
这样每次连接时就不需要passphrase了，办法是让ssh-agent记住你 
i) 首先确认你的机器上有ssh-agent在运行（一般都没有） 
ps aux|grep ssh-agent 

查看ssh-agent运行在当前窗口管理器（或者shell）下 
pstree 

如果不是，在shell中创建一个ssh-agent会话： 
ssh-agent screen 
这回启动了一个带ssh-agent的screen会话窗口，以后这个screen控制台内的所有命令都要经过ssh-agent 

ii) 添加你的ID 
ssh-add 

你可以查看当前ssh-agent中已添加的ID 
ssh-add -l 
也可以删除一个ID 
ssh-add -d 

3. 集群的路由进出 
当建立集群后，经常需要访问作为网关（或者负载均衡器）的前端计算机。要访问其它节点，你需要告诉作为网管的机器将你的请求路由到机器节点中。 

例如，假设你的网关是80.65.232.137。控制用的计算机在集群之外，这个机器是操作员用于控制集群的计算机。集群内部网络是192.0.0.0。在你的客户计算机上，启动命令： 
route add -net 192.0.0.0 gw 80.65.232.137 netmask 255.255.255.0 
这个命令需要确保网关计算机上IP forwarding 可用 

要确保正确的路由，可以维护一个常用的/etc/hosts文件，该文件中记录了你的集群中的所有计算机，例子中，集群有7台计算机组成. 
10.9.195.12   controler 
80.65.232.137 gateway 
192.0.0.11    eddieware 
192.0.0.21    yaws1 
192.0.0.22    yaws2 
192.0.0.31    mnesia1 
192.0.0.32    mnesia2 

也可以设置DNS服务器，但对小网络来说/etc/hosts文件就已经足够了 

二、启动Erlang master节点并建立Erlang集群 
一旦通过SSH连接集群不再提示输入密码后建立一个Erlang集群将变得非常容易。 

1. 启动Erlang master节点的模块： 
Erlang代码 
-module(snippet).   
-export([slaves/1]).   
  
%% Argument:   
%% Hosts: List of hostname (string)   
slaves([]) ->  ok;   
slaves([Host|Hosts]) ->   
  Args = erl_system_args(),   
  NodeName = "cluster",   
  {ok, Node} = slave:start_link(Host, NodeName, Args),   
  io:format("Erlang node started = [~p]~n", [Node]),   
  slaves(Hosts).   
  
erl_system_args()->   
  Shared = case init:get_argument(shared) of   
    error -> " ";   
    {ok,[[]]} -> " -shared "  
  end,   
  lists:append(["-rsh ssh -setcookie ",   
                atom_to_list(erlang:get_cookie()),   
                Shared, " +Mea r10b "]).   
  
%% Do not forget to start erlang with a command like:   
%% erl -rsh ssh -sname clustmaster 

%启动master 
$ erl -rsh ssh -sname demo   
Erlang (BEAM) emulator version 5.3 [source] [hipe]   
Eshell V5.3 (abort with ^G)   
(demo@controler)1> cluster:slaves(["gateway", "yaws1", "yaws2", "mnesia1", "mnesia2", "eddieware"]).   
Erlang node started = [cluster@gateway]   
Erlang node started = [cluster@yaws1]   
Erlang node started = [cluster@yaws2]   
Erlang node started = [cluster@mnesia1]   
Erlang node started = [cluster@mnesia2]   
Erlang node started = [cluster@eddieware]   
ok  

%% 这里的要点是：集群间所有节点（至少是master对所有slave节点）之间都要能实现“相互”无密码输入的ssh登录 
%% 我想无密码输入的原理可能是： 机器A生成RSA key，有一个公开的密钥，别的机器B要想无密码ssh这同机器，B必须在本地拥有A的公开密钥

%%%%%%
SchedId      = erlang:system_info(scheduler_id),
SchedNum     = erlang:system_info(schedulers),
ProcCount    = erlang:system_info(process_count),
ProcLimit    = erlang:system_info(process_limit),
ProcMemUsed  = erlang:memory(processes_used),
ProcMemAlloc = erlang:memory(processes),
MemTot       = erlang:memory(total),
io:format("abormal termination: "
          "~n   Scheduler id:                         ~p"
          "~n   Num scheduler:                        ~p"
          "~n   Process count:                        ~p"
          "~n   Process limit:                        ~p"
          "~n   Memory used by erlang processes:      ~p"
          "~n   Memory allocated by erlang processes: ~p"
          "~n   The total amount of memory allocated: ~p"
          "~n~p",
          [SchedId, SchedNum, ProcCount, ProcLimit,
           ProcMemUsed, ProcMemAlloc, MemTot, Reason]),

%%%%%%
#!/bin/bash

# This is a unix wrapper around the erlang vm. It provides the following functionality:
#
# * Spawns in foreground
# * Handle SIGHUP and call RELOADFUNC
# * Handle SIGTERM SIGQUIT and SIGINT telling to the vm to quit
# * Dies if the vm dies (for example kernel killing because out of memory)
#
# Forks and improvements are welcome.
#

export APPNAME=my_app
export BASEDIR=`readlink -f $(dirname $0)/..`
export HOME=$BASEDIR
export APPDIR=$BASEDIR/apps/$APPNAME
export ENV=production
export PORT=18080
export SERVICE=my_service_name
export NODE="$SERVICE@`hostname -f`"
export CONFIG_FILE=$APPDIR/priv/$ENV.config
export COOKIE=sbs
export RELOAD="my_module my_func [my_args]"

cd $BASEDIR

reload(){
	echo Reloading the app..
	erl_call -c $COOKIE -a "$RELOAD" -n ${NODE}
}
trap reload SIGHUP

stop(){
        echo Stopping the app
        erl_call -c $COOKIE -q -n ${NODE}
}
trap stop SIGQUIT SIGINT SIGTERM


# Maybe you should adapt this to your needs.
exec erl -pa deps/*/ebin apps/*/ebin \
        -boot start_sasl \
        -sasl sasl_error_logger '{file, "./log/sasl.log"}' \
        -s ${APPNAME}_app \
        -noinput \
        +K true \
        +A 4 \
        +swt very_low \
        -$APPNAME environment $ENV \
        -$APPNAME port $PORT \
        -config "${CONFIG_FILE}" \
        -setcookie $COOKIE \
        -name $NODE $* &
pid=$!
sleep 0


echo Controlling the erlang VM $NODE running in $pid from $$. Waiting...
while kill -0 $pid ; do wait $pid ; done

%%%
-module(snippet). 

-export([add_microsec/2, microsleep/1, test/0]). 

-define(JITTER, 5). 

add_microsec(Micro, {Mega0, Sec0, Micro0}) -> 
    Micro1 = Micro0 + Micro, 
    Sec1 = Sec0 + (Micro1 div 1000000), 
    Mega1 = Mega0 + (Sec1 div 1000000), 
    {Mega1, (Sec1 rem 1000000), (Micro1 rem 1000000)}. 

busywait_until(Target, Loops) -> 
    case now() of 
        Now when Now >= Target -> 
            {Now, Loops}; 
        _ -> 
            erlang:yield(), 
            busywait_until(Target, 1 + Loops) 
    end. 

microsleep(MicroSec) -> 
    Target = add_microsec(MicroSec, now()), 
    AdjMsec = MicroSec - ?JITTER, 
    case AdjMsec > 10000 of 
        true -> 
            timer:sleep(AdjMsec div 1000); 
        false -> 
            ok 
    end, 
    {Finish, Loops} = busywait_until(Target, 1), 
    {timer:now_diff(Finish, Target), Loops}. 

test() -> 
    {Accuracy, Loops} = microsleep(16 * 1000), 
    io:format("Jitter: ~p ms Iterations: ~p~n", [Accuracy, Loops]).

%%%
eval(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value,_} = erl_eval:exprs(Parsed,[]),
    Value.

%%%
trace(X) -> spawn(fun() -> io:format("~p~n",[X]) end).
trace(X,Y) -> spawn(fun() -> io:format("~p: ~p~n",[X,Y]) end).
traceBinary(X) -> spawn(fun() -> io:format("~p~n",[b2h(X)]) end).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

b2h(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
h2b(String) -> << << (erlang:list_to_integer([Char], 16)):4/integer >> || Char <- String >>.
t(String)-> h2b(String).
txt(Bin) -> [X || <<X>> <= Bin,X > 32, X < 127, X =/= 45].

b2s(Bin) ->  b2s1(binary_to_list(Bin),[]).
b2s1([],Str) -> lists:reverse(Str);
b2s1([H|T],Str) -> 
  case H > 32 andalso H < 127 andalso H =/= 45 of      
   true -> b2s1(T,[H|Str]);
   false -> b2s1(T,[46,46|Str])
  end.
  
pmap(F, L, Parent) -> [receive {Pid, Res} -> Res end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

timer(Time,Fun) -> spawn(fun() -> receive after Time -> ?MODULE:Fun() end end).

%% u:floor(-4.7).
%% Output: -5
floor(X) when X < 0 ->   
  T = trunc(X),   
  case (X - T) =:= 0 of       
       true -> T;
       false -> T - 1   
  end;
floor(X) ->  trunc(X).

%%添加协议头
addLen(Bin) ->   
  Len=erlang:size(Bin)+2,   
  <<Len:16,Bin/binary>>.

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
    {X, _} = eval(re:replace(Exp, "\\s", "", [{return, list},global]), 0),
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
 
%The evaluator uses a simple infix scheme that doesn't care about operator precedence, but does support brackets and parentheses alike. Thus, ((9+1)*2)+2+2 is evaluated as: 
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
%-module(snippet).

%-compile([native, {hipe, [o3]}]).

%-export([bin_to_hex/1]).

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
          ...
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

%这个选项是在escript.erl这个模块处理的。默认情况下 escript是被解释执行的，如果你的脚本很复杂，那么效率估计会是瓶颈。这种情况下， 你可以通过这个选项来让escript来先编译你的模块成opcode, 在vm里面运行。

特性3：
 -d 选项 用来调试script的
 -c 编译执行
 -i 解释执行
 -s 只检查不执行
#escript -d ./factorial 10

%%%
1) << <<(string:to_lower(C))/utf8>> || <<C/utf8>> <= Binary >>
2) unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(Binary)))

%%%
-type http_version() :: 'HTTP/1.1' | 'HTTP/1.0'.
-export_type([http_version/0]).

%%%
原始的or和and是不带"短路运算"操作的，而orelse和andalso是带短路运算操作的。

Express1 and Express2
Express1 andalso Express2

%如果Express1 为假，and会继续判断Express2，然后整体判定为假，而andalso”短路”操作，直接判定整个表达式为假，从效率上来说，andalso会高一些

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
%源代码安装
#wget https://elearning.erlang-solutions.com/binaries/sources/otp_src_R16B.tar.gz
#tar xvf otp_src_R16B.tar.gz
cd otp_src_R16B
./configure --prefix=~/erlang --enable-hipe --enable-threads --enable-smp-support --enable-kernel-poll
make
make install

%添加到环境变量中(/etc/profile)
export ERL_HOME=~/erlang export PATH=$ERL_HOME/bin:$PATH

%%%
%写好的erlang程序要放在服务器上跑，总不能一直终端打开erl的吧？ 应该是让erlang程序运行得像deamon那样，需要时候再连接上去操作。

%这个参数可以让erl节点脱离了终端。

例如： $erl -sname dp -detached

%如何链接：

     $erl -sname dp2 -remsh dp@dp0304

%如何退出：！！！超级要注意啊～～不能直接关掉退出

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
