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

%% 1. 启动Erlang master节点的模块： 
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

这里的要点是：集群间所有节点（至少是master对所有slave节点）之间都要能实现“相互”无密码输入的ssh登录 
我想无密码输入的原理可能是： 机器A生成RSA key，有一个公开的密钥，别的机器B要想无密码ssh这同机器，B必须在本地拥有A的公开密钥