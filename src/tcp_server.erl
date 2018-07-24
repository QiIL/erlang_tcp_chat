%%% tcp_server层
%%% 负责解码以及创建聊天服务进程
%%% module：tcp_server

-module(tcp_server).
-export([
    start_link/0, showets/0, stop/0, kick_all_user/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-include("../include/records.hrl").
-record(server, {listen, msg_processes=[]}).
-behaviour(gen_server).

%% 启动载入各种表
start_link() -> 
    %% 打开数据库
    mmnesia:start_link(),
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}]),
    %% 用户表，在线数据
    ets:new(user_socket, [public, named_table]),
    %% 群组表, 建立在线数据
    ets:new(groups, [public, named_table]),
    Groups = mmnesia:search(chat_group, #chat_group{_='_'}, [], ['$_']),
    create_group_ets(Groups),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Listen], []).

showets() -> gen_server:call(?MODULE, showets).

%% 踢用户下线。
kick_all_user() -> io:format("lalala"), gen_server:call(?MODULE, kick_all_user).

stop() -> gen_server:call(?MODULE, stop).

init([Listen]) -> 
    self() ! wait_connect,
    {ok, #server{listen=Listen}}.

handle_call(kick_all_user, _From, S) ->
    io:format("Pids is ~p~n", [S#server.msg_processes]),
    kick_all(S#server.msg_processes),
    {reply, ok, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(Commend, _From, S) ->
    io:format("Unkown commend: ~p~n", Commend),
    {reply, Commend, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(wait_connect, S) ->
    {ok, Socket} = gen_tcp:accept(S#server.listen),
    {ok, Pid} = msg_server:start_link(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    self() ! wait_connect,
    {noreply, S#server{msg_processes=[Pid | S#server.msg_processes]}};
handle_info(Msg, S) ->
    io:format("Unexpected Msg in tcp server: ~p~n", [Msg]),
    {noreply, S}.

terminate(normal, _S) ->
    ets:delete(user_socket),
    ets:delete(groups),
    mmnesia:stop(),
    io:format("tcp server close now~n"),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

create_group_ets([]) -> ok;
create_group_ets([{chat_group, Gid, Gname, _, _} | T]) ->
    ets:insert(groups, {Gid, Gname, []}),
    create_group_ets(T).

kick_all([]) -> ok;
kick_all([Pid | T]) ->
    io:format("Pid is: ~p~n", [Pid]),
    msg_server:stop(Pid),
    kick_all(T).