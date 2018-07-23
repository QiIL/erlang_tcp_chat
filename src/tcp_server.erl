%%% tcp_server层
%%% 负责解码以及创建聊天服务进程
%%% module：tcp_server

-module(tcp_server).
-export([
    start_link/0, showets/0, stop/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-include("../include/records.hrl").
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
    gen_server:start_link(?MODULE, [Listen], []).

showets() -> gen_server:call(?MODULE, showets).

stop() -> gen_server:call(?MODULE, stop).

init([Listen]) -> 
    self() ! wait_connect,
    {ok, Listen}.

handle_call(stop, _From, Listen) ->
    {stop, normal, ok, Listen};
handle_call(Commend, _From, Listen) ->
    io:format("Unkown commend: ~p~n", Commend),
    {reply, Commend, Listen}.

handle_cast(_Msg, Listen) ->
    {noreply, Listen}.

handle_info(wait_connect, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, Pid} = msg_server:start_link(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    self() ! wait_connect,
    {noreply, Listen};
handle_info(Msg, Listen) ->
    io:format("Unexpected Msg in tcp server: ~p~n", [Msg]),
    {noreply, Listen}.

terminate(normal, _Listen) ->
    ets:delete(user_socket),
    ets:delete(groups),
    mmnesia:stop(),
    io:format("tcp server close now~n"),
    ok.

code_change(_OldVsn, Listen, _Extra) ->
    {ok, Listen}.

create_group_ets([]) -> ok;
create_group_ets([{chat_group, Gid, Gname, _, _} | T]) ->
    ets:insert(groups, {Gid, Gname, []}),
    create_group_ets(T).
