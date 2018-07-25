%%% tcp_server层
%%% 负责解码以及创建聊天服务进程
%%% module：tcp_server

-module(tcp_server).
-export([
    start_link/0, showets/0, stop/0, kick_all_user/0,
    begin_timestamp/0, end_timestamp/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-include("../include/records.hrl").
-record(server, {listen, msg_processes=[], btimestamp, etimestamp}).
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
kick_all_user() -> gen_server:call(?MODULE, kick_all_user).

%% 计时用
begin_timestamp() -> gen_server:call(?MODULE, begin_timestamp).
end_timestamp() -> gen_server:call(?MODULE, end_timestamp).

stop() -> gen_server:call(?MODULE, stop).

init([Listen]) -> 
    self() ! wait_connect,
    {ok, #server{listen=Listen}}.

handle_call(begin_timestamp, _From, S) ->
    {reply, ok, S#server{btimestamp=os:timestamp()}};
handle_call(end_timestamp, _From, S) ->
    {Bm, Bs, Bms} = S#server.btimestamp,
    {Em, Es, Ems} = os:timestamp(),
    Result = ((Em * 1000000 + Es) * 1000000 + Ems) - ((Bm * 1000000 + Bs) * 1000000 + Bms),
    {reply, Result, S#server{etimestamp=Result}};
handle_call(kick_all_user, _From, S) ->
    io:format("Pids is ~p~n", [S#server.msg_processes]),
    kick_all(S#server.msg_processes),
    {reply, ok, S#server{msg_processes=[]}};
handle_call(stop, _From, S) ->
    io:format("I am stopping"),
    {stop, normal, ok, S};
handle_call(Commend, _From, S) ->
    io:format("Unkown commend: ~p~n", Commend),
    {reply, Commend, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(wait_connect, S) ->
    case prim_inet:async_accept(S#server.listen, -1) of
        {ok, _Ref} -> {noreply, S};
        Error -> {stop, Error, ok, S}
    end;
handle_info({inet_async, L, _Ref, {ok, Socket}}, S) ->
	case accept_opts(L, Socket) of
        {ok, Socket} ->
            inet_db:register_socket(Socket, inet_tcp),
            {ok, Pid} = msg_server:start_link(Socket),
            gen_tcp:controlling_process(Socket, Pid),
            self() ! wait_connect,
            {noreply, S#server{msg_processes=[Pid | S#server.msg_processes]}};
        Error ->
            {stop, Error, ok, S}
    end;
handle_info({inet_async, _L, _Ref, Error}, S) ->
    {stop, Error, ok, S};
handle_info(Msg, S) ->
    io:format("Unexpected Msg in tcp server: ~p~n", [Msg]),
    {noreply, S}.

terminate(normal, _S) ->
    ets:delete(user_socket),
    ets:delete(groups),
    mmnesia:stop(),
    io:format("tcp server close now~n"),
    ok;
terminate(Error, _S) ->
    ets:delete(user_socket),
    ets:delete(groups),
    mmnesia:stop(),
    io:format("tcp server err because: ~p~n", [Error]),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

create_group_ets([]) -> ok;
create_group_ets([{chat_group, Gid, Gname, _, _} | T]) ->
    ets:insert(groups, {Gid, Gname, []}),
    create_group_ets(T).

kick_all([]) -> ok;
kick_all([Pid | T]) ->
    msg_server:stop(Pid),
    kick_all(T).

%% prim_inet accept_opts
accept_opts(L, S) ->
    case prim_inet:getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case prim_inet:getopts(L, [tclass]) of
			{ok, []} ->
			    {ok, S};
			{ok, TClassOpts} ->
			    case prim_inet:setopts(S, TClassOpts) of
				ok ->
				    {ok, S};
				Error -> prim_inet:close(S), Error
			    end
		    end;
		Error -> prim_inet:close(S), Error
	    end;
	Error ->
	    prim_inet:close(S), Error
    end.