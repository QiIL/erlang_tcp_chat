%%% tcp_server层
%%% 负责解码以及创建聊天服务进程
%%% module：tcp_server

-module(tcp_server).
-export([
    start_link/0, showets/0, stop/0, kick_all_user/0,
    clean_chat_rec/0, get_msg_server/0, get_chat_mg_pid/0,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-include("../include/records.hrl").
-record(server, {listen, msg_processes=[], cpid}).
-behaviour(gen_server).

%% 启动载入各种表
start_link() -> 
    %% 打开数据库
    {ok, CPid} = chat_mg:start_link(),
    {ok, Listen} = gen_tcp:listen(4000, [binary, {active, true}, {packet, 1}]),
    %% 用户表，在线数据
    ets:new(user_socket, [public, named_table]),
    %% 群组表, 建立在线数据
    ets:new(groups, [public, named_table]),
    Groups = db_tool:search(chat_group, #chat_group{_='_'}, [], ['$_']),
    create_group_ets(Groups),
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Listen, CPid], []),
    Pid.

showets() -> gen_server:call(?MODULE, showets).

%% 踢用户下线。
kick_all_user() -> gen_server:call(?MODULE, kick_all_user).

%% 获取其中一个msg_server
get_msg_server() -> gen_server:call(?MODULE, get_msg_server).
get_chat_mg_pid() -> gen_server:call(?MODULE, get_chat_mg_pid).

%% 清除聊天记录
clean_chat_rec() -> gen_server:cast(?MODULE, clean_chat_rec, 60000).

stop() -> gen_server:call(?MODULE, stop).

init([Listen, CPid]) -> 
    self() ! wait_connect,
    {ok, #server{listen=Listen, cpid=CPid}}.

handle_call(clean_chat_rec, _From, S) ->
    R = #chat_record{id='$1', time='$2', _='_'},
    Recs = db_tool:search(chat_record, R, [], ['$1']),
    io:format("~p~n", [Recs]),
    mnesia:clear_table(chat_record),
    io:format("ok delete~n"),
    {reply, ok, S};
handle_call(get_msg_server, _From, S=#server{msg_processes=Mp}) ->
    if Mp =:= [] -> {reply, [], S};
        true -> 
            [{_, Mid} | _] = S#server.msg_processes,
            {reply, Mid, S}
    end;
handle_call(get_chat_mg_pid, _From, S=#server{cpid=Cpid}) ->
    {reply, Cpid, S};
handle_call(kick_all_user, _From, S) ->
    io:format("Pid's length is ~p~n", [length(S#server.msg_processes)]),
    kick_all(S#server.msg_processes),
    {reply, ok, S#server{msg_processes=[]}};
handle_call(stop, _From, S) ->
    io:format("I am stopping"),
    {stop, normal, ok, S};
handle_call(Commend, _From, S) ->
    io:format("Unkown commend: ~p~n", [Commend]),
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
            Ref = erlang:monitor(process, Pid),
            gen_tcp:controlling_process(Socket, Pid),
            self() ! wait_connect,
            {noreply, S#server{msg_processes=[{Ref, Pid} | S#server.msg_processes]}};
        Error ->
            {stop, Error, ok, S}
    end;
handle_info({inet_async, _L, _Ref, Error}, S) ->
    {stop, Error, ok, S};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#server{msg_processes=Mp}) ->
    {noreply, S#server{msg_processes=lists:keydelete(Ref, 1, Mp)}};
handle_info(Msg, S) ->
    io:format("Unexpected Msg in tcp server: ~p~n", [Msg]),
    {noreply, S}.

terminate(normal, _S) ->
    ets:delete(user_socket),
    ets:delete(groups),
    chat_mg:stop(),
    io:format("tcp server close now~n"),
    ok.
% terminate(Error, _S) ->
%     ets:delete(user_socket),
%     ets:delete(groups),
%     chat_mg:stop(),
%     io:format("tcp server err because: ~p~n", [Error]),
%     ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

create_group_ets([]) -> ok;
create_group_ets([{chat_group, Gid, Gname, _, _} | T]) ->
    ets:insert(groups, {Gid, Gname, []}),
    create_group_ets(T).

kick_all([]) -> ok;
kick_all([{_, Pid} | T]) ->
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