-module(msg_server).
-export([
    start_link/1,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-behaviour(gen_server).
-include("../include/records.hrl").
-record(client, {username, pass, socket}).

start_link(Socket) -> gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) -> {ok, #client{socket=Socket}}.

handle_call(Commend, _From, C) ->
    {reply, Commend, C}.

handle_cast(Msg, C) ->
    io:format("Unknow message ~p, ~n", [Msg]),
    {noreply, C}.

handle_info({tcp, _Socket, Bin}, C) ->
    Msg = binary_to_term(Bin),
    case Msg of
        {login, _, _} -> 
            NewClient = handle_msg(Msg, C#client.socket, C),
            {noreply, NewClient};
        _ -> 
            handle_msg(Msg, C#client.socket),
            {noreply, C}
    end;
handle_info({tcp_closed, _Socket}, C) ->
    {stop, normal, C}.

%% 终结
terminate(normal, _C) ->
    io:format("quit now~n"),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%% 私有函数
handle_msg({login, Username, Pass}, Socket, C) ->
    case user_service:login(Username, Pass, Socket) of
        login_success -> 
            send_msg(Socket, {login_success, Username}),
            C#client{username = Username, pass = Pass};
        {err, Reason} -> 
            send_msg(Socket, {err, Reason})
    end.

% handle_msg(showskl, _Socket, Chat) ->
% handle_msg(showets, _Socket, Chat) ->
% handle_msg({change_pass, Username, OldPass, NewPass}, Socket, Chat) ->
% handle_msg({kick, Username, Kuser}, _Socket, Chat) ->
% handle_msg({talk, User, Msg}, Socket, Chat) ->
% handle_msg({whisper, User, ToUser, Msg}, Socket, Chat) ->
% handle_msg(check_online, Socket, Chat) ->
% handle_msg({new_group, Username, GroupName}, Socket, Chat) ->
% handle_msg({join_group, GroupId, Username}, Socket, Chat) ->
% handle_msg({leave_group, GroupId, Username}, Socket, Chat) ->
% handle_msg({show_group, Username}, _Socket, Chat) ->
% handle_msg({group_speak, GroupId, Username, Msg}, Socket, Chat) ->
% handle_msg({get_rec, Username}, Socket, Chat) ->
handle_msg(Msg, Socket) ->
    io:format("Unexpected tcp message ~p, ~n", [Msg]),
    send_msg(Socket, {unexpected, Msg}).

%% 发送消息
send_msg(Socket, Msg) ->
    gen_tcp:send(Socket, term_to_binary(Msg)).
