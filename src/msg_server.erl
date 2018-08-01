-module(msg_server).
-export([
    start_link/1, stop/1,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).
-behaviour(gen_server).
-include("../include/records.hrl").
-record(client, {username, pass, socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

stop(Pid) -> gen_server:call(Pid, stop).

init([Socket]) -> {ok, #client{socket=Socket}}.

handle_call(stop, _From, C) ->
    {stop, normal, ok, C};
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
terminate({timeout, Reason}, _C) ->
    io:format("some place timeout beacuse: ~p~n", [Reason]),
    ok;
terminate(normal, C) ->
    io:format("quit now~n"),
    ets:delete(user_socket, C#client.username),
    user_service:deal_user_group(C#client.username, nil, C#client.socket, minus),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%%% 私有函数
%%% 用户相关
%% 登陆
handle_msg({login, Username, Pass}, Socket, C) ->
    case user_service:login(Username, Pass, Socket) of
        {login_success, []} ->
            send_msg(Socket, {login_success, Username}),
            C#client{username = Username, pass = Pass};
        {login_success, OldSocket} ->
            send_msg(OldSocket, {squit, "your account is logining in other place"}),
            send_msg(Socket, {login_success, Username}),
            C#client{username = Username, pass = Pass};
        {err, Reason} ->
            deal_error(login, Reason, Socket)
    end.
%% 改密码
handle_msg({change_pass, Username, OldPass, NewPass}, Socket) ->
    case user_service:change_pass(Username, OldPass, NewPass, Socket) of
        success ->
            send_msg(Socket, {change_pass_success, Username}),
            send_msg(Socket, "Pass change success!");
        {err, Reason} ->
            deal_error(change_pass, Reason, Socket)
    end;
%% 踢人下线
handle_msg({kick, Username, Kuser}, Socket) ->
    case user_service:kick(Username, Kuser) of
        {success, Ksocket} ->
            send_msg(Ksocket, {squit, "your was kick by manager"}),
            send_msg(Socket, "Kick " ++ atom_to_list(Kuser) ++ "'s ass success!");
        {err, Reason} ->
            deal_error(kick, Reason, Socket)
    end;

%%% 服务消息相关
%% 在线人数
handle_msg(check_online, Socket) ->
    OnlineNum = server_msg_service:check_online(),
    send_msg(Socket, "the online number is: " ++ integer_to_list(OnlineNum));
%% 获取聊天记录
handle_msg({get_rec, Username}, Socket) ->
    Recs = db_tool:get_record(Username),
    send_msg(Socket, {recs, Recs});

%%% 聊天相关
%% 世界说话
handle_msg({talk, User, Msg}, Socket) ->
    chat_mg:save_rec(broadcast, User, all, Msg),
    broadcast(ets:lookup_element(groups, 1, 3), Socket, world, User, Msg);
%% 私聊
handle_msg({whisper, User, ToUser, Msg}, Socket) ->
    case user_service:check_user_exist(ToUser) of
        {err, Reason} -> send_msg(Socket, Reason);
        success ->
            chat_mg:save_rec(whisper, User, ToUser, Msg),
            ToSocket = user_service:get_socket(ToUser),
            send_msg(ToSocket, {whisper, User, Msg})
    end;

%% 群组相关
%% 新建群
handle_msg({new_group, Username, GroupName}, Socket) ->
    group_service:new(GroupName, Username, Socket),
    send_msg(Socket, "new Group ok!");
%% 加入群
handle_msg({join_group, GroupId, Username}, Socket) ->
    case group_service:join(GroupId, Username, Socket) of
        {err, Reason} -> send_msg(Socket, Reason);
        true -> send_msg(Socket, "Join group success!")
    end;
%%离开群
handle_msg({leave_group, GroupId, Username}, Socket) ->
    case group_service:leave(GroupId, Username, Socket) of
        {err, Reason} -> send_msg(Socket, Reason);
        true -> send_msg(Socket, "Leave group success!")
    end;
%% 列出群
handle_msg({show_group, Username}, _Socket) ->
    UserGroups = db_tool:get_group(Username, nil, username),
    io:format("~p~n", [UserGroups]);
%% 群聊
handle_msg({group_speak, GroupId, Username, Msg}, Socket) ->
    case group_service:get_group_socket(GroupId, Username) of
        {err, Reason} -> send_msg(Socket, Reason);
        [{_, Gname, GroupSockets}] ->
            chat_mg:save_rec(group_talk, Username, Gname, Msg),
            broadcast(GroupSockets, Socket, Gname, Username, Msg)
    end;
handle_msg(Msg, Socket) ->
    io:format("Unexpected tcp message ~p, ~n", [Msg]),
    send_msg(Socket, {unexpected, Msg}).

%% 发送消息
send_msg(Socket, Msg) ->
    gen_tcp:send(Socket, term_to_binary(Msg)).

deal_error(From, Reason, Socket) ->
    io:format("msg_server error From: ~p Reason: ~p~n", [From, Reason]),
    send_msg(Socket, {err, Reason}).

%% 广播
broadcast([], _, _, _, _) ->
    ok;
broadcast([H|T], Socket, Group, User, Str) ->
    gen_tcp:send(H, term_to_binary({boardcast, Group, User, Str})),
    broadcast(T, Socket, Group, User, Str).
