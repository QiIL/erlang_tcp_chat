-module(client).
-export([
    start_link/0, stop/1,
    login/3, talk/2, quit/1, showets/1,
    showskl/1, change_pass/3, kick/2,
    whisper/3, check_online/1, new_group/2, join_group/2,
    show_group/1, leave_group/2, group_speak/3,
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3,
    get_rec/1
]).
-record(client, {username, pass, socket, min=time_min(), chat_num=0}).
-behaviour(gen_server).

% 获取客户端pid
start_link() -> gen_server:start_link(?MODULE, [], []).
stop(Pid) -> gen_server:call(Pid, stop).

login(Pid, Username, Pass) -> gen_server:call(Pid, {login, Username, Pass}).
talk(Pid, Msg) -> gen_server:call(Pid, {talk, Msg}).
quit(Pid) -> gen_server:call(Pid, quit).
showets(Pid) -> gen_server:call(Pid, showets).
showskl(Pid) -> gen_server:call(Pid, showskl).
change_pass(Pid, OldPass, NewPass) -> gen_server:call(Pid, {change_pass, OldPass, NewPass}).
kick(Pid, Username) -> gen_server:call(Pid, {kick, Username}).
whisper(Pid, Username, Msg) -> gen_server:call(Pid, {whisper, Username, Msg}).
check_online(Pid) -> gen_server:call(Pid, check_online).
new_group(Pid, GroupName) -> gen_server:call(Pid, {new_group, GroupName}).
join_group(Pid, GroupId) -> gen_server:call(Pid, {join_group, GroupId}).
show_group(Pid) -> gen_server:call(Pid, show_group).
leave_group(Pid, GroupId) -> gen_server:call(Pid, {leave_group, GroupId}).
group_speak(Pid, GroupId, Msg) -> gen_server:call(Pid, {group_speak, GroupId, Msg}).
get_rec(Pid) -> gen_server:call(Pid, get_rec).

init([]) -> 
    {ok, Socket} = gen_tcp:connect("localhost", 4000, [binary]),
    {ok, #client{socket=Socket}}.

%% 同步调用
handle_call({login, Username, Pass}, _From, Client) ->
    send_server(Client#client.socket, {login, Username, Pass}),
    {reply, ok, Client#client{username=Username, pass=Pass}};
handle_call(showskl, _From, Client) ->
    send_server(Client#client.socket, showskl),
    {reply, ok, Client};
handle_call(showets, _From, Client) ->
    send_server(Client#client.socket, showets),
    {reply, ok, Client};
handle_call({change_pass, OldPass, NewPass}, _From, Client) ->
    send_server(Client#client.socket, {change_pass, Client#client.username, OldPass, NewPass}),
    {reply, ok, Client};
handle_call({kick, Username}, _From, Client) ->
    send_server(Client#client.socket, {kick, Client#client.username, Username}),
    {reply, ok, Client};
handle_call({talk, Msg}, _From, Client) ->
    {{_, _, _}, {_, NewMins, _}} = calendar:now_to_local_time(os:timestamp()),
    case {Client#client.min =:= NewMins, Client#client.chat_num < 60000} of
        {true, true} -> 
            send_server(Client#client.socket, {talk, Client#client.username, Msg}),
            {reply, Msg, Client#client{min=NewMins, chat_num=Client#client.chat_num+1}};
        {true, false} ->
            io:format("you talk too fast, emmmmmm you should take a coffee and have a rest~n"),
            {reply, Msg, Client#client{min=NewMins}};
        {false, _} ->
            send_server(Client#client.socket, {talk, Client#client.username, Msg}),
            {reply, Msg, Client#client{min=NewMins, chat_num=0}}
    end;
handle_call({whisper, ToUser, Msg}, _From, Client) ->
    send_server(Client#client.socket, {whisper, Client#client.username, ToUser, Msg}),
    {reply, Msg, Client};
handle_call(check_online, _From, Client) ->
    send_server(Client#client.socket, check_online),
    {reply, ok, Client};
handle_call({new_group, GroupName}, _From, Client) ->
    send_server(Client#client.socket, {new_group, Client#client.username, GroupName}),
    {noreply, Client};
handle_call({join_group, GroupId}, _From, Client) ->
    send_server(Client#client.socket, {join_group, GroupId, Client#client.username}),
    {noreply, Client};
handle_call({leave_group, GroupId}, _From, Client) ->
    send_server(Client#client.socket, {leave_group, GroupId, Client#client.username}),
    {noreply, Client};
handle_call(show_group, _From, Client) ->
    send_server(Client#client.socket, {show_group, Client#client.username}),
    {noreply, Client};
handle_call({group_speak, GroupId, Msg}, _From, Client) ->
    send_server(Client#client.socket, {group_speak, GroupId, Client#client.username, Msg}),
    {reply, Msg, Client};
handle_call(get_rec, _From, Client) ->
    send_server(Client#client.socket, {get_rec, Client#client.username}),
    {reply, ok, Client};
handle_call(stop, _From, Client) ->
    {stop, normal, ok, Client}.

%% 异步调用
handle_cast(Msg, Client) ->
    io:format("Unknown commend: ~p~n", [Msg]),
    {noreply, Client}.

%% tcp消息以及自己发给自己的消息
handle_info({tcp, _Socket, _Bin}, Client) ->
    % decode(Bin, Socket),
    {noreply, Client};
handle_info({tcp_closed, _Socket}, Client) ->
    {stop, normal, Client};
handle_info({err, Reason}, Client) ->
    {stop, {err, Reason}, Client};
handle_info({squit, Reason}, Client) ->
    {stop, {squit, Reason}, Client};
handle_info(Msg, Client) ->
    io:format("Unexpected messge: ~p~n", [Msg]),
    {noreply, Client}.

%% 关闭函数
terminate({err, Reason}, Client) ->
    io:format("some error happend the reason is : ~p~n", [Reason]),
    gen_tcp:close(Client#client.socket);
terminate({squit, Reason}, Client) ->
    io:format("server force to exit because : ~p~n", [Reason]),
    gen_tcp:close(Client#client.socket);
terminate(normal, Client) ->
    io:format("Goodbye my friend!~n"),
    gen_tcp:close(Client#client.socket).

%% 热更函数
code_change(_OldVsn, Client, _Extra) ->
    {ok, Client}.

%% 私有函数
%% 解码
decode(Bin, Socket) ->
    deal(binary_to_term(Bin), Socket).

%% tcp消息处理
deal({login_success, User}, _) ->
    io:format("登陆成功!（~p）~n", [User]);
deal({boardcast, User, Msg}, _) ->
    io:format("~p: ~p~n", [User, Msg]);
deal({boardcast, GroupName, User, Msg}, _) ->
    io:format("[~p][~p]: ~p~n", [GroupName, User, Msg]);
deal({change_pass_success, Username}, _) ->
    io:format("change pass success! (~p)", [Username]),
    self() ! {squit, "Password changed"};
deal({whisper, Username, Msg}, _) ->
    io:format("Whisper(~p): ~p~n", [Username, Msg]);
deal({err, Reason}, _Socket) ->
    io:format("~p~n", [Reason]),
    self() ! {err, Reason};
deal({squit, Reason}, _Socket) ->
    io:format("~p~n", [Reason]),
    self() ! {squit, Reason};
deal({recs, Recs}, _Socket) ->
    output(Recs);
deal(Others, _) ->
    io:format("other: ~p~n", [Others]).

%% 发送tcp消息
send_server(Socket, Msg) ->
    gen_tcp:send(Socket, term_to_binary(Msg)).

time_min() ->
    {{_, _, _}, {_, Min, _}} = calendar:now_to_local_time(os:timestamp()),
    Min.

%% 输出聊天记录
output([]) -> ok;
output([{chat_record, _Id, User, Type, Target, Timestamp, Msg} | T]) ->
    DateTime = timestamp_to_datetime(Timestamp),
    io:format("[~p][~p][~p]: ~p (~p)~n", [Type, User, Target, Msg, DateTime]),
    output(T).

timestamp_to_datetime(Timestamp) ->
    {{Y, M, D}, {H, Min, S}} = calendar:gregorian_seconds_to_datetime(Timestamp +
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    {{Y, M, D}, {(H + 8) rem 24, Min, S}}.