-module(user_service).
-export([
    login/3, change_pass/4, kick/2,
    check_user_exist/1, get_socket/1
]).

%% 登陆
% - 验证密码
% - 用户socket记录
% - 群组上线
% - 顶掉前面(把OldSocket返回)
login(Username, Password, Socket) ->
    case db_tool:search_user(Username) of
        [] -> {err, "user isn't exist"};
        [{chat_user, Username, Pass}] ->
            case Password =:= Pass of
                false -> {err, "Username or Password is wrong"};
                true ->
                    OldSocket = record_socket(Username, Socket),
                    deal_user_group(Username, OldSocket, Socket, add),
                    {login_success, OldSocket}
            end
    end.

%% 修改密码
% - 验证旧密码
% - 改新的密码
% - 清除socket
change_pass(Username, OldPass, NewPass, Socket) ->
    case db_tool:search_user(Username) of
        [] -> {err, "user isn't exist"};
        [{chat_user, Username, Pass}] ->
            case OldPass =:= Pass of
                false -> {err, "Old Password isn't correct"};
                true ->
                    mmnesia:change_pass(Username, NewPass),
                    record_socket(Username, []),
                    deal_user_group(Username, nil, Socket, minus),
                    success
            end
    end.

%% 踢人下线
% - 检查权限
% - 用户是否存在
% - 检查用户是否在线
% - 移除被踢用户socket
kick(User, Kuser) ->
    case check_kick_access(User, Kuser) of
        {err, Reason} -> {err, Reason};
        success ->
            OldSocket = record_socket(Kuser, []),
            deal_user_group(Kuser, nil, OldSocket, minus),
            {success, OldSocket}
    end.

%% 私有函数
deal_user_group(Username, OldSocket, Socket, Opt) ->
    Groups = db_tool:get_group(Username, nil, username),
    case Opt of
        add -> add_group_socket(Groups, OldSocket, Socket);
        minus -> remove_groups_socket(Groups, Socket)
    end.

%% 拿取用户socket
get_socket(User) ->
    [{_, OldSocket}] = ets:lookup(user_socket, User),
    OldSocket.

%% 用户上线ets修改socket
record_socket(Username, Socket) ->
    OldRecord = ets:lookup(user_socket, Username),
    ets:insert(user_socket, {Username, Socket}),
    case OldRecord of
        [] -> [];
        [{_, OldSocket}] -> OldSocket
    end.

%% 用户上线ets添加群socket(先删除之前的旧socket)
add_group_socket([], _, _) -> ok;
add_group_socket([{group_user, _, Gid, Gname, _} | T], OldSocket, Socket) ->
    [[Esockets]] = ets:match(groups, {Gid, '_', '$1'}),
    ets:insert(groups, {Gid, Gname, [Socket | lists:delete(OldSocket, Esockets)]}),
    add_group_socket(T, OldSocket, Socket).

remove_groups_socket([], _) -> ok;
remove_groups_socket([{group_user, _, Gid, Gname, _} | T], Socket) ->
    [[Esockets]] = ets:match(groups, {Gid, '_', '$1'}),
    ets:insert(groups, {Gid, Gname, lists:delete(Socket, Esockets)}),
    remove_groups_socket(T, Socket).

%% 检查踢人权限
check_kick_access(Username, Kuser) ->
    case Username =:= admin of
        false -> {err, "you are not the manager user"};
        true -> check_user_exist(Kuser)
    end.

%% 检查用户是否存在
check_user_exist(User) ->
    case db_tool:search_user(User) of
        [] -> {err,  "the user isn't exist"};
        _ -> check_user_online(User)
    end.

%% 检查是否在线
check_user_online(User) ->
    case ets:lookup(user_socket, User) of
        [] -> {err, atom_to_list(user) ++ " isn't online"};
        [{_, []}] -> {err, atom_to_list(user) ++ " isn't online"};
        _ -> success
    end.
