-module(user_service).
-export([
    login/3
]).

%% 登陆
% 1.验证密码
% 2.群组上线
login(Username, Password, Socket) ->
    case mmnesia:search_user(Username) of
        [] -> {err, "user isn't exist"};
        [{chat_user, Username, Pass}] ->
            case Password =:= Pass of
                false -> {err, "Username or Password is wrong"};
                true ->
                    online_group(Username, Socket),
                    login_success
            end
    end.


%% 私有函数
online_group(Username, Socket) ->
    Groups = mmnesia:get_group(Username, nil, username),
    add_group_socket_ets(Groups, Socket).


%% 用户上线ets添加socket
add_group_socket_ets([], _) -> ok;
add_group_socket_ets([{group_user, _, Gid, Gname, _} | T], Socket) ->
    Esockets = ets:match(groups, {Gid, '_', '$1'}),
    ets:insert(groups, {Gid, Gname, [Socket | Esockets]}),
    add_group_socket_ets(T, Socket).
