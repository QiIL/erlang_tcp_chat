-module(group_service).
-export([
    new/3, join/3, leave/3, get_group_socket/2
]).

%% 新建群组
% - db_tool创建条目添加数据
% - ets groups插入新数据
new(Gname, Owner, Socket) ->
    {Gid, _, _} = db_tool:create_group(Gname, Owner),
    ets:insert(groups, {Gid, Gname, [Socket]}).

%% 加入群组
% - 检查群组存在
% - 检查用户是否已经在里面
% - do_tool 记录(chat_group, group_user)
% - ets group插入Socket
join(Gid, User, Socket) ->
    case check_relation(Gid, User) of
        group_not_found -> {err, "can't find this group"};
        in_group -> {err, "you are already in this group"};
        not_in_group -> 
            {_, Gname, _} = db_tool:add_group_member(Gid, User),
            OldSocketList = ets:lookup_element(groups, Gid, 3),
            ets:insert(groups, {Gid, Gname, [Socket | OldSocketList]})
    end.

%% leave
% - 检查群是否存在
% - 检查玩家是否在群
% - db_tool 清除menber
% - ets delete socket
leave(Gid, User, Socket) ->
    case check_relation(Gid, User) of
        group_not_found -> {err, "can't find this group"};
        not_in_group -> {err, "you are not in this group"};
        in_group -> 
            {_, Gname} = db_tool:minus_group_member(Gid, User),
            OldSocketList = ets:lookup_element(groups, Gid, 3),
            ets:insert(groups, {Gid, Gname, lists:delete(Socket, OldSocketList)})
    end.

%% get_group_socket
% check_relateion
% 返回ets socket
get_group_socket(Gid, User) ->
    case check_relation(Gid, User) of
        group_not_found -> {err, "can't find this group"};
        not_in_group -> {err, "you are not in this group"};
        in_group -> ets:lookup(groups, Gid)
    end.
    
%% check_relation
% - 检查群是否存在
% - 检查玩家是否在群
check_relation(Gid, User) ->
    case  db_tool:get_group(User, Gid, gid) of
        [] ->
            group_not_found;
        _ ->
            case db_tool:get_group(User, Gid, both) of
                [] -> not_in_group;
                [{group_user, _, _, _, _}] -> in_group
            end
    end.

