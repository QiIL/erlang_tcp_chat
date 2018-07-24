-module(db_tool).
-export([
    search_user/1, save_rec/4, change_pass/2,
    get_group/3, create_group/2, add_group_member/2,
    minus_group_member/2, get_record/1
]).
-export([
    init_table/0,
    reset_dbs/0, delete_tabs/0, test_search/0,
    add_member/1
]).
-include("../include/records.hrl").

init_table() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(chat_user, [{disc_copies, [node()]}, {attributes, record_info(fields, chat_user)}]),
    mnesia:create_table(chat_group, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_group)}]),
    mnesia:create_table(chat_record, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, chat_record)}]),
    mnesia:create_table(group_user, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, group_user)}]),
    mnesia:stop().

delete_tabs()->
    mnesia:delete_table(chat_user),
    mnesia:delete_table(chat_group),
    mnesia:delete_table(chat_record),
    mnesia:delete_table(group_user).

reset_dbs() ->
    mnesia:clear_table(chat_user),
    mnesia:clear_table(chat_group),
    mnesia:clear_table(chat_record),
    mnesia:clear_table(group_user),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, example_tables())
	end,
    mnesia:transaction(F).

test_search() ->
    R = #chat_record{id='$1', time='$2', _='_'},
    Guards = [{'<', '$2', 1531155084}],
    Recs = search(chat_record, R, Guards, ['$1']),
    NewRecs = lists:map(fun(Key) -> {chat_record, Key} end, Recs),
    NewRecs.

example_tables() ->
    [%% The user table
     {chat_user, qill, 11},
     {chat_user, user1, 11},
     {chat_user, user2, 11},
     {chat_user, user3, 11},
     {chat_user, admin, admin},
     %% The group table
     {chat_group, 1, world, admin, "the hold world"},
     {chat_group, 2, normalpeople, qill, "just normal people"},
     {chat_group, 3, users, user1, "our all user"},
     %% the chat record table
     {chat_record, 1, qill, broadcast, world, get_time() - 11 * 60, "the first msg"},
     {chat_record, 2, qill, broadcast, world, get_time() - 9 * 60, "second msg now"},
     {chat_record, 3, user1, whisper, qill, get_time() - 5 * 60, "user1 whisper with qill"},
     {chat_record, 4, user2, group_talk, users, get_time(), "user2 msg in users group"},
     {group_user, 1, 1, world, admin},
     {group_user, 2, 1, world, qill},
     {group_user, 3, 1, world, user1},
     {group_user, 4, 1, world, user2},
     {group_user, 5, 1, world, user3},
     {group_user, 6, 2, normalpeople, qill},
     {group_user, 7, 3, users, user1},
     {group_user, 8, 3, users, user2},
     {group_user, 9, 3, users, user3}
    ].

%% 添加僵尸号
add_member(3) -> ok;
add_member(N) ->
    UserName = list_to_atom("user" ++ integer_to_list(N)),
    F = fun() ->
		mnesia:write({chat_user, UserName, 11}),
        Id = mnesia:last(group_user),
        mnesia:write({group_user, Id+1, 1, world, UserName})
	end,
    mnesia:transaction(F),
    add_member(N-1).

%% 按照用户名搜索用户
search_user(UserName) ->
    mnesia:dirty_read(chat_user, UserName).

%% 保存记录
save_rec(Type, User, Target, Msg) ->
    RecId = last_tab(chat_record),
    case RecId of
        '$end_of_table' ->
            write({chat_record, 1, User, Type, Target, get_time(), Msg});
        _ ->
            write({chat_record, RecId+1, User, Type, Target, get_time(), Msg})
    end.

%% 修改密码
change_pass(Username, NewPass) ->
    write({chat_user, Username, NewPass}).

%% 获取用户群组
get_group(Username, Gid, Opt) ->
    R = get_group_rec(Username, Gid, Opt),
    Groups = mnesia:dirty_select(group_user, [{R, [], ['$_']}]),
    Groups.

%% 创建群组
create_group(Gname, Owner) ->
    Gid = last_tab(chat_group) + 1,
    Guid = last_tab(group_user) + 1,
    write({chat_group, Gid, Gname, Owner, ''}),
    write({group_user, Guid, Gid, Gname, Owner}),
    {Gid, Gname, Owner}.

%% 添加群成员
add_group_member(Gid, NewUser) ->
    [{_, _, Gname, _, _}] = search(chat_group, #chat_group{id='$1', _='_'}, [{'==', '$1', Gid}], ['$_']),
    Guid = last_tab(group_user) + 1,
    write({group_user, Guid, Gid, Gname, NewUser}),
    {Gid, Gname, NewUser}.

%% 减少群成员
minus_group_member(Gid, Username) ->
    [{group_user, Guid, _, Gname, _}] = search(group_user, #group_user{
        gid='$1', username='$2', _='_'}, 
        [{'andalso', 
         {'==', '$1', Gid},
         {'==', '$2', Username}
        }], 
        ['$_']),
    delete_recs(group_user, [Guid]),
    {Gid, Gname}.

%% 获取聊天记录
get_record(Username) ->
    R = #chat_record{user='$1', target='$2', _='_'},
    Guards = [{'orelse', {'==', '$1', Username}, {'==', '$2', Username}}],
    mnesia:dirty_select(chat_record, [{R, Guards, ['$_']}]).

%% 获取某表的最后一个元素
last_tab(Tab) ->
    {atomic, Val} = mnesia:transaction(fun() -> mnesia:last(Tab) end),
    Val.

%% 写表
write(Rec) ->
    mnesia:transaction(fun() -> mnesia:write(Rec) end).

%% 表搜索
search(Tab, R, Guards, Field) ->
    F = fun() ->
        mnesia:select(Tab, [{R, Guards, Field}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% 获取时间戳
get_time() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 删除表数据
delete_recs(_Tab, []) -> ok;
delete_recs(Tab, Recs) ->
    NewRecs = lists:map(fun(Key) -> {Tab, Key} end, Recs),
    F = fun() ->
        lists:foreach(fun mnesia:delete/1, NewRecs)
    end,
    mnesia:transaction(F).

%% 根据选项获取group_user表数据
get_group_rec(Username, Gid, Opt) ->
    case Opt of
        username ->
            #group_user{username = Username, _='_'};
        gid ->
            #group_user{gid = Gid, _='_'};
        both ->
            #group_user{username = Username, gid = Gid, _='_'}
    end.
