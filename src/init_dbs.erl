-module(init_dbs).
-export([
    init_table/0,
    reset_dbs/0, delete_tabs/0, test_search/0
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
