-module(mmnesia).
-export([
    start_link/0, stop/0, 
    save_rec/4, delete_recs/2, search/4, change_pass/2,
    get_group/3, create_group/2, add_group_member/2, 
    minus_group_member/2,
    search_user/1
]).
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3  
]).
-include("../include/records.hrl").
-behaviour(gen_server).
-define(TIMEOUT, 1000 * 60 * 10). % 10分钟清一次聊天记录

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

init([]) ->
    mnesia:start(),
    mnesia:wait_for_tables([chat_user, chat_group, chat_record], 20000),
    erlang:send_after(?TIMEOUT, self(), clean_record),
    {ok, []}.

%% username查询用户
search_user(Username) -> gen_server:call(?MODULE, {search_user, Username}).
%% 保存记录
save_rec(Type, User, Target, Msg) -> gen_server:call(?MODULE, {save_rec, Type, User, Target, Msg}).
%% 修改密码
change_pass(Username, NewPass) -> gen_server:call(?MODULE, {change_pass, Username, NewPass}).
%% 获取用户群组
get_group(Username, Gid, Opt) -> gen_server:call(?MODULE, {get_group, Username, Gid, Opt}).
%% 创建群组
create_group(Gname, Owner) -> gen_server:call(?MODULE, {create_group, Gname,  Owner}).
%% 添加群成员
add_group_member(Gid, Username) -> gen_server:call(?MODULE, {add_group_member, Gid,  Username}).
%% 减少群成员
minus_group_member(Gid, Username) -> gen_server:call(?MODULE, {minus_group_member, Gid, Username}).

handle_call({search_user, Username}, _From, State) ->
    R = #chat_user{username = Username, _='_'},
    UserMessage = search(chat_user, R, [], ['$_']),
    {reply, UserMessage, State};

handle_call({save_rec, Type, User, Target, Msg}, _From, State) ->
    io:format("Type: ~p, user: ~p, Target: ~p, Msg: ~p~n", [Type, User, Target, Msg]),
    RecId = last_tab(chat_record),
    case RecId of
        '$end_of_table' ->
            write({chat_record, 1, User, Type, Target, get_time(), Msg});
        _ ->
            write({chat_record, RecId+1, User, Type, Target, get_time(), Msg})
    end,
    {reply, save_ok, State};
    
handle_call({change_pass, Username, NewPass}, _From, State) ->
    write({chat_user, Username, NewPass}),
    {reply, ok, State};

handle_call({get_group, Username, Gid, Opt}, _From, State) ->
    R = get_group_rec(Username, Gid, Opt),
    Groups = search(group_user, R, [], ['$_']),
    {reply, Groups, State};

handle_call({create_group, Gname, Owner}, _From, State) ->
    Id = last_tab(group),
    write({chat_group, Id+1, Gname, Owner, [Owner]}),
    {reply, {ok, Id}, State};

handle_call({add_group_member, Gid, NewUser}, _From, State) ->
    [{_, _, Gname, Owner, Members}] = search(chat_group, #chat_group{id='$1', _='_'}, [{'==', '$1', Gid}], ['$_']),
    write({chat_group, Gid, Gname, Owner, [NewUser | Members]}),
    {reply, {ok, NewUser}, State};

handle_call({minus_group_member, Gid, Username}, _From, State) ->
    [{_, _, Gname, Owner, Members}] = search(chat_group, #chat_group{id='$1', _='_'}, [{'==', '$1', Gid}], ['$_']),
    write({chat_group, Gid, Gname, Owner, lists:delete(Username, Members)}),
    {reply, {ok, Username}, State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

%% handle cast
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle info
handle_info(clean_record, State) ->
    io:format("delete the record~n"),
    TenMinAgo = get_time() - 10 * 60,
    R = #chat_record{id='$1', time='$2', _='_'},
    Guards = [{'<', '$2', TenMinAgo}],
    Recs = search(chat_record, R, Guards, ['$1']),
    delete_recs(chat_record, Recs),
    erlang:send_after(?TIMEOUT, self(), clean_record),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    mnesia:stop(),
    ok.

%% 热更新
code_change(_OldVsn, Socket, _Extra) ->
    {ok, Socket}.

%% 获取某表的最后一个元素
last_tab(Tab) ->
    {atomic, Val} = mnesia:transaction(fun() -> mnesia:last(Tab) end),
    Val.

%% 表搜索
search(Tab, R, Guards, Field) ->
    F = fun() ->
        mnesia:select(Tab, [{R, Guards, Field}])
    end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% 写表
write(Rec) ->
    mnesia:transaction(fun() -> mnesia:write(Rec) end).

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
