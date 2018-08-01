-module(chat_mg).
-export([
    start_link/0, stop/0, 
    save_rec/4, write/1
]).
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3  
]).
-include("../include/records.hrl").
-behaviour(gen_server).
-define(TIMEOUT, 1000 * 60 * 10). % 10分钟清一次聊天记录
-record(state, {id=0}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

init([]) ->
    mnesia:start(),
    mnesia:wait_for_tables([chat_user, chat_group, chat_record], 20000),
    erlang:send_after(?TIMEOUT, self(), clean_record),
    RecId = last_tab(chat_record),
    case RecId of
        '$end_of_table' ->
            {ok, #state{id=0}};
        _ ->
            {ok, #state{id=RecId+1}}
    end.

%% 保存记录
save_rec(Type, User, Target, Msg) -> gen_server:call(?MODULE, {save_rec, Type, User, Target, Msg}).

handle_call({save_rec, Type, User, Target, Msg}, _From, State=#state{id=Id}) ->
    mnesia:dirty_write({chat_record, Id, User, Type, Target, get_time(), Msg}),
    {reply, save_ok, State#state{id=Id+1}};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

%% handle cast
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle info
handle_info(clean_record, State) ->
    io:format("delete chat records~n"),
    TenMinAgo = get_time() - 10 * 60 * 1000,
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
    {M, S, Ms} = os:timestamp(),
    (M * 1000000 + S) * 1000 + (Ms div 1000).

%% 删除表数据
delete_recs(_Tab, []) -> ok;
delete_recs(Tab, Recs) ->
    NewRecs = lists:map(fun(Key) -> {Tab, Key} end, Recs),
    F = fun() ->
        lists:foreach(fun mnesia:delete/1, NewRecs)
    end,
    mnesia:transaction(F).
