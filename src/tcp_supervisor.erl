-module(tcp_supervisor).
-export([start_link/0]).
-export([init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
    init({one_for_all, 3, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
    {
        ok, {
            {RestartStrategy, MaxRestart, MaxTime},
            [
                {qill_tcp_server,
                {tcp_server, start_link, []},
                permanent, 1000, worker, [tcp_server]
                }
            ]
        }
    }.
