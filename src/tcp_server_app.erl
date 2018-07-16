-module(tcp_server_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    tcp_supervisor:start_link().

start(normal, _StartArgs) ->
    tcp_supervisor:start_link().

stop(_State) ->
    ok.
