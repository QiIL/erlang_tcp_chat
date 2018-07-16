-module(tcp_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _StartArgs) ->
    tcp_supervisor:start_link().

stop(_State) ->
    ok.
