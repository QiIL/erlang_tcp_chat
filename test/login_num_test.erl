-module(login_num_test).
-export([start/1, loop_test/1]).

start(Num) ->
    tcp_server:start_link(),
    timer:tc(?MODULE, loop_test, [Num]).

loop_test(0) ->
    ok;
loop_test(Num) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    {ok, ClientPid} = client:start_link(),
    client:login(ClientPid, Username, 11),
    loop_test(Num - 1).
