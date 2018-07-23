-module(login_num_test).
-export([start/1, loop_test/1]).

start(Num) ->
    Pid = spawn(?MODULE, loop_test, [Num]),
    eprof:start(),
    eprof:start_profiling([Pid]).

loop_test(0) ->
    eprof:stop_profiling(),
    eprof:analyze(total);
loop_test(Num) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    {ok, ClientPid} = client:start_link(),
    client:login(ClientPid, Username, 11),
    loop_test(Num - 1).
