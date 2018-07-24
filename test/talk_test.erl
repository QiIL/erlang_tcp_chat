-module(talk_test).
-export([start/1, login_to_talk/1]).

start(0) -> ok;
start(Num) ->
    spawn(fun() -> start(Num - 1) end),
    login_to_talk(Num).

login_to_talk(Num) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    {ok, ClientPid} = client:start_link(),
    client:login(ClientPid, Username, 11),
    talk(ClientPid, 10000).

talk(Pid, 0) -> client:stop(Pid);
talk(Pid, N) ->
    timer:sleep(2),
    client:talk(Pid, "bbbbbbbbbbb"),
    talk(Pid, N - 1).