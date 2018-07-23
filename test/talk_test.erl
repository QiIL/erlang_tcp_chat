-module(talk_test).
-export([start/1, login_to_talk/1]).

start(0) -> ok;
start(Num) ->
    spawn(?MODULE, login_to_talk, [Num]),
    start(Num - 1).

login_to_talk(Num) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    {ok, ClientPid} = client:start_link(),
    client:login(ClientPid, Username, 11),
    talk(ClientPid).

talk(Pid) ->
    timer:sleep(2),
    client:talk(Pid, "testing!"),
    talk(Pid).