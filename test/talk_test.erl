-module(talk_test).
-export([start/2, login_to_talk/2]).

start(0, _) -> ok;
start(Num, Sleep) ->
    spawn(fun() -> start(Num - 1, Sleep) end),
    login_to_talk(Num, Sleep).

login_to_talk(Num, Sleep) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    {ok, ClientPid} = client:start_link(),
    client:login(ClientPid, Username, 11),
    talk(ClientPid, Sleep).

talk(Pid, Sleep) ->
    timer:sleep(Sleep),
    client:talk(Pid, "bbbbbbbbbbb"),
    talk(Pid, Sleep).