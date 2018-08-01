-module(login_num_test).
-export([start/1]).

start(Num) ->
    loop_test(Num, os:timestamp()).

loop_test(0, {M, S, Ms}) ->
    {Nm, Ns, Nms} = os:timestamp(),
    Ftime = ((Nm * 1000000 + Ns) * 1000000 + Nms) - ((M * 1000000 + S) * 1000000 + Ms),
    io:format("~p~n", [Ftime]),
    ok;
loop_test(Num, Timestamp) ->
    Username = list_to_atom("user" ++ integer_to_list(Num)),
    ClientPid = client:start_link(),
    client:login(ClientPid, Username, 11),
    loop_test(Num - 1, Timestamp).
