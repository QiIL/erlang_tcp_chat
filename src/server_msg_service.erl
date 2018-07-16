-module(server_msg_service).
-export([
    check_online/0
]).

%% 世界的socket数
check_online() ->
    length(ets:lookup_element(groups, 1, 3)).