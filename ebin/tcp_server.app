{
    application, tcp_server,
    [
        {description, "tcp c/s chat server"},
        {vsn, "1.0"},
        {
            modules, [
                tcp_server_app, tcp_supervisor, tcp_server,
                msg_server, mmnesia
            ]
        },
        {registered, [tcp_server, msg_server, mmnesia]},
        {applications, [kernel, stdlib]},
        {mod, {tcp_server_app, []}}
    ]
}.