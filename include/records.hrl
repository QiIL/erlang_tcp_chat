-record(chat_user, {username, pass}).
-record(chat_group, {id, name, owner, desc}).
-record(chat_record, {id, user, type, target, time, msg}).
-record(group_user, {id, gid, gname, username}).