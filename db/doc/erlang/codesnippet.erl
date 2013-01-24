Root = filename:absname_join(filename:dirname(?FILE), ".."),
application:set_env(mnesia, dir, filename:join(Root, "db")).

