-module(testing_tools).

-export([scan_and_rename/1, unslug/1, random_docker_name/0]).

scan_and_rename(Path) ->
    case {filelib:is_dir(Path), re:run(Path, "\\.er_$", [{capture, none}])} of
        {true, _} ->
            {ok, Files} = file:list_dir_all(Path),
            Pathes = [filename:join(Path, File) || File <- Files],
            [scan_and_rename(Path2) || Path2 <- Pathes],
            ok;
        {false, match} ->
            NewName = iolist_to_binary(re:replace(Path, ".er_$", ".erl")),
            do_rename(Path, NewName),
            ok;
        {false, nomatch} ->
            ct:log("Skipping file ~p", [Path]),
            ok
    end.

do_rename(OldName, NewName) ->
    case file:read_file_info(NewName) of
        {error, enoent} ->
            ct:log("Target ~s does not exist, renaming", [NewName]);
        {ok, _} ->
            ct:log("Target ~s does already exist, deleting", [NewName]),
            ok = file:delete(NewName)
    end,
    case file:rename(OldName, NewName) of
        ok ->
            ct:log("Renamed File ~s to ~s", [OldName, NewName]);
        {error, Reason} ->
            ct:log("Renaming ~s to ~s failed with reason ~p", [OldName, NewName, Reason])
    end.

unslug(Atom) when is_atom(Atom) ->
    unslug(atom_to_list(Atom));
unslug(String) ->
    Unslugged = string:replace(String, "_", "-", all),
    ct:log("Unslugged: ~s -> ~s", [String, Unslugged]),
    Unslugged.

random_docker_name() ->
    Chars = [rand:uniform(26) + $a - 1 || _ <- lists:seq(1, 10)],
    iolist_to_binary(Chars).
