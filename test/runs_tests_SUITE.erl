-module(runs_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    %% Common Test Callbacks
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,

    %% Test Cases
    two_fer/1
]).

all() ->
    [two_fer].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    scan_and_rename(DataDir),
    Config.

end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) ->
    FolderName = unslug(TestCase),
    BaseDir = filename:join(?config(data_dir, Config), FolderName),
    {ok, PWD} = file:get_cwd(),
    file:set_cwd(BaseDir),
    ct:log("Changed pwd to ~s", [BaseDir]),
    {TestCase, ModuleSpecs} = etr_compile:compile(BaseDir, FolderName),
    code:atomic_load(ModuleSpecs),
    ct:log("Modules loaded: ~p", [[Mod || {Mod, _, _} <- ModuleSpecs]]),
    eunit:start(),
    ct:log("EUnit started"),
    [
        {folder_name, FolderName},
        {base_dir, BaseDir},
        {old_pwd, PWD},
        {module_specs, ModuleSpecs}
        | Config
    ].

end_per_testcase(TestCase, Config) ->
    eunit:stop(),
    ct:log("EUnit stopped"),
    [code:purge(Mod) || {Mod, _, _} <- ?config(module_specs, Config)],
    ct:log("Purged modules from memory: ~p", [[Mod || {Mod, _, _} <- ?config(module_specs, Config)]]),
    file:set_cwd(?config(old_pwd, Config)).

two_fer(Config) ->
    EUnit = eunit:test(two_fer),
    ct:log("Result of run is ~p", [EUnit]).

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
    string:replace(String, "_", "-").
