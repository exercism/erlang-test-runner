-module(errors_files_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    %% Common Test Callbacks
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,

    %% Test Cases
    empty_file/1,
    weird_characters_in_module/1
]).

-define(assertVersion(Expr), ?assertMatch(#{version := 2}, Expr)).
-define(assertTopLevel(Status, Expr),
    ?assertVersion(Expr),
    ?assertMatch(#{status := Status}, Expr)
).

-define(assertMessage(Message, Expr), ?assertMatch(#{message := Message}, Expr)).

-define(PARSE_ERRORS, [
    empty_file,
    weird_characters_in_module
]).

all() ->
    [
        {group, parse_errors}
    ].

groups() ->
    [
        {parse_errors, [shuffle], ?PARSE_ERRORS}
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    testing_tools:scan_and_rename(DataDir),
    Config.

end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) ->
    FolderName = testing_tools:unslug(TestCase),
    BaseDir = filename:join(?config(data_dir, Config), FolderName),
    {ok, PWD} = file:get_cwd(),
    file:set_cwd(BaseDir),
    ct:log("Changed pwd to ~s", [BaseDir]),
    Config1 = [
        {folder_name, FolderName},
        {base_dir, BaseDir},
        {old_pwd, PWD}
        | Config
    ],
    ct:log("assembled config: ~p", [Config1]),
    Config1.

end_per_testcase(_TestCase, Config) ->
    %% Recreate original PWD.
    file:set_cwd(?config(old_pwd, Config)).

%% Testcase implementations
%% ========================

empty_file(Config) ->
    Result = erlang_test_runner:compile_and_run(?config(base_dir, Config), "two_fer"),
    ?assertTopLevel(<<"error">>, Result),
    ?assertMessage(
        <<"Could not parse the file 'two_fer.erl' at line 1: no module definition">>, Result
    ).

weird_characters_in_module(Config) ->
    Result = erlang_test_runner:compile_and_run(?config(base_dir, Config), "two_fer"),
    ?assertTopLevel(<<"error">>, Result),
    ?assertMessage(
        <<"Could not parse the file 'two_fer.erl' at line 1: syntax error before: $e">>, Result
    ).
