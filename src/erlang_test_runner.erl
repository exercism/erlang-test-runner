-module(erlang_test_runner).

%% API exports
-export([main/1]).

%% Export for testing
-export([compile_and_run/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> no_return().
main([Exercise, InputDir, OutputDir]) ->
    file:set_cwd(InputDir),
    Results = compile_and_run(InputDir, Exercise),
    io:format("~p~n", [Results]),
    ResultsJson = jsx:encode(Results, [space, {indent, 2}]),
    io:format("~s~n", [ResultsJson]),
    ok = write_result(ResultsJson, OutputDir),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec write_result(string(), file:filename_all()) -> ok.
write_result(Content, OutputDir) ->
    ResultJsonPath = filename:join(OutputDir, "results.json"),
    {ok, FD} = file:open(ResultJsonPath, [write]),
    io:format(FD, "~s~n", [Content]).

-spec compile_and_run(file:filename_all(), string()) -> etr_runner:result().
compile_and_run(InputDir, Exercise) ->
    case etr_compile:compile(InputDir, Exercise) of
        {ok, Module, Modules, Abstract} ->
            code:atomic_load(Modules),
            etr_runner:run(Module, Abstract);
        {error, Errors} ->
            #{
                version => 2,
                status => error,
                message => iolist_to_binary(etr_humanize:compilation(Errors))
            }
    end.
