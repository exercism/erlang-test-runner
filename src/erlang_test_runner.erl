-module(erlang_test_runner).

%% API exports
-export([main/1]).

%% Export for testing
-export([compile_and_run/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
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

write_result(Content, OutputDir) ->
    ResultJsonPath = filename:join(OutputDir, "results.json"),
    {ok, FD} = file:open(ResultJsonPath, [write]),
    io:format(FD, "~s~n", [Content]).

compile_and_run(InputDir, Exercise) ->
    case etr_compile:compile(InputDir, Exercise) of
        {Module, Modules, Abstract} ->
            code:atomic_load(Modules),
            etr_runner:run(Module, Abstract);
        {error, _Errors} ->
            #{
                version => 2,
                status => <<"error">>
            }
    end.
