-module(erlang_test_runner).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Exercise, InputDir, OutputDir]) ->
    file:set_cwd(InputDir),
    {Module, Modules} = etr_compile:compile(InputDir, Exercise),
    code:atomic_load(Modules),
    Results = etr_runner:run(Module),
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
