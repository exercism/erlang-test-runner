-module(erlang_test_runner).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Exercise, InputDir, _OutputDir]) ->
    Slug = string:replace(Exercise, "-", "_"),
    {Module, BeamFile, Binary} = compile_solution(InputDir, Slug),
    {TestModule, TestBeamFile, TestBinary} = compile_test(InputDir, Slug),
    code:atomic_load([
        {Module, BeamFile, Binary},
        {TestModule, TestBeamFile, TestBinary}
    ]),
    eunit:start(),
    R = eunit:test(Module),
    io:format("R: ~p~n", [R]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

compile_solution(InputFolder, Name) ->
    FileName = binary_to_list(
        filename:join(
            [
                InputFolder,
                src,
                iolist_to_binary([Name | ".erl"])
            ]
        )
    ),
    CompileOpts = [binary, verbose, report_errors, report_warnings],
    {ok, Module, Binary} = compile:file(FileName, CompileOpts),
    BeamName = binary_to_list(iolist_to_binary([Name | ".beam"])),
    {Module, BeamName, Binary}.

compile_test(InputFolder, Name) ->
    FileName = binary_to_list(
        filename:join(
            [
                InputFolder,
                test,
                iolist_to_binary([Name | "_tests.erl"])
            ]
        )
    ),
    CompileOpts = [binary, verbose, report_errors, report_warnings],
    {ok, Module, Binary} = compile:file(FileName, CompileOpts),
    BeamName = binary_to_list(iolist_to_binary([Name | ".beam"])),
    {Module, BeamName, Binary}.
