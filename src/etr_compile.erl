-module('etr_compile').

-export([compile/2]).

compile(BaseFolder, Exercise) ->
    ModuleName = string:replace(Exercise, "-", "_", all),
    Solution = {Module, _, _} = compile_solution(BaseFolder, ModuleName),
    Tests = compile_test(BaseFolder, ModuleName),
    {Module, [Solution, Tests]}.

%%====================================================================
%% Internal functions
%%====================================================================

compile_solution(BaseFolder, Name) ->
    FileName = binary_to_list(
        filename:join(
            [
                BaseFolder,
                src,
                iolist_to_binary([Name | ".erl"])
            ]
        )
    ),
    CompileOpts = [binary, verbose, report_errors, report_warnings],
    {ok, Module, Binary} = compile:file(FileName, CompileOpts),
    BeamName = binary_to_list(iolist_to_binary([Name | ".beam"])),
    {Module, BeamName, Binary}.

compile_test(BaseFolder, Name) ->
    FileName = binary_to_list(
        filename:join(
            [
                BaseFolder,
                test,
                iolist_to_binary([Name | "_tests.erl"])
            ]
        )
    ),
    CompileOpts = [binary, verbose, report_errors, report_warnings],
    {ok, Module, Binary} = compile:file(FileName, CompileOpts),
    BeamName = binary_to_list(iolist_to_binary([Name | ".beam"])),
    {Module, BeamName, Binary}.
