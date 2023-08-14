-module('etr_compile').

-export([compile/2]).
-export_type([module_data/0]).

-type module_data() :: {module(), string(), binary()}.

-spec compile(file:name_all(), string()) ->
    {error, [string()]} | {ok, module(), [module_data()], [erl_parse:abstract_form()]}.
compile(BaseFolder, Exercise) ->
    ModuleName = string:replace(Exercise, "-", "_", all),
    case compile_solution(BaseFolder, ModuleName) of
        {ok, Solution = {Module, _, _}} ->
            {Tests, AbstractTests} = compile_test(BaseFolder, ModuleName),
            {ok, Module, [Solution, Tests], AbstractTests};
        {error, Errors} ->
            {error, Errors}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec compile_solution(file:name_all(), string()) ->
    {ok, module_data()} | {error, [string()]}.
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
    CompileOpts = [binary, verbose, return_errors, return_warnings],
    case compile:file(FileName, CompileOpts) of
        {ok, Module, Binary, _Warnings} ->
            BeamName = binary_to_list(iolist_to_binary([Name | ".beam"])),
            {ok, {Module, BeamName, Binary}};
        {error, Errors, _Warnings} ->
            {error, Errors}
    end.

-spec compile_test(file:name_all(), string()) -> {module_data(), [erl_parse:abstract_form()]}.
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

    {ok, Abstract} = epp:parse_file(FileName, []),

    {{Module, BeamName, Binary}, Abstract}.
