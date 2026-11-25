-module('etr_compile').

-export([compile/2]).
-export_type([module_data/0]).

-type module_data() :: {module(), string(), binary()}.

-spec compile(file:name_all(), string()) ->
    {error, [string()]}
    | {error, {module_name, module(), module()}}
    | {ok, module(), [module_data()], [erl_parse:abstract_form()]}.
compile(BaseFolder, Exercise) ->
    ModuleName = string:replace(Exercise, "-", "_", all),
    Expected = list_to_atom(unicode:characters_to_list(ModuleName)),
    case compile_solution(BaseFolder, ModuleName) of
        {ok, Solution = {Expected, _, _}} ->
            {Tests, AbstractTests} = compile_test(BaseFolder, ModuleName),
            {ok, Expected, [Solution, Tests], AbstractTests};
        %% The tests call the module by the exercise's name, so any other name can't work.
        {ok, {Module, _, _}} ->
            {error, {module_name, Module, Expected}};
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
