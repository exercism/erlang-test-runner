-module(erlang_test_runner).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Exercise, InputDir, _OutputDir]) ->
    Slug = string:replace(Exercise, "-", "_"),
    FileName = binary_to_list(filename:join([
                              InputDir,
                              src,
                              iolist_to_binary([Slug | ".erl"])
                             ])),
    TestFileName = binary_to_list(filename:join([
                                  InputDir,
                                  test,
                                  iolist_to_binary([Slug | "_tests.erl"])
                                 ])),
    CompileOpts = [binary, verbose, report_errors, report_warnings],
    {ok, Module, Binary} = compile:file(FileName, CompileOpts),
    io:format(TestFileName),
    {ok, TestModule, TestBinary} = compile:file(TestFileName, CompileOpts),
    code:atomic_load([
                      {Module, FileName, Binary},
                      {TestModule, TestFileName, TestBinary}
                     ]),
    eunit:start(),
    R = eunit:test(Module),
    io:format("R: ~p~n", [R]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
