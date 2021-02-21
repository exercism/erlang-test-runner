-module(erlang_test_runner).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([Exercise, InputDir, _OutputDir]) ->
    {Module, Modules} = etr_compile:compile(InputDir, Exercise),
    code:atomic_load(Modules),
    eunit:start(),
    R = eunit:test(Module),
    io:format("R: ~p~n", [R]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
