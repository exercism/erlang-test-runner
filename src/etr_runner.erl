-module(etr_runner).

-export([run/1]).

run(Module) ->
    TestModule = get_test_module_from(Module),
    TestFuns = [
        {Fun, match == re:run(atom_to_binary(Fun, utf8), "_$", [{capture, none}])}
     || {Fun, _} <- TestModule:module_info(exports),
        match == re:run(atom_to_binary(Fun, utf8), "_test_?$", [{capture, none}])
    ],
    Runs = [run_case(TestModule, Fun, Gen) || {Fun, Gen} <- TestFuns].

get_test_module_from(Module) when is_atom(Module) ->
    get_test_module_from(atom_to_binary(Module, utf8));
get_test_module_from(Module) ->
    case re:run(Module, "_tests$", [{capture, none}]) of
        match ->
            binary_to_existing_atom(Module, utf8);
        nomatch ->
            binary_to_existing_atom(iolist_to_binary([Module | "_tests"]), utf8)
    end.

run_case(Module, Fun, false) ->
    {atom_to_binary(Fun, utf8), run_case(erlang:make_fun(Module, Fun, 0))};
run_case(Module, Gen, true) ->
    {Description, {_, Fun}} = Module:Gen(),
    {Description, run_case(Fun)}.

run_case(Fun) ->
    try Fun() of
        Result -> Result
    catch
        Error -> Error
    end.
