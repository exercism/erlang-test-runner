-module(etr_runner).

-export([run/2]).
-export_type([result/0, error_result/0, fail_result/0, pass_result/0]).

-type result() :: error_result() | fail_result() | pass_result().
-type version() :: 2.

-type error_result() :: #{
    version := version(),
    status := error,
    message := binary(),
    tests => [entry()]
}.

-type fail_result() :: #{
    version := version(),
    status := fail,
    tests := [pass_entry() | fail_entry()]
}.

-type pass_result() :: #{
    version := version(),
    status := pass,
    tests := [pass_entry()]
}.

-type entry() :: pass_entry() | fail_entry() | error_entry().

-type pass_entry() :: #{
    status := pass,
    name := string() | binary()
}.
-type fail_entry() :: #{
    status := fail,
    name := string() | binary(),
    message := string() | binary()
}.
-type error_entry() :: #{
    status := error,
    name := string() | binary()
}.

-spec run(module(), [erl_parse:abstract_form()]) -> result().
run(Module, Abstract) ->
    TestModule = get_test_module_from(Module),
    TestFuns = [
        {Fun, match == re:run(atom_to_binary(Fun, utf8), "_$", [{capture, none}])}
     || {Fun, _} <- TestModule:module_info(exports),
        match == re:run(atom_to_binary(Fun, utf8), "_test_?$", [{capture, none}])
    ],
    Results = [run_case(TestModule, Fun, Gen) || {Fun, Gen} <- TestFuns],
    Tests = lists:flatten([grade_test(Result, Abstract) || Result <- Results]),
    #{
        version => 2,
        status => get_status(Tests),
        tests => Tests
    }.

-spec get_test_module_from(module() | string() | binary()) -> module().
get_test_module_from(Module) when is_atom(Module) ->
    get_test_module_from(atom_to_binary(Module, utf8));
get_test_module_from(Module) ->
    case re:run(Module, "_tests$", [{capture, none}]) of
        match ->
            binary_to_existing_atom(Module, utf8);
        nomatch ->
            binary_to_existing_atom(iolist_to_binary([Module | "_tests"]), utf8)
    end.

-spec run_case
    (module(), atom(), false) -> {binary(), ok};
    (module(), atom(), true) -> {binary(), ok} | [{binary(), ok}].
run_case(Module, Fun, false) ->
    {atom_to_binary(Fun, utf8), run_case(erlang:make_fun(Module, Fun, 0))};
run_case(Module, Gen, true) ->
    Runner = fun({Description, {_Line, Fun}}) -> {list_to_binary(Description), run_case(Fun)} end,
    case Module:Gen() of
        {_, {_, _}} = Case -> Runner(Case);
        L when is_list(L) -> lists:map(Runner, L)
    end.

-spec run_case(fun(() -> Result)) -> {pass, Result} | {fail, term()}.
run_case(Fun) ->
    {Self, Ref} = {self(), make_ref()},
    {Pid, MonRef} = erlang:spawn_monitor(fun() -> Self ! {Ref, Fun()} end),
    receive
        {Ref, Anything} ->
            {pass, sweep_inbox(Ref, MonRef, Anything)};
        {'DOWN', MonRef, process, Pid, {Exception, _}} ->
            {fail, Exception}
    end.

-spec sweep_inbox(reference(), reference(), Result) -> Result.
sweep_inbox(Ref, MRef, Result) ->
    receive
        {Ref, _} -> sweep_inbox(Ref, MRef, Result);
        {'DOWN', MRef, process, _, _} -> sweep_inbox(Ref, MRef, Result)
    after 0 -> Result
    end.

-spec grade_test([Data] | Data, [AST] | AST) -> [Result] | Result when
    Data :: {string(), {pass, term()}}, AST :: erl_parse:abstract_form(), Result :: entry().
grade_test(L, Abstract) when is_list(L) -> lists:map(fun(X) -> grade_test(X, Abstract) end, L);
grade_test({Name, {pass, ok}}, _Abstract) ->
    #{
        name => Name,
        status => pass
    };
grade_test({Name, {fail, Exception}}, Abstract) ->
    Message = iolist_to_binary(etr_humanize:assertion(Exception)),
    #{
        name => Name,
        status => fail,
        message => Message,
        test_code => find_test_code(Abstract, Exception)
    }.

find_test_code(Abstract, Exception) ->
    Line = proplists:get_value(line, element(2, Exception)),
    Funs = [
        Node
     || Node <- Abstract,
        erl_syntax:type(Node) =:= function,
        Line > erl_syntax:get_pos(Node)
    ],
    Fun = hd(lists:reverse(Funs)),
    unicode:characters_to_binary(erl_pp:form(Fun)).

-spec get_status([entry()]) -> pass | fail | error.
get_status(Tests) -> get_status(Tests, pass).

-spec get_status([entry()], pass | fail | error) -> pass | fail | error.
get_status([], Status) -> Status;
get_status([#{status := error} | _], _) -> error;
get_status([#{status := fail} | T], _) -> get_status(T, fail);
get_status([_ | T], Status) -> get_status(T, Status).
