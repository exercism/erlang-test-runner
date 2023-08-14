-module(runs_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    %% Common Test Callbacks
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,

    %% Generic test implementation
    '$handle_undefined_function'/2,

    %% Special Test Cases
    allergies/1,
    two_fer/1
]).

-define(assertVersion(Expr), ?assertMatch(#{version := 2}, Expr)).
-define(assertTopLevel(Status, Expr),
    ?assertVersion(Expr),
    ?assertMatch(#{status := Status}, Expr)
).
-define(assertNumFails(Num, Expr), begin
    ((fun() ->
        __X = (Num),
        __Y = (Expr),
        __Tests = map_get(tests, __Y),
        __NumTests = lists:foldl(
            fun
                (#{status := pass}, C) -> C;
                (#{status := fail}, C) -> C + 1
            end,
            0,
            __Tests
        ),
        case __NumTests =:= __X of
            true ->
                ok;
            false ->
                erlang:error(
                    [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {expression, (??Expr)},
                        {expected, __X},
                        {value, __NumTests}
                    ]
                )
        end
    end)())
end).

-define(EXERCISES, [
    accumulate,
    allergies,
    all_your_base,
    anagram,
    armstrong_numbers,
    atbash_cipher,
    bank_account,
    beer_song,
    bob,
    book_store,
    bracket_push,
    change,
    circular_buffer,
    clock,
    collatz_conjecture,
    complex_numbers,
    connect,
    crypto_square,
    custom_set,
    darts,
    diamond,
    difference_of_squares,
    dominoes,
    etl,
    forth,
    gigasecond,
    grade_school,
    grains,
    hamming,
    hello_world,
    isbn_verifier,
    isogram,
    largest_series_product,
    leap,
    list_ops,
    luhn,
    meetup,
    minesweeper,
    nth_prime,
    nucleotide_count,
    palindrome_products,
    pangram,
    parallel_letter_frequency,
    pascals_triangle,
    perfect_numbers,
    phone_number,
    poker,
    prime_factors,
    protein_translation,
    pythagorean_triplet,
    queen_attack,
    rail_fence_cipher,
    raindrops,
    rational_numbers,
    rna_transcription,
    robot_simulator,
    roman_numerals,
    rotational_cipher,
    run_length_encoding,
    saddle_points,
    satellite,
    scrabble_score,
    secret_handshake,
    series,
    sieve,
    simple_linked_list,
    space_age,
    spiral_matrix,
    strain,
    sublist,
    sum_of_multiples,
    transpose,
    triangle,
    two_fer,
    variable_length_quantity,
    word_count,
    zipper
]).

all() ->
    [{group, sample_exercises}].

groups() ->
    [
        {sample_exercises, [shuffle], ?EXERCISES}
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    testing_tools:scan_and_rename(DataDir),
    Config.

end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) ->
    FolderName = testing_tools:unslug(TestCase),
    BaseDir = filename:join(?config(data_dir, Config), FolderName),
    {ok, PWD} = file:get_cwd(),
    file:set_cwd(BaseDir),
    ct:log("Changed pwd to ~s", [BaseDir]),
    {ok, TestCase, ModuleSpecs, Abstract} = etr_compile:compile(BaseDir, FolderName),
    code:atomic_load(ModuleSpecs),
    ct:log("Modules loaded: ~p", [[Mod || {Mod, _, _} <- ModuleSpecs]]),
    Config1 = [
        {folder_name, FolderName},
        {base_dir, BaseDir},
        {old_pwd, PWD},
        {module_specs, ModuleSpecs},
        {test_abstract, Abstract}
        | Config
    ],
    ct:log("assembled config: ~p", [Config1]),
    Config1.

end_per_testcase(TestCase, Config) ->
    ct:log("EUnit stopped for ~p", [TestCase]),
    [code:purge(Mod) || {Mod, _, _} <- ?config(module_specs, Config)],
    ct:log("Purged modules from memory: ~p", [[Mod || {Mod, _, _} <- ?config(module_specs, Config)]]),
    file:set_cwd(?config(old_pwd, Config)).

'$handle_undefined_function'(F, A) ->
    case {lists:member(F, ?EXERCISES), A} of
        {true, [Config]} ->
            Result = etr_runner:run(F, ?config(test_abstract, Config)),
            ?assertTopLevel(pass, Result),
            ?assertNumFails(0, Result);
        _ ->
            error(undef)
    end.

allergies(Config) ->
    TestCodesExpected = [
        <<"'12_allergic_only_to_shellfish_test_'() ->\n    Score = 4,\n    {\"allergic only to shellfish\",\n     {73,\n      fun() ->\n             begin\n                 fun() ->\n                        X__T = is_process_alive(self()),\n                        case\n                            allergies:is_allergic_to(shellfish, Score)\n                        of\n                            X__T ->\n                                ok;\n                            X__V ->\n                                error({assert,\n                                       [{module, allergies_tests},\n                                        {line, 73},\n                                        {expression,\n                                         \"allergies : is_allergic_to ( \"\n                                         \"shellfish , Score )\"},\n                                        {expected, true},\n                                        case not X__T of\n                                            X__V ->\n                                                {value, false};\n                                            _ ->\n                                                {not_boolean, X__V}\n                                        end]})\n                        end\n                 end()\n             end\n      end}}.\n">>,
        <<"'13_allergic_to_shellfish_and_something_else_test_'() ->\n    Score = 14,\n    {\"allergic to shellfish and something else\",\n     {79,\n      fun() ->\n             begin\n                 fun() ->\n                        X__T = is_process_alive(self()),\n                        case\n                            allergies:is_allergic_to(shellfish, Score)\n                        of\n                            X__T ->\n                                ok;\n                            X__V ->\n                                error({assert,\n                                       [{module, allergies_tests},\n                                        {line, 79},\n                                        {expression,\n                                         \"allergies : is_allergic_to ( \"\n                                         \"shellfish , Score )\"},\n                                        {expected, true},\n                                        case not X__T of\n                                            X__V ->\n                                                {value, false};\n                                            _ ->\n                                                {not_boolean, X__V}\n                                        end]})\n                        end\n                 end()\n             end\n      end}}.\n">>,
        <<"'15_allergic_to_everything_test_'() ->\n    Score = 255,\n    {\"allergic to everything\",\n     {90,\n      fun() ->\n             begin\n                 fun() ->\n                        X__T = is_process_alive(self()),\n                        case\n                            allergies:is_allergic_to(shellfish, Score)\n                        of\n                            X__T ->\n                                ok;\n                            X__V ->\n                                error({assert,\n                                       [{module, allergies_tests},\n                                        {line, 90},\n                                        {expression,\n                                         \"allergies : is_allergic_to ( \"\n                                         \"shellfish , Score )\"},\n                                        {expected, true},\n                                        case not X__T of\n                                            X__V ->\n                                                {value, false};\n                                            _ ->\n                                                {not_boolean, X__V}\n                                        end]})\n                        end\n                 end()\n             end\n      end}}.\n">>,
        <<"'46_more_than_eggs_but_not_peanuts_test_'() ->\n    {\"more than eggs but not peanuts\",\n     {252,\n      fun() ->\n             begin\n                 fun() ->\n                        case lists:sort(allergies:allergies(5)) of\n                            [eggs, shellfish] ->\n                                ok;\n                            X__V ->\n                                error({assertMatch,\n                                       [{module, allergies_tests},\n                                        {line, 252},\n                                        {expression,\n                                         \"lists : sort ( allergies : al\"\n                                         \"lergies ( 5 ) )\"},\n                                        {pattern,\n                                         \"[ eggs , shellfish ]\"},\n                                        {value, X__V}]})\n                        end\n                 end()\n             end\n      end}}.\n">>,
        <<"'48_everything_test_'() ->\n    {\"everything\",\n     {263,\n      fun() ->\n             begin\n                 fun() ->\n                        case lists:sort(allergies:allergies(255)) of\n                            [cats, chocolate, eggs, peanuts, pollen,\n                             shellfish, strawberries, tomatoes] ->\n                                ok;\n                            X__V ->\n                                error({assertMatch,\n                                       [{module, allergies_tests},\n                                        {line, 264},\n                                        {expression,\n                                         \"lists : sort ( allergies : al\"\n                                         \"lergies ( 255 ) )\"},\n                                        {pattern,\n                                         \"[ cats , chocolate , eggs , p\"\n                                         \"eanuts , pollen , shellfish ,\"\n                                         \" strawberries , tomatoes ]\"},\n                                        {value, X__V}]})\n                        end\n                 end()\n             end\n      end}}.\n">>,
        <<"'49_no_allergen_score_parts_test_'() ->\n    {\"no allergen score parts\",\n     {269,\n      fun() ->\n             begin\n                 fun() ->\n                        case lists:sort(allergies:allergies(509)) of\n                            [cats, chocolate, eggs, pollen, shellfish,\n                             strawberries, tomatoes] ->\n                                ok;\n                            X__V ->\n                                error({assertMatch,\n                                       [{module, allergies_tests},\n                                        {line, 270},\n                                        {expression,\n                                         \"lists : sort ( allergies : al\"\n                                         \"lergies ( 509 ) )\"},\n                                        {pattern,\n                                         \"[ cats , chocolate , eggs , p\"\n                                         \"ollen , shellfish , strawberr\"\n                                         \"ies , tomatoes ]\"},\n                                        {value, X__V}]})\n                        end\n                 end()\n             end\n      end}}.\n">>
    ],
    Result = etr_runner:run(allergies, ?config(test_abstract, Config)),
    TestCodesActual = extract_test_codes(map_get(tests, Result)),
    %% erlang:error(Result),
    ?assertTopLevel(fail, Result),
    ?assertNumFails(6, Result),
    ?assertEqual(length(TestCodesExpected), length(TestCodesActual)),
    ct:log(lists:zip(TestCodesExpected, TestCodesActual)),
    [
        ?assertEqual(TestCodeExp, TestCodeAct)
     || {TestCodeExp, TestCodeAct} <- lists:zip(TestCodesExpected, TestCodesActual)
    ].

two_fer(Config) ->
    Result = etr_runner:run(two_fer, ?config(test_abstract, Config)),
    ?assertTopLevel(fail, Result),
    ?assertNumFails(1, Result).

extract_test_codes(L) ->
    extract_test_codes(L, []).

extract_test_codes([], Acc) -> lists:reverse(Acc);
extract_test_codes([#{test_code := Code} | T], Acc) -> extract_test_codes(T, [Code | Acc]);
extract_test_codes([_ | T], Acc) -> extract_test_codes(T, Acc).
