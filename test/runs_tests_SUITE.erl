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

    '$handle_undefined_function'/2,

    %% Test Cases
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
                (#{status := <<"pass">>}, C) -> C;
                (#{status := <<"fail">>}, C) -> C + 1
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
    {TestCase, ModuleSpecs} = etr_compile:compile(BaseDir, FolderName),
    code:atomic_load(ModuleSpecs),
    ct:log("Modules loaded: ~p", [[Mod || {Mod, _, _} <- ModuleSpecs]]),
    Config1 = [
        {folder_name, FolderName},
        {base_dir, BaseDir},
        {old_pwd, PWD},
        {module_specs, ModuleSpecs}
        | Config
    ],
    ct:log("assembled config: ~p", [Config1]),
    Config1.

end_per_testcase(TestCase, Config) ->
    %% eunit:stop(),
    ct:log("EUnit stopped"),
    [code:purge(Mod) || {Mod, _, _} <- ?config(module_specs, Config)],
    ct:log("Purged modules from memory: ~p", [[Mod || {Mod, _, _} <- ?config(module_specs, Config)]]),
    file:set_cwd(?config(old_pwd, Config)).

'$handle_undefined_function'(F, A) ->
    case {lists:member(F, ?EXERCISES), A} of
        {true, [Config]} ->
            Result = etr_runner:run(F),
            ?assertTopLevel(<<"pass">>, Result),
            ?assertNumFails(0, Result);
        _ ->
            error(undef)
    end.

allergies(_Config) ->
    Result = etr_runner:run(allergies),
    ?assertTopLevel(<<"fail">>, Result),
    ?assertNumFails(6, Result).

two_fer(_Config) ->
    Result = etr_runner:run(two_fer),
    ?assertTopLevel(<<"fail">>, Result),
    ?assertNumFails(1, Result).
