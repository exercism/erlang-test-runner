-module(runs_in_docker_SUITE).

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

    '$handle_undefined_function'/2

    %% Test Cases
    %% accumulate/1,
    %% allergies/1,
    %% all_your_base/1,
    %% anagram/1,
    %% armstrong_numbers/1,
    %% atbash_cipher/1,
    %% bank_account/1,
    %% beer_song/1,
    %% bob/1,
    %% book_store/1,
    %% bracket_push/1,
    %% change/1,
    %% circular_buffer/1,
    %% clock/1,
    %% collatz_conjecture/1,
    %% complex_numbers/1,
    %% connect/1,
    %% crypto_square/1,
    %% custom_set/1,
    %% darts/1,
    %% diamond/1,
    %% difference_of_squares/1,
    %% dominoes/1,
    %% etl/1,
    %% forth/1,
    %% gigasecond/1,
    %% grade_school/1,
    %% grains/1,
    %% hamming/1,
    %% hello_world/1,
    %% isbn_verifier/1,
    %% isogram/1,
    %% largest_series_product/1,
    %% leap/1,
    %% list_ops/1,
    %% luhn/1,
    %% meetup/1,
    %% minesweeper/1,
    %% nth_prime/1,
    %% nucleotide_count/1,
    %% palindrome_products/1,
    %% pangram/1,
    %% parallel_letter_frequency/1,
    %% pascals_triangle/1,
    %% perfect_numbers/1,
    %% phone_number/1,
    %% poker/1,
    %% prime_factors/1,
    %% protein_translation/1,
    %% pythagorean_triplet/1,
    %% queen_attack/1,
    %% rail_fence_cipher/1,
    %% raindrops/1,
    %% rational_numbers/1,
    %% rna_transcription/1,
    %% robot_simulator/1,
    %% roman_numerals/1,
    %% rotational_cipher/1,
    %% run_length_encoding/1,
    %% saddle_points/1,
    %% satellite/1,
    %% scrabble_score/1,
    %% secret_handshake/1,
    %% series/1,
    %% sieve/1,
    %% simple_linked_list/1,
    %% space_age/1,
    %% spiral_matrix/1,
    %% strain/1,
    %% sublist/1,
    %% sum_of_multiples/1,
    %% transpose/1,
    %% triangle/1,
    %% two_fer/1,
    %% variable_length_quantity/1,
    %% word_count/1,
    %% zipper/1
]).

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
    scan_and_rename(DataDir),
    case os:find_executable("docker") of
        false ->
            {skip, no_docker};
        Docker ->
            [{docker, Docker} | Config]
    end.

end_per_suite(Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    FolderName = unslug(TestCase),
    BaseDir = filename:join(?config(data_dir, Config), FolderName),
    Config1 = [
        {folder_name, FolderName},
        {base_dir, BaseDir}
        | Config
    ],
    ct:log("assembled config: ~p", [Config1]),
    Config1.

end_per_testcase(_TestCase, Config) -> Config.

run_docker(Config) ->
    Target = filename:join(?config(priv_dir, Config), ?config(folder_name, Config)),
    Cmd = unicode:characters_to_list(
        io_lib:format(
            "~s run --volume ~s:/input --volume ~s:/output erlang_test_runner:common_test ~s /input /output/",
            [
                ?config(docker, Config),
                ?config(base_dir, Config),
                Target,
                ?config(folder_name, Config)
            ]
        )
    ),
    ct:log(os:cmd(Cmd)).

check_result(Config) ->
    {ok, JSON} = file:read_file(
        filename:join([?config(priv_dir, Config), ?config(folder_name, Config), "results.json"])
    ),
    #{<<"version">> := 2} = jsx:decode(JSON).

'$handle_undefined_function'(F, A) ->
    case {lists:member(F, ?EXERCISES), A} of
        {true, [Config]} ->
            run_docker(Config),
            check_result(Config);
        _ ->
            error(undef)
    end.

scan_and_rename(Path) ->
    case {filelib:is_dir(Path), re:run(Path, "\\.er_$", [{capture, none}])} of
        {true, _} ->
            {ok, Files} = file:list_dir_all(Path),
            Pathes = [filename:join(Path, File) || File <- Files],
            [scan_and_rename(Path2) || Path2 <- Pathes],
            ok;
        {false, match} ->
            NewName = iolist_to_binary(re:replace(Path, ".er_$", ".erl")),
            do_rename(Path, NewName),
            ok;
        {false, nomatch} ->
            ct:log("Skipping file ~p", [Path]),
            ok
    end.

do_rename(OldName, NewName) ->
    case file:read_file_info(NewName) of
        {error, enoent} ->
            ct:log("Target ~s does not exist, renaming", [NewName]);
        {ok, _} ->
            ct:log("Target ~s does already exist, deleting", [NewName]),
            ok = file:delete(NewName)
    end,
    case file:rename(OldName, NewName) of
        ok ->
            ct:log("Renamed File ~s to ~s", [OldName, NewName]);
        {error, Reason} ->
            ct:log("Renaming ~s to ~s failed with reason ~p", [OldName, NewName, Reason])
    end.

unslug(Atom) when is_atom(Atom) ->
    unslug(atom_to_list(Atom));
unslug(String) ->
    Unslugged = string:replace(String, "_", "-", all),
    ct:log("Unslugged: ~p, ~s", [Unslugged, Unslugged]),
    Unslugged.
