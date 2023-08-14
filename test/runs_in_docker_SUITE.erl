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

    %% Generic test implementation
    '$handle_undefined_function'/2
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
    testing_tools:scan_and_rename(DataDir),
    case {os:find_executable("docker"), os:getenv("SOURCE_PATH", false)} of
        {false, _} ->
            {skip, no_docker};
        {_, false} ->
            {skip, no_sources};
        {Docker, SourcePath} ->
            ImageName = testing_tools:random_docker_name(),
            {done, 0, DockerBuildOutput} = erlsh:run([
                Docker,
                build,
                "-t",
                ImageName,
                SourcePath
            ]),
            ct:log("Built image '~s': ~s", [ImageName, DockerBuildOutput]),
            [
                {image_name, ImageName},
                {docker, Docker}
                | Config
            ]
    end.

end_per_suite(Config) ->
    {done, 0, DockerRm} = erlsh:run([?config(docker, Config), rmi, ?config(image_name, Config)]),
    ct:log("Deleted image '~s': ~s", [?config(image_name, Config), DockerRm]),
    Config.

init_per_testcase(TestCase, Config) ->
    FolderName = testing_tools:unslug(TestCase),
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
    ImageName = ?config(image_name, Config),
    Target = filename:join(?config(priv_dir, Config), ?config(folder_name, Config)),
    {done, 0, _} = erlsh:run([
        ?config(docker, Config),
        "run",
        "--rm",
        "--volume",
        io_lib:format("~s:/input:ro", [?config(base_dir, Config)]),
        "--volume",
        io_lib:format("~s:/output", [Target]),
        ImageName,
        ?config(folder_name, Config),
        "/input",
        "/output"
    ]).

check_result(Config) ->
    {ok, JSON} = file:read_file(
        filename:join([?config(priv_dir, Config), ?config(folder_name, Config), "results.json"])
    ),
    #{<<"version">> := Version, <<"status">> := Status} = R = jsx:decode(JSON),
    ct:log("Testresults: ~p", [R]),
    {Version, Status}.

'$handle_undefined_function'(F, A) ->
    case {lists:member(F, ?EXERCISES), A} of
        {true, [Config]} ->
            run_docker(Config),
            {V, S} = check_result(Config),
            ?assertEqual(2, V),
            ?assertMatch(<<"pass">>, S);
        _ ->
            error(undef)
    end.
