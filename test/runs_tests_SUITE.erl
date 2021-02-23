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

    %% Test Cases
    accumulate/1,
    allergies/1,
    all_your_base/1,
    anagram/1,
    armstrong_numbers/1,
    atbash_cipher/1,
    bank_account/1,
    beer_song/1,
    bob/1,
    book_store/1,
    bracket_push/1,
    change/1,
    circular_buffer/1,
    clock/1,
    collatz_conjecture/1,
    complex_numbers/1,
    connect/1,
    crypto_square/1,
    custom_set/1,
    darts/1,
    diamond/1,
    difference_of_squares/1,
    dominoes/1,
    etl/1,
    forth/1,
    gigasecond/1,
    grade_school/1,
    grains/1,
    hamming/1,
    hello_world/1,
    isbn_verifier/1,
    isogram/1,
    largest_series_product/1,
    leap/1,
    list_ops/1,
    luhn/1,
    meetup/1,
    minesweeper/1,
    nth_prime/1,
    nucleotide_count/1,
    palindrome_products/1,
    pangram/1,
    parallel_letter_frequency/1,
    pascals_triangle/1,
    perfect_numbers/1,
    phone_number/1,
    poker/1,
    prime_factors/1,
    protein_translation/1,
    pythagorean_triplet/1,
    queen_attack/1,
    rail_fence_cipher/1,
    raindrops/1,
    rational_numbers/1,
    rna_transcription/1,
    robot_simulator/1,
    roman_numerals/1,
    rotational_cipher/1,
    run_length_encoding/1,
    saddle_points/1,
    satellite/1,
    scrabble_score/1,
    secret_handshake/1,
    series/1,
    sieve/1,
    simple_linked_list/1,
    space_age/1,
    spiral_matrix/1,
    strain/1,
    sublist/1,
    sum_of_multiples/1,
    transpose/1,
    triangle/1,
    two_fer/1,
    variable_length_quantity/1,
    word_count/1,
    zipper/1
]).

-define(assertVersion(Expr), ?assertMatch(#{version := 2}, Expr)).
-define(assertTopLevel(Status, Expr),
    ?assertVersion(Expr),
    ?assertMatch(#{status := Status}, Expr)
).

all() ->
    [{group, sample_exercises}].

groups() ->
    [
        {sample_exercises, [shuffle], [
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
        ]}
    ].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    scan_and_rename(DataDir),
    Config.

end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) ->
    FolderName = unslug(TestCase),
    BaseDir = filename:join(?config(data_dir, Config), FolderName),
    {ok, PWD} = file:get_cwd(),
    file:set_cwd(BaseDir),
    ct:log("Changed pwd to ~s", [BaseDir]),
    {TestCase, ModuleSpecs} = etr_compile:compile(BaseDir, FolderName),
    code:atomic_load(ModuleSpecs),
    ct:log("Modules loaded: ~p", [[Mod || {Mod, _, _} <- ModuleSpecs]]),
    eunit:start(),
    ct:log("EUnit started"),
    [
        {folder_name, FolderName},
        {base_dir, BaseDir},
        {old_pwd, PWD},
        {module_specs, ModuleSpecs}
        | Config
    ].

end_per_testcase(TestCase, Config) ->
    eunit:stop(),
    ct:log("EUnit stopped"),
    [code:purge(Mod) || {Mod, _, _} <- ?config(module_specs, Config)],
    ct:log("Purged modules from memory: ~p", [[Mod || {Mod, _, _} <- ?config(module_specs, Config)]]),
    file:set_cwd(?config(old_pwd, Config)).

accumulate(_Config) ->
    Result = etr_runner:run(accumulate),
    ?assertTopLevel(<<"pass">>, Result).

allergies(_Config) -> ok = eunit:test(allergies).
all_your_base(_Config) -> ok = eunit:test(all_your_base).
anagram(_Config) -> ok = eunit:test(anagram).
armstrong_numbers(_Config) -> ok = eunit:test(armstrong_numbers).
atbash_cipher(_Config) -> ok = eunit:test(atbash_cipher).
bank_account(_Config) -> ok = eunit:test(bank_account).
beer_song(_Config) -> ok = eunit:test(beer_song).
bob(_Config) -> ok = eunit:test(bob).
book_store(_Config) -> ok = eunit:test(book_store).
bracket_push(_Config) -> ok = eunit:test(bracket_push).
change(_Config) -> ok = eunit:test(change).
circular_buffer(_Config) -> ok = eunit:test(circular_buffer).
clock(_Config) -> ok = eunit:test(clock).
collatz_conjecture(_Config) -> ok = eunit:test(collatz_conjecture).
complex_numbers(_Config) -> ok = eunit:test(complex_numbers).
connect(_Config) -> ok = eunit:test(connect).
crypto_square(_Config) -> ok = eunit:test(crypto_square).
custom_set(_Config) -> ok = eunit:test(custom_set).
darts(_Config) -> ok = eunit:test(darts).
diamond(_Config) -> ok = eunit:test(diamond).
difference_of_squares(_Config) -> ok = eunit:test(difference_of_squares).
dominoes(_Config) -> ok = eunit:test(dominoes).
etl(_Config) -> ok = eunit:test(etl).
forth(_Config) -> ok = eunit:test(forth).
gigasecond(_Config) -> ok = eunit:test(gigasecond).
grade_school(_Config) -> ok = eunit:test(grade_school).
grains(_Config) -> ok = eunit:test(grains).
hamming(_Config) -> ok = eunit:test(hamming).
hello_world(_Config) -> ok = eunit:test(hello_world).
isbn_verifier(_Config) -> ok = eunit:test(isbn_verifier).
isogram(_Config) -> ok = eunit:test(isogram).
largest_series_product(_Config) -> ok = eunit:test(largest_series_product).
leap(_Config) -> ok = eunit:test(leap).
list_ops(_Config) -> ok = eunit:test(list_ops).
luhn(_Config) -> ok = eunit:test(luhn).
meetup(_Config) -> ok = eunit:test(meetup).
minesweeper(_Config) -> ok = eunit:test(minesweeper).
nth_prime(_Config) -> ok = eunit:test(nth_prime).
nucleotide_count(_Config) -> ok = eunit:test(nucleotide_count).
palindrome_products(_Config) -> ok = eunit:test(palindrome_products).
pangram(_Config) -> ok = eunit:test(pangram).
parallel_letter_frequency(_Config) -> ok = eunit:test(parallel_letter_frequency).
pascals_triangle(_Config) -> ok = eunit:test(pascals_triangle).
perfect_numbers(_Config) -> ok = eunit:test(perfect_numbers).
phone_number(_Config) -> ok = eunit:test(phone_number).
poker(_Config) -> ok = eunit:test(poker).
prime_factors(_Config) -> ok = eunit:test(prime_factors).
protein_translation(_Config) -> ok = eunit:test(protein_translation).
pythagorean_triplet(_Config) -> ok = eunit:test(pythagorean_triplet).
queen_attack(_Config) -> ok = eunit:test(queen_attack).
rail_fence_cipher(_Config) -> ok = eunit:test(rail_fence_cipher).
raindrops(_Config) -> ok = eunit:test(raindrops).
rational_numbers(_Config) -> ok = eunit:test(rational_numbers).
rna_transcription(_Config) -> ok = eunit:test(rna_transcription).
robot_simulator(_Config) -> ok = eunit:test(robot_simulator).
roman_numerals(_Config) -> ok = eunit:test(roman_numerals).
rotational_cipher(_Config) -> ok = eunit:test(rotational_cipher).
run_length_encoding(_Config) -> ok = eunit:test(run_length_encoding).
saddle_points(_Config) -> ok = eunit:test(saddle_points).
satellite(_Config) -> ok = eunit:test(satellite).
scrabble_score(_Config) -> ok = eunit:test(scrabble_score).
secret_handshake(_Config) -> ok = eunit:test(secret_handshake).
series(_Config) -> ok = eunit:test(series).
sieve(_Config) -> ok = eunit:test(sieve).
simple_linked_list(_Config) -> ok = eunit:test(simple_linked_list).
space_age(_Config) -> ok = eunit:test(space_age).
spiral_matrix(_Config) -> ok = eunit:test(spiral_matrix).
strain(_Config) -> ok = eunit:test(strain).
sublist(_Config) -> ok = eunit:test(sublist).
sum_of_multiples(_Config) -> ok = eunit:test(sum_of_multiples).
transpose(_Config) -> ok = eunit:test(transpose).
triangle(_Config) -> ok = eunit:test(triangle).

two_fer(_Config) ->
    Result = etr_runner:run(two_fer),
    ?assertTopLevel(<<"fail">>, Result).

variable_length_quantity(_Config) -> ok = eunit:test(variable_length_quantity).
word_count(_Config) -> ok = eunit:test(word_count).
zipper(_Config) -> ok = eunit:test(zipper).

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
