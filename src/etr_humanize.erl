-module(etr_humanize).

-export([assertion/1, compilation/1]).

-spec assertion({atom(), proplists:proplist()}) -> iodata().
assertion({assertStringEqual, Info}) ->
    io_lib:format(
        "The expression `~s` was expected to return a string or binary "
        "or nested list of either, that compares equal to ~p, but it returned "
        "~p instead",
        [
            proplists:get_value(expression, Info),
            proplists:get_value(expected, Info),
            proplists:get_value(value, Info)
        ]
    );
assertion({assert, Info}) ->
    io_lib:format(
        "The expression `~s` was expected to return `true`, but it returned "
        "`~p` instead",
        [
            proplists:get_value(expression, Info),
            proplists:get_value(value, Info)
        ]
    );
assertion({assertMatch, Info}) ->
    io_lib:format(
        "The expression `~s` was expected to return a value that matches the "
        "value `~p`, though the returned value `~p` does not match",
        [
            proplists:get_value(expression, Info),
            proplists:get_value(pattern, Info),
            proplists:get_value(value, Info)
        ]
    );
assertion({assertEqual, Info}) ->
    io_lib:format(
        "The expression `~p` was expected to return a value that equals the "
        "value `~p`, though the returned value `~p` is different",
        [
            proplists:get_value(expression, Info),
            proplists:get_value(expected, Info),
            proplists:get_value(value, Info)
        ]
    );
assertion({Assertion, Info}) ->
    io_lib:format(
        "An unknown assertion occured and failed, please report an issue at "
        "https://github.com/exercism/erlang-test-runner/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc "
        "for the '~p' assertion unless it already exists. Please provide the "
        "following additional info: Exercise, failed test and assertion info: '~p'",
        [Assertion, Info]
    ).

-spec compilation([
    {file:filename_all(), [
        {{non_neg_integer(), non_neg_integer()}, erl_parse | erl_lint, term()} | term()
    ]}
]) -> iodata().
compilation([]) ->
    "No errors, please report this together with your code at https://github.com/exercism/erlang-test-runner/issues/new";
compilation([{File, [{{L, _C}, erl_parse, ErrData}]} | _]) ->
    ErrMessage = erl_parse:format_error(ErrData),
    FileName = filename:basename(File),
    io_lib:format("Could not parse the file '~s' at line ~p: ~s", [FileName, L, ErrMessage]);
compilation([{File, [{{L, _C}, erl_lint, ErrData}]} | _]) ->
    ErrMessage = erl_lint:format_error(ErrData),
    FileName = filename:basename(File),
    io_lib:format("Could not parse the file '~s' at line ~p: ~s", [FileName, L, ErrMessage]);
compilation([Unknown | _]) ->
    io_lib:format(
        "An unknown build error occured, please report an issue at "
        "https://github.com/exercism/erlang-test-runner/issues/new  "
        "and paste your full file into the report as well as "
        "this error info: ~p.",
        [Unknown]
    ).
