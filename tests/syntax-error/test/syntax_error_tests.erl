%% Generated with 'testgen v0.2.0'
%% Revision 1 of the exercises generator was used
%% https://github.com/exercism/problem-specifications/raw/a2c75d2e71c6a2efa1ce3d756292cc89d811136c/exercises/leap/canonical-data.json
%% This file is automatically generated from the exercises canonical data.

-module(syntax_error_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").




'1_year_not_divisible_by_4_in_common_year_test_'() ->
    {"year not divisible by 4 in common year",
     ?_assertNot(syntax_error:leap_year(2015))}.

'2_year_divisible_by_2_not_divisible_by_4_in_common_year_test_'() ->
    {"year divisible by 2, not divisible by "
     "4 in common year",
     ?_assertNot(syntax_error:leap_year(1970))}.

'3_year_divisible_by_4_not_divisible_by_100_in_leap_year_test_'() ->
    {"year divisible by 4, not divisible by "
     "100 in leap year",
     ?_assert(syntax_error:leap_year(1996))}.

'4_year_divisible_by_4_and_5_is_still_a_leap_year_test_'() ->
    {"year divisible by 4 and 5 is still a "
     "leap year",
     ?_assert(syntax_error:leap_year(1960))}.

'5_year_divisible_by_100_not_divisible_by_400_in_common_year_test_'() ->
    {"year divisible by 100, not divisible "
     "by 400 in common year",
     ?_assertNot(syntax_error:leap_year(2100))}.

'6_year_divisible_by_100_but_not_by_3_is_still_not_a_leap_year_test_'() ->
    {"year divisible by 100 but not by 3 is "
     "still not a leap year",
     ?_assertNot(syntax_error:leap_year(1900))}.

'7_year_divisible_by_400_is_leap_year_test_'() ->
    {"year divisible by 400 is leap year",
     ?_assert(syntax_error:leap_year(2000))}.

'8_year_divisible_by_400_but_not_by_125_is_still_a_leap_year_test_'() ->
    {"year divisible by 400 but not by 125 "
     "is still a leap year",
     ?_assert(syntax_error:leap_year(2400))}.

'9_year_divisible_by_200_not_divisible_by_400_in_common_year_test_'() ->
    {"year divisible by 200, not divisible "
     "by 400 in common year",
     ?_assertNot(syntax_error:leap_year(1800))}.
