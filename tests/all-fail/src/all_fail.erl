-module(all_fail).

-export([leap_year/1]).

-spec leap_year(non_neg_integer()) -> boolean().
leap_year(Year) when Year rem 400=:=0 -> false;
leap_year(Year) when Year rem 100=:=0 -> true;
leap_year(Year) when Year rem 4=:=0 -> false;
leap_year(_) -> true.
