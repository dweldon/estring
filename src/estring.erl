%% Copyright (c) 2009 David Weldon
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(estring).
-export([begins_with/2,
         ends_with/2,
         contains/2,
         edit_distance/2,
         edit_distance/3,
         similarity/2,
         similarity/3,
         is_integer/1,
         format/2,
         strip/1,
         split/2,
         strip_split/2,
         squeeze/1,
         squeeze/2]).
-include_lib("eunit/include/eunit.hrl").

%-------------------------------------------------------------------------------
begins_with_test_() ->
    [?_assertEqual(true, begins_with("foobar", "foo")),
     ?_assertEqual(false, begins_with("foobar", "bar"))].

begins_with(String, SubString) ->
    string:substr(String, 1, length(SubString)) =:= SubString.

%-------------------------------------------------------------------------------
ends_with_test_() ->
    [?_assertEqual(false, ends_with("foobar", "foo")),
     ?_assertEqual(true, ends_with("foobar", "bar"))].

ends_with(String, SubString) ->
    begins_with(lists:reverse(String), lists:reverse(SubString)).

%-------------------------------------------------------------------------------
contains_test_() ->
    [?_assertEqual(true, contains("foobar", "foo")),
     ?_assertEqual(true, contains("foobar", "bar")),
     ?_assertEqual(true, contains("foobar", "oba")),
     ?_assertEqual(false, contains("foobar", "car"))].

contains(String, SubString) ->
    string:str(String, SubString) > 0.

%-------------------------------------------------------------------------------
edit_distance_test_() ->
    [?_assertEqual(0, edit_distance("computer", "computer")),
     %deletion
     ?_assertEqual(1, edit_distance("computer", "compter")),
     %substitution
     ?_assertEqual(1, edit_distance("computer", "camputer")),
     %insertion
     ?_assertEqual(1, edit_distance("computer", "computter")),
     %transposition
     ?_assertEqual(1, edit_distance("computer", "comupter")),
     %deletion + substitution + insertion
     ?_assertEqual(3, edit_distance("computer", "camputte")),
     %transposition + insertion + deletion
     ?_assertEqual(3, edit_distance("computer", "cmoputte")),
     %transposition + insertion + deletion, with source and target swapped
     ?_assertEqual(3, edit_distance("cmoputte", "computer")),
     ?_assertEqual(3, edit_distance("cars", "BaTS", false)),
     ?_assertEqual(3, edit_distance("cars", "BaTS")),
     ?_assertEqual(2, edit_distance("cars", "BaTS", true))].

edit_distance(String1, String2, true) ->
    S1 = string:to_lower(String1),
    S2 = string:to_lower(String2),
    edit_distance(S1, S2);
edit_distance(String1, String2, false) ->
    edit_distance(String1, String2).

edit_distance(Source, Source) -> 0;
edit_distance(Source, []) -> length(Source);
edit_distance([], Source) -> length(Source);
edit_distance(Source, Target) ->
    D1 = lists:seq(0, length(Target)),
    outer_loop([[]|Source], [[]|Target], {D1, D1}, 1).

outer_loop([S1|[S0|S]], T, {D2, D1}, I) ->
    D0 = inner_loop(T, [S1, S0], {[[]|D2], D1, [I]}),
    outer_loop([S0|S], T, {D1, D0}, I + 1);
outer_loop([_S|[]], _, {_D1, D0}, _) ->
    lists:last(D0).

inner_loop([_T|[]], _, {_D2, _D1, D0}) ->
    lists:reverse(D0);
inner_loop([T1|[T0|T]], [S1, S0], {D2, D1, D0}) ->
    [S1T1|[S1T0|_]] = D1,
    Cost = if T0 =:= S0 -> 0; true -> 1 end,
    NewDist1 = lists:min([hd(D0) + 1, S1T0 + 1, S1T1 + Cost]),
    NewDist2 =
        if T1 =/= [] andalso S1 =/= [] andalso T1 =:= S0 andalso T0 =:= S1 ->
                lists:min([NewDist1, hd(D2) + Cost]);
           true -> NewDist1
        end,
    inner_loop([T0|T], [S1, S0], {tl(D2), tl(D1), [NewDist2|D0]}).

%-------------------------------------------------------------------------------
similarity_test_() ->
    [?_assertEqual(0.8, similarity("yaho", "yahoo")),
     ?_assertEqual(0.75, similarity("espn", "epsn")),
     ?_assertEqual(0.25, similarity("car", "BaTS", false)),
     ?_assertEqual(0.25, similarity("car", "BaTS")),
     ?_assertEqual(0.5, similarity("cars", "BATS", true)),
     ?_assertEqual(0.0, similarity("cars", "c")),
     ?_assertEqual(0.25, similarity("c", "cars")),
     ?_assertEqual(1.0, similarity("", ""))].

similarity(String, TargetString, true) ->
    S = string:to_lower(String),
    TS = string:to_lower(TargetString),
    similarity(S, TS);
similarity(String, TargetString, false) ->
    similarity(String, TargetString).

similarity([], []) ->
    1.0;
similarity(String, TargetString) ->
    Score = (length(TargetString) - edit_distance(String, TargetString)) /
            length(TargetString),
    case Score > 0 of
        true -> Score;
        false -> 0.0
    end.

%-------------------------------------------------------------------------------
is_integer_test_() ->
    [?_assertEqual(true, ?MODULE:is_integer("0123")),
     ?_assertEqual(true, ?MODULE:is_integer("456789")),
     ?_assertEqual(true, ?MODULE:is_integer("9")),
     ?_assertEqual(false, ?MODULE:is_integer("10.3")),
     ?_assertEqual(false, ?MODULE:is_integer("01 23")),
     ?_assertEqual(false, ?MODULE:is_integer("1x2")),
     ?_assertEqual(false, ?MODULE:is_integer("")),
     ?_assertEqual(false, ?MODULE:is_integer("abc"))].

is_integer([]) ->
    false;
is_integer(String) ->
    lists:all(fun(C) -> C >= 48 andalso C =< 57 end, String).

%-------------------------------------------------------------------------------
format_test_() ->
    [?_assertEqual("99 bottles of beer on the wall",
                   format("~w bottles of ~s on the wall", [99, "beer"])),
     ?_assertEqual("", format("",[]))].

format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%-------------------------------------------------------------------------------
squeeze_test_() ->
    [?_assertEqual("i need a squeeze!", squeeze("i need   a  squeeze!")),
     ?_assertEqual("i need a squeeze!", squeeze("i need   a  squeeze!", " ")),
     ?_assertEqual("yelow moon", squeeze("yellow moon", "l")),
     ?_assertEqual("babon mon", squeeze("baboon moon", "o")),
     ?_assertEqual("babon mon", squeeze("baboon moon", $o)),
     ?_assertEqual("the cow says mo", squeeze("the cow says moooo", $o))].

squeeze(String) ->
    squeeze(String, " ").

squeeze(String, Char) when erlang:is_integer(Char) ->
    squeeze(String, Char, [], []);
squeeze(String, Char) when is_list(Char) ->
    squeeze(String, hd(Char), [], []).

squeeze([], _, _, Result) ->
    lists:reverse(Result);
squeeze([H|T], H, H, Result) ->
    squeeze(T, H, H, Result);
squeeze([H|T], Char, _, Result) ->
    squeeze(T, Char, H, [H|Result]).

%-------------------------------------------------------------------------------
strip_test_() ->
    [?_assertEqual("hello world", strip("  hello world ")),
     ?_assertEqual("hello world", strip(" \t hello world\f\r")),
     ?_assertEqual("hello world", strip("hello world")),
     ?_assertEqual("hello  \tworld", strip(" hello  \tworld ")),
     ?_assertEqual("hello world", strip("hello world\n\n \t")),
     ?_assertEqual("", strip(" ")),
     ?_assertEqual("", strip(""))].

strip(String) ->
    strip(String, [], []).

strip([], _, Result) ->
    lists:reverse(Result);
strip([H|T], [], []) ->
    case whitespace(H) of
        true -> strip(T, [], []);
        false -> strip(T, [], [H])
    end;
strip([H|T], WhiteSpace, Result) ->
    case whitespace(H) of
        true -> strip(T, [H|WhiteSpace], Result);
        false -> strip(T, [], [H|WhiteSpace] ++ Result)
    end.

whitespace($\t) -> true;
whitespace($\n) -> true;
whitespace($\f) -> true;
whitespace($\r) -> true;
whitespace($\ ) -> true;
whitespace(_) -> false.

%-------------------------------------------------------------------------------
split_test_() ->
    [?_assertEqual(["ab", "cd", "ef"], split("ab,cd,ef", ",")),
     ?_assertEqual(["ab", "cd", "ef"], split("ab<#>cd<#>ef", "<#>")),
     ?_assertEqual(["a,b,c"], split("a,b,c", "x")),
     ?_assertEqual([[], "a", "b", [], [], "c", [] ], split(",a,b,,,c,", ","))].

split(String, SeparatorString) ->
    split(String, SeparatorString, [], []).

split([], _, Current, Result) ->
    lists:reverse([lists:reverse(Current)|Result]);
split(String, Sep, Current, Result) ->
    case begins_with(String, Sep) of
        true ->
            NewString = string:substr(String, length(Sep) + 1, length(String)),
            split(NewString, Sep, [], [lists:reverse(Current)|Result]);
        false ->
            [H|T] = String,
            split(T, Sep, [H|Current], Result)
    end.

%-------------------------------------------------------------------------------
strip_split_test_() ->
    [?_assertEqual(["ab", "cd", "ef"], strip_split(" ab<#>cd<#>ef \n", "<#>")),
     ?_assertEqual(["a", "b", [], "c" ], strip_split("\ta,b,,c\r\f", ","))].

strip_split(String, SeparatorString) ->
    split(strip(String), SeparatorString).
