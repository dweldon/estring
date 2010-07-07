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
         similarity/4,
         strip/1,
         strip_split/2,
         squeeze/1,
         squeeze/2,
         is_integer/1,
         format/2,
         random/1]).
-define(CHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").
-include_lib("eunit/include/eunit.hrl").

% @spec begins_with(String::string(), SubString::string()) -> bool()
begins_with(String, SubString) ->
    string:substr(String, 1, length(SubString)) =:= SubString.

% @spec ends_with(String::string(), SubString::string()) -> bool()
ends_with(String, SubString) ->
    begins_with(lists:reverse(String), lists:reverse(SubString)).

% @spec contains(String::string(), SubString::string()) -> bool()
contains(String, SubString) ->
    string:str(String, SubString) > 0.

% @spec edit_distance(String1::string(), String2::string()) -> integer()
edit_distance(String1, String2, true) ->
    S1 = string:to_lower(String1),
    S2 = string:to_lower(String2),
    edit_distance(S1, S2);
edit_distance(String1, String2, false) ->
    edit_distance(String1, String2).

% @spec edit_distance(String1::string(), String2::string(),
%                     CaseInsensitive::bool()) -> integer()
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

% @spec edit_distance_estimate(L1::list(), L2::list()) -> integer()
% @doc establishes a very conservate lower bound for edit distance - useful only
% for early exit evaluations.
edit_distance_estimate(L, L) -> 0.0;
edit_distance_estimate(L1, L2) ->
    % divide the estimate by 2 because replacements will be double counted.
    % the downside of this is that inserts or deletes are undercounted.
    edit_distance_estimate(lists:sort(L1), lists:sort(L2), 0.0) / 2.

edit_distance_estimate([], L, D) ->
    D + length(L);
edit_distance_estimate(L, [], D) ->
    D + length(L);
edit_distance_estimate([H1|L1], [H2|L2], D) ->
    if
        H1 =:= H2 ->
            edit_distance_estimate(L1, L2, D);
        H1 < H2 ->
            edit_distance_estimate(L1, [H2|L2], D+1);
        H1 > H2 ->
            edit_distance_estimate([H1|L1], L2, D+1)
    end.

% @spec similarity(Source::string(), Target::string()) -> float()
similarity(Source, Source) -> 1.0;
similarity(Source, Target) ->
    Score = (length(Target) - edit_distance(Source, Target)) / length(Target),
    case Score > 0 of
        true -> Score;
        false -> 0.0
    end.

% @spec similarity(Source::string(), Target::string(),
%                  CaseInsensitive::bool()) -> float()
similarity(Source, Target, true) ->
    S = string:to_lower(Source),
    T = string:to_lower(Target),
    similarity(S, T);
similarity(Source, Target, false) ->
    similarity(Source, Target).

% @spec similarity(Source::string(), Target::string(), CaseInsensitive::bool(),
%                  LowerLimit::float()) -> {ok, float()} | {error, limit_reached}
similarity(Source, Target, CaseInsensitive, LowerLimit) ->
    {S, T} = case CaseInsensitive of
                 true -> {string:to_lower(Source), string:to_lower(Target)};
                 false -> {Source, Target}
             end,
    case similarity_estimate(S, T) >= LowerLimit of
        true ->
            Score = similarity(S, T),
            case Score >= LowerLimit of
                true -> {ok, Score};
                false ->  {error, limit_reached}
            end;
        false -> {error, limit_reached}
    end.

% @spec similarity_estimate(Source::string(), Target::string()) -> float()
% @doc establishes a very conservate upper bound for string similarity
similarity_estimate(S, S) -> 1.0;
similarity_estimate(S, T) ->
    DistanceEstimate = edit_distance_estimate(S, T),
    SimilarityEstimate = (length(T) - DistanceEstimate ) / length(T),
    case SimilarityEstimate > 0 of
        true -> SimilarityEstimate;
        false -> 0.0
    end.

% @spec strip(String::string()) -> string()
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

% @spec strip_split(String:string(), SeparatorString:string()) -> list()
strip_split(String, SeparatorString) ->
    re:split(strip(String), SeparatorString, [{return, list}]).

% @spec squeeze(String::string()) -> string()
squeeze(String) -> squeeze(String, " ").

% @spec squeeze(String::string(), Char::character()) -> string()
% where
%       character() = integer() | string() with length =:= 1
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

% @spec is_integer(String::string()) -> bool()
is_integer([]) ->
    false;
is_integer(String) ->
    lists:all(fun(C) -> C >= 48 andalso C =< 57 end, String).

% @spec format(Format::string(), Data::list()) -> string()
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

% @spec random(N::integer()) -> string()
random(N) when N > 0->
    [random_character() || _ <- lists:seq(1, N)].

random_character() ->
    lists:nth(random:uniform(62), ?CHARS).

begins_with_test_() ->
    [?_assertEqual(true, begins_with("foobar", "foo")),
     ?_assertEqual(false, begins_with("foobar", "bar"))].

ends_with_test_() ->
    [?_assertEqual(false, ends_with("foobar", "foo")),
     ?_assertEqual(true, ends_with("foobar", "bar"))].

contains_test_() ->
    [?_assertEqual(true, contains("foobar", "foo")),
     ?_assertEqual(true, contains("foobar", "bar")),
     ?_assertEqual(true, contains("foobar", "oba")),
     ?_assertEqual(false, contains("foobar", "car"))].

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

edit_distance_estimate_test_() ->
    [?_assertEqual(0.0, edit_distance_estimate("abc", "abc")),
     ?_assertEqual(0.0, edit_distance_estimate("", "")),
     ?_assertEqual(1.5, edit_distance_estimate("abc", "")),
     ?_assertEqual(1.5, edit_distance_estimate("", "abc")),
     ?_assertEqual(0.0, edit_distance_estimate("abc", "cba")),
     ?_assertEqual(1.0, edit_distance_estimate("abc", "xbc")),
     ?_assertEqual(0.5, edit_distance_estimate("abc", "abbc")),
     ?_assertEqual(1.5, edit_distance_estimate("abcd", "abbcx")),
     ?_assertEqual(2.0, edit_distance_estimate("abcd", "aabbccdd"))].

similarity_estimate_test_() ->
    [?_assertEqual(1.0, similarity_estimate("", "")),
     ?_assertEqual(0.0, similarity_estimate("abc", "def")),
     ?_assertEqual(1.0, similarity_estimate("abc", "cba")),
     ?_assertEqual(0.8, similarity_estimate("abcde", "xbcde"))].

similarity2_test_() ->
    [?_assertEqual(0.8, similarity("yaho", "yahoo")),
     ?_assertEqual(0.75, similarity("espn", "epsn")),
     ?_assertEqual(0.25, similarity("car", "BaTS")),
     ?_assertEqual(0.0, similarity("cars", "c")),
     ?_assertEqual(0.25, similarity("c", "cars")),
     ?_assertEqual(1.0, similarity("", ""))].

similarity3_test_() ->
    [?_assertEqual(0.25, similarity("car", "BaTS", false)),
     ?_assertEqual(0.5, similarity("cars", "BATS", true))].

similarity4_test_() ->
    [?_assertEqual({ok, 1.0}, similarity("yahoo", "yahoo", true, 0.8)),
     ?_assertEqual({ok, 0.8}, similarity("yahoo", "bahoo", true, 0.8)),
     ?_assertEqual({ok, 0.8}, similarity("yahoo", "Yahoo", false, 0.7)),
     ?_assertEqual({error, limit_reached},
                   similarity("yahoo", "Yahoo", false, 0.9)),
     ?_assertEqual({error, limit_reached},
                   similarity("yahoo", "bahoo", true, 0.9))].

strip_test_() ->
    [?_assertEqual("hello world", strip("  hello world ")),
     ?_assertEqual("hello world", strip(" \t hello world\f\r")),
     ?_assertEqual("hello world", strip("hello world")),
     ?_assertEqual("hello  \tworld", strip(" hello  \tworld ")),
     ?_assertEqual("hello world", strip("hello world\n\n \t")),
     ?_assertEqual("", strip(" ")),
     ?_assertEqual("", strip(""))].

strip_split_test_() ->
    [?_assertEqual(["ab", "cd", "ef"], strip_split(" ab<#>cd<#>ef \n", "<#>")),
     ?_assertEqual(["a", "b", [], "c" ], strip_split("\ta,b,,c\r\f", ","))].

squeeze_test_() ->
    [?_assertEqual("i need a squeeze!", squeeze("i need   a  squeeze!")),
     ?_assertEqual("i need a squeeze!", squeeze("i need   a  squeeze!", " ")),
     ?_assertEqual("yelow moon", squeeze("yellow moon", "l")),
     ?_assertEqual("babon mon", squeeze("baboon moon", "o")),
     ?_assertEqual("babon mon", squeeze("baboon moon", $o)),
     ?_assertEqual("the cow says mo", squeeze("the cow says moooo", $o))].

is_integer_test_() ->
    [?_assertEqual(true, ?MODULE:is_integer("0123")),
     ?_assertEqual(true, ?MODULE:is_integer("456789")),
     ?_assertEqual(true, ?MODULE:is_integer("9")),
     ?_assertEqual(false, ?MODULE:is_integer("10.3")),
     ?_assertEqual(false, ?MODULE:is_integer("01 23")),
     ?_assertEqual(false, ?MODULE:is_integer("1x2")),
     ?_assertEqual(false, ?MODULE:is_integer("")),
     ?_assertEqual(false, ?MODULE:is_integer("abc"))].

format_test_() ->
    [?_assertEqual("99 bottles of beer on the wall",
                   format("~w bottles of ~s on the wall", [99, "beer"])),
     ?_assertEqual("", format("",[]))].

random_test() ->
    ?assertEqual(100, length(random(100))).
