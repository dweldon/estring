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
-export([begins/2,
         ends/2,
         contains/2,
         levenshtein/2,
         similarity/2]).
-include_lib("eunit/include/eunit.hrl").

%-------------------------------------------------------------------------------
begins_test_() ->
    [?_assertEqual(true, begins("foobar", "foo")),
     ?_assertEqual(false, begins("foobar", "bar"))].

begins(String, SubString) ->
    string:str(String, SubString) =:= 1.

%-------------------------------------------------------------------------------
ends_test_() ->
    [?_assertEqual(false, ends("foobar", "foo")),
     ?_assertEqual(true, ends("foobar", "bar"))].

ends(String, SubString) ->
    begins(lists:reverse(String), lists:reverse(SubString)).

%-------------------------------------------------------------------------------
contains_test_() ->
    [?_assertEqual(true, contains("foobar", "foo")),
     ?_assertEqual(true, contains("foobar", "bar")),
     ?_assertEqual(true, contains("foobar", "oba")),
     ?_assertEqual(false, contains("foobar", "car"))].

contains(String, SubString) ->
    string:str(String, SubString) > 0.

%-------------------------------------------------------------------------------
levenshtein_test_() ->
    [?_assertEqual(0, levenshtein("computer", "computer")),
     %deletion
     ?_assertEqual(1, levenshtein("computer", "compter")),
     %substitution
     ?_assertEqual(1, levenshtein("computer", "camputer")),
     %insertion
     ?_assertEqual(1, levenshtein("computer", "computter")),
     %transposition
     ?_assertEqual(1, levenshtein("computer", "comupter")),
     %deletion + substitution + insertion
     ?_assertEqual(3, levenshtein("computer", "camputte")),
     %transposition + insertion + deletion
     ?_assertEqual(3, levenshtein("computer", "cmoputte")),
     %transposition + insertion + deletion, with source and target swapped
     ?_assertEqual(3, levenshtein("cmoputte", "computer"))].

levenshtein(Source, Source) -> 0;
levenshtein(Source, []) -> length(Source);
levenshtein([], Source) -> length(Source);
levenshtein(Source, Target) ->
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
     ?_assertEqual(0.5, similarity("cars", "BATS")),
     ?_assertEqual(0.0, similarity("cars", "c")),
     ?_assertEqual(0.25, similarity("c", "cars")),
     ?_assertEqual(1.0, similarity("", ""))].

similarity([], []) ->
    1.0;
similarity(String, TargetString) ->
    S = string:to_lower(String),
    TS = string:to_lower(TargetString),    
    Score = (length(TS) - levenshtein(S, TS)) / length(TS),
    case Score > 0 of
        true -> Score;
        false -> 0.0
    end.
