%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2012 VMware, Inc.  All rights reserved.
%%

-module(test_do).
-compile({parse_transform, do}).

-compile(export_all).

test_sequence() ->
    List = lists:seq(1,5),
    ListM = [do([maybe_m || return(N)]) || N <- List],
    {ok, List} = monad:sequence(maybe_m, ListM).

test_join() ->
    {ok, 5} = monad:join(maybe_m,
                           maybe_m:return(maybe_m:return(5))),
    {ok, 5} = monad:join(maybe_m,
                           do([maybe_m || return(maybe_m:return(5))])),
    {ok, 5} = monad:join(maybe_m,
                           do([maybe_m || return(do([maybe_m || return(5)]))])).

test_maybe() ->
    undefined = maybe(atom),
    {ok, 9} = maybe(3).

maybe(Arg) ->
    do([maybe_m
        || monad_plus:guard(maybe_m, is_number(Arg)),
           return(Arg*Arg)]).

test_keyflatten() -> 
	[{a, 1}, {b, 2}] = maybe_m:keyflatten(2, 
		[{a, {ok, 1}}, {b, {ok, 2}}, {c, undefined}]).

test_fib() ->
    true = lists:all(fun ({X, Y}) -> X =:= Y end,
                     [{fib_m(N), fib_rec(N)} || N <- lists:seq(0, 20)]).

%% Classic monadic implementation of fibonnaci
fib_m(N) ->
    StateT = state_t:new(identity_m),
    {_, R} = StateT:exec(
               monad:sequence(StateT,
                              lists:duplicate(N, fib_m_step(StateT))), {0, 1}),
    R.

fib_m_step(StateT) -> StateT:modify(fun ({X, Y}) -> {Y, X+Y} end).

%% Classic recursive implementation of fibonnaci
fib_rec(N) when N >= 0 -> fib_rec(N, 0, 1).
fib_rec(0, _X, Y) -> Y;
fib_rec(N,  X, Y) -> fib_rec(N-1, Y, X+Y).

test_list() ->
    %% Demonstrate equivalence of list comprehensions and list monad
    A = [{X,Y} || X <- "abcd",
                  Y <- [1,2]],
    A = do([list_m || X <- "abcd",
                      Y <- [1,2],
                      return({X,Y})]),
    %% Classic pythagorean triples
    P = [{X, Y, Z} || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)],
    P = do([list_m || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      monad_plus:guard(
                        list_m, math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)),
                      return({X,Y,Z})]).

test_omega() ->
    A = [{X,Y,Z} || X <- "abcd",
                    Y <- lists:seq(1,5),
                    Z <- lists:seq(11,15)],
    B = do([omega_m || X <- "abcd",
                       Y <- lists:seq(1,5),
                       Z <- lists:seq(11,15),
                       return({X,Y,Z})]),
    true = A =/= B,
    true = A =:= lists:usort(B).

test_error_t_list() ->
    M = error_t:new(list_m),
    R = M:run(do([M || E1 <- M:lift([1, 2, 3]),
                       E2 <- M:lift([4, 5, 6]),
                       case (E1 * E2) rem 2 of
                           0 -> return({E1, E2});
                           _ -> fail(not_even_product)
                       end])),
    R = [{ok, {1, 4}}, {error, not_even_product}, {ok, {1, 6}},
         {ok, {2, 4}}, {ok, {2, 5}},              {ok, {2, 6}},
         {ok, {3, 4}}, {error, not_even_product}, {ok, {3, 6}}],

    %% Compare with the non-error_t version, which will remove failures:
    S = do([list_m || E1 <- [1, 2, 3],
                      E2 <- [4, 5, 6],
                      case (E1 * E2) rem 2 of
                          0 -> return({E1, E2});
                          _ -> fail(not_even_product)
                      end]),
    S = [{1, 4}, {1, 6}, {2, 4}, {2, 5}, {2, 6}, {3, 4}, {3, 6}].

test() ->
    test:test([{?MODULE, [test_sequence,
                          test_join,
                          test_maybe,
			  test_keyflatten,
                          test_fib,
                          test_list,
                          test_omega,
                          test_error_t_list]}],
              [report, {name, ?MODULE}]).
