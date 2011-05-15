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
%% Copyright (c) 2011-2011 VMware, Inc.  All rights reserved.
%%

-module(test_do).
-compile({parse_transform, do}).

-compile(export_all).

test_sequence() ->
    List = lists:seq(1,5),
    ListM = [do([maybe_m || return(N)]) || N <- List],
    {just, List} = monad:sequence(maybe_m, ListM).

test_join() ->
    {just, 5} = monad:join(maybe_m,
                           maybe_m:return(maybe_m:return(5))),
    {just, 5} = monad:join(maybe_m,
                           do([maybe_m || return(maybe_m:return(5))])),
    {just, 5} = monad:join(maybe_m,
                           do([maybe_m || return(do([maybe_m || return(5)]))])).

test_maybe() ->
    nothing = maybe(atom),
    {just, 9} = maybe(3).

maybe(Arg) ->
    do([maybe_m
        || monad_plus:guard(maybe_m, is_number(Arg)),
           return(Arg*Arg)]).

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

test() ->
    test:test([{?MODULE, [test_sequence,
                          test_join,
                          test_maybe,
                          test_fib,
                          test_list,
                          test_omega]}],
              [report, {name, ?MODULE}]).
