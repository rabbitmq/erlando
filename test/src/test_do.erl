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
-compile({parse_transform, erlando}).

-compile(export_all).

test_maybe(Arg) ->
    do([maybe
        || X <- return(Arg),
           case is_atom(Arg) of
               true  -> fail(argh);
               false -> return(io:format("~p~n", [X]))
           end,
           return(ok)]).

test_state_t(Arg) ->
    StateT = state_t:new(maybe), %% state_t wrapping of maybe monad
    StateT:exec(
      do([StateT
          || S <- return(1),
             S <- return(2),
             foo(StateT),
             StateT:put(some_new_state),
             StateT:modify(fun (some_new_state) -> pi end),
             return(io:format("~p~n",[S])),
             return(wibble)
         ]), Arg).

foo(StateT) ->
    do([StateT || S <- StateT:get(),
                  return(io:format("~p~n", [S])),
                  StateT:put(S),
                  return(wibble)]).

test_state_t_identity() ->
    StateT = state_t:new(identity),
    StateT:run(
      do([StateT
          || X <- foo(StateT),
             return(X)]), argh).

test_list() ->
    A = [{X,Y} || X <- "abcd",
                  Y <- [1,2]],
    A = do([list || X <- "abcd",
                    Y <- [1,2],
                    return({X,Y})]),
    %% Classic pythagorean triples
    P = [{X, Y, Z} || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)],
    P = do([list || Z <- lists:seq(1,20),
                    X <- lists:seq(1,Z),
                    Y <- lists:seq(X,Z), %% TODO: guard shouldn't be in list
                    list:guard(math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)),
                    return({X,Y,Z})]).

test_omega() ->
    A = [{X,Y,Z} || X <- "abcd",
                    Y <- lists:seq(1,5),
                    Z <- lists:seq(11,15)],
    B = do([omega || X <- "abcd",
                     Y <- lists:seq(1,5),
                     Z <- lists:seq(11,15),
                     return({X,Y,Z})]),
    true = A =/= B,
    true = A =:= lists:usort(B).
