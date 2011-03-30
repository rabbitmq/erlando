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

test_statet(Arg) ->
    StateT = statet:new(maybe), %% statet wrapping of maybe monad
    StateT:execStateT(
      do([StateT
          || S0 <- StateT:get(),
             return(io:format("~p~n",[S0])),
             StateT:put(some_new_state),
             return(wibble)
         ]), Arg).
