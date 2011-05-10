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

-module(state_t, [InnerMonad]).
-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, '>>'/2, return/1, fail/1]).
-export([get/0, put/1, eval/2, exec/2, run/2, modify/1, modify_and_return/1]).

'>>='(X, Fun) -> fun (S) -> do([InnerMonad || {A, S1} <- X(S),
                                              (Fun(A))(S1)]) end.

'>>'(X, Fun)  -> fun (S) -> do([InnerMonad || {_A, S1} <- X(S),
                                              (Fun())(S1)]) end.

return(A)     -> fun (S) -> InnerMonad:return({A, S}) end.
fail(Str)     -> fun (_) -> InnerMonad:fail(Str) end.

get()         -> fun (S) -> InnerMonad:return({S, S}) end.

put(S)        -> fun (_) -> InnerMonad:return({ok, S}) end.

eval(Monad, S) -> do([InnerMonad || {A, _S1} <- Monad(S),
                                    return(A)]).

exec(Monad, S) -> do([InnerMonad || {_A, S1} <- Monad(S),
                                    return(S1)]).

run(Monad, S)  -> do([InnerMonad || Monad(S)]).

modify(Fun) -> fun (S) -> InnerMonad:return({ok, Fun(S)}) end.

modify_and_return(Fun) -> fun (S) -> InnerMonad:return(Fun(S)) end.
