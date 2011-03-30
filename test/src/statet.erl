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

-module(statet, [InnerMonad]).
-compile({parse_transform, erlando}).

-behaviour(monad).
-export(['>>='/2, '>>'/2, return/1, fail/1]).
-export([get/0, put/1, evalStateT/2, execStateT/2, runStateT/2]).

'>>='(X, Fun) -> fun (S) -> do([InnerMonad || {A, S1} <- X(S),
                                              (Fun(A))(S1)]) end.

'>>'(X, Fun)  -> fun (S) -> do([InnerMonad || {_A, S1} <- X(S),
                                              (Fun())(S1)]) end.

return(A)     -> fun (S) -> InnerMonad:return({A, S}) end.
fail(Str)     -> fun (_) -> InnerMonad:fail(Str) end.

get()         -> fun (S) -> InnerMonad:return({S, S}) end.

put(S)        -> fun (_) -> InnerMonad:return({ok, S}) end.

evalStateT(Monad, S) -> do([InnerMonad || {A, _S1} <- Monad(S),
                                          return(A)]).

execStateT(Monad, S) -> do([InnerMonad || {_A, S1} <- Monad(S),
                                          return(S1)]).

runStateT(Monad, S) -> do([InnerMonad || Monad(S)]).
