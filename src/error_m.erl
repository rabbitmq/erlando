%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(error_m).

-export_type([error_m/1]).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type error_m(A) :: ok | {ok, A} | {error, any()}.


-spec '>>='(error_m(A), fun( (A) -> error_m(B) )) -> error_m(B).
'>>='({error, _Err} = Error, _Fun) -> Error;
'>>='({ok, Result},           Fun) -> Fun(Result);
'>>='(ok,                     Fun) -> Fun(ok).


-spec return(A) -> error_m(A).
return(ok) -> ok;
return(X ) -> {ok, X}.


-spec fail(any()) -> error_m(_A).
fail(X) ->
    {error, X}.
