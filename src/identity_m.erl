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

-module(identity_m).
-behaviour(monad).

-export_type([identity_m/1]).

-export(['>>='/2, return/1, fail/1]).


-type identity_m(A) :: A.


-spec '>>='(identity_m(A), fun( (A) -> identity_m(B) )) -> identity_m(B).
'>>='(X, Fun) -> Fun(X).


-spec return(A) -> identity_m(A).
return(X) -> X.


-spec fail(any()) -> identity_m(_A).
fail(E) ->
    throw({error, E}).
