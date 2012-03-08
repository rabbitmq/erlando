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

%% List Monad. Mainly just for fun! As normal, this is depth first.

-module(list_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-behaviour(monad_plus).
-export([mzero/0, mplus/2]).

-ifdef(use_specs).
-type(monad(A) :: [A]).
-include("monad_specs.hrl").
-include("monad_plus_specs.hrl").
-endif.

%% Note that using a list comprehension is (obviously) cheating, but
%% it's easier to read. The "real" implementation is also included for
%% completeness.

'>>='(X, Fun) -> lists:append([Fun(E) || E <- X]).
%%               lists:foldr(fun (E, Acc) -> Fun(E) ++ Acc end, [], X).

return(X) -> [X].
fail(_X)  -> [].

mzero() -> [].
mplus(X, Y) -> lists:append(X, Y).
