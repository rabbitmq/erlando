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

%% This is the Omega monad which is like the list monad, but does
%% breadth first, not depth first (though not strictly). This
%% implementation is based on Luke Palmer's Control.Monad.Omega module
%% for Haskell
%% (http://hackage.haskell.org/packages/archive/control-monad-omega/latest/doc/html/Control-Monad-Omega.html). As
%% the documentation there states:
%%
%%    Warning: Omega is only a monad when the results are interpreted
%%    as a set; that is, a valid transformation according to the monad
%%    laws may change the order of the results. However, the same set
%%    of results will always be reachable.
%%

-module(omega).

-behaviour(monad).
-export(['>>='/2, '>>'/2, return/1, fail/1]).
-export([diagonal/1]).

'>>='(X, Fun) -> diagonal([Fun(E) || E <- X]).
'>>'(X, Fun)  -> diagonal([Fun() || _E <- X]).

return(X) -> [X].
fail(_X)  -> [].

%% [[a, b, c, d],
%%  [e, f, g, h],
%%  [i, j, k, l],
%%  [m, n, o, p]].
%%
%% diagonal traverses diagonally from north-west corner, heading east
%% then south-west. I.e.
%% [a, b, e, c, f, i, d, g, j, m, h, k, n, l, o, p]
diagonal(LoL) -> lists:append(stripe(LoL)).

stripe([])           -> [];
stripe([[]|Xss])     -> stripe(Xss);
stripe([[X|Xs]|Xss]) -> [[X] | zip_cons(Xs, stripe(Xss))].

zip_cons([], Ys)         -> Ys;
zip_cons(Xs, [])         -> [[X] || X <- Xs];
zip_cons([X|Xs], [Y|Ys]) -> [[X|Y] | zip_cons(Xs, Ys)].
