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

-module(maybe_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-behaviour(monad_plus).
-export([mzero/0, mplus/2]).

-ifdef(use_specs).
-type(monad(A) :: {ok, A} | undefined).
-include("monad_specs.hrl").
-include("monad_plus_specs.hrl").
-endif.

'>>='({ok, X}, Fun) -> Fun(X);
'>>='(undefined,  _Fun) -> undefined.

return(X) -> {ok, X}.
fail(_X)  -> undefined.

mzero() -> undefined.
mplus(undefined, Y) -> Y;
mplus(X,      _Y) -> X.

-spec keyflatten/2 :: (pos_integer(), [tuple()]) -> [tuple()].
keyflatten(N, List) ->
	Filtered = lists:filter(fun(X) -> element(N, X) /= undefined end, List),
	lists:map(fun(X) -> 
		{L1, [{ok, V} | L2]} = lists:split(N - 1, tuple_to_list(X)),
		list_to_tuple([L1, [V | L2]])
	end).

