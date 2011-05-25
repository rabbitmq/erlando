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

-module(error_t, [InnerMonad]).
-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1, run/1]).

-ifdef(use_specs).
-type(monad(A) :: {'ok', A} | {'error', any()}).
-include("monad_specs.hrl").
-endif.

'>>='(X, Fun) -> fun () ->
                         case X() of
                             {error, _Err} = Error -> Error;
                             {ok,  Result}         -> (Fun(Result))();
                             ok                    -> (Fun(ok))()
                         end
                 end.

return(X) -> fun () -> InnerMonad:return({ok, X}) end.

%% This is the equivalent of
%%     fail msg = ErrorT $ return (Left (strMsg msg))
%% from the instance (Monad m, Error e) => Monad (ErrorT e m)
%%
%% http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Error.html#ErrorT
%%
%% I.e. note that calling fail on the outer monad is not a failure of
%% the inner monad: it is success of the inner monad, but the failure
%% is encapsulated.
fail(X)   -> fun () -> InnerMonad:return({error, X}) end.

run(Fun) -> Fun().
