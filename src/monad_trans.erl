%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.

-module(monad_trans).
-compile({parse_transform, do}).

%% Monad primitives
-callback '>>='(monad:monadic(TM, A), fun( (A) -> monad:monadic(TM, B) ), M) -> monad:monadic(TM, B) when TM :: monad:monad(), M :: monad:monad().
-callback return(A, M) -> monad:monadic(TM, A) when TM :: monad:monad(), M :: monad:monad().
-callback fail(any(), M) -> monad:monadic(TM, _A) when TM :: monad:monad(), M :: monad:monad().

%% Lift a computation form the argument monad to the constructed
%% monad.
-callback lift(monad:monadic(M, A), M) -> monad:monadic(TM, A) when TM :: monad:monad(), M :: monad:monad().
