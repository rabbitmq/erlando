# Erlando



## Introduction

Erlando is a set of syntax extensions for Erlang. Currently it
consists of two syntax extensions, both of which take the form of
[parse transformers](http://www.erlang.org/doc/man/erl_id_trans.html).

* **Cut**: This adds support for cuts to Erlang. These are
  inspired by the
  [Scheme form of cuts](http://srfi.schemers.org/srfi-26/srfi-26.html). Cuts
  can be thought of as a light-weight form of abstraction, with
  similarities to partial application (or currying).

* **Do**: This adds support for do-syntax and monads to
  Erlang. These are heavily inspired by [Haskell](http://haskell.org),
  and the monads and libraries are near-mechanical translations from
  the Haskell GHC libraries.



## Use

To use any of these parse transformers, you must add the necessary
`-compile` attributes to your Erlang source files. For example:

    -module(test).
    -compile({parse_transform, cut}).
    -compile({parse_transform, do}).
    ...

Then, when compiling `test.erl`, you must ensure `erlc` can locate
`cut.beam` by passing the suitable path to `erlc` with a `-pa` or
`-pz` argument. For example:

    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/cut.erl
    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/do.erl
    erlc -Wall +debug_info -I ./include -pa test/ebin -pa ./ebin -o test/ebin test/src/test.erl

Note, if you're using QLC, you may find you need to be careful as to
the order of the parse transforms: I've found that the
`-compile({parse_transform, cut}).` must occur before the
`-include_lib("stdlib/include/qlc.hrl").`



## Cut

### Motivation

Cut is motivated by the frequency with which simple abstractions (in a
lambda-calculus sense) are used in Erlang, and the relatively noisy
nature of declaring `fun`s. For example, it's quite common to see code
like:

    with_resource(Resource, Fun) ->
        case lookup_resource(Resource) of
            {ok, R}          -> Fun(R);
            {error, _} = Err -> Err
        end.

    my_fun(A, B, C) ->
        with_resource(A, fun (Resource) ->
                             my_resource_modification(Resource, B, C)
                         end).

I.e. a fun is very simply created in order to perform variable capture
from the its surrounding scope but to leave holes for further
arguments to be provided. Using a cut, the function `my_fun` can be
rewritten as:

    my_fun(A, B, C) ->
        with_resource(A, my_resource_modification(_, B, C)).


### Definition

Normally, the variable `_` can only occur in patterns: i.e. where
match occurs. This can be in assignment, in cases, and in function
heads. For example:

    {_, bar} = {foo, bar}.

Cut uses `_` in expressions to indicate where abstraction should
occur. Abstraction from cuts is **always** performed on the
*shallowest* enclosing expression. For example:

    list_to_binary([1, 2, math:pow(2, _)]).

will create the expression

    list_to_binary([1, 2, fun (X) -> math:pow(2, X) end]).

and not

    fun (X) -> list_to_binary([1, 2, math:pow(2, X)]) end.

It is fine to use multiple cuts in the same expression, and the
arguments to the created abstraction will match the order in which the
`_` var is found in the expression. For example:

    assert_sum_3(X, Y, Z, Sum) when X + Y + Z == Sum -> ok;
    assert_sum_3(_X, _Y, _Z, _Sum) -> {error, not_sum}.
    
    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        ok = Equals12(9, 2, 1).

It is perfectly legal to take cuts of cuts as the abstraction created
by the cut is a normal `fun` expression and thus can be re-cut as
necessary:

    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        Equals5 = Equals12(_, _, 7),
        ok = Equals5(2, 3).

Note that because a simple `fun` is being constructed by the cut, the
arguments are evaluated prior to the cut function. For example:

    f1(_, _) -> io:format("in f1~n").

    test() ->
        F = f1(io:format("test line 1~n"), _),
        F(io:format("test line 2~n")).

will print out

    test line 2
    test line 1
    in f1

This is because the cut creates `fun (X) -> f1(io:format("test line
1~n"), X) end`. Thus it is clear that `X` must be evaluated first,
before the `fun` can be invoked.

Of course, no one would be crazy enough to have side-effects in
function argument expressions, so this will never cause any issues!

Cuts are not limited to function calls. They can be used in any
expression where they make sense:


#### Tuples

    F = {_, 3},
    {a, 3} = F(a).


#### Lists

    dbl_cons(List) -> [_, _ | List].
    
    test() ->
        F = dbl_cons([33]),
        [7, 8, 33] = F(7, 8).

Note that if you nest a list as a list tail in Erlang, it's still
treated as one expression. For example:

    A = [a, b | [c, d | [e]]]

is exactly the same (right from the Erlang parser onwards) as:

    A = [a, b, c, d, e]

I.e. those sub-lists, when they're in the tail position **do not**
form sub-expressions. Thus:

    F = [1, _, _, [_], 5 | [6, [_] | [_]]],
    %% This is the same as:
    %%  [1, _, _, [_], 5, 6, [_], _]
    [1, 2, 3, G, 5, 6, H, 8] = F(2, 3, 8),
    [4] = G(4),
    [7] = H(7).

However, be very clear about the difference between `,` and `|`: the
tail of a list is **only** defined following a `|`. Following a `,`,
you're just defining another list element.

    F = [_, [_]],
    %% This is **not** the same as [_, _] or its synonym: [_ | [_]]
    [a, G] = F(a),
    [b] = G(b).


#### Records

    -record(vector, { x, y, z }).
    
    test() ->
        GetZ = _#vector.z,
        7 = GetZ(#vector { z = 7 }),
        SetX = _#vector{x = _},
        V = #vector{ x = 5, y = 4 } = SetX(#vector{ y = 4 }, 5).


#### Case

    F = case _ of
            N when is_integer(N) -> N + N;
            N                    -> N
        end,
    10 = F(5),
    ok = F(ok).


See
[test_cut.erl](http://hg.rabbitmq.com/erlando/file/default/test/src/test_cut.erl)
for more examples, including use of cuts in list comprehensions and
binary construction.

Note that cuts are not allowed where the result of the cut can only be
useful by interacting with the evaluation scope. For example:

    F = begin _, _, _ end.

This is not allowed, because the arguments to `F` would have to be
evaluated before the invocation of its body, which would then have no
effect, as they're already fully evaluated by that point.



## Do

The Do parse transformer permits Haskell-style *do-notation* in
Erlang, which makes using monads, and monad transformers possible and
easy. With *do-notation*, monads tend to look like a lot of line
noise.


### The Inevitable Monad Tutorial

#### The Mechanics of a Comma

What follows is a brief and mechanical introduction to monads. It
differs from a lot of the Haskell monad tutorials, because they tend
to view monads as a means of achieving sequencing of operations in
Haskell, which is a lazy language. Erlang is not a lazy language, but
the powerful abstractions possible from using monads are still very
worthwhile. Whilst this is a very mechanical tutorial, it should be
possible to see the more advanced abstractions possible.

Let's say we have the three lines of code:

    A = foo(),
    B = bar(A, dog),
    ok.

They are three, simple statements, which are evaluated
consecutively. What a monad gives you is control over what happens
between the statements: in Erlang, it is a programmatic comma.

If you wanted to implement a programmatic comma, how would you do it?
You might start with something like:

    A = foo(),
    comma(),
    B = bar(A, dog),
    comma(),
    ok.

But that's not quite powerful enough, because unless `comma/0` throws
some sort of exception, it can't actually stop the subsequent
expression from being evaluated. Most of the time we'd probably like
the `comma/0` function to be able to act on some variables which are
currently in scope, and that's not possible here either. So we should
extend the function `comma/0` so that it takes the result of the
preceding expression, and can choose whether or not the subsequent
expressions should be evaluated:

    comma(foo(),
          fun (A) -> comma(bar(A, dog),
                           fun (B) -> ok end)).

Thus the function `comma/2` takes all results from the previous
expression, and controls how and whether they are passed to the next
expression.

As defined, the `comma/2` function is the monadic function `>>=/2`.

Now it's pretty difficult to read the program with the `comma/2`
function (especially as Erlang annoyingly doesn't allow us to define
new infix functions), which is why some special syntax is
necessary. Haskell has it's *do-notation*, and so we've borrowed from
that and abused Erlang's list comprehensions. Haskell also has lovely
type-classes, which we've sort of faked specifically for monads. So,
with the Do parse transformer, you can write in Erlang:

    do([Monad ||
        A <- foo(),
        B <- bar(A, dog),
        ok]).

which is readable and straightforward, but is transformed into:

    Monad:'>>='(foo(),
                fun (A) -> Monad:'>>='(bar(A, dog),
                                       fun (B) -> ok end)).

There is no intention that this latter form is any more readable than
the `comma/2` form - it is not. However, it should be clear that the
function `Monad:'>>='/2` now has complete control over what happens:
does the fun on the right hand side ever get invoked? If so, with what
value?


#### Lots of different types of Monads

So now that we have some relatively nice syntax for using monads, what
can we do with them? Also, in the code

    do([Monad ||
        A <- foo(),
        B <- bar(A, dog),
        ok]).

what are the possible values of `Monad`?

The answer to the first question is *almost anything*; and to the
later question, is *any module name that implements the monad
behaviour*.

Above, we covered one of the four monadic operators, `>>=/2`. The
others are:

* `>>/2`: This is exactly the same as `>>=/2`, except there is no
  variable to bind. E.g.

        do([Monad ||
            foo(),
            A <- bar(),
            ok]).

  is transformed into

        Monad:'>>'(foo(),
                   fun () -> Monad:'>>='(bar(),
                                         fun (A) -> ok end)).

* `return/1`: This *lifts* a value into the monad. We'll see examples
  of this shortly.

* `fail/1`: This takes a string, describing the error encountered, and
  informs whichever monad currently in use that some sort of error has
  occured.

Note that within *do-notation*, any function call to functions named
`return` or `fail`, are automatically rewritten to invoke `return` or
`fail` within the current monad.

The simplest monad possible is the Identity-monad:

    -module(identity_m).
    -behaviour(monad).
    -export(['>>='/2, '>>'/2, return/1, fail/1]).

    '>>='(X, Fun) -> Fun(X).
    '>>'(_X, Fun) -> Fun().
    return(X)     -> X.
    fail(X)       -> throw({error, X}).

This makes our programmatic comma behave just like Erlang's comma
normally does. The two sequencing combinators (`>>/2` and `>>=/2`) do
not inspect the values passed to them, and always invoke the
subsequent expression fun.

What could we do if we did inspect the values passed to the sequencing
combinators? One possibility results in the Maybe-monad:

    -module(maybe_m).
    -behaviour(monad).
    -export(['>>='/2, '>>'/2, return/1, fail/1]).
    
    '>>='({just, X}, Fun) -> Fun(X);
    '>>='(nothing,  _Fun) -> nothing.
    
    '>>'({just, _X}, Fun) -> Fun();
    '>>'(nothing,   _Fun) -> nothing.
    
    return(X) -> {just, X}.
    fail(_X)  -> nothing.

Thus if the result of the preceding expression is `nothing`, then the
subsequent expressions are not evaluated. This means that we can write
very neat looking code which immediately stops should any failure be
encountered.

    if_safe_div_zero(X, Y, Fun) ->
        do([maybe_m ||
            Result <- case Y == 0 of
                          true  -> fail("Cannot divide by zero");
                          false -> return(X / Y)
                      end,
            return(Fun(Result))]).

If `Y` is equal to 0, then `Fun` will not be invoked, and the result
of the `if_safe_div_zero` function call will be `nothing`. If `Y` is
not equal to 0, then the result of the `if_safe_div_zero` function
call will be `{just, Fun(X / Y)}`.

We see here that within the do-block, there is no mention of `nothing`
or `just`: they are abstracted away by the Maybe-monad. As a result,
it is possible to change the monad in use, without having to rewrite
any further code.

One common place to use a monad like the Maybe-monad is where you'd
otherwise have a lot of nested case statements in order to detect
errors. For example:

    write_file(Path, Data, Modes) ->
        Modes1 = [binary, write | (Modes -- [binary, write])],
        case make_binary(Data) of
            Bin when is_binary(Bin) ->
                case file:open(Path, Modes1) of
                    {ok, Hdl} ->
                        case file:write(Hdl, Bin) of
                            ok ->
                                case file:sync(Hdl) of
                                    ok ->
                                        file:close(Hdl);
                                    {error, _} = E ->
                                        file:close(Hdl),
                                        E
                                end;
                            {error, _} = E ->
                                file:close(Hdl),
                                E
                        end;
                    {error, _} = E -> E
                end;
            {error, _} = E -> E
        end.

    make_binary(Bin) when is_binary(Bin) ->
        Bin;
    make_binary(List) ->
        try
            iolist_to_binary(List)
        catch error:Reason ->
                {error, Reason}
        end.

can be transformed into the much shorter

    write_file(Path, Data, Modes) ->
        Modes1 = [binary, write | (Modes -- [binary, write])],
        do([error_m ||
            Bin <- make_binary(Data),
            {ok, Hdl} <- file:open(Path, Modes1),
            {ok, Result} <- return(do([error_m ||
                                       ok <- file:write(Hdl, Bin),
                                       file:sync(Hdl)])),
            file:close(Hdl),
            Result]).

Note that we have a nested do-block so that, as with the non-monadic
code, we ensure that once the file is opened, we always call
`file:close/1` even if an error occurs in a subsequent operation. This
is achieved by wrapping the nested do-block with a `return/1` call:
even if the inner do-block errors, the error is *lifted* to a
non-error value in the outer do-block, and thus execution continues to
the subsequent `file:close/1` call.

Here we are using an Error-monad which is remarkably similar to the
Maybe-monad, but matches the typical Erlang practice of indicating
errors by an `{error, Reason}` tuple:

    -module(error_m).
    -behaviour(monad).
    -export(['>>='/2, '>>'/2, return/1, fail/1]).
    
    '>>='({error, _Err} = Error, _Fun) -> Error;
    '>>='(Result,                 Fun) -> Fun(Result).
    
    '>>'({error, _Err} = Error, _Fun) -> Error;
    '>>'(_Result,                Fun) -> Fun().
    
    return(X) -> {ok,    X}.
    fail(X)   -> {error, X}.


#### Monad Transformers

Monads can be nested: not just by having do-blocks inside do-blocks,
but also by defining a monad as a transformation of another, inner
monad. The State Transform is a very commonly used monad transformer,
and is especially relevant for Erlang.




## License

(The MPL)

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Erlando.

The Initial Developer of the Original Code is VMware, Inc.
Copyright (c) 2011-2011 VMware, Inc.  All rights reserved.
