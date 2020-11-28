%% @doc The newton_iteration module implements the computation of
%% roots of real functions using Newton's method.

-module(newton_iteration).
-export([iterate/5]).

%% @doc Runs Newton's iteration
%%
%% `F' is an anonymous function of one real variable and `Fp' is the
%% derivative of `F'.
%%
%% `StartValue' is the first value used in the iteration process.
%% Usually, this is a guess of where the wanted root of `F' may be
%% located.
%%
%% `MaxIterationDifference' and `MaxIterations' are used to terminate
%% the iteration process.
%%
%% If the absolute value of the difference between two consecutive
%% values produced by the iteration process is smaller than
%% `MaxIterationDifference', then `iterate/5' returns `{:ok, x_k}'
%% where `x_k' is the latest value produced by the iteration.
%%
%% If the absolute value of the difference between two consecutive
%% values after `MaxIterations' is still larger than
%% `MaxIterationDifference', then `iterate/5' returns `error'.
%%
%% == Examples ==
%%
%% The root of the identity function `fun(X) -> X end' is `0'.
%%
%% ```
%% 1> newton_iteration:iterate(
%% 1> fun(X) -> X end,
%% 1> fun(_X) -> 1 end,
%% 1> 1.0,
%% 1> 1.0e-9,
%% 1> 4
%% 1> ).
%% {ok,0.0}
%% '''
%%
%% The roots of the quadratic function `fun(X) -> X * X - 4 end' are
%% `2' and `-2' but `4' iterations are not sufficient to compute a
%% root with the required accuracy.
%%
%% ```
%% 1> F = fun(X) -> X * X - 4 end.
%% #Fun<erl_eval.44.97283095>
%% 2> Fp = fun(X) -> 2 * X end.
%% #Fun<erl_eval.44.97283095>
%% 3> newton_iteration:iterate(F, Fp, 4.0, 1.0e-9, 4).
%% error
%% 4> newton_iteration:iterate(F, Fp, 4.0, 1.0e-9, 8).
%% {ok,2.0}
%% '''

iterate(F, Fp, StartValue, MaxIterationDifference, MaxIterations) ->
    iterate(F, Fp, StartValue, MaxIterationDifference, MaxIterations, 0).

iterate(_, _, _, _, MaxIterations, IterationCount) when IterationCount > MaxIterations ->
    error;

iterate(F, Fp, PreviousIteration, MaxIterationDifference, MaxIterations, IterationCount) ->
    Iteration = PreviousIteration - F(PreviousIteration) / Fp(PreviousIteration),

    if abs(Iteration - PreviousIteration) =< MaxIterationDifference ->
            {ok, Iteration};
       true ->
            iterate(F, Fp, Iteration, MaxIterationDifference, MaxIterations, IterationCount + 1)
    end.
