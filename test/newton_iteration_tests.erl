-module(newton_iteration_tests).
-include_lib("eunit/include/eunit.hrl").

iterate_finds_the_root_of_the_identity_function_test() ->
    F = fun(X) -> X end,
    Fp = fun(_X) -> 1 end,

    {ok, Root} = newton_iteration:iterate(F, Fp, 1.0, 1.0e-9, 4),

    ?assert(abs(Root) < 1.0e-9).


iterate_does_not_find_root_of_polynomial_if_too_few_iterations_are_allowed_test() ->
    F = fun(X) -> math:pow(X, 4) - 1 end,
    Fp = fun(X) -> 4 * math:pow(X, 3) end,

    ?assertEqual(error, newton_iteration:iterate(F, Fp, 2.0, 1.0e-9, 4)).


iterate_finds_root_of_polynomial_if_sufficient_iterations_are_allowed_test() ->
    F = fun(X) -> math:pow(X, 4) - 1 end,
    Fp = fun(X) -> 4 * math:pow(X, 3) end,

    {ok, Root} = newton_iteration:iterate(F, Fp, 2.0, 1.0e-9, 8),

    ?assert(abs(Root - 1.0) < 1.0e-9).
