-module(effective_interest_rate_tests).
-include_lib("eunit/include/eunit.hrl").

effective_interest_rate_is_0_in_a_very_simple_case_test() ->
    Payments = [{2000, {2013, 6, 1}}, {-1000, {2014, 6, 1}}, {-1000, {2015, 6, 1}}],

    {ok, EffectiveInterestRate} = effective_interest_rate:effective_interest_rate(Payments),

    ?assert(abs(EffectiveInterestRate) < 1.0e-6).


effective_interest_rate_has_the_expected_sign_in_a_simple_case_test() ->
    Payments = [{2000, {2013, 6, 1}}, {-1000, {2014, 6, 1}}, {-1000, {2015, 6, 1}}, {-100, {2015, 7, 1}}],

    {ok, EffectiveInterestRate} = effective_interest_rate:effective_interest_rate(Payments),

    ?assert(EffectiveInterestRate > 0).


effective_interest_rate_returns_the_expected_value_for_a_stream_of_monthly_payments_test() ->
    Payments = [{240000, {2015, 1, 1}}] ++
        [{-1200, {Year, Month, 1}} || Year <- lists:seq(2015, 2034),
                                      Month <- lists:seq(1, 12)],

    {ok, EffectiveInterestRate} = effective_interest_rate:effective_interest_rate(Payments),

    ?assert(abs(EffectiveInterestRate - 1.91 / 100) < 10.0e-4).


effective_interest_rate_returns_the_expected_value_for_simple_real_life_case_test() ->
    Payments = [{-1065.25, {2011, 4, 21}}, {130.69, {2014, 5, 23}}],

    {ok, EffectiveInterestRate} = effective_interest_rate:effective_interest_rate(Payments),

    ?assert(abs(EffectiveInterestRate - (- 0.4931)) < 0.0001).
