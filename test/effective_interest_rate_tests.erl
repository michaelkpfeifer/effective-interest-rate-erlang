-module(effective_interest_rate_tests).
-include_lib("eunit/include/eunit.hrl").

very_simple_effective_interest_rate_test() ->
    PaymentsWithDate = [{2000, {2013, 6, 1}},
                        {-1000, {2014, 6, 1}},
                        {-1000, {2015, 6, 1}}],
    EffectiveInterestRate = effective_interest_rate:effective_interest_rate(PaymentsWithDate),
    ?assert(abs(EffectiveInterestRate) < 10.0e-6).

simple_effective_interest_rate_test() ->
    PaymentsWithDate = [{2000, {2013, 6, 1}},
                        {-1000, {2014, 6, 1}},
                        {-1000, {2015, 6, 1}},
                        {-100, {2015, 7, 1}}],
    EffectiveInterestRate = effective_interest_rate:effective_interest_rate(PaymentsWithDate),
    ?assert(EffectiveInterestRate > 0).

effective_interest_rate_for_monthly_payment_test() ->
    PaymentsWithDate = [{240000, {2015, 1, 1}}] ++
        [{-1200, {Year, Month, 1}} || Year <- lists:seq(2015, 2034),
                                      Month <- lists:seq(1, 12)],
    EffectiveInterestRate = effective_interest_rate:effective_interest_rate(PaymentsWithDate),
    ?assert(abs(EffectiveInterestRate - 1.91 / 100) < 10.0e-4).

effective_interest_rate_for_simple_real_life_case_test() ->
    PaymentsWithDate = [{-1065.25, {2011, 4, 21}},
			{130.69, {2014, 5, 23}}],
    EffectiveInterestRate = effective_interest_rate:effective_interest_rate(PaymentsWithDate),
    ?assert(abs(EffectiveInterestRate - (- 0.4931)) < 0.0001).

trivial_convert_to_payments_with_offset_test() ->
    PaymentsWithDate = [{2000, {2015, 6, 1}}],
    PaymentsWithOffset = effective_interest_rate:convert_to_payments_with_offset(PaymentsWithDate),
    [{Payment, Offset} | _] = PaymentsWithOffset,
    ?assertEqual(2000, Payment),
    ?assertEqual(0.0, Offset).

simple_convert_to_payments_with_offset_test() ->
    PaymentsWithDate = [{2000, {2013, 6, 1}},
                        {-1000, {2014, 6, 1}},
                        {-1000, {2015, 6, 1}},
                        {-100, {2015, 7, 1}}],
    PaymentsWithOffset = effective_interest_rate:convert_to_payments_with_offset(PaymentsWithDate),
    Payments = lists:map(fun(X) -> element(1, X) end, PaymentsWithOffset),
    [Offset1, Offset2, Offset3, Offset4] = lists:map(fun(X) -> element(2, X) end, PaymentsWithOffset),

    ?assertEqual([2000, -1000, -1000, -100], Payments),
    ?assertEqual(0.0, Offset1),
    ?assertEqual(1.0, Offset2),
    ?assertEqual(2.0, Offset3),
    ?assert(Offset4 > 2.05),
    ?assert(Offset4 < 2.1).


trivial_first_payment_with_date_test() ->
    PaymentsWithDate = [{2000, {2015, 6, 1}}],
    FirstPaymentWithDate = effective_interest_rate:first_payment_with_date(PaymentsWithDate),
    ?assertEqual({2000, {2015, 6, 1}}, FirstPaymentWithDate).

simple_first_payment_with_date_test() ->
    PaymentsWithDate = [{-1000, {2015, 6, 1}},
                        {-1000, {2014, 6, 1}},
                        {2000, {2013, 6, 1}},
                        {-100, {2015, 7, 1}}],
    FirstPaymentWithDate = effective_interest_rate:first_payment_with_date(PaymentsWithDate),
    ?assertEqual({2000, {2013, 6, 1}}, FirstPaymentWithDate).


payment_offset_test() ->
    PaymentWithDate1 = {1000, {2015, 1, 1}},
    PaymentOffset1 = effective_interest_rate:payment_offset(PaymentWithDate1, 2015, 0),
    ?assertEqual(0.0, PaymentOffset1),
    PaymentWithDate2 = {1000, {2016, 1, 1}},
    PaymentOffset2 = effective_interest_rate:payment_offset(PaymentWithDate2, 2015, 0),
    ?assertEqual(1.0, PaymentOffset2),
    PaymentWithDate3 = {1000, {2016, 7, 2}},
    PaymentOffset3 = effective_interest_rate:payment_offset(PaymentWithDate3, 2016, 0),
    ?assertEqual(0.5, PaymentOffset3).


derive_payment_with_offset_test() ->
    ?assertEqual({0.0, 1.0}, effective_interest_rate:derive_payment_with_offset({1000.0, 0.0})),
    ?assertEqual({-1000.0, 2.0}, effective_interest_rate:derive_payment_with_offset({1000.0, 1.0})),
    ?assertEqual({-2000.0, 3.0}, effective_interest_rate:derive_payment_with_offset({1000.0, 2.0})),
    ?assertEqual({-500.0, 1.5}, effective_interest_rate:derive_payment_with_offset({1000.0, 0.5})).


evaluate_test() ->
    PaymentsWithDate = [{2000, {2013, 6, 1}},
                        {-1000, {2014, 6, 1}},
                        {-1000, {2015, 6, 1}}],
    PaymentsWithOffset = effective_interest_rate:convert_to_payments_with_offset(PaymentsWithDate),
    Value0 = effective_interest_rate:evaluate(PaymentsWithOffset, 0.0),
    ?assertEqual(0.0, Value0),
    Value1 = effective_interest_rate:evaluate(PaymentsWithOffset, 1.0),
    ?assertEqual(1250.0, Value1).
