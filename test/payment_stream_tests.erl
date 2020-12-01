-module(payment_stream_tests).
-include_lib("eunit/include/eunit.hrl").

erliest_payment_of_a_stream_of_one_payments_is_the_given_payment_test() ->
    Payment = {1000, {2013, 6, 1}},
    PaymentStream = [Payment],

    ?assertEqual(payment_stream:earliest_payment(PaymentStream), Payment).


earliest_payment_of_a_stream_of_payments_is_the_earliest_one_test() ->
    EarliestPayment = {-1000, {2020, 1, 1}},
    Payment = {500, {2020, 7, 1}},
    NewestPayment = {500, {2021, 1, 1}},
    PaymentStream = [NewestPayment, Payment, EarliestPayment],

    ?assertEqual(payment_stream:earliest_payment(PaymentStream), EarliestPayment).


relative_date_for_relative_payment_stream_with_one_payment_is_zero_test() ->
    PaymentStream = [{1000, {2020, 1, 1}}],

    [{Amount, RelativeDayInYear}] = payment_stream:to_relative_payment_stream(PaymentStream),

    ?assertEqual(Amount, 1000),
    ?assertEqual(RelativeDayInYear, 0.0).


relative_date_for_earliest_payment_in_relative_payment_stream_is_zero_test() ->
    EarliestPayment = {-1000, {2020, 1, 1}},
    Payment = {500, {2021, 1, 1}},
    NewestPayment = {500, {2022, 1, 1}},
    PaymentStream = [EarliestPayment, Payment, NewestPayment],

    ?assertEqual(payment_stream:to_relative_payment_stream(PaymentStream),
                 [{-1000, 0.0}, {500, 1.0}, {500, 2.0}]).


relative_date_respects_non_leap_year_in_relative_payment_stream_test() ->
    PaymentJan01 = {-1000, {2019, 1, 1}},
    PaymentJan02 = {500, {2019, 1, 2}},
    PaymentDec31 = {500, {2019, 12, 31}},
    PaymentStream = [PaymentJan01, PaymentJan02, PaymentDec31],

    [{_, _}, {_, T_1}, {_, T_2}] = payment_stream:to_relative_payment_stream(PaymentStream),

    ?assert(abs(T_1 - 1 / 365) < 1.0e-9),
    ?assert(abs(T_2 - 364 / 365) < 1.0e-9).


relative_date_respects_leap_year_in_relative_payment_stream_test() ->
    PaymentJan01 = {-1000, {2020, 1, 1}},
    PaymentJan02 = {500, {2020, 1, 2}},
    PaymentDec31 = {500, {2020, 12, 31}},
    PaymentStream = [PaymentJan01, PaymentJan02, PaymentDec31],

    [{_, _}, {_, T_1}, {_, T_2}] = payment_stream:to_relative_payment_stream(PaymentStream),

    ?assert(abs(T_1 - 1 / 366) < 1.0e-9),
    ?assert(abs(T_2 - 365 / 366) < 1.0e-9).


leap_years_are_respected_across_year_boundaries_in_relative_payment_stream_test() ->
    FirstPayment = {-1000, {2019, 12, 1}},
    SecondPayment = {1000, {2020, 1, 31}},
    PaymentStream = [FirstPayment, SecondPayment],

    [{_, _}, {_, T_1}] = payment_stream:to_relative_payment_stream(PaymentStream),

    ?assert(abs(T_1 - (1 - 334 / 365 + 30 / 366)) < 1.0e-9).


net_present_value_for_an_interest_rate_of_0_is_equal_to_the_sum_of_amounts_test() ->
    Payment1 = {-1000, {2019, 1, 1}},
    Payment2 = {1600, {2019, 4, 4}},
    Payment3 = {-2000, {2019, 7, 7}},
    Payment4 = {1600, {2019, 10, 10}},

    Npv = payment_stream:net_present_value(
            payment_stream:to_relative_payment_stream([Payment1, Payment2, Payment3, Payment4])
           ),
    Npv0 = Npv(0.0),

    ?assert(abs(Npv0 - 200.0) < 1.0e-9).


net_present_value_returns_the_expected_manually_computed_result_test() ->
    Payment1 = {-1000, {2019, 1, 1}},
    Payment2 = {500, {2020, 1, 1}},
    Payment3 = {500, {2021, 1, 1}},

    Npv = payment_stream:net_present_value(
            payment_stream:to_relative_payment_stream([Payment1, Payment2, Payment3])
           ),
    Npv1 = Npv(1.0),

    ?assert(abs(Npv1 + 625.0) < 1.0e-9).


net_present_value_derivative_returns_the_expected_manually_computed_result_test() ->
    Payment1 = {-1000, {2019, 1, 1}},
    Payment2 = {500, {2020, 1, 1}},
    Payment3 = {500, {2021, 1, 1}},

    Npvp = payment_stream:net_present_value_derivative(
            payment_stream:to_relative_payment_stream([Payment1, Payment2, Payment3])
           ),
    Npvp1 = Npvp(1.0),

    ?assert(abs(Npvp1 + 250.0) < 1.0e-9).
