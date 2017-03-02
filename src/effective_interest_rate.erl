-module(effective_interest_rate).
-export([effective_interest_rate/1]).

-ifdef(TEST).
-export([convert_to_payments_with_offset/1]).
-export([derive_payment_with_offset/1]).
-export([evaluate/2]).
-export([first_payment_with_date/1]).
-export([payment_offset/3]).
-endif.

-define(MAX_ITERATIONS_COUNT, 64).
-define(MAX_DIFF_ITERATIONS, 10.0e-10).

effective_interest_rate(PaymentsWithDate) ->
    PaymentsWithOffset = convert_to_payments_with_offset(PaymentsWithDate),
    PaymentsWithOffsetDerivative = lists:map(fun(X) -> derive_payment_with_offset(X) end, PaymentsWithOffset),
    effective_interest_rate(PaymentsWithOffset, PaymentsWithOffsetDerivative, 0, -0.75).

effective_interest_rate(_, _, ?MAX_ITERATIONS_COUNT, Iteration) ->
    Iteration;
effective_interest_rate(PaymentsWithOffset,
                        PaymentsWithOffsetDerivative,
                        IterationsCount,
                        PreviousIteration) ->
    Nominator = evaluate(PaymentsWithOffset, PreviousIteration),
    Denominator = evaluate(PaymentsWithOffsetDerivative, PreviousIteration),
    NextIteration = PreviousIteration - (Nominator / Denominator),
    case abs(NextIteration - PreviousIteration) < ?MAX_DIFF_ITERATIONS of
        true ->
            NextIteration;
        false ->
            effective_interest_rate(PaymentsWithOffset,
                                    PaymentsWithOffsetDerivative,
                                    IterationsCount + 1,
                                    NextIteration)
    end.


evaluate(Terms, X) ->
    lists:foldl(fun(Term, Sum) ->
                        {Amount, Offset} = Term,
			Sum + Amount * math:pow((1 + X), -Offset)
                end,
                0,
                Terms).


convert_to_payments_with_offset(PaymentsWithDate) ->
    {_, FirstPaymentDate} = first_payment_with_date(PaymentsWithDate),
    FirstPaymentOffset = payment_offset_in_year(FirstPaymentDate),
    {FirstPaymentYear, _, _} = FirstPaymentDate,
    lists:map(fun(X) -> convert_to_payment_with_offset(X, FirstPaymentYear, FirstPaymentOffset) end, PaymentsWithDate).

convert_to_payment_with_offset(PaymentWithDate, FirstPaymentYear, FirstPaymentOffset) ->
    {Payment, _} = PaymentWithDate,
    Offset = payment_offset(PaymentWithDate, FirstPaymentYear, FirstPaymentOffset),
    {Payment, Offset}.


first_payment_with_date([PaymentWithDate | RemainingPaymentsWithDate]) ->
    first_payment_with_date(PaymentWithDate, RemainingPaymentsWithDate).

first_payment_with_date(PaymentWithDate, []) ->
    PaymentWithDate;
first_payment_with_date(PaymentWithDate, [NextPaymentWithDate | NextRemainingPaymentsWithDate]) ->
    {_, Date} = PaymentWithDate,
    {_, NextDate} = NextPaymentWithDate,
    case Date < NextDate of
        true ->
            first_payment_with_date(PaymentWithDate, NextRemainingPaymentsWithDate);
        false ->
            first_payment_with_date(NextPaymentWithDate, NextRemainingPaymentsWithDate)
    end.


payment_offset_in_year(PaymentDate) ->
    {Year, _, _} = PaymentDate,
    GregorianDaysJanFirst = calendar:date_to_gregorian_days({Year, 1, 1}),
    GregorianDaysNow = calendar:date_to_gregorian_days(PaymentDate),
    DaysInYear = GregorianDaysNow - GregorianDaysJanFirst,

    case calendar:is_leap_year(Year) of
        true ->
            DaysInYear / 366;
        false ->
            DaysInYear / 365
    end.


payment_offset(PaymentWithDate, FirstPaymentYear, FirstPaymentOffset) ->
    {_, PaymentDate} = PaymentWithDate,
    {Year, _, _} = PaymentDate,
    YearDifference = Year - FirstPaymentYear,
    PaymentOffsetInYear = payment_offset_in_year(PaymentDate),
    PaymentOffsetInYear + YearDifference - FirstPaymentOffset.


%% PaymentsWithOffset is a list
%%   [{A_1, t_1}, ... {A_n, t_n}]
%% of payments A_k with offsets t_k (in years). PaymentsWithOffset
%% defines a function
%%   S(X) = \sum_{k = 1}^n A_n * (1 + X)^{-t_k}.
%% The derivative of S is
%%   S'(X) = \sum_{k = 1}^n A_n * (-t_k) * (1 + X)^{-t_k - 1}.
%% The derivative has the same structure as the original function, so
%% we can store it in the same way. We simply represent the derivative
%% as
%%   [{-t_1 * A_1, -t_1 - 1}, ..., {-t_k * A_k, -t_k - 1}].

derive_payment_with_offset(PaymentWithOffset) ->
    {Amount, Offset} = PaymentWithOffset,
    {-Offset * Amount, -(-Offset - 1)}.
