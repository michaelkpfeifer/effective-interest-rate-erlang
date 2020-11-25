%% @doc The payment_stream module provides utilities for dealing with
%% streams of payments.
%%
%% == Payment Streams ==
%%
%% A <i>payment</i> is a tuple `{amount, date}' consisting of
%% an amount (in whatever currency) and a date. The amount can be
%% positive or negative.
%%
%% For example, `{-2000, {2015, 1, 1}}' represents an amount of -200
%% transferred at Jan 01, 2015.
%%
%% A <i>payment stream</i> is a list of payments
%%
%% == Relative Payment Streams ==
%%
%%  Let `[{a_1, t_1}, ..., {a_n, t_n}]' be a payment stream and let
%%  `{a_f, t_f}' be the earliest payment in this stream. A <i>relative
%%  payment stream</i> is a list `[{a_1, r_1}, ..., {a_n, r_n}]' where
%%  `r_k' is the difference of `t_k' and `t_f' "expressed in years".
%%
%%  More precisely, `r_k' is computed as follows: Let `t_f' be the
%%  `d_f'th day in a year `y_f' and let `t_k' be the `d_k'th day in year
%%  `y_k'. (Days are indexed starting at `0'. Jan 01 is day `0'.)  Let
%%  `D(y)' denote the number of days in a year `y'. For a leap year `y',
%%  `D(y)' is 366. Otherwise, `D(y)' is 365. Then
%%
%% ```
%% r_k = (y_k - y_f) + (d_k / D(y_k) - d_f / D(y_f)).
%% '''
%%
%% == The Net Present Value Function ==
%%
%% A relative payment stream `[{a_1, r_1}, ..., {a_n, r_n}]' gives rise
%% to the definition of the net present value function
%%
%% ```
%% npv(x) = a_1 * (1 + x)^(-r_1) + ... + a_n * (1 + x)^(-r_n)
%% '''
%%
%% of single real variable `x'. The internal interest rate of the
%% original payment stream is the root of the `npv' function.
%%
%% In general, there is no closed formula for the computation of the
%% roots of `npv'. However, given a "reasonable" start value, Newton's
%% method converges very fast to the wanted root.
%%
%% Newton's method requires the computation of the derivative `` npv' ''
%% of `npv'.  Fortunately, `` npv' '' can be easily written in a
%% closed form:
%%
%% ```
%% npv' = a_1 * (-r_1) * (1 + x)^(-r_1 - 1) + ... + a_n * (-r_n) * (1 + x)^(-r_n - 1)
%% '''

-module(payment_stream).
-export([earliest_payment/1, to_relative_payment_stream/1, net_present_value/1, net_present_value_derivative/1]).

%% @doc Finds the earliest payment in a payment stream
%%
%% == Examples ==
%%
%% ```
%% 1> payment_stream:earliest_payment([{-1000, {2021, 1, 1}}, {1000, {2020, 1, 1}}]).
%% {1000,{2020,1,1}}
%% '''

earliest_payment(PaymentStream) ->
    lists:nth(1, lists:sort(fun({_AmountA, DateA}, {_AmountB, DateB}) -> DateA < DateB end, PaymentStream)).

%% @doc Converts a payment stream to a relative payment stream
%%
%% == Examples ==
%% ```
%% 1> payment_stream:to_relative_payment_stream([{1000, {2020, 1, 1}}, {1000, {2021, 1, 1}}]).
%% [{1000,0.0},{1000,1.0}]
%% '''

to_relative_payment_stream(PaymentStream) ->
    P_f = earliest_payment(PaymentStream),
    lists:map(fun(P) -> to_relative_payment(P_f, P) end, PaymentStream).


to_relative_payment({_A_f, T_f}, {A_k, T_k}) ->
    {Y_f, _, _} = T_f,
    {Y_k, _, _} = T_k,
    {A_k, Y_k - Y_f + relative_day_in_year(T_k) - relative_day_in_year(T_f)}.


relative_day_in_year(T)  ->
    {Y, _, _ } = T,
    D = day_in_year(T),

    case calendar:is_leap_year(Y) of
        true ->
            D / 366;
        false ->
            D / 365
    end.


day_in_year(T) ->
    {Y, _, _ } = T,
    calendar:date_to_gregorian_days(T) - calendar:date_to_gregorian_days({Y, 1, 1}).

%% @doc Computes the net present value function `npv' of a relative
%% payment stream
%%
%% == Examples ==
%%
%% Let `[{1000, {2021, 1, 1}}, {-1000, {2022, 1, 1}}]' be a very
%% simple payment stream. Since the amount payed on Jan 01, 2021 is
%% the negative of the amount received one year later on Jan 01, 2022,
%% the internal interest rate for this payment stream should be `0'.
%%
%% The relative payment stream corresponding to the payment stream
%% above is `[{1000, 0.0}, {-1000, 1.0}]' and then the corresponding
%% `npv' function is
%% ```
%% npv(x) = 1000 * (1 + x)^0.0 + (-1000) * (1 + x)^(-1.0)
%% '''
%% so that
%% ```
%% npv(0) = 1000 * (1 + 0)^0.0 + (-1000) * (1 + 0)^(-1.0)
%%        = 1000 * 1 - 1000 * 1
%%        = 0
%% '''
%% ```
%% 1> PaymentStream = [{1000, {2021, 1, 1}}, {-1000, {2022, 1, 1}}].
%% [{1000,{2021,1,1}},{-1000,{2022,1,1}}]
%% 2> RelativePaymentStream = payment_stream:to_relative_payment_stream(PaymentStream).
%% [{1000,0.0},{-1000,1.0}]
%% 3> Npv = payment_stream:net_present_value(RelativePaymentStream).
%% #Fun<payment_stream.2.28137773>
%% 4> Npv(0).
%% 0.0
%% '''

net_present_value(RelativePaymentStream) ->
    fun(X) ->
            lists:foldl(
              fun({A, R}, Sum) -> Sum + A * math:pow(1 + X, -R) end,
              0,
              RelativePaymentStream
             )
    end.

%% @doc Computes the derivative `` npv' '' of the net present value
%% function `npv' of a relative payment stream
%%
%% == Examples ==
%%
%% Let `[{1000, 0.0}, {-1000, 1.0}]' be a very simple relative payment
%% stream with a corresponding net present value function
%% ```
%% npv(x) = 1000 * (1 + x)^0.0 + (-1000) * (1 + x)^(-1.0)
%% '''
%% Then the derivative of `npv' is
%% ```
%% npv'(x) = 0.0 * 1000 * (1 + x)^(-1.0) + (-1.0) * (-1000) * (1 + x)^-(2.0)
%%         = 1000 * (1 + x)^(-2.0)
%% '''
%% ```
%% npv'(x) = 0.0 * 1000 * (1 + x)^(-1.0) + (-1.0) * (-1000) * (1 + x)^-(2.0)
%%         = 1000 * (1 + x)^(-2.0)
%% '''
%% ```
%% 1> RelativePaymentStream = [{1000, 0.0}, {-1000, 1.0}].
%% [{1000,0.0},{-1000,1.0}]
%% 2> Npvp = payment_stream:net_present_value_derivative(RelativePaymentStream).
%% #Fun<payment_stream.3.28137773>
%% 3> Npvp(0).
%% 1.0e3
%% '''

net_present_value_derivative(RelativePaymentStream) ->
    fun(X) ->
            lists:foldl(
              fun({A, R}, Sum) -> Sum + A * (-R) * math:pow(1 + X, -R - 1) end,
              0,
              RelativePaymentStream
             )
    end.
