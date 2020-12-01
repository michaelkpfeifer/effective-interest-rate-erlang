%% @doc The effective_interest_rate module implements the computation
%% of the effective interest rate of a stream of payments in a very
%% general case.
%%
%% Details about payment streams can be found in the documentation of
%% the payment_stream module.

-module(effective_interest_rate).
-export([effective_interest_rate/1]).

-define(START_VALUE, -0.75).
-define(MAX_ITERATION_DIFFERENCE, 1.0e-9).
-define(MAX_ITERATIONS, 64).

%% @doc Computes the effective interest rate of a payment stream
%%
%% == Examples ==
%%
%% ```
%% 1> Payments = [{2000, {2021, 6, 1}}, {-1000, {2022, 6, 1}}, {-1000, {2023, 6, 1}}].
%% [{2000,{2021,6,1}},{-1000,{2022,6,1}},{-1000,{2023,6,1}}]
%% 2> effective_interest_rate:effective_interest_rate(Payments).
%% {ok,5.2216352665974564e-17}
%% '''

effective_interest_rate(PaymentStream) ->
    RelativePaymentStream = payment_stream:to_relative_payment_stream(PaymentStream),

    newton_iteration:iterate(
      payment_stream:net_present_value(RelativePaymentStream),
      payment_stream:net_present_value_derivative(RelativePaymentStream),
      ?START_VALUE,
      ?MAX_ITERATION_DIFFERENCE,
      ?MAX_ITERATIONS
     ).
