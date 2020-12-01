### Introduction

The effective_interest_rate module provides a function for computing
the effective interest rate of a stream of payments in a very general
case.

### Payment Streams

A *payment* is a tuple `{amount, date}` consisting of the amount of
the payment (in whatever currency) and the date of the payment.

For example, `{-2000, {2015, 1, 1}}` represents an amount of -2000 transferred at Jan 01, 2015.

A *payment stream* is a list of payments.

### Example

```
$ erl -pa ebin/
Erlang/OTP 23 [erts-11.0.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Eshell V11.0.2  (abort with ^G)
1> Payments = [{-2000, {2015, 1, 1}},
1> {1000, {2016, 1, 1}},
1> {1000, {2017, 1, 1}},
1>  {200, {2017, 1, 1}}].
[{-2000,{2015,1,1}},
 {1000,{2016,1,1}},
 {1000,{2017,1,1}},
 {200,{2017,1,1}}]
2> {ok, EffectiveInterestRate} = effective_interest_rate:effective_interest_rate(Payments).
{ok,0.06394102980498531}
```

### Documentation

Run `make docs`

### Tests

Run `make tests`
