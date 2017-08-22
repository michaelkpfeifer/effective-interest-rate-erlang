# Introduction

The EffectiveInterestRate module provides a function for computing
effective interest rates of a series of payments in a very general
case.

# Installation

Copy src/effective_interest_rate.erl to a place where your application
can find it.

# Data Structures

A payment is a tuple `{amount, date}` consisting of the amount of the
payment and the date of the payment. For example,
`{-2000, {2015, 1, 1}}` represents a payment of -2000 at January 01,
2015. A series of payments is represented as a list of payments.

# Example


```
/src$ erl
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> c("effective_interest_rate.erl").
{ok,effective_interest_rate}
2> PaymentsWithDate = [{-2000, {2015, 1, 1}},
2> {1000, {2016, 1, 1}},
2> {1000, {2017, 1, 1}},
2> {200, {2017, 1, 1}}].
[{-2000,{2015,1,1}},
 {1000,{2016,1,1}},
 {1000,{2017,1,1}},
 {200,{2017,1,1}}]
3> effective_interest_rate:effective_interest_rate(PaymentsWithDate).
0.06394102980498531
4>

```
