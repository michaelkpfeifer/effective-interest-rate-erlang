all:
	erl -make

tests: clean
	erl -make
	erl -noshell -pa ebin -eval "eunit:test([effective_interest_rate, newton_iteration, payment_stream], [verbose])" -s init stop

docs:
	erl -noshell -eval "edoc:run([\"src/effective_interest_rate.erl\", \"src/newton_iteration.erl\", \"src/payment_stream.erl\"], [{dir, \"./doc\"}])" -s init stop

clean:
	rm -f ebin/*.beam
