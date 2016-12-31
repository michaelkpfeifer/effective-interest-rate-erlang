all:
	erl -make

tests: clean
	erl -make -DTEST
	erl -noshell -pa ebin -eval "eunit:test(effective_interest_rate, [verbose])" -s init stop

clean:
	rm -f ebin/*.beam