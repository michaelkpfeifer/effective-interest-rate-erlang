all:
	erl -make

tests: 
	erl -make
	erl -noshell -pa ebin -eval "eunit:test(effective_interest_rate, [verbose])" -s init stop

clean:
	rm -f ebin/*.beam
