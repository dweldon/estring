compile: 
	erl -make
clean:
	rm -rf ebin/*.beam
test:
	erl -pa "ebin" -noshell -eval "estring:test()" -s init stop
