-module(ej1_1).
-export([main/1]).

main(X) ->
	A = f(X),
	B = g(X, A),
	C = p(10000),
	h(A, B).

p(0) -> 
	[];
p(N) -> 
	[N|p(N - 1)].

f(A) ->
	case A of 
		0 -> 
			A;
		_ -> 
			A * A
	end.

g(A, B) ->
	case (A / (B + 1)) of 
		A -> 
			B * A;
		_ -> 
			A * B
	end.

h(A, B) -> 
	case A =< B of 
		true -> 
			B / A;
		false -> 
			A * B 
	end.

