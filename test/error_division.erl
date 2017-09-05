-module(error_division).
-export([main/2]).

main(Y1, X1) when Y1 < X1 ->
	f(X1,Y1)
.
f(A1,B1) ->
	A1/B1
.