% c(transformer).
% compile:file(op,[{parse_transform,transformer},verbose,report_errors,report_warnings]).
-module(op).
-export([main/2]).

main(0, 0)->
	0;
main(0, X)->
	X;
main(X, Y)->
	X+Y.