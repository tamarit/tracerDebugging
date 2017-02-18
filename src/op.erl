% c(transformer).
% compile:file(op,[{parse_transform,transformer},verbose,report_errors,report_warnings]).
-module(op).
-export([main/0]).

main()->
	List = [{1,2},{3,4},{5,6}],
	[{X,Y}|| {X, Y} <- List].