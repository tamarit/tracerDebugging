-module(adbg).
-export([run/0, prof/0, profiling/0]).

run() -> 
	compile:file("ej1", [{parse_transform, transformer}]),
	unregister_servers(),
	register_servers(),
	try 
		ej1:main(0)
	catch 
		_:_ -> 
			notify_all(list_processes(), error),
			ok
	end,
	notify_all(list_processes(), exit),
	ok.

% run() -> 
% 	code:purge(ej1_1),
% 	code:delete(ej1_1),
% 	compile:file("ej1_1", []),
%     BefWO = 
%     	erlang:monotonic_time(millisecond),
%     try 
% 		ej1_1:main(0)
% 	catch 
% 		_:_ -> 
% 			ok
% 	end,
%     AftWO = 
%     	erlang:monotonic_time(millisecond),
%     TimeExecutingWOInst = AftWO - BefWO,
%  	code:purge(ej1),
%  	code:delete(ej1),
% 	compile:file("ej1", [{parse_transform, transformer}]),
% 	unregister_servers(),
% 	register_servers(),
% 	BefW = 
%     	erlang:monotonic_time(millisecond),
% 	try 
% 		ej1:main(0)
% 	catch 
% 		_:_ -> 
% 			ok
% 	end,
% 	AftW = 
%     	erlang:monotonic_time(millisecond),
%     TimeExecutingWInst = AftW - BefW,
%     io:format("Time without instrumentation: ~p\n", [TimeExecutingWOInst]),
%     io:format("Time with instrumentation: ~p\n", [TimeExecutingWInst]),
% 	dbg_tracer!exit,
% 	ok.


unregister_servers() ->
	catch unregister(dbg_tracer).

register_servers() ->
	register(
		dbg_tracer, 
		spawn(dbg_tracer, init, [])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Profiling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() -> 
	code:purge(ej1_1),
	code:delete(ej1_1),
	compile:file("ej1_1", []),
 	code:purge(ej1),
 	code:delete(ej1),
	compile:file("ej1", [{parse_transform, transformer}]),
	unregister_servers(),
	register_servers(),
	ok.

init(Name) -> 
 	code:purge(Name),
 	code:delete(Name),
	compile:file(Name, [{parse_transform, transformer}]),
	unregister_servers(),
	register_servers(),
	ok.

prof() ->
	init(),
    eprof:start(),
    eprof:start_profiling([self(), dbg_tracer]),
    main1(),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop(),
    eprof:start(),
    eprof:start_profiling([self(), dbg_tracer]),
    main2(),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop(). 

profiling() ->
	init(op),
    eprof:start(),
    eprof:start_profiling([self(), dbg_tracer]),
   	op(),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop(). 

op() ->
    StartInput = os:timestamp(),
    try 
		op:main(1,1)
	catch 
		_:_ -> 
			notify_all(list_processes(), error),
			ok
	end,
	notify_all(list_processes(), exit),
    io:format("Without: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)/1000000]),
    ok.

main1() ->
    StartInput = os:timestamp(),
    try 
		ej1_1:main(0)
	catch 
		_:_ -> 
			notify_all(list_processes(), error),
			ok
	end,
	notify_all(list_processes(), exit),
    io:format("Without: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartInput)/1000000]),
    ok.

main2() ->
    StartRes = os:timestamp(),
    try 
		ej1:main(0)
	catch 
		_:_ -> 
			notify_all(list_processes(), error),
			ok
	end,
	notify_all(list_processes(), exit),
    io:format("With: total time taken ~p seconds~n", [timer:now_diff(os:timestamp(), StartRes)/1000000]),
    ok.

notify_all([], Msg) ->
	ok;
notify_all([T|L], Msg) ->
	T ! Msg,
	notify_all([L], Msg).

list_processes() ->
 	[dbg_tracer, dbg_free_vars_server].