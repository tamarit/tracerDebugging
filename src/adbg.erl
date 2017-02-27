-module(adbg).
-export([run/0]).

run() -> 
	compile:file("ej1", [{parse_transform, transformer}]),
	unregister_servers(),
	register_servers(),
	try 
		ej1:main(0)
	catch 
		_:_ -> 
			ok
	end,
	dbg_tracer!exit,
	ok.


unregister_servers() ->
	catch unregister(dbg_tracer).

register_servers() ->
	register(
		dbg_tracer, 
		spawn(dbg_tracer, init, [])).