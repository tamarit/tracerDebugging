-module(dbg_tracer).
-export([init/0]).
-record(state,{stackbegins, stacktrace, stackcall}).


init()->
	loop(State).

loop(State) ->
	receive
		{exit, Pid} ->
			io:format("Menssage: Pid(~p) decided finalize the execution.~n~n",[Pid]);

		{newBegin, Pid} ->
			
			loop(NewState);

		{newValExpr, Pid} ->
			
			loop(NewState);

		Other ->
			erlang:exit(self(), {error, {"Title: Error option.~n", Other}})

	after
		50000  ->
			% io:format("Refresc New state is:  ~p~n",[State]),
			loop(State)

	end.
