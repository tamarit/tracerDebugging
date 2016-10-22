-module(dbg_tracer).
-export([init/0]).
-record(state,{idStackBegins, idStackCalls, stackbegins, stacktrace, stackcall}).


init()->
	State = #state{	idStackBegins = 0, idStackCalls = 0, 
					stackbegins = undefine, 
					stacktrace = undefine,
					stackcall = undefine},
	loop(State).

loop(State) ->
	receive
		{exit, Pid} ->
			dets:close(State#state.stackbegins),
			dets:close(State#state.stacktrace),
			dets:close(State#state.stackcall),
			io:format("Menssage: Pid(~p) decided finalize the execution.~n~n",[Pid]);

		{newBeginTrace, Begin} ->
			io:format("Entro ~p~n",[Begin]),
			{ok, Refbegin} = dets:open_file("stackbegins.file",[]), %State#state.stackbegins,
			Acc = State#state.idStackBegins + 1,
			dets:insert(Refbegin, {Acc, Begin}),
			dets:close(Refbegin),
			NewState = #state{idStackBegins = Acc},
			loop(NewState);

		{showBeginTrace, Pid} ->
			{ok, Refbegin} = State#state.stackbegins,
			Pid ! dets:match(Refbegin, '$1'),
			loop(State);

		{newExpresion, Term} ->
			loop(State);

		{newValueTerm, Term} ->
			loop(State);

		{newBeginCall, Begin} ->
			{ok, Refbegin} = State#state.stackcall,
			Acc = State#state.idStackCalls + 1,
			dets:insert(Refbegin, {Acc, Begin}),
			NewState = #state{idStackCalls = Acc},
			loop(NewState);

		{showBeginCall, Pid} ->
			{ok, RefCall} = State#state.stackcall,
			Pid ! dets:match(RefCall, '$1'),
			loop(State);

		Other ->
			dets:close(State#state.stackbegins),
			dets:close(State#state.stacktrace),
			dets:close(State#state.stackcall),
			io:format("Error other:  ~p~n",[{Other}]),
			erlang:exit(self(), {error, {"Title: Error option.~n", Other}})

	after
		30000  ->
			loop(State)

	end.
