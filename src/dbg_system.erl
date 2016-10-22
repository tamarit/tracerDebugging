-module(dbg_system).
-export([init/0]).
-record(state,{listVariables, maxLengthVariable, freeVariable, acc}).

init() ->
	State = #state{listVariables = [], maxLengthVariable = "", freeVariable= "", acc = 0},
	loop(State).

loop(State) ->
	receive
		{exit, Pid} ->
			io:format("Menssage: Pid(~p) decided finalize the execution.~n~n",[Pid]);
		
		{newVariable, Variable} ->
			case lists:member(Variable, State#state.listVariables) of
				false ->
					NewState = State#state{listVariables = lists:append(State#state.listVariables, [Variable])},
					loop(NewState);
				true ->
					loop(State)
			end;
			
		{freeVariable, Pid} ->
			NewState = State#state{maxLengthVariable = biggest_variable(State#state.listVariables, "", 0)},
			loop(NewState);

		{newFreeVariable, Pid} ->
			NewState  =	State#state{
								freeVariable = State#state.maxLengthVariable ++ integer_to_list(State#state.acc + 1), 
								acc = State#state.acc + 1
								},

			Pid ! {ok, NewState#state.freeVariable},
			loop(NewState);

		Other ->
			erlang:exit(self(), {error, {"Title: Error option.~n", Other}})

	after
		80000  ->
			loop(State)

	end.

biggest_variable([Hd|Ld], Value, Leng) ->
	SizeVariable = string:len(Hd),
	if
		 SizeVariable > Leng ->
			biggest_variable(Ld, Hd, SizeVariable);
		true ->
			biggest_variable(Ld, Value, Leng)
	end;

biggest_variable([], Value, _) ->
	Value.
