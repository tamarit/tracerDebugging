-module(dbg_tracer).
-export([init/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% macros definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define( debug, true ).
-ifdef ( debug ).
	-define( PVALUE(_Type, _Mess, _Value), 
	        _toString = if
	                _Type == "p"-> "~nPRINT: ~p ~n ~p ~n";
	                _Type == "s"-> "~nPRINT: ~p ~n ~s ~n";
	                _Type == "w"-> "~nPRINT: ~p ~n ~w ~n";
	                _Type == "c"-> "~nPRINT: ~p ~n ~c ~n";
	                _Type == "f"-> "~nPRINT: ~p ~n ~f ~n";
	                _Type == "e"-> "~nPRINT: ~p ~n ~e ~n";
	                _Type == "g"-> "~nPRINT: ~p ~n ~g ~n";
	                _Type == "W"-> "~nPRINT: ~p ~n ~W ~n";
	                _Type == "P"-> "~nPRINT: ~p ~n ~P ~n";
	                _Type == "B"-> "~nPRINT: ~p ~n ~B ~n";
	                _Type == "X"-> "~nPRINT: ~p ~n ~X ~n";
	                true  -> "PRINT: <<ERROR>>"
	            end,
	            io:format( _toString , [ _Mess, _Value ] ) ).
	-define( PRINT( _Print ), 
	            io:format( "~n~p~n", [_Print] ) ).
	-define( LOG( _Content ),
	            timer:sleep(1200),
	            io:format("-----------------<(^_^)>-------------------"),
	            io:format(" ( Estoy en: )-->>> Linea: (~p) 
	                        ~n -- Maquina:(~p) ~n -- Fichero: (~p) ~n -- Module: (~p) ~n -- Version: (~p) ~n -- LOG: ~p ~n ~n",
	                        [ ?LINE, ?MACHINE, ?FILE, ?MODULE, ?VERSION, _Content ] ) ).
-else.
	-define( LOG( _Content ), void ).
	-define( PVALUE( _Type, _Mess, _Value ), void ).
	-define( PRINT( _Print ), void ).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% records definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(
	state, 
	{	
		id = 0, 
		pila = [],  
		bd = undefined,  
		openData = undefined
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% records definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init()->
	State = #state{	id = 0, pila = [], bd = undefine, openData = undefine},
	%dets:open_file("bd.file",[])},
	% %%Debug
	% 	{ok, Refbegin} = State#state.bd,
	% %%%
	loop(State).

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].


add_element_store(Type, [], Id, IdBegin) ->
Id;

add_element_store(Type, [T|L], Id, IdBegin) ->
	case dict:is_key(T, Id) of
		false ->
			add_element_store(Type, L, dict:store(T, IdBegin, Id), IdBegin);
		true ->
			add_element_store(Type, L, Id, IdBegin)
	end.

loop(State) ->
	receive
		{exit, Pid} ->
			dets:close(State#state.bd),
			io:format("Menssage: Pid(~p) decided finalize the execution.~n~n",[Pid]),
			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Exit.~n", Pid}});


		{newExpresion, Term} ->
			[TypeMsg, IdAst, PP, Bindings, TypeNode] = Term,
			case lists:member(TypeMsg, save_msg()) of 
				true ->
					{ok, Df} = 
						dets:open_file("bd.file",[]),
					
					dets:insert(
						Df, 
						{State#state.id + 1, {Term}}),
					
					dets:close(Df),
					% Open = 
					% 	case is_Open(TypeMsg) of
					% 		'not' ->;
					% 		'begin' ->;
					% 		'begin_pattern' ->;
					% 	end,
					NewState = 
						State#state{
							id = State#state.id + 1, 
							pila = State#state.pila ++ [{State#state.id + 1, {Term}}],
							openData = State#state.openData ++ [TypeMsg]},
					
					loop(NewState);
				false ->
					?PVALUE("p", "Existe un error en el tipo de mensaje", {State#state.id})
			end,
			loop(State);

		{dropPila} ->
			loop(
				State#state{
					pila = 
						[]});

		{dropBD} ->
			loop(
				State#state{
					pila = 
					[]});

		{showBD, Pid} ->
			{ok, Df} = State#state.bd,
			?PVALUE("p", "Contenido de la base de datos", dets:match(Df, '$1')),
			% Pid ! {data, dets:match(Df, '$1')},
			loop(State);

		{showPila, Pid} ->
			?PVALUE("p", "Contenido de la pila", State#state.pila),
			% Pid ! {data, State#state.pila},
			loop(State);

		Other ->
			% dets:close(
			% 	State#state.bd),

			erlang:exit(
				self(), 
				{
					error, 
					{"Title: Error option.~n", Other}})

	after
		30000  ->
			loop(State)

	end.

save_msg() ->
	['begin', 'end', 'begin_pattern', 'end_pattern'].

is_Open(Msg) ->
	case Msg of
		'end' ->
			'begin';
		'end_pattern' ->
			'begin_pattern';
		_ ->
			'not'
	end.
