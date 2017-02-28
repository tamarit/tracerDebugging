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
		stack_exp_clauses = [],  
		stack_calls = [],  
		environment = [],
		bd = undefined%,  
		% openData = undefined
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% records definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init()->
	{ok, Df} = 
		dets:open_file(traces, [{type, set}]),
	State = 
		#state{bd = Df},
	loop(State).

loop(State = #state{
		id = Id, 
		stack_exp_clauses = StackEC,
		environment = Env}) ->
	receive
		exit ->
			io:format("Sale\n"),
			io:format("Final Stack: ~w\n", [StackEC]),
			io:format("Final Environment: ~p\n", [Env]),
			dets:traverse(
				traces,
				fun(X) -> 
					io:format("~p\n", [X]), 
					continue 
				end),
			case StackEC of 
				[] -> 
					io:format("The execution finished succefully.\n");
				_ -> 
					io:format("An error occurred while executing the following expressions.\n"),
					[case dets:lookup(traces, N) of 
						[{N, {begin_exp, Type, {ASTId, Pos, PP, MemInfo}}}] -> 
							io:format("~s\n\tin line ~p\n", [PP, Pos]);
						[{N, {begin_clause, _, _, {ASTId, Pos, PP, MemInfo}}}] -> 
							io:format("Clause:\n~s\n\tin line ~p\n", [PP, Pos])
					 end
					|| N <- StackEC]
			end,
			dets:close(traces),
			file:delete("traces"),
			ok;
		T = {begin_exp, Type, {ASTId, Pos, PP, MemInfo}} ->
			io:format("Entra trace BEGIN ~p\n", [Id]),
			% Store partial trace info in the dets
			dets:insert(traces, {Id, T}),
			% Stack the expression 
			NStackEC = 
				[Id | StackEC],
			% New trace id
			NId = 
				Id + 1,
			% Si es llamada guardar en pila de llamadas y crear nuevo contexto de vars
			NState = 
				State#state{
					id = NId,
					stack_exp_clauses = NStackEC
				},
			loop(NState);
		T = {end_exp, Value, BoundedVarsNames, BoundedVarsValues} -> 
			io:format("Entra trace END\n"),
			DictBindings = 
				lists:zip(BoundedVarsNames, BoundedVarsValues),
			% Unstack trace id
			NStackEC = 
				tl(StackEC),
			% Complete the trace info on the dets
			[{TraceId, {begin_exp, Type, {ASTId, Pos, PP, MemInfo}}}] = 
				dets:lookup(traces, hd(StackEC)),
			dets:insert(traces, {TraceId, {expression, ASTId, Type, Pos, PP, MemInfo, Value, DictBindings, NStackEC}}),
			% Store bindings in the environment table
			NEnv = 
					[{Var, {TraceId, VarValue}} || {Var, VarValue} <- DictBindings] 
				++ 	Env,
			% si es una llamada desapilar de pila de llamadas
			% Si es funcion anonima o named func guardar el contexto actual
			NState = 
				State#state{
					stack_exp_clauses = NStackEC,
					environment = NEnv
				},			
			loop(NState);
		T = {begin_clause, PrevClausesFailReason, DictBindings, Info} ->
			io:format("Entra trace BEGIN CLAUSE\n"),
			% Store partial trace info in the dets
			dets:insert(traces, {Id, T}),
			% Stack the expression 
			NStackEC = 
				[Id | StackEC],
			% Store bindings in the environment table
			NEnv = 
					[{Var, {Id, VarValue}} || {Var, VarValue} <- DictBindings] 
				++ 	Env,
			% New trace id
			NId = 
				Id + 1,
			NState = 
				State#state{
					id = NId,
					stack_exp_clauses = NStackEC,
					environment = NEnv
				},	
			loop(NState);
		T = {end_clause, Value} ->
			io:format("Entra trace END CLAUSE\n"),
			% Unstack trace id
			NStackEC = 
				tl(StackEC),
			% Complete the trace info on the dets
			[{TraceId, {begin_clause, PrevClausesFailReason, DictBindings, Info}}] = 
				dets:lookup(traces, hd(StackEC)),
			dets:insert(traces, {TraceId, {clause, PrevClausesFailReason, DictBindings, Info, Value, NStackEC}}),
			NState = 
				State#state{
					stack_exp_clauses = NStackEC
				},
			loop(NState)
		% {newExpresion, Term} ->
		% 	[TypeMsg, IdAst, PP, Bindings, TypeNode] = Term,
		% 	case lists:member(TypeMsg, save_msg()) of 
		% 		true ->
		% 			{ok, Df} = 
		% 				dets:open_file("bd.file",[]),
					
		% 			dets:insert(
		% 				Df, 
		% 				{State#state.id + 1, {Term}}),
					
		% 			dets:close(Df),
		% 			% Open = 
		% 			% 	case is_Open(TypeMsg) of
		% 			% 		'not' ->;
		% 			% 		'begin' ->;
		% 			% 		'begin_pattern' ->;
		% 			% 	end,
		% 			NewState = 
		% 				State#state{
		% 					id = State#state.id + 1, 
		% 					pila = State#state.pila ++ [{State#state.id + 1, {Term}}],
		% 					openData = State#state.openData ++ [TypeMsg]},
					
		% 			loop(NewState);
		% 		false ->
		% 			?PVALUE("p", "Existe un error en el tipo de mensaje", {State#state.id})
		% 	end,
		% 	loop(State);

		% {dropPila} ->
		% 	loop(
		% 		State#state{
		% 			pila = 
		% 				[]});

		% {dropBD} ->
		% 	loop(
		% 		State#state{
		% 			pila = 
		% 			[]});

		% {showBD, Pid} ->
		% 	{ok, Df} = State#state.bd,
		% 	?PVALUE("p", "Contenido de la base de datos", dets:match(Df, '$1')),
		% 	% Pid ! {data, dets:match(Df, '$1')},
		% 	loop(State);

		% {showPila, Pid} ->
		% 	?PVALUE("p", "Contenido de la pila", State#state.pila),
		% 	% Pid ! {data, State#state.pila},
		% 	loop(State);

		% Other ->
		% 	% dets:close(
		% 	% 	State#state.bd),

		% 	erlang:exit(
		% 		self(), 
		% 		{
		% 			error, 
		% 			{"Title: Error option.~n", Other}})
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
