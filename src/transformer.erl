-module(transformer).
-export([parse_transform/2]).

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
	annotation, 
	{	
		id = 0, 
		modify = undefined,  
		bindings = undefined
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_transform 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_transform(Forms, _Options) ->
	unregister_servers(),
    register_servers(),
	% Add annotations with binding information
	FormsAnnBindings = 
		lists:map(
			fun annotate_bindings_form/1,
			Forms),
	% Add annotations with identifier and instrumentation policy information
	{FormsAnn, _} = 
		lists:mapfoldl(
			fun annotate_form/2,
			0,
			FormsAnnBindings),
	dbg_free_vars_server!all_variables_added,
	% ?PVALUE("p", "Annotated", FormsAnn),
	% Intrument the AST to send the traces
	InstForms = 
		lists:map(
			fun instrument_form/1,
			FormsAnn),
	% ?PVALUE("p", "Instumented", InstForms),
	dbg_free_vars_server!exit,
	NewForms = 
		[erl_syntax:revert(IF) || IF <- InstForms],
	[io:format(erl_prettypr:format(F) ++ "\n") || F <- NewForms],
	io:format("Reached\n"),
	Forms.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotate Bindings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_bindings_form(Form)->
	annotate_bindings_form(
		erl_syntax:type(Form), 
		Form).

annotate_bindings_form(_, Form)->
	erl_syntax_lib:annotate_bindings(
		Form,
		ordsets:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Full Annotation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_form(Form, CurrentNodeId) ->
	erl_syntax_lib:mapfold(
		fun annotate_node/2,
		CurrentNodeId, 
		Form).

annotate_node(Node0, CurrentNodeId) ->
	Type = 
		erl_syntax:type(Node0),
	case Type of 
		variable -> 
			dbg_free_vars_server!
				{
					add_variable, 
					erl_syntax:variable_literal(Node0)
				};
		_ -> 
			ok
	end,
	Bindings = 
		erl_syntax:get_ann(Node0),
	Modify = 
		lists:member(Type, instrumented_types()),
	Ann = 
		#annotation
		{
			id = CurrentNodeId,
			modify = Modify,
			bindings = Bindings
		},
	Node1 = 
		case lists:member(Type, expressions_with_patterns()) of 
			true -> 
				NodeDMP = 
					disable_modify_in_patterns(Type, Node0),
				erl_syntax:set_ann(NodeDMP, [Ann]);
			false ->
				erl_syntax:set_ann(Node0, [Ann])
		end,
	Node2 = 
		case lists:member(
				Type, 
				children_not_instrumented_types()) of 
			true -> 
				erl_syntax_lib:map_subtrees(
					fun disable_modify_node/1,
					Node1);
			false ->
				Node1
		end,
	{Node2, CurrentNodeId + 1}.

disable_modify_in_patterns(clause, Node) ->
	Patterns = 
		lists:map(
			fun disable_modify/1,
			erl_syntax:clause_patterns(Node)),
	Guard = 
		disable_modify(
			erl_syntax:clause_guard(Node)),
	Body = 
		erl_syntax:clause_body(Node),	
	erl_syntax:clause(Patterns, Guard, Body);
disable_modify_in_patterns(match_expr, Node) ->
	Pattern = 
		disable_modify(
			erl_syntax:match_expr_pattern(Node)),
	Body = 
		erl_syntax:match_expr_body(Node),
	erl_syntax:match_expr(Pattern, Body);
disable_modify_in_patterns(generator, Node) ->
	Pattern = 
		disable_modify(
			erl_syntax:generator_pattern(Node)),
	Body = 
		erl_syntax:generator_body(Node),
	erl_syntax:generator(Pattern, Body);
disable_modify_in_patterns(binary_generator, Node) ->
	Pattern = 
		disable_modify(
			erl_syntax:binary_generator_pattern(Node)),
	Body = 
		erl_syntax:binary_generator_body(Node),
	erl_syntax:binary_generator(Pattern, Body).


disable_modify(none) ->
	none;
disable_modify(Node) ->
	erl_syntax_lib:map(
		fun disable_modify_node/1,
		Node).

disable_modify_node(Node) ->
	case erl_syntax:get_ann(Node) of 
		[Ann] -> 
			erl_syntax:set_ann(
				Node,
				[Ann#annotation{modify = false}])%;
		% _ ->
		% 	io:format("Node without annotations: ~p\n", [Node]), 
		% 	Node
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Instrumentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instrument_form(Forms) ->
	erl_syntax_lib:map(
		fun instrument_node/1,
		Forms
	).

instrument_node(Node0) ->
	[Annotation] = 
		erl_syntax:get_ann(Node0),
	Type = 
		erl_syntax:type(Node0),
	Node1 = 
		case Type of
			clause -> 
				NewClause = 
					instrument_clause(Node0),
				erl_syntax:set_ann(NewClause, Annotation);
			_ ->
				Node0
		end,
	Node2 = 
		case lists:member(Type, expressions_with_clauses()) of
			true ->
				NewEWC = 
					instrument_expression_with_clauses(Node1, Type),
				erl_syntax:set_ann(NewEWC, Annotation);
			false ->
				Node1
		end,
	% Node2.
	case Annotation#annotation.modify of
		true ->
			instrument_expression(Node2, Annotation);
		false ->
			Node2
	end.

instrument_expression(Term, Annotation) ->
	FreeVariable = 
		get_free_variable(),
	VarsBound = 
		build_bound_vars_array(
			Annotation#annotation.bindings),
	erl_syntax:block_expr(
		[	
			build_send(
				[
					erl_syntax:atom(begin_exp), 
					erl_syntax:integer(Annotation#annotation.id),
					erl_syntax:atom(erl_syntax:type(Term)), 
					erl_syntax:list(
						lists:map(
							fun bindings_to_ast/1, 
							Annotation#annotation.bindings))
				]),	
			erl_syntax:match_expr(
				FreeVariable, 
				erl_syntax:catch_expr(Term)),
			build_case_catch(FreeVariable, VarsBound)
		]).

build_case_catch(VarValue, VarsBound) ->
	erl_syntax:case_expr(
		VarValue,
		[ 
			build_case_catch_clause('ERROR', VarValue, VarsBound),
		  	build_case_catch_clause('THROW', VarValue, VarsBound),
		  	build_case_catch_clause('EXIT',  VarValue, VarsBound),
		  	build_case_catch_clause(no_error, VarValue, VarsBound)
		]).

build_case_catch_clause(no_error, VarValue, VarsBound) ->
	erl_syntax:clause(
  		[erl_syntax:underscore()], 
  		none, 
  		[
  			build_send(
  				[
  					erl_syntax:atom(end_exp), 
  					VarValue,
  					VarsBound
  				]), 
  			VarValue
  		]);
build_case_catch_clause(Other, VarValue, _) ->
	erl_syntax:clause(
		[erl_syntax:tuple(
			[
				erl_syntax:atom(Other),  
				erl_syntax:underscore()
			])], 
  		none, 
  		[
  			build_send(
  				[
  					erl_syntax:atom(error_exp), 
  					VarValue
  				]),
  			% Last line is to stop the computation when error is raised
			erl_syntax:application(
				erl_syntax:atom(erlang) , 
				erl_syntax:atom(exit), 
				[
					erl_syntax:atom(error_in_computation)
				]
			)
  		]).

build_bound_vars_array([{bound, Vars} | _]) ->
	erl_syntax:list(
		[erl_syntax:variable(V) || V <- Vars]);
build_bound_vars_array([_ | Tail]) ->
	build_bound_vars_array(Tail);
build_bound_vars_array([]) ->
	erl_syntax:list([]).

instrument_clause(Clause) ->
	erl_syntax:clause(
		erl_syntax:clause_patterns(Clause), 
		erl_syntax:clause_guard(Clause), 
		instrument_clause_body(
			erl_syntax:clause_body(Clause))
	).

instrument_clause_body(Body) ->
	BodyLast = 
		lists:last(Body),
	BodyRemainder = 
		lists:droplast(Body),
	BodyRemainder
	++ 	instrument_clause_body_last(BodyLast).	

instrument_clause_body_last(Exp) ->
	FreeVariable = 
		get_free_variable(),
	[	
		erl_syntax:match_expr(FreeVariable, Exp),
		build_send(
			[
				erl_syntax:atom(end_clause), 
				FreeVariable
			]),	
		FreeVariable
	].

instrument_expression_with_clauses(Exp, case_expr) ->
	CaseArg = 
		erl_syntax:case_expr_argument(Exp),
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:case_expr_clauses(Exp)),
	erl_syntax:case_expr(
		CaseArg, 
		Clauses);
instrument_expression_with_clauses(Exp, cond_expr) ->
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:cond_expr_clauses(Exp)),
	erl_syntax:cond_expr(
		Clauses);
instrument_expression_with_clauses(Exp, fun_expr) ->
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:fun_expr_clauses(Exp)),
	erl_syntax:fun_expr(
		Clauses);
instrument_expression_with_clauses(Exp, function) ->
	FunName = 
		erl_syntax:function_name(Exp),
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:function_clauses(Exp)),
	erl_syntax:function(
		FunName, 
		Clauses);
instrument_expression_with_clauses(Exp, if_expr) ->
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:if_expr_clauses(Exp)),
	erl_syntax:if_expr(
		Clauses);
instrument_expression_with_clauses(Exp, named_fun_expr) ->
	FunName = 
		erl_syntax:named_fun_expr_name(Exp),
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:named_fun_expr_clauses(Exp)),
	erl_syntax:named_fun_expr(
		FunName,
		Clauses);
instrument_expression_with_clauses(Exp, receive_expr) ->
	Action = 
		erl_syntax:receive_expr_action(Exp),
	Timeout = 
		erl_syntax:receive_expr_timeout(Exp),
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:receive_expr_clauses(Exp)),
	erl_syntax:receive_expr(
		Clauses,
		Timeout,
		Action);
instrument_expression_with_clauses(Exp, try_expr) ->
	Body = 
		erl_syntax:try_expr_body(Exp),
	{Clauses, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:try_expr_clauses(Exp)),
	{Handlers, _} = 
		lists:mapfoldl(
			fun build_clause_begin/2,
			[],
			erl_syntax:try_expr_handlers(Exp)),
	After = 
		erl_syntax:try_expr_after(Exp),
	erl_syntax:try_expr(
		Body,
		Clauses,
		Handlers,
		After).


build_clause_begin(Clause, PreviousPatterns) -> 
	Annotation = 
		erl_syntax:get_ann(Clause),
	Patterns0 = 
		erl_syntax:clause_patterns(Clause),
	Patterns = 
		replace_underscores(Patterns0),
	Guard = 
		erl_syntax:clause_guard(Clause),
	Body = 
		erl_syntax:clause_body(Clause),
	{PreviousClausesTries, PreviousClausesFailReasons} = 
		build_clause_begin_previous_patterns_info(
			Patterns, 
			PreviousPatterns),
	NBody = 
			PreviousClausesTries
		++ 	[build_send([
				erl_syntax:atom(begin_clause),
				erl_syntax:list(PreviousClausesFailReasons),
				build_bound_vars_array(Annotation#annotation.bindings)
			])]
		++ 	Body,
	NewClause = 
		erl_syntax:clause(
			Patterns,
			Guard,
			NBody),
	{
		erl_syntax:set_ann(NewClause, Annotation),
		PreviousPatterns ++ [Patterns]
	}.


build_clause_begin_previous_patterns_info(
		CurrentPatterns, 
		[PrevPatterns | Tail]) -> 
	FreeVar = 
		get_free_variable(),
	Try = 
		erl_syntax:match_expr(
			FreeVar,
			erl_syntax:try_expr(
				[erl_syntax:case_expr(
					erl_syntax:list(CurrentPatterns),
					[erl_syntax:clause(
						[erl_syntax:list(PrevPatterns)],
						none,
						[erl_syntax:atom(guard)])]
				)],
				[erl_syntax:clause(
					[erl_syntax:class_qualifier(
						erl_syntax:underscore(),
						erl_syntax:underscore() )],
					none,
					[erl_syntax:atom(pattern)]
				)]
			)
		),
	{TailTries, TailFailReasons} = 
		build_clause_begin_previous_patterns_info(
			CurrentPatterns, 
			Tail),
	{
		[Try | TailTries], 
		[FreeVar | TailFailReasons]
	};
build_clause_begin_previous_patterns_info(_, []) -> 
	{[], []}.

replace_underscores(Nodes) -> 
	lists:map(
		fun(Node) -> 
			erl_syntax_lib:map(
				fun(N) -> 
					case erl_syntax:type(N) of 
						underscore -> 
							get_free_variable();
						_ ->
							N
					end
				end,
				Node)
		end,
		Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type classifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expressions_with_patterns() ->
	[clause, generator, match_expr, binary_generator].

expressions_with_clauses() ->
	[	case_expr, cond_expr, fun_expr, function, if_expr
	, 	named_fun_expr, receive_expr, try_expr].

children_not_instrumented_types() ->
	not_instrumented_types() -- [clause, function].

instrumented_types() ->
	[
			application, binary, block_expr
		, 	case_expr, catch_expr, fun_expr
		, 	if_expr, infix_expr, list, list_comp
		, 	map_expr, match_expr, named_fun_expr
		, 	prefix_expr, receive_expr, record_access
		, 	record_expr, try_expr, tuple
	].

not_instrumented_types() ->
	[
			annotated_type, arity_qualifier, atom
		, 	attribute, binary_field, binary_generator
		, 	bitstring_type
		, 	char, class_qualifier, clause, conjunction
		, 	comment, cond_expr, constrained_function_type
		, 	constraint, disjunction, eof_marker
		, 	error_marker, float, form_list, fun_type
		, 	function, function_type, generator
		, 	implicit_fun, integer, integer_range_type
		, 	macro, map_field_assoc, map_field_exact
		, 	map_type, map_type_assoc, map_type_exact
		, 	module_qualifier, nil, operator, parentheses
		, 	record_field, record_index_expr, record_type
		, 	record_type_field, size_qualifier, string
		, 	text, tuple_type, typed_record_field
		, 	type_application, type_union, underscore
		, 	user_type_application, variable, warning_marker
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The dbg_tracer should not be on when instrumenting the code. 
% Instead, it should be on when running the code (i.e. after instrumentation).
unregister_servers() ->
	catch unregister(dbg_free_vars_server).

register_servers() ->
	register(
		dbg_free_vars_server, 
		spawn(dbg_free_vars_server, init, [])).

get_free_variable() ->
	dbg_free_vars_server ! {get_free_variable, self()},
	receive 
		Value ->
			erl_syntax:variable(Value)
	end.

bindings_to_ast({Type, ListVars}) -> 
	erl_syntax:tuple(
	[
		erl_syntax:atom(Type),
		erl_syntax:list(
			lists:map(fun erl_syntax:atom/1, ListVars))
	]).

build_send(Msg) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) , 
		erl_syntax:atom(send), 
		[
			erl_syntax:atom(dbg_tracer),
	 		erl_syntax:tuple(Msg)
		]
	).