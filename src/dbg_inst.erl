-module(dbg_inst).

-export([parse_transform/2]).

-record(ann_state, {id = 0, modify = true }).

parse_transform(Forms, Options) ->

    erlUnregister(),
    erlRegister(),

	{NewAnnForms, NumNodes} = lists:mapfoldl(
									fun selectForm/2,
									0,
									Forms
								),

	dbg_system ! {freeVariable, self()},
	NewFormsSyntaxTree = lists:map(
					fun selectAst/1,
					NewAnnForms
				),

	NewFormsAst = erl_syntax:revert_forms(NewFormsSyntaxTree),

	erlApplyAst("", NewFormsAst),
	erlUnregister(),
	NewFormsAst.

selectAst(Form)->
	case erl_syntax:type(Form) of
		'attribute' ->
			Form;		
		Other ->
			erl_syntax_lib:map(
				fun buildBlockExprs/1,
				Form)
				
	end.

buildBlockExprs(Term) ->
	[State_ann] = erl_syntax:get_ann(Term), 
	if
		State_ann#ann_state.modify ->
			dbg_system ! {newFreeVariable, self()},
			NewFreeVariable = erl_syntax:variable(freeVariable()),
			erl_syntax:block_expr([	
							erl_syntax:match_expr(
									NewFreeVariable, 
									erl_syntax:catch_expr(Term)
								),
							buildCaseError(NewFreeVariable)
						]);

		true ->
			Term
	end.

createAnn(Ann, Term, BuildTerm, Status, Acc) ->
	case Ann of
		add ->
			{erl_syntax:add_ann(#ann_state{id = Acc, modify = Status}, BuildTerm), Acc + 1};

		set ->
			[State] = erl_syntax:get_ann(Term),
			erl_syntax:set_ann(Term, [State#ann_state{modify = Status}])

	end.

selectTerm(Term, Acc) ->
	case erl_syntax:type(Term) of
		'function' ->		
				BuildName = case erl_syntax:function_name(Term) of
								'none' ->
									none;

								Val ->
									erl_syntax_lib:map(
											fun (Val) ->
												createAnn(set, Val, none, false, none)
											end,
											Val
										)
							end,

				BuildClauses = erl_syntax:function_clauses(Term),
				BuildFunction = erl_syntax:function(BuildName, BuildClauses),
				createAnn(add, Term, BuildFunction, false, Acc);

		'eof_marker' ->
				createAnn(add, none, Term, false, Acc);

		'variable' ->
				NewTerm = erl_syntax:variable_literal(Term),
				dbg_system ! {newVariable, NewTerm},
				createAnn(add, none, Term, true, Acc);

		'clause' ->
				Guard = case erl_syntax:clause_guard(Term) of
							'none' ->
								none;
							Val ->
								erl_syntax_lib:map(
										fun (Val) ->
											createAnn(set, Val, none, false, none)
										end,
										Val
						 			)
						 end,

				Pattern = lists:map(
								fun (Val) ->
									createAnn(set, Val, none, false, none)
								end,
								erl_syntax:clause_patterns(Term)
							),

				BuildTerm = erl_syntax:clause(Pattern, Guard, erl_syntax:clause_body(Term)),
				createAnn(add, Term, BuildTerm, false, Acc);

		'binary_generator' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									createAnn(set, Term, none, false, none)
								end,
								erl_syntax:binary_generator_pattern(Term)
							),
				BuildTerm = erl_syntax:binary_generator(Pattern, erl_syntax:binary_generator_body(Term)),
				createAnn(add, Term, BuildTerm, false, Acc);
		
		'generator' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									createAnn(set, Term, none, false, none)
								end,
								erl_syntax:generator_pattern(Term)
							),
				BuildTerm = erl_syntax:generator(Pattern, erl_syntax:generator_body(Term)),
				createAnn(add, Term, BuildTerm, false, Acc);

		'operator' ->
				createAnn(add, none, Term, false, Acc);

		'atom' ->
				createAnn(add, none, Term, false, Acc);

		'char' ->
				createAnn(add, none, Term, false, Acc);

		'integer' ->
				createAnn(add, none, Term, false, Acc);													
		
		'float ' ->
				createAnn(add, none, Term, false, Acc);							
		
		'match_expr' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									createAnn(set, Term, none, false, none)
								end,
								erl_syntax:match_expr_pattern(Term)
							),

				BuildTerm = erl_syntax:match_expr(Pattern, erl_syntax:match_expr_body(Term)),
				createAnn(add, Term, BuildTerm, true, Acc);
		
		Other ->
			createAnn(add, none, Term, true, Acc)
	end.

selectForm(Form, Acc) ->
	erl_syntax_lib:mapfold(
			fun selectTerm/2,
			Acc, 
			Form
		).

erlApplyAst(Apply, Ast)->
	{ok, Module, Bin}  = compile:forms(Ast),
	code:load_binary(Module, "nofile", Bin),
	erlang:apply(Module, main, [0, 4]).

freeVariable() ->
	receive 
		{ok, Value} ->
			Value;

		Error ->
			erlang:exit(self(), {error, {"Title: Fail to create free variable.~n", Error}})
	end.

erlRegister() ->
	% register(dbg_server, spawn(dbg_server, init, [])),
	register(dbg_tracer, spawn(dbg_tracer, init, [])),
	register(dbg_system, spawn(dbg_system, init, [])).

erlUnregister() ->
	% catch unregister(dbg_server),
	catch unregister(dbg_tracer),
	catch unregister(dbg_system).

buildSendProcess(Process, Msg) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) , 
		erl_syntax:atom(send), 
			[
				erl_syntax:atom(Process),
		 		erl_syntax:tuple(Msg)
		 	]
		 ).

buildCaseError(Value) ->
	dbg_system ! {newFreeVariable, self()},
	NewVariable = freeVariable(),
	erl_syntax:case_expr(
		 Value,
		[
		  createClause(error, [Value, NewVariable]),
		  createClause(throw, [Value, NewVariable]),
		  createClause(exit,  [Value, NewVariable]),
		  createClause(other, [Value, NewVariable])
		]).

createClause(other, [Value, _]) ->
	erl_syntax:clause(
  		[Value], 
  		none, 
  		[buildSendProcess(dbg_tracer, [erl_syntax:atom('newValExpr'), Value]), Value]);

createClause(error, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('ERROR'),  
				erl_syntax:variable(NewVariable)])],
		none,
		[buildSendProcess(dbg_tracer, [erl_syntax:atom('newValExpr'), Value])
		]);

createClause(throw, [Value, NewVariable]) ->	
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('THROW'),  
				erl_syntax:variable(NewVariable)])],
		none,
		[buildSendProcess(dbg_tracer, [erl_syntax:atom('newValExpr'), Value])
		]);
	
createClause(exit, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('EXIT'), 
				erl_syntax:variable(NewVariable)])],
		none,
		[buildSendProcess(dbg_tracer, [erl_syntax:atom('newValExpr'), Value])
		]);
	
createClause(Other, [_]) ->
	erl_syntax:clause([
		none,
		none,
		none]).