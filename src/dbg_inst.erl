-module(dbg_inst).

-export([parse_transform/2]).

-record(ann_state, {id = 0, modify = true }).

parse_transform(Forms, Options) ->

    erl_unregister(),
    erl_register(),

	{NewAnnForms, NumNodes} = lists:mapfoldl(
									fun select_form/2,
									0,
									Forms
								),

	dbg_system ! {free_variable, self()},
	NewFormsSyntaxTree = lists:map(
									fun select_ast/1,
									NewAnnForms
								),
	NewFormsAst = erl_syntax:revert_forms(NewFormsSyntaxTree),
	erl_apply_ast("", NewFormsAst),
	NewFormsAst.

select_ast(Form)->
	case erl_syntax:type(Form) of
		'attribute' ->
			Form;		
		Other ->
			erl_syntax_lib:map(
				fun build_block_exprs/1,
				Form)
				
	end.

build_block_exprs(Term) ->
	[State_ann] = erl_syntax:get_ann(Term),
	
	if
		State_ann#ann_state.modify ->
			dbg_system ! {newfree_variable, self()},
			Newfree_variable = erl_syntax:variable(free_variable()),
			erl_syntax:block_expr([	
							build_send_process(dbg_tracer, [
									erl_syntax:atom('newExpresion'),
									erl_syntax:list([
											erl_syntax:atom('begin'),
											erl_syntax:integer(State_ann#ann_state.id),
											erl_syntax:atom(erl_syntax:type(Term))
										])
								]),
							erl_syntax:match_expr(
									Newfree_variable, 
									erl_syntax:catch_expr(Term)
								),
							build_case_error(Newfree_variable)
						]);

		true ->
			Term
	end.

create_ann(Ann, Term, BuildTerm, Status, Acc) ->
	case Ann of
		add ->
			{erl_syntax:add_ann(#ann_state{id = Acc, modify = Status}, BuildTerm), Acc + 1};

		set ->
			[State] = erl_syntax:get_ann(Term),
			erl_syntax:set_ann(Term, [State#ann_state{modify = Status}])

	end.

select_term(Term, Acc) ->
	case erl_syntax:type(Term) of
		'function' ->		
				BuildName = case erl_syntax:function_name(Term) of
								'none' ->
									none;

								Val ->
									erl_syntax_lib:map(
											fun (Val) ->
												create_ann(set, Val, none, false, none)
											end,
											Val
										)
							end,

				BuildClauses = erl_syntax:function_clauses(Term),
				BuildFunction = erl_syntax:function(BuildName, BuildClauses),
				create_ann(add, Term, BuildFunction, false, Acc);

		'eof_marker' ->
				create_ann(add, none, Term, false, Acc);

		'variable' ->
				NewTerm = erl_syntax:variable_literal(Term),
				dbg_system ! {newVariable, NewTerm},
				create_ann(add, none, Term, true, Acc);

		'clause' ->
				Guard = case erl_syntax:clause_guard(Term) of
							'none' ->
								none;
							Val ->
								erl_syntax_lib:map(
										fun (Val) ->
											create_ann(set, Val, none, false, none)
										end,
										Val
						 			)
						 end,

				Pattern = lists:map(
								fun (Val) ->
									create_ann(set, Val, none, false, none)
								end,
								erl_syntax:clause_patterns(Term)
							),
				BuildTerm = erl_syntax:clause(Pattern, Guard, erl_syntax:clause_body(Term)),
				create_ann(add, Term, BuildTerm, false, Acc);

		'binary_generator' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									create_ann(set, Term, none, false, none)
								end,
								erl_syntax:binary_generator_pattern(Term)
							),
				BuildTerm = erl_syntax:binary_generator(Pattern, erl_syntax:binary_generator_body(Term)),
				create_ann(add, Term, BuildTerm, false, Acc);
		
		'generator' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									create_ann(set, Term, none, false, none)
								end,
								erl_syntax:generator_pattern(Term)
							),
				BuildTerm = erl_syntax:generator(Pattern, erl_syntax:generator_body(Term)),
				create_ann(add, Term, BuildTerm, false, Acc);

		'operator' ->
				create_ann(add, none, Term, false, Acc);

		'atom' ->
				create_ann(add, none, Term, false, Acc);

		'char' ->
				create_ann(add, none, Term, false, Acc);

		'integer' ->
				create_ann(add, none, Term, false, Acc);													
		
		'float ' ->
				create_ann(add, none, Term, false, Acc);							
		
		'match_expr' ->
				Pattern = erl_syntax_lib:map(
								fun (Term) ->
									create_ann(set, Term, none, false, none)
								end,
								erl_syntax:match_expr_pattern(Term)
							),

				BuildTerm = erl_syntax:match_expr(Pattern, erl_syntax:match_expr_body(Term)),
				create_ann(add, Term, BuildTerm, true, Acc);

		Other ->
			create_ann(add, none, Term, true, Acc)
	end.

select_form(Form, Acc) ->
	erl_syntax_lib:mapfold(
			fun select_term/2,
			Acc, 
			Form
		).

erl_apply_ast(Apply, Ast)->
	{ok, Module, Bin}  = compile:forms(Ast),
	code:load_binary(Module, "nofile", Bin),
	erlang:apply(Module, main, [2, 4]).

free_variable() ->
	receive 
		{ok, Value} ->
			Value;

		Error ->
			erlang:exit(self(), {error, {"Title: Fail to create free variable.~n", Error}})
	end.

erl_register() ->
	register(dbg_tracer, spawn(dbg_tracer, init, [])),
	register(dbg_system, spawn(dbg_system, init, [])).

erl_unregister() ->
	catch unregister(dbg_tracer),
	catch unregister(dbg_system).

build_send_process(Process, Msg) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) , 
		erl_syntax:atom(send), 
			[
				erl_syntax:atom(Process),
		 		erl_syntax:tuple(Msg)
		 	]
		 ).

build_case_error(Value) ->
	dbg_system ! {newfree_variable, self()},
	NewVariable = erl_syntax:variable(free_variable()),
	erl_syntax:case_expr(
		 Value,
		[
		  erl_create_clause(error, [Value, NewVariable]),
		  erl_create_clause(throw, [Value, NewVariable]),
		  erl_create_clause(exit,  [Value, NewVariable]),
		  erl_create_clause(other, [Value, NewVariable])
		]).

erl_create_clause(other, [Value, _]) ->
	erl_syntax:clause(
  		[Value], 
  		none, 
  		[build_send_process(dbg_tracer, [
  				erl_syntax:atom(newValueTerm), 
  				Value]), Value]);

erl_create_clause(error, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('ERROR'),  
				NewVariable])],
		none,
		[build_send_process(dbg_tracer, [Value])
		]);

erl_create_clause(throw, [Value, NewVariable]) ->	
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('THROW'),  
				NewVariable])],
		none,
		[build_send_process(dbg_tracer, [Value])
		]);
	
erl_create_clause(exit, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
				erl_syntax:atom('EXIT'), 
				NewVariable])],
		none,
		[build_send_process(dbg_tracer, [Value])
		]);
	
erl_create_clause(Other, [_]) ->
	erl_syntax:clause([
		none,
		none,
		none]).