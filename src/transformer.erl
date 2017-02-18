-module(transformer).
-export([parse_transform/2]).

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
	%Transformar los nodos a formato SyntaxTree
	FormsAnnBindings = 
		lists:map(
			fun annotate_bindings_form/1,
			Forms),
	% io:format("~p\n", [FormsAnnBindings]),
	{FormsAnn, _} = 
		lists:mapfoldl(
			fun annotate_form/2,
			0,
			FormsAnnBindings),
	?PVALUE("p", "Etiquetado", FormsAnn),

	% InstForms = 
	% 	lists:map(
	% 		fun inst_anno_form/1,
	% 		FormsAnn),
	% ?PVALUE("p", "Arbol instrumentado", InstForms),

	dbg_free_vars_server!exit,
	Forms.
		% ?PVALUE("p", "Arbol SyntaxTree", CreerSyntaxTree),
	% %Etiqueta todos los nodos con nuestra etiqueta
	% {NouveauSyntaxTreeEttiquete, Compte} = lists:mapfoldl(
	% 										fun abreSyntaxTree/2,
	% 										0,
	% 										CreerSyntaxTree
	% 									),
	% 	% ?PVALUE("p", "Abre Nouveau SyntaxTree Etiquette", NouveauSyntaxTreeEttiquete),
	% 	% ?PVALUE("p", "Abre Nouveau SyntaxTree Compte", Compte),

	% %Cambia las etiquetas de forma que se ponen a true los nodos que se han de instrumentar
	% NouveauSyntaxTree = lists:map(
	% 								fun creerNouveauNoeud/1,
	% 								NouveauSyntaxTreeEttiquete
	% 							),
	% 	?PVALUE("p", "Abre Nouveau SyntaxTree", NouveauSyntaxTree),
	% 		% dbg_free_vars_server ! {freeVariable, self()},
	% %Parse a AST desde SyntaxTree
	% NouveauAST = erl_syntax:revert_forms(CreerSyntaxTree),
	% 	% ?PVALUE("p", "Abre Nouveau AST ", NouveauAST),

	% NouveauAST.

% etiquettes(Noeud, Compte, Etat)->
% 	case erl_syntax:get_ann(Noeud) of
% 		[] -> 
% 		% ?PVALUE("p", "Entro dentro", Noeud),
% 			Noeud;
% 		[#annotation{}]  -> 
% 			[Etiquette]  = erl_syntax:get_ann(Noeud),
% 				% ?PVALUE("p", "Entro dentro", Etiquette),
% 			erl_syntax:set_ann(Noeud, [#annotation{
% 											id = Etiquette#annotation.id, 
% 											modify = Etat, 
% 											%payload = State#annotation.payload,
% 											bindings = Etiquette#annotation.bindings}]
% 										);
% 		[Env, Bound, Free] -> 
% 			{EnvElement, EnvValeur} = Env, 
% 			{BoundElement, BoundValeur} = Bound, 
% 			{FreeElement, FreeValeur} = Free,

% 				% ?PVALUE("p", "Entro en el otro", {erl_syntax:type(Noeud), Noeud}),
% 			erl_syntax:set_ann(Noeud, [#annotation{
% 											id = Compte, 
% 											modify = Etat, 
% 											%payload = erl_syntax:string(whatIS(erl_syntax:type(Noeud), Noeud)),
% 											bindings = [
% 												erl_syntax:tuple([erl_syntax:atom(EnvElement), erl_syntax:list([erl_syntax:string(X) || X <- EnvValeur])]),
% 												erl_syntax:tuple([erl_syntax:atom(BoundElement), erl_syntax:list([erl_syntax:string(X) || X <- BoundValeur])]),
% 												erl_syntax:tuple([erl_syntax:atom(FreeElement), erl_syntax:list([erl_syntax:string(X) || X <- FreeValeur])])
% 											]}]
% 										)
% 	end.

% 	whatIS(attribute, Noeud) ->
% 		erl_pp:attribute(erl_syntax:revert(Noeud));

% 	whatIS(function, Noeud) ->
% 		erl_pp:function(erl_syntax:revert(Noeud));

% 	whatIS(block_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(case_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(catch_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(cond_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(fun_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(if_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));
		
% 	whatIS(infix_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));	

% 	whatIS(map_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(match_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(named_fun_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(receive_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(record_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(record_index_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(try_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% 	whatIS(try_expr, Noeud) ->
% 		erl_pp:expr(erl_syntax:revert(Noeud));

% whatIS(Other, Noeud) ->
% 	erl_pp:expr(erl_syntax:revert(Noeud)).

% creerNouveauNoeud('eof_marker', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('attribute', Noeud) ->
% 	[EtiquetteNoeud] = erl_syntax:get_ann(Noeud),
% 	BuildName = case erl_syntax:attribute_name(Noeud) of
% 						'none' ->
% 							none;

% 						Name ->
% 							erl_syntax_lib:map(
% 								fun (Lcl_Name) ->
% 									[EtiquetteName] = erl_syntax:get_ann(Lcl_Name),
% 									etiquettes(Lcl_Name, EtiquetteName#annotation.id, false)
% 								end,
% 								Name
% 							)
% 					end,

% 	BuildArg = case erl_syntax:attribute_arguments(Noeud) of
% 						'none' ->
% 							none;
% 						Arg ->
% 							lists:map(
% 								fun (Term) ->
% 									[EtiquetteArg] = erl_syntax:get_ann(Term),
% 									etiquettes(Term, EtiquetteArg#annotation.id, false)
% 								end,
% 								Arg
% 							)
% 					end,

% 	BuildAttribute = erl_syntax:attribute(BuildName, BuildArg),
% 	NewEtiquette = erl_syntax:set_ann(BuildAttribute, [EtiquetteNoeud]),
% 	etiquettes(NewEtiquette, EtiquetteNoeud#annotation.id, false);	

% creerNouveauNoeud('function', Noeud) ->
% 	[EtiquetteNoeud] = erl_syntax:get_ann(Noeud),
% 	BuildName = case erl_syntax:function_name(Noeud) of
% 						'none' ->
% 							none;

% 						Name ->
% 							erl_syntax_lib:map(
% 								fun (Name) ->
% 									[EtiquetteName] = erl_syntax:get_ann(Name),
% 									etiquettes(Name, EtiquetteName#annotation.id, false)
% 								end,
% 								Name
% 							)
% 					end,

% 	BuildClauses = erl_syntax:function_clauses(Noeud),
% 	BuildFunction = erl_syntax:function(BuildName, BuildClauses),
% 	NewEtiquette = erl_syntax:set_ann(BuildFunction, [EtiquetteNoeud]),
% 	etiquettes(NewEtiquette, EtiquetteNoeud#annotation.id, false);

% creerNouveauNoeud('clause', Noeud) ->
% 	[EtiquetteNoeud] = erl_syntax:get_ann(Noeud),

% 	BuildGuard = case erl_syntax:clause_guard(Noeud) of
% 					'none' ->
% 						none;
% 					Guard ->
% 						erl_syntax_lib:map(
% 							fun (Term) ->
% 								[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 								etiquettes(Term, EtiquetteTerm, false)
% 							end,
% 							Guard
% 						 )
% 				end,

% 	BuildPattern = lists:map(
% 						fun (Term) ->
% 							[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 							etiquettes(Term, EtiquetteTerm, false)
% 						end,
% 						erl_syntax:clause_patterns(Noeud)
% 					),

% 	BuildBody = erl_syntax:clause_body(Noeud),

% 	NewBuildBody = create_ins_build_body(BuildBody),

% 	BuildClause = erl_syntax:clause(BuildPattern, BuildGuard, NewBuildBody),	
% 	NewEtiquette = erl_syntax:set_ann(BuildClause, [EtiquetteNoeud]),

% 	etiquettes(NewEtiquette, EtiquetteNoeud#annotation.id, false);	

% creerNouveauNoeud('binary_generator', Noeud) ->
% 	[EtiquetteNoeud] = erl_syntax:get_ann(Noeud),
% 	BuildBinaryPattern = case erl_syntax:binary_generator_pattern(Noeud) of
% 						'none' ->
% 							none;

% 						BinaryPattern ->
% 							erl_syntax_lib:map(
% 								fun (Term) ->
% 									[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 									etiquettes(Term, EtiquetteTerm#annotation.id, false)
% 								end,
% 								BinaryPattern
% 							)
% 					end,

% 	BuildBinaryBody = erl_syntax:binary_generator_body(Noeud),
% 	BuildBinaryGenerator = erl_syntax:binary_generator(BuildBinaryPattern, BuildBinaryBody),
% 	NewEtiquette = erl_syntax:set_ann(BuildBinaryGenerator, [EtiquetteNoeud]),
% 	etiquettes(NewEtiquette, EtiquetteNoeud#annotation.id, false);

% creerNouveauNoeud('generator', Noeud) ->
% 	[EtiquetteNoeud] = erl_syntax:get_ann(Noeud),
% 	BuildPattern = case erl_syntax:generator_pattern(Noeud) of
% 						'none' ->
% 							none;

% 						Pattern ->
% 							erl_syntax_lib:map(
% 								fun (Term) ->
% 									[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 									etiquettes(Term, EtiquetteTerm#annotation.id, false)
% 								end,
% 								Pattern
% 							)
% 					end,

% 	BuildBody = erl_syntax:generator_body(Noeud),
% 	BuildGenerator = erl_syntax:generator(BuildPattern, BuildBody),
% 	NewEtiquette = erl_syntax:set_ann(BuildGenerator, [EtiquetteNoeud]),
% 	etiquettes(NewEtiquette, EtiquetteNoeud#annotation.id, false);

% creerNouveauNoeud('match_expr', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	NouveauNoeud = erl_syntax:match_expr(
% 							erl_syntax:match_expr_pattern(Noeud), 
% 							erl_syntax:match_expr_body(Noeud)
% 						),
% 	NewEtiquette = erl_syntax:set_ann(NouveauNoeud, [Etiquette]),
% 	etiquettes(NewEtiquette, Etiquette#annotation.id, true);

% creerNouveauNoeud('operator', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('variable', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('atom', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('char', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('integer', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud('float', Noeud) ->
% 	[Etiquette] = erl_syntax:get_ann(Noeud),
% 	etiquettes(Noeud, Etiquette#annotation.id, false);

% creerNouveauNoeud(Other, Noeud) ->
% 	Noeud.

% %%%%%%%%%%%Send Proces
% buildSendProcess(Process, Msg) ->
% 	erl_syntax:application(
% 		erl_syntax:atom(erlang) , 
% 		erl_syntax:atom(send), 
% 			[
% 				erl_syntax:atom(Process),
% 		 		erl_syntax:tuple(Msg)
% 		 	]
% 		 ).

% buildCaseError(Value) ->
% 	% dbg_free_vars_server ! {newFreeVariable, self()},
% 	NewVariable = freeVariable(),
% 	erl_syntax:case_expr(
% 		 Value,
% 		[
% 		  createClause(error, [Value, NewVariable]),
% 		  createClause(throw, [Value, NewVariable]),
% 		  createClause(exit,  [Value, NewVariable]),
% 		  createClause(other, [Value, NewVariable])
% 		]).

% createClause(other, [Value, _]) ->
% 	erl_syntax:clause(
%   		[Value], 
%   		none, 
%   		[buildSendProcess(dbg_tracer, [
%   				erl_syntax:atom(newValueTerm), 
%   				Value]), Value]);

% createClause(error, [Value, NewVariable]) ->
% 	erl_syntax:clause([
% 		erl_syntax:tuple([
% 				erl_syntax:atom('ERROR'),  
% 				NewVariable])],
% 		none,
% 		[buildSendProcess(dbg_tracer, [Value])
% 		]);

% createClause(throw, [Value, NewVariable]) ->	
% 	erl_syntax:clause([
% 		erl_syntax:tuple([
% 				erl_syntax:atom('THROW'),  
% 				NewVariable])],
% 		none,
% 		[buildSendProcess(dbg_tracer, [Value])
% 		]);
	
% createClause(exit, [Value, NewVariable]) ->
% 	erl_syntax:clause([
% 		erl_syntax:tuple([
% 				erl_syntax:atom('EXIT'), 
% 				NewVariable])],
% 		none,
% 		[buildSendProcess(dbg_tracer, [Value])
% 		]);
	
% createClause(Other, [_]) ->
% 	erl_syntax:clause([
% 		none,
% 		none,
% 		none]).
% create_ins_build_body(BuildBody) ->
% 	FinalClause = lists:last(BuildBody),
% 	BodyOriSinUlt = lists:droplast(BuildBody),
% 	% ?PVALUE("p", "Nose", func_catching_body_f1(BodyOriSinUlt)),	
% 	% ?PVALUE("p", "Nose", func_catching_body_f2(FinalClause)),	
% 	func_catching_body_f1(BodyOriSinUlt) ++ [func_catching_body_f2(FinalClause)].
% func_send_begin(NameTracer , Type, Term, EtiquetteTerm) ->
% 	buildSendProcess(NameTracer, [
% 		erl_syntax:atom('newExpresion'),
% 		erl_syntax:list([
% 			erl_syntax:atom(Type),
% 			erl_syntax:integer(EtiquetteTerm#annotation.id),
% 			erl_syntax:string(EtiquetteTerm#annotation.payload),
% 			erl_syntax:list(EtiquetteTerm#annotation.bindings),
% 			erl_syntax:atom(erl_syntax:type(Term))
% 		])
% ]).

% func_catching_body_f1(Body) ->
% 	lists:map(
% 				fun (Term) ->
% 					[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 					case EtiquetteTerm#annotation.modify of 
% 						true ->
% 							NewFreeVariable = freeVariable(),
% 							BlockExpr =	erl_syntax:block_expr([	
% 												func_send_begin(dbg_tracer,'begin', Term, EtiquetteTerm),											
% 												erl_syntax:match_expr(
% 													NewFreeVariable, 
% 													erl_syntax:catch_expr(Term)
% 												),
												
% 												buildCaseError(NewFreeVariable),
										
% 												func_send_begin(dbg_tracer,'end', Term, EtiquetteTerm)
% 										]),
% 							BlockExpr;
% 						false ->
% 							Term;
% 						Error ->
% 							io:format("Error ni true ni false")
% 					end
% 				end,
% 				Body
% 			).

% func_catching_body_f2(Term) ->
% 					[EtiquetteTerm] = erl_syntax:get_ann(Term),
% 					case EtiquetteTerm#annotation.modify of 
% 						true ->
% 							NewFreeVariable = freeVariable(),
% 							BlockExpr =	erl_syntax:block_expr([	
% 												func_send_begin(dbg_tracer,'begin', Term, EtiquetteTerm), 											
												
% 												erl_syntax:match_expr(
% 													NewFreeVariable, 
% 													erl_syntax:catch_expr(Term)
% 												),
												
% 												buildCaseError(NewFreeVariable),
										
% 												func_send_begin(dbg_tracer,'end', Term, EtiquetteTerm),
% 												Term
% 										]),
% 							BlockExpr;
% 						false ->
% 							Term;
% 						Error ->
% 							io:format("Error ni true ni false")
% 					end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Annotate Bindings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotate_bindings_form(Form)->
	annotate_bindings_form(
		erl_syntax:type(Form), 
		Form).


% annotate_bindings_form(eof_marker, Form)->
% 	Form;
annotate_bindings_form(_, Form)->
	erl_syntax_lib:annotate_bindings(
		Form,
		ordsets:new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Complete Annotation
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
		lists:member(Type, annotated_types()),
	Node1 = 
		erl_syntax:set_ann(
			Node0, 
			[#annotation
				{
					id = CurrentNodeId,
					modify = Modify,
					bindings = Bindings
				}
			]),
	Node2 = 
		case lists:member(Type, expressions_with_patterns()) of 
			true -> 
				disable_modify_in_patterns(Type, Node1);
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
				[Ann#annotation{modify = false}]);
		_ ->
			io:format("Node without annotations: ~p\n", [Node]), 
			Node
	end.


expressions_with_patterns() ->
	% binary_generator should be also in this list, but it does not appear in the erl_syntax:type/1 listing
	[clause, generator, match_expr, binary_generator].

annotated_types() ->
	[
			application, binary, block_expr
		, 	case_expr, catch_expr, fun_expr
		, 	if_expr, infix_expr, list, list_comp
		, 	map_expr, match_expr, named_fun_expr
		, 	prefix_expr, receive_expr, record_access
		, 	record_expr, try_expr, tuple
	].

not_annotated_types() ->
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
	% catch unregister(dbg_server),
	% catch unregister(dbg_tracer),
	catch unregister(dbg_free_vars_server).

register_servers() ->
	% register(dbg_server, spawn(dbg_server, init, [])),
	% register(dbg_tracer, spawn(dbg_tracer, init, [])),
	register(
		dbg_free_vars_server, 
		spawn(dbg_free_vars_server, init, [])).

get_free_variable() ->
	dbg_free_vars_server ! {get_free_variable, self()},
	receive 
		Value ->
			erl_syntax:variable(Value)
	end.


inst_anno_form(Forms) ->
	erl_syntax_lib:map(
					fun shuttle_inst_anno_form/1,
					Forms
				).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%		Fase instrumentacion	%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
shuttle_inst_anno_form(Form) ->
	[Anno] = 
		erl_syntax:get_ann(Form),
	
	InstNodeCatchClauseBody = 
		case erl_syntax:type(Form) of
			'clause' -> 
				inst_catching_last_term(Form);
			_ ->
				Form
		end,

	InstNodeCatchClausePattern = 
		case lists:member(
				erl_syntax:type(InstNodeCatchClauseBody), 
				expressions_with_patterns()) of
			true ->
				inst_pattern_clause(InstNodeCatchClauseBody);
			
			false->
				InstNodeCatchClauseBody
		end,

	InstNodeCatchClause = 
		case Anno#annotation.modify of
			true ->
				inst_catching_term(InstNodeCatchClausePattern);
			false ->
				InstNodeCatchClausePattern
		end.

%%Anotar cualquier termino
inst_catching_term(Term) ->
	[Annotation] = 
		erl_syntax:get_ann(Term),
	FreeVariable = 
		get_free_variable(),
	erl_syntax:block_expr([	
		func_send_begin(
			dbg_tracer,
			'begin', 
			Term, 
			Annotation),	
		erl_syntax:match_expr(
			FreeVariable, 
			erl_syntax:catch_expr(Term)),
		build_case_Error(FreeVariable),
		func_send_begin(
			dbg_tracer,
			'end', 
			Term, 
			Annotation)]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Anota el ultimo termino del clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inst_clause_body(Node) ->
	erl_syntax:set_ann(		
		erl_syntax:clause(
			erl_syntax:clause_patterns(Node), 
			erl_syntax:clause_guard(Node), 
			inst_catching_last_term(
				erl_syntax:clause_body(Node))),
			[erl_syntax:get_ann(Node)]);

inst_catching_last_term(ClauseBody) ->
	BodyLast = 
		lists:last(ClauseBody),
	
	BodyRemainder = 
		lists:droplast(ClauseBody),
	
	BodyRemainder ++ [
		inst_clause_body_last(BodyRemainder)].

inst_clause_body_last(Term) ->
	[Annotation] = 
		erl_syntax:get_ann(Term),
	FreeVariable = 
		get_free_variable(),
	erl_syntax:block_expr([	
		func_send_begin(
			dbg_tracer,
			'begin', 
			Term, 
			Annotation), 											
												
	erl_syntax:match_expr(
		FreeVariable, 
		erl_syntax:catch_expr(Term)),
		build_case_error(FreeVariable),
		func_send_begin(
			dbg_tracer,
			'end', 
			Term, 
			Annotation),
		Term]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Send MSG process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
func_send_begin(Process , TypeMsg, Term, Annotation) ->
	build_send_process(Process, [
		erl_syntax:atom('newExpresion'),
		erl_syntax:list([
			erl_syntax:atom(TypeMsg),
			erl_syntax:integer(Annotation#annotation.id),
			erl_syntax:string(Annotation#annotation.payload),
			erl_syntax:list(Annotation#annotation.bindings),
			erl_syntax:atom(erl_syntax:type(Term))])]).

build_send_process(Process, Msg) ->
	erl_syntax:application(
		erl_syntax:atom(erlang) , 
		erl_syntax:atom(send), 
			[
				erl_syntax:atom(Process),
		 		erl_syntax:tuple(Msg)
		 	]
		 ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_case_error(Term) ->
	% dbg_free_vars_server ! {newFreeVariable, self()},
	FreeVariable = 
		get_free_variable(),
	erl_syntax:case_expr(
		 Term,
		[ build_error(error, [Term, FreeVariable]),
		  build_error(throw, [Term, FreeVariable]),
		  build_error(exit,  [Term, FreeVariable]),
		  build_error(other, [Term, FreeVariable])]).


build_error(other, [Value, _]) ->
	erl_syntax:clause(
  		[Value], 
  		none, 
  		[buildSendProcess(
  			dbg_tracer, 
  			[erl_syntax:atom(newValueTerm), 
  			Value]), 
  		Value]);

build_error(error, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
			erl_syntax:atom('ERROR'),  
			NewVariable])],
		none,
		[buildSendProcess(
			dbg_tracer, 
			[Value])]);

build_error(throw, [Value, NewVariable]) ->	
	erl_syntax:clause([
		erl_syntax:tuple([
			erl_syntax:atom('THROW'),  
			NewVariable])],
		none,
		[buildSendProcess(
			dbg_tracer, 
			[Value])]);
	
build_error(exit, [Value, NewVariable]) ->
	erl_syntax:clause([
		erl_syntax:tuple([
			erl_syntax:atom('EXIT'), 
			NewVariable])],
		none,
		[buildSendProcess(
			dbg_tracer, 
			[Value])]);
	
build_error(Other, [_]) ->
	erl_syntax:clause([
		none,
		none,
		none]).



  
   
  
 
  
     
         
    
       
        
 

