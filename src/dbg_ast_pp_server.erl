-module(dbg_ast_pp_server). 

-export([init/0]).

-record(
	state,
	{
		ast = none
	}).
% TODO: Should be in a common library not replaced
-record(
	annotation, 
	{	
		id = 0, 
		modify = undefined,  
		bindings = undefined
	}).

init() ->
	loop(#state{}).

loop(State) ->
	receive
		exit ->
			ok;
		{store_ast, AST} ->
			loop(State#state{ast = AST});
		{get_pp_node, PidAnswer, NodeId} ->
			PP = 
				lists:foldl(
					fun(Form, AccForms) -> 
						erl_syntax_lib:fold(
							fun(CNode, Acc) -> 
								get_pp_node(CNode, NodeId, Acc) 
							end,
							AccForms,
							Form)
					end,
					"",
					State#state.ast),
			PidAnswer!PP,
			loop(State)
	end.


get_pp_node(CNode, SearchedNodeId, "") -> 
	[Annotation] = 
		erl_syntax:get_ann(CNode),
	case Annotation#annotation.id of 
		SearchedNodeId -> 
			erl_prettypr:format(CNode);
		_ -> 
			""
	end;
get_pp_node(_, _, Acc) -> 
	Acc.


