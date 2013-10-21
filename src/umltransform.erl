-module(umltransform).

-compile(export_all).

%% We should collect all states, and for each state record the entry
%% and exit functions.
%%

parse_transform(AST, _Options) ->
  Map = entexit_states(AST),
  io:format("~nstates:~n~p~n",[Map]),
  io:format("~ntransitions to change:~n~p~n",[change_transitions(AST,Map)]),
  AST.

change_transitions(AST,Map) ->
  lists:map
    (fun ({function,L1,state,L2,Clauses}) ->
	 {function,L1,state,L2,
	 lists:map
	   (fun (Cl={clause,L1,A=[{atom,_,Name}],L2,[{record,L3,uml_state,Items}]}) ->
		case item_in_record(transitions,Items,void) of
		  void -> Cl;
		  TransitionList ->
		    {clause,L1,A,L2,
		     [{record,L3,uml_state,
		       rewrite_transitions(Name,TransitionList,Map)}]}
		end;
		(Other) -> Other
	    end, Clauses)};
	 (Other) -> Other
     end, AST).

rewrite_transitions(_Name,L={nil,_},_Map) ->
  L;
rewrite_transitions(Name,{cons,L1,First,Rest},Map) ->
  {cons,
   L1,
   rewrite_transition(Name,First,Map),
   rewrite_transitions(Name,Rest,Map)}.

rewrite_transition(FromState,T={record,L1,transition,Items},Map) ->
  IsExternal = not_is_internal(item_in_record(internal_state,Items,void)),
  {atom,_,ToState} = item_in_record(next_state,Items,void),
  Fun = item_in_record(guard,Items,void),
  {'fun',L2,{clauses,Clauses}} = Fun,
  OtherRecords = other_records(guard,Items),
  {record,L1,transition,
   [{'fun',L2,{clauses,lists:map(fun (Clause) -> rewrite_clause(IsExternal,Clause,FromState,ToState,Map) end, Clauses)}}|OtherRecords]}.

rewrite_clause(IsExternal,Clause,FromState,ToState,Map) ->
  {clause,L2,P1,P2,Code} = Clause,
  {clause,L2,P1,P2,
  if
    IsExternal ->
      {value,{_,{_,Exiting}}} = lists:keysearch(FromState,1,Map),
      {value,{_,{Entering,_}}} = lists:keysearch(ToState,1,Map),
      if
	Entering=/=void, Exiting=/=void ->
	  compose_code(false,compose_code(false,Code,Exiting),Entering);
	Entering=/=void ->
	  compose_code(true,Code,Entering);
	Exiting=/=void ->
	  compose_code(false,Code,Exiting);
	true ->
	  Code
      end;
    true -> Code
  end}.

not_is_internal({atom,_,true}) ->
  false;
not_is_internal(_) ->
  true.

entexit_states(AST) ->
  lists:foldl
    (fun ({function,_,state,_,Clauses},Acc) ->
	 lists:map
	   (fun ({clause,_,[{atom,_,Name}],_,[{record,_,uml_state,Items}]}) ->
		{Name,
		 {item_in_record(entry,Items,void),
		  item_in_record(exit,Items,void)}}
	    end,
	    Clauses)++
	   Acc;
	 (_,Acc) ->
	 Acc
     end, [], AST).

item_in_record(_Name,[],Default) ->
  Default;
item_in_record(Name,[{record_field,_,{atom,_,Name},Item}|_],_Default) ->
  Item;
item_in_record(Name,[_|Rest],Default) ->
  item_in_record(Name,Rest,Default).

other_records(Name,[]) ->
  [];
other_records(Name,[{record_field,_,{atom,_,Name},Item}|Rest]) ->
  Rest;
other_records(Name,[First|Rest]) ->
  [First|other_records(Name,Rest)].

%% entry:
%% for each clause (with same clause variables):
%% case eval(code) of
%%   {true,F} -> {true,fun (X) -> Entry(F(X)) end};
%%   Other -> Other
%% end.

%% exit:
%% for each clause (with same clause variables):
%% case eval(code) of
%%   {true,F} -> {true,fun (X) -> F(Exit(X)) end};
%%   Other -> Other
%% end.

%% so, compose(Entry,F,X) or compose(F,Exit,X) or
%% compose(Entry,compose(F,Exit,X)).
%%  

compose_code(Pre,Code,OtherFun) ->
  {'case',46,
   Code,
   [{clause,47,
     [{tuple,47,[{atom,47,true},{var,47,'___F'}]}],
     [],
     [{tuple,48,
       [{atom,48,true},
	{'fun',49,
	 {clauses,
	  [{clause,49,
	    [{var,49,'___X'}],
	    [],
	    if
	      Pre ->
		[{call,50,
		  OtherFun,
		  [{call,50,
		    {var,50,'___F'},
		    [{var,50,'___X'}]}]}];
	      true ->
		[{call,50,
		  {var,50,'___F'},
		  [{call,50,
		    OtherFun,
		    [{var,50,'___X'}]}]}]
	    end}]}}]}]},
    {clause,52,
     [{var,52,'___Other'}],
     [],
     [{var,52,'___Other'}]}]}.

