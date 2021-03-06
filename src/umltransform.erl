%% Copyright (c) 2013, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(umltransform).

-include("records.hrl").

-compile(export_all).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

parse_transform(AST, _Options) ->
  Map = entexit_states(AST),
  ?LOG("~nstates:~n~p~n",[Map]),
  ?LOG("~nold AST:~n~p~n",[AST]),
  NewAST = change_transitions(AST,Map),
  ?LOG("~nnew AST:~n~p~n",[NewAST]),
  NewAST.

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
		       [{record_field,L1,{atom,L3,transitions},
			 rewrite_transitions(Name,TransitionList,Map)}|
			other_records(transitions,Items)]}]}
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
  case Fun of
    {'fun',L2,{clauses,Clauses}} ->
      OtherRecords = other_records(guard,Items),
      {record,L1,transition,
       [{record_field,L1,{atom,L1,guard},
	 {'fun',L2,{clauses,lists:map(fun (Clause) -> rewrite_clause(IsExternal,Clause,FromState,ToState,Map) end, Clauses)}}}|OtherRecords]};
    Other ->
      io:format
	("*** Error: unexpected transition format~n~p~n",
	 [Fun]),
      throw(bad)
  end.

rewrite_clause(IsExternal,Clause,FromState,ToState,Map) ->
  {clause,L2,P1,P2,Code} = Clause,
  RawProcessTerm = 
    case length(P1) of
      2 -> lists:nth(1,P1);
      3 -> lists:nth(2,P1)
    end,
  ProcessTerm = 
    case RawProcessTerm of
      {'var',_,'_'} ->
	{atom,L2,void};
      Other ->
	Other
    end,
  ?LOG("ProcessTerm is ~p~n",[ProcessTerm]),
  NewCode =
    if
      IsExternal ->
	{_,Exiting} = find_state(FromState,Map),
	{Entering,_} = find_state(ToState,Map),
	if
	  Entering=/=void, Exiting=/=void ->
	    compose_code(false,compose_code(true,Code,Exiting,ProcessTerm),Entering,ProcessTerm);
	  Entering=/=void ->
	    compose_code(false,Code,Entering,ProcessTerm);
	  Exiting=/=void ->
	    compose_code(true,Code,Exiting,ProcessTerm);
	  true ->
	    Code
	end;
      true -> Code
    end,
  {clause,L2,P1,P2,NewCode}.

find_state(StateName,Map) ->
  case lists:keysearch(StateName,1,Map) of
    {value,{_,Res}} ->
      Res;
    _ ->
      io:format
	("*** Error: state ~p not defined~n",[StateName]),
      throw(bad)
  end.

not_is_internal({atom,_,true}) ->
  false;
not_is_internal(_) ->
  true.

entexit_states(AST) ->
  lists:foldl
    (fun ({function,_,state,_,Clauses},Acc) ->
	 lists:map
	   (fun ({clause,_,[{atom,_,Name}],_,[{record,_,uml_state,Items}]}) ->
		case item_in_record(name,Items,void) of
		  {atom,_,Name2} when Name=/=Name2 ->
		    io:format
		      ("*** Error: state clause defines a state named ~p"++
			 " but the record defines a state named ~p~n",
		       [Name,Name2]),
		    throw(bad);
		  {atom,_,Name} -> 
		    ok;
		  _ ->
		    io:format
		      ("*** Error: state clause defines a state named ~p"++
			 " but the record does not provide a name~n",
		       [Name]),
		    throw(bad)
		end,
		{Name,
		 {item_in_record(entry,Items,void),
		  item_in_record(exit,Items,void)}};
		(_) -> Acc
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
%%
%% case Code of
%%   {true,F} ->
%%     {true,
%%        fun (X) -> OtherFun(process,F(X)) end}};
%%   Other -> Other
%% end
%%          
%%  

compose_code(Pre,Code,OtherFun,ProcessTerm) ->
  ?LOG("Code is~n~p~nOtherFun=~n~p~n",[Code,OtherFun]),
  [
   {'case',46,
    {block,46,Code},
    [
     {clause,47,
      [{tuple,47,[{atom,47,true},{var,47,'___ActionFun'}]}],
      [],
      [{tuple,48,
	[{atom,48,true},
	 {'fun',49,
	  {clauses,
	   [{clause,49,
	     [{var,49,'___DataState'}],
	     [],
	     if
	       Pre ->
		 [{call,50,
		   {var,50,'___ActionFun'},
		   [{call,50,
		     OtherFun,
		     [ProcessTerm,{var,50,'___DataState'}]}]}];
	       true ->
		 [{call,50,
		   OtherFun,
		   [ProcessTerm,
		    {call,50,
		     {var,50,'___ActionFun'},
		     [{var,50,'___DataState'}]}]}]
	     end}]}}]}]},

     {clause,52,
      [{var,52,'___Other'}],
      [],
      [{var,52,'___Other'}]}
    ]}
  ].


  

