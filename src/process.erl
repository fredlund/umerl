-module(process).

-compile(export_all).

-include("records.hrl").

-record(process,
	{machines=[],
	 memory}).

-record(machine,
	{
	  module,
	  pid,
	  mailbox,
	  wants_permission=void,
	  permissions
	}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

start(MachineSpecs) ->
  start(MachineSpecs,fun (_) -> ok end).

start(MachineSpecs,InitVars) ->
  Memory = ets:new(private,[public]),
  InitVars({in_process,Memory}),
  Self = self(),
  Machines =
    lists:map
      (fun ({Module,Init}) ->
	   MachinePid =
	     spawn_link(fun () -> machine:start(Module,Init,Self,Memory) end),
	   {MachinePid,
	    #machine
	    {pid=MachinePid,
	     module=Module,
	     mailbox=[],
	     permissions=[read]}}
       end,
       MachineSpecs),
  loop
    (#process
     {machines=Machines,
      memory=Memory}).

loop(PermissionsState) ->
  ?LOG("~p: loop(~p)~n",[self(),PermissionsState]),
  State = check_permissions(PermissionsState),
  receive
    RawMsg -> 
      ?LOG("~p: message ~p received~n",[self(),RawMsg]),
      case RawMsg of

	{message,Msg} ->
	  ?LOG("got a message ~p~n",[Msg]),
	  loop
	    (State#process
	     {machines=
		lists:map
		  (fun ({MachinePid,Machine}) ->
		       {MachinePid,
			Machine#machine
			{mailbox=Machine#machine.mailbox++[Msg],
			 permissions=add_permission('receive',Machine#machine.permissions)}}
		   end, State#process.machines)});

	{ask_transitions,{MachinePid,Type}} ->
	  {value,{_,Machine}} =
	    lists:keysearch(MachinePid,1,State#process.machines),
	  case has_permission(Machine#machine.permissions,Type) of
	    true ->
	      MachinePid!ok,
	      loop
		(State#process
		 {machines=
		    lists:keyreplace
		      (MachinePid,1,State#process.machines,
		       {MachinePid,Machine#machine
			{permissions=[],
			 wants_permission=void}})});
	    false ->
	      loop
		(State#process
		 {machines=
		    lists:keyreplace
		      (MachinePid,1,State#process.machines,
		       {MachinePid,Machine#machine{wants_permission=Type}})})
	  end;

	{transitions,{MachinePid,StateName,DataState}} ->
	  {value,{_,Machine}} =
	    lists:keysearch(MachinePid,1,State#process.machines),
	  UMLState =
	    (Machine#machine.module):state(StateName),
	  Transitions =
	    UMLState#uml_state.transitions,
	  {ReadTransitions, ReceiveTransitions} =
	    classify_transitions(Transitions),
	  EnabledReads =
	    lists:foldl
	      (fun (ReadTransition,Enabled) ->
		   Guard = ReadTransition#transition.guard,
		   case Guard({in_process,State#process.memory}, DataState) of
		     {true,GuardAction} ->
		       [{GuardAction,ReadTransition,Machine}|Enabled];
		     false ->
		       Enabled
		   end
	       end, [], ReadTransitions),
	  EnabledReceives =
	    find_enabled_receives(ReceiveTransitions,DataState,Machine,State),
	  case pickTransition(EnabledReads++EnabledReceives) of
	    {ok,{GuardAction,ChosenTransition,NewMachine}} ->
	      ?LOG("transition choosen is~n~p~n",[ChosenTransition]),
	      MachineData = run_guard_action(GuardAction,DataState),
	      MachinePid!{state,ChosenTransition#transition.next_state,MachineData},
	      loop
		(State#process
		 {machines=
		    lists:keyreplace
		      (MachinePid,1,State#process.machines,
		       {MachinePid,
			NewMachine#machine
			{permissions=[read,'receive']}})});

	    _ ->
	      MachinePid!none,
	      loop
		(State#process
		 {machines=
		    lists:keyreplace
		      (MachinePid,1,State#process.machines,
		       {MachinePid,Machine#machine
			{permissions=[]}})})
	  end;

	{do,{_MachinePid,_StateName,_DataState}} ->
	  ?LOG("do not handled yet~n",[]),
	  loop(State);

	{write,MachinePid,Var,Value} ->
	  ?LOG
	    ("~p: machine ~p wrote ~p to variable ~p~n",
	     [self(),MachinePid,Value,Var]),
	  ets:insert(State#process.memory,{Var,Value}),
	  loop
	    (State#process
	     {machines=
		lists:map
		  (fun ({_,Machine}) ->
		       {MachinePid,
			Machine#machine
			{permissions=add_permission(read,Machine#machine.permissions)}}
		   end, State#process.machines)});
	  
	_ ->
	  ?LOG
	    ("*** ~p: warning: received strange message~n~p~n",
	     [self(),RawMsg]),
	  loop(State)
      end
  end.

check_permissions(State) ->
  NewMachines = 
    lists:map
      (fun ({MachinePid,Machine}) ->
	   if
	     Machine#machine.wants_permission =/= void ->
	       case has_permission(Machine#machine.permissions,
				   Machine#machine.wants_permission) of
		 true ->
		   ?LOG
		      ("~p: giving permission to ~p~n",
		       [self(),Machine#machine.pid]),
		   Machine#machine.pid!ok,
		   {MachinePid,Machine#machine{wants_permission=void}};
		 false ->
		   {MachinePid,Machine}
	       end;
	     true -> {MachinePid,Machine}
	   end
       end, State#process.machines),
  State#process{machines=NewMachines}.

has_permission([],_) ->
  false;
has_permission([Permission|_],Permission) ->
  true;
has_permission([read|_],receive_read) ->
  true;
has_permission(['receive'|_],receive_read) ->
  true;
has_permission([_|Rest],Permission) ->
  has_permission(Rest,Permission).

add_permission(Permission,[]) ->
  [Permission];
add_permission(Permission,Permissions=[Permission|_]) ->
  Permissions;
add_permission(Permission,[First|Rest]) ->
  [First|add_permission(Permission,Rest)].

classify_transitions(Transitions) ->
  lists:foldl
    (fun (Transition,{Reads,Receives}) ->
	 if
	   Transition#transition.type==void ->
	     io:format
	       ("~n*** Error: transition ~p has type void~n",
		[Transition]),
	     throw(baddata);
	   Transition#transition.type==read ->
	     {[Transition|Reads],Receives};
	   true ->
	     {Reads,[Transition|Receives]}
	 end
     end, {[],[]}, Transitions).

find_enabled_receives([],_DataState,_Machine,_State) -> [];
find_enabled_receives(Transitions,DataState,Machine,State) -> 
  try_receive_msgs(Machine#machine.mailbox,[],Transitions,DataState,Machine,State).

try_receive_msgs([],_Seen,_Transitions,_DataState,_Machine,_State) ->  [];
try_receive_msgs([Msg|Rest],Seen,Transitions,DataState,Machine,State) -> 
  case try_receive_msg(Msg,Transitions,DataState,State) of
    [] ->
      try_receive_msgs(Rest,[Msg|Seen],Transitions,DataState,Machine,State);
    Results ->
      NewMailbox = lists:reverse(Seen,Rest),
      NewMachine = Machine#machine{mailbox=NewMailbox},
      lists:map
	(fun ({GuardAction,Transition}) -> {GuardAction,Transition,NewMachine} end,
	 Results)
  end.

try_receive_msg(Msg,Transitions,DataState,State) ->
  lists:foldl
    (fun (Transition,Collected) ->
	 Guard = Transition#transition.guard,
	 case Guard(Msg, {in_process,State#process.memory}, DataState) of
	   false -> Collected;
	   {true,GuardAction} ->
	     ?LOG
		("message ~p is receivable by~n~p~nin state~n~p~n",
		 [Msg,Transition,DataState]),
	     [{GuardAction,Transition}|Collected]
	 end
     end, [], Transitions).

run_guard_action(GuardAction,DataState) ->
  ?LOG("running guard action~n",[]),
  GuardAction(DataState).

pickTransition(Transitions=[_|_]) ->
  Length = length(Transitions),
  {ok, lists:nth(random:uniform(Length),Transitions)};
pickTransition(_) ->
  no.





      
  
