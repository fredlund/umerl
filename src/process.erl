-module(process).

-export([start/1,start/2]).

-include("records.hrl").

-record(machine,
	{
	  module :: atom(),
	  pid :: pid(),
	  state :: atom(),
	  mailbox :: [any()],
	  wants_permission=void :: 'void' | permission(),
	  permissions :: [permission()]
	}).

-record(process,
	{
	  machines=[] :: [{pid(),#machine{}}],
	  memory
	}).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

-spec start([{atom(),any()}]) -> any().
start(MachineSpecs) ->
  start(MachineSpecs,fun (_) -> ok end).

-spec start([{atom(),any()}],fun((context())->any())) -> any().
start(MachineSpecs,InitVars) ->
  Memory = ets:new(private,[public]),
  InitVars({in_process,{Memory,self()}}),
  Self = self(),
  Machines =
    lists:foldl
      (fun ({Module,Init},Acc) ->
	   MachinePid =
	     spawn_link
	       (fun () ->
		    machine:start(Module,Init,Self,Memory)
		end),
	   add_machine
	     (#machine
	      {pid=MachinePid,
	       module=Module,
	       mailbox=[],
	       permissions=[read]},
	      Acc)
       end,
       [],
       MachineSpecs),
  loop
    (#process
     {machines=Machines,
      memory=Memory}).

-spec loop(#process{}) -> no_return().
loop(PermissionsState) ->
  ?LOG("~p: loop(~p)~n",[symbolic_name(self()),PermissionsState]),
  State = check_permissions(PermissionsState),
  receive
    RawMsg -> 
      ?LOG("~p: message ~p received~n",[symbolic_name(self()),RawMsg]),
      case RawMsg of

	{message,Msg} ->
	  ?LOG("~p: got a message ~p~n",[symbolic_name(self()),Msg]),
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
	  Machine = get_machine(MachinePid,State),
	  case has_permission(Machine#machine.permissions,Type) of
	    true ->
	      MachinePid!ok,
	      loop
		(replace_machine
		   (Machine#machine
		    {permissions=[],
		     wants_permission=void},
		    State));
	    false ->
	      loop
		(replace_machine
		 (Machine#machine{wants_permission=Type},State))
	  end;

	{transitions,{MachinePid,StateName,DataState,Doer}} ->
	  Machine = get_machine(MachinePid,State),
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
		   try check_guard(Guard, data, DataState, State) of
		     {true,GuardAction} ->
		       [{GuardAction,ReadTransition,Machine}|Enabled];
		     false ->
		       Enabled
		   catch Class:Reason ->
		       Stacktrace = erlang:get_stacktrace(),
		       io:format
			 ("~n*** Error: ~p: evaluation of data guard ~p~n"
			  ++"for machine ~p (~p) in"
			  ++" machine state ~p with process state~n  ~p"
			  ++"~nwith data~n  ~p~nraises exception ~p:~p~n"
			  ++"~nStacktrace:~n~p~n~n",
			  [symbolic_name(self()),
			   Guard,
			   Machine#machine.module,
			   Machine#machine.pid,
			   StateName,
			   State,
			   DataState,
			   Class,
			   Reason,
			   Stacktrace]),
		       erlang:raise(Class,Reason,Stacktrace)
		   end
	       end, [], ReadTransitions),
	  EnabledReceives =
	    find_enabled_receives(ReceiveTransitions,DataState,Machine,State,StateName,Machine),
	  case pickTransition(EnabledReads++EnabledReceives) of
	    {ok,{GuardAction,ChosenTransition,NewMachine}} ->
	      ?LOG("transition choosen is~n~p~n",[ChosenTransition]),
	      put(var_write,false),
	      {MachineData,NewMailbox} =
		run_guard_action
		  (GuardAction,
		   DataState,
		   StateName,
		   ChosenTransition#transition.next_state,
		   NewMachine,
		   State),
	      NewerMachine =
		case get(var_write) of
		  true ->
		    case not(lists:member(read,NewMachine#machine.permissions)) of
		      true ->
			NewMachine#machine{permissions=[read|NewMachine#machine.permissions]};
		      false -> NewMachine
		    end;
		  false -> NewMachine
		end,
	      if
		not(ChosenTransition#transition.is_internal),
		Doer=/=void ->
		  exit(Doer,kill);
		true ->
		  ok
	      end,
	      MachinePid!
		{state,ChosenTransition#transition.next_state,MachineData},
	      loop
		(replace_machine
		   (NewerMachine#machine
		    {permissions=[read,'receive'],
		     mailbox=NewMailbox},
		    State));

	    _ ->
	      MachinePid!none,
	      loop
		(replace_machine
		 (Machine#machine{permissions=[]},State))
	  end;

	{write,MachinePid,Var,Value} ->
	  ?LOG
	    ("~p: machine ~p wrote ~p to variable ~p~n",
	     [symbolic_name(self()),symbolic_name(MachinePid),Value,Var]),
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
	  io:format
	    ("*** ~p: warning: received strange message~n~p~n",
	     [symbolic_name(self()),RawMsg]),
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
		       [symbolic_name(self()),symbolic_name(Machine#machine.pid)]),
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

find_enabled_receives([],_DataState,_Machine,_State,_StateName,_Machine) -> [];
find_enabled_receives(Transitions,DataState,Machine,State,StateName,Machine) -> 
  try_receive_msgs(Machine#machine.mailbox,[],Transitions,DataState,Machine,State,StateName,Machine).

try_receive_msgs([],_Seen,_Transitions,_DataState,_Machine,_State,_StateName,_Machine) ->  [];
try_receive_msgs([Msg|Rest],Seen,Transitions,DataState,Machine,State,StateName,Machine) -> 
  case try_receive_msg(Msg,Transitions,DataState,State,StateName,Machine) of
    [] ->
      try_receive_msgs(Rest,[Msg|Seen],Transitions,DataState,Machine,State,StateName,Machine);
    Results ->
      NewMailbox = lists:reverse(Seen,Rest),
      NewMachine = Machine#machine{mailbox=NewMailbox},
      lists:map
	(fun ({GuardAction,Transition}) -> {GuardAction,Transition,NewMachine} end,
	 Results)
  end.

try_receive_msg(Msg,Transitions,DataState,State,StateName,Machine) ->
  lists:foldl
    (fun (Transition,Collected) ->
	 Guard = Transition#transition.guard,
	 try check_guard(Guard,{msg,Msg}, DataState, State) of
	   false -> Collected;
	   {true,GuardAction} ->
	     ?LOG
		("~p: message ~p is receivable by~n~p~nin state~n~p~n",
		 [symbolic_name(self()),Msg,Transition,DataState]),
	     [{GuardAction,Transition}|Collected]
	 catch Class:Reason ->
	     Stacktrace = erlang:get_stacktrace(),
	     io:format
	       ("~n*** Error: ~p: evaluation of data guard ~p~n"
		++" on message ~p~n"
		++"for machine ~p (~p) in"
		++" machine state ~p with process state~n  ~p"
		++"~nwith data~n  ~p~nraises exception ~p:~p~n"
		++"~nStacktrace:~n~p~n~n",
		[symbolic_name(self()),
		 Guard,
		 Msg,
		 Machine#machine.module,
		 Machine#machine.pid,
		 StateName,
		 State,
		 DataState,
		 Class,
		 Reason,
		 Stacktrace]),
	     erlang:raise(Class,Reason,Stacktrace)
	 end
     end, [], Transitions).

check_guard(Guard, {msg,Msg}, DataState, State) ->
  Guard(Msg, {in_process,{State#process.memory,self()}}, DataState);
check_guard(Guard, _, DataState, State) ->
  Guard({in_process,{State#process.memory,self()}}, DataState).

run_guard_action(GuardAction,DataState,FromState,ToState,Machine,State) ->
  ?LOG("~p: running guard action~n",[symbolic_name(self())]),
  Mailbox = Machine#machine.mailbox,
  NewMailbox =
    if
      FromState =/=  ToState ->
	NewState = (Machine#machine.module):state(ToState),
	Defer = NewState#uml_state.defer,
	if
	  Defer == all -> Mailbox;
	  Defer == none -> [];
	  true ->
	    lists:filter
	      (fun (Msg) -> 
		   try Defer(Msg,DataState,{in_process,{State#process.memory,self()}})
		   catch Class:Reason ->
		       Stacktrace = erlang:get_stacktrace(),
		       io:format
			 ("~n*** Error: ~p: evaluation of defer function ~p~n"
			  ++"for machine ~p (~p) in"
			  ++" transition from machine state ~p to machine state ~p with process state~n  ~p"
			  ++"~nwith data~n  ~p~nraises exception ~p:~p~n"
			  ++"~nStacktrace:~n~p~n~n",
			  [symbolic_name(self()),
			   GuardAction,
			   Machine#machine.module,
			   Machine#machine.pid,
			   FromState,
			   ToState,
			   State,
			   DataState,
			   Class,
			   Reason,
			   Stacktrace]),
		       erlang:raise(Class,Reason,Stacktrace)
		   end
	       end, Mailbox)
	end;
      true -> Mailbox
  end,
    try GuardAction(DataState) of
	NewDataState -> {NewDataState,NewMailbox}
    catch Class:Reason ->
	Stacktrace = erlang:get_stacktrace(),
	io:format
	  ("~n*** Error: ~p: evaluation of guard action ~p~n"
	   ++"for machine ~p (~p) in"
	   ++" transition from machine state ~p to machine state ~p with process state~n  ~p"
	   ++"~nwith data~n  ~p~nraises exception ~p:~p~n"
	   ++"~nStacktrace:~n~p~n~n",
	   [symbolic_name(self()),
	    GuardAction,
	    Machine#machine.module,
	    Machine#machine.pid,
	    FromState,
	    ToState,
	    State,
	    DataState,
	    Class,
	    Reason,
	    Stacktrace]),
	erlang:raise(Class,Reason,Stacktrace)
    end.

pickTransition(Transitions=[_|_]) ->
  Length = length(Transitions),
  {ok, lists:nth(random:uniform(Length),Transitions)};
pickTransition(_) ->
  no.

replace_machine(NewMachine,State) ->
  MachinePid = 
    NewMachine#machine.pid,
  State#process
    {machines=
       lists:keyreplace
	 (MachinePid,1,State#process.machines,{MachinePid,NewMachine})}.

get_machine(MachinePid,State) ->
  {value,{_,Machine}} = lists:keysearch(MachinePid,1,State#process.machines),
  Machine.

add_machine(Machine,Machines) ->  
  [{Machine#machine.pid,Machine}|Machines].

symbolic_name(Pid) when is_pid(Pid) ->
  case process_info(Pid,registered_name) of
    {registered_name,Name} -> Name;
    _ -> Pid
  end.



      
  
