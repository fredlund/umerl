-module(process).

-export([start/1,start/2]).
-export([run_machines/1,do_read/1,run_transition/2]).

-include("records.hrl").

-ifdef(McErlang).
-define(CHOOSE(L),mce_erl:choice(L)).
-else.
-define(CHOOSE(L),choose(L)).
-endif.

-record(machine,
	{
	  module :: atom(),
	  id :: integer(),
	  mailbox :: [any()],
	  uml_state_name :: atom(),
	  data_state :: any(),
	  doer=void :: 'void' | pid()
	}).

-record(process,
	{
	  machines=[] :: [{integer(),#machine{}}],
	  changed=true :: boolean(),
	  memory :: any()
	}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

-spec start([{atom(),any()}]) -> no_return().
start(MachineSpecs) ->
  start(MachineSpecs,fun (_) -> ok end).

-spec start([{atom(),any()}],fun((context())->any())) -> no_return().
start(MachineSpecs,InitVars) ->
  Memory = ets:new(private,[public]),
  InitVars({in_process,{Memory,self()}}),
  {_,Machines} =
    lists:foldl
      (fun ({Module,Init},{N,Acc}) ->
	   PreMachine = #machine{module=Module},
	   {UMLStateName,DataState} = Module:init(Init),
	   {N+1,
	    add_machine
	      (PreMachine#machine
	       {id=N,
		mailbox=[],
		uml_state_name=UMLStateName,
		data_state=DataState},
	       Acc)}
       end,
       {0,[]},
       MachineSpecs),
  loop
    (#process
     {machines=Machines,
      memory=Memory}).

-spec loop(#process{}) -> no_return().
loop(State) ->
  ?LOG("~p: loop(~p)~n",[symbolic_name(self()),State]),
  {HasWrite,ReadState} = do_read_messages(State),
  Changed = State#process.changed,
  case {Changed,HasWrite} of
    {true,false} ->
      run_machines(ReadState);
    {false,true} ->
      do_read(ReadState);
    {true,true} -> 
      ?CHOOSE
	 ([{?MODULE,run_machines,[ReadState]},
	   {?MODULE,do_read,[ReadState]}]);
    {false,false} ->
      do_read(ReadState)
  end.

run_machines(State) ->
  case compute_transitions(State) of
    [] -> loop(modify_change_status(State,false));
    Transitions -> pick_a_transition(Transitions,State)
  end.

do_read(State) ->
  receive
    RawMsg -> 
      ?LOG("~p: message ~p received~n",[symbolic_name(self()),RawMsg]),
      case RawMsg of
	{message,Msg} ->
	  ?LOG("~p: got a message ~p~n",[symbolic_name(self()),Msg]),
	  loop
	    (State#process
	     {changed=true,
	      machines=
		lists:map
		  (fun ({MachinePid,Machine}) ->
		       {MachinePid,
			Machine#machine
			{mailbox=Machine#machine.mailbox++[Msg]}}
		   end, State#process.machines)});
	{write,{MachineId,Var,Value}} ->
	  ?LOG
	     ("~p: machine ~p wrote ~p to variable ~p~n",
	      [symbolic_name(self()),MachineId,Value,Var]),
	  ets:insert(State#process.memory,{Var,Value}),
	  loop(modify_change_status(State,true));
	OtherMsg ->
	  io:format
	    ("*** warning: strange message ~p received~n",
	     [OtherMsg]),
	  loop(State)
      end
  end.

do_read_messages(State) ->
  case erlang:process_info(self(),message_queue_len) of
    {message_queue_len,0} -> {false,State};
    _ ->
      case erlang:process_info(self(),messages) of
	{messages,[{write,_}|_]} -> {true,State};
	_ ->
	  receive
	    {message,Msg} ->
	      ?LOG("~p: got a message ~p~n",[symbolic_name(self()),Msg]),
	      do_read_messages
		(State#process
		 {changed=true,
		  machines=
		    lists:map
		      (fun ({MachinePid,Machine}) ->
			   {MachinePid,
			    Machine#machine
			    {mailbox=Machine#machine.mailbox++[Msg]}}
		       end, State#process.machines)});
	    OtherMsg ->
	      io:format
		("*** warning: strange message ~p received~n",
		 [OtherMsg]),
	      do_read_messages(State)
	  end
      end
  end.
  
compute_transitions(State) ->
  lists:foldl
    (fun ({_,Machine},Acc) ->
	 StateName = Machine#machine.uml_state_name,
	 DataState = Machine#machine.data_state,
	 UMLState = (Machine#machine.module):state(StateName),
	 Transitions = UMLState#uml_state.transitions,
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
			  Machine#machine.id,
			  StateName,
			  State,
			  DataState,
			  Class,
			  nReason,
			  Stacktrace]),
		      erlang:raise(Class,Reason,Stacktrace)
		  end
	      end, [], ReadTransitions),
	 EnabledReceives =
	   find_enabled_receives
	     (ReceiveTransitions,DataState,Machine,State,StateName,Machine),
	 EnabledReads++EnabledReceives++Acc
     end,
     [],
     State#process.machines).

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
		 Machine#machine.id,
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

pick_a_transition([T],State) ->
  run_transition(T,State);
pick_a_transition(Transitions,State) ->
  ?CHOOSE
    (lists:map
       (fun (Transition) ->
	    {?MODULE,run_transition,[Transition,State]}
	end, Transitions)).

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
			   Machine#machine.id,
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
	    Machine#machine.id,
	    FromState,
	    ToState,
	    State,
	    DataState,
	    Class,
	    Reason,
	    Stacktrace]),
	erlang:raise(Class,Reason,Stacktrace)
    end.

replace_machine(NewMachine,State) ->
  MachinePid = 
    NewMachine#machine.id,
  State#process
    {machines=
       lists:keyreplace
	 (MachinePid,1,State#process.machines,{MachinePid,NewMachine})}.

add_machine(Machine,Machines) ->  
  [{Machine#machine.id,Machine}|Machines].

symbolic_name(Pid) when is_pid(Pid) ->
  case process_info(Pid,registered_name) of
    {registered_name,Name} -> Name;
    _ -> Pid
  end.

modify_change_status(State,NewValue) ->
  State#process{changed=NewValue}.

choose(L) ->
  Len = length(L),
  R = random:uniform(Len),
  case lists:nth(R,L) of
    F when is_function(F) -> F();
    {M,F,Args} -> apply(M,F,Args)
  end.

run_transition({GuardAction,ChosenTransition,Machine},State) ->  
  NextState =
    ChosenTransition#transition.next_state,
  {NewDataState,NewMailbox} =
    run_guard_action
      (GuardAction,
       Machine#machine.data_state,
       Machine#machine.uml_state_name,
       NextState,
       Machine,
       State),
  Doer = Machine#machine.doer,
  NewDoer =
    if
      not(ChosenTransition#transition.is_internal), Doer=/=void ->
	exit(Doer,kill),
	UMLState = (Machine#machine.module):state(NextState),
	if
	  UMLState#uml_state.do=/=void ->
	    Process =
	      {outside_process,
	       {Machine#machine.id,
		self(),
		State#process.memory}},
	    spawn
	      (fun () ->
		   try ((UMLState#uml_state.do)(Process))
		   catch Class:Reason ->
		       Stacktrace = erlang:get_stacktrace(),
		       io:format
			 ("~n*** Error: ~p: evaluation of do action ~p~n"
			  ++"for machine ~p in"
			  ++" machine state ~p~n"
			  ++"~nwith data~n  ~p~nraises exception ~p:~p~n"
			  ++"~nStacktrace:~n~p~n~n",
			  [self(),
			   UMLState#uml_state.do,
			   Machine#machine.module,
			   Machine#machine.uml_state_name,
			   NewDataState,
			   Class,
			   Reason,
			   Stacktrace]),
		       erlang:raise(Class,Reason,Stacktrace)
		   end
	       end);
	  true -> void
	end;
      true -> void
    end,
  NewState =
    replace_machine
      (Machine#machine
       {uml_state_name=NextState,
	data_state=NewDataState,
	doer=NewDoer,
	mailbox=NewMailbox},
       State),
  loop(NewState#process{changed=true}).


  
  


      
  
      
  
