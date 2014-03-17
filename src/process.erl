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

-module(process).

-export([start/1,start/2,start/3]).
-export([start_link/1,start_link/2,start_link/3]).
-export([run_machines/1,do_read/1,run_transition/2]).

-include("records.hrl").

-ifdef(McErlang).
-define(CHOOSE(L),mce_erl:choice(L)).
-define(OUTPUT(FORMAT,ARGS),mce_erl:apply(io,format,[FORMAT,ARGS])).
-define(GET_OPTION(OPTION),mce_erl:apply(umerl,getOption,[OPTION])).
-else.
-define(CHOOSE(L),choose(L)).
-define(OUTPUT(FORMAT,ARGS),io:format(FORMAT,ARGS)).
-define(GET_OPTION(OPTION),umerl:getOption(OPTION)).
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

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% that should run when the container is created.
-spec start([{atom(),any()}]) -> no_return().
start(MachineSpecs) ->
  start(MachineSpecs,fun (_) -> ok end).

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% (a module name, and a value corresponding to the initial
%% state of the machine)
%% that should run when the container is created, and
%% the InitVars parameter is a function to initialise the
%% shared variables in the container which is called when the
%% container is created.
-spec start([{atom(),any()}],fun((context())->any())) -> no_return().
start(MachineSpecs,InitVars) ->
  start(MachineSpecs,InitVars,[]).

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% (a module name, and a value corresponding to the initial
%% state of the machine)
%% that should run when the container is created, and
%% the InitVars parameter is a function to initialise the
%% shared variables in the container which is called when the
%% container is created.
-spec start([{atom(),any()}],fun((context())->any()),[any()]) -> no_return().
start(MachineSpecs,InitVars,Options) ->
  Memory = ets:new(private,[public]),
  InitVars({in_process,{Memory,self()}}),
  {_,Machines} =
    lists:foldl
      (fun ({Module,Init},{N,Acc}) ->
	   {N+1,add_machine(create_machine(N,Module,Init),Acc)};
	   ({Name,Module,Init},{N,Acc}) ->
	   {N,add_machine(create_machine(Name,Module,Init),Acc)}
       end,
       {0,[]},
       MachineSpecs),
  SpawnFun =
    case lists:member(link,Options) of
      true -> spawn_link;
      false -> spawn
    end,
  erlang:SpawnFun
    (fun () ->
	 loop
	   (#process
	    {machines=Machines,
	     memory=Memory})
     end).

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% that should run when the container is created.
-spec start_link([{atom(),any()}]) -> no_return().
start_link(MachineSpecs) ->
  start_link(MachineSpecs,fun (_) -> ok end).

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% (a module name, and a value corresponding to the initial
%% state of the machine)
%% that should run when the container is created, and
%% the InitVars parameter is a function to initialise the
%% shared variables in the container which is called when the
%% container is created.
-spec start_link([{atom(),any()}],fun((context())->any())) -> no_return().
start_link(MachineSpecs,InitVars) ->
  start_link(MachineSpecs,InitVars,[]).

%% @doc Spawns a new container for state machines.
%% The MachineSpecs parameter is a list of the state machines
%% (a module name, and a value corresponding to the initial
%% state of the machine)
%% that should run when the container is created, and
%% the InitVars parameter is a function to initialise the
%% shared variables in the container which is called when the
%% container is created.
-spec start_link([{atom(),any()}],fun((context())->any()),[any()]) -> no_return().
start_link(MachineSpecs,InitVars,Options) ->
  start(MachineSpecs,InitVars,[link|Options]).

create_machine(Name,Module,Init) ->
  PreMachine = #machine{module=Module},
  {UMLStateName,DataState} = Module:init(Init),
  PreMachine#machine
    {id=Name,
     mailbox=[],
     uml_state_name=UMLStateName,
     data_state=DataState}.

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

%% @private
run_machines(State) ->
  case compute_transitions(State) of
    [] -> loop(modify_change_status(State,false));
    Transitions -> pick_a_transition(Transitions,State)
  end.

%% @private
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
		  (fun ({MachineId,Machine}) ->
		       {MachineId,
			case not(?GET_OPTION(early_discard))
			  orelse keep_message(Msg,Machine,State) of
			  true -> 
			    Machine#machine
			      {mailbox=Machine#machine.mailbox++[Msg]};
			  false ->
			    ?OUTPUT
			      ("*** warning: machine ~p discarded "++
				 "~p in ~p~n",
			       [MachineId,Msg,Machine#machine.uml_state_name]),
			    case ?GET_OPTION(discard_is_error) of
			      true ->
				throw({discard,Msg,Machine#machine.uml_state_name});
			      false ->
				ok
			    end,
			    Machine
			end}
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
      NewMailbox = lists:reverse(discard_messages(Seen,Machine,State),Rest),
      NewMachine = Machine#machine{mailbox=NewMailbox},
      lists:map
	(fun ({GuardAction,Transition}) -> {GuardAction,Transition,NewMachine} end,
	 Results)
  end.

discard_messages(Messages,Machine,State) ->
  case ?GET_OPTION(early_discard) of
    true ->
      Messages;
    false ->
      lists:filter(fun (Msg) -> keep_message(Msg,Machine,State) end, Messages)
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
	Defer =
	  case ?GET_OPTION(early_discard) of
	    true -> defer_value(NewState#uml_state.defer);
	    false -> all
	  end,
	if
	  Defer == all -> Mailbox;
	  Defer == none -> [];
	  true ->
	    lists:filter
	      (fun (Msg) -> 
		   case
		     run_defer
		     (Defer,Msg,ToState,DataState,
		      {in_process,{State#process.memory,self()}}) of
		     true ->
		       true;
		     false ->
		       ?OUTPUT
			  ("*** warning: machine ~p discarded "++
			     "~p in ~p~n",
			   [Machine#machine.id,Msg,ToState]),
		       case ?GET_OPTION(discard_is_error) of
			 true ->
			   throw({discard,Msg,ToState});
			 false ->
			   ok
		       end,
		       false
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

%% @private
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

%% A message should be kept if can possibly be read, or deferred, 
%% in the current state, irrespective of the current mailbox.
keep_message(Msg,Machine,State) ->
  StateName = Machine#machine.uml_state_name,
  DataState = Machine#machine.data_state,
  UMLState = (Machine#machine.module):state(StateName),
  Transitions = UMLState#uml_state.transitions,
  {ReadTransitions, ReceiveTransitions} = classify_transitions(Transitions),
  case try_receive_msg(Msg,ReceiveTransitions,DataState,State,StateName,Machine) of
    [] -> is_deferrable(Msg,Machine,State);
    _ -> true
  end.

is_deferrable(Msg,Machine,State) ->
  StateName = Machine#machine.uml_state_name,
  DataState = Machine#machine.data_state,
  UMLState = (Machine#machine.module):state(StateName),
  Defer = defer_value(UMLState#uml_state.defer),
  if
    Defer == all ->
      true;
    Defer == none ->
      false;
    true ->
      run_defer
	(Defer,Msg,StateName,DataState,
	 {in_process,{State#process.memory,self()}})
  end.

run_defer(Defer,Msg,StateName,DataState,Context) ->
  try Defer(Msg,DataState,Context)
  catch Class:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      io:format
	("~n*** Error: ~p: evaluation of defer function ~p~n"
	 ++"for machine ~p (~p) in"
	 ++" machine state ~p "
	 ++"~nwith data~n  ~p~nraises exception ~p:~p~n"
	 ++"~nStacktrace:~n~p~n~n",
	 [symbolic_name(self()),
	  Defer,
	  StateName,
	  DataState,
	  Class,
	  Reason,
	  Stacktrace]),
      erlang:raise(Class,Reason,Stacktrace)
  end.
  
defer_value(void) ->
  case umerl:getOption(discard_is_default) of
    true -> none;
    false -> all
  end;
defer_value(Other) ->
  Other.



      
      
  
