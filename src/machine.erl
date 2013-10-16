-module(machine).

-compile(export_all).

-include("records.hrl").

-record(machine_int,{module,uml_state_name,data_state,process,memory}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

%% Handle defers

start(Module,InitArg,Process,Memory) ->
  {UMLStateName,DataState} = Module:init(InitArg),
  loop
    (#machine_int
     {module=Module,
      process=Process,
      memory=Memory,
      uml_state_name=UMLStateName,
      data_state=DataState}).

loop(State) ->
  UMLPreState =
    (State#machine_int.module):state(State#machine_int.uml_state_name),
  if
    UMLPreState#uml_state.type==void ->
      io:format
	("~n*** Error: uml state ~p has type void~n",
	 [UMLPreState#uml_state.name]),
      throw(baddata);
    true -> ok
  end,
  (State#machine_int.process)!
    {ask_transitions,
     {self(),
      UMLPreState#uml_state.type}},
  Process =
    {outside_process,self(),State#machine_int.process,State#machine_int.memory},
  receive
    ok ->
      ?LOG("~p received ok",[self()]),
      (State#machine_int.process)!
	{transitions,
	 {self(),
	  State#machine_int.uml_state_name,
	  State#machine_int.data_state}},
      receive
	Msg={state,NewUMLStateName,NewDataState} ->
	  ?LOG("~p received ~p",[self(),Msg]),
	  UMLState = (State#machine_int.module):state(NewUMLStateName),
	  NDS1 =
	    if
	      UMLState#uml_state.entry=/=void ->
		?LOG("~p: running entry action~n",[self()]),
		(UMLState#uml_state.entry)(Process,NewDataState);
	      true ->
		NewDataState
	    end,
	  NDS2 =
	    if
	      UMLState#uml_state.do=/=void ->
		(State#machine_int.process)!
		  {do,
		   {self(),
		    State#machine_int.uml_state_name,
		    NDS1}},
		receive
		  ok ->
		    ?LOG("~p received ok~n",[self()]),
		    ?LOG("~p: running do action~n",[self()]),
		    (UMLState#uml_state.do)(Process,NDS1);
		  not_ok ->
		    ?LOG("~p received not_ok~n",[self()]),
		    NDS1
		end;
	      true -> NDS1
	    end,
	  %% Maybe we should wait until running the exit loop until
	  %% we are really leaving -- yes this is wrong.
	  NDS3 =
	    if
	      UMLState#uml_state.exit=/=void ->
		?LOG("~p: running exit action~n",[self()]),
		(UMLState#uml_state.exit)(Process,NDS2);
	      true ->
		NDS2
	    end,
	  loop
	    (State#machine_int
	     {uml_state_name=NewUMLStateName,
	      data_state=NDS3});
	Other ->
	  ?LOG("~p received ~p~n",[self(),Other]),
	  loop(State)
      end;
    Other ->
      ?LOG("~p received ~p~n",[self(),Other]),
      loop(State)
  end.
