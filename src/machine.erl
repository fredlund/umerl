-module(machine).

-export([start/4]).

-include("records.hrl").

-record(machine_int,
	{
	  module :: atom(),
	  uml_state_name :: atom(),
	  data_state :: any(),
	  process :: pid(),
	  memory,
	  doer=void :: 'void' | pid()
	}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

%% Handle defers

-spec start(atom(),any(),pid(),any()) -> any().
start(Module,InitArg,Process,Memory) ->
  {UMLStateName,DataState} = Module:init(InitArg),
  loop
    (#machine_int
     {module=Module,
      process=Process,
      memory=Memory,
      uml_state_name=UMLStateName,
      data_state=DataState}).

-spec loop(#machine_int{}) -> no_return().
loop(State) ->
  ?LOG("~p: loop(~p)~n",[self(),State]),
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
	  State#machine_int.data_state,
	  State#machine_int.doer}},
      receive
	Msg={state,NewUMLStateName,NewDataState} ->
	  ?LOG("~p received ~p",[self(),Msg]),
	  UMLState = (State#machine_int.module):state(NewUMLStateName),
	  if
	    UMLState#uml_state.do=/=void ->
	      DoProcess =
		spawn
		  (fun () ->
		       (UMLState#uml_state.do)(Process,NewDataState)
		   end),
	      loop
		(State#machine_int
		 {doer=DoProcess,
		  uml_state_name=NewUMLStateName,
		  data_state=NewDataState});
	    true -> 
	      loop(State#machine_int
		   {uml_state_name=NewUMLStateName,
		    data_state=NewDataState})
	  end;
	Other ->
	  ?LOG("~p received ~p~n",[self(),Other]),
	  loop(State)
      end;
    Other ->
      ?LOG("~p received ~p~n",[self(),Other]),
      loop(State)
  end.
