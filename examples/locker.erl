% A fork programmed using a guard

-module(locker).

-include("../src/records.hrl").
-include("../src/umerl.hrl").

-compile(export_all).

init(_Arg) ->
  {idle, void}.

state(idle) ->
  #uml_state
    {name=idle,
     type='receive',
     transitions=
       [
	#transition
	{type='receive',
	 next_state=acquired,
	 guard=
	   fun ({call,acquire,From},_Process,_State) -> 
	       {true,
		fun (_State) ->
		    uml:return(From,ok)
		end};
	       (_,_,_) -> false
	   end}
       ]};

state(acquired) ->
  #uml_state
    {name=acquired,
     type='receive',
     transitions=
       [
	#transition
	{type='receive',
	 next_state=idle,
	 guard=
	   fun ({call,release,From},_Process,_) -> 
	       {true,
		fun (_State) ->
		    uml:return(From,ok)
		end};
	       (_,_,_) -> false
	   end}
       ]}.

