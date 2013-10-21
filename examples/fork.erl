% A fork programmed using a guard

-module(fork).

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
	   fun ({acquire,From},_Process,_State) -> 
	       {true,
		fun (_State) ->
		    uml:signal(From,acquired),
		    From
		end};
	       (_,_,_) -> false
	   end}
       ]};

state(acquired) ->
  #uml_state
    {name=acquired,
     type='receive',
     defer=fun ({acquire,_},_,_) -> true; (_,_,_) -> false end,
     transitions=
       [
	#transition
	{type='receive',
	 next_state=idle,
	 guard=
	   fun (release,_Process,From) -> 
	       {true,
		fun (_State) ->
		    void
		end};
	       (_,_,_) -> false
	   end}
       ]}.

