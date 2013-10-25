% A fork programmed using a guard

-module(locker_client).

-include("../src/records.hrl").
-include("../src/umerl.hrl").

-compile(export_all).

init(Locker) ->
  {initial, Locker}.

state(initial) ->
  #uml_state
    {name=initial,
     type='read',
     transitions=
       [
	#transition
	{type='read',
	 next_state=final,
	 guard=
	   fun (_Process,Locker) -> 
	       {true,
		fun (_State) ->
		    ReturnValue = uml:call(Locker,acquire),
		    io:format
		      ("~p: acquired locker which returned value ~p~n",
		       [self(),ReturnValue]),
		    Locker
		end}
	   end}
       ]};

state(final) ->
  #uml_state
    {name=final,
     type='read',
     transitions=
       [
	#transition
	{type='read',
	 next_state=initial,
	 guard=
	   fun (__Process,Locker) -> 
	       {true,
		fun (_State) ->
		    ReturnValue = uml:call(Locker,release),
		    io:format
		      ("~p: released locker which returned value ~p~n",
		       [self(),ReturnValue]),
		    Locker
		end}
	   end}
       ]}.

