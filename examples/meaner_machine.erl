% A fork programmed using an entry action

-module(meaner_machine).

-include("../src/records.hrl").

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
		    From
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
	   fun (release,_Process,_State) -> 
	       {true,
		fun (_State) ->
		    void
		end};
	       (_,_,_) -> false
	   end}
       ],
     entry=fun (Process,From) ->
	       io:format("got acquire message from ~p~n",[From]),
	       uml:signal(From,ok)
	   end
    }.

