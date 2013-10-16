%% A fork programmed using a local variable

-module(local_var_machine).

-include("records.hrl").

-compile(export_all).

init(_Arg) ->
  {single, {non_acquired,void}}.

state(single) ->
  #uml_state
    {name=single,
      type='receive',
     transitions=
       [
	#transition
	{type='receive',
	 next_state=single,
	 guard=
	   fun ({acquire,From},_Process,{non_acquired,_}) -> 
	       {true,
		fun (_State) ->
		    io:format("got acquire message from ~p~n",[From]),
		    uml:signal(From,ok),
		    {acquired,From}
		end};
	       (_,_,_) -> false
	   end},

	#transition
	{type='receive',
	 next_state=single,
	 guard=
	   fun (release,_Process,{acquired,From}) -> 
	       {true,
		fun (_State) ->
		    uml:signal(From,ok),
		    {non_acquired,void}
		end};
	       (_,_,_) -> false
	   end}
       ]}.

