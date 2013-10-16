%% A fork programmed using a object variable

-module(process_var_machine).

-include("../src/records.hrl").

-compile(export_all).

init(_Arg) ->
  {single, void}.

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
	   fun ({acquire,From},Process,_) -> 
	       case uml:read(Process,acquired) of
		 true -> false;
		 false ->
		   {true,
		    fun (_State) ->
			io:format("got acquire message from ~p~n",[From]),
			uml:signal(From,ok),
			uml:assign(Process,acquired,true),
			From
		    end}
		 end;
	       (_,_,_) -> false
	   end},

	#transition
	{type='receive',
	 next_state=single,
	 guard=
	   fun (release,Process,From) -> 
	       case uml:read(Process,acquired) of
		 true -> 
		   {true,
		    fun (_State) ->
			uml:signal(From,ok),
			uml:write(Process,acquired,false)
		    end};
		 _ -> false
	       end;
	       (_,_,_) -> false
	   end}
       ]}.

