%% A fork programmed using a object variable

-module(process_var_machine_with_entry).

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
			uml:assign(Process,acquired,false)
		    end};
		 _ -> false
	       end;
	       (_,_,_) -> false
	   end}
       ],
     entry=
       fun (Process,State) ->
	   Counter = uml:read(Process,counter),
	   io:format("Counter was ~p; will increment~n",[Counter]),
	   uml:assign(Process,counter,Counter+1),
	   State
       end
    }.

