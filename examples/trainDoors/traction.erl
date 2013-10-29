%% Traction SM
-module(traction).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-export([init/1,state/1]).

init(_Arg) ->
  {stopped, void}.

state(stopped) ->
  #uml_state
    {name = stopped,
     type = 'receive',
     transitions=
       [
	#transition
	{type        =   'receive',
	 next_state  =   moving,
	 guard       =
	   fun (enable, Process, State) ->
	       {true,
		fun (State) ->
		    uml:assign(Process,accelerating,true),
		    State
		end};
	       (_, _, _) -> false
	   end}
       ]
    };


state(moving) ->
  #uml_state
    {name = moving,
     type = 'receive',
     transitions=
       [
	#transition
	{type        =   'receive',
	 next_state  =   breaking,
	 guard       =
	   fun (disable, Process, State) ->
	       {true,
		fun (State) ->
		    uml:assign(Process,accelerating,false),
		    uml:assign(Process,breaking,true),
		    State
		end 
	       };
	       (_, _, _) -> false
	   end}
       ],
     do=
       fun(_Process) ->
	   io:format("Moving train...~n")
       end
    };


state(breaking) ->
  #uml_state
    {name = breaking,
     type = 'read',
     transitions=
       [
	#transition
	{type        =   'read',
	 next_state  =   stopped,
	 guard       =
	   fun(Process, T) ->
	       case uml:read(Process, speed) of
		 0 ->
		   {true,
		    fun(State) ->
			uml:signal(T, trainStopped),
			uml:assign(Process,breaking,false),
			State
		    end};
		 false -> false
	       end
	   end}
       ],
     do= 
       fun(_Process) ->
	   io:format("Breaking train...~n")
       end
    }.

