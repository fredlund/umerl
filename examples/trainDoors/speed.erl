%% Speed SM
-module(speed).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-export([init/1,state/1]).

-define(NONZERO,5).

init(_Arg) ->
  {stopped, void}.

state(stopped) ->
  #uml_state
    {name = stopped,
     type = 'read',
     transitions=
       [
	#transition
	{type        =   'read',
	 next_state  =   moving,
	 guard       =
	   fun (Process, State) ->
	       case uml:read(Process,accelerating) of
		 true ->
		   {true,
		    fun (State) ->
			uml:assign(Process,speed,?NONZERO),
			io:format("speed became nonzero~n"),
			State
		    end};
		 false ->
		   false
	       end;
	       (_, _) -> false
	   end}
       ]
    };

state(moving) ->
  #uml_state
    {name = moving,
     type = 'read',
     transitions=
       [
	#transition
	{type        =   'read',
	 next_state  =   breaking,
	 guard       =
	   fun (Process, State) ->
	       case uml:read(Process,breaking) of
		 true -> {true, fun(State) -> io:format("beginning to brake~n"), State end};
		 false -> false
	       end;
	       (_, _) -> false
	   end}
       ]
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
	       {true,
		fun (State) ->
		    uml:assign(Process,speed,0),
		    io:format("speed became zero~n"),
		    State
		end}
	   end}
       ]
    }.

