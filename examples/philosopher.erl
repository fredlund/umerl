% A philosopher programmed using a guard

-module(philosopher).

-include("../src/records.hrl").
-include("../src/umerl.hrl").

-compile(export_all).

init(Arg) ->
  {thinking, Arg}.

state(thinking) ->
  #uml_state
    {name=thinking,
     type='read',
     defer='none',
     transitions=
       [
	#transition
	{type='read',
	 next_state=hungry,
	 guard=
	   fun (_Process,_) -> 
	       {true,
		fun (State={FL,FR}) ->
		    io:format("~p: sending acquire~n",[self()]),
		    uml:signal(FL,{acquire,self()}),
		    State
		end};
	       (_,_) -> false
	   end}
       ]};

state(hungry) ->
  #uml_state
    {name=hungry,
     type='receive',
     defer='none',
     transitions=
       [
	#transition
	{type='receive',
	 next_state=waiting,
	 guard=
	   fun (acquired,_Process,_State) -> 
	       {true,
		fun (State={FL,FR}) ->
		    io:format("~p: got one, sending acquire~n",[self()]),
		    uml:signal(FR,{acquire,self()}),
		    State
		end};
	       (_,_,_) -> false
	   end}
       ]};

state(waiting) ->
  #uml_state
    {name=waiting,
     type='receive',
     defer='none',
     transitions=
       [
	#transition
	{type='receive',
	 next_state=eating,
	 guard=
	   fun (acquired,_Process,_State) -> 
	       {true,
		fun (State) ->
		    io:format("~p: got second~n",[self()]),
		    State
		end};
	       (_,_,_) -> false
	   end}
       ]};

state(eating) ->
  #uml_state
    {name=eating,
     type='read',
     defer='none',
     transitions=
       [
	#transition
	{type='read',
	 next_state=thinking,
	 guard=
	   fun (_Process,_State) -> 
	       {true,
		fun (State) ->
		    State
		end};
	       (_,_) -> false
	   end}
       ],
    do=
       fun (_Process,_State) ->
	   io:format("~p: eating spaghetti...~n",[self()])
       end,
     exit=
       fun (_Process,State={FR,FL}) ->
	   io:format("~p: releasing...~n",[self()]),
	   uml:signal(FR,release),
	   uml:signal(FL,release),
	   State
       end
    }.

