%% Copyright (c) 2013, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
		end}
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
		end}
	   end}
       ],
    do=
       fun (_Process) ->
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

