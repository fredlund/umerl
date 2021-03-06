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

