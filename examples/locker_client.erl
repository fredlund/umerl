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

