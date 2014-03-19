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

-type context() :: {in_process,{any(),pid()|atom()}}
		 | {outside_process,{any()|pid(),pid()|atom(),any()}}.

-type actionfun() ::
	fun((any()) -> any()).

-type contextfun() ::
	fun((context()) -> any()).

-type statefun() ::
	fun((context(),any()) -> any()).

-type deferfun() ::
	fun((any(),any(),context()) -> boolean()).

-type rcv_triggerfun() ::
	fun((any(),context(),any()) -> {true,actionfun()} | false).

-type data_triggerfun() ::
	fun((context(),any()) -> {true,actionfun()} | false).

-type triggerfun() :: rcv_triggerfun() | data_triggerfun().

-type permission() :: 'read' | 'receive' | 'receive_read'.

-record(transition,
	{
	  type=void :: 'void' | permission(),
	  is_internal=false :: boolean(),
	  %% Guard function (which returns the {true, action function} or false)
	  guard :: triggerfun(),
	  %% Target state
	  next_state :: atom()
	}).

-record(uml_state,
	{
	  %% State name
	  name :: atom(),
	  %% External and internal transitions
	  transitions :: [#transition{}],
	  %% Type of state, an "upper bound" for the transition types
	  type=void :: 'void' | permission(),
	  %% May be all, or a function
	  %% (over messages, object state and machine state).
	  %% Default is all.
	  defer=void :: 'void' | 'all' | 'none' | deferfun(),
	  %% Entry action
	  entry=void :: 'void' | statefun(),
	  %% Do action
	  do=void :: 'void' | contextfun(),
	  %% Exit action
	  exit=void :: 'void' | statefun()
	}).

-type option() :: {discard_is_error,boolean()}
		| {discard_is_default,boolean()}.

