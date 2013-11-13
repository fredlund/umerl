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

-module(umerl).

-include("records.hrl").

-language(erlang).

-export([setOptions/1,getOption/1,setOption/2,defaultValue/1]).

-spec setOptions([option()]) -> any().
setOptions(Options) ->
  case ets:info(umerl_conf) of
    undefined ->
      Self = self(),
      spawn(fun () ->
		ets:new(umerl_conf,[named_table,public]),
		ets:insert(umerl_conf,{options,Options}),
		Self!{initialized,true},
		wait_forever()
	    end),
      receive
	{initialized,true} -> ok
      end;
    _ -> 
      ets:insert(umerl_conf,{options,Options}),
      ok
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

getOption(Option) ->
  case ets:info(umerl_conf) of
    undefined -> defaultValue(Option);
    _ ->
      case ets:lookup(umerl_conf,options) of
	[{options,Options}] ->
	  proplists:get_value(Option,Options,defaultValue(Option))
      end
  end.

setOption(Option,Value) ->
  case ets:lookup(umerl_conf,options) of
    [{options,Options}] ->
      ets:insert(umerl_conf,{options,[{Option,Value}]})
  end.

defaultValue(discard_is_error) ->
  false;
defaultValue(discard_is_default) ->
  true;
defaultValue(_) ->
  void.




	      
  
