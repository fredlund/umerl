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

-module(monPreds).
-language(erlang).

-include("erlang/state.hrl").
-include("erlang/node.hrl").
-include("erlang/process.hrl").

-export([not_is_closed/1,is_closed/1,speed_zero/1,speed_greater_than_zero/1,not_is_open/1,is_open/1,statePred_to_buechiPred/1]).

speed_zero(State) ->
  case registered_process(State,'traction') of
    {ok,P} -> 
      Value = ets_lookup_var('speed',P#process.pid,State),
      if
	Value == 0 -> true;
	true -> {speed,Value}
      end;
    _ -> true
  end.

speed_greater_than_zero(State) ->
  case registered_process(State,'traction') of
    {ok,P} -> 
      Value = ets_lookup_var('speed',P#process.pid,State),
      if
	Value > 0 -> true;
	true -> false
      end;
    _ -> false
  end.

is_closed(Doors) ->
  fun (State) ->
      lists:all(fun (Door) -> is_closed_door(State,Door) end, Doors)
  end.

is_open(Doors) ->
  fun (State) ->
      lists:any(fun (Door) -> is_open_door(State,Door) end, Doors)
  end.
  
not_is_open(Doors) ->
  fun (State) ->
      not((is_open(Doors))(State))
  end.
  
not_is_closed(Doors) ->
  fun (State) ->
      not((is_closed(Doors))(State))
  end.

is_open_door(State,Door) ->
  case registered_process(State,Door) of
    {ok,P} -> 
      Value = ets_lookup_var('status',P#process.pid,State),
      if
	Value == opened -> true;
	true -> false
      end;
    _ -> false
  end.

is_closed_door(State,Door) ->
  case registered_process(State,Door) of
    {ok,P} -> 
      Value = ets_lookup_var('status',P#process.pid,State),
      io:format("door ~p is ~p~n",[Door,Value]),
      if
	Value == closed -> true;
	true -> false
      end;
    _ -> false
  end.
  

registered_process(State,Name) ->
  [Node] = State#state.nodes,
  case lists:keysearch(Name,1,Node#node.registered) of
    {value,{_,Pid}} ->
      [P] =
	lists:filter
	  (fun (P) -> P#process.pid==Pid end, Node#node.processes),
      {ok,P};
    _ ->
      false
  end.

ets_lookup_var(Name,Pid,State) ->
  [Node] = State#state.nodes,
  lists:foldl
    (fun ({{_,PidN},{ets,{KeyPos,Values}}},_) when PidN==Pid ->
	 case lists:keysearch(Name,KeyPos,Values) of
	   {value,{_,Value}} -> Value;
	   _ -> 
	     io:format
	       ("*** Error: cannot find ~p at keypos ~p in ~p~n",
		[Name,KeyPos,Values]),
	     throw(bad)
	 end;
	 (_,Acc) -> Acc
     end, void, Node#node.dict).

statePred_to_buechiPred(StatePred) ->
  fun (State,_,_) ->
      case StatePred(State) of
	true -> true;
	_ -> false
      end
  end.

	 
  
