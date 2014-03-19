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

-module(sched).

-behaviour(scheduler).

-export([init/1,choose/4,willCommit/1]).

init(StartState) -> {ok,StartState}.

willCommit(_) -> true.
  
choose(Transitions,SchedState,Monitor,Conf) ->
  FilteredTransitions =
    lists:filter
      (fun reqFilter/1,
       lists:map(fun (T) -> mce_erl_opsem:commit(T,Monitor,Conf) end, Transitions)),

  case length(FilteredTransitions) of
    N when N>0 ->
      SelectedNumber = random:uniform(N),
      {ok,{lists:nth(SelectedNumber,FilteredTransitions),SchedState}};
    0 -> no_transitions
  end.

reqFilter({Actions,_}) -> not(lists:any(fun send2traction/1, Actions)).

%%send2traction(Action) ->
%%  io:format("Action is ~p~n",[Action]),
%%  case mce_erl_actions:is_send(Action) of
%%    true -> mce_erl_actions:get_send_pid(Action)=:=doorButton_1;
%%    false -> false
%%  end.

send2traction(Action) ->
  %%io:format("Action is ~p~n",[Action]),
  case mce_erl_actions:is_run(Action) of
    true -> 
      case mce_erl_actions:get_run_expr(Action) of
	{example,_,_} -> true;
	_ -> false
      end;
    false -> false
  end.

      
  

