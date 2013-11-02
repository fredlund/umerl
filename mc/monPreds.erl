-module(monPreds).
-language(erlang).

-include("erlang/state.hrl").
-include("erlang/node.hrl").
-include("erlang/process.hrl").

-export([speed_always_zero/1]).

speed_always_zero(State) ->
  case registered_process(State,'traction') of
    {ok,P} -> 
      Value = ets_lookup_var('speed',P#process.pid,State),
      if
	Value == 0 -> true;
	true -> {speed,Value}
      end;
    _ -> true
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
	 {value,{_,Value}} = lists:keysearch(Name,KeyPos,Values),
	 Value;
	 (_,Acc) -> Acc
     end, void, Node#node.dict).

	 
  
