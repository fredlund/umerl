-module(stateMon).

-export([init/1,stateChange/3,monitorType/0]).
-behaviour(mce_behav_monitor).

init(Pred) ->
  {ok,Pred}.

stateChange(State,Pred,_) -> 
  case Pred(State) of
    true -> {ok,Pred};
    Other -> {mon_error,Other}
  end.

monitorType() ->
  safety.

	 
  
