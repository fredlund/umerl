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




	      
  
