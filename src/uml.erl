-module(uml).

-export([signal/2,assign/3,read/2,call/2,return/2,self/1]).

-include("records.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

-spec signal(pid()|atom(),any()) -> any().
signal(To,Msg) ->
  ?LOG("signal ~p to ~p~n",[Msg,To]),
  To!{message,Msg}.

-spec call(pid()|atom(),any()) -> any().
call(To,Msg) ->
  To!{message,{call,Msg,self()}},
  receive
    {return,Result} ->
      Result
  end.

-spec return(pid()|atom(),any()) -> any().
return(To,Msg) ->
  To!{return,Msg},
  Msg.

-spec assign(context(),atom(),any()) -> any().
assign({in_process,{Table,_Process}},Var,Value) ->
  ets:insert(Table,{Var,Value}),
  put(var_write,true),
  Value;
assign({outside_process,{MachinePid,Process,Table}},Var,Value) ->
  Process!{write,MachinePid,Var,Value},
  Value.

-spec read(context(),atom()) -> any().
read({in_process,{Table,_Process}},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value;
read({outside_process,{MachinePid,Process,Table}},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value.

-spec self(context()) -> pid()|atom().
self({in_process,{_Table,Process}}) ->
  symbolic_name(Process);
self({outside_process,{_MachinePid,Process,_Table}}) ->
  symbolic_name(Process).

symbolic_name(Pid) when is_pid(Pid) ->
  case process_info(Pid,registered_name) of
    {registered_name,Name} -> Name;
    _ -> Pid
  end;
symbolic_name(Other) ->
  Other.


