-module(uml).

-compile(export_all).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

signal(To,Msg) ->
  ?LOG("signal ~p to ~p~n",[Msg,To]),
  To!Msg.

assign({in_process,Table},Var,Value) ->
  ets:insert(Table,{Var,Value}),
  put(var_write,true),
  Value;
assign({outside_process,MachinePid,Process,Table},Var,Value) ->
  Process!{write,MachinePid,Var,Value}.

read({in_process,Table},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value;
read({outside_process,MachinePid,Process,Table},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value.



