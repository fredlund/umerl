-module(uml).

-compile(export_all).

signal(To,Msg) ->
  io:format("signal ~p to ~p~n",[Msg,To]),
  To!Msg.

assign({in_process,Table},Var,Value) ->
  ets:insert(Table,{Var,Value}),
  Value;
assign({outside_process,MachinePid,Process,Table},Var,Value) ->
  Process!{write,MachinePid,Var,Value}.

read({in_process,Table},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value;
read({outside_process,MachinePid,Process,Table},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value.



