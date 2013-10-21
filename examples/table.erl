-module(table).

-compile(export_all).

table(N) ->
  Forks = lists:map (fun (_) -> start_fork() end, lists:seq(1,N)),
  lists:foreach
    (fun ({L,R}) -> start_philosopher(L, R) end, adjacent(Forks)).

adjacent([]) -> [];
adjacent([X|Xs]) -> lists:zip([X] ++ Xs, Xs ++ [X]).

start_fork() ->
  spawn_link(fun () -> process:start([{fork,void}]) end).

start_philosopher(L,R) ->
  spawn_link(fun () -> process:start([{philosopher,{L,R}}]) end).

test_table() ->
  Left = start_fork(),
  Right = start_fork(),
  start_philosopher(Left,Right).

  
