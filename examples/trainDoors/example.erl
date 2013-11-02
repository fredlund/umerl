-module(example).

-export([example1/0]).

example1() ->
  sleep(10000)
    ,uml:signal(list_to_atom("doorButton_1"),press)
    ,uml:signal(list_to_atom("driverButton"),press)
    ,uml:signal(list_to_atom("tcms"),disableDoors)
    .

sleep(N) ->
  receive
  after N -> ok 
  end.
