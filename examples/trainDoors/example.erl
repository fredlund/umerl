-module(example).

-export([example1/0]).

example1() ->
  sleep(10000)
    ,uml:signal(list_to_atom("driverButton"),press)
    ,uml:signal(list_to_atom("tcms"),disableDoors)
    ,uml:signal(list_to_atom("tcms"),stopTrain)
    ,uml:signal(list_to_atom("driverButton"),press)
    ,uml:signal(list_to_atom("doorButton_1"),press)
    .

sleep(N) ->
  receive
  after N -> ok 
  end.
