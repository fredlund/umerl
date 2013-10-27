-module(system).

-export([start/0]).

start() ->
  Door = spawn_link(fun () -> process:start({door,void}) end),
  DoorButton = spawn_link(fun () -> process:start({doorButton,Door}) end),
  DriverButton = spawn_link(fun () -> process:start({driverButton,void}) end),
  LimitSensor = spawn_link(fun () -> process:start({limitSensor,void}) end),
  ObstacleSensor = spawn_link(fun () -> process:start({ObstacleSensor,void}) end),
  Traction = spawn_link(fun () -> process:start({Traction,void}) end),
  TCMS = spawn_link(fun () -> process:start({TCMS,void}) end).




			
