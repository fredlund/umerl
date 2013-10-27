-module(system).

-export([start/0]).

start() ->
  Traction = spawn_link(fun () -> process:start({traction,void}) end),
  TCMS = spawn_link(fun () -> process:start({tcms,void}) end),
  DriverButton = spawn_link(fun () -> process:start({driverButton,TCMS}) end),
  OpenSensor = spawn_link(fun () -> process:start({limitSensor,Door}) end),
  CloseSensor = spawn_link(fun () -> process:start({limitSensor,Door}) end),
  ObstacleSensor = spawn_link(fun () -> process:start({obstacleSensor,Door}) end),
  Door = spawn_link(fun () -> process:start({door,{Traction, OpenSensor, CloseSensor, ObstacleSensor}}) end),
  DoorButton = spawn_link(fun () -> process:start({doorButton,Door}) end).




			
