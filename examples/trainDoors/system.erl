-module(system).

-export([start/1]).

start(NDoors) ->
  TCMS = 'tcms',
  Doors = create_doors(NDoors,TCMS),
  DriverButton = 'driverButton',
  register
    (DriverButton,
     spawn_link(fun () -> process:start({driverButton,TCMS}) end)),
  Traction = 'traction',
  register
    (Traction,
     spawn_link(fun () -> process:start({traction,void}) end)),
  register
    (TCMS,
     spawn_link(fun () -> process:start({tcms,{Doors,Traction,DriverButton}}) end)).

create_doors(N,TCMS) ->
  lists:foreach
    (fun (N) ->
	 NS = integer_to_list(N),
	 Door = list_to_atom("door_"++NS),
	 OpenSensor = list_to_atom("limitSensor_"++NS),
	 register
	   (OpenSensor,
	    spawn_link(fun () -> process:start({limitSensor,Door}) end)),
	 CloseSensor = list_to_atom("closeSensor_"++NS),
	 register
	   (CloseSensor,
	    spawn_link(fun () -> process:start({closeSensor,Door}) end)),
	 ObstacleSensor = list_to_atom("closeSensor_"++NS),
	 register
	   (ObstacleSensor,
	    spawn_link(fun () -> process:start({obstacleSensor,Door}) end)),
	 DoorButton = list_to_atom("dootButton_"++NS),
	 register
	   (DoorButton,
	    spawn_link(fun () -> process:start({doorButton,Door}) end)),
	 register
	   (Door,
	    spawn_link
	      (fun () ->
		   process:start({door,{TCMS, OpenSensor, CloseSensor, ObstacleSensor}})
	       end))
     end, lists:seq(1,N)).


  



			
