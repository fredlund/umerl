-module(system).

-export([start/1]).

%% We add timer sleeps to ensure that the all processes have started
%% before we start communicating.

start(NDoors) ->
  TCMS = 'tcms',
  Doors = create_doors(NDoors,TCMS),
  DriverButton = 'driverButton',
  register
    (DriverButton,
     spawn_link(fun () -> timer:sleep(100), process:start([{driverButton,TCMS}]) end)),
  Traction = 'traction',
  register
    (Traction,
     spawn_link
       (fun () ->
	    timer:sleep(100),
	    process:start
	      ([{traction,void},
		{speed,void}],
	      fun (Process) ->
		  uml:assign(Process,speed,0),
		  uml:assign(Process,breaking,false),
		  uml:assign(Process,accelerating,false)
	      end)
	end)),
  register
    (TCMS,
     spawn_link(fun () -> timer:sleep(100), process:start([{tcms,{Doors,Traction,DriverButton}}],fun (Process) -> uml:assign(Process,doorLength,length(Doors)) end) end)),
  
  %% we fake a doorbutton press
  uml:signal(list_to_atom("doorButton_2"),press),
  uml:signal(list_to_atom("driverButton"),press),
  uml:signal(list_to_atom("tcms"),disableDoors).

create_doors(N,TCMS) ->
  lists:map
    (fun (N) ->
	 NS = integer_to_list(N),
	 Door = list_to_atom("door_"++NS),
	 OpenSensor = list_to_atom("limitSensor_"++NS),
	 register
	   (OpenSensor,
	    spawn_link(fun () -> timer:sleep(100), process:start([{limitSensor,Door}]) end)),
	 CloseSensor = list_to_atom("closeSensor_"++NS),
	 register
	   (CloseSensor,
	    spawn_link(fun () -> timer:sleep(100), process:start([{limitSensor,Door}]) end)),
	 ObstacleSensor = list_to_atom("obstacleSensor_"++NS),
	 register
	   (ObstacleSensor,
	    spawn_link(fun () -> timer:sleep(100), process:start([{obstacleSensor,Door}]) end)),
	 DoorButton = list_to_atom("doorButton_"++NS),
	 register
	   (DoorButton,
	    spawn_link(fun () -> timer:sleep(100), process:start([{doorButton,Door}]) end)),
	 register
	   (Door,
	    spawn_link
	      (fun () ->
		   timer:sleep(100), process:start([{door,{TCMS, OpenSensor, CloseSensor, ObstacleSensor}}])
	       end)),
	 Door
     end, lists:seq(1,N)).


  



			
