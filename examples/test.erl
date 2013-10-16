-module(test).

-compile(export_all).

test() ->
  MachineSpecs = [{mean_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = spawn_link(fun () -> process:start(MachineSpecs) end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok -> P!{message,release}
  end.

test2() ->
  MachineSpecs = [{meaner_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = spawn_link(fun () -> process:start(MachineSpecs) end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok -> P!{message,release}
  end.

test3() ->
  MachineSpecs = [{local_var_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = spawn_link(fun () -> process:start(MachineSpecs) end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok -> P!{message,release}
  end.

test4() ->
  MachineSpecs = [{process_var_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P =
    spawn_link
      (fun () ->
	   process:start
	     (MachineSpecs,
	      fun (Process) ->
		  uml:assign(Process,acquired,false)
	      end)
       end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok -> P!{message,release}
  end.

test5() ->
  MachineSpecs = [{process_var_machine_with_entry,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P =
    spawn_link
      (fun () ->
	   process:start
	     (MachineSpecs,
	      fun (Process) ->
		  uml:assign(Process,acquired,false),
		  uml:assign(Process,counter,0)
	      end)
       end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok -> P!{message,release}
  end.



  
