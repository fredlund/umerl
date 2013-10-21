-module(example).

-compile(export_all).

example1() ->
  MachineSpecs = [{mean_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P =
    spawn_link
      (fun () ->
	   process:start(MachineSpecs)
       end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    ok ->
      P!{message,release}
  end.
  
