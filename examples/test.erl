-module(test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

t_test() ->
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
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).

t2_test() ->
  MachineSpecs = [{meaner_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = spawn_link(fun () -> process:start(MachineSpecs) end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).

t3_test() ->
  MachineSpecs = [{local_var_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = spawn_link(fun () -> process:start(MachineSpecs) end),
  io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).

t4_test() ->
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
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).

t5_test() ->
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
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).



  
