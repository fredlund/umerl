%% Copyright (c) 2013, Lars-Ake Fredlund
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

t_test() ->
  MachineSpecs = [{mean_machine,void}],
  io:format("Starting test with machine spec~n~p~n",MachineSpecs),
  P = process:start_link(MachineSpecs),
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
  P = process:start_link(MachineSpecs),
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
  P = process:start_link(MachineSpecs),
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
    process:start_link
      (MachineSpecs,
       fun (Process) ->
	   uml:assign(Process,acquired,false)
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
    process:start_link
      (MachineSpecs,
       fun (Process) ->
	   uml:assign(Process,acquired,false),
	   uml:assign(Process,counter,0)
       end),
    io:format("Started process ~p~n",[P]),
  P!{message,hola},
  P!{message,{acquire,self()}},
  receive
    {message,ok} ->
      P!{message,release}
  end,
  timer:sleep(200).

locker_test() ->
  Locker = process:start_link([{locker,void}]),
  Client1 = process:start_link([{locker_client,Locker}]),
  Client2 = process:start_link([{locker_client,Locker}]),
  timer:sleep(200).
    

			   

  
