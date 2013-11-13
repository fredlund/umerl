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

-module(uml).

-export([signal/2,assign/3,read/2,call/2,return/2,self/1]).

-include("records.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y), ok).
-endif.

-spec signal(pid()|atom(),any()) -> any().
signal(To,Msg) ->
  ?LOG("signal ~p to ~p~n",[Msg,To]),
  To!{message,Msg}.

-spec call(pid()|atom(),any()) -> any().
call(To,Msg) ->
  To!{message,{call,Msg,self()}},
  receive
    {return,Result} ->
      Result
  end.

-spec return(pid()|atom(),any()) -> any().
return(To,Msg) ->
  To!{return,Msg},
  Msg.

-spec assign(context(),atom(),any()) -> any().
assign({in_process,{Table,_Process}},Var,Value) ->
  ets:insert(Table,{Var,Value}),
  Value;
assign({outside_process,{MachinePid,Process,Table}},Var,Value) ->
  Process!{write,MachinePid,Var,Value},
  Value.

-spec read(context(),atom()) -> any().
read({in_process,{Table,_Process}},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value;
read({outside_process,{MachinePid,Process,Table}},Var) ->
  [{_,Value}] = ets:lookup(Table,Var),
  Value.

-spec self(context()) -> pid()|atom().
self({in_process,{_Table,Process}}) ->
  symbolic_name(Process);
self({outside_process,{_MachinePid,Process,_Table}}) ->
  symbolic_name(Process).

symbolic_name(Pid) when is_pid(Pid) ->
  case process_info(Pid,registered_name) of
    {registered_name,Name} -> Name;
    _ -> Pid
  end;
symbolic_name(Other) ->
  Other.


