-module(mc).

-include("mce_opts.hrl").

-compile(export_all).

mc() ->
  mce:start
    (#mce_opts
     {program={example,example1,[]}}).

mc2() ->
  mce:start
    (#mce_opts
     {program={table,table,[2]}}).

mc3() ->
  mce:start
    (#mce_opts
     {program={table,table,[2]},
      table={mce_table_bitHash,[800025]}}).

mc4() ->
  mce:start
    (#mce_opts
     {program={example,example1,[]},
      monitor=mce_mon_deadlock}).

db() ->
  mce:start
    (#mce_opts
     {program={system,start,[2]},
      algorithm=mce_alg_debugger}).

mc5(N) ->
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_safety,void},
			    output = false,
			    is_infinitely_fast=true,
			    sends_are_sefs=true}}}}).

db2(N) ->
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      monitor=mce_mon_deadlock,
      is_infinitely_fast=true,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_debugger,void},
			    is_infinitely_fast=true}}}}).

sim2(N) ->
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      monitor=mce_mon_deadlock,
      is_infinitely_fast=true,
      sends_are_sefs=true,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    is_infinitely_fast=true}}}}).

mch(N) ->
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_safety,void},
			    output = false,
			    is_infinitely_fast=true,
			    monitor={stateMon,fun monPreds:speed_zero/1},
			    sends_are_sefs=true}}}}).

mev(N,Discard) ->
  umerl:setOptions([{discard_is_default,Discard}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_buechi,void},
			    output = false,
			    is_infinitely_fast=true,
			    monitor=
			      eventually:monEv
				(monPreds:statePred_to_buechiPred
				 (fun monPreds:speed_zero/1)),
			    sends_are_sefs=true}}}}).

md(N) ->
  umerl:setOptions([{discard_is_error,true}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_safety,void},
			    output = false,
			    monitor={stateMon,fun monPreds:speed_is_zero/1},
			    is_infinitely_fast=true,
			    sends_are_sefs=true}}}}).

md2(N) ->
  umerl:setOptions([{discard_is_error,true}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_safety,void},
      table=mce_table_hashWithActions,
      shortest=true,
      sends_are_sefs=true}).

  
mc_prop1(N) ->
  %%umerl:setOptions([{discard_is_error,true}]),
  umerl:setOptions([{discard_is_default,false}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_safety,void},
			    output = false,
			    is_infinitely_fast=true,
			    monitor=
			      {any,
			       [fun monPreds:speed_zero/1,
				(monPreds:not_is_open([door_1,door_2]))]},
			    sends_are_sefs=true}}}}).

mc_prop2(N) ->
  %%umerl:setOptions([{discard_is_error,true}]),
  umerl:setOptions([{discard_is_default,false}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_safety,void},
			    output = false,
			    is_infinitely_fast=true,
			    monitor=
			      {any,
			       [fun monPreds:speed_zero/1,
				monPreds:is_closed([door_1,door_2])]},
			    sends_are_sefs=true}}}}).
  

mc_prop3(N) ->
  umerl:setOptions([{discard_is_default,false}]),
  mce:start
    (#mce_opts
     {program={system,start,[N]},
      is_infinitely_fast=true,
      output = false,
      algorithm={mce_alg_combine,
		 {#mce_opts{algorithm={mce_alg_simulation,void},
			    sends_are_sefs=true,
			    output = false,
			    is_infinitely_fast=true,
			    scheduler={sched,void}},
		  #mce_opts{algorithm={mce_alg_buechi,void},
			    output = false,
			    is_infinitely_fast=true,
			    monitor=
			      eventually:monEv
				(monPreds:statePred_to_buechiPred
				 (monPreds:is_open([door_1]))),
			    sends_are_sefs=true}}}}).
