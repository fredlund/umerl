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
			    monitor={stateMon,fun monPreds:speed_always_zero/1},
			    sends_are_sefs=true}}}}).
