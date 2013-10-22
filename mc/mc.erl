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

db2() ->
  mce:start
    (#mce_opts
     {program={table,table,[2]},
      algorithm=mce_alg_debugger}).

