-module(mc).

-include("mce_opts.hrl").

-compile(export_all).

mc() ->
  mce:start
    (#mce_opts
     {program={example,example1,[]}}).

