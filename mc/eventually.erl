-module(eventually).

-export([monEv/1]).

monEv(F) -> 
  define_eventually(),
  {ev,{void,[{'P',F}]}}.

define_eventually() ->
  mce_ltl_parse:ltl_string2module_and_load("eventually(not P)",ev).
