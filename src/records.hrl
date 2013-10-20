-record(uml_state,
	{
	  %% State name
	  name,
	  %% External and internal transitions
	  transitions,
	  %% Type of state, an "upper bound" for the transition types
	  type=void,
	  %% May be all, or a function
	  %% (over messages, object state and machine state).
	  %% Default is all.
	  defer=all, 
	  %% Entry action
	  entry=void,
	  %% Do action
	  do=void,
	  %% Exit action
	  exit=void
	}).

-record(transition,
	{
	  %% Type: 'receive', 'read' or 'receive_read'.
	  type=void,
	  %% Internal transition (i.e., will not execute enter and exit actions)
	  %% or external
	  is_internal=false,
	  %% Guard function (which returns the {true, action function} or false)
	  guard,
	  %% Target state
	  next_state
	}).
	 
