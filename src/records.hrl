-type actionfun() ::
	fun((any()) -> any()).

-type rcv_triggerfun() ::
	fun((any(),any(),any()) -> {true,actionfun()} | false).

-type data_triggerfun() ::
	fun((any(),any()) -> {true,actionfun()} | false).

-type triggerfun() :: rcv_triggerfun() | data_triggerfun().

-type permission() :: 'read' | 'receive' | 'receive_read'.

-record(transition,
	{
	  type=void :: 'void' | permission(),
	  is_internal=false :: boolean(),
	  %% Guard function (which returns the {true, action function} or false)
	  guard :: triggerfun(),
	  %% Target state
	  next_state :: atom()
	}).

-record(uml_state,
	{
	  %% State name
	  name :: atom(),
	  %% External and internal transitions
	  transitions :: [#transition{}],
	  %% Type of state, an "upper bound" for the transition types
	  type=void :: 'void' | permission(),
	  %% May be all, or a function
	  %% (over messages, object state and machine state).
	  %% Default is all.
	  defer=all :: 'all' | 'none' | fun(), 
	  %% Entry action
	  entry=void :: 'void' | fun(),
	  %% Do action
	  do=void :: 'void' | fun(),
	  %% Exit action
	  exit=void :: 'void' | fun()
	}).

	 
