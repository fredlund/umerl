-type context() :: {in_process,{any(),pid()|atom()}}
		 | {outside_process,{any()|pid(),pid()|atom(),any()}}.

-type actionfun() ::
	fun((any()) -> any()).

-type contextfun() ::
	fun((context()) -> any()).

-type statefun() ::
	fun((context(),any()) -> any()).

-type deferfun() ::
	fun((any(),any(),context()) -> boolean()).

-type rcv_triggerfun() ::
	fun((any(),context(),any()) -> {true,actionfun()} | false).

-type data_triggerfun() ::
	fun((context(),any()) -> {true,actionfun()} | false).

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
	  defer=void :: 'void' | 'all' | 'none' | deferfun(),
	  %% Entry action
	  entry=void :: 'void' | statefun(),
	  %% Do action
	  do=void :: 'void' | contextfun(),
	  %% Exit action
	  exit=void :: 'void' | statefun()
	}).

-type option() :: {discard_is_error,boolean()}
		| {discard_is_default,boolean()}.

