% Traction SM
-module(traction).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

init(_Arg) ->
    {stopped, void}.

state(stopped) ->
    #uml_state
    {name = stopped,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   moving,
                 guard       =
                    fun (enable, _Process, State) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    };
    
state(moving) ->
    #uml_state
    {name = moving,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   breaking,
                 guard       =
                    fun (disable, _Process, State) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ],
		do=
			fun(_Process, State) ->
				io:format("Moving train...~n"),
				State
			end
    };


state(breaking) ->
    #uml_state
    {name = breaking,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   stopped,
                 guard       =
                    fun(Process, T) ->
                    	case uml:read(Process, speed) of
                        	0 ->
                        		{true, fun(State) ->
                        					uml:signal(T, trainStopped),
					      					State
                                       end};
                            false -> false
                            end
						 end}
            ],
		do= 
			fun(_Process, State) ->
				io:format("Breaking train...~n"),
				State
			end
    }.

