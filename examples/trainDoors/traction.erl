% Traction SM
-module(traction).
-include("../../src/records.hrl").
-compile(export_all).

init(_Arg) ->
    {enabled, void}.

state(enabled) ->
    #uml_state
    {name = enabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   disabled,
                 guard       =
                    fun (disable, _Process, _State) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ],
		do=
			fun(Process, State) ->
				uml:assign(Process, status, enabled),
				State
			end
    };

state(disabled) ->
    #uml_state
    {name = disabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   enabled,
                 guard       =
                    fun (enable, _Process, _State) ->
                        {true,
                         fun (X) -> X end};
                        (_, _, _) -> false
                    end}
            ],
		do= 
			fun(Process, State) ->
				uml:assign(Process, status, disabled),
				State
			end
    }.

