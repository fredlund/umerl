% LimitSensor SM
-module(limitSensor).
-include("../../src/records.hrl").
-compile(export_all).

init(_Arg) ->
    {idle, void}.

state(idle) ->
    #uml_state
    {name = idle,
     	type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   activated,
                 guard       =
                    fun (activate, _Process, _State) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    };

state(activated) ->
    #uml_state
    {name = activated,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   idle,
                 guard       =
                    fun (deactivate, _Process, _State) ->
                        {true,
                         fun (X) -> X end};
                        (_, _, _) -> false
                    end},
                #transition
				{type        =   'read',
                 next_state  =   limitReached,
                 guard       =
                        fun(Process, _) ->
                            case uml:read(Process, isLimitReached) of
                                true ->
                                    {true, fun(_State) ->
                                                uml:signal(D, limitReached)
                                           end};
                                false ->
                                    false
                            end
						 end
                    }
            ]
    };

state(limitReached) ->
    #uml_state
    {name = limitReached,
     	type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   activated,
                 guard       =
                    fun (ack, _Process, _State) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    }.

