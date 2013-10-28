% LimitSensor SM
-module(limitSensor).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

init(D) ->
    {idle, D}.

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
                    fun (activate, _Process, D) ->
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
                    fun (deactivate, _Process, D) ->
                        {true,
                         fun (X) -> X end};
                        (_, _, _) -> false
                    end},
                #transition
				{type        =   'read',
                 next_state  =   limitReached,
                 guard       =
                        fun(Process, D) ->
                            case uml:read(Process, isLimitReached) of
                                true ->
                                    {true, fun(D) ->
                                                uml:signal(D, limitReached),
                                                D
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
                    fun (ack, _Process, D) ->
                        {true,
                        fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    }.

