% ObstacleSensor SM
-module(obstacleSensor).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-export([init/1,state/1]).

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
                 next_state  =   obstacleDetected,
                 guard       =
                        fun(Process, D) ->
                            case uml:read(Process, anyObstacleDetected) of
                                true ->
                                    {true, fun(D) ->
                                                uml:signal(D, obsDetected)
                                           end};
                                false ->
                                    false
                            end
						 end
                    }
            ]
    };

state(obstacleDetected) ->
    #uml_state
    {name = obstacleDetected,
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

