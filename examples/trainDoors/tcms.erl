% TCMS SM
-module(tcms).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

init({Doors, TR, DB}) ->
    {idle, {Doors, TR, DB}}.

state(movingTrain) ->
    #uml_state
    {name = movingTrain,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   stoppingTrain,
                 guard       =
                    fun (stopTrain, Process, {Doors, TR, DB}) ->
                        {true,
                         fun (Process, {Doors, TR, DB}) -> 
                            uml:signal(TR, disable),
			    			{Doors, TR, DB}
                         end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    };
    
state(stoppingTrain) ->
    #uml_state
    {name = stoppingTrain,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   idle,
                 guard       =
                    fun (trainStopped, Process, {Doors, TR, DB}) ->
                        {true,
                         fun (X) -> X end 
						 };
                        (_, _, _) -> false
                    end}
            ]
    };

state(idle) ->
    #uml_state
    {name = idle,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   enablingDoors,
                 guard       =
                    fun (enableDoors, Process, {Doors, TR, DB}) ->
                        {true,
                         fun (Process, {Doors, TR}) -> 
                            uml:assign(Process, i, 0),
			    			{Doors, TR, DB}
                         end 
						 };
                        (_, _, _) -> false
                    end}
             ]
    };

state(enablingDoors) ->
    #uml_state
    {name = enablingDoors,
        type = 'read',
        transitions=
            [
                #transition
				{type        =   'read',
                 next_state  =   enabledDoor_entry,
                 guard       =
                        fun(Process, {Doors, TR, DB}) ->
                            case uml:read(Process, i) < uml:read(Process, doorLength) of
			      				true ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                false ->
                                    false
                            end
						 end
                },
                #transition
                {type        =   'read',
                 next_state  =   doorsEnabled,
                 guard       =
                        fun(Process, {Doors, TR, DB}) ->
                            case uml:read(Process, i) == uml:read(Process, doorLength) of
                            	true ->
                            		{true, fun(State) ->
                                                uml:signal(DB, switchOff),
					      						State
                                           end};
                                false ->
                                    false
                            end
						 end
                }

            ]
    };

state(enabledDoor_entry) ->
    #uml_state
    {name = enabledDoor_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   enableDoor,
                 guard       =
                    fun (Process, Doors) ->
                        {true, 
                         fun (Process, {Doors, TR, DB}) ->
                            Counter = uml:read(Process, i),
			     			Door = lists:nth(i, Doors),
			     			uml:signal(Door, enable),
                            uml:assign(Process, i, Counter + 1),
                            {Doors, TR, DB}
                         end}
                    end}
            ]
    };

state(enableDoor) ->
    #uml_state
    {name = enableDoor,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   enablingDoors,
                 guard       =
                    fun ({notify, D, DS}, _Process, {Doors, TR, DB}) ->
                        {true,
                         fun (State) ->
                            io:format("Processing notification~n"),
                            State
                         end};
                         (_, _, _) -> false
                    end}
            ]
    };

state(doorsEnabled) ->
    #uml_state
    {name = doorsEnabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   disablingDoors,
                 guard       =
                    fun (disableDoors, _Process, {Doors, TR, DB}) ->
                        {true,
                         fun (Process, State) -> 
                            uml:assign(Process, i, 0),
                            State
                         end}; 
                         (_, _, _) -> false
                    end}
            ]
    };

state(disablingDoors) ->
    #uml_state
    {name = disablingDoors,
        type = 'read',
        transitions=
            [
                #transition
				{type        =   'read',
                 next_state  =   disabledDoor_entry,
                 guard       =
                        fun(Process, {Doors, TR, DB}) ->
                            case uml:read(Process, i) < uml:read(Process, doorLength) of
                            	true ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                false ->
                                    false
                            end
						 end
                },
                #transition
                {type        =   'read',
                 next_state  =   doorsDisabled,
                 guard       =
                        fun(Process, {Doors, TR, DB}) ->
                            case uml:read(Process, i) == uml:read(Process, doorLength) of
                            	true ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                false ->
                                    false
                            end
						 end
                }

            ]
    };


state(disabledDoor_entry) ->
    #uml_state
    {name = disabledDoor_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   disableDoor,
                 guard       =
                    fun (Process, {Doors, TR, DB}) ->
                        {true, 
                         fun (Process, State) ->
                            Counter = uml:read(Process, i),
                            Door = lists:nth(i, Doors),
			     			uml:signal(Door, disable),
                            uml:assign(Process, i, Counter + 1),
                            State
                         end}
                    end}
            ]
    };

state(disableDoor) ->
    #uml_state
    {name = disableDoor,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   disablingDoors,
                 guard       =
                    fun ({notify, D, DS}, _Process, {Doors, TR, DB}) ->
                        {true,
                         fun (State) ->
                            io:format("Processing notification~n"),
                            State
                         end};
                         (_, _, _) -> false
                    end}
            ]
    };

state(doorsDisabled) ->
    #uml_state
    {name = doorsDisabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   disablingDoors,
                 guard       =
                    fun ({notify, D, DS}, _Process, {Doors, TR, DB}) ->
                        {true,
                         fun (State) ->
                            io:format("Processing notification~n"),
                            State
                         end};
                         (_, _, _) -> false
                    end
                 },
                 #transition
                {type        =   'read',
                 next_state  =   movingTrain,
                 %%XXX This transition may represent a TOUT expiration
                 guard       =
                    fun (timeoutEXPIRED, Process, TR) ->
                        {true,
                         fun (X) ->
                            uml:signal(TR, enable),
                            X
                         end};
                        (_, _, _) -> false
                    end
                 }
            ],
        do=
            fun(_Process, State) ->
                io:format("Setting a timout...~n"), %XXX Something must be added to generate a self-signal representing the expiration of a TOUT
                State
            end
    }.

