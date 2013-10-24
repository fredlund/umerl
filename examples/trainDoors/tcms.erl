% TCMS SM
-module(tcms).
-include("../../src/records.hrl").
-compile(export_all).

init(_Arg) ->
    {movingTrain, void}.

state(movingTrain) ->
    #uml_state
    {name = movingTrain,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   idle,
                 guard       =
                    fun (Process, _State) ->
                        if 
                            uml:read(Process, speed) == 0  ->
                                {true, fun(X) ->
                                        X
                                       end};
                            true -> 
                                false
                        end
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
                    fun (enableDoors, Process, _State) ->
                        {true,
                         fun (Process) -> 
                            uml:signal(TR, disable),
                            uml:assign(Process, i, 0)
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
                 next_state  =   enableDoor_entry,
                 guard       =
                        fun(Process, _) ->
                            if 
                                uml:read(Process, i) < uml:read(Process, doorLength) ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                true ->
                                    false
                            end
						 end
                },
                #transition
                {type        =   'read',
                 next_state  =   doorsEnabled,
                 guard       =
                        fun(Process, _) ->
                            if 
                                uml:read(Process, i) == uml:read(Process, doorLength) ->
                                    {true, fun(_State) ->
                                                uml:signal(DB, switchOff)
                                           end};
                                true ->
                                    false
                            end
						 end
                }

            ]
    };

state(enableDoor_entry) ->
    #uml_state
    {name = enabledDoor_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   enableDoor,
                 guard       =
                    fun (Process, _State) ->
                        {true, 
                         fun (Process) ->
                            Counter = uml:read(Process, i),
                            uml:signal(Doors, enable), % XXX This must be sent to doors[i] ¿?
                            uml:assign(Process, i, Counter + 1)
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
                    fun ({notify, D, DS}, _Process, _State) ->
                        {true,
                         fun (Process, {D, DS}) ->
                            io:format("Processing notification~n"),
                            uml:assign(Process, D, DS) % XXX How we can handle this??
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
                    fun (disableDoors, _Process, _State) ->
                        {true,
                         fun (Process) -> 
                            uml:assign(Process, i, 0)
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
                 next_state  =   disableDoor_entry,
                 guard       =
                        fun(Process, _) ->
                            if 
                                uml:read(Process, i) < uml:read(Process, doorLength) ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                true ->
                                    false
                            end
						 end
                },
                #transition
                {type        =   'read',
                 next_state  =   doorsDisabled,
                 guard       =
                        fun(Process, _) ->
                            if 
                                uml:read(Process, i) == uml:read(Process, doorLength) ->
                                    {true, fun(X) ->
                                                X
                                           end};
                                true ->
                                    false
                            end
						 end
                }

            ]
    };


state(disableDoor_entry) ->
    #uml_state
    {name = disabledDoor_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   disableDoor,
                 guard       =
                    fun (Process, _State) ->
                        {true, 
                         fun (Process) ->
                            Counter = uml:read(Process, i),
                            uml:signal(Doors, disable), % XXX This must be sent to doors[i] ¿?
                            uml:assign(Process, i, Counter + 1)
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
                    fun ({notify, D, DS}, _Process, _State) ->
                        {true,
                         fun (Process, {D, DS}) ->
                            io:format("Processing notification~n"),
                            uml:assign(Process, D, DS) % XXX How we can handle this??
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
                    fun ({notify, D, DS}, _Process, _State) ->
                        {true,
                         fun (Process, {D, DS}) ->
                            io:format("Processing notification~n"),
                            uml:assign(Process, D, DS), % XXX How we can handle this??
                            uml:assign(Process, i, 0)
                         end};
                         (_, _, _) -> false
                    end
                 },
                 #transition
                {type        =   'read',
                 next_state  =   movingTrain,
                 guard       =   %XXX This transition may represent a TOUT expiration
                 guard       =
                    fun (timeoutEXPIRED, Process, _State) ->
                        {true,
                         fun (X) ->
                            uml:signal(TR, enable)
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
    };

