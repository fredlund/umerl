% TCMS SM
-module(tcms).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

init(Doors) ->
    {movingTrain, Doors}.

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
                    fun (Process, Doors) ->
                        case uml:read(Process, speed) of
			  0  -> {true, fun(State) -> State end};
			  _ -> false
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
                    fun (enableDoors, Process, Doors) ->
                        {true,
                         fun (State) -> 
                            uml:signal(TR, disable),
                            uml:assign(Process, i, 0),
			    Doors
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
                        fun(Process, _) ->
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
                        fun(Process, _) ->
                            if 
                                uml:read(Process, i) == uml:read(Process, doorLength) ->
                                    {true, fun(State) ->
                                                uml:signal(DB, switchOff),
					       State
                                           end};
                                true ->
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
                    fun (Process, _State) ->
                        {true, 
                         fun (Process) ->
                            Counter = uml:read(Process, i),
			     lists:foreach
			       (fun (Door) -> uml:signal(Door,enable) end,
				Doors),
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
                 next_state  =   disabledDoor_entry,
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
                 %%XXX This transition may represent a TOUT expiration
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
    }.

