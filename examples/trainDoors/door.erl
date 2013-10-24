% Door SM
-module(door).
-include("../../src/records.hrl").
-compile(export_all).

init(_Arg) ->
    {closedAndDisabled_entry, void}.

state(closedAndDisabled_entry) ->
    #uml_state
    {name = closedAndDisabled_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   closedAndDisabled,
                 guard       =
                    fun (_Process, _State) ->
                        {true, 
                         fun (_State) ->
                            uml:signal(T, {notify, self, 'closed'})
                         end}
                    end}
            ]
    };


state(closedAndDisabled) ->
    #uml_state
    {name = closedAndDisabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   enabled_entry,
                 guard       =
                    fun (enable, Process, _State) ->
                        {true,
                         fun (Process) ->
                            uml:assign(Process, status, 'enabled')
                         end};
                        (_, _, _) -> false
                    end
                },
                #transition
                {type        =   'receive',
                 next_state  =   closedAndDisabled_entry,
                 is_internal = true,
                 guard       =
                    fun (disable, Process, _State) ->
                        {true,
                         fun (X) ->
                            X
                         end};
                        (_, _, _) -> false
                    end
                },

            ]
    };

state(enabled_entry) ->
    #uml_state
    {name = enabled_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   enabled,
                 guard       =
                    fun (_Process, _State) ->
                        {true, 
                         fun (_State) ->
                            uml:signal(T, {notify, self, 'enabled'})
                         end}
                    end}
            ]
    };


state(enabled) ->
    #uml_state
    {name = enabled,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   opening_entry,
                 guard       =
                    fun (buttonPressed, Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:assign(Process, status, 'opening')
                         end};
                        (_, _, _) -> false
                    end}
            ]
    };

state(opening_entry) ->
    #uml_state
    {name = opening_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   opening,
                 guard       =
                    fun (_Process, _State) ->
                        {true, 
                         fun (_State) ->
                            uml:signal(T, {notify, self, 'opening'})
                         end}
                    end}
            ]
    };

state(opening) ->
    #uml_state
    {name = opening,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   wait4opening,
                 guard       =
                    fun (_Process, _State) ->
                        {true,
                         fun (X) ->
                            X
                         end}
                    end}
            ]
     };

state(wait4opening) ->
    #uml_state
    {name = wait4opening,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   opened_entry,
                 guard       =
                    fun (limitReached, _Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:signal(OpenSensor, ack)
                         end};
                         (_, _, _) -> false
                    end}
            ],
         do = 
            fun(_Process, State) ->
                io:format("Opening door...~n"),
                State
            end
    };

state(opened_entry) ->
    #uml_state
    {name = opened_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   opened,
                 guard       =
                    fun (_Process, _State) ->
                        {true, 
                         fun (_State) ->
                            uml:signal(T, {notify, self, 'opened'})
                         end}
                    end}
            ]
    };


state(opened) ->
    #uml_state
    {name = opened,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   closing_entry,
                 guard       =
                    fun (disable, Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:assign(Process, status, 'closing')
                         end};
                        (_, _, _) -> false
                    end}
            ]
    };

state(closing_entry) ->
    #uml_state
    {name = closing_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   closing,
                 guard       =
                    fun (_Process, _State) ->
                        {true, 
                         fun (_State) ->
                            uml:signal(T, {notify, self, 'closing'})
                         end}
                    end}
            ]
    };

state(closing) ->
    #uml_state
    {name = closing,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   wait4closing,
                 guard       =
                    fun (_Process, _State) ->
                        {true,
                         fun (X) ->
                            X
                         end}
                    end}
            ]
     };

state(wait4closing) ->
    #uml_state
    {name = wait4closing,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   closedAndDisabled_entry,
                 guard       =
                    fun (limitReached, _Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:signal(CloseSensor, ack)
                         end};
                         (_, _, _) -> false
                    end},
                #transition
                {type        =   'receive',
                 next_state  =   opening_entry,
                 guard       =
                    fun (obsDetected, _Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:signal(ObsSensor, ack)
                         end};
                         (_, _, _) -> false
                    end}

            ],
         do = 
            fun(_Process, State) ->
                io:format("Closing door...~n"),
                State
            end
    }.

