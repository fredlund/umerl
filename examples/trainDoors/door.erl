% Door SM
-module(door).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").

-export([init/1,state/1]).

init({T, OpenSensor, CloseSensor, ObsSensor}) ->
    {closedAndDisabled_entry, {T, OpenSensor, CloseSensor, ObsSensor}}.

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
                    fun (Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true, 
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(T, {notify, uml:self(Process), 'closed'}),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (enable, Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:assign(Process, status, 'enabled'),
                            {T, OpenSensor, CloseSensor, ObsSensor}
                         end};
                        (_, _, _) -> false
                    end
                },
                #transition
                {type        =   'receive',
                 next_state  =   closedAndDisabled_entry,
                 is_internal = true,
                 guard       =
                    fun (disable, Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun (X) -> X end};
                        (_, _, _) -> false
                    end
                }
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
                    fun (Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true, 
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(T, {notify, uml:self(Process), 'enabled'}),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (buttonPressed, Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:assign(Process, status, 'opening'),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true, 
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(T, {notify, uml:self(Process), 'opening'}),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (_Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,fun (X) -> X end}
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
                    fun (limitReached, _Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(OpenSensor, ack),
                            {T, OpenSensor, CloseSensor, ObsSensor}
                         end};
                         (_, _, _) -> false
                    end}
            ],
         do = 
            fun(_Process) ->
                io:format("Opening door...~n")
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
                    fun (Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true, 
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(T, {notify, uml:self(Process), 'opened'}),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (disable, Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:assign(Process, status, 'closing'),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true, 
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(T, {notify, uml:self(Process), 'closing'}),
                            {T, OpenSensor, CloseSensor, ObsSensor}
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
                    fun (_Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
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
                    fun (limitReached, _Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(CloseSensor, ack),
                            {T, OpenSensor, CloseSensor, ObsSensor}
                         end};
                         (_, _, _) -> false
                    end},
                #transition
                {type        =   'receive',
                 next_state  =   opening_entry,
                 guard       =
                    fun (obsDetected, _Process, {T, OpenSensor, CloseSensor, ObsSensor}) ->
                        {true,
                         fun ({T, OpenSensor, CloseSensor, ObsSensor}) ->
                            uml:signal(ObsSensor, ack),
                            {T, OpenSensor, CloseSensor, ObsSensor}
                         end};
                         (_, _, _) -> false
                    end}

            ]
    }.

