% DriverButton SM
-module(driverButton).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-export([init/1,state/1]).

init(TCMS) ->
    {notPressed, TCMS}.

state(notPressed) ->
    #uml_state
    {name = notPressed,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   pressed_entry,
                 guard       =
                    fun (press, Process, TCMS) ->
                        {true,
                         fun (TCMS) ->
                            uml:assign(Process, ledStatus, true),
                            TCMS
                         end};
                        (_, _, _) -> false
                    end}
            ]
    };

state(pressed_entry) ->
    #uml_state
    {name = pressed_entry,
        type = 'read',
        transitions=
            [
                #transition
                {type        =   'read',
                 next_state  =   pressed,
                 guard       =
                    fun (Process, TCMS) ->
                        {true,
                         fun (TCMS) ->
                            uml:signal(TCMS, enableDoors), % Send signal
                            TCMS
                         end}
                    end}
            ]
    };
    

state(pressed) ->
    #uml_state
    {name = pressed,
        type = 'receive',
        transitions=
            [
                #transition
                {type        =   'receive',
                 next_state  =   notPressed,
                 guard       =
                    fun (switchOff, Process, TCMS) ->
                        {true,
                         fun (TCMS) ->
                            uml:assign(Process, ledStatus, false),
                            TCMS
                         end};
                        (_, _, _) -> false
                    end}
            ]
    }.    
