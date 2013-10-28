% DriverButton SM
-module(driverButton).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

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
                         fun (Process, TCMS) ->
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
                    fun (_M, _Process, TCMS) ->
                        {true,
                         fun (TCMS) ->
                            uml:signal(TCMS, enableDoors), % Send signal
                            TCMS
                         end};
                        (_, _, _) -> false
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
                         fun (Process, TCMS) ->
                            uml:assign(Process, ledStatus, false),
                            TCMS
                         end};
                        (_, _, _) -> false
                    end}
            ]
    }.    
