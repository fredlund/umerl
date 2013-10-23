% DriverButton SM
-module(driverButton).
-include("../../src/records.hrl").
-compile(export_all).

init(_Arg) ->
    {notPressed, void}.

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
                    fun (press, Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:assign(Process, ledStatus, true) % This is the effect, an internal method called
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
                    fun (_M, _Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:signal(TCMS, enableDoors) % Send signal
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
                    fun (switchOff, Process, _State) ->
                        {true,
                         fun (_State) ->
                            uml:assign(Process, ledStatus, false)
                         end};
                        (_, _, _) -> false
                    end}
            ]
    }.    
