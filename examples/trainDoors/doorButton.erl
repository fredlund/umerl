% DoorButton SM
-module(doorButton).
-include("../../src/records.hrl").
-include("../../src/umerl.hrl").
-compile(export_all).

init(Door) ->
    {notPressed, Door}.

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
                    fun (press, Process, Door) ->
                        {true,
                         fun (State) ->
			     uml:assign(Process, ledStatus, true),
			     State
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
                    fun (_Process, Door) ->
                        {true,
                         fun (State) ->
			     uml:signal(Door, buttonPressed),
			     State
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
                    fun (switchOff, Process, Door) ->
                        {true,
                         fun (State) ->
                            uml:assign(Process, ledStatus, false),
                            State
                         end};
                        (_, _, _) -> false
                    end}
            ]
    }.    
