%% Entry is always executed, and cannot be preempted.
%% After Entry has concluded, we do a check to see if
%% a message is receivable, if so we preempt the do,
%% otherwise do is executed (without checking again, maybe, or
%% continuously checking rather).
%% Exit is evaluated upon termination.
%% These activities are not atomic with respect to other machines.

-record(uml_state,
	{name,
	 transitions,
	 internal_transitions,
	 type=void,
	 defers=[],  %% A list or all
	 entry=void,
	 do=void,
	 exit=void}).

-record(transition,
	{type=void,
	 guard,
	 next_state}).
	 
