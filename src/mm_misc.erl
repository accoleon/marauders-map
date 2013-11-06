%% Miscellaneous shared functions
-module(mm_misc).
-author("Kevin Xu").
-export([timestamp/1]).

%% Calculates a timestamp based on system timestamp in microseconds with 2 levels of detail: seconds or microseconds
timestamp(Level) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	case Level of
		secs ->
			MegaSecs * 1000000 + Secs;
		microsecs ->
			MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs
	end.