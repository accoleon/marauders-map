%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc Miscellaneous Library Functions
%%
%% This module provides frequently-used functions to the application.
-module(mm_misc).
-author("Kevin Xu").
-export([timestamp/1]).

%% @doc Calculates a timestamp based on system timestamp in microseconds with 2 levels of detail: seconds or microseconds
-spec timestamp(Level) -> integer() when
	Level :: secs | microsecs.
timestamp(Level) ->
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	case Level of
		secs ->
			MegaSecs * 1000000 + Secs;
		microsecs ->
			MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs
	end.