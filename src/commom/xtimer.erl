-module(xtimer).

-include("log.hrl").

-export([start_timer/2, start_timer_persist/2, cancel_timer/1, on_timer/2]).


-spec start_timer(Timeout, CallbackInfo) -> TimerIndex when
		  Timeout :: integer(),
		  CallbackInfo :: {Module,Func} | {Module,Func,Args},
		  Module :: module(),
		  Func :: atom(),
		  Args :: term(),
		  TimerIndex :: integer().
start_timer(Timeout, CallbackInfo) ->
	start_timer(Timeout, once, CallbackInfo).

-spec start_timer_persist(Timeout, CallbackInfo) -> TimerIndex when
		  Timeout :: integer(),
		  CallbackInfo :: {Module,Func} | {Module,Func,Args},
		  Module :: module(),
		  Func :: atom(),
		  Args :: term(),
		  TimerIndex :: integer().
start_timer_persist(Timeout, CallbackInfo) ->
	start_timer(Timeout, {persist, Timeout}, CallbackInfo).

%% cancel_timer(TimerIndex) -> Time | false
cancel_timer(Idx) ->
	case get_ref(Idx) of
		{ok, Ref} ->
			del_ref(Idx),
			erlang:cancel_timer(Ref);
		_ -> false
	end.


new_timer_index() ->
	Idx = 
	case get(xtimer_index) of
		Val when erlang:is_integer(Val) -> Val;
		_ -> 1
	end,
	put(xtimer_index, Idx+1),
	Idx.

add_ref(Idx, Ref) ->
	TimerTable = 
	case get(xtimer_table) of
		undefined -> dict:new();
		D -> D
	end,
	put(xtimer_table, dict:store(Idx, Ref, TimerTable)).

del_ref(Idx) ->
	put(xtimer_table, dict:erase(Idx, get(xtimer_table))).

get_ref(Idx) ->
	dict:find(Idx, get(xtimer_table)).

start_timer(Timeout, Type, CallbackInfo) ->
	{Module, Fun, Args, ArgNum} = 
	case CallbackInfo of
		{M, F, A} when is_list(A) -> {M, F, A,length(A)};
		{M, F, A}  -> {M, F, [A], 1};
		{M, F} -> {M, F, [], 0}
	end,
	case lists:member({Fun, ArgNum}, Module:module_info(exports)) of
		true ->
			Idx = new_timer_index(),
			Ref = erlang:start_timer(Timeout, self(), {xtimer_callback, {Type, Idx, {Module, Fun, Args}}}),
			add_ref(Idx, Ref),
			Idx;
		_ ->
			erlang:error(invalid_callback)
	end.
	

on_timer(_TimerRef, {once, Idx, {Module, Fun, Args}}) ->
	del_ref(Idx),
	erlang:apply(Module, Fun, Args);
	
on_timer(_TimerRef, {{persist, Timeout}, Idx, CallbackInfo = {Module, Fun, Args}}) ->
	Ref = erlang:start_timer(Timeout, self(), {xtimer_callback, {{persist, Timeout}, Idx, CallbackInfo}}),
	add_ref(Idx, Ref),
	erlang:apply(Module, Fun, Args).
	
