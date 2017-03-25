-module(world_svr).
-behaviour(gen_server).
-export([start_link/2, stop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(state, {key,lastontime=0}).

start_link(Key,Moudle) ->
	?debug("Info:start world server,key=~p,moudle=~p", [Key,Moudle]),
	%process_flag(trap_exit, true),
	{ok, Pid} = gen_server:start_link({global, Key}, ?MODULE, [Key,Moudle], []),
	?debug("key=~p,Pid=~p",[Key,Pid]),
	{ok, Pid}.

stop(Key) ->
	gen_server:cast({global, Key}, stop).

init([Key,Moudle]) -> 
	put(world_moudle,Moudle),
	put(world_key,Key),
%	random:seed(erlang:now()),
	Ret = do_init(),
	
	erlang:send_after(1000, self(), timercast),
	Ret.

handle_call({apply, Module, Func, Args}, _From, State) ->
	Ret = 
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> {call_failed, {E,R,erlang:get_stacktrace()}}
	end,
	{reply, Ret, State};
handle_call(Msg, _From, State) ->
	?debug("handle_call:~p,~p",[Msg,State]),
	Reply = try 
		Moudle = get_moudle(),
		Moudle:do_call(Msg)
	catch 
		E:R -> ?log_error("world handle_call error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()]),error
	end,
	{reply, Reply, State}.  


handle_cast(stop, State) ->
	{stop, normal, State}; 
handle_cast({apply, Module, Func, Args}, State) ->
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> ?log_error("apply error E=~p,R=~p,stack=~p", [E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
handle_cast(Msg, State) ->
	?debug("handle_cast:~p,~p",[Msg,State]),
	try 
		Moudle = get_moudle(),
		Moudle:do_msg(Msg)
	catch 
		E:R -> ?log_error("world handle_cast error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,		   
	{noreply, State}.
handle_info(timercast, State) ->
	try
		Now = util:longunixtime(),
		Moudle = get_moudle(),
		Last = Moudle:do_time(Now),
		case erlang:is_integer(Last) of
			?TRUE ->
				erlang:send_after(Last, self(), timercast);
			_ -> ?SKIP
		end
	catch 
		E:R -> ?log_error("world handle_info  error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
handle_info({timeout, TimerRef, {xtimer_callback, CallBackInfo}}, State) ->
	try
		xtimer:on_timer(TimerRef, CallBackInfo)
	catch E:R ->?log_error("timeout error,data=~p,E=~p,R=~p,stack=~p",[CallBackInfo,E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
%handle_info({http, Msg}, State) ->
%	fun_http:async_http_response(Msg),
%	{noreply, State};
handle_info(Info, State) ->
	try 
		Moudle = get_moudle(),
		Moudle:do_info(Info)
	catch 
		E:R -> ?log_error("world handle_info  error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	{noreply, State}.

terminate(_Reason, State) ->
	?log_error("Info:public server stoped!Reason=~p,State=~p", [_Reason, State]),
	try 
		Moudle = get_moudle(),
		Moudle:do_close()
	catch 
		E:R -> ?log_error("world terminate  error E=~p,R=~p",[E,R])
	end,	  
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_key() -> get(world_key).
get_moudle() -> get(world_moudle).

do_init() ->
	Key = get_key(),
	Moudle = get_moudle(),
	Moudle:do_init(),
	{ok, #state{key=Key}}.
