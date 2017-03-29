-module(agent).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3,on_time/0]).

-include("common.hrl").

-record(state, {aid = 0, uid = 0, sid = 0,loadstate=0,ip=0,mapHid=0}).
-define(AGENT_TIMER,1000).

start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

stop() ->
    gen_server:cast(?MODULE, stop).

init({Sid,Ip,Seq,Aid,Uid,AgentIdx}) ->
	?log_trace("agent init aid = ~p,Uid=~p Ip = ~p",[Aid,Uid,Ip]),
	server_tools:send_to_gateway(Sid, {set_val, agent_pid, self()}),
	server_tools:send_to_gateway(Sid, {set_val, uid, Uid}),
%%	random:seed(erlang:now()),
	timer:apply_after(?AGENT_TIMER, gen_server, cast, [self(), {login,Seq}]),
	fun_agent:on_init({Sid,Ip,Seq,Aid,Uid,AgentIdx}),
    {ok, #state{aid = Aid, uid = Uid ,sid = Sid,ip=Ip}}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call({apply, Module, Func, Args}, _From, State) ->
	Ret = 
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> {call_failed, {E,R,erlang:get_stacktrace()}}
	end,
	{reply, Ret, State};
handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_info({http, Msg}, State) ->
	try
		fun_http:async_http_response(Msg)
	catch E:R ->?log_error("handle_info,error,fun_http:async_http_response,E=~p,R=~p,Msg=~p",[?MODULE,E,R,Msg])
	end,
	{noreply, State};
handle_info({timeout, TimerRef, {xtimer_callback, CallBackInfo}}, State) ->
	try
		xtimer:on_timer(TimerRef, CallBackInfo)
	catch E:R ->?log_error("timeout error,data=~p,E=~p,R=~p,stack=~p",[CallBackInfo,E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
handle_info(_Request, State) ->
    {noreply, State}.

handle_cast({login,Seq}, #state{aid = Aid, uid = Uid , sid = Sid ,ip =Ip} = State) ->
	try	
		load_data(Uid),
		fun_agent:on_login(Seq),
		xtimer:start_timer_persist(?AGENT_TIMER, {?MODULE,on_time})
	catch E:R ->?log_error("login error,E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};


handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast(gateway_tcp_closed, #state{uid = Uid, sid = Sid} = State) ->    
    ?log_trace("tcp_closed, uid=~p, sid =~p ~n", [Uid, Sid]),
	try
		fun_agent:on_logout()
	catch E:R ->?log_error("gateway_tcp_closed error,E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	erlang:send_after(10000, self(), agent_out_timeout),
	%timer:apply_after(3000, gen_server, cast, [self(), stop]),
    {noreply, State};

handle_cast({apply, Module, Func, Args}, State) ->
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> ?log_error("apply error E=~p,R=~p,stack=~p", [E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
handle_cast(agent_out_complete, State = #state{uid = Uid}) ->
	%% 确保退出相关的流程结束后再结束进程
	?debug("agent_out_complete,uid=~p,pid=~p",[Uid, self()]),
	{stop, normal, State};
handle_cast(agent_out_timeout, State = #state{uid = Uid}) ->
	?debug("agent_out_timeout,uid=~p,pid=~p",[Uid, self()]),
	{stop, normal, State};
handle_cast(Msg, State) ->
%% 	?debug("handle_cast:~p,~p",[Msg,State]),
	try 
		fun_agent:do_msg(Msg)
	catch 
		E:R -> ?log_error("world handle_cast error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,		   
	{noreply, State}.

on_time() -> 
	try 
		Now = util:longunixtime(),
		fun_agent:do_time(Now)
	catch 
		E:R -> ?log_error("world handle_cast error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end.

load_data(Uid) ->
	case db:dirty_get(cache, Uid) of
		[] -> 					
			do_load_data(Uid),
			db:dirty_put(#cache{uid = Uid, cached = 1});				
		_ -> skip
	end.
do_load_data(Uid) ->
	F = fun({Tab,Field}) ->
				Ret = dbm_worker:work({load, Tab, Field, list_to_binary(integer_to_list(Uid))}),                                                                                                                                                                                                                                                                                                                                                  
				on_load_complete(Tab, Uid, Ret)
		end,
	lists:foreach(F,?PlyLoadList).

on_load_complete(_Tab, _Uid, _Ret) ->
	ok.

