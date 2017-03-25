-module(agent_ctr).
-behaviour(gen_server).

-export([start_link/0, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3 ]).

-include("log.hrl").

-record(state, {id = 0}).

start_link() ->
    {ok, Id} = application:get_env(index),
	{ok, Maxagent} = application:get_env(maxagent),	
	db:set_config(idx,Id),	
	
    process_flag(trap_exit, true),
    gen_server:start_link(?MODULE, [Id,Maxagent], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([Id,Maxagent]) ->
	?debug("agent_ctr_start"),
	Rep = server_tools:call_persist({global, agent_mng}, {agent_ctr_start, {Id,self(),Maxagent}}, 2000),
	?debug("agent_ctr_start succ,reply=~p", [Rep]),
	%server_tools:send_to_agent_mng({agent_ctr_start, {Id,self(),Maxagent}}),
    {ok, #state{id=Id}}.

terminate(_Reason, #state{id = Id}) ->
	server_tools:send_to_agent_mng({agent_ctr_close, Id}),
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

handle_info(Request, State) ->
	?debug("Request=~p,State=~p",[Request,State]),
    {noreply, State}.

handle_cast({let_it_in, Sid,Ip,Seq,Aid,Uid}, #state{id = Id} = State) ->
    agent_sup:add(Sid,Ip,Seq,Aid,Uid,Id),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.



