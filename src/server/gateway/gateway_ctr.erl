-module(gateway_ctr).
-behaviour(gen_server).

-include("log.hrl").


-export([start_link/0, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_call/3, handle_info/2]).


-record(state,{index}).

start_link() ->
	{ok, Index} = application:get_env(index),
	{ok, Ip} = application:get_env(ip),
	{ok, Port} = application:get_env(port),	
	{ok, MaxConnection} = application:get_env(max_connection_num),
    gen_server:start_link({global, {gateway_ctr,Index}}, ?MODULE, [Index,Ip,Port,MaxConnection], []).


stop() ->
    gen_server:cast(?MODULE, stop).


init([Index,Ip,Port,MaxConnection]) ->
	?log_trace("gateway_svr init,index=~p,ip=~p,port=~p,max_connection=~p",[Index,Ip,Port,MaxConnection]),
	server_tools:regist_pt(),
	Ret =
	try
		server_tools:call_persist({global, login}, {regist_gateway,Index,MaxConnection,Ip,Port}, 1000)
	catch E:R -> {error, {E,R}}
	end,
	
	case Ret of
		{ok, registered} -> start_listener(Index, Port, MaxConnection);
		Other ->
			?log_warning("regist gateway failed,index=~p,ip=~p,port=~p,reason=~p", [Index,Ip,Port,Other]),
			exit({start_gateway_failed, Other})
	end,
	{ok, #state{index=Index}}.

handle_cast(stop, State) ->
    {stop, shutdown, State};

handle_cast(_Request, State) ->
	?log_warning("unknown request, ~p", [_Request]),
	{noreply, State}.

handle_call({apply, Module, Func, Args}, _From, State) ->
	Ret = 
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> {call_failed, {E,R,erlang:get_stacktrace()}}
	end,
	{reply, Ret, State};
handle_call(get_conns_num, _From, State) ->
	ConnSupPid = ranch_server:get_connections_sup(gateway),
	Num = ranch_conns_sup:active_connections(ConnSupPid),
	{reply, Num, State};
handle_call(_Request, _From, State) ->   
	?log_warning("unknown call, ~p", [_Request]),
    {reply, ok, State}.

handle_info(_Info, State) ->
	?log_warning("unhandled info:~p", [_Info]),
	{noreply, State}.

terminate(_Reason, State) ->
	?log_trace("gateway terminate,reason=~p",[_Reason]),
	stop_listener(),
	server_tools:call_persist({global, login}, {unregist_gateway,State#state.index}, 1000, 1),
	ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.


start_listener(Index, Port, MaxConnection) ->
	%% 实际最大允许的连接数为max(NbAcceptors,MaxConnection)
	NbAcceptors = 100,
	{ok, _} = ranch:start_listener(gateway, NbAcceptors,
		ranch_tcp, [{port, Port},{max_connections, MaxConnection},{shutdown,infinity}],
								    gateway_protocol, [{index, Index}]).

stop_listener() ->
	ranch:stop_listener(gateway).

