-module(scene_ctr).
-behaviour(gen_server).
-export([start_link/0, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3 ]).
-include("common.hrl").

-record(state, {id = 0}).
start_link() ->
	{ok, Id} = application:get_env(index),
	{ok, Maxscene} = application:get_env(maxscene),	
	db:set_config(idx,Id),
	
    process_flag(trap_exit, true),
    gen_server:start_link(?MODULE, [Id,Maxscene], []).
stop() ->
    gen_server:cast(?MODULE, stop).

init([Idx,MaxScene]) ->
	?debug("scene_ctr_add,idx=~p", [Idx]),
	case server_tools:call_persist({global, scene_mng}, {scene_ctr_add, Idx, self(), MaxScene}, 2000) of
		ok -> {ok, #state{id=Idx}};
		Other ->
			?log_warning("scene_ctr_add failed,idx=~p,error=~p", [Idx,Other])
	end.

terminate(_Reason, #state{id=Idx}) ->
	server_tools:send_to_scene_mng({scene_ctr_del, Idx}),
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(_Request, _From, State) -> 
	?log_warning("unknown call, ~p", [_Request]),
    {reply, ok, State}.

handle_info(_Request, State) ->
	?log_warning("unhandled info:~p", [_Request]),
    {noreply, State}.

%% handle_cast({req_create_scene,{Key,UsrInfoList,NeedUsrInfo,Scene,SceneData}},State) ->
handle_cast({req_create_scene,{Key,UsrInfoList,Scene,SceneData}},State) ->	
	scene_sup:add(Key,UsrInfoList,Scene,SceneData),
	{noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
	?log_warning("unknown cast, ~p", [_Request]),
    {noreply, State}.



