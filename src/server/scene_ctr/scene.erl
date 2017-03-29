-module(scene).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3]).

-include("common.hrl").

-record(state, {id = 0,type=0,start_time=0,on_time_check=0,on_time_off=0,on_time_module=0,scene_model=0,script_items=[]}).
-define(SCENE_TIMER_INTERVAL,200).


start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% init({Key,UsrInfoList,NeedUsrInfo,Scene,SceneData}) ->
%% UsrInfoList => [{Uid,Seq,Pos,_} | Next]
init({Key,UsrInfoList,Scene,SceneData}) ->
	Id = db:get_new_scene_id(),
	put(id,Id),
	put(key,Key),
	put(scene,Scene),
	put(scene_usr_info,UsrInfoList),
	put(scene_info,SceneData),
	?debug("scene init Scene=~p,ID = ~p",[Scene,Id]),
%%	random:seed(erlang:now()),
%% 	gen_server:cast({global, scene_mng}, {scene_add,{Id,Key,Scene,self(),db:get_config(idx),UsrInfoList,NeedUsrInfo}}),
	gen_server:cast({global, scene_mng}, {scene_add,{Id,Key,Scene,self(),db:get_config(idx),UsrInfoList}}),
	timer:apply_after(?SCENE_TIMER_INTERVAL, gen_server, cast, [self(), {load}]),	
	
	try 
		fun_scene:on_init()
	catch 
		E:R -> ?log_error("scene init error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	
	?debug("scene inited Scene=~p,ID = ~p",[Scene,Id]),
	{GetOff,GetOntime} = case data_scene_config:get_scene(Scene) of
		{} -> {0,0};
		#st_scene_config{on_time_off=0} -> {0,0};
		#st_scene_config{on_time_off=Off,on_time=Ontime} -> {Off,util:to_atom(Ontime)}
	end,
	State = 
	case data_scene_config:get_scene(Scene) of
		#st_scene_config{sort = ?SCENE_SORT_COPY} -> 
			#state{id = Id,type =Scene,on_time_off=GetOff,on_time_module=GetOntime,on_time_check=util:longunixtime(),start_time=util:longunixtime(),scene_model = fun_scene_copy};
		#st_scene_config{sort = ?SCENE_SORT_PEACE} -> 
			#state{id = Id,type =Scene,on_time_off=GetOff,on_time_module=GetOntime,on_time_check=util:longunixtime(),start_time=util:longunixtime(),scene_model = fun_scene_outdoor};
		#st_scene_config{sort = ?SCENE_SORT_SCUFFLE} -> 
			#state{id = Id,type =Scene,on_time_off=GetOff,on_time_module=GetOntime,on_time_check=util:longunixtime(),start_time=util:longunixtime(),scene_model = fun_scene_outdoor};
		#st_scene_config{sort = ?SCENE_SORT_ARENA} -> 
			#state{id = Id,type =Scene,on_time_off=GetOff,on_time_module=GetOntime,on_time_check=util:longunixtime(),start_time=util:longunixtime(),scene_model = fun_scene_arena};		
		_ ->
			#state{id = Id,type =Scene,on_time_off=GetOff,on_time_module=GetOntime,on_time_check=util:longunixtime(),start_time=util:longunixtime(),scene_model = fun_scene_city}
	end,
	put(scene_model,State#state.scene_model),
	{ok, State}.

    

terminate(_Reason, #state{id =ID}) ->
	?debug("scene terminate,id=~p,scene=~p",[ID,get(scene)]),
	gen_server:cast({global, scene_mng},{scene_del,ID}),		
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

handle_info({timeout, TimerRef, {xtimer_callback, CallBackInfo}}, State) ->
	try
		xtimer:on_timer(TimerRef, CallBackInfo)
	catch E:R ->?log_error("timeout error,data=~p,E=~p,R=~p,stack=~p",[CallBackInfo,E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};
%% handle_info({time},  #state{type =Scene,on_time_off=_Off,on_time_module=_Ontime,on_time_check=_CheckTime,scene_model = SceneModel,script_items=_Script_items} = State) -> 	
%% 	Now = util:longunixtime(),
%% 	try
%% 		fun_scene:do_time(Scene,Now)
%% 	catch
%% 		ES:RS -> 
%% 			?log_error("scene doTimer error SceneModel=~p,E=~p,R=~p,c=~p",[SceneModel,ES,RS,erlang:get_stacktrace()])
%% 	end,
		
handle_info(Request, State) ->
	?log_warning("unknown info,info=~p",[Request]),
    {noreply, State}.

handle_cast({load}, #state{type =Scene,scene_model = SceneModel} = State) -> 
	try 
		fun_scene:on_scene_loaded(SceneModel,Scene)
	catch 
		E:R -> ?log_error("scene on_scene_loaded error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
	erlang:send_after(?SCENE_TIMER_INTERVAL, self(), {'$gen_cast', {time}}),
%%	timer:apply_after(?SCENE_TIMER, gen_server, cast, [self(), {time}]),
%% 	case SceneModel of
%% 		fun_scene_copy -> skip;
%% 		_ -> timer:apply_after(?SCENE_TIMER, gen_server, cast, [self(), {time}])
%% 	end,	
	{noreply, State};


%% handle_cast({usr_enter_scene, Uid,Sid,Seq,EnterSceneData},#state{id =_ID} = State) ->	
%% 	?log_trace("usr_enter_scene, uid = ~p", [Uid]),	
%% 	try		
%% 		fun_scene_usr:enter_scene(Uid,Sid,EnterSceneData,Seq)
%% 	catch
%% 		ES:RS -> 
%% 			?log_error("scene usr_enter_scene error E=~p,R=~p,c=~p",[ES,RS,erlang:get_stacktrace()])
%% 	end,
%%     {noreply, State};
%% 
%% handle_cast({agent_out, Uid}, #state{id =_ID} = State) ->    
%%     ?log_trace("agent_out, uid = ~p", [Uid]),
%% 	try
%% 		fun_scene_usr:leave_scene(Uid)
%% 	catch
%% 		ES:RS -> 
%% 			?log_error("scene usr_enter_scene error E=~p,R=~p,c=~p",[ES,RS,erlang:get_stacktrace()])
%% 	end,
%%     {noreply, State};

handle_cast({time},  #state{type =Scene,on_time_off=_Off,on_time_module=_Ontime,on_time_check=_CheckTime,scene_model = SceneModel,script_items=_Script_items} = State) -> 	
	Now = util:longunixtime(),
	try
		fun_scene:do_time(Scene,Now)
	catch
		ES:RS -> 
			?log_error("scene doTimer error SceneModel=~p,E=~p,R=~p,c=~p",[SceneModel,ES,RS,erlang:get_stacktrace()])
	end,
		
	
%% 	NewCheckTime =if
%% 					  Off > 0 -> 
%% 						  if
%% 							  Now - CheckTime > Off -> 
%% 								  try
%% 									  Ontime:on_time(Scene,Now - StartTime)
%% 								  catch
%% 									  E:R ->
%% 										  ?log_error("scene ontime script error Scene=~p,E=~p,R=~p,S=~p",[Scene,E,R,erlang:get_stacktrace()])
%% 								  end,
%% 								  Now;
%% 							  true -> CheckTime
%% 						  end;
%% 					  true -> CheckTime
%% 				  end,
%% 	
	Te = util:longunixtime(),
	if
		Te - Now > 50 -> ?log("scene time over time,time=~p,Scene=~p",[Te - Now,Scene]);
		true -> skip
	end,
	erlang:send_after(?SCENE_TIMER_INTERVAL, self(), {'$gen_cast', {time}}),
	%%timer:apply_after(?SCENE_TIMER, gen_server, cast, [self(), {time}]),	
    {noreply, State#state{on_time_check = Now}};

handle_cast(stop, #state{type =Scene} = State) ->
	try 
		fun_scene:on_close(Scene)
	catch 
		E:R -> ?log_error("scene on_close error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,
    {stop, normal, State};

handle_cast({apply, Module, Func, Args}, State) ->
	try
		erlang:apply(Module, Func, Args)
	catch
		E:R -> ?log_error("apply error E=~p,R=~p,stack=~p", [E,R,erlang:get_stacktrace()])
	end,
	{noreply, State};

handle_cast(Msg, State) ->
	try 
		fun_scene:on_msg(Msg)
	catch 
		E:R -> ?log_error("scene handle_cast error E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()])
	end,		   
	{noreply, State}.
