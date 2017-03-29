-module(fun_scene).


-include("common.hrl").


-export([on_init/0,do_msg/1]).



on_init()-> 

	ok.

% on_scene_loaded(_SceneModel,Scene) -> 
% 	%put(scene_model,SceneModel),	
% 	put(on_time,[]),
% 	do_module_script(on_create,[Scene]),
% 	server_tools:send_to_agent_mng({on_scene_created, get(id), self(), Scene}),
% 	ok.

% on_close(Scene) -> 
% 	do_module_script(on_stop,[Scene]),
% 	ok.


% do_time(Scene,Now) ->ok;
	


% % get_scene_model() -> get(scene_model).



% run_scene_script(Fun,Args) ->
% 	ScriptMoudle = fun_scene:get_script_module(),	
% 	case erlang:module_loaded(ScriptMoudle) of
% 		true ->
% 			try
% 				erlang:apply(ScriptMoudle, Fun, Args)
% 			catch E:R -> ?log_error("scene run_scene_script error Module = ~p,E = ~p,R=~p,stack=~p",[ScriptMoudle,E,R,erlang:get_stacktrace()])
% 			end;
% 		_ -> 
% 			case code:load_file(ScriptMoudle) of
% 				{module,_} ->
% 					try
% 						erlang:apply(ScriptMoudle, Fun, Args)
% 					catch E:R -> ?log_error("scene run_scene_script error Module = ~p,E = ~p,R=~p,stack=~p",[ScriptMoudle,E,R,erlang:get_stacktrace()])
% 					end;
% 				_ -> ?debug("run_scene_script no moudle,ScriptMoudle = ~p",[ScriptMoudle])
% 			end
% 	end.

% get_script_module() ->
% 	util:to_atom("scene_config_" ++ util:to_list(get(scene))).


% on_msg(Msg) ->
% 	case do_module_script(do_msg, [Msg]) of
% 		continue -> do_msg(Msg);
% 		_ ->skip
% 	end.


% do_msg({process_pt, PtModule, Seq, Pt, Sid, Uid}) ->
% 	process_pt(PtModule,Seq,Pt,Sid,Uid);
% do_msg({usr_enter_scene, Uid,Sid,Seq,EnterSceneData}) ->
% 	?debug("usr_enter_scene, uid=~p,scene=~p", [Uid,get(scene)]),	
% 	fun_scene_usr:enter_scene(Uid,Sid,EnterSceneData,Seq);
% do_msg({usr_leave_scene, Uid}) ->
% 	?debug("usr_leave_scene, uid=~p,scene=~p", [Uid,get(scene)]),
% 	fun_scene_usr:leave_scene(Uid);
% do_msg({agent_out, Uid}) ->
% 	%% 退出游戏
% 	?debug("agent_out, uid=~p,scene=~p", [Uid,get(scene)]),
% 	case fun_scene_obj:get_obj(Uid, ?SPIRIT_SORT_USR) of
% 		#scene_spirit_ex{data=#scene_usr_ex{pid=AgentPid}} ->
% 			fun_scene_arena:agentout_check_budo_gameover(Uid),
% 			on_msg({usr_leave_scene, Uid}),
% 			server_tools:send_to_agent(AgentPid, agent_out_complete);
% 		_ -> skip
% 	end;

do_msg(_Msg) ->
	?debug("unknown msg,msg=~p", [_Msg]),
	ok.



process_pt(PtName,_Seq,_Pt,_Sid,_Uid) ->
	?log_warning("unprocessed proto:~p",[PtName]).

%% test() -> process_pt(pt_scene_fly_by_fly_point_c004,0,{pt_scene_fly_by_fly_point,900000001,},Sid,Uid) .


