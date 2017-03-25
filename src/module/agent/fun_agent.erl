-module(fun_agent).


-include("common.hrl").


-export([on_init/1,on_login/1,on_logout/0,do_time/1,do_msg/1,fresh_all_prop/1,task_count_for_active_spcieal/3,task_count_for_active_spcieal/2,
		 refresh_num/1]).

 
on_init({Sid,Ip,Seq,Aid,Uid,AgentIdx}) ->
	?debug("on_init  ....Seq=~p",[Seq]),
	{{_,_,Day},_}=util:unix_to_localtime(util:unixtime()),
	put(sid,Sid),
	put(day,Day),
    put(uid, Uid),
    put(aid, Aid),
	put(ip, Ip),
	put(agentIdx, AgentIdx),
	put(in_fight,{?FALSE,0}),
	put(scene_pid, ?UNDEFINED),
	ok.


on_login(Seq)->
	Uid = get(uid),
	case db:dirty_get(usr, Uid) of
		[Usr] ->
			Sid = get(sid),
			Ip = get(ip),
			if 
				Usr#usr.last_login_time == 0 ->
					%%新角色首次登录
					on_first_login(Seq);
				true -> skip
			end,
			Camp=1,
			Hp = max(0, Usr#usr.hp),
			Mp = max(0, Usr#usr.mp),	
			NewUsr = Usr#usr{hp=Hp,mp=Mp,last_login_time=util:unixtime()},
			db:dirty_put(NewUsr),		
			%%战力初始化需要在其他系统之后		
			Fight_score=0,
			server_tools:send_to_agent_mng({agent_in,NewUsr,Sid,Ip,InitData,self(),get(agentIdx)}),
			Pt = pt_usr_info_b102:new(),
			Pt1 = Pt#pt_usr_info{
								 id=Usr#usr.id,  
								 name=util:to_list(Usr#usr.name),
								 level=Usr#usr.lev,
								 prof=Usr#usr.prof,
								 cur_hp=Hp,  
								 cur_mp=Mp,
								 exp=Usr#usr.exp,   
								 property_list=[],
								 equip_id_list= [],
								 camp=Camp,
								 resource_list = [],
								 model_clothes_id=[], 
								 title_id=0,	
								 guild_name=[],
								 sla=Usr#usr.sla,	
							 	 fiight_score=0
								 },
			?send(Sid,pt_usr_info_b102:to_binary(Pt1, Seq)),
			sync_time(Sid);
		_ ->
			?log_warning("on_login failed,uid=~p", [Uid])
	end,
	ok.

on_first_login(Seq) ->
	Uid = get(uid),
	Sid = get(sid),
	ok.	 
on_logout() ->
	Uid = get(uid),
	server_tools:send_to_agent_mng({agent_out, Uid}),
	ok.
do_msg({process_pt, PtModule, Seq, Pt, Sid}) ->
	process_pt(PtModule, Seq, Pt, Sid);		
do_msg({start_fly, Sid,_Uid,Seq, SceneId,{Scene,Pos}}) ->
	%服务端场景准备完毕，通知客户端加载场景
	?send(Sid,pt_req_load_scene_b104:to_binary(Pt1, Seq)),
	ok;
do_msg({add_exp, Exp}) ->ok;
do_msg(_Msg) ->
	?log_warning("unknown msg,msg=~p",[_Msg]),
	ok.

do_time(Time) ->
	ok.

process_pt(pt_usr_enter_scene_b003,Seq,Pt,Sid) ->ok;
	%% 登录游戏后请求场景
%% 客户端场景加载完成时 %
process_pt(pt_load_scene_finish_b005,Seq,Pt,Sid) ->ok;
process_pt(_PtName,_Seq,_Pt,_Sid) ->
	?log_warning("unprocessed proto:~p",[_PtName]),
	ok.
%% agent通用0点刷新定时器
refresh_num(Uid) ->ok;
	% case db:dirty_get(usr,Uid) of
	% 	[Usr] -> 
	% 		Taday_Time=util:get_relative_day(?COMMOM_REST_HOUR),  
	% 		if Taday_Time > Usr#usr.last_reset_time  ->
	% 			   Xiu_wei_num = 0,
	% 			   Vip_reborn_num = 0,
	% 			   db:dirty_put(Usr#usr{xiu_wei_num=Xiu_wei_num,
	% 			   	last_reset_time=Taday_Time,reborn_num=Vip_reborn_num});
	% 		   true ->skip
	% 		end,
	% 		Timeout = (util:relative_day_to_unixtime(Taday_Time + 1, ?COMMOM_REST_HOUR) - util:unixtime()) * 1000,
	% 		xtimer:start_timer(Timeout,{?MODULE, refresh_num, Uid});
	% 	_ -> skip
	% end.

