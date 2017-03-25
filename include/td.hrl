-ifndef('__TD_H__').
-define('__TD_H__', true).
%%,pass_last_ref_time=0
-record(account,{id,username,password,platform}).
-record(usr,{id,acc_id = 0,name="",prof=0,exp=0,lev = 1,hp=1,mp= 1,can_use_bags=60,unlock_bags=0,init_bags=1,
			 vip_lev=0,fight_score=0,vip_exp=0}).
-record(item,{id,uid,type=0,num=0,pos=0,bind=0}).


-define(Tabs,
		[
		 [account | record_info(fields, account)],
		 [usr | record_info(fields, usr)], 
		 [item | record_info(fields, item)]
		]).

-define(StartLoadList,
		[
		 ]).

-define(PlyLoadList, 
		[{usr,id},
		 {item,uid}]).



-define(WorldTabs,
		[]).


-define(AgentTabs,
		[
		 usr
		]).

-define(SceneTabs,
		[
		 ]).








-endif. %%-ifndef('__TD_H__').