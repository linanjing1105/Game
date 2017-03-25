-ifndef('__RECORD_H__').
-define('__RECORD_H__', true).

-record(cache,  {uid,cached = 0}).

-record(ply, {uid, aid = 0,sid=0,name="",prof=0,lev=0,ip={0,0,0,0},accname="",agent_pid=0,agent_idx=0,scene_pid=0,scene_idx = 0,scene_id=0,
				scene_type=0,hp=0,hp_limit=0,fight_score=0}).

-record(scene_usr_ex,{pid
					  ,sid
					  ,prof=0
					  ,lev=0
					  ,pk_mode=0
					  ,pet_id=0
					  ,equip_list=[]
					  ,skill_list=[]
					  ,model_clothes_id=[]
					  ,wing_id=0
					  ,wing_lev=0
					  ,team = {}
					  ,title_id=0
					  ,prev_save_pos=""
					  ,recently_be_atk_info=[]
					  ,guild_id=0
					  ,guild_name=""
					  ,sla=0
					  ,justice=0
					  ,pk_level=0
					  ,red_name_kill_times=0
					  ,white_name_kill_times=0
					  ,vip_revive_times=0
					  ,magic_weapon_id=0
					  ,magic_strength_lev=0
					  ,magic_strength_star=0,
					  ride_state=0,
					  ride_type=0,
					  currskin=0
					 }).
-record(scene_monster_ex, {type=0
						   ,ai_module=0
						   ,ai_data=0
						   ,ai_time =0
						   ,ai_enabled=true
						   ,ontime_start=0
						   ,ontime_check=0
						   ,ontime_off=0
						   ,script=0	
						   ,owner = 0
						   ,birth_pos={0,0,0}
						   ,send_client=true
						   ,refresh_rule=0
						   ,cart_data=0
						   ,recover_timer=0}).

-record(enter_scene_data,{usr={},a_hid=0,pos={0,0,0},property={},equip_list=[],skill_list=[],passive_skill=[],model_clothes_id=[],title_id=0,guild_name="",guild_id=0,
						  buffs=[],enter_scene_times=0,magic_weapon_id=0,magic_strength_lev=0,magic_strength_star=0,pet_fightinfo={},ride_state=0,
								   ride_type=0,currskin=0,wing_id=0,wing_lev=0}).






-endif.  %%-ifndef('__RECORD_H__').
