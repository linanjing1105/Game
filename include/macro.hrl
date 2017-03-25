-ifndef('__MACRO_H__').
-define('__MACRO_H__', true).




-define(STATIC_DATA_ERROR,1).%%静态表数据缺失或者不匹配
-define(BACKPACK_FULL,2).%%背包空间不足
-define(VIP_LEV_NOT_ENTHOU,3).%%背包空间不足

%%装备部位
-define(Eqp_BASE,10000).
-define(Sarmor,10001).
-define(Body,10002).
-define(Gaiter,10003).
-define(Bracers,10004).
-define(Belt,10005).
-define(Shoes,10006).
-define(Head,10007).
-define(Necklace,10008).
-define(Badge,10009).
-define(Ring,10010).
-define(Weapon,10011).
-define(Glove,10012).


%%快捷购买
-define(NO_GUIK_BUY,0). 
-define(GUIK_BUY,1). 

% code
-define(TRUE, true).
-define(FALSE, false).
-define(CONTINUE, continue).
-define(UNDEFINED, undefined).
-define(OK, ok).
-define(SKIP, skip).
-define(RETURN, return).
-define(ERROR, error).


%% 场景
-define(SCENE_SORT_CITY,"CITY").
-define(SCENE_SORT_PEACE,"PEACEFIELD").
-define(SCENE_SORT_SCUFFLE,"SCUFFLEFIELD").
-define(SCENE_SORT_COPY,"DUNGEONS").
-define(SCENE_SORT_ARENA,"ARENA").



%%服务器时间(秒)
-define(NOW,util:unixtime()). 
%% 服务器实体sort
-define(SPIRIT_SORT_NULL,no).
-define(SPIRIT_SORT_NPC,npc).
-define(SPIRIT_SORT_ITEM,sceneitem).
-define(SPIRIT_SORT_MONSTER,monster).
-define(SPIRIT_SORT_USR,usr).





%%消息类型
-define(World,1).
-define(Team,2).
-define(Guild,3).
-define(Private,4).
-define(System,5). 
-define(Special,6). 

	
%% 职业
-define(PROF_WARRIOR, 1). % 战士
-define(PROF_MAGE, 2). %法师
-define(PROF_ASSASSIN, 3). %刺客


 


%% 属性
-define(PROPERTY_ATKUP,1).%% 攻击上限
-define(PROPERTY_ATKDOWN,2).%% 攻击下限
-define(PROPERTY_DEFUP,3).%% 防御上限
-define(PROPERTY_DEFDOWN,4).%% 防御下限
-define(PROPERTY_HPLIMIT,5).%% 生命上限
-define(PROPERTY_MPLIMIT,6).%% 法力上限
-define(PROPERTY_HIT,7).%% 命中
-define(PROPERTY_DOD,8).%% 闪避
-define(PROPERTY_CRI,9).%% 暴击
-define(PROPERTY_TOUGH,10).%% 韧性
-define(PROPERTY_ADDHURT,11). %% 增伤
-define(PROPERTY_REDUCEHURT,12). %%免伤
-define(PROPERTY_ATK, 13). %%攻击
-define(PROPERTY_DEF, 14). %%防御

-define(PROPERTY_MOVESPD,100).%% 移速
-define(PROPERTY_LEV,101).%% 等级
-define(PROPERTY_HP,102).%% 生命
-define(PROPERTY_MP,103).%% 法力
-define(PROPERTY_EXP,104).%% 经验
-define(PROPERTY_LEVLIMIT,105).%% 等级上限
-define(PROPERTY_EXPNEEDED,106).%% 经验上限
-define(PROPERTY_MODEL_CLOTHES,107).%% 时装
-define(PROPERTY_TITLE,108).%% 称号
-define(PROPERTY_FIGHT_SCORE,109). %%战斗力




-define(PROPERTY_BINDING_COIN,300).%% 绑定金币
-define(PROPERTY_UNBINGDING_COIN,301).%% 未绑定的金币
-define(PROPERTY_LIJIN,302).%% 礼金
-define(PROPERTY_YUANBAO,303).%% 元宝
-define(PROPERTY_REPAIR,304).%% 修为
-define(PROPERTY_SAVVY,305).   %%悟性
-define(PROPERTY_ANIMA,306). %%灵气
-define(PROPERTY_XIAN_Qi,307). %%仙气
-define(PROPERTY_EXPLOIT ,308). %%功勋
-define(PROPERTY_REPUTATION ,309). %%声望
-define(PROPERTY_INTEGRAL ,310). %%积分
-define(PROPERTY_VIP_EXP ,311). %%VIP经验
-define(PROPERTY_CONTRIBUTE ,312). %%仙盟贡献




%%资源


-define(RESOUCE_SYCEE,1).%%元宝
-define(RESOUCE_CASH, 2).%%礼金
-define(RESOUCE_EXP,3).%%经验
-define(RESOUCE_SKILL_POINT,4).  %%技能点


%%任务状态
-define(NO_ACCEPT_STATE,0). %%未接取
-define(ACCEPT_STATE,1). %%接取
-define(CAN_FINISH_STATE,2).%%能完成
-define(FINISH_STATE,3).%%已提交完成


-define(TALK_TO_NPC,19001).                          %%跟NPC对话
-define(TASK_SCENE_ITEM2,8002).					%%采集道具	
-define(EQUIP,20001).                 %%穿装备

%%怪物分类 
-define(NORMAL_MONSTER,0). 
-define(ELITIST_MONSTER,1).
-define(BOSS_MONSTER,2).


%%每天刷新时间
-define(AUTO_REFRESH_TIME,0).
%%任务
-define(SORT_MAINTASK, 1).%%主线
-define(SORT_BRANCH_TASK, 2).%%支线
-define(SORT_SURROUND_TASK, 3).%%跑环任务
-define(SORT_SHILIAN_TASK, 4).%%试炼任务
-define(SORT_JIEYI_TASK, 5).%%结义任务


-define(STATE_ONLINE, 0).%%在线
-define(STATE_OFFLINE,1).%%离线

%%活动副本类型
-define(COPY_SORT_ENDLESS,"ENDLESS").
-define(COPY_SORT_SINGLE_MANY,"SINGLE_MANY").
-define(COPY_SORT_QUELL_DEMON,"QUELL_DEMON").
-define(COPY_SORT_GUILD_GUARD,"GUILD_GUARD").
-define(COPY_SORT_SEAL_BOSS,"SEAL_BOSS").

%%活动场景
-define(ACT_SCENE_QUELL_DEMON, 100002).
-define(ACT_SCENE_BUDO, 160004).
-define(ACT_SCENE_GUILD_BATTLE, 150007).
-define(ACT_SCENE_FENG_SHEN, 160003).
-define(ACT_SCENE_GUILD_BONFIRE,150004).
-define(ACT_SCENE_GUILD_GUARD_1,150001).
-define(ACT_SCENE_GUILD_GUARD_2,150005).
-define(ACT_SCENE_GUILD_GUARD_3,150006).
-define(ACT_SCENE_GUILD_STORM_CITY,150002).
%%活动ID
-define(ACT_ID_QUELL_DEMON, 1).
-define(ACT_ID_FENG_SHEN,2).     %%封神之路
-define(ACT_ID_CART_ESCORT, 3).	 %% 单人运镖
-define(ACT_ID_GUILD_BONFIRE,4). %%仙盟篝火
-define(ACT_ID_GUILD_GUARD,6).  %%仙盟守护
-define(ACT_ID_GUILD_CART_ESCORT, 9). %% 仙盟运镖
-define(ACT_ID_GUILD_STORM_CITY,10).  


%%BOSS挑战
-define(BOSS_TYPE_UNDERGROUND_PALACE,1).
-define(BOSS_TYPE_SCENE,2).
-define(BOSS_TYPE_SEAL,3).
-define(BOSS_TYPE_SMELTTERS,4). %熔恶BOss 
-define(BOSS_TYPE_LI_SMELTTERS,5).  %%里熔恶




%%奖励
-define(REWARD_TYPE_LEV,1).
-define(REWARD_TYPE_ONLINE,2).


%% 红点状态
-define(RED_DOT_STATE_NORMAL, 1).		%% 有红点
-define(RED_DOT_STATE_CLEAR, 0).		%% 红点清除

%%%  更新红点类型
-define(RED_DOT_ACHIEVEMENT,1).	%% 成就
-define(RED_DOT_FIRST_RECHARGE, 2). %% 首充

-endif.