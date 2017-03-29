-module(fun_agent_mng).

-include("common.hrl").

-export([do_init/0,do_close/0,do_call/1,do_info/1,do_time/1,do_msg/1]).
-export([agent_msg_by_uid/2,scene_msg_by_uid/2,agent_call_by_uid/2, scene_call_by_uid/2]).

-record(agent_svr, {idx = 0, pid =0,maxagent=0,agent=0}).


do_init()-> 
	init_agent_svr_info(),
	ok.
  





process_pt(PtName,_Seq,_Pt,_Sid,_Uid) ->
	?log_warning("unprocessed proto:~p",[PtName]).


do_close() -> ok.

do_time(_Time) -> ok.


do_call({agent_ctr_start, {Idx,Pid,MaxAgent}}) ->
	?debug("agent_ctr_start,idx=~p,pid=~p,max_agent=~p",[Idx,Pid,MaxAgent]),
	set_agent_svr_info(#agent_svr{idx=Idx,pid=Pid,maxagent=MaxAgent,agent=0}),
	ok;

do_call({shutdown}) ->
	fun_gateway_mng:shutdown(),
	ok;
do_call(_Msg) -> ok.

do_info({timeout, TimerRef, {xtimer_callback, Arg}}) ->
	try
		xtimer:on_timer(TimerRef, Arg)
	catch E:R ->?log_error("timeout error,data=~p,E=~p,R=~p,stack=~p",[Arg,E,R,erlang:get_stacktrace()])
	end;
	
do_info(_Info) ->
	?debug("unknown info,info=~p",[_Info]),
	ok.
do_msg({agent_ctr_close, Idx}) ->
	?debug("agent_ctr_close,idx=~p", [Idx]),
	del_agent_svr_info(Idx);
do_msg({process_pt, PtModule, Seq, Pt, Sid, Uid}) ->
	process_pt(PtModule,Seq,Pt,Sid,Uid);
do_msg({let_it_in, Sid,Ip,Seq,Aid,Uid}) ->
	?debug("fun_agent_mng let_it_in Aid=~p,Uid=~p",[Aid,Uid]),
	Agentsvrs = match_agent_svr_info(#agent_svr{_ = '_'}),
	case get_best(Agentsvrs,no) of
		 {ok,AgentCentHid,_} -> 
			?debug("find ctr = ~p",[AgentCentHid]),
			server_tools:send_to_agent_ctr(AgentCentHid, {let_it_in, Sid,Ip,Seq,Aid,Uid});
		R -> 
			?log_trace("fun_agent_mng let_it_in no game,get_best:R=~p~n",[R])		
			% ?discon(Sid,nogame,0)
	end;

do_msg({agent_in,Usr,Sid,Ip,InitData,AgentHid,AgentIndex}) ->
	?debug("agent_in uid=~p",[Usr#usr.id]), 
	db:dirty_put(#ply{uid=Usr#usr.id, aid=Usr#usr.acc_id, sid=Sid}),
	case get_agent_svr_info(AgentIndex) of
		[Agentsvr = #agent_svr{agent = Cur}] ->
			set_agent_svr_info(Agentsvr#agent_svr{agent = Cur + 1});
		_ -> skip
	end;
do_msg({agent_out, Uid}) ->	
	?debug("agent_out uid=~p",[Uid]),
	case db:dirty_get(ply,Uid) of
		[Ply] -> 
			case get_agent_svr_info(Ply#ply.agent_idx) of
				[Agentsvr = #agent_svr{agent = Cur}] when Cur > 0 ->
					set_agent_svr_info(Agentsvr#agent_svr{agent = Cur - 1});
				_ -> skip
			end,
			if
				is_pid(Ply#ply.scene_pid) -> server_tools:send_to_scene(Ply#ply.scene_pid, {agent_out, Uid});
				true -> skip
			end,
			db:dirty_del(ply,Uid);
		_ -> skip
	end;

do_msg(_Msg) ->
	?log_warning("unknown msg,msg=~p",[_Msg]),
	ok.

init_agent_svr_info() ->
	ets:new(agent_svr, [set,public,named_table,{keypos, #agent_svr.idx}]).
get_agent_svr_info(Idx) ->
	ets:lookup(agent_svr, Idx).
match_agent_svr_info(Pat) ->
	ets:match_object(agent_svr, Pat).
set_agent_svr_info(Data) ->
	ets:insert(agent_svr, Data).
del_agent_svr_info(Idx) ->
	ets:delete(agent_svr, Idx).

get_best([],R) -> R;
get_best([This|Next],no) -> 
	if
		This#agent_svr.agent < This#agent_svr.maxagent -> get_best(Next,{ok,This#agent_svr.pid,{This#agent_svr.agent,This#agent_svr.maxagent}});
		true -> get_best(Next,no)
	end;
get_best([This|Next],{ok,OldHid,{OldAgent,OldMaxAgent}}) -> 
	if
		This#agent_svr.agent < This#agent_svr.maxagent -> 
			if
				This#agent_svr.agent > OldAgent -> get_best(Next,{ok,This#agent_svr.pid,{This#agent_svr.agent,This#agent_svr.maxagent}});
				true -> get_best(Next,{ok,OldHid,{OldAgent,OldMaxAgent}})
			end;
		true -> {ok,OldHid,{OldAgent,OldMaxAgent}}
	end.







agent_msg_by_uid(Uid, Msg) ->
	case db:dirty_get(ply,Uid) of	
		[#ply{agent_pid=Pid}] when is_pid(Pid) -> server_tools:send_to_agent(Pid, Msg);
			_R-> ?log("agent_msg_by_uid send failed,uid=~p,msg=~p",[Uid, Msg]), skip
	end.

scene_msg_by_uid(Uid,Msg)->
	case db:dirty_get(ply,Uid) of	
		[#ply{scene_pid=ScenePid}] when is_pid(ScenePid) -> server_tools:send_to_scene(ScenePid,Msg);
		_R-> ?log("scene_msg_by_uid send failed,uid=~p,msg=~p",[Uid, Msg]),skip
	end.

%% only for test
agent_call_by_uid(Uid,{M,F,A})->
	case db:dirty_get(ply,Uid) of	
		[#ply{agent_pid=Pid}] when is_pid(Pid) -> server_tools:apply_call(Pid, M, F, A);
		_R-> 
			?log("scenehid_exception data=~p",[_R]),
			{error, no_ply}
	end.

scene_call_by_uid(Uid,{M,F,A})->
	case db:dirty_get(ply,Uid) of	
		[#ply{scene_pid=ScenePid}] when is_pid(ScenePid) -> server_tools:apply_call(ScenePid, M, F, A);
		_R-> 
			?log("scenehid_exception data=~p",[_R]),
			{error, no_ply}
	end.

