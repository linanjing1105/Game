-module(server_tools).
-include("tools.hrl").

-export([send_packet/2,send_packets/2,disconnect/3]).
-export([send_to_login/1,send_to_agent/2,send_to_agent_mng/1,send_to_gateway/2,
		 send_to_gateway_ctr/2,send_to_scene/2, send_to_scene_mng/1,
		 send_to_agent_ctr/2, send_to_scene_ctr/2, send_to_offline_agent_mng/1]).
-export([regist_pt/0,regist_pt/1,parse_pt/1]).
-export([call_persist/3,call_persist/4,apply_cast/4,apply_call/4]).

send_packet(Sid, Data) ->
    case is_pid(Sid) of
        true ->
            gen_server:cast(Sid, {send, Data});
        false ->
			?assert(false),
            error
    end.

send_packets(Sid, Datas) ->
    case is_pid(Sid) of
        true ->
            gen_server:cast(Sid, {sends, Datas});
        false ->
			?assert(false),
            error
    end.


disconnect(Sid, Reason, Time) ->
	case is_pid(Sid) of
		true ->
			if
				Time > 0 -> timer:apply_after(Time, gen_server, cast, [Sid, {discon, Reason}]);
				true -> gen_server:cast(Sid, {discon, Reason})
			end;
		_ -> error
	end.
	


send_to_login(Msg) -> gen_server:cast({global, login}, Msg).
send_to_gateway(Pid, Msg) -> gen_server:cast(Pid, Msg).
send_to_gateway_ctr(Index, Msg) -> gen_server:cast({global, {gateway_ctr,Index}}, Msg).
send_to_agent(Pid, Msg) -> gen_server:cast(Pid, Msg).
send_to_agent_mng(Msg) -> gen_server:cast({global, agent_mng}, Msg).
send_to_agent_ctr(Pid, Msg) -> gen_server:cast(Pid, Msg).
send_to_scene_mng(Msg)-> gen_server:cast({global, scene_mng}, Msg).
send_to_scene(Pid, Msg) -> gen_server:cast(Pid, Msg).
send_to_scene_ctr(Pid, Msg) -> gen_server:cast(Pid, Msg).
send_to_offline_agent_mng(Msg) -> gen_server:cast({global, offline_agent_mng}, Msg).

call_persist(ServerRef, Request, Interval) ->
	call_persist(ServerRef, Request, Interval, infinity).
call_persist(ServerRef, Request, Interval, MaxRetryTimes) ->
	call_persist(ServerRef, Request, Interval, 1, MaxRetryTimes).
call_persist(ServerRef, Request, Interval, CurTimes, MaxTimes) ->
	Ret = 
	try
		Reply = gen_server:call(ServerRef, Request, infinity),
		{ok, Reply}
	catch
		E:R -> {call_failed, {E,R}}
	end,
	case Ret of
		{call_failed, {exit, {noproc, _}}} ->
			Continue =
				case MaxTimes of
					infinity -> true;
					_ -> CurTimes < MaxTimes
				end,
			case Continue of
				true ->
					util:sleep(Interval),
					call_persist(ServerRef, Request, Interval, CurTimes+1, MaxTimes);
				_ -> throw({call_failed,max_retry_times})
			end;
		{call_failed, Err} ->
			throw(Err);
		{ok, Rep} -> Rep
	end.

apply_cast(Pid, Module, Func, Args) -> gen_server:cast(Pid, {apply, Module, Func, Args}).
apply_call(Pid, Module, Func, Args) -> gen_server:call(Pid, {apply, Module, Func, Args}).



regist_pt() ->
	case code:where_is_file(util:to_list(?MODULE) ++ ".beam") of
		non_existing -> erlang:error({non_existing, ?MODULE});
		Path ->
			{ok, Filenames} = file:list_dir(filename:dirname(Path)),
			F = fun(Filename, List) ->
					case filename:extension(Filename) of
						".beam" ->
							case string:left(Filename, 3) of
								"pt_" ->
									ModName = filename:basename(Filename, ".beam"),
									case string:equal(ModName, "pt_public_class") of
										true -> List;
										_ -> [util:to_atom(ModName) | List]
									end;
								_ -> List
							end;
						_ -> List
					end
				end,
			PtList = lists:foldl(F, [], Filenames),
			regist_pt(PtList)
	end.
	
regist_pt(PtList) ->
	lib_protocol:init_pt(),
	Fun = fun(Pt) -> 
				  case lib_protocol:regist_pt(Pt) of
					  ok -> skip;
					  {registed, ProtoId, OldModule} -> 
						  erlang:error({duplicate_proto_id, integer_to_list(ProtoId,16), {OldModule, Pt}})
				  end
		  end,
	lists:foreach(Fun, PtList).

parse_pt(Data) ->
	case lib_protocol:fill_pt(Data) of
		{ok,ProtoId,Module,Seq,Pt} -> {ok,ProtoId,Module,Seq,Pt};
		Other -> {error, Other}
	end.
