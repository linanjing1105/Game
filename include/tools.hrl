-ifndef('__TOOLS_H__').
-define('__TOOLS_H__', true).

-define(send(Sid, Data), server_tools:send_packet(Sid, Data)).
-define(sends(Sid, Data), server_tools:send_packets(Sid, Data)).
-define(send_world(Type,Msg), server_tools:send_to_world(Type, Msg)).
-define(discon(Sid,Reason), server_tools:disconnect(Sid, Reason, 0)).
-define(discon(Sid,Reason,Time), server_tools:disconnect(Sid, Reason, Time)).

% -define(error_report(Sid,Error), fun_error_report:send_error_report(Sid, Error)).
% -define(error_report(Sid,Error,Seq), fun_error_report:send_error_report(Sid, Error,Seq)).
% -define(error_report(Sid,Error,Seq,Data), fun_error_report:send_error_report(Sid, Error, Seq, Data)).

% -define(world_system_report(Seq,ID), fun_chat:send_world_system_report(Seq, ID,[], [])).
% -define(world_system_report(Seq,ID,List), fun_chat:send_world_system_report(Seq, ID,List, [])).
% -define(world_system_report(Seq, ID,List,ItemList), fun_chat:send_world_system_report(Seq, ID, List, ItemList)).
% -define(guild_system_report(Seq,ID), fun_chat:send_guild_system_report(Seq,Guild_id, ID, [], [])).
% -define(guild_system_report(Seq,Guild_id,ID,List,ItemList), fun_chat:send_guild_system_report(Seq, Guild_id,ID, List, ItemList)).
% -define(system_noctice(ID,String), fun_chat:system_notic(ID, String)).

-define(str(A), (byte_size(util:to_binary(A))):16/integer, (util:to_binary(A))/binary).
-define(u8,     8/unsigned-integer).
-define(u16,    16/unsigned-integer).
-define(u32,    32/unsigned-integer).
-define(i8,     8/signed-integer).
-define(i16,    16/signed-integer).
-define(i32,    32/signed-integer).
-define(f,      32/float).



% -ifdef(gm_cord).
% -define(proc_gm_cmd(Content,Sid,Uid,Seq),fun_gm:proc_gm_cmd(Content,Sid,Uid,Seq)).
% -else.
% -define(proc_gm_cmd(Content,Sid,Uid,Seq),ok).
% -endif.

-define(record2proplist(Tag, Record), util:record_to_proplist(Tag, Record, record_info(fields, Tag))).

-ifdef(debug).
-define(assert(Expr),
		(fun() ->
			case (Expr) of
				true -> ok;
				_ -> erlang:error(assert_failed)
			end
		end)()).
-else.
-define(assert(Expr), ok).
-endif.

-endif. %%-ifndef('__TOOLS_H__').


