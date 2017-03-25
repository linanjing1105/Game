-module(fun_login).

-include("common.hrl").

-export([do_init/0,do_close/0,do_call/1,do_info/1,do_time/1,do_msg/1]).
-export([lyn_auth_callback/2]).

-record(login_data, {sid,login_closed = false, aid=0,uid=0,ip=0,key=0,usrlist=[]}).




-define(KEY, "NiJrMIkdp4lTf6sb").

	



do_init()->
	fun_gateway_mng:init(),
	init_login_data(),
	ok.
do_close() -> ok.

do_time(_Time) -> ok.

do_call({regist_gateway, Index,MaxSocketNum,MyAddress,Port}) ->
	case fun_gateway_mng:regist(Index,MaxSocketNum,MyAddress,Port) of
		ok -> {ok, registered};
		Other -> Other
	end;
do_call({unregist_gateway, Index}) ->
	fun_gateway_mng:unregist(Index),
	ok;
do_call(_Msg) -> ok.
do_info(_Info) -> ok.

do_msg({process_pt,PtModule, Seq, Pt, Sid, Ip}) ->
	process_pt(PtModule,Seq,Pt,Sid,Ip);
do_msg({gateway_tcp_closed, Sid})->
	case get_login_data(Sid) of
		#login_data{uid = 0} ->	del_login_data(Sid);
		LoginData = #login_data{} -> put_login_data(LoginData#login_data{login_closed = true});
		_ -> skip
	end;
do_msg({gateway_conn_add, Index}) ->
	fun_gateway_mng:on_connect(Index);
do_msg({gateway_conn_dec, Index}) ->
	fun_gateway_mng:on_disconnect(Index);

do_msg(_Msg) ->
	?log_warning("unknown msg,msg=~p",[_Msg]),
	ok.



init_login_data() ->
	ets:new(login_data, [set,public,named_table,{keypos, #login_data.sid}]).
get_login_data(Sid) ->
	case ets:lookup(login_data, Sid) of
        [Data] -> Data;
		[] -> null;
        Other -> ?log_error("Error:get login_data error,Sid=~p,Other=~p",[Sid,Other]),?UNDEFINED
    end.
match_login_data(Pat) ->
	ets:match_object(login_data, Pat).
put_login_data(Data) ->
	ets:insert(login_data, Data).
del_login_data(Key) ->
	ets:delete(login_data, Key).

is_valid_account(Account) ->
	case length(Account) > 0 of
		true ->
			case re:run(Account, "[^a-zA-Z0-9_]") of
				nomatch -> true;
				_ -> false
			end;
		_ -> false
	end.

is_valid_name(Name) ->
	if
		Name == "" -> false;
		true ->
			case length(xmerl_ucs:from_utf8(Name)) > ?MAX_NAME_LEN of
				true -> false;
				_ ->
					case fun_sensitive_words:check_sensitive_words(Name) of
						true -> true;
						_ -> {false, invalid_words}
					end
			end
	end.

process_pt(pt_req_create_usr_a006,Seq,Pt,Sid,_Ip) -> 
	?debug("get req_create_usr Pt = ~p,Sid = ~p",[Pt,Sid]),
	Name =Pt#pt_req_create_usr.name,
	Prof =Pt#pt_req_create_usr.prof,
	case is_valid_name(Name) of
		true -> 
			case find_usr_by_name(Name) of
				[] -> create_usr(Name,Prof,Sid,Seq);
				_ -> ?error_report(Sid,"usr_name_renam",Seq)
			end;
		{false, invalid_words} -> ?error_report(Sid,"pet10",Seq);
		_ -> ?error_report(Sid,"check_data_error",Seq)
	end,
	ok;


process_pt(pt_login_a001,Seq,Pt,Sid,Ip) ->
	?debug("get login Pt = ~p,Sid = ~p",[Pt,Sid]),
	Account = Pt#pt_login.account, 
	Password = Pt#pt_login.password,
	Platform = Pt#pt_login.platform,
	
	case get_login_data(Sid) of
		#login_data{} -> 
			?error_report(Sid,"already_login",Seq),
			?discon(Sid, login_failed, 100);
		_ ->
			case Platform of
				?PLATFORM_NULL ->
					do_real_login(Sid, Account, Password, Platform, Seq, Ip);
				?PLATFORM_LYNSDK ->
					lyn_auth(Sid, Seq, Account, Platform, Ip);
				_ ->
					?error_report(Sid,"check_data_error",Seq),
					?log_warning("unknown login platform,platform=~p", [Platform]),
					?discon(Sid, login_failed, 100)
			end
	end;

process_pt(pt_usr_enter_b001,Seq,Pt,Sid,Ip) ->
	Uid = Pt#pt_usr_enter.uid,
	Key = Pt#pt_usr_enter.key,
	?debug("pt_usr_enter_b001............................."),
	%%TODO:verify key
	case match_login_data(#login_data{uid = Uid, key=Key, ip=Ip, _ = '_'}) of
		[#login_data{aid=Aid, sid = OldSid} | _] ->	
			del_login_data(OldSid),
			%%开始进入游戏
			server_tools:send_to_agent_mng({let_it_in, Sid,Ip,Seq,Aid,Uid}),
			ok;
		_ ->
			?discon(Sid, login_failed, 100)
	end;
	
process_pt(Name,_Seq,_Pt,_Sid,_Ip) ->
	?log_warning("unprocessed proto:~p",[Name]).


load_account(Account,Password,Platform) ->
	BinAcc = util:to_binary(Account),
	BinPwd = util:to_binary(Password),
	AccList =
	case db:dirty_get(account, BinAcc, #account.username) of
		[] -> db:getnet(account, username, BinAcc);
		Other -> Other
	end,
	case AccList of
		[#account{id = Aid, password = BinPwd, platform=Platform}] -> {ok, Aid};
		[] ->
			case db:insert(#account{username=BinAcc,password=BinPwd, platform=Platform}) of
				[#account{id = Aid}] ->
					?log_trace("no acc,create one,acc = ~p pass = ~p",[Account,Password]),
					{ok, Aid};
				_ ->
					?log_warning("create acc error,acc=~p pass=~p", [Account,Password]),
					{false, "check_data_error"}
			end;
		_ ->
			{false, "password_error"}
	end.







sign(Uid, TimeStamp, RandStr) ->
	util:md5(io_lib:format("~p#~p#~s#~s", [Uid,TimeStamp,RandStr,?KEY])).

gen_key(Uid) ->
	TimeStamp = util:unixtime(),
	RandStr = util:random_string(16),
	sign(Uid, TimeStamp, RandStr).
	


	


real_login(Sid, StrAcc, StrPsw, Platform, Seq, Ip) ->
	case get_login_data(Sid) of
		#login_data{sid=Sid}->
			?debug("already login"),
			skip;
		_ ->
			do_real_login(Sid, StrAcc, StrPsw, Platform, Seq, Ip)
	end.

do_real_login(Sid, Account, Password, Platform, Seq, Ip) ->
	?debug("do_real_login"),
	case is_valid_account(Account) of
		true ->
			case load_account(Account, Password, Platform) of
				{ok, Aid} ->
					case match_login_data(#login_data{aid=Aid, _ = '_'}) of
						[#login_data{sid = OldSid}] ->
							del_login_data(OldSid),
							?log_trace("relogin, account=~p", [Account]),
							?error_report(OldSid,"relogin"),
							?discon(OldSid, relogin, 100);
						_ ->
							case db:dirty_get(ply, Aid, #ply.aid) of
								[#ply{sid = OldSid, uid=OldUid}] ->
									?error_report(OldSid,"relogin"),
									?log_trace("relogin, account=~p,oldUid=~p", [Account, OldUid]),
									?discon(OldSid, relogin, 100);							
								_ -> skip
							end
					end,
					request_usr_list(Sid, Aid,Seq,Ip);
				{false, Error} ->
					?error_report(Sid, Error, Seq),
					?discon(Sid, login_failed, 100)
			end;
		_ -> 
			?error_report(Sid,"account_field_error",Seq),
			?discon(Sid, login_failed, 100)
	end.


						
	
