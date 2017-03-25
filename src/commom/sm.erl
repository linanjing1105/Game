%%uthor: mmo
%% Created: 2011-12-29
%% update: 2017-3-23
%% Description: TODO: Add description to sm
-module(sm).

%%
%% Include files
%%
-include("common.hrl").

%%
%% Exported Functions
%%
-export([makeall/0,install/0,check/0,reconnect/1,topN/2]).
-export([select/2,select/3,selectall/1,update/3,get_max_ply/0, set_max_ply/1, add_max_ply/1,get_login_data/2,disable_login/0,enable_login/0,shutdown/0]).

%%
%% API Functions
%%

makeall()->
	systools:make_script("dbm"),
	io:format("make dbm ok.~n"),
	 systools:make_script("gateway"),
	 io:format("make gateway ok.~n"),
	% systools:make_script("world"),
	% io:format("make mynet ok.~n"),
	 systools:make_script("agent_ctr"),
	 io:format("make agent_ctr ok.~n"),
	% systools:make_script("scene_ctr"),
	% io:format("make scene_ctr ok.~n"),
	% systools:make_script("http"),
	% io:format("make http ok.~n").
	ok.

install()->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	
	{atomic, ok} = mnesia:create_table(config, [{ram_copies, [node()]}, {attributes, record_info(fields, config)}]),
	{atomic, ok} = mnesia:create_table(sql_config, [{ram_copies, [node()]}, {attributes, record_info(fields, sql_config)}]),
	{atomic, ok} = mnesia:create_table(sql_key_static, [{ram_copies, [node()]}, {attributes, record_info(fields, sql_key_static)}]),
	
	{atomic, ok} = mnesia:create_table(cache, [{ram_copies, [node()]}, {attributes, record_info(fields, cache)}]),
	{atomic, ok} = mnesia:create_table(ply, [{ram_copies, [node()]}, {attributes, record_info(fields, ply)}, {index, [aid,name]}]),


	{atomic, ok} = mnesia:create_table(account, [{ram_copies, [node()]}, {attributes, record_info(fields, account)}, {index, [username]}]),
	{atomic, ok} = mnesia:create_table(usr, [{ram_copies, [node()]}, {attributes, record_info(fields, usr)}, {index, [acc_id,name]}]),
	{atomic, ok} = mnesia:create_table(item, [{ram_copies, [node()]}, {attributes, record_info(fields, item)}, {index, [uid]}]),
	
	mnesia:stop().

reconnect(Dbm) ->
    case net_adm:ping(Dbm) of
		pong -> io:format("connect ok~n");
		_ -> io:format("connect fail")
	end.


check() ->
	gen_server:cast({global, dbm}, {check}).

selectall(Tab) ->
	select(Tab, "*", all).

select(Tab, FiltStr) ->
	select(Tab, "*", FiltStr).

select(Tab, Columns, FiltStr) ->
	TabStr = util:to_list(Tab),
	ColList = mnesia:table_info(Tab, attributes),
	WhereStr =
	case FiltStr of
	all -> {?OK, ""};
	_ ->
		FiltCount = string:words(FiltStr, $,),
		TempL = lists:seq(1, FiltCount),
		FWhere =
		fun(N, Ret) ->
			Filt = string:sub_word(FiltStr, N, $,),
			{ok,Scanned,_} = erl_scan:string(Filt),
			Col = element(3, lists:nth(1, Scanned)),
			Oper = element(1, lists:nth(2, Scanned)),
			Value = element(3, lists:nth(3, Scanned)),
	
			case Ret of
			{?ERROR, Reason} -> {?ERROR, Reason};
			{?OK, RetStr} ->
				case get_col_pos(ColList, Col, 1) of
				0 -> {?ERROR, "Error:column " ++ util:to_list(Col) ++ " not found in record " ++ TabStr};
				Pos when is_integer(Value) -> {?OK, RetStr ++ ",element(" ++ integer_to_list(Pos+1) ++ ",X)" ++ util:to_list(Oper) ++ util:to_list(Value)};
				Pos -> {?OK, RetStr ++ ",element(" ++ integer_to_list(Pos+1) ++ ",X)" ++ util:to_list(Oper) ++ "<<\"" ++ util:to_list(Value) ++ "\">>"}
				end
			end
		end,
		lists:foldl(FWhere, {?OK, ""}, TempL)
	end,
	
	SelectColumn =
	case Columns of
	"*" -> "X";
	_ ->
		ColCount = string:words(Columns, $,),
		TempL1 = lists:seq(1, ColCount),
		FCol =
		fun(N, Ret) ->
			Col = util:to_atom(string:sub_word(Columns, N, $,)),
	
			case Ret of
			{?ERROR, Reason} -> {?ERROR, Reason};
			{?OK, RetStr} ->
				case get_col_pos(ColList, Col, 1) of
				0 -> {?ERROR, "Error:column " ++ util:to_list(Col) ++ " not found in record " ++ TabStr};
				Pos when length(RetStr) =:= 0 -> {?OK, RetStr ++ "element(" ++ integer_to_list(Pos+1) ++ ",X)"};
				Pos -> {?OK, RetStr ++ ",element(" ++ integer_to_list(Pos+1) ++ ",X)"}
				end
			end
		end,
		
		ColStr1 = lists:foldl(FCol, {?OK, ""}, TempL1),
		case ColStr1 of
		{?OK, Str} -> "{" ++ Str ++ "}";
		{?ERROR, Why} -> io:format("~s~n", [Why]),"X"
		end
	end,
	%ColStr = "{" ++ SelectColumn ++ "}",
	case WhereStr of
	{?OK, EndStr} ->
		End = EndStr ++ "])).",
		S = "db:do(qlc:q([" ++ SelectColumn ++ " || X<-mnesia:table("++TabStr++")" ++ End,
		io:format("exec ~s~n", [S]),
		exe_str(S);
	{?ERROR, Msg} -> Msg
	end.

update(Tab, Expr, Where) ->
	RecordList = select(Tab, Where),
	case length(RecordList) > 0 of
	?TRUE ->
		Fdo =
		fun(Record) ->
			%Record = lists:nth(1, RecordList),
			TabStr = util:to_list(Tab),
			ColList = mnesia:table_info(Tab, attributes),
			FiltCount = string:words(Expr, $,),
			TempL = lists:seq(1, FiltCount),
			F =
			fun(N, Ret) ->
				Filt = string:sub_word(Expr, N, $,),
				{ok,Scanned,_} = erl_scan:string(Filt),
				Col = element(3, lists:nth(1, Scanned)),
				Value = element(3, lists:nth(3, Scanned)),
				io:format("Col=~p,Value=~p~n",[Col,Value]),
				case Ret of
				{?ERROR, Reason} -> {?ERROR, Reason};
				{?OK, RetRec} ->
					case get_col_pos(ColList, Col, 1) of
					0 -> {?ERROR, "Error:column " ++ util:to_list(Col) ++ " not found in record " ++ TabStr};
					Pos when is_integer(Value) -> {?OK, setelement(Pos+1, RetRec, Value)};
					Pos -> {?OK, setelement(Pos+1, RetRec, util:to_binary(Value))}
					end
				end
			end,
			Result = lists:foldl(F, {?OK, Record}, TempL),
			case Result of
			{?OK, Rec} ->
				io:format("newrecord ~w~n", [Rec]),
				db:put(Rec);
			{?ERROR, Msg} -> io:format("~s~n", [Msg])
			end
		end,
		lists:foreach(Fdo, RecordList);
	?FALSE when length(RecordList) =:= 0 -> "Error:record not found"
	end.

get_col_pos(L, _Colname, N) when N > length(L) -> 0;
get_col_pos(L, Colname, N) ->
	%io:format("aaa=~p~n", [Colname]),
	case lists:nth(N, L) =:= util:to_atom(Colname) of
	?TRUE -> N;
	?FALSE -> get_col_pos(L, Colname, N+1)
	end.

exe_str(Str) ->
	{ok,Scanned,_} = erl_scan:string(Str),
	{ok,Parsed} = erl_parse:parse_exprs(Scanned),
	{value, Value,_} = erl_eval:exprs(Parsed,[]),
	Value.

get_login_data(Acc, Psw) ->
	Data = "1347933926.98",
	Key = "DNF2NJFT64ETYJUM",
	Ret = util:md5(lists:concat([util:to_list(Acc), "&", util:to_list(Psw), "&", util:to_list(Data), Key])),
	{Data, Ret}.

%% get_max_ply() -> {ok, MaxPly}
get_max_ply() ->
	login_svr:get_max_ply().

%% set_max_ply() -> {ok, MaxPly} | {error, Reason}
set_max_ply(Value) ->
	login_svr:set_max_ply(Value).

%% add_max_ply() -> {ok, MaxPly} | {error, Reason}
add_max_ply(Value) ->
	login_svr:add_max_ply(Value).

disable_login() ->
	 gen_server:cast({global, login}, {enable_login, false}).

enable_login() ->
	 gen_server:cast({global, login}, {enable_login, true}).


%% getnowonline()->
%% 	L=db:dirty_match(ply,#ply{_='_'}),
%% 	erlang:length(L).
%% 
%% kick_all_usr() ->
%% 	case db:tran_match(ply, #ply{_ = '_'}) of
%% 		[] -> skip;
%% 		List ->
%% 			lists:foreach(fun(#ply{sid=Sid,uid=Uid}) -> record_back(Uid),?discon(Sid, shutdown, 1) end, List)
%% 	end,
%% 	gen_server:cast({global, login}, {kick_all_usr}).

shutdown() ->
	gen_server:cast({global, agent_mng}, {shutdown}).



topN(N, binary)->
        [{P, M, process_info(P, [registered_name, initial_call,current_function, dictionary]), B} ||
        {P, M, B} <- lists:sublist(processes_sorted_by_binary(),N)];
topN(N, memory)->
	 [{P, M, process_info(P, [registered_name, initial_call,current_function, dictionary])} ||
        {P, M} <- lists:sublist(processes_sorted_by_memory(),N)];
topN(N, msg_queue_len) ->
	 [{P, M, process_info(P, [registered_name, initial_call,current_function, dictionary])} ||
        {P, M} <- lists:sublist(processes_sorted_by_msg_queue_len(),N)].

processes_sorted_by_binary()->
	L =
     [case process_info(P, binary) of
              {_, Bins} ->
                 SortedBins = lists:usort(Bins),
                 {_, Sizes, _} = lists:unzip3(SortedBins),
                 {P, lists:sum(Sizes), []};
              _ ->
                {P, 0, []}
         end ||P <- processes()],
	 lists:reverse(lists:keysort(2,L)).
processes_sorted_by_memory() ->
	L =
	 [case process_info(P, memory) of
              {_, Size} -> {P, Size};
              _ -> {P, 0}
         end ||P <- processes()],
	 lists:reverse(lists:keysort(2,L)).
processes_sorted_by_msg_queue_len() ->
	L =
	 [case process_info(P, message_queue_len) of
              {_, Len} -> {P, Len};
              _ -> {P, 0}
         end ||P <- processes()],
	lists:reverse(lists:keysort(2,L)).

	
