-module(fun_gateway_mng).

-export([init/0, regist/4, unregist/1, on_connect/1, on_disconnect/1, allocate_gateway/0, shutdown/0]).

-include("log.hrl").

-record(gateway_svr, {index,address,port,max_num,cur_num}).

init() ->
	ets:new(gateway_svr, [set,public,named_table,{keypos, #gateway_svr.index}]).


regist(Index,MaxSocketNum,MyAddress,Port) ->
	case ets:lookup(gateway_svr, Index) of
		[Old] ->
			?log_warning("gateway regist failed,index=~p,ip=~p,port=~p,old_ip=~p,old_port=~p,old_num=~p",[Index,MyAddress,
							Port,Old#gateway_svr.address,Old#gateway_svr.port,Old#gateway_svr.cur_num]),
			{error, duplicate_index};
		_ -> 
			?log_trace("gateway registered,index=~p,ip=~p,port=~p",[Index,MyAddress,Port]),
			ets:insert(gateway_svr, #gateway_svr{index = Index, address = MyAddress,port = Port,max_num = MaxSocketNum,cur_num = 0}),
			ok
	end.

unregist(Index) ->
	case ets:lookup(gateway_svr, Index) of
		[Old] ->
			?log_trace("gateway unregisterd,index=~p,ip=~p,cur_num=~p",[Index,Old#gateway_svr.address,Old#gateway_svr.cur_num]),
			ets:delete(gateway_svr, Index);
		_ -> skip
	end,
	ok.

on_connect(Index) ->
	case ets:lookup(gateway_svr, Index) of
		[Old] -> 
			NewNum = Old#gateway_svr.cur_num + 1,
			?debug("on_connect,Index:~p,Num:~p",[Index,NewNum]),
			ets:insert(gateway_svr, Old#gateway_svr{cur_num = NewNum});
		_ -> error
	end.

on_disconnect(Index) ->
	case ets:lookup(gateway_svr, Index) of
		[Old] -> 
			NewNum = Old#gateway_svr.cur_num - 1,
			?debug("on_disconnect,Index:~p,Num:~p",[Index,NewNum]),
			ets:insert(gateway_svr, Old#gateway_svr{cur_num = NewNum});
		_ -> error
	end.

allocate_gateway() ->
	case ets:match_object(gateway_svr, #gateway_svr{_='_'}) of
		[] -> error;
		List ->
			[#gateway_svr{address=Address,port=Port}|_] = lists:keysort(#gateway_svr.cur_num, List),
			{ok, {Address, Port}}
	end.

shutdown() ->
	case ets:match_object(gateway_svr, #gateway_svr{_='_'}) of
		[] -> skip;
		List ->
			F = fun(#gateway_svr{index=Index}) ->
					server_tools:send_to_gateway_ctr(Index, stop)
				end,
			lists:foreach(F, List)
	end.
			
