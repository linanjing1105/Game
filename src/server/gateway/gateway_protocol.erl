-module(gateway_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("log.hrl").
-include("tools.hrl").

%% API.
-export([start_link/4]).


%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket, transport, sid, uid, agent_pid, scene_pid, ip, recv_buf, index}).

-define(HEADER_SIZE, 4).
-define(MIN_MSG_SIZE, 6).	%% proto_id(2 bytes) + seq(4 bytes)
-define(MAX_MSG_SIZE, 8192).

-define(PT_ASK_PING,16#A10A).


start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
	process_flag(trap_exit, true),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once},{keepalive,true},{send_timeout,10000}]),
	?debug("~p,~p,~p",[Socket, self(), Opts]),
	State = on_accept(Socket, Transport, Opts),
	gen_server:enter_loop(?MODULE, [], State).

handle_info({tcp, Socket, Data}, State=#state{
		socket=Socket, transport=Transport, recv_buf=RecvBuf}) ->
	Transport:setopts(Socket, [{active, once}]),
	try
		{ok, RecvBuf1} = parse(State, <<RecvBuf/binary, Data/binary>>),
		%Transport:send(Socket, Data),
		{noreply, State#state{recv_buf=RecvBuf1}}
	catch E:R ->
		?log_error("parse data error,E=~p,R=~p,stack=~p",[E,R,erlang:get_stacktrace()]),
		{stop, normal, State}
	end;
handle_info({tcp_closed, _Socket}, State) ->
	?debug("tcp_closed"),
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	?debug("tcp error,reason=~p",[Reason]),
	{stop, Reason, State};
%% handle_info(timeout, State) ->
%% 	?debug("tcp timeout"),
%% 	{noreply, State};
handle_info(Info, State) ->
	?debug("recv unknown info,msg=~p",[Info]),
	{noreply, State}.

handle_call(_Request, _From, State) ->
	?debug("unknown call,msg=~p",[_Request]),
	{reply, ok, State}.

handle_cast({discon,Reason}, State) ->
	?debug("disconnect,reason=~p", [Reason]),
	%Transport:close(Sock),
	{stop, normal, State};
handle_cast({send, Data}, State=#state{socket=Socket, transport=Transport}) ->
	case Transport:send(Socket, Data) of
		ok -> {noreply, State};
		Error -> 
			?debug("send packet error,sid=~p,reason=~p",[self(),Error]),
			{stop, normal, State}
	end;
	
handle_cast({set_val, agent_pid, AgentPid}, State) ->
	{noreply, State#state{agent_pid=AgentPid}};
handle_cast({set_val, scene_pid, ScenePid}, State) ->
	{noreply, State#state{scene_pid=ScenePid}};
handle_cast({set_val, uid, Uid}, State) ->
	{noreply, State#state{uid=Uid}};
handle_cast(_Msg, State) ->
	?debug("unknown cast,msg=~p",[_Msg]),
	{noreply, State}.

terminate(Reason, State) ->
	?debug("terminate,reason=~p",[Reason]),
	on_close(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

on_accept(Socket, Transport, Opts) ->
	?debug("on_accept"),
	{index, Index} = lists:keyfind(index, 1, Opts),
	server_tools:send_to_login({gateway_conn_add, Index}),
	Ip = 
		case Transport:peername(Socket) of
			{ok, {Addr, _Port}} -> Addr;
			_ -> {0,0,0,0}
		end,
	Buf = <<>>,
	
	#state{socket=Socket, transport=Transport,recv_buf=Buf,ip=Ip,sid=self(),agent_pid=0,index=Index}.

parse(State=#state{sid=Sid}, Data) ->
	case Data of
		<<Len:?u32, Remain/binary >> ->			
			BodyLen = Len - ?HEADER_SIZE,
			%?debug("-------------~p,~p",[Data,BodyLen]),
			if 
				BodyLen < ?MIN_MSG_SIZE -> ?discon(Sid,'msg error minibody',4000),{ok, <<>>};
				BodyLen > ?MAX_MSG_SIZE -> ?discon(Sid,'msg error maxbody',4000),{ok, <<>>};
				true ->
					case Remain of
						<<Data1:BodyLen/binary, Remain2/binary >> ->
							%?debug("-----,~p",[Data1]),
							on_packet_recv(Data1, State),
							case byte_size(Remain2) > 0 of
				                true ->					
				                    parse(State, Remain2);
				                _ ->
				                    {ok, <<>>}
				            end;
						_ ->
					         {ok, Data}
					end
			end;
		_ -> {ok, Data}        
    end.

on_packet_recv(Data, State=#state{sid=Sid}) ->
	case Data of
		<<?PT_ASK_PING:?u16, _Seq:?u32, _Data/binary>>->
			?debug("PT_ASK_PING get"),
			Len = byte_size(Data) + ?HEADER_SIZE,
			?send(Sid, <<Len:?u32,Data/binary>>);
		_ ->
			parse_pt(Data, State)
	end.

parse_pt(Data, #state{sid=Sid,agent_pid=AgentPid,scene_pid=ScenePid,uid=Uid,ip=Ip}) ->
	case server_tools:parse_pt(Data) of
		{ok,ProtoId,PtModule,Seq,Pt} ->
 			?debug("recv_pt,name=~p,Seq=~p,data=~p",[PtModule,Seq,Pt]),
			case fun_agent_pt_post:fill_pt(ProtoId) of
				agent ->
					if
						erlang:is_pid(AgentPid) -> server_tools:send_to_agent(AgentPid, {process_pt, PtModule, Seq, Pt, Sid});
						true -> skip
					end;
				scene ->
					if
						erlang:is_pid(ScenePid) ->
							server_tools:send_to_scene(ScenePid, {process_pt, PtModule, Seq, Pt, Sid, Uid});
						true -> skip
					end;
				login -> server_tools:send_to_login({process_pt,PtModule, Seq, Pt, Sid, Ip});
				agent_mng -> server_tools:send_to_agent_mng({process_pt, PtModule, Seq, Pt, Sid, Uid})
			end;
		R -> ?log_warning("parse pt failed,R=~p,Data=~p",[R,Data])
	end.

on_close(#state{sid=Sid,agent_pid=AgentPid,index=Index}) ->
	case is_pid(AgentPid) of
		true -> server_tools:send_to_agent(AgentPid, gateway_tcp_closed);
		_ -> server_tools:send_to_login({gateway_tcp_closed, Sid})
	end,
	server_tools:send_to_login({gateway_conn_dec, Index}).


%% test() ->
%% 	Data = <<0,0,0,49,192,1,0,0,0,0,0,0,0,57,0,0,0,1,67,89,0,0,1,0,2,67,154,29,41,65,135,70,83,66,189,213,24,67,156,238,19,65,135,66,33,66,187,144,235>>,
%% 	F = fun(_)->parse(#state{},Data) end,
%% 	F1 = fun()->util:for(1,1000000,F) end,
%% 	timer:tc(F1).
