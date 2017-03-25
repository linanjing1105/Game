-module(key_svr).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3]).


-include("common.hrl").
-record(state, {name = nudefine}).

start_link([]) ->
    gen_server:start_link({global, key}, ?MODULE, [], []).

stop() ->
    gen_server:cast({global, key}, stop).

init([]) ->
    ?log("start key"),
	{ok, Host} = application:get_env(dbhost),
	{ok, Port} = application:get_env(dbport),
	{ok, Usr} = application:get_env(dbusr),
	{ok, Psw} = application:get_env(dbpsw),
	{ok, Database} = application:get_env(dbdatabase),
	{ok, Code} = application:get_env(dbcode),
	
	F1 = fun(_) -> 
			add_connect(?DB_S,Host, Port, Usr, Psw, Database,Code) 
	end,
	util:for(1,?SQLSCONNUM,F1),
	
	F = fun([Tab|V]) ->
    	initKey(?DB_S, V, atom_to_list(Tab))
    end,
    lists:foreach(F,?Tabs),
	?log("key started"),
	erlang:put(sys_scene,1),
%% 	erlang:put(sys_pet,150000001),	
	erlang:put(sys_team,1),
	erlang:put(sys_match_team,1),
    {ok, #state{name= key}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call({getkey,Tab}, _From,State) -> 
	Get = case erlang:get(Tab) of
		underfined -> fail;
		Key -> erlang:put(Tab,Key + 1),
			   Key
	end,
	{reply, Get, State};

handle_call(_Request, _From, State) ->
	?log_error("key unknow call:~p", [_Request]),
    {reply, ok, State}.

handle_info(_Request, State) ->
    ?debug("info, _Request = ~p ~n", [_Request]),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

initKey(Con, Fs, RecName) ->
	Pk = lists:nth(1, Fs),
	QryMax = "SELECT max(" ++ util:to_list(Pk) ++ ") FROM " ++ lists:concat([RecName]),
	?log("init tables ~s",[RecName]),
	ResMax=case mysql:fetch(Con, QryMax) of
		{data, {_,_,[[undefined]],_,_}} -> 0;
		{data, {_,_,[[Max]],_,_}} when Max < 0 -> 0;
		{data, {_,_,[[Max]],_,_}} -> Max;
		OtherMax -> ?log("init table ~s error,~p,QryMax=~p", [RecName, OtherMax,QryMax]),error
	end,
	?log("RecName=~p,ResMax=~p",[list_to_atom(RecName),ResMax]),	
%% 	io:format("RecName=~p,ResMax=~p~n",[list_to_atom(RecName),ResMax]),
	case ResMax of
		Data when erlang:is_integer(Data) -> erlang:put(list_to_atom(RecName),ResMax + 1);
		_ -> erlang:put(list_to_atom(RecName),1),?log("initKey error RecName = ~p,ResMax =~p",[RecName,ResMax])
	end.

add_connect(DB,Host, Port, Usr, Psw, Database,Code) ->
	?log_trace("dbm connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p", [DB,Host,Port,Usr,Database,Code]),
	case mysql:connect(DB, Host, Port, Usr, Psw, Database,Code, true) of
		{?OK, _} -> ?OK;
		Other1 -> ?log_trace("Error:dbm init,mysql:connect.Other=~p", [Other1])
	end.

