-module(dbm_svr).
-behaviour(gen_server).

-export([start_link/1, stop/0, init/1, terminate/2, code_change/3, handle_cast/2, handle_info/2, handle_call/3]).


-include("common.hrl").
-record(state, {name = nudefine,dbData = {},inited = false,con = ""}).

start_link([]) ->
    gen_server:start_link({global, dbm}, ?MODULE, [], []).

stop() ->
    gen_server:cast({global, dbm}, stop).

init([]) ->
    ?log("start dbm"),
	ok = mnesia:start(),
	erlang:send_after(100, self(), {initdb}),
	
	{ok, Host} = application:get_env(dbhost),
	{ok, Port} = application:get_env(dbport),
	{ok, Usr} = application:get_env(dbusr),
	{ok, Psw} = application:get_env(dbpsw),
	{ok, Database} = application:get_env(dbdatabase),
	{ok, Code} = application:get_env(dbcode),
	DbData = {Host,Port,Usr,Psw,Database,Code},
	
    {ok, #state{dbData= DbData}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

handle_call(where, _From, #state{inited = true} = State) -> {reply, {ok,node()}, State};
handle_call(where, _From, State) -> {reply, {starting,node()}, State};

handle_call({gettablesize, Tab}, _From, State) ->
	Rs = case mnesia:table_info(Tab, size) of
		 Size when is_integer(Size) -> Size;
		 Other -> ?log_error("Error:get_table_size error,table=~p,Other=~p", [Tab, Other]),0
		 end,
	{reply,Rs,State};

handle_call(_Request, _From, State) ->
	?log_error("dbm unknow call:~p", [_Request]),
    {reply, ok, State}.
	
handle_info({initdb},#state{dbData = {Host,Port,Usr,Psw,Database,Code}} = State) ->
	?log("init_db"),
	load_config(),

	
	F = fun(_) -> 
			add_connect(?DB,Host, Port, Usr, Psw, Database,Code) 
	end,
	?log("dbm connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p,ConNUM=~p", [?DB,Host,Port,Usr,Database,Code,?SQLCONNUM]),
	util:for(1,?SQLCONNUM - 1,F),
	
	?log("dbm init tab "),
    initTab(?DB_S),

	?log("dbm init tables end"),
	loadCache(?DB_S),

	dbm_worker:start({?DB}),
	info_handle_worker:start({}),
	
	?log("dbm load tables"),
	[db:mysql_run({loadall,StartLoadDB}) || StartLoadDB <- ?StartLoadList],
	?log("dbm load tables end"),
	
	?log("dbm started"),
    {noreply, State#state{name = dbm_worker:get_factory_name(), con = ?DB,inited=true}};

handle_info(_Request, State) ->
    ?debug("info, _Request = ~p ~n", [_Request]),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({check}, State) ->
    gen_server:cast({global, sm}, {checkok,"dbm ok"}),
	{noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.
			
initLoad(Fs, Tab) ->
    Fun = fun(Elem, AccIn) ->
			io_lib:format("~s`~s`,", [AccIn,atom_to_list(Elem)])	  
        end,
	Fst1 = lists:foldl(Fun, "", Fs),
	StrFields = lists:sublist(Fst1, length(Fst1) -1 ),
    Fst = "SELECT " ++ Fst1,
    Str = lists:sublist(Fst, length(Fst) -1 ) ++ " FROM " ++ lists:concat([Tab]) ++ " WHERE id= ?",
	SqlStr = lists:flatten(Str),	
	mysql:prepare(db:get_db_load_name(Tab), util:to_binary(SqlStr)),
	
	Str1 = lists:sublist(Fst, length(Fst) -1 ) ++ " FROM " ++ lists:concat([Tab]),
	SqlStr1 = lists:flatten(Str1),	
	mysql:prepare(db:get_db_load_all_name(Tab), util:to_binary(SqlStr1)),
	
	mnesia:transaction(fun() -> mnesia:write(#sql_config{key=Tab, sql=StrFields}) end).

initSave(Con, Fs, RecName) ->
    F1 = fun(Elem, AccIn) ->
            io_lib:format("~s`~s`,", [AccIn,atom_to_list(Elem)])
        end,
    Fst = lists:foldl(F1, "SELECT ", Fs),
    Qry = lists:sublist(Fst, length(Fst) -1 ) ++ " FROM " ++ lists:concat([RecName]) ++ " WHERE ",
	Pk = lists:nth(1, Fs),
	QryMax = "SELECT max(" ++ util:to_list(Pk) ++ ") FROM " ++ lists:concat([RecName]),
	?log("init tables ~s",[RecName]),
	ResMax=case mysql:fetch(Con, QryMax) of
		{data, {_,_,[[undefined]],_,_}} -> 0;
		{data, {_,_,[[Max]],_,_}} when Max < 0 -> 0;
		{data, {_,_,[[Max]],_,_}} -> Max;
		OtherMax -> ?log_error("init table ~s error,error=~p,sql=~p", [RecName, OtherMax, QryMax]),error
	end,
	?log("ResMax=~p",[ResMax]),
	
    Sql = lists:concat([Qry, Pk, " = ", 999999999]),
	Res=case mysql:fetch(Con, Sql) of
		{data, R} -> R;
		Other -> ?log_error("init table ~s error,error=~p,sql=~p", [RecName, Other, Sql]),error
	end,
    Fields = mysql:get_result_field_info(Res),
    
%%     Fun = fun({_Tab, Field, _Length, Mode}) ->
%%             FieldName = binary_to_list(Field),
%%             case Mode of
%%                 'VAR_STRING' -> 
%%                     {util:to_list(FieldName) ++ "='~s'"};
%% 				'BLOB' ->
%% 					{util:to_list(FieldName) ++ "='~s'"};
%%                 _ -> 
%% 					{util:to_list(FieldName) ++ "=~p"}
%%             end
%%     end,

	Fun = fun({_Tab, Field, _Length, _Mode}) ->
            FieldName = binary_to_list(Field),
			{"`" ++ util:to_list(FieldName) ++ "`=?"}
    end,
	
    [{Where}|SetList] = lists:map(Fun, Fields),
				  
    FunT = fun({SetStr}, AccIn) -> 
            AccIn ++ "," ++ SetStr
    end,
    [_|Set] = lists:foldl(FunT, [], SetList),
	
	Str = io_lib:format("UPDATE ~s SET ~s WHERE ~s", [RecName, Set, Where]),
%% 	Str = io_lib:format("REPLACE INTO ~s (~s) VALUES (~s)", [RecName, Title, Fmt]),
	SqlStr = lists:flatten(Str),
	mysql:prepare(db:get_db_save_name(util:to_atom(RecName)), util:to_binary(SqlStr)),
	
%% 	Key = {list_to_atom(RecName),save},
%% 	case mnesia:transaction(fun() -> mnesia:write(#sql_config{key=Key, sql=SqlStr}) end) of
%% 	{atomic, _} -> ok;
%% 	Other1 -> ?log_error("Error:write sql_config error,other=~p", [Other1])
%% 	end,
	
	KeyKey = list_to_atom(RecName),
	case mnesia:transaction(fun() -> mnesia:write(#sql_key_static{key=KeyKey, max=ResMax}) end) of
	{atomic, _} -> ok;
	Other3 ->
		?log_error("Error:write sql_key_static error,key=~p,other=~p", [KeyKey,Other3])
	end.

initInsert(Con, Fs, RecName) ->
    F1 = fun(Elem, AccIn) ->
			case Elem of
				id -> AccIn;% ignore the id column
				_ -> io_lib:format("~s`~s`,", [AccIn,atom_to_list(Elem)])
			end
        end,
    Fst = lists:foldl(F1, "SELECT ", Fs),
    Qry = lists:sublist(Fst, length(Fst) -1 ) ++ " FROM " ++ lists:concat([RecName]) ++ " WHERE ",

	Pk = lists:nth(1, Fs),
    Sql = lists:concat([Qry, Pk, " = ", 999999999]),
    %Sql = lists:concat([Qry, "id = ", 999999999]),	
		
	%?log("init tables ~s",[RecName]),
	Res=case mysql:fetch(Con, Sql) of
		{data, R} -> R;
		Other -> ?log_error("init table ~s error,error=~p,sql=~p", [RecName, Other,Sql]),error
	end,
    Fields = mysql:get_result_field_info(Res),
    
%%     Fun = fun({_Tab, Field, _Length, Mode}, AccIn) ->
%%             FieldName = binary_to_list(Field),
%% 			case FieldName of
%% 				"id" -> AccIn;% ignore the id column
%% 				_ ->
%% 		            case Mode of
%% 		                'VAR_STRING' -> 
%% 		                    [{FieldName, "'~s'", "Rec#" ++ RecName ++ "." ++FieldName } | AccIn];
%% 						'BLOB' ->
%% 							[{FieldName, "'~s'", "Rec#" ++ RecName ++ "." ++FieldName } | AccIn];
%% 		                _ -> 
%% 		                    [{FieldName, "~p",   "Rec#" ++ RecName ++ "." ++ FieldName } | AccIn]
%% 		            end
%% 			end
%%     end,

	Fun = fun({_Tab, Field, _Length, _Mode}, AccIn) ->
            FieldName = binary_to_list(Field),
			[{FieldName, "?", "Rec#" ++ RecName ++ "." ++FieldName } | AccIn]
    end,
    A = lists:foldr(Fun, [], Fields),

    FunT = fun({B, _, _}, AccIn) -> 
            AccIn ++ ",`" ++ B ++ "`"
    end,
    [_|Title] = lists:foldl(FunT, [], A),

    FunF = fun({_, Title1, _}, AccIn) -> 
            AccIn ++ "," ++ Title1
    end,
    [_|Fmt] = lists:foldl(FunF, [], A),

    Str = io_lib:format("INSERT INTO ~s (~s) VALUES (~s)", [ RecName, Title, Fmt]),
	SqlStr = lists:flatten(Str),
	
%% 	Key = {list_to_atom(RecName),insert_re_id},
%% 	mnesia:transaction(fun() -> mnesia:write(#sql_config{key=Key, sql=SqlStr}) end),
	
	mysql:prepare(db:get_db_insert_no_id_name(util:to_atom(RecName)), util:to_binary(SqlStr)),
	
	TitleId = lists:foldl(FunT, "id", A),
	FmtId = lists:foldl(FunF, "?", A),
	
	StrId = io_lib:format("INSERT INTO ~s (~s) VALUES (~s)", [ RecName, TitleId, FmtId]),
	SqlStrId = lists:flatten(StrId),
%% 	KeyId = {list_to_atom(RecName),insert},
%% 	mnesia:transaction(fun() -> mnesia:write(#sql_config{key=KeyId, sql=SqlStrId}) end).
	mysql:prepare(db:get_db_insert_name(util:to_atom(RecName)), util:to_binary(SqlStrId)).

initTab(Con) ->
    F = fun([Tab|V]) ->
    	initSave(Con, V, atom_to_list(Tab))
    end,
    lists:foreach(F, ?Tabs),
	
    F1 = fun([T|V]) ->
        initLoad(V, T)
    end,
    lists:foreach(F1, ?Tabs),

    F2 = fun([T|V]) ->
        initInsert(Con, V, atom_to_list(T))
    end,
    lists:foreach(F2, ?Tabs),
    ok.

%%dbm start load
loadCache(_Con) ->
	ok.

load_config() ->
	?log("load_config"),
	mnesia:wait_for_tables([config,
							sql_config,
							sql_key_static
							], infinity),
	
	{ok, Serverid} = application:get_env(serverid),
	db:set_all_config(serverid, Serverid),
	?log("all config read").

	
add_connect(DB,Host, Port, Usr, Psw, Database,Code) ->
	?log("dbm connect db,DB=~p,Host=~p,Port=~p,Usr=~p,Database=~p,Code=~p", [DB,Host,Port,Usr,Database,Code]),
	case mysql:connect(DB, Host, Port, Usr, Psw, Database,Code, true) of
		{?OK, _} -> ?OK;
		Other1 -> ?log_error("Error:dbm init,mysql:connect.Other=~p", [Other1])
	end.

