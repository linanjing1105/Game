-module(info_handle_worker).

%% -behaviour(worker).

-export([do_init_factory/1,do_info_factory/2,do_init/2, do_work/2,get_task/1,do_terminate/2,do_call_factory/3]).
-export([start/1,get_factory_name/0,new_task/1]).

-include("common.hrl").

-record(info, { idx = 0}).

start(Args) -> 
	factory_sup:start_link({{?INFO_WORK_NAME,?MODULE,Args},?INFO_WORK_MAX,fast}).

get_factory_name() -> 
	factory:get_factory_name(?INFO_WORK_NAME).

get_task(Info) -> {ok,[],Info}.

do_terminate(R,Idx) ->
	?log_error("terminate R =~p,Idx=~p",[R,Idx]),
	ok.

do_init_factory(_) -> 
	db:init_sql_data(),
	subscribe_tab(),
	{ok,ok}.

do_init(Idx,{}) ->
	{ok, #info{ idx = Idx}};
do_init(Idx,Arg) ->
	{error, {badarg,{idx,Idx},{arg,Arg}}}.

do_call_factory(gettask,_From,Info)-> 
	{ok,db:pick_all_sql_data(),Info};

do_call_factory(_Request,_From,Info)-> 
%% 	?log("do_call_factory Request=~p",[_Request]),
	{ok,{},Info}.

do_info_factory({mnesia_table_event, {write, schema, _NewRec, _OldRec, _ActivityId}},Info) ->
	{ok,free, Info};
do_info_factory({mnesia_table_event, {write, Tab, NewRec, OldRec, _ActivityId}},Info) ->
	case Tab of
		_ ->
			case OldRec of
				[] -> Id = erlang:element(2, NewRec),
					  case db:check_sql_id(Tab,Id) of 
						  add -> {ok, {(Id rem ?INFO_WORK_MAX) + 1, #sql_data{key={Tab,Id},opType=add,rec=NewRec}},Info};
						  _ -> {ok,free, Info}
					  end;
		        [Old] when Old =:= NewRec -> 
 					?debug("Warning:dbm write,data no change,Rec=~p",[Old]),
					{ok,free, Info};
				_-> Id = erlang:element(2, NewRec),{ok, {(Id rem ?INFO_WORK_MAX) + 1, #sql_data{key={Tab,Id},opType=change,rec=NewRec}},Info}
		    end
	end;

do_info_factory({mnesia_table_event, {delete, schema, {_Tab, _Key}, _OldRec, _ActivityId}},Info) ->
	?debug("truncate table:~p,", [_Key]),
	{ok,free, Info};
do_info_factory({mnesia_table_event, {delete, _Tab, {Tab, Key}, OldRec, _ActivityId}},Info) ->
	case Tab of
		recharge -> {ok,free, Info};
		acc -> {ok,free, Info};
		reportfile -> {ok,free, Info};
		_ -> {ok,{(Key rem ?INFO_WORK_MAX) + 1,#sql_data{key={Tab,Key},opType=del,rec=OldRec}}, Info}
	end;

do_info_factory(_Request,Info) -> ?debug("Request=~p",[_Request]),{ok,Info}.
	
do_work(TaskData,Info) when erlang:is_record(TaskData, sql_data) ->
	new_task(TaskData),
	{ok,Info};

do_work(_Task,Info) ->
%% 	?log("do_work Task=~p",[Task]),
	{ok,Info}.

subscribe_tab()->
    F = fun([T|_V]) -> mnesia:subscribe({table, T, detailed}) end,
    lists:foreach(F, ?Tabs).

new_task(TaskData = #sql_data{key=Key,opType=add,rec=Rec}) ->
%% 	?debug("new_task add Key=~p,Rec=~p",[Key,Rec]),
	case db:get_sql_data(Key) of
		?UNDEFINED -> fail;
		null -> db:insert_sql_data(TaskData);
		#sql_data{opType=add} -> ?log_error("double add Rec=~p",[Rec]);
		#sql_data{opType=change} -> ?log_error("change befor add Rec=~p",[Rec]);
		#sql_data{opType=del} -> ?log_error("del befor add Rec=~p",[Rec]);
		R -> ?log_error("unknow Task R = ~p",[R])
	end;

new_task(TaskData = #sql_data{key=Key,opType=change,rec=Rec}) ->
%% 	?debug("new_task change Key=~p,Rec=~p",[Key,Rec]),
	case db:get_sql_data(Key) of
		?UNDEFINED -> fail;
		null -> db:insert_sql_data(TaskData);
		#sql_data{opType=add} -> db:insert_sql_data(TaskData#sql_data{opType=add});
		#sql_data{opType=change} -> db:insert_sql_data(TaskData);
		#sql_data{opType=del} -> ?log_error("del befor change Rec=~p",[Rec]);
		R -> ?log_error("unknow Task R = ~p",[R])
	end;

new_task(TaskData = #sql_data{key=Key,opType=del,rec=Rec}) ->
%% 	?debug("new_task del Key=~p,Rec=~p",[Key,Rec]),
	case db:get_sql_data(Key) of
		?UNDEFINED -> fail;
		null -> db:insert_sql_data(TaskData);
		#sql_data{opType=add} -> db:delete_sql_data(Key);
		#sql_data{opType=change} -> db:insert_sql_data(TaskData#sql_data{opType=del});
		#sql_data{opType=del} -> ?log_error("double del Rec=~p",[Rec]);
		R -> ?log_error("unknow Task R = ~p",[R])
	end;

new_task(T) ->
	?log_error("unknow Task T = ~p",[T]).
		

