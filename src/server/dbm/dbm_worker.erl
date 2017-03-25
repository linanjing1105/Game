-module(dbm_worker).

%% -behaviour(worker).

-export([do_init_factory/1,do_info_factory/2,do_init/2, do_work/2,get_task/1,do_terminate/2,do_call_factory/3]).
-export([start/1,get_factory_name/0,work/1]).

-include("common.hrl").

-record(info, {con = 0, idx = 0}).


work(Arg)->
	try
		db:mysql_run(Arg)
	catch E:R ->
		?log_error("dbm_worker work error,Arg=~p,E=~p,R=~p,stack=~p",[Arg,E,R,erlang:get_stacktrace()])
	end.

start(Args) -> 
	factory_sup:start_link({{?DBM_WORK_NAME,?MODULE,Args},?DBM_WORK_MAX,done}).

get_factory_name() -> 
	factory:get_factory_name(?DBM_WORK_NAME).

get_task(Info) -> 
	Tasks = case get(task_list) of
				[ThisTasks | NextTasks] ->
					put(task_list,NextTasks),
					ThisTasks;
				_ -> 
					case catch gen:call(info_handle_worker:get_factory_name(), '$gen_call', gettask,infinity) of
						{ok,Res} ->
							case Res of
								{TaskList1,TaskList2,TaskList3} -> 
									put(task_list,[TaskList2,TaskList3]),
									TaskList1;
								_ -> []
							end;	    
						{'EXIT',Reason} ->
							?log_error("get_task exit,Reason=~p",[Reason]),
							[]
					end
			end,
	{ok,Tasks,Info}.

do_terminate(R,Idx) ->
	?log_error("terminate R =~p,Idx=~p",[R,Idx]),
	ok.

do_init_factory(_) -> 
	{ok,ok}.

do_init(Idx,{Con}) ->
	{ok, #info{ con = Con,idx = Idx}};
do_init(Idx,Arg) ->
	{error, {badarg,{idx,Idx},{arg,Arg}}}.

do_call_factory(_Request,_From,Info)-> 
	?debug("do_call_factory Request=~p",[_Request]),
	{ok,{},Info}.

do_info_factory(_Request,Info) -> ?debug("Request=~p",[_Request]),{ok,free,Info}.
	
do_work(#sql_data{key={Tab,_Id},opType=change,rec=Rec},Info) ->
	work({update,Tab,Rec}),
	{ok,Info};

do_work(#sql_data{key={Tab,_Id},opType=add,rec=Rec},Info) ->
	work({insert_sql,Tab,Rec}),
	{ok,Info};

do_work(#sql_data{key={Tab,Id},opType=del,rec=_Rec},Info) ->
	work({delete,Tab, Id}),
	{ok,Info};

do_work(_Task,Info) ->
%% 	?debug("do_work Task=~p",[_Task]),
	{ok,Info}.
