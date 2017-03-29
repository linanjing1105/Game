-module(scene_ctr_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->    
    {ok, Name} = application:get_env(name),
    {ok, Cookie} = application:get_env(cookie),
    net_kernel:start([Name, longnames]),
	util:sleep(1000),
    erlang:set_cookie(node(), Cookie),

    {ok, Dbm} = application:get_env(dbm),
    check_start(Dbm).    

stop(_State) ->    
    ok.

check_start(Dbm) ->
	?log_trace("connect dbm..."),
	case net_adm:ping(Dbm) of
		pong -> 
			global:sync(),
			case db:start() of
				ok -> do_start();
				_ -> util:sleep(2000),
					 check_start(Dbm)
			end;			
		_ -> util:sleep(2000),
			 check_start(Dbm)
	end.

do_start()-> 
	db:load_tables(?SceneTabs),	
    case scene_ctr_sup:start_link() of      
        {ok, Pid} ->      {ok, Pid};      
        Other ->          {error, Other}    
    end.


