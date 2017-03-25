-module(world_app).

-include("common.hrl").

-export([start/2,stop/1]).

start(_StartType, _StartArgs) ->    
    {ok, Name} = application:get_env(name),
    {ok, Cookie} = application:get_env(cookie),
    net_kernel:start([Name, longnames]),
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
				ok ->
					wait_for_dbm_init(),
					do_start();
				_ -> util:sleep(2000),
					 check_start(Dbm)
			end;			
		_ -> util:sleep(2000),
			 check_start(Dbm)
	end.

wait_for_dbm_init() ->
	?log_trace("wait for dbm init..."),
	case server_tools:call_persist({global, dbm}, where, 2000) of
		{ok, _Node} -> ok;
		_ -> wait_for_dbm_init()
	end.


do_start()-> 
	db:load_tables(?WorldTabs),
	inets:start(),
    case world_sup:start_link() of      
        {ok, Pid} -> {ok, Pid};
        Other ->     {error, Other}
    end.



