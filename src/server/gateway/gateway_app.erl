-module(gateway_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("log.hrl").

start(_Type, _Args) ->
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
			do_start();
		_ -> util:sleep(2000),
			 check_start(Dbm)
	end.


do_start()->
	case gateway_sup:start_link() of      
        {ok, Pid} ->          {ok, Pid};      
        Other ->          {error, Other}    
    end.

