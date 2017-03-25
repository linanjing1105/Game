-module(dbm_app).
-behaviour(application).

-export([start/2, stop/1]).
-include("common.hrl").

start(_StartType, _StartArgs) ->    
    {ok, Name} = application:get_env(name),
    {ok, Cookie} = application:get_env(cookie),
    net_kernel:start([Name, longnames]),
	util:sleep(1000),
    erlang:set_cookie(node(), Cookie),

	{ok, Host} = application:get_env(dbhost),
	{ok, Port} = application:get_env(dbport),
	{ok, Usr} = application:get_env(dbusr),
	{ok, Psw} = application:get_env(dbpsw),
	{ok, Database} = application:get_env(dbdatabase),
	{ok, Code} = application:get_env(dbcode),
	
	DbData = [?DB,Host,Port,Usr,Psw,Database, fun(_, _, _, _) -> ok end,Code],
    case dbm_sup:start_link(DbData) of      
        {ok, Pid} ->          {ok, Pid};      
        Other ->          {error, Other}    
    end.

stop(_State) ->    
    ok.


