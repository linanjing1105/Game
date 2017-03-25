-module(world_sup).
-behaviour(supervisor).
-export([start/0,start_link/0,init/1]).
-include("common.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    ?MODULE:start_link().

init([]) ->
	SvrList = [	
			   {login,		{world_svr,start_link,[login,fun_login]},permanent, 5000, worker, [world_svr]},
			   %{mynet_mng,	{world_svr,start_link,[mynet_mng,fun_mynet_mng]},permanent, 5000, worker, [world_svr]},
			  % {scene_mng, 	{world_svr,start_link,[scene_mng,fun_scene_mng]},permanent, 5000, worker, [world_svr]},
%			   {agent_mng, 	{world_svr,start_link,[agent_mng,fun_agent_mng]},permanent, 5000, worker, [world_svr]},
			  % {offline_agent_mng, 	{world_svr,start_link,[offline_agent_mng,fun_offline_agent_mng]},permanent, 5000, worker, [world_svr]}
			  ],
    RestartStrategy = {one_for_one, 3, 10},
	?log("Info:public_sup init"),
	{ok, {RestartStrategy, SvrList}}.
