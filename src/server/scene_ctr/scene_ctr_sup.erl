-module(scene_ctr_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start/0]).

%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    ?MODULE:start_link().

init([]) ->
    SceneCtr = {scene_ctr, {scene_ctr, start_link, []}, permanent, 5000, worker, [scene_ctr]},
	SceneSup = {scene_sup, {scene_sup, start_link, []}, permanent, 5000, supervisor, [scene_sup]},
    RestartStrategy = {one_for_one, 3, 100000},
	{ok, {RestartStrategy, [SceneCtr,SceneSup]}}.



