-module(agent_ctr_sup).
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
    AgentCtr = {agent_ctr, {agent_ctr, start_link, []}, permanent, 5000, worker, [agent_ctr]},
    AgentSup = {agent_sup, {agent_sup, start_link, []}, permanent, 5000, supervisor, [agent_sup]},
    RestartStrategy = {one_for_one, 3, 100000},
	{ok, {RestartStrategy, [AgentCtr, AgentSup]}}.



