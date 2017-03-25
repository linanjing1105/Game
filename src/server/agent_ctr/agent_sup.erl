-module(agent_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start/0]).
-export([add/6]).

%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    ?MODULE:start_link([]).

init([]) ->
    Children = {agent, {agent, start_link, []}, temporary, 2000, worker, [agent]},
    RestartStrategy = {simple_one_for_one, 10, 10},
    {ok, {RestartStrategy, [Children]}}.

add(Sid,Ip,Seq,Aid,Uid,AgentIdx) ->
    supervisor:start_child(?MODULE, [{Sid,Ip,Seq,Aid,Uid,AgentIdx}]).


