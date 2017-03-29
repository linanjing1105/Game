-module(gateway_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10}, [{gateway_ctr, {gateway_ctr, start_link, []}, transient, 30000, worker, [gateway_ctr]}]}}.
