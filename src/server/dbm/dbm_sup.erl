-module(dbm_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([start/0]).

%%--------------------------------------------------------------------
start_link(Data) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Data).

start() ->
    ?MODULE:start_link().

init(Data) ->
	DbmSer = {dbm_server, {dbm_svr, start_link, [[]]}, permanent, 2000, worker, [dbm_svr]},
	KeySer = {key_server, {key_svr, start_link, [[]]}, permanent, 2000, worker, [key_svr]},
	MysqlSer = {mysql_server, {mysql, start_link, Data}, permanent, 2000, worker, [mysql]},
    RestartStrategy = {one_for_one, 3, 100000},
	{ok, {RestartStrategy, [MysqlSer,KeySer,DbmSer]}}.


