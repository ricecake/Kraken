
-module(kraken_tasker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
        PoolArgs = [{name, {local, tasker}}, {worker_module, kraken_tasker}, {size, 5}, {max_overflow, 2000}],
        PoolSpec = poolboy:child_spec(tasker, PoolArgs, []),
    {ok, { {one_for_one, 5, 10}, [PoolSpec]} }.

