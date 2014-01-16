
-module(message_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
        ListenArgs = [{name, {local, tasker}}, {worker_module, kraken_tasker}, {size, 5}, {max_overflow, 2000}],
        ListenSpec = poolboy:child_spec(tasker, ListenArgs, []),
	{ok, { {one_for_one, 5, 10}, [ListenSpec]} }.
