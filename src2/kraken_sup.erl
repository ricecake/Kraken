
-module(kraken_sup).

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
	{ok, ControlParams}   = application:get_env(kraken, controllerParams),
	{ok, { {one_for_one, 5, 10}, [?CHILD(kraken_controller, worker, [ControlParams]),
				      ?CHILD(kraken_tasker_sup, supervisor, []),
				      ?CHILD(kraken_pool_sup, supervisor, [])
                                ]} }.
