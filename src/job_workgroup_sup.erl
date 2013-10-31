
-module(job_workgroup_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
	supervisor:start_link(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({Name, SizeArgs, WorkArgs, AmqpParams} = Args) ->
	PoolArgs = [{name, {local, Name}}, {worker_module, job_worker}] ++ SizeArgs,
	AmqpArgs = lists:append(AmqpParams, [{poolName, Name}]),
	WorkerArgs = lists:append(WorkArgs, [{workgroup, Name}]),
	PoolSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
	job_util:setArgs(Name, Args),
	job_util:setSupervisor(Name, self()),
	{ok, { {one_for_one, 5, 10}, [PoolSpec, ?CHILD(job_amqp_emiter, worker, [AmqpArgs]), ?CHILD(job_amqp_worker, worker, [AmqpArgs])]} }.
