
-module(job_workpool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, createWorkgroup/1, stopWorkgroup/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, I, Type, Args), {Name, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	job_controler ! {initialize, boot},
	{ok, { {one_for_one, 5, 10}, []} }.

%{sample, [{size, 50}, {max_overflow, 200}], [{program, "sample.pl"},{args, []}], [{connection, Connection},{listen, {exchange, <<"SonarEvents">>, [<<"#">>]}}]}
createWorkgroup({Name, _SizeArgs, _WorkerArgs, _AmqpParams} = Args) ->
	job_util:startWorkGroup(Name),
	supervisor:start_child(?MODULE, ?CHILD(Name, job_workgroup_sup, supervisor, [Args])).

stopWorkgroup(Name) ->
	job_util:stopWorkGroup(Name),
	supervisor:terminate_child(?MODULE, Name),
	supervisor:delete_child(?MODULE, Name).
