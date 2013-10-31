-module(kraken_controller).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(controller_state, {connection, channel}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	{amqpData, Connection, Channel} = amqp_utils:amqpSetup(Args),
	{ok, Hostname} = inet:gethostname(),
	{ok, {hostent, Fqdn, _, _, _, _}} = inet:gethostbyname(Hostname),
        DomainParts = string:tokens(Fqdn, "."),
	RoutingKeys = [list_to_binary(string:join(lists:reverse(DomainKey),"."))
                || DomainKey <- [lists:nthtail(N, DomainParts)
                        || N<- lists:seq(0, erlang:length(DomainParts)-1)]],
	Exchanges = [<<"krakenControlBus">>, <<"krakenInfoBus">>],
	ok = amqp_utils:declareExchange(Channel, Exchanges),
	{ok, Queue} = amqp_utils:declareQueue(Channel, undefined),
	ok = amqp_utils:bindKeys(Channel, <<"krakenControlBus">>, Queue, RoutingKeys),
	ok = amqp_utils:subscribe(Channel, Queue),
	process_flag(trap_exit, true),
	self() ! {initialize, tasker},
	{ok, #controller_state{connection = Connection, channel = Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
