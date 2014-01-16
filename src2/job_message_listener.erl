-module(job_message_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {poolName, channel}).

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
	gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
% [{channel, Channel},{listen, {queue, <<"ConsoleIt">>}},{poolName, sample}]
init(Args) ->
	process_flag(trap_exit, true),
	PoolName    = proplists:get_value(poolName, Args),
	Connection  = proplists:get_value(connection, Args),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
	job_util:setWorker(PoolName, self()),
	QueueName = proplists:get_value(queue, Args),
	ExchangeName = proplists:get_value(exchange, Args),
	Keys = proplists:get_value(keys, Args),
	ok = connectAMQP(Channel, ExchangeName, Keys, QueueName),
	{ok, #state{poolName = PoolName, channel = Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({#'basic.deliver'{routing_key = RoutingKey}, #amqp_msg{payload = Body}}, #state{poolName = PoolName} = State) ->
	Worker = poolboy:checkout(PoolName, true, infinity),
	ToPort = jiffy:encode({[{<<"key">>, RoutingKey},{<<"message">>, jiffy:decode(Body)}]}),
	job_worker:message(Worker, ToPort),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel}) ->
	ok = amqp_channel:close(Channel),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

connectAMQP(Channel, ExchangeName, Keys, QueueName) ->
	ok = declareExchange(Channel, ExchangeName),
	{ok, Queue} = declareQueue(Channel, QueueName),
	ok = bindKeys(Channel, ExchangeName, Queue, Keys),
	amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end.

declareExchange(_Channel, undefined) -> ok;
declareExchange(Channel, Exchange) ->
	amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange, type = <<"topic">>}),
	ok.

declareQueue(Channel, undefined) ->
	#'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
	{ok, Queue};

declareQueue(Channel, QueueName) when is_atom(QueueName) -> declareQueue(Channel, atom_to_binary(QueueName, latin1));
declareQueue(Channel, QueueName) ->
	#'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = QueueName}),
	{ok, Queue}.

bindKeys(_Channel, undefined, _Queue, _Keys) -> ok;
bindKeys(_Channel, _ExchangeName, _Queue, []) -> ok;
bindKeys(_Channel, _ExchangeName, _Queue, undefined) -> ok;
bindKeys(Channel, ExchangeName, Queue, Keys) ->
	[amqp_channel:call(Channel,
			#'queue.bind'{exchange    = ExchangeName,
				      routing_key = BindingKey,
				      queue       = Queue})
	|| BindingKey <- Keys],
	ok.
