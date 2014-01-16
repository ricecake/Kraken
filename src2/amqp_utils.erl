-module(amqp_utils).

-export([amqpSetup/1, connectAMQP/4, declareExchange/2, declareQueue/2, bindKeys/4, subscribe/2]).

-include_lib("amqp_client/include/amqp_client.hrl").
amqpSetup(AmqpParams) ->
	User    = proplists:get_value(username, AmqpParams),
	Pass    = proplists:get_value(password, AmqpParams),
	Host    = proplists:get_value(host, AmqpParams),
	Port    = proplists:get_value(port, AmqpParams),
	Vhost   = proplists:get_value(vhost, AmqpParams),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host, port = Port, username = User, password = Pass, virtual_host = Vhost}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	{amqpData, Connection, Channel}.


connectAMQP(Channel, ExchangeName, Keys, QueueName) ->
	ok = declareExchange(Channel, ExchangeName),
	{ok, Queue} = declareQueue(Channel, QueueName),
	ok = bindKeys(Channel, ExchangeName, Queue, Keys).

declareExchange(_Channel, undefined) -> ok;
declareExchange(_Channel, []) -> ok;
declareExchange(Channel, [Exchange |Rest]) ->
	declareExchange(Channel, Exchange),
	declareExchange(Channel, Rest);
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

subscribe(Channel, Queue) ->
	amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end.
