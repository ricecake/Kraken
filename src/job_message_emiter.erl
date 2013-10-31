-module(job_message_emiter).
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
	job_util:setEmiter(PoolName, self()),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	{ok, #state{poolName = PoolName, channel = Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Pid, {reply, Data}}, #state{poolName = PoolName} = State) ->
	poolboy:checkin(PoolName, Pid),
	handleSend(Pid, Data, State);

handle_info({Pid, {send, Data}}, State) -> handleSend(Pid, Data, State);

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


handleSend(_Pid, Data, #state{channel = Channel} = State) ->
	Reply = jiffy:decode(Data),
	ok = doReply(Channel, Reply),
	{noreply, State}.


doReply(Channel, {Keys}) ->
	case proplists:get_value(<<"action">>, Keys) of
		{[{<<"reply">>,  {ReplyFields}}]} -> send(Channel, ReplyFields);
		{[{<<"reply">>,  Replies}]} when is_list(Replies) -> [send(Channel, ReplyFields) || {ReplyFields} <- Replies], ok;
		{[{<<"noReply">>, _ }]} -> ok
	end.

send(Channel, ReplyFields) ->
	Key  = proplists:get_value(<<"key">>,  ReplyFields),
	Body = proplists:get_value(<<"body">>, ReplyFields),
	Exch = proplists:get_value(<<"exchange">>, ReplyFields),
	amqp_channel:cast(Channel,
			  #'basic.publish'{
			  exchange = Exch,
			  routing_key = Key},
			  #amqp_msg{payload = jiffy:encode(Body)}),
	ok.
