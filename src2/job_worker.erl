-module(job_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, message/2, message/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port, workgroup, state=auto}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

message(Pid, Message) -> gen_server:cast(Pid, {self(), {message, Message}}).
message(Pid, Message, Client) -> gen_server:cast(Pid, {Client, {message, Message}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	process_flag(trap_exit, true),
	WorkGroup = proplists:get_value(workgroup, Args),
	Program = proplists:get_value(program, Args),
	ProgArgs = proplists:get_value(args, Args),
	Port = open_port({spawn_executable, Program}, [stream, {line, 65536}, {args, ProgArgs}]),
	{ok, #state{port = Port, workgroup = WorkGroup}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({_Client, {message, Message}}, #state{port=Port} = State) ->
	Port ! {self(), {command, io_lib:format("~s~n",[Message])}},
	{noreply, State#state{state = client} };

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({_Port, {data, {eol, Data}}}, #state{workgroup = WorkGroup, state=client} = State) ->
	Client = job_util:getEmiter(WorkGroup),
	Client ! {self(), {reply, Data}},
	{noreply, State#state{state=auto}};

handle_info({_Port, {data, {eol, Data}}}, #state{workgroup = WorkGroup, state=auto} = State) ->
	Client = job_util:getEmiter(WorkGroup),
	Client ! {self(), {send, Data}},
	{noreply, State};

handle_info({'EXIT', Port, _Reason}, #state{port=Port} = State) -> {stop, portExit, State};
handle_info({'EXIT', _Pid, Reason}, State) -> {stop, Reason, State};

handle_info(_Reason, State) -> {noreply, State}.

terminate(portExit, _State) -> ok;

terminate(_Reason, #state{port = Port}) ->
	true = port_close(Port),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
