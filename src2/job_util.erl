
-module(job_util).

-include_lib("stdlib/include/qlc.hrl").

-record(workgroup,          {name, setUpArgs, supervisor, worker, emiter}).
-record(executeDirs,        {filename}).
-record(commandWhitelist,   {command, args}).

-export([startWorkGroup/1, stopWorkGroup/1, setSupervisor/2,
	 setWorker/2, setEmiter/2, getSupervisor/1,
	 getWorker/1, getEmiter/1, initWorkgroupTable/0,
	 getArgs/1, setArgs/2, getAllStartArgs/0]).

-export([initExecuteDirs/0, addSafeFile/1]).

-export([initCommandWhitelist/0, addWhiteListCommand/2]).

initWorkgroupTable() ->
	ets:new(workgroups, [set, public, named_table, {keypos, #workgroup.name}]),
	ok.

startWorkGroup(Name) ->
	case ets:lookup(workgroups, Name) of
		[] -> ets:insert(workgroups, #workgroup{name=Name});
		_  -> ok
	end,
	ok.

stopWorkGroup(Name)  ->
	ets:delete(workgroups, Name),
	ok.

setSupervisor(Name, Pid) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> ets:insert(workgroups, WorkGroup#workgroup{supervisor = Pid})
	end,
	ok.

setArgs(Name, Args) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> ets:insert(workgroups, WorkGroup#workgroup{setUpArgs = Args})
	end,
	ok.

setWorker(Name, Pid) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> ets:insert(workgroups, WorkGroup#workgroup{worker = Pid})
	end,
	ok.

setEmiter(Name, Pid) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> ets:insert(workgroups, WorkGroup#workgroup{emiter = Pid})
	end,
	ok.

getSupervisor(Name) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> WorkGroup#workgroup.supervisor
	end.

getWorker(Name) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> WorkGroup#workgroup.worker
	end.

getEmiter(Name) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> WorkGroup#workgroup.emiter
	end.

getArgs(Name) ->
	case ets:lookup(workgroups, Name) of
		[WorkGroup] when is_record(WorkGroup, workgroup) -> WorkGroup#workgroup.setUpArgs
	end.

getAllStartArgs() -> qlc:eval(qlc:q([Row#workgroup.setUpArgs || Row <- ets:table(workgroups)])).

initExecuteDirs() ->
	ets:new(executeDirs, [set, public, named_table, {keypos, #executeDirs.filename}]),
	ok.

addSafeFile(File) ->
	ets:insert(executeDirs, #executeDirs{filename = File}),
	ok.

initCommandWhitelist() ->
	ets:new(commandWhitelist, [duplicate_bag, public, named_table, {keypos, #commandWhitelist.command}]),
	ok.

addWhiteListCommand(Command, Args) ->
	ets:insert(commandWhitelist, #commandWhitelist{command = Command, args = Args}),
	ok.
