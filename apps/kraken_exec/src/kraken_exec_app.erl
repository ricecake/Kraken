-module(kraken_exec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    kraken_exec_sup:start_link().

stop(_State) ->
    ok.
