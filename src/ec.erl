-module(ec).

-export([start/0, start/2, stop/0]).

%% @doc Start the application
-spec start() -> {ok, [atom()]}.
start() ->
    {ok, _} = application:ensure_all_started(ec).

%% @doc Start the application
-spec start(atom(), term()) -> {ok, [atom()]}.
start(_StartType, _StartArgs) ->
    ec_sup:start_link().

%% @doc Stop the application
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(ec).
