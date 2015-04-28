-module(ec_kv).

-export([get/1, get/2, set/2, set/3, delete/1, delete/2]).

%%
%% API
%%

%% @reference See <a href="https://www.consul.io/docs/agent/http/kv.html">https://www.consul.io/docs/agent/http/kv.html</a>
%% @doc By default, the datacenter of the agent is queried; however, the dc can be provided using the "?dc=" query parameter.

%% @doc
%% Gets value by specified key
%%
%% @spec get(Key::string()) -> {ok, term()} | {error, bad_request, term()} | {error, key_not_found} | {error, request_failed}
get(Key) ->
    get(Key, []).

%% @doc
%% Gets value by specified key with query string params
%%
%% @spec get(Key::string(), QueryParams::list()) -> {ok, term()} | {error, bad_request, term()} | {error, key_not_found} | {error, request_failed}
get(Key, QueryParams) ->
    Response = ec_request:get("/kv/" ++ Key, QueryParams),
    io:format("~n~p~n", [Response]),
    case Response of
        {ok, {{200, _}, _Headers, Body}} ->
            {ok, build_get_response(Body)};
        {ok, {{400, _}, _Headers, Body}} ->
            {error, bad_request, Body};
        {ok, {{404, _}, _Headers, _Body}} ->
            {error, key_not_found};
        _ ->
            error_logger:info_msg("~p~n", [Response]),
            {error, request_failed}
    end.

%% @doc
%% Sets value for key
%%
%% @spec set(Key::string(), Value::string()) -> {ok, term()} | {error, bad_request, term()} | {error, request_failed}
set(Key, Value) ->
    set(Key, Value, []).

%% @doc
%% Sets value for key with query string params
%%
%% @spec set(Key::string(), Value::string(), QueryParams::list()) -> {ok, term()} | {error, bad_request, term()} | {error, request_failed}
set(Key, Value, QueryParams) ->
    Response = ec_request:put("/kv/" ++ Key, ec_utils:to_list(Value), QueryParams),
    case Response of
        {ok, {{200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{400, _}, _Headers, Body}} ->
            {error, bad_request, Body};
        _ ->
            error_logger:info_msg("~p~n", [Response]),
            {error, request_failed}
    end.

%% @doc
%% Deletes value by specified key
%%
%% @spec delete(Key::string()) -> {ok, term()} | {error, bad_request, term()} | {error, request_failed}
delete(Key) ->
    delete(Key, []).

%% @doc
%% Deletes value by specified key with query string params
%%
%% @spec delete(Key::string(), QueryParams::list()) -> {ok, term()} | {error, bad_request, term()} | {error, request_failed}
delete(Key, QueryParams) ->
    Response = ec_request:delete("/kv/" ++ Key, QueryParams),
    case Response of
        {ok, {{200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{400, _}, _Headers, Body}} ->
            {error, bad_request, Body};
        _ ->
            error_logger:info_msg("~p~n", [Response]),
            {error, request_failed}
    end.

%%
%% Internal functions
%%

build_get_response(Payload) ->
    case jiffy:decode(Payload) of
        [{DecodedPayload}] ->
            Get = fun proplists:get_value/2,
            [
              {key, binary_to_list(Get(<<"Key">>, DecodedPayload))},
              {value, base64:decode_to_string(binary_to_list(Get(<<"Value">>, DecodedPayload)))},
              {flags, Get(<<"Flags">>, DecodedPayload)},
              {lock_index, Get(<<"LockIndex">>, DecodedPayload)},
              {create_index, Get(<<"CreateIndex">>, DecodedPayload)},
              {modify_index, Get(<<"ModifyIndex">>, DecodedPayload)}
            ];
      RawValue ->
          RawValue
    end.
