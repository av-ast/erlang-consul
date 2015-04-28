-module(ec_request).

-export([get/2, put/3, delete/2]).

%%
%% API functions
%%

get(RelativeUrl, QueryParams) ->
    FullUrl = full_url(RelativeUrl, QueryParams),
    lhttpc:request(FullUrl, "GET", [], request_timeout()).

put(RelativeUrl, Body, QueryParams) ->
    FullUrl = full_url(RelativeUrl, QueryParams),
    lhttpc:request(FullUrl, "PUT", [], Body, request_timeout()).

delete(RelativeUrl, QueryParams) ->
    FullUrl = full_url(RelativeUrl, QueryParams),
    lhttpc:request(FullUrl, "DELETE", [], request_timeout()).

%%
%% Internal functions
%%

full_url(RelativeUrl, QueryParams) ->
    base_url() ++ RelativeUrl ++ ec_utils:to_query_string(QueryParams).

base_url() ->
  {ok, Host} = application:get_env(ec, host),
  {ok, Port} = application:get_env(ec, port),
  {ok, ProtoVersion} = application:get_env(ec, protocol_version),
  lists:flatten(io_lib:format("http://~s:~p/~s", [Host, Port, ProtoVersion])).

request_timeout() ->
  {ok, Timeout} = application:get_env(ec, request_timeout),
  Timeout.
