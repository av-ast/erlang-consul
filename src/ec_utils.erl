-module(ec_utils).

-export([to_list/1, to_query_string/1]).

to_query_string(Params) when is_list(Params) ->
    KVs = lists:map(fun({K,V}) ->
        lists:concat([K, '=', http_uri:encode(to_list(V))])
    end, Params),
    "?" ++ string:join(KVs, "&");

to_query_string(_Params) -> "?".

to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_tuple(V) -> tuple_to_list(V);
to_list(V) -> V.
%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_query_string_test() ->
    ?assertEqual("?", to_query_string(not_a_list)),
    ?assertEqual("?a=b", to_query_string([{a,"b"}])),
    ?assertEqual("?a=b&c=d", to_query_string([{a,"b"},{c,"d"}])),
    ?assertEqual("?a=b&c=5", to_query_string([{a,b},{c,5}])),
    ?assertEqual("?a=b%20c", to_query_string([{a,"b c"}])),
    ?assertEqual("?a=%20%2B%40%3D", to_query_string([{a," +@="}])).

to_list_test() ->
    "foo" = to_list(foo),
    "123" = to_list(123),
    "foo" = to_list(foo),
    "foo" = to_list(<<"foo">>),
    1.123 = to_list(1.123).

-endif.
