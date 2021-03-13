%% Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%% coding: latin-1
-module(jsone_encode_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('NO_MAP_TYPE').
-define(OBJ0, {[]}).
-define(OBJ1(K, V), {[{K, V}]}).
-define(OBJ2(K1, V1, K2, V2), {[{K1, V1}, {K2, V2}]}).
-define(OBJECT_FROM_LIST(List), List).
-else.
-define(OBJ0, #{}).
-define(OBJ1(K, V), #{K => V}).
-define(OBJ2(K1, V1, K2, V2), #{K1 => V1, K2 => V2}).
-define(OBJECT_FROM_LIST(List), maps:from_list(List)).
-endif.

encode_test_() ->
    [
     %% Symbols
     {"false",
      fun () ->
              ?assertEqual({ok, <<"false">>}, jsone_encode:encode(false))
      end},
     {"true",
      fun () ->
              ?assertEqual({ok, <<"true">>}, jsone_encode:encode(true))
      end},
     {"null",
      fun () ->
              ?assertEqual({ok, <<"null">>}, jsone_encode:encode(null))
      end},

     %% Numbers: Inline json term
     {"json",
      fun () ->
              ?assertEqual(
                 {ok, <<"{\"foo\":[1,2,3],\"bar\":\"",195,169,"ok\"}">>},
                 jsone_encode:encode(
                   ?OBJ2(foo, {{json, ["["|[$1, ",2",<<",3]">>]]}},
                         <<"bar">>, {{json_utf8, [$", 233, "ok", $"]}}))),
              ?assertEqual(
                 {ok, <<"{\"foo\":[1,2,3],\"bar\":\"",233,"ok\"}">>},
                 jsone_encode:encode(
                   ?OBJ2(foo, {{json, ["["|[$1, ",2",<<",3]">>]]}},
                         <<"bar">>, {{json, [$", 233, "ok", $"]}}))),
              ?assertEqual(
                 {ok, <<"{\"json\":\"[1,2,3]\"}">>},
                 jsone_encode:encode([{json, <<"[1,2,3]">>}])),
              ?assertEqual(
                 {ok, <<"[[1,2,3]]">>},
                 jsone_encode:encode([{{json, <<"[1,2,3]">>}}]))
      end},
     %% Numbers: Integer
     {"zero",
      fun () ->
              ?assertEqual({ok, <<"0">>}, jsone_encode:encode(0))
      end},
     {"positive integer",
      fun () ->
              ?assertEqual({ok, <<"1">>}, jsone_encode:encode(1))
      end},
     {"negative integer",
      fun () ->
              ?assertEqual({ok, <<"-1">>}, jsone_encode:encode(-1))
      end},
     {"large number",
      fun () ->
              ?assertEqual({ok, <<"11111111111111111111111111111111111111111111111111111111111111111111111">>},
                           jsone_encode:encode(11111111111111111111111111111111111111111111111111111111111111111111111))
      end},

     %% Numbers: Float",
     {"float",
      fun () ->
              Input = 1.234,
              ?assertMatch({ok, _}, jsone_encode:encode(Input)),
              ?assertEqual(Input, binary_to_float(element(2, jsone_encode:encode(Input))))
      end},
     {"float_format option",
      fun () ->
              Input = 1.23,
              ?assertEqual({ok, <<"1.22999999999999998224e+00">>}, jsone_encode:encode(Input)),
              ?assertEqual({ok, <<"1.2300e+00">>},                 jsone_encode:encode(Input, [{float_format, [{scientific, 4}]}])),
              ?assertEqual({ok, <<"1.2e+00">>},                    jsone_encode:encode(Input, [{float_format, [{scientific, 1}]}])),
              ?assertEqual({ok, <<"1.2300">>},                     jsone_encode:encode(Input, [{float_format, [{decimals, 4}]}])),
              ?assertEqual({ok, <<"1.23">>},                       jsone_encode:encode(Input, [{float_format, [{decimals, 4}, compact]}]))
      end},

     %% Strings
     {"simple string",
      fun () ->
              ?assertEqual({ok, <<"\"abc\"">>}, jsone_encode:encode(<<"abc">>))
      end},
     {"atom is regarded as string",
      fun () ->
              ?assertEqual({ok, <<"\"abc\"">>}, jsone_encode:encode(abc))
      end},
     {"string: contains escaped characters",
      fun () ->
              Input    = <<"\"\/\\\b\f\n\r\t">>,
              Expected = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input)),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input, [native_utf8]))
      end},
     {"string: contains forward slashes",
      fun () ->
              Input = <<"1/2">>,
              ?assertEqual({ok, <<"\"1\\/2\"">>}, jsone_encode:encode(Input)),
              ?assertEqual({ok, <<"\"1/2\"">>}, jsone_encode:encode(Input, [native_forward_slash]))
      end},
     {"string: contains control characters",
      fun () ->
              Ctrls    = lists:seq(16#00, 16#1F) -- [$\b, $\f, $\n, $\r, $\t],
              Input    = list_to_binary(Ctrls),
              Expected = list_to_binary([$", [io_lib:format("\\u00~2.16.0b", [C]) || C <- Ctrls], $"]),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input)),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input, [native_utf8]))
      end},
     {"string: contains multi-byte (UTF-8 encoded) characters",
      fun () ->
              %% japanese
              Input1    = <<"あいうえお">>,  % assumed that the encoding of this file is UTF-8
              Expected1 = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304a\"">>,
              ?assertEqual({ok, Expected1}, jsone_encode:encode(Input1)),
              Expected12 = <<$", Input1/binary, $">>,
              ?assertEqual({ok, Expected12}, jsone_encode:encode(Input1, [native_utf8])),

              %% other multi-byte characters
              Input2    = <<"۝۞ႮႯ">>,
              Expected2 = <<"\"\\u06dd\\u06de\\u10ae\\u10af\"">>,
              ?assertEqual({ok, Expected2}, jsone_encode:encode(Input2)),
              Expected22 = <<$", Input2/binary, $">>,
              ?assertEqual({ok, Expected22}, jsone_encode:encode(Input2, [native_utf8]))
      end},
     {"string: containts surrogate pairs",
      fun () ->
              Input    = <<"𢁉𢂚𢃼">>,
              Expected = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},

     %% Strings variant: Datetimes
     {"datetime: iso8601: utc",
      fun () ->
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25Z\"">>}, jsone_encode:encode({{2015,6,25},{14,57,25}})),
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25Z\"">>}, jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, iso8601}])),
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25Z\"">>}, jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, {iso8601, utc}}]))
      end},
     {"datetime: iso8601: local",
      fun () ->
              {ok, Json} = jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, {iso8601, local}}]),

              UTC = {{1970, 1, 2}, {0,0,0}},
              Local = calendar:universal_time_to_local_time({{1970, 1, 2}, {0,0,0}}),
              case UTC =:= Local of
                  false -> ?assertMatch(<<"\"2015-06-25T14:57:25",_:6/binary,"\"">>, Json);
                  true  -> ?assertMatch(<<"\"2015-06-25T14:57:25Z\"">>, Json)
              end
      end},
     {"datetime: iso8601: timezone",
      fun () ->
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25Z\"">>},      jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, {iso8601, 0}}])),
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25+00:01\"">>}, jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, {iso8601, 60}}])),
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25-00:01\"">>}, jsone_encode:encode({{2015,6,25},{14,57,25}}, [{datetime_format, {iso8601, -60}}]))
      end},
     {"datetime as head of array",
       ?_assertEqual({ok, <<"[\"2015-06-25T14:57:25Z\"]">>},
                      jsone_encode:encode([{{2015,6,25},{14,57,25}}]))},

     {"datetime: iso8601: with fractions of seconds",
      fun () ->
              ?assertEqual({ok, <<"\"2015-06-25T14:57:25.325Z\"">>},
                           jsone_encode:encode({{2015,6,25},{14,57,25.3245}})),
              ?assertEqual({ok, <<"\"2015-06-25T14:57:05.320Z\"">>},
                           jsone_encode:encode({{2015,6,25},{14,57,5.32}}))
      end},

     %% Arrays
     {"simple array",
      fun () ->
              Input    = [1, 2, 3],
              Expected = <<"[1,2,3]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"empty array",
      fun () ->
              Input    = [],
              Expected = <<"[]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},

     %% Objects
     {"simple object",
      fun () ->
              Input1   = {[{<<"key">>, <<"value">>}, {<<"1">>, 2}]},
              Input2   = [{<<"key">>, <<"value">>}, {<<"1">>, 2}],
              Expected = <<"{\"key\":\"value\",\"1\":2}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input1)),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input2))
      end},
     {"empty object",
      fun () ->
              Input1   = {[]},
              Input2   = [{}],
              Expected = <<"{}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input1)),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input2))
      end},
     {"simple object: map",
      fun () ->
              Input = ?OBJ2(<<"1">>, 2, <<"key">>, <<"value">>),
              Expected = <<"{\"1\":2,\"key\":\"value\"}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"empty object: map",
      fun () ->
              Input = ?OBJ0,
              Expected = <<"{}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"atom key is allowed",
      fun () ->
              Expected = <<"{\"key\":2}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode({[{key, 2}]}))
      end},
     {"object_key_type option",
      fun () ->
              %% key: atom
              ?assertEqual({ok, <<"{\"a\":2}">>}, jsone_encode:encode(?OBJ1(a, 2), [{object_key_type, string}])), % OK
              ?assertEqual({ok, <<"{\"a\":2}">>}, jsone_encode:encode(?OBJ1(a, 2), [{object_key_type, scalar}])), % OK
              ?assertEqual({ok, <<"{\"a\":2}">>}, jsone_encode:encode(?OBJ1(a, 2), [{object_key_type, value}])),  % OK

              %% key: number
              ?assertMatch({error, {badarg, _}},  jsone_encode:encode(?OBJ1(1, 2), [{object_key_type, string}])), % NG
              ?assertEqual({ok, <<"{\"1\":2}">>}, jsone_encode:encode(?OBJ1(1, 2), [{object_key_type, scalar}])), % OK
              ?assertEqual({ok, <<"{\"1\":2}">>}, jsone_encode:encode(?OBJ1(1, 2), [{object_key_type, value}])),  % OK

              %% key: datetime
              ?assertMatch({error, {badarg, _}},  jsone_encode:encode(?OBJ1({{2000,1,1}, {0,0,0}}, 2), [{object_key_type, string}])), % NG
              ?assertEqual({ok, <<"{\"2000-01-01T00:00:00Z\":2}">>}, jsone_encode:encode(?OBJ1({{2000,1,1}, {0,0,0}}, 2), [{object_key_type, scalar}])), % OK
              ?assertEqual({ok, <<"{\"2000-01-01T00:00:00Z\":2}">>}, jsone_encode:encode(?OBJ1({{2000,1,1}, {0,0,0}}, 2), [{object_key_type, value}])),  % OK

              %% key: array
              ?assertMatch({error, {badarg, _}},    jsone_encode:encode(?OBJ1([1], 2), [{object_key_type, string}])), % NG
              ?assertMatch({error, {badarg, _}},    jsone_encode:encode(?OBJ1([1], 2), [{object_key_type, scalar}])), % NG
              ?assertEqual({ok, <<"{\"[1]\":2}">>}, jsone_encode:encode(?OBJ1([1], 2), [{object_key_type, value}])),  % OK

              %% key: object
              ?assertMatch({error, {badarg, _}},   jsone_encode:encode(?OBJ1(?OBJ0, 2), [{object_key_type, string}])), % NG
              ?assertMatch({error, {badarg, _}},   jsone_encode:encode(?OBJ1(?OBJ0, 2), [{object_key_type, scalar}])), % NG
              ?assertEqual({ok, <<"{\"{}\":2}">>}, jsone_encode:encode(?OBJ1(?OBJ0, 2), [{object_key_type, value}]))    % OK
      end},
     {"non binary object member key is disallowed",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{1, 2}]})),
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{"1", 2}]}))
      end},
     {"undefined_as_null option",
      fun() ->
              ?assertEqual({ok,<<"null">>},          jsone_encode:encode(undefined,[undefined_as_null])), % OK
              ?assertEqual({ok,<<"\"undefined\"">>}, jsone_encode:encode(undefined,[])) % OK
      end},
     {"skip_undefined option",
      fun() ->
              Object = #{<<"1">> => undefined, <<"2">> => 3, <<"3">> => undefined},
              ?assertEqual({ok,<<"{\"1\":null,\"2\":3,\"3\":null}">>}, jsone_encode:encode(Object,[undefined_as_null])),
              ?assertEqual({ok,<<"{\"2\":3}">>},                       jsone_encode:encode(Object,[skip_undefined]))
      end},

     %% Pretty Print
     {"space",
      fun () ->
              ?assertEqual({ok, <<"[]">>}, jsone_encode:encode([], [{space, 1}])),
              ?assertEqual({ok, <<"[1, 2, 3]">>}, jsone_encode:encode([1,2,3], [{space, 1}])),
              ?assertEqual({ok, <<"[1,  2,  3]">>}, jsone_encode:encode([1,2,3], [{space, 2}])),
              ?assertEqual({ok, <<"{}">>}, jsone_encode:encode(?OBJ0, [{space, 1}])),
              ?assertEqual({ok, <<"{\"a\": 1, \"b\": 2}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{space, 1}])),
              ?assertEqual({ok, <<"{\"a\":  1,  \"b\":  2}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{space, 2}]))
      end},
     {"indent",
      fun () ->
              ?assertEqual({ok, <<"[]">>}, jsone_encode:encode([], [{indent, 1}])),
              ?assertEqual({ok, <<"[\n 1,\n 2,\n 3\n]">>}, jsone_encode:encode([1,2,3], [{indent, 1}])),
              ?assertEqual({ok, <<"[\n  1,\n  2,\n  3\n]">>}, jsone_encode:encode([1,2,3], [{indent, 2}])),
              ?assertEqual({ok, <<"{}">>}, jsone_encode:encode(?OBJ0, [{indent, 1}])),
              ?assertEqual({ok, <<"{\n \"a\":1,\n \"b\":2\n}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{indent, 1}])),
              ?assertEqual({ok, <<"{\n  \"a\":1,\n  \"b\":2\n}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{indent, 2}]))
      end},
     {"indent+space",
      fun () ->
              ?assertEqual({ok, <<"[]">>}, jsone_encode:encode([], [{indent, 1}, {space, 1}])),
              ?assertEqual({ok, <<"[\n 1,\n 2,\n 3\n]">>}, jsone_encode:encode([1,2,3], [{indent, 1}, {space, 1}])),
              ?assertEqual({ok, <<"[\n  1,\n  2,\n  3\n]">>}, jsone_encode:encode([1,2,3], [{indent, 2}, {space, 2}])),
              ?assertEqual({ok, <<"{}">>}, jsone_encode:encode(?OBJ0, [{indent, 1}, {space, 1}])),
              ?assertEqual({ok, <<"{\n \"a\": 1,\n \"b\": 2\n}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{indent, 1}, {space, 1}])),
              ?assertEqual({ok, <<"{\n  \"a\":  1,\n  \"b\":  2\n}">>}, jsone_encode:encode(?OBJ2(a, 1, b, 2), [{indent, 2}, {space, 2}]))
      end},

     %% `map_unknown_value` option
     {"`map_unknown_value` option",
      fun () ->
              Input = [{1,2,3,4}],
              MapFun = fun ({_,_,_,_} = Ip4) -> {ok, list_to_binary(inet:ntoa(Ip4))};
                           (_)               -> error
                       end,
              Expected = <<"[\"1.2.3.4\"]">>,
              ?assertEqual(Expected, jsone:encode(Input, [{map_unknown_value, MapFun}]))
      end},
     {"`map_unknown_value` option with singleton tuple",
      fun () ->
              Input = [{foo}],
              MapFun = fun (Value) -> {ok, unicode:characters_to_binary(io_lib:format("~p~n", [Value]))}
                       end,
              Expected = <<"[\"{foo}\\n\"]">>,
              ?assertEqual(Expected, jsone:encode(Input, [{map_unknown_value, MapFun}]))
      end},

     %% Others
     {"compound data",
      fun () ->
              Input    = [true, {[{<<"1">>, 2}, {<<"array">>, [[[[1]]], {[{<<"ab">>, <<"cd">>}]}, [], ?OBJ0, false]}]}, null],
              Expected = <<"[true,{\"1\":2,\"array\":[[[[1]]],{\"ab\":\"cd\"},[],{},false]},null]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input)),

              PpExpected = <<"[\n true,\n {\n  \"1\": 2,\n  \"array\": [\n   [\n    [\n     [\n      1\n     ]\n    ]\n   ],\n   {\n    \"ab\": \"cd\"\n   },\n   [],\n   {},\n   false\n  ]\n },\n null\n]">>,
              ?assertEqual({ok, PpExpected}, jsone_encode:encode(Input, [{indent, 1}, {space, 1}]))
      end},
     {"invalid value",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode(self()))
      end},
     {"wrong option",
      fun () ->
              ?assertError(badarg, jsone_encode:encode(1, [{no_such_option, hoge}]))
      end},
     {"canonical_form",
      fun () ->
          Obj1 = ?OBJECT_FROM_LIST( [{<<"key", (integer_to_binary(I))/binary >>, I} || I <- lists:seq(1000, 0, -1)] ),
          Obj2 = ?OBJECT_FROM_LIST( [{<<"key", (integer_to_binary(I))/binary >>, I} || I <- lists:seq(0, 1000, 1)] ),
          ?assertEqual(jsone_encode:encode(Obj1, [canonical_form]), jsone_encode:encode(Obj2, [canonical_form]))
      end}
    ].
