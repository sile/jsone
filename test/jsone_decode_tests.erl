%% Copyright (c) 2013-2015, Takeru Ohta <phjgt308@gmail.com>
%% coding: latin-1
-module(jsone_decode_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('NO_MAP_TYPE').
-define(MAP_OBJECT_TYPE, tuple).
-define(OBJ0, {[]}).
-define(OBJ1(K, V), {[{K, V}]}).
-define(OBJ2(K1, V1, K2, V2), {[{K1, V1}, {K2, V2}]}).
-define(OBJ2_DUP_KEY(K1, V1, K2, V2), ?OBJ2(K1, V1, K2, V2)).
-else.
-define(MAP_OBJECT_TYPE, map).
-define(OBJ0, #{}).
-define(OBJ1(K, V), #{K => V}).
-define(OBJ2(K1, V1, K2, V2), #{K1 => V1, K2 => V2}).
-define(OBJ2_DUP_KEY(K1, V1, _K2, _V2), #{K1 => V1}).  % the first (leftmost) value is used
-define(OBJ2_DUP_KEY_LAST(_K1, _V1, K2, V2), #{K2 => V2}).  % the last value is used
-endif.

decode_test_() ->
    [
     %% Symbols
     {"false", fun () -> ?assertEqual({ok, false, <<"">>}, jsone_decode:decode(<<"false">>)) end},
     {"true", fun () -> ?assertEqual({ok, true, <<"">>}, jsone_decode:decode(<<"true">>)) end},
     {"null", fun () -> ?assertEqual({ok, null, <<"">>}, jsone_decode:decode(<<"null">>)) end},

     %% Numbers: Integer
     {"positive integer", fun () -> ?assertEqual({ok, 1, <<"">>}, jsone_decode:decode(<<"1">>)) end},
     {"zero", fun () -> ?assertEqual({ok, 0, <<"">>}, jsone_decode:decode(<<"0">>)) end},
     {"negative integer", fun () -> ?assertEqual({ok, -1, <<"">>}, jsone_decode:decode(<<"-1">>)) end},
     {"large integer (no limit on size)",
      fun () ->
              ?assertEqual({ok, 111111111111111111111111111111111111111111111111111111111111111111111111111111, <<"">>},
                           jsone_decode:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>))
      end},
     {"integer with leading zero (interpreted as zero and remaining binary)",
      fun () ->
              ?assertEqual({ok, 0, <<"0">>}, jsone_decode:decode(<<"00">>)),
              ?assertEqual({ok, 0, <<"1">>}, jsone_decode:decode(<<"01">>)),
              ?assertEqual({ok, 0, <<"1">>}, jsone_decode:decode(<<"-01">>))
      end},
     {"integer can't begin with an explicit plus sign",
      fun () -> ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"+1">>)) end},

     %% Numbers: Floats
     {"float: decimal notation", fun () -> ?assertEqual({ok, 1.23, <<"">>}, jsone_decode:decode(<<"1.23">>)) end},
     {"float: exponential notation",
      fun () ->
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345e-3">>)),  % lower case 'e'
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345E-3">>)),  % upper case 'E'
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345.0e-3">>)),
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345E2">>)),
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345e+2">>)),  % exponent part can begin with plus sign
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345E+2">>)),
              ?assertEqual({ok, -12.345, <<"">>}, jsone_decode:decode(<<"-0.012345e3">>)),
              ?assertEqual({ok, 123.0, <<"">>}, jsone_decode:decode(<<"1.23000000000000000000e+02">>))
      end},
     {"float: invalid format",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<".123">>)),  % omitted integer part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.">>)),  % omitted fraction part: EOS
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.e+3">>)),  % omitted fraction part: with exponent part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e">>)),  % incomplete fraction part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e-">>)),  % incomplete fraction part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1ee-1">>)),  % duplicated 'e'
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e--1">>)),  % duplicated sign
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"1e999">>)),  % exponent out of range
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e999">>)),  % exponent out of range
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"100000000000000000000000000000000000e300">>)),  % product out of range
              ?assertEqual({ok, 0.1, <<".2">>}, jsone_decode:decode(<<"0.1.2">>))  % duplicated '.': interpreted as individual tokens
      end},

     %% Strings
     {"simple string", fun () -> ?assertEqual({ok, <<"abc">>, <<"">>}, jsone_decode:decode(<<"\"abc\"">>)) end},
     {"string: escaped characters",
      fun () ->
              Input = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              Expected = <<"\"\/\\\b\f\n\r\t">>,
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"string: escaped Unicode characters",
      fun () ->
              %% japanese
              Input1 = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
              Expected1 = <<"あいうえお">>,  % assumed that the encoding of this file is UTF-8
              ?assertEqual({ok, Expected1, <<"">>}, jsone_decode:decode(Input1)),

              %% ascii
              Input2 = <<"\"\\u0061\\u0062\\u0063\"">>,
              Expected2 = <<"abc">>,
              ?assertEqual({ok, Expected2, <<"">>}, jsone_decode:decode(Input2)),

              %% other multi-byte characters
              Input3 = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
              Expected3 = <<"۝۞ႮႯ">>,
              ?assertEqual({ok, Expected3, <<"">>}, jsone_decode:decode(Input3)),

              %% mixture of ascii and japanese characters
              Input4 = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
              Expected4 = <<"aあ1いbbう22えcccお333">>,  % assumed that the encoding of this file is UTF-8
              ?assertEqual({ok, Expected4, <<"">>}, jsone_decode:decode(Input4))
      end},
     {"string: surrogate pairs",
      fun () ->
              Input = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              Expected = <<"𢁉𢂚𢃼">>,
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"string: control characters",
      fun () ->
              Ctrls = lists:seq(0, 16#1f),
              lists:foreach(fun (C) ->
                                    %% Control characters are unacceptable
                                    ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<$", C, $">>))
                            end,
                            Ctrls),
              lists:foreach(fun (C) ->
                                    %% `allow_ctrl_chars' option allows strings which contain unescaped control characters
                                    ?assertEqual({ok, <<C>>, <<"">>},
                                                 jsone_decode:decode(<<$", C, $">>, [{allow_ctrl_chars, true}]))
                            end,
                            Ctrls)
      end},
     {"string: invalid escape characters",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\z\"">>)),  % '\z' is undefined
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\uab\"">>)),  % too few hex characters
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\ud848\"">>)),  % high(first) surrogate only
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\udc49\"">>)),  % low(second) surrogate only
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\ud848\\u0061\"">>)),  % missing low(second) surrogate
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\udf0u\"">>)),
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\ud848\\udf0u\"">>)),
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\u-3351\"">>))
      end},

     %% Arrays
     {"simple array",
      fun () ->
              Input = <<"[1,2,\"abc\",null]">>,
              Expected = [1, 2, <<"abc">>, null],
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"array: contains whitespaces",
      fun () ->
              Input = <<"[  1,\t2, \n \"abc\",\r null]">>,
              Expected = [1, 2, <<"abc">>, null],
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"empty array",
      fun () ->
              ?assertEqual({ok, [], <<"">>}, jsone_decode:decode(<<"[]">>)),
              ?assertEqual({ok, [], <<"">>}, jsone_decode:decode(<<"[ \t\r\n]">>))
      end},
     {"array: trailing comma is disallowed",
      fun () ->
              Input = <<"[1, 2, \"abc\", null, ]">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"array: missing comma",
      fun () ->
              Input = <<"[1 2, \"abc\", null]">>,  % a missing comma between '1' and '2'
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"array: missing closing bracket",
      fun () ->
              Input = <<"[1, 2, \"abc\", null">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},

     %% Objects
     {"simple object",
      fun () ->
              Input = <<"{\"1\":2,\"key\":\"value\"}">>,
              Expected = ?OBJ2(<<"1">>, 2, <<"key">>, <<"value">>),
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input)),  % `map' is the default format
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, ?MAP_OBJECT_TYPE}]))
      end},
     {"simple object: tuple or proplist",
      fun () ->
              Input = <<"{\"1\":2,\"key\":\"value\"}">>,
              Expected = {[{<<"1">>, 2}, {<<"key">>, <<"value">>}]},
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, tuple}])),
              ?assertEqual({ok, element(1, Expected), <<"">>}, jsone_decode:decode(Input, [{object_format, proplist}]))
      end},
     {"object: contains whitespaces",
      fun () ->
              Input = <<"{  \"1\" :\t 2,\n\r\"key\" :   \n  \"value\"}">>,
              Expected = ?OBJ2(<<"1">>, 2, <<"key">>, <<"value">>),
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"empty object",
      fun () ->
              ?assertEqual({ok, ?OBJ0, <<"">>}, jsone_decode:decode(<<"{}">>)),
              ?assertEqual({ok, ?OBJ0, <<"">>}, jsone_decode:decode(<<"{ \t\r\n}">>)),
              ?assertEqual({ok, {[]}, <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, tuple}])),
              ?assertEqual({ok, [{}], <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, proplist}]))
      end},
     {"empty object: map",
      fun () ->
              ?assertEqual({ok, ?OBJ0, <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, ?MAP_OBJECT_TYPE}]))
      end},
     {"duplicated members: map",
      fun () ->
              Input = <<"{\"1\":\"first\",\"1\":\"second\"}">>,
              Expected = ?OBJ2_DUP_KEY(<<"1">>, <<"first">>, <<"1">>, <<"second">>),
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, ?MAP_OBJECT_TYPE}]))
      end},
     {"duplicated members last: map",
      fun () ->
              Input = <<"{\"1\":\"first\",\"1\":\"second\"}">>,
              Expected = ?OBJ2_DUP_KEY_LAST(<<"1">>, <<"first">>, <<"1">>, <<"second">>),
              ?assertEqual({ok, Expected, <<"">>},
                           jsone_decode:decode(Input, [{object_format, ?MAP_OBJECT_TYPE}, {duplicate_map_keys, last}]))
      end},
     {"object: trailing comma is disallowed",
      fun () ->
              Input = <<"{\"1\":2, \"key\":\"value\", }">>,
              io:format("~p\n", [catch jsone_decode:decode(Input)]),
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input, [{object_format, tuple}]))
      end},
     {"object: missing comma",
      fun () ->
              Input = <<"{\"1\":2 \"key\":\"value\"}">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"object: missing field key",
      fun () ->
              Input = <<"{:2, \"key\":\"value\"}">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"object: non string key",
      fun () ->
              Input = <<"{1:2, \"key\":\"value\"}">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"object: missing field value",
      fun () ->
              Input = <<"{\"1\", \"key\":\"value\"}">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"object: missing closing brace",
      fun () ->
              Input = <<"{\"1\":2 \"key\":\"value\"">>,
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(Input))
      end},
     {"atom keys",
      fun () ->
              KeyOpt = fun (Keys) -> [{keys, Keys}, {object_format, proplist}] end,
              Input = <<"{\"foo\":\"ok\"}">>,
              ?assertEqual([{<<"foo">>, <<"ok">>}], jsone:decode(Input, KeyOpt(binary))),
              ?assertEqual([{foo, <<"ok">>}], jsone:decode(Input, KeyOpt(atom))),
              ?assertEqual([{foo, <<"ok">>}], jsone:decode(Input, KeyOpt(existing_atom))),
              ?assertError(badarg, jsone:decode(<<"{\"@#$%^!\":\"ok\"}">>, KeyOpt(existing_atom))),
              ?assertEqual([{foo, <<"ok">>}], jsone:decode(Input, KeyOpt(attempt_atom))),
              ?assertEqual([{<<"@#$%^!">>, <<"ok">>}], jsone:decode(<<"{\"@#$%^!\":\"ok\"}">>, KeyOpt(attempt_atom))),
              Value = integer_to_binary(1234),
              % do not make atom in test code
              [{Atom, <<"ok">>}] = jsone:decode(<<"{\"", Value/binary, "\":\"ok\"}">>, KeyOpt(atom)),
              ?assertEqual(Value, atom_to_binary(Atom, latin1))
      end},
     {"garbage remainings chars",
      fun () ->
              ?assertError(badarg, jsone:decode(<<"1@">>)),
              ?assertEqual(1, jsone:decode(<<"1 \n\t\r ">>))  % Whitespaces are OK
      end},

     %% Others
     {"compound data",
      fun () ->
              Input = <<"  [true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]   ">>,
              Expected = [true, ?OBJ2(<<"1">>, 2, <<"array">>, [[[[1]]], ?OBJ1(<<"ab">>, <<"cd">>), false]), null],
              ?assertEqual({ok, Expected, <<"   ">>}, jsone_decode:decode(Input))
      end},
     {"undefined_as_null option",
      fun () ->
              ?assertEqual({ok, undefined, <<>>}, jsone_decode:decode(<<"null">>, [undefined_as_null])),  % OK
              ?assertEqual({ok, null, <<>>}, jsone_decode:decode(<<"null">>, []))  % OK
      end},
     {"Invalid UTF-8 characters",
      fun () ->
              Input =
                  <<123, 34, 105, 100, 34, 58, 34, 190, 72, 94, 90, 253, 121, 94, 71, 73, 68, 91, 122, 211, 253, 32, 94,
                    86, 67, 163, 253, 230, 34, 125>>,
              ?assertMatch({ok, _, _}, jsone:try_decode(Input)),
              ?assertMatch({error, {badarg, _}}, jsone:try_decode(Input, [reject_invalid_utf8]))
      end}].

stream_decode_full_test_() ->
    [{"stream one chunk",
      fun () ->
              {incomplete, Fun} = jsone_decode:decode_stream(<<"null">>, []),
              ?assertEqual({ok, null, <<>>}, Fun(end_stream))
      end},
     {"stream two chunks",
      fun () ->
              {incomplete, Fun1} = jsone_decode:decode_stream(<<"nul">>, []),
              {incomplete, Fun2} = Fun1(<<"l">>),
              ?assertEqual({ok, null, <<>>}, Fun2(end_stream))
      end}].

stream_decode_byte_by_byte_test_() ->
    %% A complex JSON document without errors.
    %% Escaped strings include japanese, ascii, surrugate pairs and mixed.
    Jsons =
        [<<"true">>,
         <<"false">>,
         <<"null">>,  % symbols
         <<"1">>,
         <<"0">>,
         <<"-1">>,
         <<"-0">>,  % integers
         %% large integer
         <<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>,
         %% floats
         <<"1.23">>,
         <<"12345e-3">>,
         <<"12345E-3">>,
         <<"12345.0e-3">>,
         <<"0.12345E2">>,
         <<"0.12345e+2">>,
         <<"0.12345E+2">>,
         <<"-0.012345e3">>,
         <<"1.23000000000000000000e+02">>,
         %% strings
         <<"\"abc\"">>,
         <<"\"\\\"\\/\\\\\\b\\f\\n\\r\\t\"">>,
         <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
         <<"\"\\u0061\\u0062\\u0063\"">>,
         <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
         <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
         <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
         <<"[1,2,\"abc\",null]">>,  % simple array
         <<"[  1,\t2, \n \"abc\",\r null]">>,  % array: contains whitespaces
         <<"[]">>,
         <<"[ \t\r\n]">>,  % empty array
         <<"{\"1\":2,\"key\":\"value\"}">>,  % simple object
         %% object: contains whitespaces
         <<"{  \"1\" :\t 2,\n\r\"key\" :   \n  \"value\"}">>,
         <<"{}">>,
         <<"{ \t\r\n}">>,  % empty object
         <<"{\"1\":\"first\",\"1\":\"second\"}">>,  % duplicated members
         %% compound data
         <<"[true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]">>],
    %% Test cases
    [{"'" ++ binary_to_list(Json) ++ "'", fun () -> byte_by_byte(Json) end} || Json <- Jsons].

%% Parse one byte at a time, interleaved with parsing an empty binary
%% between every chunk.
byte_by_byte(Bin) ->
    Result =
        lists:foldl(fun (Byte, {incomplete, Continue1}) ->
                            {incomplete, Continue2} = Continue1(<<Byte>>),
                            Continue2(<<>>)
                    end,
                    jsone_decode:decode_stream(<<>>, []),
                    binary_to_list(Bin)),
    {incomplete, Fun} = Result,
    {ok, Expected, <<>>} = jsone_decode:decode(Bin),
    ?assertEqual({ok, Expected, <<>>}, Fun(end_stream)).

stream_decode_byte_by_byte_using_jsone_test_() ->
    Cases =
        [{"trailing whitespace", <<"[true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]  ">>},
         {"trailing garbage", <<"[true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null] bla bla">>}],
    [{Name, fun () -> byte_by_byte_using_jsone(Bin) end} || {Name, Bin} <- Cases].

%% Using jsone:decode_stream/2 as opposed to jsone_decode:decode_stream/2.
byte_by_byte_using_jsone(Bin) ->
    %% Parse one byte at a time, interleaved with parsing an empty binary
    %% between every chunk.
    {incomplete, Fun} =
        lists:foldl(fun (Byte, {incomplete, Continue1}) ->
                            {incomplete, Continue2} = Continue1(<<Byte>>),
                            Continue2(<<>>)
                    end,
                    jsone:decode_stream(<<>>, []),
                    binary_to_list(Bin)),
    Expected =
        try
            jsone:decode(Bin)
        catch
            C1:E1 ->
                {C1, E1}
        end,
    Actual =
        try
            Fun(end_stream)
        catch
            C2:E2 ->
                {C2, E2}
        end,
    ?assertEqual(Expected, Actual).
