%% Copyright (c) 2013-2015, Takeru Ohta <phjgt308@gmail.com>
%% coding: latin-1
-module(jsone_decode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [
     %% Symbols
     {"false",
      fun () ->
              ?assertEqual({ok, false, <<"">>}, jsone_decode:decode(<<"false">>))
      end},
     {"true",
      fun () ->
              ?assertEqual({ok, true, <<"">>}, jsone_decode:decode(<<"true">>))
      end},
     {"null",
      fun () ->
              ?assertEqual({ok, null, <<"">>}, jsone_decode:decode(<<"null">>))
      end},

     %% Numbers: Integer
     {"positive integer",
      fun () ->
              ?assertEqual({ok, 1, <<"">>}, jsone_decode:decode(<<"1">>))
      end},
     {"zero",
      fun () ->
              ?assertEqual({ok, 0, <<"">>}, jsone_decode:decode(<<"0">>))
      end},
     {"negative integer",
      fun () ->
              ?assertEqual({ok, -1, <<"">>}, jsone_decode:decode(<<"-1">>))
      end},
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
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"+1">>))
      end},

     %% Numbers: Floats
     {"float: decimal notation",
      fun () ->
              ?assertEqual({ok, 1.23, <<"">>}, jsone_decode:decode(<<"1.23">>))
      end},
     {"float: exponential notation",
      fun () ->
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345e-3">>)), % lower case 'e'
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345E-3">>)), % upper case 'E'
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"12345.0e-3">>)),
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345E2">>)),
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345e+2">>)), % exponent part can begin with plus sign
              ?assertEqual({ok, 12.345, <<"">>}, jsone_decode:decode(<<"0.12345E+2">>)),
              ?assertEqual({ok, -12.345, <<"">>}, jsone_decode:decode(<<"-0.012345e3">>))
      end},
     {"float: invalid format",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<".123">>)),  % omitted integer part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.">>)),    % omitted fraction part: EOS
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.e+3">>)), % omitted fraction part: with exponent part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e">>)),    % imcomplete fraction part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e-">>)),   % imcomplete fraction part
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1ee-1">>)), % duplicated 'e'
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"0.1e--1">>)), % duplicated sign
              ?assertEqual({ok, 0.1, <<".2">>}, jsone_decode:decode(<<"0.1.2">>))     % duplicated '.': interpreted as individual tokens
      end},

     %% Strings
     {"simple string",
      fun () ->
              ?assertEqual({ok, <<"abc">>,  <<"">>}, jsone_decode:decode(<<"\"abc\"">>))
      end},
     {"string: escaped characters",
      fun () ->
              Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              Expected = <<"\"\/\\\b\f\n\r\t">>,
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"string: escaped Unicode characters",
      fun () ->
              %% japanese
              Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
              Expected1 = <<"あいうえお">>,  % assumed that the encoding of this file is UTF-8
              ?assertEqual({ok, Expected1, <<"">>}, jsone_decode:decode(Input1)),

              %% ascii
              Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
              Expected2 = <<"abc">>,
              ?assertEqual({ok, Expected2, <<"">>}, jsone_decode:decode(Input2)),

              %% other multi-byte characters
              Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
              Expected3 = <<"۝۞ႮႯ">>,
              ?assertEqual({ok, Expected3, <<"">>}, jsone_decode:decode(Input3)),

              %% mixture of ascii and japanese characters
              Input4    = <<"\"a\\u30421\\u3044bb\\u304622\\u3048ccc\\u304A333\"">>,
              Expected4 = <<"aあ1いbbう22えcccお333">>,  % assumed that the encoding of this file is UTF-8
              ?assertEqual({ok, Expected4, <<"">>}, jsone_decode:decode(Input4))
      end},
     {"string: surrogate pairs",
      fun () ->
              Input    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              Expected = <<"𢁉𢂚𢃼">>,
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"string: invalid escape characters",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\z\"">>)),    % '\z' is undefined
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\uab\"">>)),  % too few hex characters
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\ud848\"">>)), % high(first) surrogate only
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\udc49\"">>)), % low(second) surrogate only
              ?assertMatch({error, {badarg, _}}, jsone_decode:decode(<<"\"\\ud848\\u0061\"">>)) % missing low(second) surrogate
      end},

     %% Arrays
     {"simple array",
      fun () ->
              Input    = <<"[1,2,\"abc\",null]">>,
              Expected = [1, 2, <<"abc">>, null],
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"array: contains whitespaces",
      fun () ->
              Input    = <<"[  1,\t2, \n \"abc\",\r null]">>,
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
              Input = <<"[1 2, \"abc\", null]">>, % a missing comma between '1' and '2'
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
              Input    = <<"{\"1\":2,\"key\":\"value\"}">>,
              Expected = #{<<"1">> => 2, <<"key">> => <<"value">>},
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input)), % `map' is the default format
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, map}]))
      end},
     {"simple object: tuple or proplist",
      fun () ->
              Input    = <<"{\"1\":2,\"key\":\"value\"}">>,
              Expected = {[{<<"1">>, 2},{<<"key">>, <<"value">>}]},
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, tuple}])),
              ?assertEqual({ok, element(1, Expected), <<"">>}, jsone_decode:decode(Input, [{object_format, proplist}]))
      end},
     {"object: contains whitespaces",
      fun () ->
              Input    = <<"{  \"1\" :\t 2,\n\r\"key\" :   \n  \"value\"}">>,
              Expected = #{<<"1">> => 2, <<"key">> => <<"value">>},
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"empty object",
      fun () ->
              ?assertEqual({ok, #{}, <<"">>}, jsone_decode:decode(<<"{}">>)),
              ?assertEqual({ok, #{}, <<"">>}, jsone_decode:decode(<<"{ \t\r\n}">>)),
              ?assertEqual({ok, {[]}, <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, tuple}])),
              ?assertEqual({ok, [{}], <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, proplist}]))
      end},
     {"empty object: map",
      fun () ->
              ?assertEqual({ok, #{}, <<"">>}, jsone_decode:decode(<<"{}">>, [{object_format, map}]))
      end},
     {"duplicated members: map",
      fun () ->
              Input    = <<"{\"1\":\"first\",\"1\":\"second\"}">>,
              Expected = #{<<"1">> => <<"first">>}, % the first (leftmost) value is used
              ?assertEqual({ok, Expected, <<"">>}, jsone_decode:decode(Input, [{object_format, map}]))
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

     %% Others
     {"compound data",
      fun () ->
              Input    = <<"  [true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]   ">>,
              Expected = [true, #{<<"1">> => 2, <<"array">> => [[[[1]]], #{<<"ab">> => <<"cd">>}, false]}, null],
              ?assertEqual({ok, Expected, <<"   ">>}, jsone_decode:decode(Input))
      end}
    ].
