%% Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>
%% coding: latin-1
-module(jsone_encode_tests).

-include_lib("eunit/include/eunit.hrl").

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

     %% Strings
     {"simple string",
      fun () ->
              ?assertEqual({ok, <<"\"abc\"">>}, jsone_encode:encode(<<"abc">>))
      end},
     {"string: contains escaped characters",
      fun () ->
              Input    = <<"\"\/\\\b\f\n\r\t">>,
              Expected = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
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
     {"atom key is allowed",
      fun () ->
              Expected = <<"{\"key\":2}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode({[{key, 2}]}))
      end},
     {"non binary object member key is disallowed",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{1, 2}]})),
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{"1", 2}]}))
      end},

     %% Others
     {"compound data",
      fun () ->
              Input    = [true, {[{<<"1">>, 2}, {<<"array">>, [[[[1]]], {[{<<"ab">>, <<"cd">>}]}, false]}]}, null],
              Expected = <<"[true,{\"1\":2,\"array\":[[[[1]]],{\"ab\":\"cd\"},false]},null]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"invalid value",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode(self()))
      end}
    ].
