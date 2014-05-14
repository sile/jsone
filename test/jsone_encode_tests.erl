%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_encode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [
     %% シンボル系
     {"false がエンコード可能",
      fun () ->
              ?assertEqual(<<"false">>, jsone_encode:encode(false))
      end},
     {"true がエンコード可能",
      fun () ->
              ?assertEqual(<<"true">>, jsone_encode:encode(true))
      end},
     {"null がエンコード可能",
      fun () ->
              ?assertEqual(<<"null">>, jsone_encode:encode(null))
      end},

     %% 数値系: 整数
     {"0がエンコード可能",
      fun () ->
              ?assertEqual(<<"0">>, jsone_encode:encode(0))
      end},
     {"正の整数がエンコード可能",
      fun () ->
              ?assertEqual(<<"1">>, jsone_encode:encode(1))
      end},
     {"負の整数がエンコード可能",
      fun () ->
              ?assertEqual(<<"-1">>, jsone_encode:encode(-1))
      end},
     {"巨大な整数がエンコード可能",
      fun () ->
              ?assertEqual(<<"11111111111111111111111111111111111111111111111111111111111111111111111">>,
                           jsone_encode:encode(11111111111111111111111111111111111111111111111111111111111111111111111))
      end},

     %% 数値系: 小数
     {"小数がエンコード可能",
      fun () ->
              Input   = 1.234,
              Encoded = jsone_encode:encode(Input),
              ?assertEqual(Input, binary_to_float(Encoded))
      end},

     %% 文字列系
     {"文字列がエンコード可能",
      fun () ->
              ?assertEqual(<<"\"abc\"">>, jsone_encode:encode(<<"abc">>))
      end},
     {"各種エスケープ文字を含む文字列をエンコード可能",
      fun () ->
              Input    = <<"\"\/\\\b\f\n\r\t">>,
              Expected = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},
     {"UTF-8形式のマルチバイト文字列がエンコード可能",
      fun () ->
              %% 日本語
              Input1    = <<"あいうえお">>,  % このファイルの文字エンコーディングがUTF-8であることが前提
              Expected1 = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304a\"">>,
              ?assertEqual(Expected1, jsone_encode:encode(Input1)),

              %% 日本語以外のマルチバイト文字
              Input2    = <<"۝۞ႮႯ">>,
              Expected2 = <<"\"\\u06dd\\u06de\\u10ae\\u10af\"">>,
              ?assertEqual(Expected2, jsone_encode:encode(Input2))
      end},
     {"サロゲートペアを含む文字列がエンコード可能",
      fun () ->
              Input    = <<"𢁉𢂚𢃼">>,
              Expected = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},

     %% 配列系
     {"配列(リスト)がエンコード可能",
      fun () ->
              Input    = [1, 2, 3],
              Expected = <<"[1,2,3]">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},
     {"空配列がエンコード可能",
      fun () ->
              Input    = [],
              Expected = <<"[]">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},

     %% オブジェクト系
     {"オブジェクトがエンコード可能",
      fun () ->
              Input    = {[{<<"key">>, <<"value">>}, {<<"1">>, 2}]},
              Expected = <<"{\"key\":\"value\",\"1\":2}">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},
     {"空オブジェクトがエンコード可能",
      fun () ->
              Input    = {[]},
              Expected = <<"{}">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},
     {"オブジェクトのメンバのキーにはバイナリのみが使用可能",
      fun () ->
              ?assertError(badarg, jsone_encode:encode({[{1, 2}]})),
              ?assertError(badarg, jsone_encode:encode({[{"1", 2}]})),
              ?assertError(badarg, jsone_encode:encode({[{true, 2}]}))
      end},

     %% その他
     {"複雑なデータがエンコード可能",
      fun () ->
              Input    = [true, {[{<<"1">>, 2}, {<<"array">>, [[[[1]]], {[{<<"ab">>, <<"cd">>}]}, false]}]}, null],
              Expected = <<"[true,{\"1\":2,\"array\":[[[[1]]],{\"ab\":\"cd\"},false]},null]">>,
              ?assertEqual(Expected, jsone_encode:encode(Input))
      end},
     {"不正な値",
      fun () ->
              ?assertError(badarg, jsone_encode:encode(self()))
      end}
    ].
