%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_encode_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [
     %% シンボル系
     {"false がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"false">>}, jsone_encode:encode(false))
      end},
     {"true がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"true">>}, jsone_encode:encode(true))
      end},
     {"null がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"null">>}, jsone_encode:encode(null))
      end},

     %% 数値系: 整数
     {"0がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"0">>}, jsone_encode:encode(0))
      end},
     {"正の整数がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"1">>}, jsone_encode:encode(1))
      end},
     {"負の整数がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"-1">>}, jsone_encode:encode(-1))
      end},
     {"巨大な整数がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"11111111111111111111111111111111111111111111111111111111111111111111111">>},
                           jsone_encode:encode(11111111111111111111111111111111111111111111111111111111111111111111111))
      end},

     %% 数値系: 小数
     {"小数がエンコード可能",
      fun () ->
              Input = 1.234,
              ?assertMatch({ok, _}, jsone_encode:encode(Input)),
              ?assertEqual(Input, binary_to_float(element(2, jsone_encode:encode(Input))))
      end},

     %% 文字列系
     {"文字列がエンコード可能",
      fun () ->
              ?assertEqual({ok, <<"\"abc\"">>}, jsone_encode:encode(<<"abc">>))
      end},
     {"各種エスケープ文字を含む文字列をエンコード可能",
      fun () ->
              Input    = <<"\"\/\\\b\f\n\r\t">>,
              Expected = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"UTF-8形式のマルチバイト文字列がエンコード可能",
      fun () ->
              %% 日本語
              Input1    = <<"あいうえお">>,  % このファイルの文字エンコーディングがUTF-8であることが前提
              Expected1 = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304a\"">>,
              ?assertEqual({ok, Expected1}, jsone_encode:encode(Input1)),

              %% 日本語以外のマルチバイト文字
              Input2    = <<"۝۞ႮႯ">>,
              Expected2 = <<"\"\\u06dd\\u06de\\u10ae\\u10af\"">>,
              ?assertEqual({ok, Expected2}, jsone_encode:encode(Input2))
      end},
     {"サロゲートペアを含む文字列がエンコード可能",
      fun () ->
              Input    = <<"𢁉𢂚𢃼">>,
              Expected = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},

     %% 配列系
     {"配列(リスト)がエンコード可能",
      fun () ->
              Input    = [1, 2, 3],
              Expected = <<"[1,2,3]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"空配列がエンコード可能",
      fun () ->
              Input    = [],
              Expected = <<"[]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},

     %% オブジェクト系
     {"オブジェクトがエンコード可能",
      fun () ->
              Input    = {[{<<"key">>, <<"value">>}, {<<"1">>, 2}]},
              Expected = <<"{\"key\":\"value\",\"1\":2}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"空オブジェクトがエンコード可能",
      fun () ->
              Input    = {[]},
              Expected = <<"{}">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"オブジェクトのメンバのキーにはバイナリのみが使用可能",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{1, 2}]})),
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{"1", 2}]})),
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode({[{true, 2}]}))
      end},

     %% その他
     {"複雑なデータがエンコード可能",
      fun () ->
              Input    = [true, {[{<<"1">>, 2}, {<<"array">>, [[[[1]]], {[{<<"ab">>, <<"cd">>}]}, false]}]}, null],
              Expected = <<"[true,{\"1\":2,\"array\":[[[[1]]],{\"ab\":\"cd\"},false]},null]">>,
              ?assertEqual({ok, Expected}, jsone_encode:encode(Input))
      end},
     {"不正な値",
      fun () ->
              ?assertMatch({error, {badarg, _}}, jsone_encode:encode(self()))
      end}
    ].
