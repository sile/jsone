%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_decode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [
     %% シンボル系
     {"'false'がデコード可能",
      fun () ->
              ?assertEqual({false, <<"">>}, jsone_decode:decode(<<"false">>))
      end},
     {"'true'がデコード可能",
      fun () ->
              ?assertEqual({true, <<"">>}, jsone_decode:decode(<<"true">>))
      end},
     {"'null'がデコード可能",
      fun () ->
              ?assertEqual({null, <<"">>}, jsone_decode:decode(<<"null">>))
      end},
     {"正の整数がデコード可能",
      fun () ->
              ?assertEqual({1, <<"">>}, jsone_decode:decode(<<"1">>))
      end},

     %% 数値系: 整数
     {"0がデコード可能",
      fun () ->
              ?assertEqual({0, <<"">>}, jsone_decode:decode(<<"0">>))
      end},
     {"負の整数がデコード可能",
      fun () ->
              ?assertEqual({-1, <<"">>}, jsone_decode:decode(<<"-1">>))
      end},
     {"整数の値の大きさに制限はなし",
      fun () ->
              ?assertEqual({111111111111111111111111111111111111111111111111111111111111111111111111111111, <<"">>},
                           jsone_decode:decode(<<"111111111111111111111111111111111111111111111111111111111111111111111111111111">>))
      end},
     {"先頭に余計な0がつく場合は、先頭文字とそれ以降が別々のトークンと判断される",
      fun () ->
              ?assertEqual({0, <<"0">>}, jsone_decode:decode(<<"00">>)),
              ?assertEqual({0, <<"1">>}, jsone_decode:decode(<<"01">>)),
              ?assertEqual({0, <<"1">>}, jsone_decode:decode(<<"-01">>))
      end},
     {"正の整数の前の'+'記号は許可されない",
      fun () ->
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"+1">>))
      end},

     %% 数値系: 小数
     {"小数がデコード可能",
      fun () ->
              ?assertEqual({1.23, <<"">>}, jsone_decode:decode(<<"1.23">>))
      end},
     {"指数形式の小数がデコード可能",
      fun () ->
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345e-3">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345E-3">>)), % 'e'は大文字でも可
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345.0e-3">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"0.12345E2">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"0.12345e+2">>)), % 指数部では'+'をつけても良い
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"0.12345E+2">>)), % 指数部では'+'をつけても良い
              ?assertEqual({-12.345, <<"">>}, jsone_decode:decode(<<"-0.012345e3">>))
      end},
     {"不正な形式の小数",
      fun () ->
              ?assertError({invalid_json, _}, jsone_decode:decode(<<".123">>)),  % 整数部が省略されている
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.">>)),    % '.'の後ろに小数部が続かない
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.e+3">>)), % '.'の後ろに指数部が来る
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.1e">>)),    % 指数部が欠けている
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.1e-">>)),   % 指数部が欠けている
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.1ee-1">>)), % 'e'が複数ある
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"0.1e--1">>)), % 符号が複数ある
              ?assertEqual({0.1, <<".2">>}, jsone_decode:decode(<<"0.1.2">>))  % '.'が複数ある => 別々のトークンと判断される
      end},
     
     %% 文字列系
     {"文字列がデコード可能",
      fun () ->
              ?assertEqual({<<"abc">>,  <<"">>}, jsone_decode:decode(<<"\"abc\"">>))
      end},
     {"各種エスケープ文字がデコード可能",
      fun () ->
              Input    = list_to_binary([$", [[$\\, C] || C <- [$", $/, $\\, $b, $f, $n, $r, $t]], $"]),
              Expected = <<"\"\/\\\b\f\n\r\t">>,
              ?assertEqual({Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"エスケープされたUTF-16文字列がデコード可能",
      fun () ->
              %% 日本語
              Input1    = <<"\"\\u3042\\u3044\\u3046\\u3048\\u304A\"">>,
              Expected1 = <<"あいうえお">>,  % このファイルの文字エンコーディングがUTF-8であることが前提
              ?assertEqual({Expected1, <<"">>}, jsone_decode:decode(Input1)),

              %% ascii
              Input2    = <<"\"\\u0061\\u0062\\u0063\"">>,
              Expected2 = <<"abc">>,
              ?assertEqual({Expected2, <<"">>}, jsone_decode:decode(Input2)),

              %% 日本語以外のマルチバイト文字
              Input3    = <<"\"\\u06DD\\u06DE\\u10AE\\u10AF\"">>,
              Expected3 = <<"۝۞ႮႯ">>,
              ?assertEqual({Expected3, <<"">>}, jsone_decode:decode(Input3))
      end},
     {"サロゲートペアを含む文字列がデコード可能",
      fun () ->
              Input    = <<"\"\\ud848\\udc49\\ud848\\udc9a\\ud848\\udcfc\"">>,
              Expected = <<"𢁉𢂚𢃼">>,
              ?assertEqual({Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"不正なエスケープ文字",
      fun () ->
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"\"\\z\"">>)),    % '\z'は未定義のエスケープ文字
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"\"\\uab\"">>)),  % '\u'の後ろに続く数値が足りない
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"\"\\ud848\"">>)), % 上位サロゲートが単独で出現
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"\"\\udc49\"">>)), % 下位サロゲーが単独で出現
              ?assertError({invalid_json, _}, jsone_decode:decode(<<"\"\\ud848\\u0061\"">>)) % 上位サロゲートの後ろに下位サロゲートが続かない
      end},

     %% 配列系
     {"配列がデコード可能",
      fun () ->
              Input    = <<"[1, 2, \"abc\", null]">>,
              Expected = [1, 2, <<"abc">>, null],
              ?assertEqual({Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"空配列がデコード可能",
      fun () ->
              ?assertEqual({[], <<"">>}, jsone_decode:decode(<<"[]">>)),
              ?assertEqual({[], <<"">>}, jsone_decode:decode(<<"[ \t\r\n]">>))
      end},
     {"配列の末尾のカンマは許容されない",
      fun () ->
              Input = <<"[1, 2, \"abc\", null, ]">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"区切り文字のカンマが抜けているとエラーとなる",
      fun () ->
              Input = <<"[1 2, \"abc\", null]">>, % 1と2の間にカンマがない
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"配列が閉じていないとエラー",
      fun () ->
              Input = <<"[1, 2, \"abc\", null">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},

     %% オブジェクト系
     {"オブジェクトがデコード可能",
      fun () ->
              Input    = <<"{\"1\":2, \"key\":\"value\"}">>,
              Expected = {object, [{<<"1">>, 2}, {<<"key">>, <<"value">>}]},
              ?assertEqual({Expected, <<"">>}, jsone_decode:decode(Input))
      end},
     {"空オブジェクトがデコード可能",
      fun () ->
              ?assertEqual({{object, []}, <<"">>}, jsone_decode:decode(<<"{}">>)),
              ?assertEqual({{object, []}, <<"">>}, jsone_decode:decode(<<"{ \t\r\n}">>))
      end},
     {"オブジェクトの末尾のカンマは許容されない",
      fun () ->
              Input = <<"{\"1\":2, \"key\":\"value\", }">>,
              io:format("~p\n", [catch jsone_decode:decode(Input)]),
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"区切り文字のカンマが抜けているとエラーとなる",
      fun () ->
              Input = <<"{\"1\":2 \"key\":\"value\"}">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"メンバのキーがない場合はエラー",
      fun () ->
              Input = <<"{:2, \"key\":\"value\"}">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"メンバのキーが文字列以外の場合はエラー",
      fun () ->
              Input = <<"{1:2, \"key\":\"value\"}">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"メンバの値がない場合はエラー",
      fun () ->
              Input = <<"{\"1\", \"key\":\"value\"}">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},
     {"オブジェクトが閉じていないとエラー",
      fun () ->
              Input = <<"{\"1\":2 \"key\":\"value\"">>,
              ?assertError({invalid_json, _}, jsone_decode:decode(Input))
      end},

     %% その他
     {"複雑なデータがデコード可能",
      fun () ->
              Input    = <<"  [true, {\"1\" : 2, \"array\":[[[[1]]], {\"ab\":\"cd\"}, false]}, null]   ">>,
              Expected = [true, {object, [{<<"1">>, 2}, {<<"array">>, [[[[1]]], {object, [{<<"ab">>, <<"cd">>}]}, false]}]}, null],
              ?assertEqual({Expected, <<"   ">>}, jsone_decode:decode(Input))
      end}
    ].
