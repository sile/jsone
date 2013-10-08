%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_decode_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [
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
              ?assertError({invalid_json, number_integer_part, _}, jsone_decode:decode(<<"+1">>))
      end},
     {"小数がデコード可能",
      fun () ->
              ?assertEqual({1.23, <<"">>}, jsone_decode:decode(<<"1.23">>))
      end},
     {"指数形式の小数がデコード可能",
      fun () ->
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345e-3">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345E-3">>)), % 'e'は大文字でも可
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"12345.0e-3">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"0.12345e2">>)),
              ?assertEqual({12.345, <<"">>}, jsone_decode:decode(<<"0.12345e+2">>)), % 指数部では'+'をつけても良い
              ?assertEqual({-12.345, <<"">>}, jsone_decode:decode(<<"-0.012345e3">>))
      end}%% ,
     %% {"不正な形式の小数",
     %%  fun () ->
     %%          ?assertEqual(ok, jsone_decode:decode(<<"
     %%  end}
    ].
