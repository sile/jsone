%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    [
     {"エンコード処理は jsone_encode:encode/1 が行う",
      fun () ->
              ok = meck:new(jsone_encode),
              ok = meck:expect(jsone_encode, encode, 1, dummy_result),
              ?assertEqual(dummy_result, jsone:encode([1, 2, 3])),
              ok = meck:unload(jsone_encode)
      end}
    ].

decode_test_() ->
    [
     {"デコード処理は jsone_decode:decode/1 が行う",
      fun () ->
              ok = meck:new(jsone_decode),
              ok = meck:expect(jsone_decode, decode, 1, dummy_result),
              ?assertEqual(dummy_result, jsone:decode(<<"[1, 2, 3]">>)),
              ok = meck:unload(jsone_decode)
      end}
    ].

