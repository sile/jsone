%% Copyright (c) 2013-2021, Takeru Ohta <phjgt308@gmail.com>
-module(jsone_inet_tests).

-include_lib("eunit/include/eunit.hrl").


format_ipv4_test() ->
    Expected = <<"127.0.0.1">>,
    {ok, Addr} = inet:parse_ipv4_address(binary_to_list(Expected)),
    {ok, Actual} = jsone_inet:ip_address_to_json_string(Addr),
    ?assertEqual(Actual, Expected).


format_ipv6_test() ->
    Addresses = [<<"::127.0.0.1">>,
                 <<"::ffff:192.0.2.1">>,
                 <<"::ffff:0:255.255.255.255">>,
                 <<"64:ff9b::0.0.0.0">>,
                 <<"64:ff9b:1::192.168.1.1">>,
                 <<"64:ff9b:1::1:192.168.1.1">>,
                 <<"::1:2:3:2001:db8">>,
                 <<"2001:db8::">>,
                 <<"2001:db8::1">>,
                 <<"2001:db8::1:0:0:1">>,
                 <<"2001:db8:0:1:1:1:1:1">>,
                 <<"2001:0:0:1::1">>,
                 <<"2001:db8:85a3::8a2e:370:7334">>],
    lists:foreach(fun(Expected) ->
                          {ok, Addr} = inet:parse_ipv6_address(binary_to_list(Expected)),
                          {ok, Bin} = jsone_inet:ip_address_to_json_string(Addr),
                          ?assertEqual(Expected, Bin)
                  end,
                  Addresses).


invalid_ip_addr_test() ->
    ?assertEqual(jsone_inet:ip_address_to_json_string(foo), error),
    ?assertEqual(jsone_inet:ip_address_to_json_string({1, 2, 3}), error),
    ?assertEqual(jsone_inet:ip_address_to_json_string({0, 10000, 0, 0}), error),
    ?assertEqual(jsone_inet:ip_address_to_json_string({-1, 0, 0, 0, 0, 0, 0, 0}), error).
