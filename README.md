jsone (1.0.0)
=============

An Erlang library for encoding, decoding [JSON](http://json.org/index.html) data.


Features
--------
- Provides simple encode/decode function only
- [RFC4627](http://www.ietf.org/rfc/rfc4627.txt)-compliant
- Supports UTF-8 encoded binary
- Pure Erlang
- Highly Efficient
  - Maybe one of the fastest JSON library (except those which are implemented in NIF)
      - See [Benchmark](#benchmark)
  - Decode function is written in continuation-passing style(CPS)
      - CPS facilitates application of 'creation of sub binary delayed' optimization
      - See also [Erlang Efficiency Guide](http://www.erlang.org/doc/efficiency_guide/binaryhandling.html)


QuickStart
----------

```sh
# clone
$ git clone git://github.com/sile/jsone.git
$ cd jsone

# If you want to use HiPE enabled version, please execute following command.
# $ git checkout hipe

# compile
$ make compile

# run tests
$ make eunit

# dialyze
$ make dialyze

# Erlang shell
$ make start
1> jsone:decode(<<"[1,2,3]">>).
[1,2,3]
```


Usage Example
-------------

```erlang
%% Decode
> jsone:decode(<<"[1,2,3]">>).
[1,2,3]

> jsone:decode(<<"{\"1\":2}">>).
#{<<"1">> => 2}

> jsone:decode(<<"{\"1\":2}">>, [{object_format, tuple}]). % tuple format
{[{<<"1">>, 2}]}

> jsone:decode(<<"{\"1\":2}">>, [{object_format, proplist}]). % proplist format
[{<<"1">>, 2}]

> jsone:try_decode(<<"[1,2,3] \"next value\"">>). % try_decode/1 returns remaining (unconsumed binary)
{ok,[1,2,3],<<" \"next value\"">>}

% error: raises exception
> jsone:decode(<<"1.x">>).
** exception error: bad argument
     in function  jsone_decode:number_fraction_part_rest/6
        called as jsone_decode:number_fraction_part_rest(<<"x">>,1,1,0,[],<<>>)
     in call from jsone:decode/1 (src/jsone.erl, line 71)

% error: returns {error, Reason}
> jsone:try_decode(<<"1.x">>).
{error,{badarg,[{jsone_decode,number_fraction_part_rest,
                              [<<"x">>,1,1,0,[],<<>>],
                              [{line,228}]}]}}


%% Encode
> jsone:encode([1,2,3]).
<<"[1,2,3]">>

> jsone:encode(#{<<"key">> => <<"value">>}).  % map format
> jsone:encode({[{<<"key">>, <<"value">>}]}). % tuple format
> jsone:encode([{<<"key">>, <<"value">>}]).  % proplist format
<<"{\"key\":\"value\"}">>

> jsone:encode(#{key => <<"value">>}). % atom key is allowed
<<"{\"key\":\"value\"}">>

% error: raises exception
> jsone:encode(#{123 => <<"value">>}). % non binary|atom key is not allowed
** exception error: bad argument
     in function  jsone_encode:object_members/3
        called as jsone_encode:object_members([{123,<<"value">>}],[],<<"{">>)
     in call from jsone:encode/1 (src/jsone.erl, line 97)

% error: returns {error, Reason}
> jsone:try_encode({[{123, <<"value">>}]}).
{error,{badarg,[{jsone_encode,object_members,
                              [[{123,<<"value">>}],[],<<"{">>],
                              [{line,138}]}]}}

% 'object_key_type' option allows non-string object key
> jsone:encode({[{123, <<"value">>}]}, [{object_key_type, scalar}]).
<<"{\"123\":\"value\"}">>

%% Pretty Print
> Data = [true, #{<<"1">> => 2, <<"array">> => [[[[1]]], #{<<"ab">> => <<"cd">>}, false]}, null].
> io:format("~s\n", [jsone:encode(Data, [{indent, 1}, {space, 2}])]).
[
  true,
  {
    "1": 2,
    "array": [
      [
        [
          [
            1
          ]
        ]
      ],
      {
        "ab": "cd"
      },
      false
    ]
  },
  null
]
ok

%% Number Format
> jsone:encode(1). % integer
<<"1">>

> jsone:encode(1.23). % float
<<"1.22999999999999998224e+00">> % default: scientific notation

> jsone:encode(1.23, [{float_format, [{decimals, 4}]}]). % decimal notation
<<"1.2300">>

> jsone:encode(1.23, [{float_format, [{decimals, 4}, compact]}]). % compact decimal notation
<<"1.23">>
```


Data Mapping (Erlang <=> JSON)
-------------------------------

```
Erlang                  JSON             Erlang
=================================================================================================

null                 -> null          -> null
true                 -> true          -> true
false                -> false         -> false
<<"abc">>            -> "abc"         -> <<"abc">>
abc                  -> "abc"         -> <<"abc">> % non-special atom is regarded as a binary
123                  -> 123           -> 123
123.4                -> 123.4         -> 123.4
[1,2,3]              -> [1,2,3]       -> [1,2,3]
{[]}                 -> {}            -> {[]}                       % object_format=tuple
{[{key, <<"val">>}]} -> {"key":"val"} -> {[{<<"key">>, <<"val">>}]} % object_format=tuple
[{}]                 -> {}            -> [{}]                       % object_format=proplist
[{<<"key">>, val}]   -> {"key":"val"} -> [{<<"key">>, <<"val">>}]   % object_format=proplist
#{}                  -> {}            -> #{}                        % object_format=map
#{key => val}        -> {"key":"val"} -> #{<<"key">> => <<"val">>}  % object_format=map
```

API
---

See [EDoc Document](doc/jsone.md)


Benchmark
---------

### Environment/Method

- OS: CentOS 6.5
- CPU: Intel(R) Xeon(R) CPU E5-2680 v2 @ 2.80GHz (x8)
- Erlang/OTP: R17.1
- Benchmark Tool: [erl_json_test](https://github.com/si14/erl_json_test/tree/7ae5a254943ce3e5d9e4d5eb9cd2e86b92ce8e83)
- CompileOption: [native, {hipe, [o3]}]

### Target

- [jiffy-0.11.3](https://github.com/davisp/jiffy/tree/0.11.3) (NIF)
- [jsone-v0.2.3](https://github.com/sile/jsone/tree/0.2.3-hipe) (HiPE)
- [jsonerl](https://github.com/lambder/jsonerl/tree/9720df66052dfc66c9935d954061eda56f81a6f2) (HiPE)
- [jsonx](https://github.com/iskra/jsonx/tree/9c95948c6835827ed61a9506ae4a9aba61acf335) (NIF)
- [jsx-v2.0.1](https://github.com/talentdeficit/jsx/tree/v2.0.1) (HiPE)
- [mochijson2](https://github.com/bjnortier/mochijson2/tree/3663fb01fd98958181adc2d1300c7bfa553e1434) (HiPE)
- [yawsjson2](https://github.com/spawnproc/yawsjson2/tree/863b7476b4bf7615b578316670c3bd7f04e0048f) (HiPE)

### Decode Result

- column: module name
- row: data size (bytes)
- cell: elapsed time (micro seconds)

|                   | jiffy | jsone | jsonerl | jsonx |  jsx  | mochijson2 | yawsjson2 |
|------------------:|------:|------:|--------:|------:|------:|-----------:|----------:|
| 559 (1x)          | 12    | 11    | 61      | 7     | 50    | 28         | 37        |
| 1583 (3x)         | 24    | 25    | 66      | 15    | 134   | 68         | 84        |
| 4637 (9x)         | 65    | 86    | 178     | 36    | 410   | 186        | 311       |
| 13914 (27x)       | 189   | 271   | 533     | 109   | 1466  | 550        | 582       |
| 41542 (81x)       | 525   | 813   | 1578    | 299   | 4684  | 1599       | 1939      |
| 124726 (243x)     | 1549  | 2406  | 4709    | 852   | 14562 | 4799       | 6123      |

### Encode Result

- column: module name
- row: data size (bytes)
- cell: elapsed time (micro seconds)

|                   | jiffy | jsone | jsonerl | jsonx |  jsx  | mochijson2 | yawsjson2 |
|------------------:|------:|------:|--------:|------:|------:|-----------:|----------:|
| 559 (1x)          | 14    | 19    | 21      | 8     | 83    | 19         | 15        |
| 1583 (3x)         | 29    | 49    | 65      | 14    | 228   | 61         | 42        |
| 4637 (9x)         | 77    | 133   | 229     | 36    | 638   | 225        | 161       |
| 13914 (27x)       | 215   | 393   | 737     | 101   | 1993  | 664        | 435       |
| 41542 (81x)       | 621   | 1172  | 2058    | 300   | 6237  | 2310       | 1192      |
| 124726 (243x)     | 1830  | 3968  | 5842    | 828   | 17032 | 6979       | 5266      |
