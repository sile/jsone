jsone
=====

[![hex.pm version](https://img.shields.io/hexpm/v/jsone.svg)](https://hex.pm/packages/jsone)

An Erlang library for encoding, decoding [JSON](http://json.org/index.html) data.


Features
--------
- Provides simple encode/decode function only
- [RFC7159](http://www.ietf.org/rfc/rfc7159.txt)-compliant
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

null                 -> null                   -> null
true                 -> true                   -> true
false                -> false                  -> false
<<"abc">>            -> "abc"                  -> <<"abc">>
abc                  -> "abc"                  -> <<"abc">> % non-special atom is regarded as a binary
{{2010,1,1},{0,0,0}} -> "2010-01-01T00:00:00Z" -> <<"2010-01-01T00:00:00Z">> % datetime (see: `jsone:datetime_encode_format/0`)
123                  -> 123                    -> 123
123.4                -> 123.4                  -> 123.4
[1,2,3]              -> [1,2,3]                -> [1,2,3]
{[]}                 -> {}                     -> {[]}                       % object_format=tuple
{[{key, <<"val">>}]} -> {"key":"val"}          -> {[{<<"key">>, <<"val">>}]} % object_format=tuple
[{}]                 -> {}                     -> [{}]                       % object_format=proplist
[{<<"key">>, val}]   -> {"key":"val"}          -> [{<<"key">>, <<"val">>}]   % object_format=proplist
#{}                  -> {}                     -> #{}                        % object_format=map
#{key => val}        -> {"key":"val"}          -> #{<<"key">> => <<"val">>}  % object_format=map
```

API
---

See [EDoc Document](doc/jsone.md)


Benchmark
---------

The results of [poison](https://github.com/devinus/poison) benchmarking.

See the [BENCHMARK.md](BENCHMARK.md) file for more information.

### EncoderBench Result

__Non HiPE__:

|                  | jiffy        | jsone             | poison        | jazz          | jsx           |
|:-----------------|-------------:|------------------:|--------------:|--------------:|--------------:|
| maps             |   7.35 μs/op |   10.00 μs/op (2) |   13.26 μs/op |   18.97 μs/op |   30.03 μs/op |
| lists            | 214.93 μs/op |  151.70 μs/op (2) |  114.34 μs/op |  197.31 μs/op |  337.25 μs/op |
| strings*         | 103.77 μs/op |  550.78 μs/op (5) |  350.49 μs/op |  445.60 μs/op |  237.08 μs/op |
| string escaping* | 141.46 μs/op |  934.83 μs/op (2) | 1387.10 μs/op | 1568.84 μs/op | 1371.11 μs/op |
| large value**    | 404.95 μs/op | 1379.04 μs/op (3) | 1322.36 μs/op | 1818.49 μs/op | 2027.41 μs/op |
| pretty print**   | 415.19 μs/op | 1734.20 μs/op (3) | 1453.36 μs/op | 2031.73 μs/op | 5223.72 μs/op |


__HiPE__:

|                  | jiffy        | jsone             | poison        | jazz          | jsx           |
|:-----------------|-------------:|------------------:|--------------:|--------------:|--------------:|
| maps             |   7.12 μs/op |    5.58 μs/op (1) |   10.71 μs/op |   21.08 μs/op |   25.38 μs/op |
| lists            | 198.89 μs/op |   64.76 μs/op (1) |   76.80 μs/op |  217.64 μs/op |  220.85 μs/op |
| strings*         | 109.11 μs/op |  323.63 μs/op (4) |  140.58 μs/op |  363.87 μs/op |  179.35 μs/op |
| string escaping* | 140.35 μs/op |  481.52 μs/op (2) | 1222.81 μs/op | 1312.68 μs/op |  699.05 μs/op |
| large value**    | 388.06 μs/op |  634.06 μs/op (2) |  744.13 μs/op | 1572.41 μs/op | 1667.37 μs/op |
| pretty print**   | 404.84 μs/op |  956.29 μs/op (3) |  802.29 μs/op | 1795.51 μs/op | 4434.13 μs/op |

\* binary representation of [UTF-8-demo.txt](https://github.com/devinus/poison/blob/2.1.0/bench/data/UTF-8-demo.txt)  <br />
\** [generated.json](https://github.com/devinus/poison/blob/2.1.0/bench/data/generated.json)

### ParserBench Result

__Non HiPE__:

|                    | jiffy        | jsone             | poison        | jsx           |
|:-------------------|-------------:|------------------:|--------------:|--------------:|
| json value*        | 522.25 μs/op | 1217.44 μs/op (2) | 1223.37 μs/op | 1630.77 μs/op |
| UTF-8 unescaping** |  59.63 μs/op |  342.50 μs/op (4) |  219.79 μs/op |  243.64 μs/op |


__HiPE__:

|                    | jiffy        | jsone             | poison        | jsx           |
|:-------------------|-------------:|------------------:|--------------:|--------------:|
| json value*        | 504.77 μs/op |  527.77 μs/op (2) |  686.90 μs/op | 1320.74 μs/op |
| UTF-8 unescaping** |  59.95 μs/op |   83.79 μs/op (2) |  109.70 μs/op |  159.35 μs/op |

\* [generated.json](https://github.com/devinus/poison/blob/2.1.0/bench/data/generated.json) <br />
\** [UTF-8-demo.txt](https://github.com/devinus/poison/blob/2.1.0/bench/data/UTF-8-demo.txt)

License
-------

This library is released under the MIT License.

See the [COPYING](COPYING) file for full license information.
