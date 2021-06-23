jsone
=====

[![hex.pm version](https://img.shields.io/hexpm/v/jsone.svg)](https://hex.pm/packages/jsone)
[![Build Status](https://github.com/sile/jsone/workflows/build/badge.svg)](https://github.com/sile/jsone)
[![Code Coverage](https://codecov.io/gh/sile/jsone/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/jsone/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

An Erlang library for encoding, decoding [JSON](http://json.org/index.html) data.


Features
--------
- Provides simple encode/decode function only
- [RFC7159](http://www.ietf.org/rfc/rfc7159.txt)-compliant
- Supports UTF-8 encoded binary
- Pure Erlang
- Highly Efficient
  - Maybe one of the fastest JSON library (except those which are implemented in NIF)
      - See [Benchmark](benchmark/README.md)
  - Decode function is written in continuation-passing style(CPS)
      - CPS facilitates application of 'creation of sub binary delayed' optimization
      - See also [Erlang Efficiency Guide](http://www.erlang.org/doc/efficiency_guide/binaryhandling.html)


QuickStart
----------

```sh
# clone
$ git clone git://github.com/sile/jsone.git
$ cd jsone

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

Enable HiPE
-----------

If you want to use HiPE compiled version, please add following code to your rebar.config.

```erlang
{overrides,
  [
    {override, jsone, [{erl_opts, [{d, 'ENABLE_HIPE'}, inline]}]}
  ]}.
```

or use `native` profile. The `make` command supports profile as well. For example:

```sh
$ make start profile=native
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

% 'undefined_as_null' option allows encoding atom undefined as null
> jsone:encode(undefined,[undefined_as_null]).
<<"null">>

%% Pretty Print
> Data = [true, #{<<"1">> => 2, <<"array">> => [[[[1]]], #{<<"ab">> => <<"cd">>}, [], #{}, false]}, null].
> io:format("~s\n", [jsone:encode(Data, [{indent, 2}, {space, 1}])]).
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
      [],
      {},
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

%% If you want to safely cast object keys to atoms, the `attempt_atom' option will help.
> jsone:decode(<<"{\"hello\": \"world\"}">>, [{keys, attempt_atom}]).
#{<<"hello">> => <<"world">>}  % There is no atom named "hello", so the key is decoded as binary.

> hello.  % Create "hello" atom.
hello

> jsone:decode(<<"{\"hello\": \"world\"}">>, [{keys, attempt_atom}]).
#{hello => <<"world">>} % Now, the key is decoded as atom.
```


Data Mapping (Erlang <=> JSON)
-------------------------------

```
Erlang                  JSON             Erlang
=================================================================================================

null                   -> null                       -> null
undefined              -> null                       -> undefined                  % undefined_as_null
true                   -> true                       -> true
false                  -> false                      -> false
<<"abc">>              -> "abc"                      -> <<"abc">>
abc                    -> "abc"                      -> <<"abc">> % non-special atom is regarded as a binary
{{2010,1,1},{0,0,0}}   -> "2010-01-01T00:00:00Z"     -> <<"2010-01-01T00:00:00Z">>     % datetime*
{{2010,1,1},{0,0,0.0}} -> "2010-01-01T00:00:00.000Z" -> <<"2010-01-01T00:00:00.000Z">> % datetime*
123                    -> 123                        -> 123
123.4                  -> 123.4                      -> 123.4
[1,2,3]                -> [1,2,3]                    -> [1,2,3]
{[]}                   -> {}                         -> {[]}                       % object_format=tuple
{[{key, <<"val">>}]}   -> {"key":"val"}              -> {[{<<"key">>, <<"val">>}]} % object_format=tuple
[{}]                   -> {}                         -> [{}]                       % object_format=proplist
[{<<"key">>, val}]     -> {"key":"val"}              -> [{<<"key">>, <<"val">>}]   % object_format=proplist
#{}                    -> {}                         -> #{}                        % object_format=map
#{key => val}          -> {"key":"val"}              -> #{<<"key">> => <<"val">>}  % object_format=map
{{json, IOList}}       -> Value                      -> ~~~                        % UTF-8 encoded term**
{{json_utf8, Chars}}   -> Value                      -> ~~~                        % Unicode code points**
```

\* see [jsone:datetime_encode_format()](doc/jsone.md#type-datetime_encode_format)

\** `{json, IOList}` and `{json_utf8, Chars}` allows inline already encoded JSON
values. For example, you obtain JSON encoded data from database so you don't
have to decode it first and encode again. See [jsone:json_term()](doc/jsone.md#type-json_term).

API
---

See [EDoc Document](doc/jsone.md)


Benchmark
---------

The results of [poison](https://github.com/devinus/poison) benchmarking.

See the [benchmark/README.md](benchmark/README.md) file for more information.

### Encoding (Unit: IPS=inputs per second)

| Input data \ Library | Jason      | jiffy       | JSON*  | jsone      | JSX    | Poison | Tiny   |
|----------------------|-----------:|------------:|-------:|-----------:|-------:|-------:|-------:|
| [Blockchain]         |     2.77 K |  **4.55 K** | 0.45 K | 1.44 K (3) | 0.60 K | 1.30 K | 0.99 K |
| [Giphy]              |     230.65 |  **487.67** |  47.73 | 114.57 (4) |  44.97 | 114.57 | 113.59 |
| [GitHub]             |     880.03 | **1566.67** | 139.79 | 300.26 (5) |  99.68 | 424.75 | 455.07 |
| [GovTrack]           |       6.57 |   **24.92** |   2.33 |   5.35 (5) |   2.65 |   7.06 |   7.86 |
| [Issue 90]           |  **22.80** |       21.92 |   0.77 |  14.30 (3) |   5.33 |  12.60 |  12.95 |
| [JSON Generateor]    |     200.40 |  **606.81** |  42.45 | 147.12 (4) |  68.73 | 187.95 | 123.93 |
| [Pokedex]            |     209.51 |  **776.67** |  62.60 | 161.45 (4) |  69.87 | 190.93 | 125.16 |
| [UTF-8 unescaped]    |     626.25 | **6644.53** |1167.89 | 582.41 (4) | 273.48 | 401.44 | 220.14 |

\* Only `JSON` didn't escape non-ASCII unicode characters on the encoding

[Blockchain]: https://github.com/devinus/poison/blob/4.0.1/bench/data/blockchain.json
[Giphy]: https://github.com/devinus/poison/blob/4.0.1/bench/data/giphy.json
[GitHub]: https://github.com/devinus/poison/blob/4.0.1/bench/data/github.json
[GovTrack]: https://github.com/devinus/poison/blob/4.0.1/bench/data/govtrack.json
[Issue 90]: https://github.com/devinus/poison/blob/4.0.1/bench/data/issue-90.json
[JSON Generateor]: https://github.com/devinus/poison/blob/4.0.1/bench/data/json-generator.json
[JSON Generateor (Pretty)]: https://github.com/devinus/poison/blob/4.0.1/bench/data/json-generator-pretty.json
[Pokedex]: https://github.com/devinus/poison/blob/4.0.1/bench/data/pokedex.json
[UTF-8 escaped]: https://github.com/devinus/poison/blob/4.0.1/bench/data/utf-8-escaped.json
[UTF-8 unescaped]: https://github.com/devinus/poison/blob/4.0.1/bench/data/utf-8-unescaped.json

### Decoding (Unit: IPS=inputs per second)

| Input data \ Library       | Jason      | jiffy       | JSON   | jsone      | JSX    | Poison | Tiny   |
|----------------------------|-----------:|------------:|-------:|-----------:|-------:|-------:|-------:|
| [Blockchain]               | **2.75 K** |      2.62 K | 0.35 K | 2.21 K (3) | 0.89 K | 1.32 K | 1.49 K |
| [Giphy]                    |     212.18 |  **243.45** |  35.67 | 109.11 (5) |  64.32 | 110.76 | 114.54 |
| [GitHub]                   |     973.41 | **1052.94** | 137.02 | 662.39 (3) | 271.97 | 438.79 | 542.67 |
| [GovTrack]                 |  **10.77** |        8.32 |   0.80 |   5.08 (3) |   2.81 |   3.58 |   3.65 |
| [Issue 90]                 |      17.85 |   **41.16** |   0.88 |  10.79 (5) |   6.02 |  13.63 |  14.03 |
| [JSON Generateor]          | **320.79** |      243.93 |  25.16 | 184.23 (3) | 111.24 | 135.47 | 139.78 |
| [JSON Generateor (Pretty)] | **273.57** |      205.09 |  25.04 | 158.82 (3) |  97.93 | 123.31 | 136.65 |
| [Pokedex]                  | **527.63** |      285.43 |  33.70 | 245.36 (3) | 140.90 | 172.45 | 152.59 |
| [UTF-8 escaped]            |    1224.48 | **7923.08** | 326.43 | 573.70 (4) | 550.36 | 918.21 | 520.31 |
| [UTF-8 unescaped]          |     5.56 K | **12.54 K** | 1.35 K | 5.09 K (3) | 3.30 K | 4.39 K | 1.46 K |


License
-------

This library is released under the MIT License.

See the [COPYING](COPYING) file for full license information.
