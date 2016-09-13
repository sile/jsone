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
{json, IOList}       -> Value                  -> ~~~                        % UTF-8 encoded term
{json_utf8, Chars}   -> Value                  -> ~~~                        % Unicode code points
```

`{json, IOList} and {json_utf8, Chars} allows inline already encoded JSON
values. For example, you obtain JSON encoded data from database so you don't
have to decode it first and encode again. See [json_term()](doc/jsone.md#type-json_term).

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
| maps             |   7.23 μs/op |   10.64 μs/op (2) |   13.58 μs/op |   19.30 μs/op |   29.28 μs/op |
| lists            | 210.40 μs/op |  157.39 μs/op (3) |  109.30 μs/op |  201.82 μs/op |  357.25 μs/op |
| strings*         |  98.80 μs/op |  595.63 μs/op (5) |  416.78 μs/op |  399.89 μs/op |  262.18 μs/op |
| string escaping* | 144.01 μs/op |  732.44 μs/op (2) | 1318.82 μs/op | 1197.06 μs/op | 1324.04 μs/op |
| large value**    | 408.03 μs/op | 1556.85 μs/op (3) | 1447.71 μs/op | 1824.05 μs/op | 2184.59 μs/op |
| pretty print**   | 420.94 μs/op | 1686.55 μs/op (3) | 1534.74 μs/op | 2041.22 μs/op | 5533.04 μs/op |


__HiPE__:

|                  | jiffy        | jsone             | poison        | jazz          | jsx           |
|:-----------------|-------------:|------------------:|--------------:|--------------:|--------------:|
| maps             |   7.69 μs/op |    6.12 μs/op (1) |   12.32 μs/op |   22.90 μs/op |   27.03 μs/op |
| lists            | 207.75 μs/op |   69.93 μs/op (1) |   79.04 μs/op |  229.95 μs/op |  278.01 μs/op |
| strings*         |  96.67 μs/op |  321.69 μs/op (5) |  142.43 μs/op |  310.10 μs/op |  179.96 μs/op |
| string escaping* | 146.85 μs/op |  317.10 μs/op (2) | 1277.54 μs/op | 1311.85 μs/op |  767.67 μs/op |
| large value**    | 409.73 μs/op |  664.34 μs/op (2) |  806.24 μs/op | 1630.21 μs/op | 1777.62 μs/op |
| pretty print**   | 419.55 μs/op |  724.28 μs/op (2) |  844.76 μs/op | 1888.71 μs/op | 4872.34 μs/op |

\* binary representation of [UTF-8-demo.txt](https://github.com/devinus/poison/blob/2.1.0/bench/data/UTF-8-demo.txt)  <br />
\** [generated.json](https://github.com/devinus/poison/blob/2.1.0/bench/data/generated.json)

### ParserBench Result

__Non HiPE__:

|                    | jiffy        | jsone             | poison        | jsx           |
|:-------------------|-------------:|------------------:|--------------:|--------------:|
| json value*        | 544.84 μs/op | 1364.38 μs/op (2) | 1401.35 μs/op | 1844.55 μs/op |
| UTF-8 unescaping** |  63.01 μs/op |  399.38 μs/op (4) |  249.70 μs/op |  281.84 μs/op |


__HiPE__:

|                    | jiffy        | jsone             | poison        | jsx           |
|:-------------------|-------------:|------------------:|--------------:|--------------:|
| json value*        | 542.77 μs/op |  561.15 μs/op (2) |  751.36 μs/op | 1435.10 μs/op |
| UTF-8 unescaping** |  62.42 μs/op |   92.63 μs/op (2) |  118.97 μs/op |  172.07 μs/op |

\* [generated.json](https://github.com/devinus/poison/blob/2.1.0/bench/data/generated.json) <br />
\** [UTF-8-demo.txt](https://github.com/devinus/poison/blob/2.1.0/bench/data/UTF-8-demo.txt)

License
-------

This library is released under the MIT License.

See the [COPYING](COPYING) file for full license information.
