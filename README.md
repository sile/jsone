jsone (0.2.3)
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
      - TODO: benchmark result
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
{[{<<"1">>,2}]}

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

> jsone:encode({[{<<"key">>, <<"value">>}]}).
<<"{\"key\":\"value\"}">>

% error: raises exception
> jsone:encode({[{key, <<"value">>}]}). % non binary key is not allowed
** exception error: bad argument
     in function  jsone_encode:object_members/3
        called as jsone_encode:object_members([{key,<<"value">>}],[],<<"{">>)
     in call from jsone:encode/1 (src/jsone.erl, line 97)

% error: returns {error, Reason}
> jsone:try_encode({[{key, <<"value">>}]}).
{error,{badarg,[{jsone_encode,object_members,
                              [[{key,<<"value">>}],[],<<"{">>],
                              [{line,138}]}]}}
```


Data Mapping (Erlangの<=> JSON)
-------------------------------

|         | Erlang                       | JSON            |
|:-------:|-----------------------------:|----------------:|
| number  |                          123 |             123 |
| null    |                         null |            null |
| boolean |                         true |            true |
| string  |                    <<"abc">> |           "abc" |
| array   |                      [1,2,3] |         [1,2,3] |
| object  | {[{<<"key">>, <<"value">>}]} | {"key":"value"} |


API
---
See [EDoc Document](doc/jsone.md)
