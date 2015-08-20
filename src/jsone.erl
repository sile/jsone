%%% @doc JSON decoding/encoding module
%%% @end
%%%
%%% Copyright (c) 2013-2015, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(jsone).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         decode/1, decode/2,
         try_decode/1, try_decode/2,
         encode/1, encode/2,
         try_encode/1, try_encode/2
        ]).

-export_type([
              json_value/0,
              json_boolean/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,
              json_object_members/0,
              json_object_format_tuple/0,
              json_object_format_proplist/0,
              json_object_format_map/0,

              encode_option/0,
              decode_option/0,
              float_format_option/0
             ]).

%%--------------------------------------------------------------------------------
%% Types & Macros
%%--------------------------------------------------------------------------------
-type json_value()          :: json_number() | json_string() | json_array() | json_object() | json_boolean() | null.
-type json_boolean()        :: boolean().
-type json_number()         :: number().
-type json_string()         :: binary() | atom(). % NOTE: `decode/1' always returns `binary()' value
-type json_array()          :: [json_value()].
-type json_object()         :: json_object_format_tuple()
                             | json_object_format_proplist()
                             | json_object_format_map().
-type json_object_members() :: [{json_string(), json_value()}].

-type json_object_format_map() :: map().
-type json_object_format_tuple() :: {json_object_members()}.
-type json_object_format_proplist() :: [{}] | json_object_members().

-type float_format_option() :: {scientific, Decimals :: 0..249}
                             | {decimals, Decimals :: 0..253}
                             | compact.
%% `scientific': <br />
%% - The float will be formatted using scientific notation with `Decimals' digits of precision. <br />
%%
%% `decimals': <br />
%% - The encoded string will contain at most `Decimals' number of digits past the decimal point. <br />
%% - If `compact' is provided the trailing zeros at the end of the string are truncated. <br />
%%
%% For more details, see <a href="http://erlang.org/doc/man/erlang.html#float_to_list-2">erlang:flaot_to_list/2</a>.
%%
%% ```
%% > jsone:encode(1.23).
%% <<"1.22999999999999998224e+00">>
%%
%% > jsone:encode(1.23, [{float_format, [{scientific, 4}]}]).
%% <"1.2300e+00">>
%%
%% > jsone:encode(1.23, [{float_format, [{scientific, 1}]}]).
%% <<"1.2e+00">>
%%
%% > jsone:encode(1.23, [{float_format, [{decimals, 4}]}]).
%% <<"1.2300">>
%%
%% > jsone:encode(1.23, [{float_format, [{decimals, 4}, compact]}]).
%% <<"1.23">>
%%'''

-type encode_option() :: native_utf8
                       | {float_format, [float_format_option()]}
                       | {space, non_neg_integer()}
                       | {indent, non_neg_integer()}.
%% `native_utf8': <br />
%% - Encodes UTF-8 characters as a human-readable(non-escaped) string <br />
%%
%% `{float_format, Optoins}`:
%% - Encodes a `float()` value in the format which specified by `Options' <br />
%% - default: `[{scientific, 20}]' <br />
%%
%% `{space, N}': <br />
%% - Inserts `N' spaces after every commna and colon <br />
%% - default: `0' <br />
%%
%% `{indent, N}': <br />
%% - Inserts a newline and `N' spaces for each level of indentation <br />
%% - default: `0' <br />

-type decode_option() :: {object_format, tuple | proplist | map}.
%% object_format: <br />
%%  - Decoded JSON object format <br />
%%  - `tuple': An object is decoded as `{[]}' if it is empty, otherwise `{[{Key, Value}]}'. <br />
%%  - `proplist': An object is decoded as `[{}]' if it is empty, otherwise `[{Key, Value}]'. <br />
%%  - `map': An object is decoded as `#{}' if it is empty, otherwise `#{Key => Value}'. <br />
%%  - default: `map' <br />

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @equiv decode(Json, [])
-spec decode(binary()) -> json_value().
decode(Json) ->
    decode(Json, []).

%% @doc Decodes an erlang term from json text (a utf8 encoded binary)
%%
%% Raises an error exception if input is not valid json
%%
%% ```
%% > jsone:decode(<<"1">>, []).
%% 1
%%
%% > jsone:decode(<<"wrong json">>, []).
%% ** exception error: bad argument
%%     in function  jsone_decode:number_integer_part/4
%%        called as jsone_decode:number_integer_part(<<"wrong json">>,1,[],<<>>)
%%     in call from jsone:decode/1 (src/jsone.erl, line 71)
%% '''
-spec decode(binary(), [decode_option()]) -> json_value().
decode(Json, Options) ->
    try
        {ok, Value, _} = try_decode(Json, Options),
        Value
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ->
            erlang:raise(error, Reason, [StackItem | erlang:get_stacktrace()])
    end.

%% @equiv try_decode(Json, [])
-spec try_decode(binary()) -> {ok, json_value(), Remainings::binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
try_decode(Json) ->
    try_decode(Json, []).

%% @doc Decodes an erlang term from json text (a utf8 encoded binary)
%%
%% ```
%% > jsone:try_decode(<<"[1,2,3] \"next value\"">>, []).
%% {ok,[1,2,3],<<" \"next value\"">>}
%%
%% > jsone:try_decode(<<"wrong json">>, []).
%% {error,{badarg,[{jsone_decode,number_integer_part,
%%                               [<<"wrong json">>,1,[],<<>>],
%%                               [{line,208}]}]}}
%% '''
-spec try_decode(binary(), [decode_option()]) -> {ok, json_value(), Remainings::binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
try_decode(Json, Options) ->
    jsone_decode:decode(Json, Options).

%% @equiv encode(JsonValue, [])
-spec encode(json_value()) -> binary().
encode(JsonValue) ->
    encode(JsonValue, []).

%% @doc Encodes an erlang term into json text (a utf8 encoded binary)
%%
%% Raises an error exception if input is not an instance of type `json_value()'
%%
%% ```
%% > jsone:encode([1, null, 2]).
%% <<"[1,null,2]">>
%%
%% > jsone:encode([1, hoge, 2]).  % 'hoge' atom is not a json value
%% ** exception error: bad argument
%%      in function  jsone_encode:value/3
%%         called as jsone_encode:value(hoge,[{array_values,[2]}],<<"[1,">>)
%%      in call from jsone:encode/1 (src/jsone.erl, line 97)
%% '''
-spec encode(json_value(), [encode_option()]) -> binary().
encode(JsonValue, Options) ->
    try
        {ok, Binary} = try_encode(JsonValue, Options),
        Binary
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ->
            erlang:raise(error, Reason, [StackItem | erlang:get_stacktrace()])
    end.

%% @equiv try_encode(JsonValue, [])
-spec try_encode(json_value()) -> {ok, binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
try_encode(JsonValue) ->
    try_encode(JsonValue, []).

%% @doc Encodes an erlang term into json text (a utf8 encoded binary)
%%
%% ```
%% > jsone:try_encode([1, null, 2]).
%% {ok,<<"[1,null,2]">>}
%%
%% > jsone:try_encode([1, hoge, 2]).  % 'hoge' atom is not a json value
%% {error,{badarg,[{jsone_encode,value,
%%                               [hoge,[{array_values,[2]}],<<"[1,">>],
%%                               [{line,86}]}]}}
%% '''
-spec try_encode(json_value(), [encode_option()]) -> {ok, binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
try_encode(JsonValue, Options) ->
    jsone_encode:encode(JsonValue, Options).
