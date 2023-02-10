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
-export([decode/1, decode/2,
         try_decode/1, try_decode/2,
         encode/1, encode/2,
         try_encode/1, try_encode/2,
         term_to_json_string/1,
         ip_address_to_json_string/1]).

-export_type([json_value/0,
              json_boolean/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,
              json_object_members/0,
              json_term/0,
              json_object_format_tuple/0,
              json_object_format_proplist/0,
              json_object_format_map/0,
              json_scalar/0,

              encode_option/0,
              decode_option/0,
              float_format_option/0,
              datetime_encode_format/0,
              datetime_format/0,
              timezone/0,
              utc_offset_seconds/0,
              stack_item/0]).

%%--------------------------------------------------------------------------------
%% Types & Macros
%%--------------------------------------------------------------------------------
-type json_value() :: json_number() |
                      json_string() |
                      json_array() |
                      json_object() |
                      json_boolean() |
                      null |
                      undefined |
                      json_term().
-type json_boolean() :: boolean().
-type json_number() :: number().
-type json_string() :: binary() | atom() | calendar:datetime().  % NOTE: `decode/1' always returns `binary()' value
-type json_array() :: [json_value()].
-type json_object() :: json_object_format_tuple() | json_object_format_proplist() | json_object_format_map().
-type json_object_members() :: [{json_string(), json_value()}].
-type json_term() :: {{json, iolist()}} | {{json_utf8, unicode:chardata()}}.
%% `json_term()' allows inline already encoded JSON value. `json' variant
%% expects byte encoded utf8 data values as list members. `json_utf8' expect
%% Unicode code points as list members. Binaries are copied "as is" in both
%% variants except `json_utf8' will check if binary contain valid `UTF-8'
%% encoded data. In short, `json' uses `erlang:iolist_to_binary/1' and
%% `json_utf8' uses `unicode:chardata_to_binary/1' for encoding.
%%
%% A simple example is worth a thousand words.
%%
%% ```
%% 1> S = "hélo".
%% "hélo"
%% 2> shell:strings(false).
%% true
%% 3> S.
%% [104,233,108,111]
%% 4> B = jsone:encode({{json, S}}).  % invalid UTF-8
%% <<104,233,108,111>>
%% 5> B2 = jsone:encode({{json_utf8, S}}). % valid UTF-8
%% <<104,195,169,108,111>>
%% 6> jsone:encode({{json, B}}).
%% <<104,233,108,111>>
%% 7> jsone:encode({{json_utf8, B}}).
%% ** exception error: {invalid_json_utf8,<<104>>,<<233,108,111>>}
%%      in function  jsone_encode:value/4
%%         called as jsone_encode:value({json_utf8,<<104,233,108,111>>},
%%                                      [],<<>>,
%%                                      {encode_opt_v2,false,
%%                                                     [{scientific,20}],
%%                                                     {iso8601,0},
%%                                                     string,0,0})
%%      in call from jsone:encode/2 (/home/hynek/work/altworx/jsone/_build/default/lib/jsone/src/jsone.erl, line 302)
%% 8> jsone:encode({{json_utf8, B2}}).
%% <<104,195,169,108,111>>
%% 9> shell:strings(true).
%% false
%% 10> jsone:encode({{json_utf8, B2}}).
%% <<"hélo"/utf8>>
%% 11> jsone:encode({{json, binary_to_list(B2)}}). % UTF-8 encoded list leads to valid UTF-8
%% <<"hélo"/utf8>>
%% '''
%%
-type json_object_format_tuple() :: {json_object_members()}.
-type json_object_format_proplist() :: [{}] | json_object_members().

-ifdef('NO_MAP_TYPE').
-opaque json_object_format_map() :: json_object_format_proplist().
%% `maps' is not supported in this erts version
-else.
-type json_object_format_map() :: map().
-endif.

-type json_scalar() :: json_boolean() | json_number() | json_string().

-type float_format_option() :: {scientific, Decimals :: 0..249} | {decimals, Decimals :: 0..253} | compact | short.
%% `scientific': <br />
%% - The float will be formatted using scientific notation with `Decimals' digits of precision. <br />
%%
%% `decimals': <br />
%% - The encoded string will contain at most `Decimals' number of digits past the decimal point. <br />
%% - If `compact' is provided the trailing zeros at the end of the string are truncated. <br />
%% - If `short' is provided the float is formatted with the smallest number of digits. <br />
%%
%% For more details, see <a href="http://erlang.org/doc/man/erlang.html#float_to_list-2">erlang:float_to_list/2</a>.
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
%% '''

-type datetime_encode_format() :: Format :: datetime_format() | {Format :: datetime_format(), TimeZone :: timezone()}.
%% Datetime encoding format.
%%
%% The default value of `TimeZone' is `utc'.
%%
%% ```
%% %
%% % Universal Time
%% %
%% > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, iso8601}]).
%% <<"\"2000-03-10T10:03:58Z\"">>
%%
%% %
%% % Local Time (JST)
%% %
%% > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, {iso8601, local}}]).
%% <<"\"2000-03-10T10:03:58+09:00\"">>
%%
%% %
%% % Explicit TimeZone Offset
%% %
%% > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, {iso8601, -2*60*60}}]).
%% <<"\"2000-03-10T10:03:58-02:00\"">>
%% '''

-type datetime_format() :: iso8601.
-type timezone() :: utc | local | utc_offset_seconds().
-type utc_offset_seconds() :: -86399..86399.

-type common_option() :: undefined_as_null.
%%
%% `undefined_as_null': <br />
%% - Treats `undefined' in Erlang as the conversion target for `null' in JSON. This means that `undefined' will be encoded to `null' and `null' will be decoded to `undefined'<br />

-type encode_option() :: native_utf8 |
                         native_forward_slash |
                         canonical_form |
                         {float_format, [float_format_option()]} |
                         {datetime_format, datetime_encode_format()} |
                         {object_key_type, string | scalar | value} |
                         {space, non_neg_integer()} |
                         {indent, non_neg_integer()} |
                         {map_unknown_value, undefined | fun((term()) -> {ok, json_value()} | error)} |
                         skip_undefined |
                         common_option().
%% `native_utf8': <br />
%% - Encodes non ASCII UTF-8 characters as a human-readable(non-escaped) string <br />
%%
%% `native_forward_slash': <br />
%% - Prevents forward slashes in a JSON string from being escaped <br />
%%
%% `canonical_form': <br />
%% - produce a canonical form of a JSON document <br />
%%
%% `{float_format, Options}':
%% - Encodes a `float()` value in the format which specified by `Options' <br />
%% - default: `[{scientific, 20}]' <br />
%%
%% `{datetime_format, Format}`:
%% - Encodes a `calendar:datetime()` value in the format which specified by `Format' <br />
%% - default: `{iso8601, utc}' <br />
%%
%% `object_key_type':
%% - Allowable object key type <br />
%% - `string': Only string values are allowed (i.e. `json_string()' type) <br />
%% - `scalar': In addition to `string', following values are allowed: nulls, booleans, numerics (i.e. `json_scalar()' type) <br />
%% - `value': Any json compatible values are allowed (i.e. `json_value()' type) <br />
%% - default: `string' <br />
%% - NOTE: If `scalar' or `value' option is specified, non `json_string()' key will be automatically converted to a `binary()' value (e.g. `1' => `<<"1">>', `#{}' => `<<"{}">>') <br />
%%
%% `{space, N}': <br />
%% - Inserts `N' spaces after every comma and colon <br />
%% - default: `0' <br />
%%
%% `{indent, N}': <br />
%% - Inserts a newline and `N' spaces for each level of indentation <br />
%% - default: `0' <br />
%%
%% `skip_undefined': <br />
%% - If specified, each entry having `undefined' value in a object isn't included in the result JSON <br />
%%
%% `{map_unknown_value, Fun}`: <br />
%% - If `Fun' is a function, unknown values encountered during an encoding process are converted to `json_value()` by applying `Fun'. <br />
%% - If `Fun' is `undefined', the encoding results in an error if there are unknown values. <br />
%% - default: `term_to_json_string/1' <br />

-type decode_option() :: {object_format, tuple | proplist | map} |
                         {allow_ctrl_chars, boolean()} |
                         reject_invalid_utf8 |
                         {'keys', 'binary' | 'atom' | 'existing_atom' | 'attempt_atom'} |
                         {duplicate_map_keys, first | last} |
                         common_option().
%% `object_format': <br />
%% - Decoded JSON object format <br />
%% - `tuple': An object is decoded as `{[]}' if it is empty, otherwise `{[{Key, Value}]}'. <br />
%% - `proplist': An object is decoded as `[{}]' if it is empty, otherwise `[{Key, Value}]'. <br />
%% - `map': An object is decoded as `#{}' if it is empty, otherwise `#{Key => Value}'. <br />
%% - default: `map' if OTP version is OTP-17 or more, `tuple' otherwise <br />
%%
%% `allow_ctrl_chars': <br />
%% - If the value is `true', strings which contain unescaped control characters will be regarded as a legal JSON string <br />
%% - default: `false'<br />
%%
%% `reject_invalid_utf8': <br />
%% - Rejects JSON strings which contain invalid UTF-8 byte sequences <br />
%%
%% `keys': <br />
%% Defines way how object keys are decoded. The default value is `binary'.
%% The option is compatible with `labels' option in `jsx'. <br />
%% - `binary': The key is left as a string which is encoded as binary. It's default
%% and backward compatible behaviour. <br />
%% - `atom': The key is converted to an atom. Results in `badarg' if Key value
%% regarded as UTF-8 is not a valid atom. <br />
%% - `existing_atom': Returns existing atom. Any key value which is not
%% existing atom raises `badarg' exception. <br />
%% - `attempt_atom': Returns existing atom as `existing_atom' but returns a
%% binary string if fails find one.
%%
%% `duplicate_map_keys': <br />
%% https://www.ietf.org/rfc/rfc4627.txt says that keys SHOULD be
%% unique, but they don't have to be. Most JSON parsers will either
%% give you the value of the first, or last duplicate property
%% encountered. When `object_format' is `tuple' or `proplist' all
%% duplicates are returned. When `object_format' is `map' by default
%% the first instance of a duplicate is returned. Setting
%% `duplicate_map_keys' to `last' will change this behaviour to return
%% the last such instance.
%% - If the value is `first' then the first duplicate key/value is returned.  <br />
%% - If the value is `last' then the last duplicate key/value is returned.
%% - default: `first'<br />
%%

-type stack_item() :: {Module :: module(),
                       Function :: atom(),
                       Arity :: arity() | (Args :: [term()]),
                       Location :: [{file, Filename :: string()} | {line, Line :: pos_integer()}]}.
%% An item in a stack back-trace.
%%
%% Note that the `erlang' module already defines the same `stack_item/0' type,
%% but it is not exported from the module.
%% So, maybe as a temporary measure, we redefine this type for passing full dialyzer analysis.

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE,     __StackTrace).
-else.
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE,     erlang:get_stacktrace()).
-endif.


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
        {ok, Value, Remainings} = try_decode(Json, Options),
        check_decode_remainings(Remainings),
        Value
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ?CAPTURE_STACKTRACE->
            erlang:raise(error, Reason, [StackItem | ?GET_STACKTRACE])
    end.


%% @equiv try_decode(Json, [])
-spec try_decode(binary()) -> {ok, json_value(), Remainings :: binary()} | {error, {Reason :: term(), [stack_item()]}}.
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
-spec try_decode(binary(), [decode_option()]) ->
          {ok, json_value(), Remainings :: binary()} | {error, {Reason :: term(), [stack_item()]}}.
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
%% > jsone:encode([1, self(), 2]).  % A pid is not a json value
%% ** exception error: bad argument
%%      in function  jsone_encode:value/3
%%         called as jsone_encode:value(<0,34,0>,[{array_values,[2]}],<<"[1,">>)
%%      in call from jsone:encode/1 (src/jsone.erl, line 97)
%% '''
-spec encode(json_value(), [encode_option()]) -> binary().
encode(JsonValue, Options) ->
    try
        {ok, Binary} = try_encode(JsonValue, Options),
        Binary
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ?CAPTURE_STACKTRACE->
            erlang:raise(error, Reason, [StackItem | ?GET_STACKTRACE])
    end.


%% @equiv try_encode(JsonValue, [])
-spec try_encode(json_value()) -> {ok, binary()} | {error, {Reason :: term(), [stack_item()]}}.
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
-spec try_encode(json_value(), [encode_option()]) -> {ok, binary()} | {error, {Reason :: term(), [stack_item()]}}.
try_encode(JsonValue, Options) ->
    jsone_encode:encode(JsonValue, Options).


%% @doc Converts the given term `X' to its string representation (i.e., the result of `io_lib:format("~p", [X])').
-spec term_to_json_string(term()) -> {ok, json_string()} | error.
term_to_json_string(X) ->
    {ok, list_to_binary(io_lib:format("~0p", [X]))}.


%% @doc Convert an IP address into a text representation.
%%
%% This function can be specified as the value of the `map_unknown_value' encoding option.
%%
%% This function formats IPv6 addresses by following the recommendation defined in RFC 5952.
%% Note that the trailing 32 bytes of special IPv6 addresses such as IPv4-Compatible (::X.X.X.X),
%% IPv4-Mapped (::ffff:X.X.X.X), IPv4-Translated (::ffff:0:X.X.X.X) and IPv4/IPv6 translation
%% (64:ff9b::X.X.X.X and 64:ff9b:1::X.X.X.X ~ 64:ff9b:1:ffff:ffff:ffff:X.X.X.X) are formatted
%% using the IPv4 format.
%%
%% ```
%% > EncodeOpt = [{map_unknown_value, fun jsone:ip_address_to_json_string/1}].
%%
%% > jsone:encode(#{ip => {127, 0, 0, 1}}, EncodeOpt).
%% <<"{\"ip\":\"127.0.0.1\"}">>
%%
%% > {ok, Addr} = inet:parse_address("2001:DB8:0000:0000:0001:0000:0000:0001").
%% > jsone:encode(Addr, EncodeOpt).
%% <<"\"2001:db8::1:0:0:1\"">>
%%
%% > jsone:encode([foo, {0, 0, 0, 0, 0, 16#FFFF, 16#7F00, 16#0001}], EncodeOpt).
%% <<"[\"foo\",\"::ffff:127.0.0.1\"]">>
%% '''
-spec ip_address_to_json_string(inet:ip_address() | any()) -> {ok, json_string()} | error.
ip_address_to_json_string(X) ->
    jsone_inet:ip_address_to_json_string(X).


%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec check_decode_remainings(binary()) -> ok.
check_decode_remainings(<<>>) ->
    ok;
check_decode_remainings(<<$ , Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\t, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\r, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<$\n, Bin/binary>>) ->
    check_decode_remainings(Bin);
check_decode_remainings(<<Bin/binary>>) ->
    erlang:error(badarg, [Bin]).
