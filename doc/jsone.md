

# Module jsone #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

JSON decoding/encoding module.

<a name="types"></a>

## Data Types ##




### <a name="type-common_option">common_option()</a> ###


<pre><code>
common_option() = undefined_as_null
</code></pre>

`undefined_as_null`: <br />
- Treats `undefined` in Erlang as the conversion target for `null` in JSON. This means that `undefined` will be encoded to `null` and `null` will be decoded to `undefined`<br />



### <a name="type-datetime_encode_format">datetime_encode_format()</a> ###


<pre><code>
datetime_encode_format() = <a href="#type-datetime_format">datetime_format()</a> | {Format::<a href="#type-datetime_format">datetime_format()</a>, TimeZone::<a href="#type-timezone">timezone()</a>}
</code></pre>

 Datetime encoding format.

The default value of `TimeZone` is `utc`.

```
  %
  % Universal Time
  %
  > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, iso8601}]).
  <<"\"2000-03-10T10:03:58Z\"">>
  %
  % Local Time (JST)
  %
  > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, {iso8601, local}}]).
  <<"\"2000-03-10T10:03:58+09:00\"">>
  %
  % Explicit TimeZone Offset
  %
  > jsone:encode({{2000, 3, 10}, {10, 3, 58}}, [{datetime_format, {iso8601, -2*60*60}}]).
  <<"\"2000-03-10T10:03:58-02:00\"">>
```



### <a name="type-datetime_format">datetime_format()</a> ###


<pre><code>
datetime_format() = iso8601
</code></pre>




### <a name="type-decode_option">decode_option()</a> ###


<pre><code>
decode_option() = {object_format, tuple | proplist | map} | {allow_ctrl_chars, boolean()} | reject_invalid_utf8 | {keys, binary | atom | existing_atom | attempt_atom} | {duplicate_map_keys, first | last} | stream | <a href="#type-common_option">common_option()</a>
</code></pre>

`object_format`: <br />
- Decoded JSON object format <br />
- `tuple`: An object is decoded as `{[]}` if it is empty, otherwise `{[{Key, Value}]}`. <br />
- `proplist`: An object is decoded as `[{}]` if it is empty, otherwise `[{Key, Value}]`. <br />
- `map`: An object is decoded as `#{}` if it is empty, otherwise `#{Key => Value}`. <br />
- default: `map` if OTP version is OTP-17 or more, `tuple` otherwise <br />

`allow_ctrl_chars`: <br />
- If the value is `true`, strings which contain unescaped control characters will be regarded as a legal JSON string <br />
- default: `false`<br />

`reject_invalid_utf8`: <br />
- Rejects JSON strings which contain invalid UTF-8 byte sequences <br />

`keys`: <br />
Defines way how object keys are decoded. The default value is `binary`.
The option is compatible with `labels` option in `jsx`. <br />
- `binary`: The key is left as a string which is encoded as binary. It's default
and backward compatible behaviour. <br />
- `atom`: The key is converted to an atom. Results in `badarg` if Key value
regarded as UTF-8 is not a valid atom. <br />
- `existing_atom`: Returns existing atom. Any key value which is not
existing atom raises `badarg` exception. <br />
- `attempt_atom`: Returns existing atom as `existing_atom` but returns a
binary string if fails find one.

`duplicate_map_keys`: <br />
https://www.ietf.org/rfc/rfc4627.txt says that keys SHOULD be
unique, but they don't have to be. Most JSON parsers will either
give you the value of the first, or last duplicate property
encountered. When `object_format` is `tuple` or `proplist` all
duplicates are returned. When `object_format` is `map` by default
the first instance of a duplicate is returned. Setting
`duplicate_map_keys` to `last` will change this behaviour to return
the last such instance.
- If the value is `first` then the first duplicate key/value is returned.<br />
- If the value is `last` then the last duplicate key/value is returned.
- default: `first`<br />

`stream`: <br />

Decode the input in multiple chunks. Instead of a result or error,
`{incomplete, fun()}` is returned. The returned fun takes a single argument
and it should called to continue the decoding. When all the input has been
provided, the fun should be called with `end_stream` or `end_json` to signal
the end of input and then the fun returns a result or an error.



### <a name="type-encode_option">encode_option()</a> ###


<pre><code>
encode_option() = native_utf8 | native_forward_slash | canonical_form | {float_format, [<a href="#type-float_format_option">float_format_option()</a>]} | {datetime_format, <a href="#type-datetime_encode_format">datetime_encode_format()</a>} | {object_key_type, string | scalar | value} | {space, non_neg_integer()} | {indent, non_neg_integer()} | {map_unknown_value, undefined | fun((term()) -&gt; {ok, <a href="#type-json_value">json_value()</a>} | error)} | skip_undefined | <a href="#type-common_option">common_option()</a>
</code></pre>

`native_utf8`: <br />
- Encodes non ASCII UTF-8 characters as a human-readable(non-escaped) string <br />

`native_forward_slash`: <br />
- Prevents forward slashes in a JSON string from being escaped <br />

`canonical_form`: <br />
- produce a canonical form of a JSON document <br />

`{float_format, Options}`:
- Encodes a `float()` value in the format which specified by `Options` <br />
- default: `[{scientific, 20}]` <br />

`{datetime_format, Format}`:
- Encodes a `calendar:datetime()` value in the format which specified by `Format` <br />
- default: `{iso8601, utc}` <br />

`object_key_type`:
- Allowable object key type <br />
- `string`: Only string values are allowed (i.e. `json_string()` type) <br />
- `scalar`: In addition to `string`, following values are allowed: nulls, booleans, numerics (i.e. `json_scalar()` type) <br />
- `value`: Any json compatible values are allowed (i.e. `json_value()` type) <br />
- default: `string` <br />
- NOTE: If `scalar` or `value` option is specified, non `json_string()` key will be automatically converted to a `binary()` value (e.g. `1` => `<<"1">>`, `#{}` => `<<"{}">>`) <br />

`{space, N}`: <br />
- Inserts `N` spaces after every comma and colon <br />
- default: `0` <br />

`{indent, N}`: <br />
- Inserts a newline and `N` spaces for each level of indentation <br />
- default: `0` <br />

`skip_undefined`: <br />
- If specified, each entry having `undefined` value in a object isn't included in the result JSON <br />

`{map_unknown_value, Fun}`: <br />
- If `Fun` is a function, unknown values encountered during an encoding process are converted to `json_value()` by applying `Fun`. <br />
- If `Fun` is `undefined`, the encoding results in an error if there are unknown values. <br />
- default: `term_to_json_string/1` <br />



### <a name="type-float_format_option">float_format_option()</a> ###


<pre><code>
float_format_option() = {scientific, Decimals::0..249} | {decimals, Decimals::0..253} | compact
</code></pre>

`scientific`: <br />
- The float will be formatted using scientific notation with `Decimals` digits of precision. <br />

`decimals`: <br />
- The encoded string will contain at most `Decimals` number of digits past the decimal point. <br />
- If `compact` is provided the trailing zeros at the end of the string are truncated. <br />

For more details, see [erlang:float_to_list/2](http://erlang.org/doc/man/erlang.md#float_to_list-2).

```
  > jsone:encode(1.23).
  <<"1.22999999999999998224e+00">>
  > jsone:encode(1.23, [{float_format, [{scientific, 4}]}]).
  <"1.2300e+00">>
  > jsone:encode(1.23, [{float_format, [{scientific, 1}]}]).
  <<"1.2e+00">>
  > jsone:encode(1.23, [{float_format, [{decimals, 4}]}]).
  <<"1.2300">>
  > jsone:encode(1.23, [{float_format, [{decimals, 4}, compact]}]).
  <<"1.23">>
```



### <a name="type-incomplete">incomplete()</a> ###


<pre><code>
incomplete() = {incomplete, function()}
</code></pre>




### <a name="type-json_array">json_array()</a> ###


<pre><code>
json_array() = [<a href="#type-json_value">json_value()</a>]
</code></pre>




### <a name="type-json_boolean">json_boolean()</a> ###


<pre><code>
json_boolean() = boolean()
</code></pre>




### <a name="type-json_number">json_number()</a> ###


<pre><code>
json_number() = number()
</code></pre>




### <a name="type-json_object">json_object()</a> ###


<pre><code>
json_object() = <a href="#type-json_object_format_tuple">json_object_format_tuple()</a> | <a href="#type-json_object_format_proplist">json_object_format_proplist()</a> | <a href="#type-json_object_format_map">json_object_format_map()</a>
</code></pre>




### <a name="type-json_object_format_map">json_object_format_map()</a> ###


<pre><code>
json_object_format_map() = map()
</code></pre>




### <a name="type-json_object_format_proplist">json_object_format_proplist()</a> ###


<pre><code>
json_object_format_proplist() = [{}] | <a href="#type-json_object_members">json_object_members()</a>
</code></pre>




### <a name="type-json_object_format_tuple">json_object_format_tuple()</a> ###


<pre><code>
json_object_format_tuple() = {<a href="#type-json_object_members">json_object_members()</a>}
</code></pre>




### <a name="type-json_object_members">json_object_members()</a> ###


<pre><code>
json_object_members() = [{<a href="#type-json_string">json_string()</a>, <a href="#type-json_value">json_value()</a>}]
</code></pre>




### <a name="type-json_scalar">json_scalar()</a> ###


<pre><code>
json_scalar() = <a href="#type-json_boolean">json_boolean()</a> | <a href="#type-json_number">json_number()</a> | <a href="#type-json_string">json_string()</a>
</code></pre>




### <a name="type-json_string">json_string()</a> ###


<pre><code>
json_string() = binary() | atom() | <a href="calendar.md#type-datetime">calendar:datetime()</a>
</code></pre>

NOTE: `decode/1` always returns `binary()` value



### <a name="type-json_term">json_term()</a> ###


<pre><code>
json_term() = {{json, iolist()}} | {{json_utf8, <a href="unicode.md#type-chardata">unicode:chardata()</a>}}
</code></pre>

`json_term()` allows inline already encoded JSON value. `json` variant
expects byte encoded utf8 data values as list members. `json_utf8` expect
Unicode code points as list members. Binaries are copied "as is" in both
variants except `json_utf8` will check if binary contain valid `UTF-8`
encoded data. In short, `json` uses `erlang:iolist_to_binary/1` and
`json_utf8` uses `unicode:chardata_to_binary/1` for encoding.

A simple example is worth a thousand words.

```
  1> S = "hélo".
  "hélo"
  2> shell:strings(false).
  true
  3> S.
  [104,233,108,111]
  4> B = jsone:encode({{json, S}}).  % invalid UTF-8
  <<104,233,108,111>>
  5> B2 = jsone:encode({{json_utf8, S}}). % valid UTF-8
  <<104,195,169,108,111>>
  6> jsone:encode({{json, B}}).
  <<104,233,108,111>>
  7> jsone:encode({{json_utf8, B}}).
  ** exception error: {invalid_json_utf8,<<104>>,<<233,108,111>>}
       in function  jsone_encode:value/4
          called as jsone_encode:value({json_utf8,<<104,233,108,111>>},
                                       [],<<>>,
                                       {encode_opt_v2,false,
                                                      [{scientific,20}],
                                                      {iso8601,0},
                                                      string,0,0})
       in call from jsone:encode/2 (/home/hynek/work/altworx/jsone/_build/default/lib/jsone/src/jsone.erl, line 302)
  8> jsone:encode({{json_utf8, B2}}).
  <<104,195,169,108,111>>
  9> shell:strings(true).
  false
  10> jsone:encode({{json_utf8, B2}}).
  <<"hélo"/utf8>>
  11> jsone:encode({{json, binary_to_list(B2)}}). % UTF-8 encoded list leads to valid UTF-8
  <<"hélo"/utf8>>
```




### <a name="type-json_value">json_value()</a> ###


<pre><code>
json_value() = <a href="#type-json_number">json_number()</a> | <a href="#type-json_string">json_string()</a> | <a href="#type-json_array">json_array()</a> | <a href="#type-json_object">json_object()</a> | <a href="#type-json_boolean">json_boolean()</a> | null | undefined | <a href="#type-json_term">json_term()</a>
</code></pre>




### <a name="type-stack_item">stack_item()</a> ###


<pre><code>
stack_item() = {Module::module(), Function::atom(), Arity::arity() | (Args::[term()]), Location::[{file, Filename::string()} | {line, Line::pos_integer()}]}
</code></pre>

 An item in a stack back-trace.

Note that the `erlang` module already defines the same `stack_item/0` type,
but it is not exported from the module.
So, maybe as a temporary measure, we redefine this type for passing full dialyzer analysis.



### <a name="type-timezone">timezone()</a> ###


<pre><code>
timezone() = utc | local | <a href="#type-utc_offset_seconds">utc_offset_seconds()</a>
</code></pre>




### <a name="type-utc_offset_seconds">utc_offset_seconds()</a> ###


<pre><code>
utc_offset_seconds() = -86399..86399
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Equivalent to <a href="#decode-2"><tt>decode(Json, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td>Decodes an erlang term from json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Equivalent to <a href="#encode-2"><tt>encode(JsonValue, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td>Encodes an erlang term into json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#ip_address_to_json_string-1">ip_address_to_json_string/1</a></td><td>Convert an IP address into a text representation.</td></tr><tr><td valign="top"><a href="#term_to_json_string-1">term_to_json_string/1</a></td><td>Converts the given term <code>X</code> to its string representation (i.e., the result of <code>io_lib:format("~p", [X])</code>).</td></tr><tr><td valign="top"><a href="#try_decode-1">try_decode/1</a></td><td>Equivalent to <a href="#try_decode-2"><tt>try_decode(Json, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#try_decode-2">try_decode/2</a></td><td>Decodes an erlang term from json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#try_encode-1">try_encode/1</a></td><td>Equivalent to <a href="#try_encode-2"><tt>try_encode(JsonValue, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#try_encode-2">try_encode/2</a></td><td>Encodes an erlang term into json text (a utf8 encoded binary).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Json::binary()) -&gt; <a href="#type-json_value">json_value()</a> | <a href="#type-incomplete">incomplete()</a>
</code></pre>
<br />

Equivalent to [`decode(Json, [])`](#decode-2).

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(Json::binary(), Options::[<a href="#type-decode_option">decode_option()</a>]) -&gt; <a href="#type-json_value">json_value()</a> | <a href="#type-incomplete">incomplete()</a>
</code></pre>
<br />

Decodes an erlang term from json text (a utf8 encoded binary)

Raises an error exception if input is not valid json

```
  > jsone:decode(<<"1">>, []).
  1
  > jsone:decode(<<"wrong json">>, []).
  ** exception error: bad argument
      in function  jsone_decode:number_integer_part/4
         called as jsone_decode:number_integer_part(<<"wrong json">>,1,[],<<>>)
      in call from jsone:decode/1 (src/jsone.erl, line 71)
```

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(JsonValue::<a href="#type-json_value">json_value()</a>) -&gt; binary()
</code></pre>
<br />

Equivalent to [`encode(JsonValue, [])`](#encode-2).

<a name="encode-2"></a>

### encode/2 ###

<pre><code>
encode(JsonValue::<a href="#type-json_value">json_value()</a>, Options::[<a href="#type-encode_option">encode_option()</a>]) -&gt; binary()
</code></pre>
<br />

Encodes an erlang term into json text (a utf8 encoded binary)

Raises an error exception if input is not an instance of type `json_value()`

```
  > jsone:encode([1, null, 2]).
  <<"[1,null,2]">>
  > jsone:encode([1, self(), 2]).  % A pid is not a json value
  ** exception error: bad argument
       in function  jsone_encode:value/3
          called as jsone_encode:value(<0,34,0>,[{array_values,[2]}],<<"[1,">>)
       in call from jsone:encode/1 (src/jsone.erl, line 97)
```

<a name="ip_address_to_json_string-1"></a>

### ip_address_to_json_string/1 ###

<pre><code>
ip_address_to_json_string(X::<a href="inet.md#type-ip_address">inet:ip_address()</a> | any()) -&gt; {ok, <a href="#type-json_string">json_string()</a>} | error
</code></pre>
<br />

Convert an IP address into a text representation.

This function can be specified as the value of the `map_unknown_value` encoding option.

This function formats IPv6 addresses by following the recommendation defined in RFC 5952.
Note that the trailing 32 bytes of special IPv6 addresses such as IPv4-Compatible (::X.X.X.X),
IPv4-Mapped (::ffff:X.X.X.X), IPv4-Translated (::ffff:0:X.X.X.X) and IPv4/IPv6 translation
(64:ff9b::X.X.X.X and 64:ff9b:1::X.X.X.X ~ 64:ff9b:1:ffff:ffff:ffff:X.X.X.X) are formatted
using the IPv4 format.

```
  > EncodeOpt = [{map_unknown_value, fun jsone:ip_address_to_json_string/1}].
  > jsone:encode(#{ip => {127, 0, 0, 1}}, EncodeOpt).
  <<"{\"ip\":\"127.0.0.1\"}">>
  > {ok, Addr} = inet:parse_address("2001:DB8:0000:0000:0001:0000:0000:0001").
  > jsone:encode(Addr, EncodeOpt).
  <<"\"2001:db8::1:0:0:1\"">>
  > jsone:encode([foo, {0, 0, 0, 0, 0, 16#FFFF, 16#7F00, 16#0001}], EncodeOpt).
  <<"[\"foo\",\"::ffff:127.0.0.1\"]">>
```

<a name="term_to_json_string-1"></a>

### term_to_json_string/1 ###

<pre><code>
term_to_json_string(X::term()) -&gt; {ok, <a href="#type-json_string">json_string()</a>} | error
</code></pre>
<br />

Converts the given term `X` to its string representation (i.e., the result of `io_lib:format("~p", [X])`).

<a name="try_decode-1"></a>

### try_decode/1 ###

<pre><code>
try_decode(Json::binary()) -&gt; {ok, <a href="#type-json_value">json_value()</a>, Remainings::binary()} | <a href="#type-incomplete">incomplete()</a> | {error, {Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}
</code></pre>
<br />

Equivalent to [`try_decode(Json, [])`](#try_decode-2).

<a name="try_decode-2"></a>

### try_decode/2 ###

<pre><code>
try_decode(Json::binary(), Options::[<a href="#type-decode_option">decode_option()</a>]) -&gt; {ok, <a href="#type-json_value">json_value()</a>, Remainings::binary()} | <a href="#type-incomplete">incomplete()</a> | {error, {Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}
</code></pre>
<br />

Decodes an erlang term from json text (a utf8 encoded binary)

```
  > jsone:try_decode(<<"[1,2,3] \"next value\"">>, []).
  {ok,[1,2,3],<<" \"next value\"">>}
  > jsone:try_decode(<<"wrong json">>, []).
  {error,{badarg,[{jsone_decode,number_integer_part,
                                [<<"wrong json">>,1,[],<<>>],
                                [{line,208}]}]}}
```

<a name="try_encode-1"></a>

### try_encode/1 ###

<pre><code>
try_encode(JsonValue::<a href="#type-json_value">json_value()</a>) -&gt; {ok, binary()} | {error, {Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}
</code></pre>
<br />

Equivalent to [`try_encode(JsonValue, [])`](#try_encode-2).

<a name="try_encode-2"></a>

### try_encode/2 ###

<pre><code>
try_encode(JsonValue::<a href="#type-json_value">json_value()</a>, Options::[<a href="#type-encode_option">encode_option()</a>]) -&gt; {ok, binary()} | {error, {Reason::term(), [<a href="#type-stack_item">stack_item()</a>]}}
</code></pre>
<br />

Encodes an erlang term into json text (a utf8 encoded binary)

```
  > jsone:try_encode([1, null, 2]).
  {ok,<<"[1,null,2]">>}
  > jsone:try_encode([1, hoge, 2]).  % 'hoge' atom is not a json value
  {error,{badarg,[{jsone_encode,value,
                                [hoge,[{array_values,[2]}],<<"[1,">>],
                                [{line,86}]}]}}
```

