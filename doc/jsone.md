

# Module jsone #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


JSON decoding/encoding module.


<a name="types"></a>

## Data Types ##




### <a name="type-encode_option">encode_option()</a> ###



<pre><code>
encode_option() = native_utf8
</code></pre>



  native_utf8: Encodes UTF-8 characters as a human-readable(non-escaped) string



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
json_object() = {<a href="#type-json_object_members">json_object_members()</a>}
</code></pre>





### <a name="type-json_object_members">json_object_members()</a> ###



<pre><code>
json_object_members() = [{<a href="#type-json_string">json_string()</a>, <a href="#type-json_value">json_value()</a>}]
</code></pre>





### <a name="type-json_string">json_string()</a> ###



<pre><code>
json_string() = binary()
</code></pre>





### <a name="type-json_value">json_value()</a> ###



<pre><code>
json_value() = <a href="#type-json_number">json_number()</a> | <a href="#type-json_string">json_string()</a> | <a href="#type-json_array">json_array()</a> | <a href="#type-json_object">json_object()</a> | <a href="#type-json_boolean">json_boolean()</a> | null
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decodes an erlang term from json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Equivalent to <a href="#encode-2"><tt>encode(JsonValue, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td>Encodes an erlang term into json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#try_decode-1">try_decode/1</a></td><td>Decodes an erlang term from json text (a utf8 encoded binary).</td></tr><tr><td valign="top"><a href="#try_encode-1">try_encode/1</a></td><td>Equivalent to <a href="#try_encode-2"><tt>try_encode(JsonValue, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#try_encode-2">try_encode/2</a></td><td>Encodes an erlang term into json text (a utf8 encoded binary).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Json::binary()) -&gt; <a href="#type-json_value">json_value()</a>
</code></pre>

<br></br>



Decodes an erlang term from json text (a utf8 encoded binary)



Raises an error exception if input is not valid json



```
  > jsone:decode(<<"1">>).
  1
  > jsone:decode(<<"wrong json">>).
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

<br></br>


Equivalent to [`encode(JsonValue, [])`](#encode-2).
<a name="encode-2"></a>

### encode/2 ###


<pre><code>
encode(JsonValue::<a href="#type-json_value">json_value()</a>, Options::[<a href="#type-encode_option">encode_option()</a>]) -&gt; binary()
</code></pre>

<br></br>



Encodes an erlang term into json text (a utf8 encoded binary)



Raises an error exception if input is not an instance of type `json_value()`



```
  > jsone:encode([1, null, 2]).
  <<"[1,null,2]">>
  > jsone:encode([1, hoge, 2]).  % 'hoge' atom is not a json value
  ** exception error: bad argument
       in function  jsone_encode:value/3
          called as jsone_encode:value(hoge,[{array_values,[2]}],<<"[1,">>)
       in call from jsone:encode/1 (src/jsone.erl, line 97)
```

<a name="try_decode-1"></a>

### try_decode/1 ###


<pre><code>
try_decode(Json::binary()) -&gt; {ok, <a href="#type-json_value">json_value()</a>, Remainings::binary()} | {error, {Reason::term(), [<a href="erlang.md#type-stack_item">erlang:stack_item()</a>]}}
</code></pre>

<br></br>



Decodes an erlang term from json text (a utf8 encoded binary)



```
  > jsone:try_decode(<<"[1,2,3] \"next value\"">>).
  {ok,[1,2,3],<<" \"next value\"">>}
  > jsone:try_decode(<<"wrong json">>).
  {error,{badarg,[{jsone_decode,number_integer_part,
                                [<<"wrong json">>,1,[],<<>>],
                                [{line,208}]}]}}
```

<a name="try_encode-1"></a>

### try_encode/1 ###


<pre><code>
try_encode(JsonValue::<a href="#type-json_value">json_value()</a>) -&gt; {ok, binary()} | {error, {Reason::term(), [<a href="erlang.md#type-stack_item">erlang:stack_item()</a>]}}
</code></pre>

<br></br>


Equivalent to [`try_encode(JsonValue, [])`](#try_encode-2).
<a name="try_encode-2"></a>

### try_encode/2 ###


<pre><code>
try_encode(JsonValue::<a href="#type-json_value">json_value()</a>, Options::[<a href="#type-encode_option">encode_option()</a>]) -&gt; {ok, binary()} | {error, {Reason::term(), [<a href="erlang.md#type-stack_item">erlang:stack_item()</a>]}}
</code></pre>

<br></br>



Encodes an erlang term into json text (a utf8 encoded binary)



```
  > jsone:try_encode([1, null, 2]).
  {ok,<<"[1,null,2]">>}
  > jsone:try_encode([1, hoge, 2]).  % 'hoge' atom is not a json value
  {error,{badarg,[{jsone_encode,value,
                                [hoge,[{array_values,[2]}],<<"[1,">>],
                                [{line,86}]}]}}
```

