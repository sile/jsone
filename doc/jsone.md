

# Module jsone #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


JSON decoding/encoding module.


<a name="types"></a>

## Data Types ##




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
json_object() = {object, <a href="#type-json_object_members">json_object_members()</a>}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>JSONバイナリをデコードする.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>JSON値をiodata形式にエンコードする.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Json::binary()) -&gt; {<a href="#type-json_value">json_value()</a>, RestJson::binary()}
</code></pre>

<br></br>



JSONバイナリをデコードする.


デコードに失敗した場合はエラーが送出される
<a name="encode-1"></a>

### encode/1 ###


<pre><code>
encode(JsonValue::<a href="#type-json_value">json_value()</a>) -&gt; iodata()
</code></pre>

<br></br>



JSON値をiodata形式にエンコードする.


エンコードに失敗した場合はエラーが送出される
