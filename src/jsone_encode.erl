%%% @doc JSON encoding module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013-2014, Takeru Ohta <phjgt308@gmail.com>
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
-module(jsone_encode).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([encode/1]).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc JSON値をiodata形式にエンコードする.
-spec encode(jsone:json_value()) -> iodata().
encode(null)                         -> <<"null">>;
encode(false)                        -> <<"false">>;
encode(true)                         -> <<"true">>;
encode(Value) when is_integer(Value) -> integer_to_binary(Value);
encode(Value) when is_float(Value)   -> float_to_binary(Value);
encode(Value) when is_binary(Value)  -> string(Value);
encode(Value) when is_list(Value)    -> array(Value);
encode({object, _} = Value)          -> object(Value);
encode(Value)                        -> error({invalid_json_value, Value}).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec string(jsone:json_string()) -> iodata().
string(Str) ->
    %% XXX: 手抜き実装 (一回変換を挟んで無駄)
    UnicodeStr = unicode:characters_to_list(Str),
    [$", escape_string(UnicodeStr, []), $"].

-spec escape_string(string(), iolist()) -> iodata().
escape_string([], Acc)         -> lists:reverse(Acc);
escape_string([$"  | Str], Acc) -> escape_string(Str, [$", $\\ | Acc]);
escape_string([$\/ | Str], Acc) -> escape_string(Str, [$/, $\\ | Acc]); 
escape_string([$\\ | Str], Acc) -> escape_string(Str, [$\\,$\\ | Acc]); 
escape_string([$\b | Str], Acc) -> escape_string(Str, [$b, $\\ | Acc]);
escape_string([$\f | Str], Acc) -> escape_string(Str, [$f, $\\ | Acc]); 
escape_string([$\n | Str], Acc) -> escape_string(Str, [$n, $\\ | Acc]);
escape_string([$\r | Str], Acc) -> escape_string(Str, [$r, $\\ | Acc]); 
escape_string([$\t | Str], Acc) -> escape_string(Str, [$t, $\\ | Acc]); 
escape_string([C   | Str], Acc) ->
    case C < 16#80 of
        true  -> escape_string(Str, [C | Acc]);
        false -> escape_string(Str, [escape_unicode_char(C) | Acc])
    end. 

%% NOTE: `Unicode'の値が適切であることは`unicode'モジュールが保証してくれていることを期待
-spec escape_unicode_char(char()) -> iodata().
escape_unicode_char(Unicode) when Unicode =< 16#FFFF ->
    io_lib:format("\\u~4.16.0b", [Unicode]);
escape_unicode_char(Unicode) ->
    %% サロゲートペア (非効率実装)
    <<High:10, Low:10>> = <<(Unicode - 16#10000):20>>,
    io_lib:format("\\u~4.16.0b\\u~4.16.0b", [High + 16#D800, Low + 16#DC00]).

-spec array(jsone:json_array()) -> iodata().
array(List) ->
    [$[, iodata_join(lists:map(fun encode/1, List), $,), $]].

-spec object(jsone:json_object()) -> iodata().
object({object, Members} = Object) ->
    [${,
     iodata_join([case is_binary(Key) of
                      false -> error({invalid_json_value, Object});
                      true  -> [string(Key), $:, encode(Value)]
                  end || {Key, Value} <- Members],
                 $,),
     $}].

-spec iodata_join([iodata()], char()) -> iodata().
iodata_join([], _Delimiter)           -> [];
iodata_join([Head | Tail], Delimiter) ->
    lists:foldl(fun (IoData, Acc) -> [Acc, Delimiter, IoData] end,
                Head,
                Tail).
