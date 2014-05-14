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
%% @doc JSON値をバイナリ形式にエンコードする.
-spec encode(jsone:json_value()) -> binary().
encode(Value) ->
    value(Value, <<"">>).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec value(jsone:json_value(), binary()) -> binary().
value(null, <<Buf/binary>>)                         -> <<Buf/binary, "null">>;
value(false, <<Buf/binary>>)                        -> <<Buf/binary, "false">>;
value(true, <<Buf/binary>>)                         -> <<Buf/binary, "true">>;
value(Value, <<Buf/binary>>) when is_integer(Value) -> <<Buf/binary, (integer_to_binary(Value))/binary>>;
value(Value, <<Buf/binary>>) when is_float(Value)   -> <<Buf/binary, (float_to_binary(Value))/binary>>;
value(Value, <<Buf/binary>>) when is_binary(Value)  -> string(Value, Buf);
value(Value, <<Buf/binary>>) when is_list(Value)    -> array(Value, Buf);
value({object, _} = Value, <<Buf/binary>>)          -> object(Value, Buf);
value(Value, <<_Buf/binary>>)                       -> error(badarg, [Value]).

-spec string(jsone:json_string(), binary()) -> binary().
string(<<Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $">>).

-spec escape_string(binary(), binary()) -> binary().
escape_string(<<"">>, <<Buf/binary>>) ->
    <<Buf/binary, $">>;
escape_string(<<$", Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $">>);
escape_string(<<$\/, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $\/>>);
escape_string(<<$\\, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $\\>>);
escape_string(<<$\b, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $b>>);
escape_string(<<$\f, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $f>>);
escape_string(<<$\n, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $n>>);
escape_string(<<$\r, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $r>>);
escape_string(<<$\t, Str/binary>>, <<Buf/binary>>) ->
    escape_string(Str, <<Buf/binary, $\\, $t>>);
escape_string(<<C, Str/binary>>, <<Buf/binary>>) when C < 16#80 ->
    escape_string(Str, <<Buf/binary, C>>);
escape_string(<<2#10:2, _:6, _/binary>> = Str, <<Buf/binary>>) ->
    error(badarg, [Str, Buf]);
escape_string(<<2#110:3, B1:5, 2#10:2, B2:6, Str/binary>> = Str0, <<Buf/binary>>) ->
    case is_redundant_utf8_byte(B1, B2, 5) of
        true  -> error(badarg, [Str0, Buf]);
        false -> 
            Unicode = (B1 bsl 6) + B2,
            escape_unicode_char(Str, Unicode, Buf)
    end;
escape_string(<<2#1110:4, B1:4, 2#10:2, B2:6, 2#10:2, B3:6, Str/binary>> = Str0, <<Buf/binary>>) ->
    case is_redundant_utf8_byte(B1, B2, 4) of
        true  -> error(badarg, [Str0, Buf]);
        false ->
            Unicode = (B1 bsl 12) + (B2 bsl 6) + B3,
            escape_unicode_char(Str, Unicode, Buf)
    end;
escape_string(<<2#11110:5, B1:3, 2#10:2, B2:6, 2#10:2, B3:6, 2#10:2, B4:6, Str/binary>> = Str0, <<Buf/binary>>) ->
    case is_redundant_utf8_byte(B1, B2, 3) of
        true  -> error(badarg, [Str0, Buf]);
        false ->
            Unicode = (B1 bsl 18) + (B2 bsl 12) + (B3 bsl 6) + B4,
            escape_unicode_char(Str, Unicode, Buf)
    end;    
escape_string(<<Str/binary>>, <<Buf/binary>>) ->
    error(badarg, [Str, Buf]).

-spec is_redundant_utf8_byte(byte(), byte(), 3..5) -> boolean().
is_redundant_utf8_byte(B1, B2, FirstBitN) ->
    B1 =:= 0 andalso B2 < (1 bsl (FirstBitN+1)).

-define(HEX(N, I), (binary:at(<<"0123456789abcdef">>, (N bsr (I * 4)) band 2#1111))).
-define(UNICODE_TO_HEX(Code), ?HEX(Code, 3), ?HEX(Code, 2), ?HEX(Code, 1), ?HEX(Code, 0)).

-spec escape_unicode_char(binary(), char(), binary()) -> binary().
escape_unicode_char(<<Str/binary>>, Unicode, <<Buf/binary>>) when Unicode =< 16#FFFF ->
    escape_string(Str, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(Unicode)>>);
escape_unicode_char(<<Str/binary>>, Unicode, <<Buf/binary>>) ->
    %% サロゲートペア
    <<High:10, Low:10>> = <<(Unicode - 16#10000):20>>, % 非効率
    escape_string(Str, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(High + 16#D800), $\\, $u, ?UNICODE_TO_HEX(Low + 16#DC00)>>).

-spec array(jsone:json_array(), binary()) -> binary().
array(List, <<Buf/binary>>) ->
    array_values(List, <<Buf/binary, $[>>).

-spec array_values(jsone:json_array(), binary()) -> binary().
array_values([],       <<Buf/binary>>) -> <<Buf/binary, $]>>;
array_values([X | Xs], <<Buf/binary>>) ->
    Buf2 = value(X, Buf),
    case Xs of
        [] -> <<Buf2/binary, $]>>;
        _  -> array_values(Xs, <<Buf2/binary, $,>>)
    end.

-spec object(jsone:json_object(), binary()) -> binary().
object({object, Members}, <<Buf/binary>>) ->
    object_members(Members, <<Buf/binary, ${>>).

-spec object_members(jsone:json_object_members(), binary()) -> binary().
object_members([],                             <<Buf/binary>>) -> <<Buf/binary, $}>>;
object_members([{<<Key/binary>>, Value} | Xs], <<Buf/binary>>) ->
    Buf2 = string(Key, Buf),
    Buf3 = value(Value, <<Buf2/binary, $:>>),
    case Xs of
        [] -> <<Buf3/binary, $}>>;
        _  -> object_members(Xs, <<Buf3/binary, $,>>)
    end;
object_members(Arg, <<Buf/binary>>) ->
    error(badarg, [Arg, Buf]).
