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

-compile([native, {hipe, [o3]}]).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([encode/1]).

%%--------------------------------------------------------------------------------
%% Macros & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).
-define(IS_REDUNDANT_UTF8(B1, B2, FirstBitN), (B1 =:= 0 andalso B2 < (1 bsl (FirstBitN + 1)))).
-define(HEX(N, I), (binary:at(<<"0123456789abcdef">>, (N bsr (I * 4)) band 2#1111))).
-define(UNICODE_TO_HEX(Code), ?HEX(Code, 3), ?HEX(Code, 2), ?HEX(Code, 1), ?HEX(Code, 0)).

-type encode_result() :: {ok, binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
-type next() :: {array_values, [jsone:json_value()]}
              | {object_value, jsone:json_value(), jsone:json_object_members()}
              | {object_members, jsone:json_object_members()}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc Encodes an erlang term into json text (a utf8 encoded binary)
-spec encode(jsone:json_value()) -> encode_result().
encode(Value) ->
    value(Value, [], <<"">>).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next([next()], binary()) -> encode_result().
next([], Buf)             -> {ok, Buf};
next([Next | Nexts], Buf) ->
    case Next of
        {array_values, Values} ->
            case Values of
                [] -> array_values(Values, Nexts, Buf);
                _  -> array_values(Values, Nexts, <<Buf/binary, $,>>)
            end;
        {object_value, Value, Members} ->
            object_value(Value, Members, Nexts, Buf);
        {object_members, Members} ->
            case Members of
                [] -> object_members(Members, Nexts, Buf);
                _  -> object_members(Members, Nexts, <<Buf/binary, $,>>)
            end
    end.

-spec value(jsone:json_value(), [next()], binary()) -> encode_result().
value(null, Nexts, Buf)                         -> next(Nexts, <<Buf/binary, "null">>);
value(false, Nexts, Buf)                        -> next(Nexts, <<Buf/binary, "false">>);
value(true, Nexts, Buf)                         -> next(Nexts, <<Buf/binary, "true">>);
value(Value, Nexts, Buf) when is_integer(Value) -> next(Nexts, <<Buf/binary, (integer_to_binary(Value))/binary>>);
value(Value, Nexts, Buf) when is_float(Value)   -> next(Nexts, <<Buf/binary, (float_to_binary(Value))/binary>>);
value(Value, Nexts, Buf) when is_binary(Value)  -> string(Value, Nexts, Buf);
value(Value, Nexts, Buf) when is_list(Value)    -> array(Value, Nexts, Buf);
value({_} = Value, Nexts, Buf)                  -> object(Value, Nexts, Buf);
value(Value, Nexts, Buf)                        -> ?ERROR(value, [Value, Nexts, Buf]).

-spec string(jsone:json_string(), [next()], binary()) -> encode_result().
string(<<Str/binary>>, Nexts, Buf) ->
    escape_string(Str, Nexts, <<Buf/binary, $">>).

-spec escape_string(binary(), [next()], binary()) -> encode_result().
escape_string(<<"">>,                   Nexts, Buf) -> next(Nexts, <<Buf/binary, $">>);
escape_string(<<$", Str/binary>>,       Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $">>);
escape_string(<<$\/, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\/>>);
escape_string(<<$\\, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\\>>);
escape_string(<<$\b, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $b>>);
escape_string(<<$\f, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $f>>);
escape_string(<<$\n, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $n>>);
escape_string(<<$\r, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $r>>);
escape_string(<<$\t, Str/binary>>,      Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $t>>);
escape_string(<<0:1, C:7, Str/binary>>, Nexts, Buf) -> escape_string(Str, Nexts, <<Buf/binary, C>>);
escape_string(<<2#110:3, B1:5, 2#10:2, B2:6, Str/binary>>, Nexts, Buf) when not ?IS_REDUNDANT_UTF8(B1, B2, 5) ->
    Unicode = (B1 bsl 6) + B2,
    escape_unicode_char(Str, Unicode, Nexts, Buf);
escape_string(<<2#1110:4, B1:4, 2#10:2, B2:6, 2#10:2, B3:6, Str/binary>>, Nexts, Buf) when not ?IS_REDUNDANT_UTF8(B1, B2, 4) ->
    Unicode = (B1 bsl 12) + (B2 bsl 6) + B3,
    escape_unicode_char(Str, Unicode, Nexts, Buf);
escape_string(<<2#11110:5, B1:3, 2#10:2, B2:6, 2#10:2, B3:6, 2#10:2, B4:6, Str/binary>>, Nexts, Buf) when not ?IS_REDUNDANT_UTF8(B1, B2, 3) ->
    Unicode = (B1 bsl 18) + (B2 bsl 12) + (B3 bsl 6) + B4,
    escape_unicode_char(Str, Unicode, Nexts, Buf);
escape_string(Str, Nexts, Buf) ->
    ?ERROR(escape_string, [Str, Nexts, Buf]).

-spec escape_unicode_char(binary(), char(), [next()], binary()) -> encode_result().
escape_unicode_char(<<Str/binary>>, Unicode, Nexts, Buf) when Unicode =< 16#FFFF ->
    escape_string(Str, Nexts, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(Unicode)>>);
escape_unicode_char(<<Str/binary>>, Unicode, Nexts, Buf) ->
    %% Surrogate Pair
    <<High:10, Low:10>> = <<(Unicode - 16#10000):20>>, % XXX: inefficient
    escape_string(Str, Nexts, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(High + 16#D800), $\\, $u, ?UNICODE_TO_HEX(Low + 16#DC00)>>).

-spec array(jsone:json_array(), [next()], binary()) -> encode_result().
array(List, Nexts, Buf) ->
    array_values(List, Nexts, <<Buf/binary, $[>>).

-spec array_values(jsone:json_array(), [next()], binary()) -> encode_result().
array_values([],       Nexts, Buf) -> next(Nexts, <<Buf/binary, $]>>);
array_values([X | Xs], Nexts, Buf) -> value(X, [{array_values, Xs} | Nexts], Buf).

-spec object(jsone:json_object(), [next()], binary()) -> encode_result().
object({Members}, Nexts, Buf) ->
    object_members(Members, Nexts, <<Buf/binary, ${>>).

-spec object_members(jsone:json_object_members(), [next()], binary()) -> encode_result().
object_members([],                             Nexts, Buf) -> next(Nexts, <<Buf/binary, $}>>);
object_members([{<<Key/binary>>, Value} | Xs], Nexts, Buf) -> string(Key, [{object_value, Value, Xs} | Nexts], Buf);
object_members(Arg, Nexts, Buf)                            -> ?ERROR(object_members, [Arg, Nexts, Buf]).

-spec object_value(jsone:json_value(), jsone:json_object_members(), [next()], binary()) -> encode_result().
object_value(Value, Members, Nexts, Buf) ->
    value(Value, [{object_members, Members} | Nexts], <<Buf/binary, $:>>).
