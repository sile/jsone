%%% @doc JSON encoding module
%%% @private
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
-module(jsone_encode).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([encode/1, encode/2]).

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).
-define(IS_REDUNDANT_UTF8(B1, B2, FirstBitN), (B1 =:= 0 andalso B2 < (1 bsl (FirstBitN + 1)))).
-define(HEX(N, I), (binary:at(<<"0123456789abcdef">>, (N bsr (I * 4)) band 2#1111))).
-define(UNICODE_TO_HEX(Code), ?HEX(Code, 3), ?HEX(Code, 2), ?HEX(Code, 1), ?HEX(Code, 0)).
-define(IS_STR(X), is_binary(X); is_atom(X)).

-type encode_result() :: {ok, binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
-type next() :: {array_values, [jsone:json_value()]}
              | {object_value, jsone:json_value(), jsone:json_object_members()}
              | {object_members, jsone:json_object_members()}.

-record(encode_opt_v1, { native_utf8 = false :: boolean() }).
-define(OPT, #encode_opt_v1).
-type opt() :: #encode_opt_v1{}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec encode(jsone:json_value()) -> encode_result().
encode(Value) ->
    encode(Value, []).

-spec encode(jsone:json_value(), [jsone:encode_option()]) -> encode_result().
encode(Value, Options) ->
    Opt = parse_options(Options),
    value(Value, [], <<"">>, Opt).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next([next()], binary(), opt()) -> encode_result().
next([], Buf, _)             -> {ok, Buf};
next([Next | Nexts], Buf, Opt) ->
    case Next of
        {array_values, Values} ->
            case Values of
                [] -> array_values(Values, Nexts, Buf, Opt);
                _  -> array_values(Values, Nexts, <<Buf/binary, $,>>, Opt)
            end;
        {object_value, Value, Members} ->
            object_value(Value, Members, Nexts, Buf, Opt);
        {object_members, Members} ->
            case Members of
                [] -> object_members(Members, Nexts, Buf, Opt);
                _  -> object_members(Members, Nexts, <<Buf/binary, $,>>, Opt)
            end
    end.

-spec value(jsone:json_value(), [next()], binary(), opt()) -> encode_result().
value(null, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "null">>, Opt);
value(false, Nexts, Buf, Opt)                        -> next(Nexts, <<Buf/binary, "false">>, Opt);
value(true, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "true">>, Opt);
value(Value, Nexts, Buf, Opt) when is_integer(Value) -> next(Nexts, <<Buf/binary, (integer_to_binary(Value))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when is_float(Value)   -> next(Nexts, <<Buf/binary, (float_to_binary(Value))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when ?IS_STR(Value)    -> string(Value, Nexts, Buf, Opt);
value({Value}, Nexts, Buf, Opt)                      -> object(Value, Nexts, Buf, Opt);
value([{}], Nexts, Buf, Opt)                         -> object([], Nexts, Buf, Opt);
value([{_, _}|_] = Value, Nexts, Buf, Opt)           -> object(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when is_map(Value)     -> object(maps:to_list(Value), Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when is_list(Value)    -> array(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, _)                          -> ?ERROR(value, [Value, Nexts, Buf]).

-spec string(jsone:json_string(), [next()], binary(), opt()) -> encode_result().
string(<<Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, $">>, Opt);
string(Str, Nexts, Buf, Opt) ->
    string(atom_to_binary(Str, utf8), Nexts, Buf, Opt).

-spec escape_string(binary(), [next()], binary(), opt()) -> encode_result().
escape_string(<<"">>,                   Nexts, Buf, Opt) -> next(Nexts, <<Buf/binary, $">>, Opt);
escape_string(<<$", Str/binary>>,       Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $">>, Opt);
escape_string(<<$\/, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\/>>, Opt);
escape_string(<<$\\, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\\>>, Opt);
escape_string(<<$\b, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $b>>, Opt);
escape_string(<<$\f, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $f>>, Opt);
escape_string(<<$\n, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $n>>, Opt);
escape_string(<<$\r, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $r>>, Opt);
escape_string(<<$\t, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $t>>, Opt);
escape_string(<<0:1, C:7, Str/binary>>, Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, C>>, Opt);
escape_string(<<2#110:3, B1:5, 2#10:2, B2:6, Str/binary>>, Nexts, Buf, Opt) when not ?IS_REDUNDANT_UTF8(B1, B2, 5) ->
    case Opt?OPT.native_utf8 of
        false ->
            Unicode = (B1 bsl 6) + B2,
            escape_unicode_char(Str, Unicode, Nexts, Buf, Opt);
        true ->
            unicode_char(Str, <<2#110:3, B1:5, 2#10:2, B2:6>>, Nexts, Buf, Opt)
    end;
escape_string(<<2#1110:4, B1:4, 2#10:2, B2:6, 2#10:2, B3:6, Str/binary>>, Nexts, Buf, Opt) when not ?IS_REDUNDANT_UTF8(B1, B2, 4) ->
    case Opt?OPT.native_utf8 of
        false ->
            Unicode = (B1 bsl 12) + (B2 bsl 6) + B3,
            escape_unicode_char(Str, Unicode, Nexts, Buf, Opt);
        true ->
            unicode_char(Str, <<2#1110:4, B1:4, 2#10:2, B2:6, 2#10:2, B3:6>>, Nexts, Buf, Opt)
    end;
escape_string(<<2#11110:5, B1:3, 2#10:2, B2:6, 2#10:2, B3:6, 2#10:2, B4:6, Str/binary>>, Nexts, Buf, Opt) when not ?IS_REDUNDANT_UTF8(B1, B2, 3) ->
    case Opt?OPT.native_utf8 of
        false ->
            Unicode = (B1 bsl 18) + (B2 bsl 12) + (B3 bsl 6) + B4,
            escape_unicode_char(Str, Unicode, Nexts, Buf, Opt);
        true ->
            unicode_char(Str, <<2#11000:5, B1:3, 2#10:2, B2:6, 2#10:2, B3:6, 2#10:2, B4:6>>, Nexts, Buf, Opt)
    end;
escape_string(Str, Nexts, Buf, _) ->
    ?ERROR(escape_string, [Str, Nexts, Buf]).

unicode_char(Str, Char, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, Char/binary>>, Opt).

-spec escape_unicode_char(binary(), char(), [next()], binary(), opt()) -> encode_result().
escape_unicode_char(<<Str/binary>>, Unicode, Nexts, Buf, Opt) when Unicode =< 16#FFFF ->
    escape_string(Str, Nexts, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(Unicode)>>, Opt);
escape_unicode_char(<<Str/binary>>, Unicode, Nexts, Buf, Opt) ->
    %% Surrogate Pair
    <<High:10, Low:10>> = <<(Unicode - 16#10000):20>>, % XXX: inefficient
    escape_string(Str, Nexts, <<Buf/binary, $\\, $u, ?UNICODE_TO_HEX(High + 16#D800), $\\, $u, ?UNICODE_TO_HEX(Low + 16#DC00)>>, Opt).

-spec array(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array(List, Nexts, Buf, Opt) ->
    array_values(List, Nexts, <<Buf/binary, $[>>, Opt).

-spec array_values(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array_values([],       Nexts, Buf, Opt) -> next(Nexts, <<Buf/binary, $]>>, Opt);
array_values([X | Xs], Nexts, Buf, Opt) -> value(X, [{array_values, Xs} | Nexts], Buf, Opt).

-spec object(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object(Members, Nexts, Buf, Opt) ->
    object_members(Members, Nexts, <<Buf/binary, ${>>, Opt).

-spec object_members(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_members([],                             Nexts, Buf, Opt)        -> next(Nexts, <<Buf/binary, $}>>, Opt);
object_members([{Key, Value} | Xs], Nexts, Buf, Opt) when ?IS_STR(Key) -> string(Key, [{object_value, Value, Xs} | Nexts], Buf, Opt);
object_members(Arg, Nexts, Buf, _)                                     -> ?ERROR(object_members, [Arg, Nexts, Buf]).

-spec object_value(jsone:json_value(), jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_value(Value, Members, Nexts, Buf, Opt) ->
    value(Value, [{object_members, Members} | Nexts], <<Buf/binary, $:>>, Opt).

-spec parse_options([jsone:encode_option()]) -> opt().
parse_options(Options) ->
    parse_option(Options, ?OPT{}).

parse_option([], Opt) -> Opt;
parse_option([native_utf8|T], Opt) ->
    parse_option(T, Opt?OPT{native_utf8=true}).
