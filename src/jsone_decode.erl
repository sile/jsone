%%% @doc JSON decoding module
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
-module(jsone_decode).

-compile([native, {hipe, [o3]}]).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([decode/1]).

%%--------------------------------------------------------------------------------
%% Macros & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).

-type next() :: {array_next, [jsone:json_value()]}
              | {object_value, jsone:json_object_members()}
              | {object_next, jsone:json_string(), jsone:json_object_members()}.

-type whitespace_next() :: value
                         | array
                         | object
                         | {array_next, [jsone:json_value()]}
                         | {object_key, jsone:json_object_members()}
                         | {object_value, jsone:json_string(), jsone:json_object_members()}
                         | {object_next, jsone:json_object_members()}.

-type decode_result() :: {ok, jsone:json_value(), Rest::binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc JSONバイナリをデコードする.
-spec decode(binary()) -> decode_result().
decode(<<Json/binary>>) ->
    whitespace(Json, value, [], <<"">>).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next(binary(), jsone:json_value(), [next()], binary()) -> decode_result().
next(<<Bin/binary>>, Value, [], _Buf) ->
    {ok, Value, Bin};
next(<<Bin/binary>>, Value, [Next | Nexts], Buf) ->
    case Next of
        {array_next, Values}        -> whitespace(Bin, {array_next, [Value | Values]}, Nexts, Buf);
        {object_value, Members}     -> whitespace(Bin, {object_value, Value, Members}, Nexts, Buf);
        {object_next, Key, Members} -> whitespace(Bin, {object_next, [{Key, Value} | Members]}, Nexts, Buf)
    end.

-spec whitespace(binary(), whitespace_next(), [next()], binary()) -> decode_result().
whitespace(<<$  , Bin/binary>>, Next, Nexts, Buf) -> whitespace(Bin, Next, Nexts, Buf);
whitespace(<<$\t, Bin/binary>>, Next, Nexts, Buf) -> whitespace(Bin, Next, Nexts, Buf);
whitespace(<<$\r, Bin/binary>>, Next, Nexts, Buf) -> whitespace(Bin, Next, Nexts, Buf);
whitespace(<<$\n, Bin/binary>>, Next, Nexts, Buf) -> whitespace(Bin, Next, Nexts, Buf);
whitespace(<<Bin/binary>>,      Next, Nexts, Buf) ->
    case Next of
        value  -> value(Bin, Nexts, Buf);
        array  -> array(Bin, Nexts, Buf);
        object -> object(Bin, Nexts, Buf);
        {object_key, Members}        -> object_key(Bin, Members, Nexts, Buf);
        {array_next, Values}         -> array_next(Bin, Values, Nexts, Buf);
        {object_value, Key, Members} -> object_value(Bin, Key, Members, Nexts, Buf);
        {object_next, Members}       -> object_next(Bin, Members, Nexts, Buf)
    end.

-spec value(binary(), [next()], binary()) -> decode_result().
value(<<"false", Bin/binary>>, Nexts, Buf) -> next(Bin, false, Nexts, Buf);
value(<<"true", Bin/binary>>, Nexts, Buf)  -> next(Bin, true, Nexts, Buf);
value(<<"null", Bin/binary>>, Nexts, Buf)  -> next(Bin, null, Nexts, Buf);
value(<<$[, Bin/binary>>, Nexts, Buf)      -> whitespace(Bin, array, Nexts, Buf);
value(<<${, Bin/binary>>, Nexts, Buf)      -> whitespace(Bin, object, Nexts, Buf);
value(<<$", Bin/binary>>, Nexts, Buf)      -> string(Bin, byte_size(Buf), Nexts, Buf);
value(<<Bin/binary>>, Nexts, Buf)          -> number(Bin, Nexts, Buf).

-spec array(binary(), [next()], binary()) -> decode_result().
array(<<$], Bin/binary>>, Nexts, Buf) -> next(Bin, [], Nexts, Buf);
array(<<Bin/binary>>, Nexts, Buf)     -> value(Bin, [{array_next, []} | Nexts], Buf).

-spec array_next(binary(), [jsone:json_value()], [next()], binary()) -> decode_result().
array_next(<<$], Bin/binary>>, Values, Nexts, Buf) -> next(Bin, lists:reverse(Values), Nexts, Buf);
array_next(<<$,, Bin/binary>>, Values, Nexts, Buf) -> whitespace(Bin, value, [{array_next, Values} | Nexts], Buf);
array_next(Bin,                Values, Nexts, Buf) -> ?ERROR(array_next, [Bin, Values, Nexts, Buf]).

-spec object(binary(), [next()], binary()) -> decode_result().
object(<<$}, Bin/binary>>, Nexts, Buf) -> next(Bin, {[]}, Nexts, Buf);
object(<<Bin/binary>>, Nexts, Buf)     -> object_key(Bin, [], Nexts, Buf).

-spec object_key(binary(), jsone:json_object_members(), [next()], binary()) -> decode_result().
object_key(<<$", Bin/binary>>, Members, Nexts, Buf) -> string(Bin, byte_size(Buf), [{object_value, Members} | Nexts], Buf);
object_key(<<Bin/binary>>, Members, Nexts, Buf)     -> ?ERROR(object_key, [Bin, Members, Nexts, Buf]).

-spec object_value(binary(), jsone:json_string(), jsone:json_object_members(), [next()], binary()) -> decode_result().
object_value(<<$:, Bin/binary>>, Key, Members, Nexts, Buf) -> whitespace(Bin, value, [{object_next, Key, Members} | Nexts], Buf);
object_value(Bin,                Key, Members, Nexts, Buf) -> ?ERROR(object_value, [Bin, Key, Members, Nexts, Buf]).

-spec object_next(binary(), jsone:json_object_members(), [next()], binary()) -> decode_result().
object_next(<<$}, Bin/binary>>, Members, Nexts, Buf) -> next(Bin, {Members}, Nexts, Buf);
object_next(<<$,, Bin/binary>>, Members, Nexts, Buf) -> whitespace(Bin, {object_key, Members}, Nexts, Buf);
object_next(Bin,                Members, Nexts, Buf) -> ?ERROR(object_next, [Bin, Members, Nexts, Buf]).

-spec string(binary(), non_neg_integer(), [next()], binary()) -> decode_result().
string(<<Bin/binary>>, Start, Nexts, Buf) ->
    string(Bin, Bin, Start, Nexts, Buf).

-spec string(binary(), binary(), non_neg_integer(), [next()], binary()) -> decode_result().
string(<<$", Bin/binary>>, Base, Start, Nexts, Buf) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(Bin) - 1),
    case Start =:= byte_size(Buf) of
        true  -> next(Bin, Prefix, Nexts, Buf);
        false ->
            Buf2 = <<Buf/binary, Prefix/binary>>,
            next(Bin, binary:part(Buf2, Start, byte_size(Buf2) - Start), Nexts, Buf2)
    end;
string(<<$\\, B/binary>>, Base, Start, Nexts, Buf) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(B) - 1),
    case B of
        <<$", Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $">>);
        <<$/, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $/>>);
        <<$\\,Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\\>>);
        <<$b, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\b>>);
        <<$f, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\f>>);
        <<$n, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\n>>);
        <<$r, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\r>>);
        <<$t, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\t>>);
        <<$u, Bin/binary>> -> unicode_string(Bin, Start, Nexts, Buf);
        _                  -> ?ERROR(string, [<<$\\, B/binary>>, Base, Start, Nexts, Buf])
    end;
string(<<C, Bin/binary>>, Base, Start, Nexts, Buf) when 16#20 =< C ->
    string(Bin, Base, Start, Nexts, Buf).

-spec unicode_string(binary(), non_neg_integer(), [next()], binary()) -> decode_result().
unicode_string(<<N:4/binary, Bin/binary>>, Start, Nexts, Buf) ->
    case binary_to_integer(N, 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            %% サロゲートペア
            case Bin of
                <<$\\, $u, N2:4/binary, Bin2/binary>> ->
                    case binary_to_integer(N2, 16) of
                        Low when 16#DC00 =< Low, Low =< 16#DFFF ->
                            Unicode = 16#10000 + (High - 16#D800) * 16#400 + (Low - 16#DC00),
                            string(Bin2, Start, Nexts, unicode_to_utf8(Unicode, Buf));
                        _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf])
                    end;
                _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf])
            end;
        Unicode when 16#DC00 =< Unicode, Unicode =< 16#DFFF ->  % サロゲートペアの後半部分
            ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf]);
        Unicode -> 
            string(Bin, Start, Nexts, unicode_to_utf8(Unicode, Buf))
    end;
unicode_string(Bin, Start, Nexts, Buf) ->
    ?ERROR(unicode_string, [Bin, Start, Nexts, Buf]).

-spec unicode_to_utf8(0..1114111, binary()) -> binary().
unicode_to_utf8(Code, Buf) when Code < 16#80 ->
    <<Buf/binary, Code>>;
unicode_to_utf8(Code, Buf) when Code < 16#800 ->
    A = 2#11000000 bor (Code bsr 6),
    B = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B>>;
unicode_to_utf8(Code, Buf) when Code < 16#10000 ->
    A = 2#11100000 bor (Code bsr 12),
    B = 2#10000000 bor ((Code bsr 6) band 2#111111),
    C = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B, C>>;
unicode_to_utf8(Code, Buf) -> % NOTE: サロゲートペアの仕組み上、コード値が上限を越えることはないので、ここでの範囲チェックは不要
    A = 2#11110000 bor (Code bsr 18),
    B = 2#10000000 bor ((Code bsr 12) band 2#111111),
    C = 2#10000000 bor ((Code bsr  6) band 2#111111),
    D = 2#10000000 bor (Code band 2#111111),
    <<Buf/binary, A, B, C, D>>.

-spec number(binary(), [next()], binary()) -> decode_result().
number(<<$-, Bin/binary>>, Nexts, Buf) -> number_integer_part(Bin, -1, Nexts, Buf);
number(<<Bin/binary>>,     Nexts, Buf) -> number_integer_part(Bin,  1, Nexts, Buf).

-spec number_integer_part(binary(), 1|-1, [next()], binary()) -> decode_result().
number_integer_part(<<$0, Bin/binary>>, Sign, Nexts, Buf) ->
    number_fraction_part(Bin, Sign, 0, Nexts, Buf);
number_integer_part(<<C, Bin/binary>>, Sign, Nexts, Buf) when $1 =< C, C =< $9 ->
    number_integer_part_rest(Bin, C - $0, Sign, Nexts, Buf);
number_integer_part(Bin, Sign, Nexts, Buf) ->
    ?ERROR(number_integer_part, [Bin, Sign, Nexts, Buf]).

-spec number_integer_part_rest(binary(), non_neg_integer(), 1|-1, [next()], binary()) -> decode_result().
number_integer_part_rest(<<C, Bin/binary>>, N, Sign, Nexts, Buf) when $0 =< C, C =< $9 ->
    number_integer_part_rest(Bin, N * 10 + C - $0, Sign, Nexts, Buf);
number_integer_part_rest(<<Bin/binary>>, N, Sign, Nexts, Buf) ->
    number_fraction_part(Bin, Sign, N, Nexts, Buf).

-spec number_fraction_part(binary(), 1|-1, non_neg_integer(), [next()], binary()) -> decode_result().
number_fraction_part(<<$., Bin/binary>>, Sign, Int, Nexts, Buf) ->
    number_fraction_part_rest(Bin, Sign, Int, 0, Nexts, Buf);
number_fraction_part(<<Bin/binary>>, Sign, Int, Nexts, Buf) ->
    number_exponation_part(Bin, Sign * Int, 0, Nexts, Buf).

-spec number_fraction_part_rest(binary(), 1|-1, non_neg_integer(), non_neg_integer(), [next()], binary()) -> decode_result().
number_fraction_part_rest(<<C, Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf) when $0 =< C, C =< $9 ->
    number_fraction_part_rest(Bin, Sign, N * 10 + C - $0, DecimalOffset + 1, Nexts, Buf);
number_fraction_part_rest(<<Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf) when DecimalOffset > 0 ->
    number_exponation_part(Bin, Sign * N, DecimalOffset, Nexts, Buf);
number_fraction_part_rest(Bin, Sign, N, DecimalOffset, Nexts, Buf) ->
    ?ERROR(number_fraction_part_rest, [Bin, Sign, N, DecimalOffset, Nexts, Buf]).

-spec number_exponation_part(binary(), integer(), non_neg_integer(), [next()], binary()) -> decode_result().
number_exponation_part(<<$e, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf);
number_exponation_part(<<$E, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf);
number_exponation_part(<<$e, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf);
number_exponation_part(<<$E, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf);
number_exponation_part(<<$e, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf);
number_exponation_part(<<$E, Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, Nexts, Buf) ->
    case DecimalOffset of
        0 -> next(Bin, N, Nexts, Buf);
        _ -> next(Bin, N / math:pow(10, DecimalOffset), Nexts, Buf)
    end.

-spec number_exponation_part(binary(), integer(), non_neg_integer(), 1|-1, non_neg_integer(), boolean(), [next()], binary()) -> decode_result().
number_exponation_part(<<C, Bin/binary>>, N, DecimalOffset, ExpSign, Exp, _, Nexts, Buf) when $0 =< C, C =< $9 ->
    number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp * 10 + C - $0, false, Nexts, Buf);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, ExpSign, Exp, false, Nexts, Buf) ->
    Pos = ExpSign * Exp - DecimalOffset,
    next(Bin, N * math:pow(10, Pos), Nexts, Buf);
number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf) ->
    ?ERROR(number_exponation_part, [Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf]).
