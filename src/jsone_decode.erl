%%% @doc JSON decoding module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
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

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([decode/1]).

%%--------------------------------------------------------------------------------
%% Macros & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Json), error({invalid_json, Json})).

-type next() :: {array_next, [jsone:json_value()]}
              | {object_value, jsone:json_object_members()}
              | {object_next, jsone:json_string(), jsone:json_object_members()}.

-type whitespace_next() :: value
                         | array
                         | object
                         | string
                         | {array_next, [jsone:json_value()]}
                         | {object_value, jsone:json_string(), jsone:json_object_members()}
                         | {object_next, jsone:json_string(), jsone:json_value(), jsone:json_object_members()}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc JSONテキストをデコードする.
%%
%% デコードに失敗した場合は`{invalid_json, 失敗位置より後のJSON::binary()}'形式のエラーが送出される.
-spec decode(binary()) -> {jsone:json_value(), RestJson::binary()}.
decode(<<Json/binary>>) ->
    whitespace(Json, value, []).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next(binary(), jsone:json_value(), [next()]) -> {jsone:json_value(), Rest::binary()}.
next(<<Bin/binary>>, Value, []) ->
    {Value, Bin};
next(<<Bin/binary>>, Value, [Next | Nexts]) ->
    case Next of
        {array_next, Values}        -> whitespace(Bin, {array_next, [Value | Values]}, Nexts);
        {object_value, Members}     -> whitespace(Bin, {object_value, Value, Members}, Nexts);
        {object_next, Key, Members} -> whitespace(Bin, {object_next, Key, Value, Members}, Nexts)
    end.

-spec whitespace(binary(), whitespace_next(), [next()]) -> {jsone:json_value(), Rest::binary()}.
whitespace(<<$  , Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\t, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\r, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\n, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<Bin/binary>>,      Next, Nexts) ->
    case Next of
        value  -> value(Bin, Nexts);
        array  -> array(Bin, Nexts);
        object -> object(Bin, Nexts);
        string -> case Bin of
                      <<$", Bin2/binary>> -> string(Bin2, [], Nexts);
                      _                   -> ?ERROR(Bin)
                  end;
        {array_next, Values}               -> array_next(Bin, Values, Nexts);
        {object_value, Key, Members}       -> object_value(Bin, Key, Members, Nexts);
        {object_next, Key, Value, Members} -> object_next(Bin, [{Key, Value} | Members], Nexts)
    end.

-spec value(binary(), [next()]) -> {jsone:json_value(), Rest::binary()}.
value(<<"false", Bin/binary>>, Nexts) -> next(Bin, false, Nexts);
value(<<"true", Bin/binary>>, Nexts)  -> next(Bin, true, Nexts);
value(<<"null", Bin/binary>>, Nexts)  -> next(Bin, null, Nexts);
value(<<$[, Bin/binary>>, Nexts)      -> whitespace(Bin, array, Nexts);
value(<<${, Bin/binary>>, Nexts)      -> whitespace(Bin, object, Nexts);
value(<<$", Bin/binary>>, Nexts)      -> string(Bin, [], Nexts);
value(<<Bin/binary>>, Nexts)          -> number(Bin, Nexts).

-spec array(binary(), [next()]) -> {jsone:json_value(), Rest::binary()}.
array(<<$], Bin/binary>>, Nexts) -> next(Bin, [], Nexts);
array(<<Bin/binary>>, Nexts)     -> whitespace(Bin, value, [{array_next, []} | Nexts]).

-spec array_next(binary(), [jsone:json_value()], [next()]) -> {jsone:json_value(), Rest::binary()}.
array_next(<<$], Bin/binary>>, Values, Nexts) -> next(Bin, lists:reverse(Values), Nexts);
array_next(<<$,, Bin/binary>>, Values, Nexts) -> whitespace(Bin, value, [{array_next, Values} | Nexts]);
array_next(<<Bin/binary>>, _Values, _Nexts)   -> ?ERROR(Bin).

-spec object(binary(), [next()]) -> {jsone:json_value(), Rest::binary()}.
object(<<$}, Bin/binary>>, Nexts) -> next(Bin, {object, []}, Nexts);
object(<<Bin/binary>>, Nexts) -> whitespace(Bin, string, [{object_value, []} | Nexts]).

-spec object_value(binary(), jsone:json_string(), jsone:json_object_members(), [next()]) -> {jsone:json_value(), Rest::binary()}.
object_value(<<$:, Bin/binary>>, Key, Members, Nexts) -> whitespace(Bin, value, [{object_next, Key, Members} | Nexts]);
object_value(<<Bin/binary>>, _Key, _Members, _Nexts)  -> ?ERROR(Bin).

-spec object_next(binary(), jsone:json_object_members(), [next()]) -> {jsone:json_value(), Rest::binary()}.
object_next(<<$}, Bin/binary>>, Members, Nexts) -> next(Bin, {object, lists:reverse(Members)}, Nexts);
object_next(<<$,, Bin/binary>>, Members, Nexts) -> whitespace(Bin, string, [{object_value, Members} | Nexts]);
object_next(<<Bin/binary>>, _Members, _Nexts)   -> ?ERROR(Bin).

-spec string(binary(), iolist(), [next()]) -> {jsone:json_value(), Rest::binary()}.
string(<<$", Bin/binary>>, Acc, Nexts) -> next(Bin, list_to_binary(lists:reverse(Acc)), Nexts);
string(<<$\\, B/binary>>,  Acc, Nexts) ->
    case B of
        <<$", Bin/binary>> -> string(Bin, [$" | Acc], Nexts);
        <<$/, Bin/binary>> -> string(Bin, [$/ | Acc], Nexts);
        <<$\\,Bin/binary>> -> string(Bin, [$\\| Acc], Nexts);
        <<$b, Bin/binary>> -> string(Bin, [$\b | Acc], Nexts);
        <<$f, Bin/binary>> -> string(Bin, [$\f | Acc], Nexts);
        <<$n, Bin/binary>> -> string(Bin, [$\n | Acc], Nexts);
        <<$r, Bin/binary>> -> string(Bin, [$\r | Acc], Nexts);
        <<$t, Bin/binary>> -> string(Bin, [$\t | Acc], Nexts);
        <<$u, Bin/binary>> -> unicode_string(Bin, Acc, Nexts);
        _                  -> ?ERROR(B)
    end;
string(<<C, Bin/binary>>, Acc, Nexts) when 16#20 =< C ->
    string(Bin, [C | Acc], Nexts).

-spec unicode_string(binary(), iolist(), [next()]) -> {jsone:json_value(), Rest::binary()}.
unicode_string(<<N:4/binary, Bin/binary>>, Acc, Nexts) ->
    case binary_to_integer(N, 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            %% サロゲートペア
            case Bin of
                <<$\\, $u, N2:4/binary, Bin2/binary>> ->
                    case binary_to_integer(N2, 16) of
                        Low when 16#DC00 =< Low, Low =< 16#DFFF ->
                            Unicode = 16#10000 + (High - 16#D800) * 16#400 + (Low - 16#DC00),
                            string(Bin2, unicode_to_utf8(Unicode, Acc), Nexts);
                        _ -> ?ERROR(Bin)
                    end;
                _ -> ?ERROR(Bin)
            end;
        Unicode when 16#DC00 =< Unicode, Unicode =< 16#DFFF ->  % サロゲートペアの後半部分
            ?ERROR(<<N/binary, Bin/binary>>);
        Unicode -> 
            string(Bin, unicode_to_utf8(Unicode, Acc), Nexts)
    end;
unicode_string(<<Bin/binary>>, _Acc, _Nexts) ->
    ?ERROR(Bin).

-spec unicode_to_utf8(0..1114111, iolist()) -> iolist().
unicode_to_utf8(Code, Acc) when Code < 16#80 ->
    [Code | Acc];
unicode_to_utf8(Code, Acc) when Code < 16#800 ->
    A = 2#11000000 bor (Code bsr 6),
    B = 2#10000000 bor (Code band 2#111111),
    [B, A | Acc];
unicode_to_utf8(Code, Acc) when Code < 16#10000 ->
    A = 2#11100000 bor (Code bsr 12),
    B = 2#10000000 bor ((Code bsr 6) band 2#111111),
    C = 2#10000000 bor (Code band 2#111111),
    [C, B, A | Acc];
unicode_to_utf8(Code, Acc) -> % NOTE: サロゲートペアの仕組み上、コード値が上限を越えることはないので、ここでの範囲チェックは不要
    A = 2#11110000 bor (Code bsr 18),
    B = 2#10000000 bor ((Code bsr 12) band 2#111111),
    C = 2#10000000 bor ((Code bsr  6) band 2#111111),
    D = 2#10000000 bor (Code band 2#111111),
    [D, C, B, A | Acc].

-spec number(binary(), [next()]) -> {jsone:json_value(), Rest::binary()}.
number(<<$-, Bin/binary>>, Nexts) -> number_integer_part(Bin, -1, Nexts);
number(<<Bin/binary>>,     Nexts) -> number_integer_part(Bin,  1, Nexts).

-spec number_integer_part(binary(), 1|-1, [next()]) -> {jsone:json_value(), Rest::binary()}.
number_integer_part(<<$0, Bin/binary>>, Sign, Nexts) ->
    number_fraction_part(Bin, Sign, 0, Nexts);
number_integer_part(<<C, Bin/binary>>, Sign, Nexts) when $1 =< C, C =< $9 ->
    number_integer_part_rest(Bin, C - $0, Sign, Nexts);
number_integer_part(<<Bin/binary>>, _Sign, _Nexts) ->
    ?ERROR(Bin).

-spec number_integer_part_rest(binary(), non_neg_integer(), 1|-1, [next()]) -> {jsone:json_value(), Rest::binary()}.
number_integer_part_rest(<<C, Bin/binary>>, N, Sign, Nexts) when $0 =< C, C =< $9 ->
    number_integer_part_rest(Bin, N * 10 + C - $0, Sign, Nexts);
number_integer_part_rest(<<Bin/binary>>, N, Sign, Nexts) ->
    number_fraction_part(Bin, Sign, N, Nexts).

-spec number_fraction_part(binary(), 1|-1, non_neg_integer(), [next()]) -> {jsone:json_value(), Rest::binary()}.
number_fraction_part(<<$., Bin/binary>>, Sign, Int, Nexts) ->
    number_fraction_part_rest(Bin, Sign, Int, 0, Nexts);
number_fraction_part(<<Bin/binary>>, Sign, Int, Nexts) ->
    number_exponation_part(Bin, Sign * Int, 0, Nexts).

-spec number_fraction_part_rest(binary(), 1|-1, non_neg_integer(), non_neg_integer(), [next()]) -> {jsone:json_value(), Rest::binary()}.
number_fraction_part_rest(<<C, Bin/binary>>, Sign, N, DecimalOffset, Nexts) when $0 =< C, C =< $9 ->
    number_fraction_part_rest(Bin, Sign, N * 10 + C - $0, DecimalOffset + 1, Nexts);
number_fraction_part_rest(<<Bin/binary>>, Sign, N, DecimalOffset, Nexts) when DecimalOffset > 0 ->
    number_exponation_part(Bin, Sign * N, DecimalOffset, Nexts);
number_fraction_part_rest(<<Bin/binary>>, _Sign, _N, _DecimalOffset, _Nexts) ->
    ?ERROR(Bin).

-spec number_exponation_part(binary(), integer(), non_neg_integer(), [next()]) -> {jsone:json_value(), Rest::binary()}.
number_exponation_part(<<$e, $+, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts);
number_exponation_part(<<$E, $+, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts);
number_exponation_part(<<$e, $-, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts);
number_exponation_part(<<$E, $-, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts);
number_exponation_part(<<$e, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts);
number_exponation_part(<<$E, Bin/binary>>, N, DecimalOffset, Nexts) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, Nexts) ->
    case DecimalOffset of
        0 -> next(Bin, N, Nexts);
        _ -> next(Bin, N / math:pow(10, DecimalOffset), Nexts)
    end.

-spec number_exponation_part(binary(), integer(), non_neg_integer(), 1|-1, non_neg_integer(), boolean(), [next()]) -> {jsone:json_value(), Rest::binary()}.
number_exponation_part(<<C, Bin/binary>>, N, DecimalOffset, ExpSign, Exp, _, Nexts) when $0 =< C, C =< $9 ->
    number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp * 10 + C - $0, false, Nexts);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, ExpSign, Exp, false, Nexts) ->
    Pos = ExpSign * Exp - DecimalOffset,
    next(Bin, N * math:pow(10, Pos), Nexts);
number_exponation_part(<<Bin/binary>>, _N, _DecimalOffset, _ExpSign, _Exp, _IsFirst, _Nexts) ->
    ?ERROR(Bin).
