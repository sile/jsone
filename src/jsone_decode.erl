-module(jsone_decode).

-compile(native).
-compile(bin_opt_info).

-export([
         decode/2
        ]).

decode(Bin, _Options) ->
    whitespace(Bin, value, []).

next(<<Bin/binary>>, Value, []) ->
    {Value, Bin};
next(<<Bin/binary>>, Value, [Next | Nexts]) ->
    case Next of
        {array_next, Values} -> whitespace(Bin, {array_next, [Value | Values]}, Nexts);
        {object_value, Entries} -> whitespace(Bin, {object_value, Value, Entries}, Nexts);
        {object_next, Key, Entries} -> whitespace(Bin, {object_next, Key, Value, Entries}, Nexts)
    end.

whitespace(<<$  , Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\t, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\r, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<$\n, Bin/binary>>, Next, Nexts) -> whitespace(Bin, Next, Nexts);
whitespace(<<Bin/binary>>,      Next, Nexts) ->
    case Next of
        value  -> value(Bin, Nexts);
        array  -> array(Bin, Nexts);
        object -> object(Bin, Nexts);
        string -> <<$", Bin2/binary>> = Bin,
                  string(Bin2, [], Nexts);
        {array_next, Values} -> array_next(Bin, Values, Nexts);
        {object_value, Key, Entries} -> object_value(Bin, Key, Entries, Nexts);
        {object_next, Key, Value, Entries} -> object_next(Bin, [{Key, Value} | Entries], Nexts)
    end.
 
value(<<"false", Bin/binary>>, Nexts) -> next(Bin, false, Nexts);
value(<<"true", Bin/binary>>, Nexts)  -> next(Bin, true, Nexts);
value(<<"null", Bin/binary>>, Nexts)  -> next(Bin, null, Nexts);
value(<<$[, Bin/binary>>, Nexts)      -> whitespace(Bin, array, Nexts);
value(<<${, Bin/binary>>, Nexts)      -> whitespace(Bin, object, Nexts);
value(<<$", Bin/binary>>, Nexts)      -> string(Bin, [], Nexts);
value(<<Bin/binary>>, Nexts)          -> number(Bin, Nexts).

array(<<$], Bin/binary>>, Nexts) -> next(Bin, [], Nexts);
array(<<Bin/binary>>, Nexts)     -> whitespace(Bin, value, [{array_next, []} | Nexts]).

array_next(<<$], Bin/binary>>, Values, Nexts) -> next(Bin, lists:reverse(Values), Nexts);
array_next(<<$,, Bin/binary>>, Values, Nexts) -> whitespace(Bin, value, [{array_next, Values} | Nexts]).

object(<<$}, Bin/binary>>, Nexts) -> next(Bin, {object, []}, Nexts);
object(<<Bin/binary>>, Nexts) -> whitespace(Bin, string, [{object_value, []} | Nexts]).

object_value(<<$:, Bin/binary>>, Key, Entries, Nexts) -> whitespace(Bin, value, [{object_next, Key, Entries} | Nexts]).

object_next(<<$}, Bin/binary>>, Entries, Nexts) -> next(Bin, {object, lists:reverse(Entries)}, Nexts);
object_next(<<$,, Bin/binary>>, Entries, Nexts) -> whitespace(Bin, string, [{object_value, Entries} | Nexts]).

string(<<$", Bin/binary>>,      Acc, Nexts) -> next(Bin, list_to_binary(lists:reverse(Acc)), Nexts);
string(<<$\\, B/binary>>, Acc, Nexts) ->
    case B of
        <<$", Bin/binary>> -> string(Bin, [$" | Acc], Nexts);
        <<$/, Bin/binary>> -> string(Bin, [$/ | Acc], Nexts);
        <<$\\,Bin/binary>> -> string(Bin, [$\\| Acc], Nexts);
        <<$b, Bin/binary>> -> string(Bin, [$\b | Acc], Nexts);
        <<$f, Bin/binary>> -> string(Bin, [$\f | Acc], Nexts);
        <<$n, Bin/binary>> -> string(Bin, [$\n | Acc], Nexts);
        <<$r, Bin/binary>> -> string(Bin, [$\r | Acc], Nexts);
        <<$t, Bin/binary>> -> string(Bin, [$\t | Acc], Nexts);
        <<$u, Bin/binary>> -> unicode_string(Bin, Acc, Nexts)
    end;
string(<<C, Bin/binary>>, Acc, Nexts) when 16#20 =< C ->
    string(Bin, [C | Acc], Nexts).

unicode_string(<<N:4/binary, $\\, $u, N2:4/binary, Bin/binary>>, Acc, Nexts) ->
    case {binary_to_integer(N, 16), binary_to_integer(N2, 16)} of
        {High, Low} when 16#D800 =< High, High =< 16#DBFF,
                         16#DC00 =< Low, Low =< 16#DFFF ->
            Unicode = 16#10000 + (High - 16#D800) * 16#400 + (Low - 16#DC00),
            string(Bin, unicode_to_utf8(Unicode, Acc), Nexts);
        {A, B} ->
            string(Bin, unicode_to_utf8(B, unicode_to_utf8(A, Acc)), Nexts)
    end;
unicode_string(<<N:4/binary, Bin/binary>>, Acc, Nexts) ->
    Unicode = binary_to_integer(N, 16),
    string(Bin, unicode_to_utf8(Unicode, Acc), Nexts).

unicode_to_utf8(Code, Acc) when Code < 16#80 ->
    [Code | Acc];
unicode_to_utf8(Code, Acc) when Code < 16#800 ->
    A = 2#11000000 bor (Code bsr 6),
    B = 2#10000000 bor (Code band 2#111111),
    [B, A | Acc];
unicode_to_utf8(Code, Acc) when Code < 16#10000 ->
    %% NOTE: Codeの値が 16#D800 から 16#DFFF の範囲内にないことは、呼び出し元が保証している
    A = 2#11100000 bor (Code bsr 12),
    B = 2#10000000 bor ((Code bsr 6) band 2#111111),
    C = 2#10000000 bor (Code band 2#111111),
    [C, B, A | Acc];
unicode_to_utf8(Code, Acc) ->
    %% NOTE: ユニコード範囲内云々
    A = 2#11110000 bor (Code bsr 18),
    B = 2#10000000 bor ((Code bsr 12) band 2#111111),
    C = 2#10000000 bor ((Code bsr  6) band 2#111111),
    D = 2#10000000 bor (Code band 2#111111),
    [D, C, B, A | Acc].

number(<<Bin/binary>>, Nexts) ->
    %% XXX:
    number(Bin, 0, Nexts).

number(<<C, Bin/binary>>, N, Nexts) when $0 =< C, C =< $9 ->
    number(Bin, N * 10 + C - $0, Nexts);
number(<<Bin/binary>>, N, Nexts) -> next(Bin, N, Nexts).


%% decode_number(<<$-, Bin/binary>>) -> {Num, Bin2} = decode_number_impl(Bin),
%%                                      {-Num, Bin2};
%% decode_number(<<$ , Bin/binary>>) -> decode_number(Bin);
%% decode_number(<<$\t,Bin/binary>>) -> decode_number(Bin);
%% decode_number(<<$\r,Bin/binary>>) -> decode_number(Bin);
%% decode_number(<<$\n,Bin/binary>>) -> decode_number(Bin);
%% decode_number(<<Bin/binary>>)     -> decode_number_impl(Bin).

%% decode_number_impl(<<$0, Bin/binary>>) -> decode_fraction(Bin, 0);
%% decode_number_impl(<<C, Bin/binary>>) when $1 =< C, C =< $9 -> decode_int(Bin, C - $0).

%% decode_int(<<C, Bin/binary>>, Acc) when $0 =< C, C =< $9 ->
%%     decode_int(Bin, (Acc*10) + (C-$0));
%% decode_int(<<Bin/binary>>, Acc) ->
%%     decode_fraction(Bin, Acc).

%% decode_fraction(<<$., Bin/binary>>, Acc) ->
%%     {Digit, Bin2} = decode_digit(Bin),
%%     Frac  = Digit / (math:pow(10, byte_size(Bin) - byte_size(Bin2))),
%%     decode_exponation(Bin2, Acc + Frac);
%% decode_fraction(<<Bin/binary>>, Acc) ->
%%     {Acc, Bin}.

%% decode_exponation(<<$e, $+, Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, plus);
%% decode_exponation(<<$E, $+, Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, plus);
%% decode_exponation(<<$e, $-, Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, minus);
%% decode_exponation(<<$E, $-, Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, minus);
%% decode_exponation(<<$e,     Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, plus);
%% decode_exponation(<<$E,     Bin/binary>>, Acc) -> decode_exponation(Bin, Acc, plus);
%% decode_exponation(<<Bin/binary>>, Acc)         -> {Acc, Bin}.

%% decode_exponation(<<Bin/binary>>, Acc, Sign) ->
%%     {Digit, Bin2} = decode_digit(Bin),
%%     Num = case Sign of
%%               plus  -> Acc * math:pow(10, Digit);
%%               minus -> Acc / math:pow(10, Digit)
%%           end,
%%     {Num, Bin2}.

%% decode_digit(<<C, Bin/binary>>) when $0 =< C, C =< $9 -> 
%%     decode_digit(Bin, C - $0).

%% decode_digit(<<C, Bin/binary>>, Acc) when $0 =< C, C =< $9 ->
%%     decode_digit(Bin, (Acc*10) - (C-$0));
%% decode_digit(<<Bin/binary>>, Acc) ->
%%     {Acc, Bin}.
