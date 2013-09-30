-module(jsone_decode).

-compile(inline).
-compile(native).

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
string(<<$\\, $", Bin/binary>>, Acc, Nexts) -> string(Bin, [$" | Acc], Nexts);
string(<<$\\, $/, Bin/binary>>, Acc, Nexts) -> string(Bin, [$/ | Acc], Nexts);
string(<<$\\, $\\,Bin/binary>>, Acc, Nexts) -> string(Bin, [$\\| Acc], Nexts);
string(<<$\\, $b, Bin/binary>>, Acc, Nexts) -> string(Bin, [$\b | Acc], Nexts);
string(<<$\\, $f, Bin/binary>>, Acc, Nexts) -> string(Bin, [$\f | Acc], Nexts);
string(<<$\\, $n, Bin/binary>>, Acc, Nexts) -> string(Bin, [$\n | Acc], Nexts);
string(<<$\\, $r, Bin/binary>>, Acc, Nexts) -> string(Bin, [$\r | Acc], Nexts);
string(<<$\\, $t, Bin/binary>>, Acc, Nexts) -> string(Bin, [$\t | Acc], Nexts);
string(<<$\\, $u, N:4/binary, Bin/binary>>, Acc, Nexts) ->
    case binary_to_integer(N, 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            <<$\\, $u, N2:4/binary, Bin2/binary>> = Bin,
            case binary_to_integer(N2, 16) of
                Low when 16#DC00 =< Low, Low =< 16#DFFF ->
                    Unicode = 16#10000 + (High - 16#D800) * 16#400 + (Low - 16#DC00),
                    string(Bin2, [unicode_to_utf8(Unicode) | Acc], Nexts)
            end;
        Unicode ->
            string(Bin, [unicode_to_utf8(Unicode) | Acc], Nexts)
    end;
string(<<C, Rest/binary>>, Acc, Nexts) when 16#20 =< C, C =< 16#21;
                                            16#23 =< C, C =< 16#5B;
                                            16#5D =< C, C =< 16#FF ->
    ordinal_string_end(Rest, 0, [C | Acc], Nexts).

%% XXX: name
ordinal_string_end(<<C, Bin/binary>>, Pos, Acc, Nexts) when 16#20 =< C, C =< 16#21;
                                                            16#23 =< C, C =< 16#5B;
                                                            16#5D =< C, C =< 16#FF ->
    ordinal_string_end(Bin, Pos + 1, [C | Acc], Nexts); %XXX:
ordinal_string_end(<<Bin/binary>>, _Pos, Acc, Nexts) ->
%    io:format("! ~p, ~p, ~p, ~p\n", [Bin, Pos, Acc, Nexts]),
%    <<Bin1:Pos/binary, Bin2/binary>> = Bin,
%    string(Bin2, [Bin1 | Acc], Nexts).
    string(Bin, Acc, Nexts).

unicode_to_utf8(Code) when Code < 16#80 ->
    Code;
unicode_to_utf8(Code) when Code < 16#800 ->
    [2#11000000 + (Code bsr 6),
     2#10000000 + (Code band 2#111111)];
unicode_to_utf8(Code) when Code < 16#10000 ->
    %% NOTE: Codeの値が 16#D800 から 16#DFFF の範囲内にないことは、呼び出し元が保証している
    [2#11100000 + (Code bsr 12),
     2#10000000 + ((Code bsr 6) band 2#111111),
     2#10000000 + (Code band 2#111111)];
unicode_to_utf8(Code) ->
    %% NOTE: ユニコード範囲内云々
    [2#1111000 + (Code bsr 18),
     2#1000000 + ((Code bsr 12) band 2#111111),
     2#1000000 + ((Code bsr 12) band 2#111111),
     2#1000000 + (Code band 2#111111)].

number(<<Bin/binary>>, Nexts) ->
    %% XXX:
    number(Bin, 0, Nexts).

number(<<C, Bin/binary>>, N, Nexts) when $0 =< C, C =< $9 ->
    number(Bin, N * 10 + C - $0, Nexts);
number(<<Bin/binary>>, N, Nexts) -> next(Bin, N, Nexts).


