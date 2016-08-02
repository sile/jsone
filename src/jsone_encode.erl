%%% @doc JSON encoding module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013-2016, Takeru Ohta <phjgt308@gmail.com>
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

-ifdef(ENABLE_HIPE).
-compile([native, {hipe, [o3]}]).
-endif.

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([encode/1, encode/2]).

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).
-define(IS_STR(X), (is_binary(X) orelse is_atom(X))).
-define(IS_UINT(X), (is_integer(X) andalso X >= 0)).
-define(IS_DATETIME(Y,M,D,H,Mi,S), (?IS_UINT(Y) andalso ?IS_UINT(M) andalso ?IS_UINT(D) andalso
                                    ?IS_UINT(H) andalso ?IS_UINT(Mi) andalso ?IS_UINT(S))).

-ifdef('NO_MAP_TYPE').
-define(IS_MAP(X), is_tuple(X)).
-define(ENCODE_MAP(Value, Nexts, Buf, Opt), ?ERROR(value, [Value, Nexts, Buf, Opt])).
-else.
-define(IS_MAP(X), is_map(X)).
-define(ENCODE_MAP(Value, Nexts, Buf, Opt), object(maps:to_list(Value), Nexts, Buf, Opt)).
-endif.

-type encode_result() :: {ok, binary()} | {error, {Reason::term(), [erlang:stack_item()]}}.
-type next() :: {array_values, [jsone:json_value()]}
              | {object_value, jsone:json_value(), jsone:json_object_members()}
              | {object_members, jsone:json_object_members()}
              | {char, binary()}.

-record(encode_opt_v2, {
          native_utf8 = false :: boolean(),
          float_format = [{scientific, 20}] :: [jsone:float_format_option()],
          datetime_format = {iso8601, 0} :: {jsone:datetime_format(), jsone:utc_offset_seconds()},
          object_key_type = string :: string | scalar | value,
          space = 0 :: non_neg_integer(),
          indent = 0 :: non_neg_integer()
         }).
-define(OPT, #encode_opt_v2).
-type opt() :: #encode_opt_v2{}.

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
next([], Buf, _)                       -> {ok, Buf};
next(Level = [Next | Nexts], Buf, Opt) ->
    case Next of
        {array_values, Values} ->
            case Values of
                [] -> array_values(Values, Nexts, Buf, Opt);
                _  -> array_values(Values, Nexts, pp_newline_or_space(<<Buf/binary, $,>>, Level, Opt), Opt)
            end;
        {object_value, Value, Members} ->
            object_value(Value, Members, Nexts, pp_space(<<Buf/binary, $:>>, Opt), Opt);
        {object_members, Members} ->
            case Members of
                [] -> object_members(Members, Nexts, Buf, Opt);
                _  -> object_members(Members, Nexts, pp_newline_or_space(<<Buf/binary, $,>>, Level, Opt), Opt)
            end;
        {char, C} ->
            next(Nexts, <<Buf/binary, C>>, Opt)
    end.

-spec value(jsone:json_value(), [next()], binary(), opt()) -> encode_result().
value(null, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "null">>, Opt);
value(false, Nexts, Buf, Opt)                        -> next(Nexts, <<Buf/binary, "false">>, Opt);
value(true, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "true">>, Opt);
value(Value, Nexts, Buf, Opt) when is_integer(Value) -> next(Nexts, <<Buf/binary, (integer_to_binary(Value))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when is_float(Value)   -> next(Nexts, <<Buf/binary, (float_to_binary(Value, Opt?OPT.float_format))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when ?IS_STR(Value)    -> string(Value, Nexts, Buf, Opt);
value({{_,_,_},{_,_,_}} = Value, Nexts, Buf, Opt)    -> datetime(Value, Nexts, Buf, Opt);
value({Value}, Nexts, Buf, Opt)                      -> object(Value, Nexts, Buf, Opt);
value([{}], Nexts, Buf, Opt)                         -> object([], Nexts, Buf, Opt);
value([{_, _}|_] = Value, Nexts, Buf, Opt)           -> object(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when ?IS_MAP(Value)    -> ?ENCODE_MAP(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when is_list(Value)    -> array(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt)                        -> ?ERROR(value, [Value, Nexts, Buf, Opt]).

-spec string(jsone:json_string(), [next()], binary(), opt()) -> encode_result().
string(<<Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, $">>, Opt);
string(Str, Nexts, Buf, Opt) ->
    string(atom_to_binary(Str, utf8), Nexts, Buf, Opt).

-spec datetime(calendar:datetime(), [next()], binary(), opt()) -> encode_result().
datetime({{Y,M,D}, {H,Mi,S}}, Nexts, Buf, Opt) when ?IS_DATETIME(Y,M,D,H,Mi,S) ->
    Str =
        case Opt?OPT.datetime_format of
            {iso8601, 0}  -> io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S]);
            {iso8601, Tz} ->
                {Sign, {DiffHour, DiffMinute, _}} =
                    case Tz > 0 of
                        true  -> {$+, calendar:seconds_to_time(Tz)};
                        false -> {$-, calendar:seconds_to_time(-Tz)}
                    end,
                io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~c~2..0B:~2..0B", [Y, M, D, H, Mi, S, Sign, DiffHour, DiffMinute])
        end,
    next(Nexts, <<Buf/binary, $", (list_to_binary(Str))/binary, $">>, Opt);
datetime(Datetime, Nexts, Buf, Opt) ->
    ?ERROR(datetime, [Datetime, Nexts, Buf, Opt]).

-spec object_key(jsone:json_value(), [next()], binary(), opt()) -> encode_result().
object_key(Key, Nexts, Buf, Opt) when ?IS_STR(Key) ->
    string(Key, Nexts, Buf, Opt);
object_key(Key, Nexts, Buf, Opt = ?OPT{object_key_type = scalar}) when is_number(Key) ->
    value(Key, [{char, $"} | Nexts], <<Buf/binary, $">>, Opt);
object_key(Key = {{Y,M,D},{H,Mi,S}}, Nexts, Buf, Opt = ?OPT{object_key_type = Type}) when ?IS_DATETIME(Y,M,D,H,Mi,S), Type =/= string ->
    value(Key, Nexts, Buf, Opt);
object_key(Key, Nexts, Buf, Opt = ?OPT{object_key_type = value}) ->
    case value(Key, [], <<>>, Opt) of
        {error, Reason} -> {error, Reason};
        {ok, BinaryKey} -> string(BinaryKey, Nexts, Buf, Opt)
    end;
object_key(Key, Nexts, Buf, Opt) ->
    ?ERROR(object_key, [Key, Nexts, Buf, Opt]).

-spec escape_string(binary(), [next()], binary(), opt()) -> encode_result().
escape_string(Str, Nexts, Buf, ?OPT{native_utf8 = true} = Opt) ->
    escape_string_native_utf8(Str, Nexts, Buf, Opt);
escape_string(Str, Nexts, Buf, ?OPT{native_utf8 = false} = Opt) ->
    escape_string_escaped_utf8(Str, Nexts, Buf, Opt).

-define(H8(X), (hex(X)):16).
-define(H16(X), ?H8(X bsr 8), ?H8(X band 16#FF)).

-ifdef(ENABLE_HIPE).
-define(COPY_UTF8,
escape_string_native_utf8(<<2#110:3, C1:5, C2, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_native_utf8(Str, Nexts, <<Buf/binary, (2#11000000+C1), C2>>, Opt);
escape_string_native_utf8(<<2#1110:4, C1:4, C2:16, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_native_utf8(Str, Nexts, <<Buf/binary, (2#11100000+C1), C2:16>>, Opt);
escape_string_native_utf8(<<2#11110:5, C1:3, C2:24, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_native_utf8(Str, Nexts, <<Buf/binary, (2#11110000+C1), C2:24>>, Opt)
    ).
-else.
-define(COPY_UTF8,
escape_string_native_utf8(<<Ch/utf8, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_native_utf8(Str, Nexts, <<Buf/binary, Ch/utf8>>, Opt)
    ).
-endif.

escape_string_native_utf8(<<>>, Nexts, Buf, Opt) -> next(Nexts, <<Buf/binary, $">>, Opt);
escape_string_native_utf8(<<0:1, Ch:7, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_native_utf8(Str, Nexts, ascii(Ch, Buf), Opt);
?COPY_UTF8;
escape_string_native_utf8(Str, Nexts, Buf, Opt) ->
    ?ERROR(escape_string, [Str, Nexts, Buf, Opt]).

escape_string_escaped_utf8(<<>>, Nexts, Buf, Opt) -> next(Nexts, <<Buf/binary, $">>, Opt);
escape_string_escaped_utf8(<<0:1, Ch:7, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string_escaped_utf8(Str, Nexts, ascii(Ch, Buf), Opt);
escape_string_escaped_utf8(<<Ch/utf8, Str/binary>>, Nexts, Buf, Opt) ->
    NewBuf = if
                 Ch =< 16#FFFF -> <<Buf/binary, $\\, $u, ?H16(Ch)>>;
                 true ->
                     <<H1, H2, L1, L2>> = <<Ch/utf16>>,
                     <<Buf/binary, $\\, $u, ?H8(H1), ?H8(H2), $\\, $u, ?H8(L1), ?H8(L2)>>
             end,
    escape_string_escaped_utf8(Str, Nexts, NewBuf, Opt);
escape_string_escaped_utf8(Str, Nexts, Buf, Opt) ->
    ?ERROR(escape_string, [Str, Nexts, Buf, Opt]).

-compile({inline, [hex/1]}).

hex(X) ->
  element(
    X+1,
    {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036, 16#3037,
     16#3038, 16#3039, 16#3061, 16#3062, 16#3063, 16#3064, 16#3065, 16#3066,
     16#3130, 16#3131, 16#3132, 16#3133, 16#3134, 16#3135, 16#3136, 16#3137,
     16#3138, 16#3139, 16#3161, 16#3162, 16#3163, 16#3164, 16#3165, 16#3166,
     16#3230, 16#3231, 16#3232, 16#3233, 16#3234, 16#3235, 16#3236, 16#3237,
     16#3238, 16#3239, 16#3261, 16#3262, 16#3263, 16#3264, 16#3265, 16#3266,
     16#3330, 16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
     16#3338, 16#3339, 16#3361, 16#3362, 16#3363, 16#3364, 16#3365, 16#3366,
     16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435, 16#3436, 16#3437,
     16#3438, 16#3439, 16#3461, 16#3462, 16#3463, 16#3464, 16#3465, 16#3466,
     16#3530, 16#3531, 16#3532, 16#3533, 16#3534, 16#3535, 16#3536, 16#3537,
     16#3538, 16#3539, 16#3561, 16#3562, 16#3563, 16#3564, 16#3565, 16#3566,
     16#3630, 16#3631, 16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637,
     16#3638, 16#3639, 16#3661, 16#3662, 16#3663, 16#3664, 16#3665, 16#3666,
     16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736, 16#3737,
     16#3738, 16#3739, 16#3761, 16#3762, 16#3763, 16#3764, 16#3765, 16#3766,
     16#3830, 16#3831, 16#3832, 16#3833, 16#3834, 16#3835, 16#3836, 16#3837,
     16#3838, 16#3839, 16#3861, 16#3862, 16#3863, 16#3864, 16#3865, 16#3866,
     16#3930, 16#3931, 16#3932, 16#3933, 16#3934, 16#3935, 16#3936, 16#3937,
     16#3938, 16#3939, 16#3961, 16#3962, 16#3963, 16#3964, 16#3965, 16#3966,
     16#6130, 16#6131, 16#6132, 16#6133, 16#6134, 16#6135, 16#6136, 16#6137,
     16#6138, 16#6139, 16#6161, 16#6162, 16#6163, 16#6164, 16#6165, 16#6166,
     16#6230, 16#6231, 16#6232, 16#6233, 16#6234, 16#6235, 16#6236, 16#6237,
     16#6238, 16#6239, 16#6261, 16#6262, 16#6263, 16#6264, 16#6265, 16#6266,
     16#6330, 16#6331, 16#6332, 16#6333, 16#6334, 16#6335, 16#6336, 16#6337,
     16#6338, 16#6339, 16#6361, 16#6362, 16#6363, 16#6364, 16#6365, 16#6366,
     16#6430, 16#6431, 16#6432, 16#6433, 16#6434, 16#6435, 16#6436, 16#6437,
     16#6438, 16#6439, 16#6461, 16#6462, 16#6463, 16#6464, 16#6465, 16#6466,
     16#6530, 16#6531, 16#6532, 16#6533, 16#6534, 16#6535, 16#6536, 16#6537,
     16#6538, 16#6539, 16#6561, 16#6562, 16#6563, 16#6564, 16#6565, 16#6566,
     16#6630, 16#6631, 16#6632, 16#6633, 16#6634, 16#6635, 16#6636, 16#6637,
     16#6638, 16#6639, 16#6661, 16#6662, 16#6663, 16#6664, 16#6665, 16#6666}
          ).

-compile({inline, [ascii/2]}).

ascii(0, Buf) -> <<Buf/binary, "\\u0000">>;
ascii(1, Buf) -> <<Buf/binary, "\\u0001">>;
ascii(2, Buf) -> <<Buf/binary, "\\u0002">>;
ascii(3, Buf) -> <<Buf/binary, "\\u0003">>;
ascii(4, Buf) -> <<Buf/binary, "\\u0004">>;
ascii(5, Buf) -> <<Buf/binary, "\\u0005">>;
ascii(6, Buf) -> <<Buf/binary, "\\u0006">>;
ascii(7, Buf) -> <<Buf/binary, "\\u0007">>;
ascii(8, Buf) -> <<Buf/binary, "\\b">>;
ascii(9, Buf) -> <<Buf/binary, "\\t">>;
ascii(10, Buf) -> <<Buf/binary, "\\n">>;
ascii(11, Buf) -> <<Buf/binary, "\\u000b">>;
ascii(12, Buf) -> <<Buf/binary, "\\f">>;
ascii(13, Buf) -> <<Buf/binary, "\\r">>;
ascii(14, Buf) -> <<Buf/binary, "\\u000e">>;
ascii(15, Buf) -> <<Buf/binary, "\\u000f">>;
ascii(16, Buf) -> <<Buf/binary, "\\u0010">>;
ascii(17, Buf) -> <<Buf/binary, "\\u0011">>;
ascii(18, Buf) -> <<Buf/binary, "\\u0012">>;
ascii(19, Buf) -> <<Buf/binary, "\\u0013">>;
ascii(20, Buf) -> <<Buf/binary, "\\u0014">>;
ascii(21, Buf) -> <<Buf/binary, "\\u0015">>;
ascii(22, Buf) -> <<Buf/binary, "\\u0016">>;
ascii(23, Buf) -> <<Buf/binary, "\\u0017">>;
ascii(24, Buf) -> <<Buf/binary, "\\u0018">>;
ascii(25, Buf) -> <<Buf/binary, "\\u0019">>;
ascii(26, Buf) -> <<Buf/binary, "\\u001a">>;
ascii(27, Buf) -> <<Buf/binary, "\\u001b">>;
ascii(28, Buf) -> <<Buf/binary, "\\u001c">>;
ascii(29, Buf) -> <<Buf/binary, "\\u001d">>;
ascii(30, Buf) -> <<Buf/binary, "\\u001e">>;
ascii(31, Buf) -> <<Buf/binary, "\\u001f">>;
ascii(32, Buf) -> <<Buf/binary, 32>>;
ascii(33, Buf) -> <<Buf/binary, 33>>;
ascii(34, Buf) -> <<Buf/binary, $\\, 34>>;
ascii(35, Buf) -> <<Buf/binary, 35>>;
ascii(36, Buf) -> <<Buf/binary, 36>>;
ascii(37, Buf) -> <<Buf/binary, 37>>;
ascii(38, Buf) -> <<Buf/binary, 38>>;
ascii(39, Buf) -> <<Buf/binary, 39>>;
ascii(40, Buf) -> <<Buf/binary, 40>>;
ascii(41, Buf) -> <<Buf/binary, 41>>;
ascii(42, Buf) -> <<Buf/binary, 42>>;
ascii(43, Buf) -> <<Buf/binary, 43>>;
ascii(44, Buf) -> <<Buf/binary, 44>>;
ascii(45, Buf) -> <<Buf/binary, 45>>;
ascii(46, Buf) -> <<Buf/binary, 46>>;
ascii(47, Buf) -> <<Buf/binary, $\\, 47>>;
ascii(48, Buf) -> <<Buf/binary, 48>>;
ascii(49, Buf) -> <<Buf/binary, 49>>;
ascii(50, Buf) -> <<Buf/binary, 50>>;
ascii(51, Buf) -> <<Buf/binary, 51>>;
ascii(52, Buf) -> <<Buf/binary, 52>>;
ascii(53, Buf) -> <<Buf/binary, 53>>;
ascii(54, Buf) -> <<Buf/binary, 54>>;
ascii(55, Buf) -> <<Buf/binary, 55>>;
ascii(56, Buf) -> <<Buf/binary, 56>>;
ascii(57, Buf) -> <<Buf/binary, 57>>;
ascii(58, Buf) -> <<Buf/binary, 58>>;
ascii(59, Buf) -> <<Buf/binary, 59>>;
ascii(60, Buf) -> <<Buf/binary, 60>>;
ascii(61, Buf) -> <<Buf/binary, 61>>;
ascii(62, Buf) -> <<Buf/binary, 62>>;
ascii(63, Buf) -> <<Buf/binary, 63>>;
ascii(64, Buf) -> <<Buf/binary, 64>>;
ascii(65, Buf) -> <<Buf/binary, 65>>;
ascii(66, Buf) -> <<Buf/binary, 66>>;
ascii(67, Buf) -> <<Buf/binary, 67>>;
ascii(68, Buf) -> <<Buf/binary, 68>>;
ascii(69, Buf) -> <<Buf/binary, 69>>;
ascii(70, Buf) -> <<Buf/binary, 70>>;
ascii(71, Buf) -> <<Buf/binary, 71>>;
ascii(72, Buf) -> <<Buf/binary, 72>>;
ascii(73, Buf) -> <<Buf/binary, 73>>;
ascii(74, Buf) -> <<Buf/binary, 74>>;
ascii(75, Buf) -> <<Buf/binary, 75>>;
ascii(76, Buf) -> <<Buf/binary, 76>>;
ascii(77, Buf) -> <<Buf/binary, 77>>;
ascii(78, Buf) -> <<Buf/binary, 78>>;
ascii(79, Buf) -> <<Buf/binary, 79>>;
ascii(80, Buf) -> <<Buf/binary, 80>>;
ascii(81, Buf) -> <<Buf/binary, 81>>;
ascii(82, Buf) -> <<Buf/binary, 82>>;
ascii(83, Buf) -> <<Buf/binary, 83>>;
ascii(84, Buf) -> <<Buf/binary, 84>>;
ascii(85, Buf) -> <<Buf/binary, 85>>;
ascii(86, Buf) -> <<Buf/binary, 86>>;
ascii(87, Buf) -> <<Buf/binary, 87>>;
ascii(88, Buf) -> <<Buf/binary, 88>>;
ascii(89, Buf) -> <<Buf/binary, 89>>;
ascii(90, Buf) -> <<Buf/binary, 90>>;
ascii(91, Buf) -> <<Buf/binary, 91>>;
ascii(92, Buf) -> <<Buf/binary, $\\, 92>>;
ascii(93, Buf) -> <<Buf/binary, 93>>;
ascii(94, Buf) -> <<Buf/binary, 94>>;
ascii(95, Buf) -> <<Buf/binary, 95>>;
ascii(96, Buf) -> <<Buf/binary, 96>>;
ascii(97, Buf) -> <<Buf/binary, 97>>;
ascii(98, Buf) -> <<Buf/binary, 98>>;
ascii(99, Buf) -> <<Buf/binary, 99>>;
ascii(100, Buf) -> <<Buf/binary, 100>>;
ascii(101, Buf) -> <<Buf/binary, 101>>;
ascii(102, Buf) -> <<Buf/binary, 102>>;
ascii(103, Buf) -> <<Buf/binary, 103>>;
ascii(104, Buf) -> <<Buf/binary, 104>>;
ascii(105, Buf) -> <<Buf/binary, 105>>;
ascii(106, Buf) -> <<Buf/binary, 106>>;
ascii(107, Buf) -> <<Buf/binary, 107>>;
ascii(108, Buf) -> <<Buf/binary, 108>>;
ascii(109, Buf) -> <<Buf/binary, 109>>;
ascii(110, Buf) -> <<Buf/binary, 110>>;
ascii(111, Buf) -> <<Buf/binary, 111>>;
ascii(112, Buf) -> <<Buf/binary, 112>>;
ascii(113, Buf) -> <<Buf/binary, 113>>;
ascii(114, Buf) -> <<Buf/binary, 114>>;
ascii(115, Buf) -> <<Buf/binary, 115>>;
ascii(116, Buf) -> <<Buf/binary, 116>>;
ascii(117, Buf) -> <<Buf/binary, 117>>;
ascii(118, Buf) -> <<Buf/binary, 118>>;
ascii(119, Buf) -> <<Buf/binary, 119>>;
ascii(120, Buf) -> <<Buf/binary, 120>>;
ascii(121, Buf) -> <<Buf/binary, 121>>;
ascii(122, Buf) -> <<Buf/binary, 122>>;
ascii(123, Buf) -> <<Buf/binary, 123>>;
ascii(124, Buf) -> <<Buf/binary, 124>>;
ascii(125, Buf) -> <<Buf/binary, 125>>;
ascii(126, Buf) -> <<Buf/binary, 126>>;
ascii(127, Buf) -> <<Buf/binary, 127>>.

-spec array(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array(List, Nexts, Buf, Opt) ->
    array_values(List, Nexts, pp_newline(<<Buf/binary, $[>>, Nexts, 1, Opt), Opt).

-spec array_values(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array_values([],       Nexts, Buf, Opt) -> next(Nexts, <<(pp_newline(Buf, Nexts, Opt))/binary, $]>>, Opt);
array_values([X | Xs], Nexts, Buf, Opt) -> value(X, [{array_values, Xs} | Nexts], Buf, Opt).

-spec object(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object(Members, Nexts, Buf, Opt) ->
    object_members(Members, Nexts, pp_newline(<<Buf/binary, ${>>, Nexts, 1, Opt), Opt).

-spec object_members(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_members([],                             Nexts, Buf, Opt)        -> next(Nexts, <<(pp_newline(Buf, Nexts, Opt))/binary, $}>>, Opt);
object_members([{Key, Value} | Xs], Nexts, Buf, Opt)                   -> object_key(Key, [{object_value, Value, Xs} | Nexts], Buf, Opt);
object_members(Arg, Nexts, Buf, Opt)                                   -> ?ERROR(object_members, [Arg, Nexts, Buf, Opt]).

-spec object_value(jsone:json_value(), jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_value(Value, Members, Nexts, Buf, Opt) ->
    value(Value, [{object_members, Members} | Nexts], Buf, Opt).

-spec pp_space(binary(), opt()) -> binary().
pp_space(Buf, Opt) -> padding(Buf, Opt?OPT.space).

-spec pp_newline(binary(), list(), opt()) -> binary().
pp_newline(Buf, Level, Opt) -> pp_newline(Buf, Level, 0, Opt).

-spec pp_newline(binary(), list(), non_neg_integer(), opt()) -> binary().
pp_newline(Buf, _, _,     ?OPT{indent = 0}) -> Buf;
pp_newline(Buf, L, Extra, ?OPT{indent = N}) -> lists:foldl(fun (_, B) -> padding(B, N) end, padding(<<Buf/binary, $\n>>, Extra * N), L).

-spec pp_newline_or_space(binary(), list(), opt()) -> binary().
pp_newline_or_space(Buf, _, Opt = ?OPT{indent = 0}) -> pp_space(Buf, Opt);
pp_newline_or_space(Buf, L, Opt)                    -> pp_newline(Buf, L, Opt).

-spec padding(binary(), non_neg_integer()) -> binary().
padding(Buf, 0) -> Buf;
padding(Buf, N) -> padding(<<Buf/binary, $ >>, N - 1).

-spec parse_options([jsone:encode_option()]) -> opt().
parse_options(Options) ->
    parse_option(Options, ?OPT{}).

-spec parse_option([jsone:encode_option()], opt()) -> opt().
parse_option([], Opt) -> Opt;
parse_option([native_utf8|T], Opt) ->
    parse_option(T, Opt?OPT{native_utf8=true});
parse_option([{float_format, F}|T], Opt) when is_list(F) ->
    parse_option(T, Opt?OPT{float_format = F});
parse_option([{space, N}|T], Opt) when is_integer(N), N >= 0 ->
    parse_option(T, Opt?OPT{space = N});
parse_option([{indent, N}|T], Opt) when is_integer(N), N >= 0 ->
    parse_option(T, Opt?OPT{indent = N});
parse_option([{object_key_type, Type}|T], Opt) when Type =:= string; Type =:= scalar; Type =:= value ->
    parse_option(T, Opt?OPT{object_key_type = Type});
parse_option([{datetime_format, Fmt}|T], Opt) ->
    case Fmt of
        iso8601                                 -> parse_option(T, Opt?OPT{datetime_format = {iso8601, 0}});
        {iso8601, utc}                          -> parse_option(T, Opt?OPT{datetime_format = {iso8601, 0}});
        {iso8601, local}                        -> parse_option(T, Opt?OPT{datetime_format = {iso8601, local_offset()}});
        {iso8601, N} when -86400 < N, N < 86400 -> parse_option(T, Opt?OPT{datetime_format = {iso8601, N}});
        _                                       -> error(badarg, [[{datetime_format, Fmt}|T], Opt])
    end;
parse_option(List, Opt) ->
    error(badarg, [List, Opt]).

-spec local_offset() -> jsone:utc_offset_seconds().
local_offset() ->
    UTC = {{1970, 1, 2}, {0,0,0}},
    Local = calendar:universal_time_to_local_time({{1970, 1, 2}, {0,0,0}}),
    calendar:datetime_to_gregorian_seconds(Local) - calendar:datetime_to_gregorian_seconds(UTC).
