%%% @doc Utility functions for `inet' module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013-2021, Takeru Ohta <phjgt308@gmail.com>
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
-module(jsone_inet).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([ip_address_to_json_string/1]).

%%--------------------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------------------
-define(IS_IPV4_RANGE(X), is_integer(X) andalso 0 =< X andalso X =< 255).
-define(IS_IPV4(A, B, C, D),
        ?IS_IPV4_RANGE(A) andalso ?IS_IPV4_RANGE(B) andalso ?IS_IPV4_RANGE(C) andalso ?IS_IPV4_RANGE(D)).

-define(IS_IPV6_RANGE(X), is_integer(X) andalso 0 =< X andalso X =< 65535).
-define(IS_IPV6(A, B, C, D, E, F, G, H),
        ?IS_IPV6_RANGE(A) andalso ?IS_IPV6_RANGE(B) andalso ?IS_IPV6_RANGE(C) andalso ?IS_IPV6_RANGE(D) andalso
        ?IS_IPV6_RANGE(E) andalso ?IS_IPV6_RANGE(F) andalso ?IS_IPV6_RANGE(G) andalso ?IS_IPV6_RANGE(H)).

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------


%% @doc Convert an IP address into a text representation.
%%
%% Please refer to the doc of `jsone:ip_address_to_json_string/1' for the detail.
-spec ip_address_to_json_string(inet:ip_address() | any()) -> {ok, jsone:json_string()} | error.
ip_address_to_json_string({A, B, C, D}) when ?IS_IPV4(A, B, C, D) ->
    {ok, iolist_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D]))};
ip_address_to_json_string({A, B, C, D, E, F, G, H}) when ?IS_IPV6(A, B, C, D, E, F, G, H) ->
    Text =
        case {A, B, C, D, E, F} of
            {0, 0, 0, 0, 0, 0} ->
                %% IPv4-compatible address
                io_lib:format("::~p.~p.~p.~p", [G bsr 8, G band 16#ff, H bsr 8, H band 16#ff]);
            {0, 0, 0, 0, 0, 16#ffff} ->
                %% IPv4-mapped address
                io_lib:format("::ffff:~p.~p.~p.~p", [G bsr 8, G band 16#ff, H bsr 8, H band 16#ff]);
            {0, 0, 0, 0, 16#ffff, 0} ->
                %% IPv4-translated address
                io_lib:format("::ffff:0:~p.~p.~p.~p", [G bsr 8, G band 16#ff, H bsr 8, H band 16#ff]);
            {16#64, 16#ff9b, 0, 0, 0, 0} ->
                %% IPv4/IPv6 translation
                io_lib:format("64:ff9b::~p.~p.~p.~p", [G bsr 8, G band 16#ff, H bsr 8, H band 16#ff]);
            {16#64, 16#ff9b, 1, _, _, _} ->
                %% IPv4/IPv6 translation
                {Prefix, _} = format_ipv6([A, B, C, D, E, F], 0, 0),
                Last = lists:flatten(io_lib:format("~p.~p.~p.~p", [G bsr 8, G band 16#ff, H bsr 8, H band 16#ff])),
                string:join(Prefix ++ [Last], ":");
            _ ->
                format_ipv6([A, B, C, D, E, F, G, H])
        end,
    {ok, iolist_to_binary(Text)};
ip_address_to_json_string(_) ->
    error.


%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec format_ipv6([0..65535]) -> string().
format_ipv6(Xs) ->
    case format_ipv6(Xs, 0, 0) of
        {Ys, shortening} ->
            [$: | string:join(Ys, ":")];
        {Ys, _} ->
            Text = string:join(Ys, ":"),
            case lists:last(Text) of
                $: ->
                    [Text | ":"];
                _ ->
                    Text
            end
    end.


-spec format_ipv6([0..65535], non_neg_integer(), non_neg_integer()) ->
          {[string()], not_shortened | shortening | shortened}.
format_ipv6([], _Zeros, _MaxZeros) ->
    {[], not_shortened};
format_ipv6([X | Xs], Zeros0, MaxZeros) ->
    Zeros1 =
        case X of
            0 ->
                Zeros0 + 1;
            _ ->
                0
        end,
    Shorten = Zeros1 > MaxZeros andalso Zeros1 > 1,
    case format_ipv6(Xs, Zeros1, max(Zeros1, MaxZeros)) of
        {Ys, not_shortened} ->
            case Shorten of
                true ->
                    {["" | Ys], shortening};
                false ->
                    {[to_hex(X) | Ys], not_shortened}
            end;
        {Ys, shortening} when X =:= 0 ->
            {Ys, shortening};
        {Ys, _} ->
            {[to_hex(X) | Ys], shortened}
    end.


-spec to_hex(0..65535) -> string().
to_hex(N) ->
    string:lowercase(integer_to_list(N, 16)).
