%%% @doc JSON decoding/encoding module
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
-module(jsone).

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([
         decode/1,
         encode/1
        ]).

-export_type([
              json_value/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,
              json_object_members/0,
              json_boolean/0
             ]).

%%--------------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------------
-type json_value()          :: json_number() | json_string() | json_array() | json_object() | json_boolean() | null.
-type json_boolean()        :: boolean().
-type json_number()         :: number().
-type json_string()         :: binary().
-type json_array()          :: [json_value()].
-type json_object()         :: {object, json_object_members()}.
-type json_object_members() :: [{json_string(), json_value()}].

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
%% @doc JSONバイナリをデコードする.
%%
%% デコードに失敗した場合はエラーが送出される
-spec decode(binary()) -> {json_value(), RestJson::binary()}.
decode(Json) ->
    jsone_decode:decode(Json).

%% @doc JSON値をiodata形式にエンコードする.
%%
%% エンコードに失敗した場合はエラーが送出される
-spec encode(json_value()) -> iodata().
encode(JsonValue) ->
    jsone_encode:encode(JsonValue).
