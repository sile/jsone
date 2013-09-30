-module(jsone).

-export([
         decode/1, decode/2
        ]).

decode(JsonText) ->
    decode(JsonText, []).

decode(JsonText, Options) ->
    jsone_decode:decode(JsonText, Options).
