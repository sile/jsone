-module(jsone).

-export([
         decode/1, decode/2
        ]).

-export_type([
              json_value/0,
              json_number/0,
              json_string/0,
              json_array/0,
              json_object/0,
              json_boolean/0
             ]).

-type json_value() :: json_number() | json_string() | json_array() | json_object() | json_boolean() | null.
-type json_number() :: number().
-type json_string() :: binary().
-type json_array() :: [json_value()].
-type json_object() :: {object, [{json_string(), json_value()}]}.
-type json_boolean() :: boolean().

decode(JsonText) ->
    decode(JsonText, []).

decode(JsonText, Options) ->
    jsone_decode:decode(JsonText, Options).
