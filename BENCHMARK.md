Benchmark
=========

Environment
-----------

- OS: Ubuntu-16.04
- CPU: Intel(R) Core(TM) i7-6600U CPU @ 2.60GHz (x2)
- Erlang: OTP-19.0.2 ([package link](https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_19.0.2-1~ubuntu~xenial_amd64.deb))
- Benchmark: [The benchmark of poison(v2.2.0)](https://github.com/devinus/poison/blob/2.2.0/README.md#benchmarking)

Benchmarks
----------

To execute following commands, the [Elixir](http://elixir-lang.org/) is required.

```sh
###
### Downloads poison
###
$ git clone https://github.com/devinus/poison
$ cd poison
$ git checkout 2.2.0


###
### Benchmark: Non HiPE
###
$ patch -p1 < non_hipe.patch
$ mix deps.get
$ MIX_ENV=bench mix compile
$ MIX_ENV=bench mix bench
Settings:
duration:      1.0 s
... abbrev ...
Finished in 96.87 seconds

## EncoderBench
structs (Poison)               500000   6.51 µs/op
maps (jiffy)                   500000   7.23 µs/op
maps (jsone)                   100000   10.64 µs/op
maps (Poison)                  100000   13.58 µs/op
structs (Jazz)                 100000   13.96 µs/op
structs (JSX)                  100000   15.78 µs/op
maps (Jazz)                    100000   19.30 µs/op
maps (JSX)                      50000   29.28 µs/op
strings (jiffy)                 20000   98.80 µs/op
lists (Poison)                  10000   109.30 µs/op
string escaping (jiffy)         10000   144.01 µs/op
lists (jsone)                   10000   157.39 µs/op
lists (Jazz)                    10000   201.82 µs/op
lists (jiffy)                   10000   210.40 µs/op
strings (JSX)                   10000   262.18 µs/op
lists (JSX)                      5000   357.25 µs/op
strings (Jazz)                   5000   399.89 µs/op
jiffy                            5000   408.03 µs/op
strings (Poison)                 5000   416.78 µs/op
jiffy (pretty)                   5000   420.94 µs/op
strings (jsone)                  5000   595.63 µs/op
string escaping (jsone)          2000   732.44 µs/op
string escaping (Jazz)           1000   1197.06 µs/op
string escaping (Poison)         1000   1318.82 µs/op
string escaping (JSX)            1000   1324.04 µs/op
Poison                           1000   1447.71 µs/op
Poison (pretty)                  1000   1534.74 µs/op
jsone                            1000   1556.85 µs/op
Jazz                             1000   1824.05 µs/op
jsone (pretty)                   1000   2001.25 µs/op
Jazz (pretty)                    1000   2041.22 µs/op
JSX                              1000   2184.59 µs/op
JSX (pretty)                      500   5533.04 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   63.01 µs/op
UTF-8 unescaping (Poison)       10000   249.70 µs/op
UTF-8 unescaping (JSX)          10000   281.84 µs/op
UTF-8 unescaping (jsone)         5000   399.38 µs/op
jiffy                            5000   544.84 µs/op
jsone                            1000   1364.38 µs/op
Poison                           1000   1401.35 µs/op
JSX                              1000   1844.55 µs/op


###
### Benchmark: HiPE
###
$ patch -p1 < hipe.patch
$ mix clean --deps && find . -name '*.beam' | xargs rm
$ ERL_COMPILER_OPTIONS="native" MIX_ENV=bench mix compile
$ MIX_ENV=bench mix bench
Settings:
duration:      1.0 s
... abbrev ...
Finished in 102.75 seconds

## EncoderBench
maps (jsone)                   500000   6.12 µs/op
structs (Poison)               500000   6.49 µs/op
maps (jiffy)                   500000   7.69 µs/op
maps (Poison)                  100000   12.32 µs/op
structs (JSX)                  100000   13.51 µs/op
structs (Jazz)                 100000   15.06 µs/op
maps (Jazz)                    100000   22.90 µs/op
maps (JSX)                     100000   27.03 µs/op
lists (jsone)                   50000   69.93 µs/op
lists (Poison)                  20000   79.04 µs/op
strings (jiffy)                 20000   96.67 µs/op
strings (Poison)                10000   142.43 µs/op
string escaping (jiffy)         10000   146.85 µs/op
strings (JSX)                   10000   179.96 µs/op
lists (jiffy)                   10000   207.75 µs/op
lists (Jazz)                    10000   229.95 µs/op
lists (JSX)                     10000   278.01 µs/op
strings (Jazz)                   5000   310.10 µs/op
string escaping (jsone)          5000   317.10 µs/op
strings (jsone)                  5000   321.69 µs/op
jiffy                            5000   409.73 µs/op
jiffy (pretty)                   5000   419.55 µs/op
jsone                            5000   664.34 µs/op
string escaping (JSX)            2000   767.67 µs/op
Poison                           2000   806.24 µs/op
Poison (pretty)                  2000   844.76 µs/op
jsone (pretty)                   1000   1063.73 µs/op
string escaping (Poison)         1000   1277.54 µs/op
string escaping (Jazz)           1000   1311.85 µs/op
Jazz                             1000   1630.21 µs/op
JSX                              1000   1777.62 µs/op
Jazz (pretty)                    1000   1888.71 µs/op
JSX (pretty)                      500   4872.34 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   62.42 µs/op
UTF-8 unescaping (jsone)        20000   92.63 µs/op
UTF-8 unescaping (Poison)       10000   118.97 µs/op
UTF-8 unescaping (JSX)          10000   172.07 µs/op
jiffy                            5000   542.77 µs/op
jsone                            5000   561.15 µs/op
Poison                           5000   751.36 µs/op
JSX                              1000   1435.10 µs/op
```

Libraries Version
-----------------

mix.lock:
```elixir
%{"benchfella": {:hex, :benchfella, "0.3.2"},
  "earmark": {:hex, :earmark, "0.2.1"},
  "ex_doc": {:hex, :ex_doc, "0.12.0"},
  "exjsx": {:git, "https://github.com/talentdeficit/exjsx.git", "202b2ee1b274511973de60e9fdfed218d3b5eecc", []},
  "jazz": {:git, "https://github.com/meh/jazz.git", "49f335492aca5516495199dd81dd18b845ebaa69", []},
  "jiffy": {:git, "https://github.com/davisp/jiffy.git", "330f41c486cf949707eb494b855634df324a6d92", []},
  "jsone": {:hex, :jsone, "1.2.5"},
  "jsx": {:hex, :jsx, "2.6.2"}}
```

Patches
-------

non_hipe.patch:
```patch
diff --git a/bench/encoder_bench.exs b/bench/encoder_bench.exs
index cf40dcd..3453a2d 100644
--- a/bench/encoder_bench.exs
+++ b/bench/encoder_bench.exs
@@ -10,6 +10,10 @@ defmodule EncoderBench do
     :jiffy.encode(list)
   end

+  bench "lists (jsone)", [list: gen_list] do
+    :jsone.encode(list)
+  end
+
   bench "lists (JSX)", [list: gen_list] do
     JSX.encode!(list)
   end
@@ -27,6 +31,10 @@ defmodule EncoderBench do
     :jiffy.encode(map)
   end

+  bench "maps (jsone)", [map: gen_map] do
+    :jsone.encode(map)
+  end
+
   bench "maps (JSX)", [map: gen_map] do
     JSX.encode!(map)
   end
@@ -44,6 +52,10 @@ defmodule EncoderBench do
     :jiffy.encode(string)
   end

+  bench "strings (jsone)", [string: gen_string] do
+    :jsone.encode(string, [:native_utf8])
+  end
+
   bench "strings (JSX)", [string: gen_string] do
     JSX.encode!(string)
   end
@@ -61,6 +73,10 @@ defmodule EncoderBench do
     :jiffy.encode(string, [:uescape])
   end

+  bench "string escaping (jsone)", [string: gen_string] do
+    :jsone.encode(string)
+  end
+
   bench "string escaping (JSX)", [string: gen_string] do
     JSX.encode!(string, [:uescape])
   end
@@ -90,6 +106,10 @@ defmodule EncoderBench do
     :jiffy.encode(data)
   end

+  bench "jsone", [data: gen_data] do
+    :jsone.encode(data)
+  end
+
   bench "JSX", [data: gen_data] do
     JSX.encode!(data)
   end
@@ -106,6 +126,10 @@ defmodule EncoderBench do
     :jiffy.encode(data, [:pretty])
   end

+  bench "jsone (pretty)", [data: gen_data] do
+    :jsone.encode(data, [{:indent, 1}, {:space, 2}])
+  end
+
   bench "JSX (pretty)", [data: gen_data] do
     JSX.encode!(data) |> JSX.prettify!
   end
diff --git a/bench/parser_bench.exs b/bench/parser_bench.exs
index a71fbff..114f226 100644
--- a/bench/parser_bench.exs
+++ b/bench/parser_bench.exs
@@ -12,6 +12,10 @@ defmodule ParserBench do
     :jiffy.decode(json, [:return_maps])
   end

+  bench "jsone", [json: gen_json] do
+    :jsone.decode(json)
+  end
+
   bench "JSX", [json: gen_json] do
     JSX.decode!(json, [:strict])
   end
@@ -25,6 +29,10 @@ defmodule ParserBench do
     :jiffy.decode(utf8)
   end

+  bench "UTF-8 unescaping (jsone)", [utf8: gen_utf8] do
+    :jsone.decode(utf8)
+  end
+
   bench "UTF-8 unescaping (JSX)", [utf8: gen_utf8] do
     JSX.decode!(utf8, [:strict])
   end
diff --git a/config/config.exs b/config/config.exs
index 1240a78..d176e11 100644
--- a/config/config.exs
+++ b/config/config.exs
@@ -15,7 +15,8 @@ use Mix.Config
 #       format: "$time $metadata[$level] $message\n"

 config :poison,
-  native: :erlang.system_info(:hipe_architecture) != :undefined
+#  native: :erlang.system_info(:hipe_architecture) != :undefined
+  native: false

 # It is also possible to import configuration files, relative to this
 # directory. For example, you can emulate configuration per environment
diff --git a/mix.exs b/mix.exs
index 89a8c10..ac2f53e 100644
--- a/mix.exs
+++ b/mix.exs
@@ -33,6 +33,7 @@ defmodule Poison.Mixfile do
     [{:earmark, "~> 0.2", only: :docs},
      {:ex_doc, "~> 0.11", only: :docs},
      {:benchfella, "~> 0.3", only: :bench},
+     {:jsone, "~> 1.2.5", only: :bench},
      {:jiffy, github: "davisp/jiffy", only: :bench},
      {:exjsx, github: "talentdeficit/exjsx", only: :bench},
      {:jazz, github: "meh/jazz", only: :bench}]
```

hipe.patch:
```patch
diff --git a/config/config.exs b/config/config.exs
index 013831e..1240a78 100644
--- a/config/config.exs
+++ b/config/config.exs
@@ -15,8 +15,7 @@ use Mix.Config
#       format: "$time $metadata[$level] $message\n"

config :poison,
-#  native: :erlang.system_info(:hipe_architecture) != :undefined
-  native: false
+  native: :erlang.system_info(:hipe_architecture) != :undefined

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
```
