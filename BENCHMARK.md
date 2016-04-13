Benchmark
=========

Environment
-----------

- OS: Ubuntu-15.10
- CPU: Intel(R) Core(TM) i7-6600U CPU @ 2.60GHz (x2)
- Erlang: OTP-18.3.1 ([package link](https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_18.3-1~ubuntu~wily_amd64.deb))
- Benchmark: [The benchmark of poison(v2.1.0)](https://github.com/devinus/poison#benchmarking)

Benchmarks
----------

To execute following commands, the (Elixir)[http://elixir-lang.org/] is required.

```sh
###
### Downloads poison
###
$ git clone https://github.com/devinus/poison
$ cd poison
$ git checkout 2.1.0


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
Finished in 89.84 seconds

## EncoderBench
structs (Poison)               500000   6.35 µs/op
maps (jiffy)                   500000   7.35 µs/op
maps (jsone)                   100000   10.00 µs/op
maps (Poison)                  100000   13.26 µs/op
structs (Jazz)                 100000   14.18 µs/op
structs (JSX)                  100000   17.83 µs/op
maps (Jazz)                    100000   18.97 µs/op
maps (JSX)                      50000   30.03 µs/op
strings (jiffy)                 10000   103.77 µs/op
lists (Poison)                  10000   114.34 µs/op
string escaping (jiffy)         10000   141.46 µs/op
lists (jsone)                   10000   151.70 µs/op
lists (Jazz)                    10000   197.31 µs/op
lists (jiffy)                   10000   214.93 µs/op
strings (JSX)                   10000   237.08 µs/op
lists (JSX)                      5000   337.25 µs/op
strings (Poison)                 5000   350.49 µs/op
jiffy                            5000   404.95 µs/op
jiffy (pretty)                   5000   415.19 µs/op
strings (Jazz)                   5000   445.60 µs/op
strings (jsone)                  5000   550.78 µs/op
string escaping (jsone)          2000   934.83 µs/op
Poison                           1000   1322.36 µs/op
string escaping (JSX)            1000   1371.11 µs/op
jsone                            1000   1379.04 µs/op
string escaping (Poison)         1000   1387.10 µs/op
Poison (pretty)                  1000   1453.36 µs/op
string escaping (Jazz)           1000   1568.84 µs/op
jsone (pretty)                   1000   1734.20 µs/op
Jazz                             1000   1818.49 µs/op
JSX                              1000   2027.41 µs/op
Jazz (pretty)                    1000   2031.73 µs/op
JSX (pretty)                      500   5223.72 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   59.63 µs/op
UTF-8 unescaping (Poison)       10000   219.79 µs/op
UTF-8 unescaping (JSX)          10000   243.64 µs/op
UTF-8 unescaping (jsone)         5000   342.50 µs/op
jiffy                            5000   522.25 µs/op
jsone                            1000   1217.44 µs/op
Poison                           1000   1223.37 µs/op
JSX                              1000   1630.77 µs/op


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
Finished in 103.42 seconds

## EncoderBench
maps (jsone)                   500000   5.58 µs/op
maps (jiffy)                   500000   7.12 µs/op
structs (Poison)               500000   7.66 µs/op
maps (Poison)                  100000   10.71 µs/op
structs (JSX)                  100000   14.68 µs/op
structs (Jazz)                 100000   19.85 µs/op
maps (Jazz)                    100000   21.08 µs/op
maps (JSX)                     100000   25.38 µs/op
lists (jsone)                   50000   64.76 µs/op
lists (Poison)                  20000   76.80 µs/op
strings (jiffy)                 10000   109.11 µs/op
string escaping (jiffy)         10000   140.35 µs/op
strings (Poison)                10000   140.58 µs/op
strings (JSX)                   10000   179.35 µs/op
lists (jiffy)                   10000   198.89 µs/op
lists (Jazz)                    10000   217.64 µs/op
lists (JSX)                     10000   220.85 µs/op
strings (jsone)                  5000   323.63 µs/op
strings (Jazz)                   5000   363.87 µs/op
jiffy                            5000   388.06 µs/op
jiffy (pretty)                   5000   404.84 µs/op
string escaping (jsone)          5000   481.52 µs/op
jsone                            5000   634.06 µs/op
string escaping (JSX)            5000   699.05 µs/op
Poison                           2000   744.13 µs/op
Poison (pretty)                  2000   802.29 µs/op
jsone (pretty)                   2000   956.29 µs/op
string escaping (Poison)         1000   1222.81 µs/op
string escaping (Jazz)           1000   1312.68 µs/op
Jazz                             1000   1572.41 µs/op
JSX                              1000   1667.37 µs/op
Jazz (pretty)                    1000   1795.51 µs/op
JSX (pretty)                      500   4434.13 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   59.95 µs/op
UTF-8 unescaping (jsone)        20000   83.79 µs/op
UTF-8 unescaping (Poison)       10000   109.70 µs/op
UTF-8 unescaping (JSX)          10000   159.35 µs/op
jiffy                            5000   504.77 µs/op
jsone                            5000   527.77 µs/op
Poison                           5000   686.90 µs/op
JSX                              1000   1320.74 µs/op
```

Libraries Version
-----------------

mix.lock:
```elixir
%{"benchfella": {:hex, :benchfella, "0.3.1"},
  "earmark": {:hex, :earmark, "0.2.1"},
  "ex_doc": {:hex, :ex_doc, "0.11.4"},
  "exjsx": {:git, "https://github.com/talentdeficit/exjsx.git", "53db5d995b1b070e3883381e9acb195969137f67", []},
  "jazz": {:git, "https://github.com/meh/jazz.git", "49f335492aca5516495199dd81dd18b845ebaa69", []},
  "jiffy": {:git, "https://github.com/davisp/jiffy.git", "6303ff98aaa3fce625038c8b7af2aa8b802f4742", []},
  "jsone": {:hex, :jsone, "1.2.2"},
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
+     {:jsone, "~> 1.2.2", only: :bench},
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
