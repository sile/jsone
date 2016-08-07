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
structs (Poison)               500000   6.64 µs/op
maps (jiffy)                   500000   7.30 µs/op
maps (jsone)                   100000   11.51 µs/op
maps (Poison)                  100000   13.46 µs/op
structs (Jazz)                 100000   14.23 µs/op
structs (JSX)                  100000   15.60 µs/op
maps (Jazz)                    100000   19.79 µs/op
maps (JSX)                     100000   28.94 µs/op
strings (jiffy)                 20000   94.73 µs/op
lists (Poison)                  20000   98.92 µs/op
string escaping (jiffy)         10000   149.25 µs/op
lists (jsone)                   10000   153.25 µs/op
lists (Jazz)                    10000   201.31 µs/op
lists (jiffy)                   10000   209.62 µs/op
strings (JSX)                   10000   267.73 µs/op
lists (JSX)                      5000   335.35 µs/op
strings (Jazz)                   5000   404.81 µs/op
strings (Poison)                 5000   410.55 µs/op
jiffy (pretty)                   5000   430.67 µs/op
jiffy                            5000   497.42 µs/op
strings (jsone)                  5000   545.25 µs/op
string escaping (jsone)          2000   697.40 µs/op
string escaping (Jazz)           1000   1162.74 µs/op
string escaping (JSX)            1000   1322.45 µs/op
string escaping (Poison)         1000   1352.44 µs/op
Poison                           1000   1438.31 µs/op
Poison (pretty)                  1000   1477.34 µs/op
jsone                            1000   1508.22 µs/op
Jazz                             1000   1806.94 µs/op
jsone (pretty)                   1000   2024.11 µs/op
JSX                              1000   2068.95 µs/op
Jazz (pretty)                    1000   2110.42 µs/op
JSX (pretty)                      500   5516.55 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   63.49 µs/op
UTF-8 unescaping (Poison)       10000   258.79 µs/op
UTF-8 unescaping (JSX)          10000   279.01 µs/op
UTF-8 unescaping (jsone)         5000   364.77 µs/op
jiffy                            2000   588.38 µs/op
jsone                            1000   1471.44 µs/op
Poison                           1000   1495.67 µs/op
JSX                              1000   1855.33 µs/op


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
maps (jsone)                   500000   6.40 µs/op
structs (Poison)               500000   6.66 µs/op
maps (jiffy)                   500000   7.46 µs/op
maps (Poison)                  100000   11.98 µs/op
structs (JSX)                  100000   13.05 µs/op
structs (Jazz)                 100000   15.43 µs/op
maps (Jazz)                    100000   22.37 µs/op
maps (JSX)                     100000   27.30 µs/op
lists (jsone)                   50000   68.52 µs/op
lists (Poison)                  20000   79.27 µs/op
strings (jiffy)                 20000   98.95 µs/op
strings (Poison)                10000   142.03 µs/op
string escaping (jiffy)         10000   149.08 µs/op
strings (JSX)                   10000   185.93 µs/op
lists (jiffy)                   10000   206.95 µs/op
lists (Jazz)                    10000   228.95 µs/op
lists (JSX)                     10000   274.25 µs/op
strings (Jazz)                   5000   312.56 µs/op
string escaping (jsone)          5000   343.38 µs/op
strings (jsone)                  5000   355.18 µs/op
jiffy                            5000   404.91 µs/op
jiffy (pretty)                   5000   411.19 µs/op
string escaping (JSX)            2000   739.83 µs/op
Poison                           2000   783.98 µs/op
Poison (pretty)                  2000   833.02 µs/op
jsone                            2000   945.08 µs/op
string escaping (Poison)         1000   1263.35 µs/op
string escaping (Jazz)           1000   1292.65 µs/op
jsone (pretty)                   1000   1359.26 µs/op
Jazz                             1000   1635.89 µs/op
JSX                              1000   1718.37 µs/op
Jazz (pretty)                    1000   1841.45 µs/op
JSX (pretty)                      500   4709.19 µs/op

## ParserBench
UTF-8 unescaping (jiffy)        50000   62.64 µs/op
UTF-8 unescaping (jsone)        20000   90.13 µs/op
UTF-8 unescaping (Poison)       10000   119.44 µs/op
UTF-8 unescaping (JSX)          10000   168.10 µs/op
jsone                            5000   542.69 µs/op
jiffy                            5000   543.75 µs/op
Poison                           5000   733.23 µs/op
JSX                              1000   1389.98 µs/op
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
  "jsone": {:hex, :jsone, "1.2.4"},
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
+     {:jsone, "~> 1.2.4", only: :bench},
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
