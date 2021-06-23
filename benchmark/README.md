Benchmark
=========

Using [poision](https://github.com/devinus/poison)'s benchmark.
Please execute the `./run.sh` script in this directory to run the benchmark.

Benchmark Result
----------------

Version:
- OTP-24
- jsone: [v1.6.0](https://github.com/sile/jsone/releases/tag/1.6.0)

Summary
-------

### Encoding (Unit: IPS=inputs per second)

| Input data \ Library | Jason      | jiffy       | JSON*  | jsone      | JSX    | Poison | Tiny   |
|----------------------|-----------:|------------:|-------:|-----------:|-------:|-------:|-------:|
| [Blockchain]         |     2.77 K |  **4.55 K** | 0.45 K | 1.44 K (3) | 0.60 K | 1.30 K | 0.99 K |
| [Giphy]              |     230.65 |  **487.67** |  47.73 | 114.57 (4) |  44.97 | 114.57 | 113.59 |
| [GitHub]             |     880.03 | **1566.67** | 139.79 | 300.26 (5) |  99.68 | 424.75 | 455.07 |
| [GovTrack]           |       6.57 |   **24.92** |   2.33 |   5.35 (5) |   2.65 |   7.06 |   7.86 |
| [Issue 90]           |  **22.80** |       21.92 |   0.77 |  14.30 (3) |   5.33 |  12.60 |  12.95 |
| [JSON Generateor]    |     200.40 |  **606.81** |  42.45 | 147.12 (4) |  68.73 | 187.95 | 123.93 |
| [Pokedex]            |     209.51 |  **776.67** |  62.60 | 161.45 (4) |  69.87 | 190.93 | 125.16 |
| [UTF-8 unescaped]    |     626.25 | **6644.53** |1167.89 | 582.41 (4) | 273.48 | 401.44 | 220.14 |

\* Only `JSON` didn't escape non-ASCII unicode characters on the encoding

[Blockchain]: https://github.com/devinus/poison/blob/4.0.1/bench/data/blockchain.json
[Giphy]: https://github.com/devinus/poison/blob/4.0.1/bench/data/giphy.json
[GitHub]: https://github.com/devinus/poison/blob/4.0.1/bench/data/github.json
[GovTrack]: https://github.com/devinus/poison/blob/4.0.1/bench/data/govtrack.json
[Issue 90]: https://github.com/devinus/poison/blob/4.0.1/bench/data/issue-90.json
[JSON Generateor]: https://github.com/devinus/poison/blob/4.0.1/bench/data/json-generator.json
[JSON Generateor (Pretty)]: https://github.com/devinus/poison/blob/4.0.1/bench/data/json-generator-pretty.json
[Pokedex]: https://github.com/devinus/poison/blob/4.0.1/bench/data/pokedex.json
[UTF-8 escaped]: https://github.com/devinus/poison/blob/4.0.1/bench/data/utf-8-escaped.json
[UTF-8 unescaped]: https://github.com/devinus/poison/blob/4.0.1/bench/data/utf-8-unescaped.json

### Decoding (Unit: IPS=inputs per second)

| Input data \ Library       | Jason      | jiffy       | JSON   | jsone      | JSX    | Poison | Tiny   |
|----------------------------|-----------:|------------:|-------:|-----------:|-------:|-------:|-------:|
| [Blockchain]               | **2.75 K** |      2.62 K | 0.35 K | 2.21 K (3) | 0.89 K | 1.32 K | 1.49 K |
| [Giphy]                    |     212.18 |  **243.45** |  35.67 | 109.11 (5) |  64.32 | 110.76 | 114.54 |
| [GitHub]                   |     973.41 | **1052.94** | 137.02 | 662.39 (3) | 271.97 | 438.79 | 542.67 |
| [GovTrack]                 |  **10.77** |        8.32 |   0.80 |   5.08 (3) |   2.81 |   3.58 |   3.65 |
| [Issue 90]                 |      17.85 |   **41.16** |   0.88 |  10.79 (5) |   6.02 |  13.63 |  14.03 |
| [JSON Generateor]          | **320.79** |      243.93 |  25.16 | 184.23 (3) | 111.24 | 135.47 | 139.78 |
| [JSON Generateor (Pretty)] | **273.57** |      205.09 |  25.04 | 158.82 (3) |  97.93 | 123.31 | 136.65 |
| [Pokedex]                  | **527.63** |      285.43 |  33.70 | 245.36 (3) | 140.90 | 172.45 | 152.59 |
| [UTF-8 escaped]            |    1224.48 | **7923.08** | 326.43 | 573.70 (4) | 550.36 | 918.21 | 520.31 |
| [UTF-8 unescaped]          |     5.56 K | **12.54 K** | 1.35 K | 5.09 K (3) | 3.30 K | 4.39 K | 1.46 K |



Details
-------

```
$ ./run.sh

##
## Encoding
##
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz
Number of Available Cores: 12
Available memory: 24.68 GB
Elixir 1.7.4
Erlang 24.0.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
parallel: 4
inputs: Blockchain, Giphy, GitHub, GovTrack, Issue 90, JSON Generator, Pokedex, UTF-8 unescaped
Estimated total run time: 6.53 min

##### With input Blockchain #####
Name                                ips        average  deviation         median         99th %
jiffy                            4.55 K      219.79 μs   ±227.93%      159.30 μs      600.33 μs
Jason                            2.77 K      361.51 μs   ±280.89%      164.50 μs     6000.18 μs
jsone                            1.44 K      692.75 μs   ±202.11%      303.80 μs     6609.40 μs
Poison                           1.30 K      769.30 μs   ±208.79%      324.50 μs     7034.96 μs
Tiny                             0.99 K     1009.25 μs   ±187.05%      312.70 μs     7021.59 μs
JSX                              0.60 K     1677.61 μs   ±125.78%         818 μs     8187.88 μs
JSON (w/o unicode escape)        0.45 K     2198.45 μs   ±116.54%     1010.80 μs     9246.82 μs

Comparison:
jiffy                            4.55 K
Jason                            2.77 K - 1.64x slower +141.72 μs
jsone                            1.44 K - 3.15x slower +472.95 μs
Poison                           1.30 K - 3.50x slower +549.50 μs
Tiny                             0.99 K - 4.59x slower +789.45 μs
JSX                              0.60 K - 7.63x slower +1457.82 μs
JSON (w/o unicode escape)        0.45 K - 10.00x slower +1978.65 μs

##### With input Giphy #####
Name                                ips        average  deviation         median         99th %
jiffy                            487.67        2.05 ms    ±78.70%        1.54 ms        8.92 ms
Jason                            230.65        4.34 ms    ±70.26%        2.06 ms       10.43 ms
Poison                           141.49        7.07 ms    ±47.75%        7.49 ms       13.94 ms
jsone                            114.57        8.73 ms    ±29.15%        9.66 ms       12.39 ms
Tiny                             113.59        8.80 ms    ±33.13%        9.65 ms       14.60 ms
JSON (w/o unicode escape)         47.73       20.95 ms    ±25.23%       21.45 ms       45.18 ms
JSX                               44.97       22.24 ms     ±5.54%       22.17 ms       25.09 ms

Comparison:
jiffy                            487.67
Jason                            230.65 - 2.11x slower +2.29 ms
Poison                           141.49 - 3.45x slower +5.02 ms
jsone                            114.57 - 4.26x slower +6.68 ms
Tiny                             113.59 - 4.29x slower +6.75 ms
JSON (w/o unicode escape)         47.73 - 10.22x slower +18.90 ms
JSX                               44.97 - 10.85x slower +20.19 ms

##### With input GitHub #####
Name                                ips        average  deviation         median         99th %
jiffy                           1566.67        0.64 ms   ±125.06%        0.49 ms        6.57 ms
Jason                            880.03        1.14 ms   ±157.00%        0.53 ms        7.18 ms
Tiny                             455.07        2.20 ms   ±111.76%        0.98 ms        8.56 ms
Poison                           424.75        2.35 ms   ±104.47%        1.11 ms        8.72 ms
jsone                            300.26        3.33 ms    ±83.98%        1.52 ms        9.15 ms
JSON (w/o unicode escape)        139.79        7.15 ms    ±46.69%        8.63 ms       13.20 ms
JSX                               99.68       10.03 ms    ±17.20%        9.80 ms       19.04 ms

Comparison:
jiffy                           1566.67
Jason                            880.03 - 1.78x slower +0.50 ms
Tiny                             455.07 - 3.44x slower +1.56 ms
Poison                           424.75 - 3.69x slower +1.72 ms
jsone                            300.26 - 5.22x slower +2.69 ms
JSON (w/o unicode escape)        139.79 - 11.21x slower +6.52 ms
JSX                               99.68 - 15.72x slower +9.39 ms

##### With input GovTrack #####
Name                                ips        average  deviation         median         99th %
jiffy                             24.92       40.13 ms    ±17.28%       37.63 ms       65.87 ms
Tiny                               7.86      127.26 ms    ±16.34%      120.95 ms      202.01 ms
Poison                             7.06      141.71 ms    ±17.46%      141.24 ms      230.78 ms
Jason                              6.57      152.32 ms    ±29.45%      166.96 ms      221.99 ms
jsone                              5.35      186.87 ms    ±14.14%      193.19 ms      210.20 ms
JSX                                2.65      376.97 ms     ±9.89%      372.41 ms      458.19 ms
JSON (w/o unicode escape)          2.33      429.24 ms    ±10.80%      425.02 ms      535.64 ms

Comparison:
jiffy                             24.92
Tiny                               7.86 - 3.17x slower +87.13 ms
Poison                             7.06 - 3.53x slower +101.58 ms
Jason                              6.57 - 3.80x slower +112.20 ms
jsone                              5.35 - 4.66x slower +146.75 ms
JSX                                2.65 - 9.39x slower +336.85 ms
JSON (w/o unicode escape)          2.33 - 10.70x slower +389.12 ms

##### With input Issue 90 #####
Name                                ips        average  deviation         median         99th %
Jason                             22.80       43.87 ms    ±24.63%       40.02 ms       75.01 ms
jiffy                             21.92       45.62 ms    ±19.38%       41.89 ms       77.27 ms
jsone                             14.30       69.94 ms    ±15.61%       67.54 ms      133.58 ms
Tiny                              12.95       77.24 ms    ±20.56%       72.77 ms      124.05 ms
Poison                            12.60       79.39 ms    ±21.64%       75.25 ms      136.51 ms
JSX                                5.33      187.61 ms    ±12.43%      180.47 ms      285.20 ms
JSON (w/o unicode escape)          0.77     1297.79 ms    ±11.08%     1252.49 ms     1612.29 ms

Comparison:
Jason                             22.80
jiffy                             21.92 - 1.04x slower +1.75 ms
jsone                             14.30 - 1.59x slower +26.07 ms
Tiny                              12.95 - 1.76x slower +33.37 ms
Poison                            12.60 - 1.81x slower +35.52 ms
JSX                                5.33 - 4.28x slower +143.74 ms
JSON (w/o unicode escape)          0.77 - 29.58x slower +1253.92 ms

##### With input JSON Generator #####
Name                                ips        average  deviation         median         99th %
jiffy                            606.81        1.65 ms    ±67.68%        1.34 ms        7.17 ms
Jason                            200.40        4.99 ms    ±67.53%        2.64 ms       10.38 ms
Poison                           187.95        5.32 ms    ±57.09%        3.26 ms       12.65 ms
jsone                            147.12        6.80 ms    ±44.42%        8.28 ms       11.48 ms
Tiny                             123.93        8.07 ms    ±42.50%        9.39 ms       15.41 ms
JSX                               68.73       14.55 ms    ±20.50%       13.50 ms       20.39 ms
JSON (w/o unicode escape)         42.45       23.56 ms    ±19.94%       24.33 ms       32.94 ms

Comparison:
jiffy                            606.81
Jason                            200.40 - 3.03x slower +3.34 ms
Poison                           187.95 - 3.23x slower +3.67 ms
jsone                            147.12 - 4.12x slower +5.15 ms
Tiny                             123.93 - 4.90x slower +6.42 ms
JSX                               68.73 - 8.83x slower +12.90 ms
JSON (w/o unicode escape)         42.45 - 14.29x slower +21.91 ms

##### With input Pokedex #####
Name                                ips        average  deviation         median         99th %
jiffy                            776.67        1.29 ms    ±87.38%        0.99 ms        6.88 ms
Jason                            209.51        4.77 ms    ±65.87%        2.54 ms       11.37 ms
Poison                           190.93        5.24 ms    ±53.42%        3.18 ms       10.66 ms
jsone                            161.45        6.19 ms    ±54.22%        7.61 ms       13.84 ms
Tiny                             125.16        7.99 ms    ±39.98%        9.51 ms       12.67 ms
JSX                               69.87       14.31 ms    ±26.83%       13.24 ms       26.74 ms
JSON (w/o unicode escape)         62.60       15.97 ms    ±24.85%       15.47 ms       28.53 ms

Comparison:
jiffy                            776.67
Jason                            209.51 - 3.71x slower +3.49 ms
Poison                           190.93 - 4.07x slower +3.95 ms
jsone                            161.45 - 4.81x slower +4.91 ms
Tiny                             125.16 - 6.21x slower +6.70 ms
JSX                               69.87 - 11.12x slower +13.02 ms
JSON (w/o unicode escape)         62.60 - 12.41x slower +14.69 ms

##### With input UTF-8 unescaped #####
Name                                ips        average  deviation         median         99th %
jiffy                           6644.53       0.150 ms    ±71.33%       0.111 ms        0.52 ms
JSON (w/o unicode escape)       1167.89        0.86 ms   ±197.53%        0.33 ms        7.07 ms
Jason                            626.25        1.60 ms   ±126.38%        0.79 ms        7.84 ms
jsone                            582.41        1.72 ms   ±120.31%        0.87 ms        8.40 ms
Poison                           401.44        2.49 ms   ±101.83%        1.05 ms        9.21 ms
JSX                              273.48        3.66 ms    ±72.06%        1.66 ms        8.39 ms
Tiny                             220.14        4.54 ms    ±73.89%        2.99 ms       10.39 ms

Comparison:
jiffy                           6644.53
JSON (w/o unicode escape)       1167.89 - 5.69x slower +0.71 ms
Jason                            626.25 - 10.61x slower +1.45 ms
jsone                            582.41 - 11.41x slower +1.57 ms
Poison                           401.44 - 16.55x slower +2.34 ms
JSX                              273.48 - 24.30x slower +3.51 ms
Tiny                             220.14 - 30.18x slower +4.39 ms


##
## Decoding
##
Operating System: Linux
CPU Information: Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz
Number of Available Cores: 12
Available memory: 24.68 GB
Elixir 1.7.4
Erlang 24.0.2

Benchmark suite executing with the following configuration:
warmup: 2 s
time: 5 s
memory time: 0 ns
parallel: 4
inputs: Blockchain, Giphy, GitHub, GovTrack, Issue 90, JSON Generator, JSON Generator (Pretty), Pokedex, UTF-8 escaped, UTF-8 unescaped
Estimated total run time: 8.17 min

##### With input Blockchain #####
Name             ips        average  deviation         median         99th %
Jason         2.75 K      364.27 μs    ±42.74%      318.40 μs      824.75 μs
jiffy         2.62 K      380.96 μs    ±26.26%      334.30 μs      697.00 μs
jsone         2.21 K      452.58 μs    ±33.19%      401.70 μs      916.63 μs
Tiny          1.49 K      672.69 μs    ±25.01%      622.10 μs     1144.22 μs
Poison        1.32 K      759.77 μs    ±36.93%         682 μs     1385.84 μs
JSX           0.89 K     1129.63 μs    ±22.84%     1093.05 μs     1827.75 μs
JSON          0.35 K     2853.02 μs    ±20.10%     2656.30 μs     4258.25 μs

Comparison:
Jason         2.75 K
jiffy         2.62 K - 1.05x slower +16.69 μs
jsone         2.21 K - 1.24x slower +88.31 μs
Tiny          1.49 K - 1.85x slower +308.42 μs
Poison        1.32 K - 2.09x slower +395.51 μs
JSX           0.89 K - 3.10x slower +765.36 μs
JSON          0.35 K - 7.83x slower +2488.75 μs

##### With input Giphy #####
Name             ips        average  deviation         median         99th %
jiffy         243.45        4.11 ms    ±24.50%        3.79 ms        7.30 ms
Jason         212.18        4.71 ms    ±12.22%        4.65 ms        6.38 ms
Tiny          114.54        8.73 ms    ±18.56%        8.49 ms       14.61 ms
Poison        110.76        9.03 ms    ±14.05%        8.77 ms       13.39 ms
jsone         109.11        9.17 ms    ±10.48%        8.99 ms       12.37 ms
JSX            64.32       15.55 ms    ±11.93%       15.05 ms       21.78 ms
JSON           35.67       28.03 ms    ±16.36%       27.43 ms       42.75 ms

Comparison:
jiffy         243.45
Jason         212.18 - 1.15x slower +0.61 ms
Tiny          114.54 - 2.13x slower +4.62 ms
Poison        110.76 - 2.20x slower +4.92 ms
jsone         109.11 - 2.23x slower +5.06 ms
JSX            64.32 - 3.78x slower +11.44 ms
JSON           35.67 - 6.82x slower +23.92 ms

##### With input GitHub #####
Name             ips        average  deviation         median         99th %
jiffy        1052.94        0.95 ms    ±22.77%        0.93 ms        1.61 ms
Jason         973.41        1.03 ms    ±25.72%        0.91 ms        1.63 ms
jsone         662.39        1.51 ms    ±29.01%        1.33 ms        2.44 ms
Tiny          542.67        1.84 ms    ±24.63%        1.65 ms        2.97 ms
Poison        438.79        2.28 ms    ±27.41%        2.00 ms        3.77 ms
JSX           271.97        3.68 ms    ±25.09%        3.37 ms        6.39 ms
JSON          137.02        7.30 ms    ±24.43%        6.51 ms       14.15 ms

Comparison:
jiffy        1052.94
Jason         973.41 - 1.08x slower +0.0776 ms
jsone         662.39 - 1.59x slower +0.56 ms
Tiny          542.67 - 1.94x slower +0.89 ms
Poison        438.79 - 2.40x slower +1.33 ms
JSX           271.97 - 3.87x slower +2.73 ms
JSON          137.02 - 7.68x slower +6.35 ms

##### With input GovTrack #####
Name             ips        average  deviation         median         99th %
Jason          10.77       92.89 ms     ±3.60%       92.38 ms      104.04 ms
jiffy           8.32      120.21 ms    ±13.66%      116.74 ms      209.22 ms
jsone           5.08      196.89 ms     ±2.84%      196.93 ms      213.51 ms
Tiny            3.65      273.94 ms     ±9.00%      266.38 ms      355.47 ms
Poison          3.58      279.42 ms     ±5.59%      275.79 ms      328.55 ms
JSX             2.81      356.16 ms     ±8.26%      348.67 ms      462.50 ms
JSON            0.80     1246.50 ms     ±8.67%     1200.07 ms     1461.19 ms

Comparison:
Jason          10.77
jiffy           8.32 - 1.29x slower +27.31 ms
jsone           5.08 - 2.12x slower +103.99 ms
Tiny            3.65 - 2.95x slower +181.04 ms
Poison          3.58 - 3.01x slower +186.53 ms
JSX             2.81 - 3.83x slower +263.27 ms
JSON            0.80 - 13.42x slower +1153.61 ms

##### With input Issue 90 #####
Name             ips        average  deviation         median         99th %
jiffy          41.16       24.30 ms    ±24.21%       21.18 ms       39.99 ms
Jason          17.85       56.03 ms    ±18.81%       52.29 ms      115.16 ms
Tiny           14.03       71.30 ms     ±7.87%       69.13 ms       91.71 ms
Poison         13.63       73.38 ms    ±16.26%       69.75 ms      110.67 ms
jsone          10.79       92.71 ms    ±16.86%       87.96 ms      176.42 ms
JSX             6.02      166.15 ms     ±4.41%      166.21 ms      188.03 ms
JSON            0.88     1132.61 ms    ±11.59%     1162.26 ms     1283.75 ms

Comparison:
jiffy          41.16
Jason          17.85 - 2.31x slower +31.73 ms
Tiny           14.03 - 2.93x slower +47.00 ms
Poison         13.63 - 3.02x slower +49.08 ms
jsone          10.79 - 3.82x slower +68.41 ms
JSX             6.02 - 6.84x slower +141.85 ms
JSON            0.88 - 46.61x slower +1108.31 ms

##### With input JSON Generator #####
Name             ips        average  deviation         median         99th %
Jason         320.79        3.12 ms    ±18.37%        3.01 ms        4.84 ms
jiffy         243.93        4.10 ms    ±32.94%        4.04 ms        8.01 ms
jsone         184.23        5.43 ms    ±12.12%        5.29 ms        7.40 ms
Tiny          139.78        7.15 ms    ±23.25%        6.53 ms       12.31 ms
Poison        135.47        7.38 ms    ±24.91%        6.74 ms       12.84 ms
JSX           111.24        8.99 ms    ±17.87%        8.30 ms       12.68 ms
JSON           25.16       39.75 ms    ±14.21%       38.92 ms       70.68 ms

Comparison:
Jason         320.79
jiffy         243.93 - 1.32x slower +0.98 ms
jsone         184.23 - 1.74x slower +2.31 ms
Tiny          139.78 - 2.29x slower +4.04 ms
Poison        135.47 - 2.37x slower +4.26 ms
JSX           111.24 - 2.88x slower +5.87 ms
JSON           25.16 - 12.75x slower +36.63 ms

##### With input JSON Generator (Pretty) #####
Name             ips        average  deviation         median         99th %
Jason         273.57        3.66 ms    ±22.33%        3.48 ms        6.78 ms
jiffy         205.09        4.88 ms    ±24.77%        4.60 ms        8.14 ms
jsone         158.82        6.30 ms    ±19.76%        5.99 ms       11.16 ms
Tiny          136.65        7.32 ms    ±20.28%        6.77 ms       10.96 ms
Poison        123.31        8.11 ms    ±25.48%        7.26 ms       14.10 ms
JSX            97.93       10.21 ms    ±19.97%        9.45 ms       17.34 ms
JSON           25.04       39.94 ms     ±7.43%       39.61 ms       49.30 ms

Comparison:
Jason         273.57
jiffy         205.09 - 1.33x slower +1.22 ms
jsone         158.82 - 1.72x slower +2.64 ms
Tiny          136.65 - 2.00x slower +3.66 ms
Poison        123.31 - 2.22x slower +4.45 ms
JSX            97.93 - 2.79x slower +6.56 ms
JSON           25.04 - 10.93x slower +36.28 ms

##### With input Pokedex #####
Name             ips        average  deviation         median         99th %
Jason         527.63        1.90 ms    ±14.20%        1.84 ms        2.81 ms
jiffy         285.43        3.50 ms    ±28.47%        3.33 ms        5.95 ms
jsone         245.36        4.08 ms    ±15.65%        3.97 ms        6.13 ms
Poison        172.45        5.80 ms    ±21.78%        5.28 ms        8.66 ms
Tiny          152.59        6.55 ms    ±23.87%        5.94 ms       11.96 ms
JSX           140.90        7.10 ms    ±17.08%        6.63 ms       10.11 ms
JSON           33.70       29.67 ms    ±19.74%       28.52 ms       64.83 ms

Comparison:
Jason         527.63
jiffy         285.43 - 1.85x slower +1.61 ms
jsone         245.36 - 2.15x slower +2.18 ms
Poison        172.45 - 3.06x slower +3.90 ms
Tiny          152.59 - 3.46x slower +4.66 ms
JSX           140.90 - 3.74x slower +5.20 ms
JSON           33.70 - 15.66x slower +27.78 ms

##### With input UTF-8 escaped #####
Name             ips        average  deviation         median         99th %
jiffy        7923.08       0.126 ms    ±60.63%       0.106 ms        0.23 ms
Jason        1224.48        0.82 ms    ±35.91%        0.78 ms        1.65 ms
Poison        918.21        1.09 ms    ±31.37%        0.93 ms        2.34 ms
jsone         573.70        1.74 ms    ±29.56%        1.58 ms        3.20 ms
JSX           550.36        1.82 ms    ±26.99%        1.66 ms        3.62 ms
Tiny          520.31        1.92 ms    ±28.48%        1.75 ms        3.73 ms
JSON          326.43        3.06 ms    ±16.08%        3.00 ms        4.47 ms

Comparison:
jiffy        7923.08
Jason        1224.48 - 6.47x slower +0.69 ms
Poison        918.21 - 8.63x slower +0.96 ms
jsone         573.70 - 13.81x slower +1.62 ms
JSX           550.36 - 14.40x slower +1.69 ms
Tiny          520.31 - 15.23x slower +1.80 ms
JSON          326.43 - 24.27x slower +2.94 ms

##### With input UTF-8 unescaped #####
Name             ips        average  deviation         median         99th %
jiffy        12.54 K       79.75 μs    ±93.83%       67.80 μs      147.80 μs
Jason         5.56 K      179.73 μs    ±26.63%      159.30 μs      369.25 μs
jsone         5.09 K      196.57 μs    ±34.98%      167.20 μs         406 μs
Poison        4.39 K      227.69 μs    ±40.06%      194.20 μs      432.10 μs
JSX           3.30 K      303.42 μs    ±39.01%      284.30 μs      522.46 μs
Tiny          1.46 K      686.25 μs    ±30.36%      639.30 μs     1144.77 μs
JSON          1.35 K      739.34 μs    ±64.82%      561.25 μs     2226.09 μs

Comparison:
jiffy        12.54 K
Jason         5.56 K - 2.25x slower +99.98 μs
jsone         5.09 K - 2.46x slower +116.82 μs
Poison        4.39 K - 2.85x slower +147.94 μs
JSX           3.30 K - 3.80x slower +223.67 μs
Tiny          1.46 K - 8.60x slower +606.50 μs
JSON          1.35 K - 9.27x slower +659.59 μs
```
