import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/linalg/linalg"

import "lib/github.com/monoid-gmbh/kid-annexes/base"
import "lib/github.com/monoid-gmbh/kid-annexes/category2"
import "lib/github.com/monoid-gmbh/kid-annexes/category3"

-- import "Payoff"

module linalg_f64 = mk_linalg f64

-- | Clean data: drop nan rows
let clean_data [n] [l] (v: [l][n]f64): [][n]f64 =
  map (all (f64.isnan >-> not)) v |> zip v |> filter snd |> unzip >-> fst

-- Category2

-- | SampleContract1
entry sample_contract_1 [n] [l] (t: i32) (w: [n]f64) (v: [n][l]f64): (f64,f64,i64,[]scenario) =

  -- data cleaning
  let c: [][n]f64 = transpose v |> clean_data

  -- synthetic product
  let s: []f64 = linalg_f64.matvecmul_row c w

  -- number of days in years
  let y: f64 = (r64 t / 256.0)

   in category2 y s

-- Category3

-- SampleContract2
entry sample_contract_2 [n] [l] (t: i64) (_: [n]f64) (v: [n][l]f64): (f64,f64,i64,[]scenario) =

  let rng = minstd_rand.rng_from_seed [123] -- TODO: inarg
  let dummy_payoff [n] [t] (m: [n][t]f64): f64 = 1.0 -- no risk, no profit

  let (_,var,vev,mrm,scenarios) = category3 rng t dummy_payoff v
   in (var,vev,mrm,scenarios)
