/- ## 1.1 アルゴリズム

ある固定された `x` に対して `x ^ n` を計算する問題を考える。（`n` は入力として与えらられる）

このとき、計算方法の違いによって、かなり計算時間に違いが生じる。
-/
import NumericalAnalysis.Lib.Time
import NumericalAnalysis.Lib.Binary
import NumericalAnalysis.Lib.Polynomial

namespace Ch11

/-- x の n 乗を計算する素朴なアルゴリズム。
メモリを節約するために末尾再帰にしてある。 -/
def naivePower (x n : Nat) :=
  aux x n 1
where aux (x n acc : Nat) :=
  match n with
  | 0 => acc
  | n + 1 => aux x n (acc * x)

#guard naivePower 2 10 = 1024

-- だいたい 1 秒くらいかかる
#time #eval naivePower 2 110000

/- **繰り返し二乗法** を使うと、掛け算の回数を減らすことができる。

例：たとえば `x ^ 34` を計算するときに
1. `x ^ 2`
1. `x ^ 4`
1. `x ^ 8`
1. `x ^ 16`
1. `x ^ 32`
を順に２乗することで計算して、`x ^ 34 = x ^ 32 * x ^ 2` で計算すると掛け算は 6 回で済む。
-/

-- 対数を計算する組み込みの関数
#check Nat.log2

/-- `x^(2^k), x^(2^(k-1)), ..., x⁸, x⁴, x², x` という x のべき乗のリストを作る。
ただし `k` は `2 ^ k ≤ n < 2 ^ (k + 1)` となる自然数。-/
def doublePower.createTable (x n : Nat) : List Nat := Id.run do
  let mut table := [x]
  let mut head := x
  let count := toBinary n |>.length
  for _ in [0:count-1] do
    -- この処理が一番重い
    table := head * head :: table

    head := head * head
  return table

#guard doublePower.createTable 1 34 = [1, 1, 1, 1, 1, 1]
#guard doublePower.createTable 2 5 = [16, 4, 2]
#guard doublePower.createTable 2 7 = [16, 4, 2]

/-- 繰り返し二乗法で `x ^ n` を計算する -/
@[noinline] def doublePower (x n : Nat) : Nat :=
  let table := doublePower.createTable x n
  let bin := toBinary n
  List.zip table bin
    |>.filter (fun (_, b) => b)
    |>.map (fun (x, _) => x)
    |>.foldl (· * ·) 1

#guard doublePower 2 16 = 65536
#guard doublePower 3 5 = 243
#guard doublePower 2 10 = 1024

-- だいたい 0.8 秒くらい
-- ちょっと早いかなという感じ
-- （掛け算の数が指数関数的に減っている割には、余り速くなっていないのが気になる）
#time #eval doublePower 2 110000

-- Lean の組み込みの Nat.pow と比較するとだいたい同じくらい
#time #eval Nat.pow 2 110000


end Ch11
/- 多項式の計算 `y = a₀ x⁵ + a₁ x⁴ + ⋯ + a₄ x + a₅` の右辺を計算したいとする。

`a₀ x⁵` から始めて順に `aₖ x⁵⁻ᵏ` を計算していくと、各項の計算に `5 - k` 回の掛け算が必要になるので、
全体で必要な掛け算の数は `5 + 4 + 3 + 2 + 1 = 15` となる。

しかし、
`y₁ = a₀ x + a₁`
`y₂ = y₁ x + a₂`
`y₃ = y₂ x + a₃`
`y₄ = y₃ x + a₄`
`y₅ = y₄ x + a₅`
として計算すると、各 `yₖ` の計算には 1 回の掛け算が必要で、全体で 5 回の掛け算で済む。
-/

/-- 多項式 `a₀ x⁵ + a₁ x⁴ + ⋯ + a₄ x + a₅` の計算を、
それぞれの項をまず計算してから足し合わせるという素朴な方法で計算する -/
def Polynomial.naiveEval (p : Polynomial "x") (x : Int) : Int :=
  let n := p.coes.length - 1
  p.coes.toArray
    |>.mapIdx (fun i c => c * x ^ (n - i))
    |>.foldl (· + ·) 0

def bigPolynomial : Polynomial "x" := Polynomial.mk <| (List.range 5000 |>.map (fun i => Int.ofNat i))

#eval Polynomial.naiveEval (⟨[1, 1, 1, 1, 1]⟩ : Polynomial "x") 2

-- だいたい 1 秒くらい
#time #eval Polynomial.naiveEval bigPolynomial 2