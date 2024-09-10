import NumericalAnalysis.Lib.Binary
import NumericalAnalysis.Lib.Polynomial

/- #

多項式の計算 `y = a₀ x⁵ + a₁ x⁴ + ⋯ + a₄ x + a₅` の右辺を計算したいとする。

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

/-- サンプルの大きな多項式 -/
private def bigPolynomial : Polynomial "x" := Polynomial.mk <| (List.range 5000 |>.map (fun i => Int.ofNat i))

#eval Polynomial.naiveEval (⟨[1, 1, 1, 1, 1]⟩ : Polynomial "x") 2

-- だいたい 1 秒くらい
#time #eval Polynomial.naiveEval bigPolynomial 2
