/-- 10を底とした対数を返す。
つまり、`10 ^ x ≤ n` を満たす最大の `x` を返す。-/
def Nat.log10 (n : Nat) : Nat :=
  let rec loop (n : Nat) (acc : Nat) : Nat :=
    if n < 10 then acc
    else loop (n / 10) (acc + 1)
  loop n 0

#guard Nat.log10 0 = 0
#guard Nat.log10 10 = 1
#guard Nat.log10 25 = 1
#guard Nat.log10 1000 = 3

/-- 自然数を指数表記する -/
def Nat.toSciNotation (n : Nat) : String :=
  let e := Nat.log10 n
  if e ≤ 4 then toString n
  else if e ≤ 308 then
    let m := n.toFloat / (10 ^ e).toFloat
    s!"{m} * 10^{e}"
  else
    -- n が大きすぎて Float に変換できない
    let m := n / (10 ^ e)
    s!"{m} * 10^{e}"

#eval (10 ^ 309).toFloat

/-- 10 を底とする指数表記。
たとえば `2.01 * 10^27` など -/
class SciNotaion (α : Type) where
  /-- 指数表記 -/
  toSciNotation : α → String

export SciNotaion (toSciNotation)

instance : SciNotaion Nat where
  toSciNotation := Nat.toSciNotation

#eval (100 / 2021 : Float)
#eval toSciNotation 992849692846322
#eval toSciNotation (98 ^ 999)
