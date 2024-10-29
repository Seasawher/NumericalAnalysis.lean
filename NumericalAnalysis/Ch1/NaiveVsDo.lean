import NumericalAnalysis.Lib.CalcTime

/-- `x ^ n` を計算する -/
def naivePower (x n : Nat) :=
  aux x n 1
where aux (x n acc : Nat) :=
  match n with
  | 0 => acc
  | n + 1 => aux x n (acc * x)

/-- do 構文で定義したバージョン。
`x ^n` を計算する。-/
def doPower (x n : Nat) : Nat := Id.run do
  let mut acc := 1
  for _ in [0:n] do
    acc := acc * x
  return acc

set_option linter.missingDocs false in

def main : IO Unit := do
  calc_time
    IO.println (naivePower 2 51000)
  vs
    -- 末尾再帰にはしていないが、オーバーフローしない
    -- 賢い
    IO.println (doPower 2 51000)

#eval main
