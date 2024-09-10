import NumericalAnalysis.Lib.CalcTime

def naivePower (x n : Nat) :=
  aux x n 1
where aux (x n acc : Nat) :=
  match n with
  | 0 => acc
  | n + 1 => aux x n (acc * x)

def doPower (x n : Nat) : Nat := Id.run do
  let mut acc := 1
  for _ in [0:n] do
    acc := acc * x
  return acc

def main : IO Unit := do
  calc_time
    IO.println (naivePower 2 51000)
  vs
    -- 末尾再帰にはしていないが、オーバーフローしない
    -- 賢い
    IO.println (doPower 2 51000)

#eval main
