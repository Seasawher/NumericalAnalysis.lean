/- # 末尾再帰
再帰関数を定義するときに、末尾再帰にすることでメモリを節約することができる。
-/

/-- `xⁿ` を計算する関数。素朴に定義したバージョン  -/
def naivePower (x n : Nat) : Nat :=
  match n with
  | 0 => 1
  | n + 1 => x * naivePower x n

-- 110000 とかを入れるとスタックオーバーフローになる
#eval naivePower 2 10

/-- `xⁿ` を計算する関数。末尾再帰にしたバージョン -/
def trPower (x n : Nat) : Nat :=
  let rec aux (x n acc : Nat) : Nat :=
    match n with
    | 0 => acc
    | n + 1 => aux x n (acc * x)
  aux x n 1

-- 110000 とかを入れても遅いけど落ちることはない
#eval trPower 2 10
#eval trPower 2 110000
