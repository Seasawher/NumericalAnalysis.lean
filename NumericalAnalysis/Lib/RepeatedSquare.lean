/- # 繰り返し二乗法 -/

/-- notation class for `1` -/
class One (α : Type) where
  one : α

instance {α : Type} [One α] : OfNat α 1 where
  ofNat := One.one

/-- helper function  -/
def RepeatedSquareAux {α : Type} [Mul α] [One α] (x acc : α) (n : Nat) : α :=
  if n = 0 then
    acc
  else if n % 2 = 0 then
    RepeatedSquareAux (x * x) acc (n / 2)
  else
    RepeatedSquareAux x (acc * x) (n - 1)

/-- repeated squaring -/
def RepeatedSquare {α : Type} [Mul α] [One α] (x : α) (n : Nat) : α :=
  RepeatedSquareAux x 1 n

/-- ### 比較用の素朴なアルゴリズム
naive power algorithm (tail recursive) -/
def naivePower {α : Type} [Mul α] [One α] (x : α) (n : Nat) : α :=
  aux x 1 n
where
  aux (x acc : α) (n : Nat) :=
    match n with
    | 0 => acc
    | n + 1 => aux x (acc * x) n

instance : One Nat where
  one := 1

#eval RepeatedSquare 2 10

-- 圧倒的に naive Power より速い
#time #eval naivePower 2 235000 = (default : Nat)
#time #eval RepeatedSquare 2 235000 = (default : Nat)
