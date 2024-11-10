/- ## 1.1 アルゴリズム

ある固定された `x` に対して `x ^ n` を計算する問題を考える。（`n` は入力として与えらられる）

このとき、計算方法の違いによって、かなり計算時間に違いが生じる。
-/

/-- x の n 乗を計算する素朴なアルゴリズム。
オーバーフローを防ぐために末尾再帰にしてある。 -/
def naivePower (x n : Nat) :=
  aux x n 1
where
  aux (x n acc : Nat) :=
  match n with
  | 0 => acc
  | n + 1 => aux x n (acc * x)

#guard naivePower 2 10 = 1024

-- だいたい 200 ミリ秒くらいかかる
-- `=` の右辺は、計算結果の非常に長い数値を出力させないために指定しているので、何でもよい
#time #eval naivePower 2 100000 = 0

/- **繰り返し二乗法** を使うと、掛け算の回数を減らすことができる。

例：たとえば `x ^ 34` を計算するときに
1. `x ^ 2`
1. `x ^ 4`
1. `x ^ 8`
1. `x ^ 16`
1. `x ^ 32`
を順に２乗することで計算して、`x ^ 34 = x ^ 32 * x ^ 2` で計算すると掛け算は 6 回で済む。
-/

/-- notation class for `1` -/
class One (α : Type) where
  one : α

instance {α : Type} [One α] : OfNat α 1 where
  ofNat := One.one

instance : One Nat where
  one := 1

/-- 繰り返し自乗法のためのヘルパー関数 -/
def repeatedSquareAux {α : Type} [Mul α] (x acc : α) (n : Nat) : α :=
  if n = 0 then
    acc
  else if n % 2 = 0 then
    repeatedSquareAux (x * x) acc (n / 2)
  else
    repeatedSquareAux x (acc * x) (n - 1)

/-- 繰り返し自乗法 -/
def repeatedSquare {α : Type} [Mul α] [One α] (x : α) (n : Nat) : α :=
  repeatedSquareAux x 1 n

#guard repeatedSquare 2 16 = 65536
#guard repeatedSquare 3 5 = 243
#guard repeatedSquare 2 10 = 1024

-- だいたい 20 ミリ秒くらい
-- かなり高速になった
#time #eval repeatedSquare 2 100000 = 0

-- Lean の組み込みの Nat.pow と比較すると
-- こちらはだいたい 10 ミリ秒くらい
#time #eval Nat.pow 2 100000 = 0
