
/- # 多項式
表示するための多項式。

## 技術的な問題点
* 係数は Float にしたかったが、Float にすると係数が整数の場合でもゼロがたくさん表示されてしまうため
係数を整数に限定している。
-/

/-- 多項式 -/
structure Polynomial (var : String) where
  /-- 係数のリスト -/
  coes : List Int

variable (var : String)

/-- 多項式を文字列として表示させる -/
def Polynomial.toString (p : Polynomial var) : String :=
  let coes := p.coes.toArray
  let n := coes.size
  let terms := coes.mapIdx fun i c =>
    let k := n - i - 1
    if c == 0 then ""
    else if c == 1 then
      if k == 0 then "1"
      else if k == 1 then var
      else var ++ "^" ++ ToString.toString k
    else
      if k == 0 then ToString.toString c
      else if c == -1 then var ++ "^" ++ ToString.toString k
      else ToString.toString c ++ var ++ "^" ++ ToString.toString k
  terms.filter (· ≠ "")
    |>.toList
    |> String.intercalate " + "

instance (var : String) : ToString (Polynomial var) := ⟨Polynomial.toString var⟩

/-- 例のための意味のない多項式 -/
def Polynomial.sample : Polynomial "X" :=
  ⟨[1, 2, 3, 4, 5]⟩

/-- info: X^4 + 2X^3 + 3X^2 + 4X^1 + 5 -/
#guard_msgs in
  #eval Polynomial.sample

/-- info: z^5 + 3z^3 + 1 -/
#guard_msgs in
  -- 係数が 0 の項は表示されない
  #eval (⟨[1, 0, 3, 0, 0, 1]⟩ : Polynomial "z")
