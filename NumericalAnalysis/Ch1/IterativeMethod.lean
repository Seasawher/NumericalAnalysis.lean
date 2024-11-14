import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Recharts

/-- ### 反復法
方程式 `x = f x` の解 `x` を求める。
`x₀` は初期値で、`ε` は許容誤差。
-/
def iterateMethod (f : Float → Float) (x₀ : Float) (ε : Float := 0.01) : Array Float := Id.run do
  let mut arr := #[]
  let mut x := x₀
  let mut counter := 0
  while (x - f x).abs ≥ ε do
    arr := arr.push x
    x := f x
    counter := counter + 1
    if counter > 500 then
      return #[]
  return arr

-- `2 + √2` を反復法で求める
#eval
  let result := iterateMethod (fun x => - 0.5 * x ^ 2 + 3 * x - 1.0) 1.0 (ε := 0.00000001)
  result.back! - 2.0

open Lean ProofWidgets Recharts

open scoped ProofWidgets.Jsx

#check Json

/-- LineChart を表示するために Json に変換したもの -/
def resultJson : Array Json :=
  let resultArr :=
    iterateMethod (fun x => - 0.5 * x ^ 2 + 3 * x - 1.0) 1.0 (ε := 0.00000001)
    |>.zipWithIndex
    |>.map (fun (x, y) => (y, x))
  resultArr.map (fun (x, y) => json% {x: $(toJson x), y: $(toJson y)})

#html
  <LineChart width={800} height={400} data={resultJson}>
    <XAxis dataKey?="x"/>
    <YAxis dataKey?="y"/>
    <Line type={.monotone} dataKey="y" stroke="#8884d8" dot?={Bool.true} />
  </LineChart>
