/-- 2つの処理の計算時間を調べる -/
syntax "calc_time" doElem "vs" doElem : doElem

macro_rules
  | `(doElem| calc_time $x:doElem vs $y:doElem) => `(doElem| do
    let start_time ← IO.monoMsNow;
    $x;
    let end_time ← IO.monoMsNow;
    IO.println s!"Running time: {end_time - start_time}ms"

    let start_time' ← IO.monoMsNow;
    $y;
    let end_time' ← IO.monoMsNow;
    IO.println s!"Running time: {end_time' - start_time'}ms")
