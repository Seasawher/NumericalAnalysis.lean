import Lean

open Lean Elab Command Term Meta

/-- コマンドの実行時間を計測する自作コマンド -/
elab "#time " stx:command : command => do
  -- 実行直前に計測開始
  let start_time ← IO.monoMsNow

  -- コマンドを実行
  elabCommand stx

  -- 実行後に計測終了
  let end_time ← IO.monoMsNow

  -- 差分を実行時間としてミリ秒単位で出力
  logInfo m!"time: {end_time - start_time}ms"
