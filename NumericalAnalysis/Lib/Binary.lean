import NumericalAnalysis.Lib.Time

/-- 2進数 -/
def Binary := List Bool

deriving DecidableEq

/-- 2進数を文字列に変換する -/
def Binary.toString (b : Binary) : String :=
  let body := b.foldl (fun s b => s ++ (if b then "1" else "0")) ""
  body ++ "₂"

instance : ToString Binary := ⟨Binary.toString⟩

/-- 自然数を2進数に変換する -/
partial def toBinary (n : Nat) : Binary :=
  let rec loop (n : Nat) (acc : Binary) : Binary :=
    if n == 0 then acc
    else loop (n / 2) ((n % 2 == 1) :: acc)
  loop n []

#eval toBinary 4
#guard toBinary 5 = [true, false, true]


#time #eval toBinary 400000
