module Main where

import Arrow.Col qualified as C
import Arrow.Col ((>.))
import Arrow.Val qualified as V
import Arrow.Apl qualified as Apl
import Arrow.Dtype (Dtype(..))
import Data.Char (ord)
import Data.Int (Int64)
import System.Exit (exitFailure)

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert label expected actual
  | expected == actual = putStrLn $ "  OK  " ++ label
  | otherwise = do
      putStrLn $ "  FAIL " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
      exitFailure

main :: IO ()
main = do
    putStrLn "-- construction + access --"
    let a = C.mk @'Int64 [Just 1, Nothing, Just 3]
    assert "len" 3 (C.len a)
    assert "nullCount" 1 (C.nullCount a)

    putStrLn "\n-- element access --"
    assert "get 0" (Just 1)                  (C.get a 0)
    assert "get 1" (Nothing :: Maybe Int64)  (C.get a 1)
    assert "get 2" (Just 3)                  (C.get a 2)

    putStrLn "\n-- arithmetic --"
    let b = C.mk @'Int64 [Just 10, Just 20, Just 30]
    let c = a + b
    assert "add 0" (Just 11) (C.get c 0)
    assert "add 1" Nothing   (C.get c 1)
    assert "add 2" (Just 33) (C.get c 2)

    putStrLn "\n-- comparison + filter --"
    let d = C.filter b (b >. a)
    assert "filter len" 2 (C.len d)

    putStrLn "\n-- sort --"
    let e = C.sort a True
    assert "sort 0" (Just 1) (C.get e 0)
    assert "sort 1" (Just 3) (C.get e 1)

    putStrLn "\n-- aggregation --"
    assert "sum" (Just 4)     (V.extract (C.sum a))
    assert "mean" (Just 20.0) (V.extract (C.mean b))

    putStrLn "\n-- float64 --"
    let f = C.mk @'Float64 [Just 1.5, Nothing, Just 2.5]
    assert "f64 get" (Just 1.5) (C.get f 0)
    assert "f64 sum" (Just 4.0) (V.extract (C.sum f))

    putStrLn "\n-- bool --"
    let bl = C.mk @'Bool [Just True, Just False, Nothing]
    assert "bool 0" (Just True)  (C.get bl 0)
    assert "bool 1" (Just False) (C.get bl 1)
    assert "bool 2" (Nothing :: Maybe Bool) (C.get bl 2)

    -- -------------------------------------------------------------------
    -- PS.apl literal translations
    -- -------------------------------------------------------------------

    putStrLn "\n-- PS.apl:89  t[⍸x∊prms]←P --"
    let x    = C.mk @'Int64 [Just 1, Just 2, Just 3, Just 4, Just 5]
        prms = C.mk @'Int64 [Just 2, Just 4]
        t    = C.mk @'Int64 [Just 0, Just 0, Just 0, Just 0, Just 0]
        r89  = Apl.classifyPrims x prms t 7
    assert "t[⍸x∊prms]←P [0]" (Just 0) (C.get r89 0)
    assert "t[⍸x∊prms]←P [1]" (Just 7) (C.get r89 1)
    assert "t[⍸x∊prms]←P [2]" (Just 0) (C.get r89 2)
    assert "t[⍸x∊prms]←P [3]" (Just 7) (C.get r89 3)
    assert "t[⍸x∊prms]←P [4]" (Just 0) (C.get r89 4)

    putStrLn "\n-- PS.apl:77  i←⍸2<⌿0⍪dm --"
    let dm  = C.mk @'Int64 [Just 1, Just 1, Just 2, Just 2, Just 3]
        r77 = Apl.tokenStarts dm
    assert "⍸2<⌿0⍪dm len" 3     (C.len r77)
    assert "⍸2<⌿0⍪dm [0]" (Just 0) (C.get r77 0)
    assert "⍸2<⌿0⍪dm [1]" (Just 2) (C.get r77 1)
    assert "⍸2<⌿0⍪dm [2]" (Just 4) (C.get r77 2)

    putStrLn "\n-- PS.apl:98  d←+⍀1 ¯1 0['{}'⍳x] --"
    let chars = map (fromIntegral . ord) "a{b{c}d}e" :: [Int64]
        xc  = C.mk @'Int64 (map Just chars)
        r98 = Apl.braceDepth xc
    assert "+⍀ depth [0] 'a'" (Just 0) (C.get r98 0)
    assert "+⍀ depth [1] '{'" (Just 1) (C.get r98 1)
    assert "+⍀ depth [2] 'b'" (Just 1) (C.get r98 2)
    assert "+⍀ depth [3] '{'" (Just 2) (C.get r98 3)
    assert "+⍀ depth [4] 'c'" (Just 2) (C.get r98 4)
    assert "+⍀ depth [5] '}'" (Just 1) (C.get r98 5)
    assert "+⍀ depth [6] 'd'" (Just 1) (C.get r98 6)
    assert "+⍀ depth [7] '}'" (Just 0) (C.get r98 7)
    assert "+⍀ depth [8] 'e'" (Just 0) (C.get r98 8)

    putStrLn "\n-- util.apl  P2D (parent→depth) --"
    let pv  = C.mk @'Int64 [Just (-1), Just 0, Just 0, Just 1]
        dep = Apl.computeDepth pv 4
    assert "P2D [0] root"       (Just 0) (C.get dep 0)
    assert "P2D [1] child"      (Just 1) (C.get dep 1)
    assert "P2D [2] child"      (Just 1) (C.get dep 2)
    assert "P2D [3] grandchild" (Just 2) (C.get dep 3)

    putStrLn "\nAll tests passed."
