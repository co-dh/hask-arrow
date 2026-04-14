module Main where

import Arrow.Col qualified as C
import Arrow.Col ((>.))
import Arrow.Val qualified as V
import Arrow.Apl qualified as Apl
import Arrow.Batch qualified as B
import Arrow.Parquet qualified as Pq
import Arrow.Dtype (Dtype(..))
import Control.Monad (when)
import Data.Char (ord)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory (removeFile, doesFileExist)
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

    putStrLn "\n-- rot / xorScan --"
    let ri = C.mk @'Int64 [Just 1, Just 2, Just 3, Just 4, Just 5]
        rL = Apl.rot 1 ri      -- left by 1: [2,3,4,5,1]
        rR = Apl.rot (-1) ri   -- right by 1: [5,1,2,3,4]
    assert "rot left  [0]" (Just 2) (C.get rL 0)
    assert "rot left  [4]" (Just 1) (C.get rL 4)
    assert "rot right [0]" (Just 5) (C.get rR 0)
    assert "rot right [4]" (Just 4) (C.get rR 4)
    let parityIn = C.mk @'Bool [Just True, Just False, Just True, Just True, Just False]
        parity   = Apl.xorScan parityIn  -- [T,T,F,T,T]
    assert "xorScan [0]" (Just True)  (C.get parity 0)
    assert "xorScan [1]" (Just True)  (C.get parity 1)
    assert "xorScan [2]" (Just False) (C.get parity 2)
    assert "xorScan [3]" (Just True)  (C.get parity 3)
    assert "xorScan [4]" (Just True)  (C.get parity 4)

    putStrLn "\n-- PS.apl:85  pairFind ('⍺⍺'⍷x) --"
    -- x = "a⍺⍺b⍺c" → '⍺⍺' only matches at position 1
    let pc = C.mk @'Int64 (map (Just . fromIntegral . ord) "a⍺⍺b⍺c")
        pf = Apl.pairFind (Apl.cc '⍺') (Apl.cc '⍺') pc
    assert "pairFind [0]" (Just False) (C.get pf 0)
    assert "pairFind [1]" (Just True)  (C.get pf 1)
    assert "pairFind [2]" (Just False) (C.get pf 2)
    assert "pairFind [5]" (Just False) (C.get pf 5)

    putStrLn "\n-- PS.apl:108  tm←(d=0)∧'∇'=x --"
    let tmD = C.mk @'Int64 [Just 0, Just 0, Just 1, Just 0]
        tmX = C.mk @'Int64 [Just (Apl.cc '∇'), Just 65, Just (Apl.cc '∇'), Just (Apl.cc '∇')]
        tmR = Apl.tradFnMask tmD tmX
    assert "tradFnMask [0]" (Just True)  (C.get tmR 0)   -- d=0, x=∇
    assert "tradFnMask [1]" (Just False) (C.get tmR 1)   -- x≠∇
    assert "tradFnMask [2]" (Just False) (C.get tmR 2)   -- d≠0
    assert "tradFnMask [3]" (Just True)  (C.get tmR 3)   -- d=0, x=∇

    -- -------------------------------------------------------------------
    -- RecordBatch + Parquet round-trip
    -- -------------------------------------------------------------------

    putStrLn "\n-- RecordBatch construction --"
    let idCol    = C.mk @'Int64   [Just 1, Just 2, Just 3, Just 4]
        priceCol = C.mk @'Float64 [Just 10.5, Just 20.0, Nothing, Just 40.25]
        batch1   = B.fromCols [ B.NamedCol "id"    idCol
                              , B.NamedCol "price" priceCol ]
    assert "batch rows" 4 (B.numRows batch1)
    assert "batch cols" 2 (B.numCols batch1)
    assert "batch col 0 name"  "id"    (B.colName batch1 0)
    assert "batch col 1 name"  "price" (B.colName batch1 1)
    assert "batch col 0 dtype" Int64   (B.colDtype batch1 0)
    assert "batch col 1 dtype" Float64 (B.colDtype batch1 1)

    putStrLn "\n-- Parquet round-trip --"
    let path = "/tmp/hask-arrow-test.parquet"
    exists <- doesFileExist path
    when exists (removeFile path)
    Pq.writeBatches path (batch1 :| [])

    Pq.withReader path 0 $ \r -> do
        Pq.numRows  r >>= assert "parquet rows"          (4 :: Int64)
        Pq.numCols  r >>= assert "parquet cols"          2
        Pq.colName  r 0 >>= assert "parquet col 0 name"  "id"
        Pq.colName  r 1 >>= assert "parquet col 1 name"  "price"
        Pq.colDtype r 0 >>= assert "parquet col 0 dtype" Int64
        Pq.colDtype r 1 >>= assert "parquet col 1 dtype" Float64
        -- streaming: foldBatches counts rows without retaining batches
        Pq.foldBatches r 0 (\acc bch -> pure (acc + B.numRows bch))
          >>= assert "stream row total" (4 :: Int)

    -- Re-open and pull a single batch to verify column extraction.
    Pq.withReader path 0 $ \r -> do
        Just bch <- Pq.nextBatch r
        assert "rt batch rows" 4 (B.numRows bch)
        assert "rt batch cols" 2 (B.numCols bch)
        let idBack    = B.unsafeCol bch 0 :: C.Col 'Int64
            priceBack = B.unsafeCol bch 1 :: C.Col 'Float64
        assert "rt id 0"    (Just 1)     (C.get idBack 0)
        assert "rt id 3"    (Just 4)     (C.get idBack 3)
        assert "rt price 0" (Just 10.5)  (C.get priceBack 0)
        assert "rt price 2" Nothing      (C.get priceBack 2)
        assert "rt price 3" (Just 40.25) (C.get priceBack 3)

    removeFile path

    putStrLn "\nAll tests passed."
