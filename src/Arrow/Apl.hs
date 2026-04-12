-- | APL-like operators over Arrow columns, and literal PS.apl translations.
--
-- Demonstrates the one-to-one correspondence between APL primitives
-- and hask-arrow Col operations, following Co-dfns (Aaron Hsu, 2019).
module Arrow.Apl
  ( -- * APL operators
    (⍴), (⌷), (⌿), (⍪), (∊), (⍀), (⍳)
  , (.<), (.≠), (.=)
  , whereM, set, b2i
    -- * Convenience
  , col, cc, ScanFn(..)
    -- * PS.apl translations
  , classifyPrims, tokenStarts, braceDepth, tokenEnds, maskSpace, initKind, nsDepth
    -- * Co-dfns util.apl
  , computeDepth
  ) where

import Prelude hiding (filter, take, sum, abs, product, concat, replicate)

import Arrow.Col (Col(..), ScanFn(..), (==.), (/=.), (<.))
import qualified Arrow.Col as C
import Arrow.Dtype (Dtype(..), IsOrd)

import Data.Char (ord)
import Data.Int  (Int64)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

cc :: Char -> Int64
cc = fromIntegral . ord

col :: [Int64] -> Col 'Int64
col = C.mk @'Int64 . map Just

i64 :: Int -> Int64
i64 = fromIntegral

-- | Bool→Int64 cast (APL's implicit boolean→integer)
b2i :: Col 'Bool -> Col 'Int64
b2i = C.cast 4

-- ---------------------------------------------------------------------------
-- APL operators — thin aliases over Col.hs infix ops
-- ---------------------------------------------------------------------------

infixl 8 ⍴                          -- n⍴v        fill
infixl 7 ⌷                          -- col[idx]   gather
infixr 5 ⌿                          -- mask/col   compress
infixl 5 ⍪, ∊, ⍳                    -- catenate, membership, index-of
infixl 4 .<, .≠, .=                  -- comparison
infixl 6 ⍀                          -- f⍀col      scan

(⍴) :: Int64 -> Int64 -> Col 'Int64                                    -- n⍴v  fill
(⍴) = C.fillInt64
(⌷) :: Col d -> Col 'Int64 -> Col d                                    -- col⌷idx  gather
(⌷) = C.take
(⌿) :: Col 'Bool -> Col d -> Col d                                     -- mask⌿col  compress
(⌿) = flip C.filter
(⍪) :: Col d -> Col d -> Col d                                         -- ⍺⍪⍵  catenate
(⍪) = C.concat
(∊) :: Col d -> Col d -> Col 'Bool                                     -- ⍺∊⍵  membership
(∊) = C.isIn

whereM :: Col 'Bool -> Col 'Int64                                      -- ⍸mask  where
whereM = C.where_

(⍳) :: Col 'Int64 -> Col 'Int64 -> Col 'Int64                         -- ⍺⍳⍵  index-of
a ⍳ b = C.fillNull raw (C.fillInt64 (i64 rn) (i64 n))                -- nulls → ≢⍺
  where raw = C.cast 4 (C.indexOf a b); n = C.len a; rn = C.len raw

(⍀) :: ScanFn -> Col 'Int64 -> Col 'Int64                             -- f⍀col  scan
(⍀) = C.scan

set :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64                -- t[i]←v  scatter
set = C.scatterScalar

(.<) :: IsOrd d => Col d -> Col d -> Col 'Bool                         -- ⍺<⍵
(.<) = (<.)
(.≠) :: Col d -> Col d -> Col 'Bool                                    -- ⍺≠⍵
(.≠) = (/=.)
(.=) :: Col d -> Col d -> Col 'Bool                                    -- ⍺=⍵
(.=) = (==.)

-- ---------------------------------------------------------------------------
-- Literal PS.apl translations
-- ---------------------------------------------------------------------------

-- PS.apl:89  t[⍸x∊prms]←P
classifyPrims :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyPrims x prms t p =
  set t (whereM (x ∊ prms)) p                                      -- t[⍸x∊prms]←P

-- PS.apl:77  i←⍸2<⌿0⍪dm
tokenStarts :: Col 'Int64 -> Col 'Int64
tokenStarts dm = whereM (prev .< dm)                                    -- ⍸2<⌿0⍪dm
  where dm0  = col [0] ⍪ dm                                        -- 0⍪dm
        prev = C.slice dm0 0 (i64 (C.len dm))                      -- ¯1↓0⍪dm

-- PS.apl:98  d←+⍀1 ¯1 0['{}'⍳x]
braceDepth :: Col 'Int64 -> Col 'Int64
braceDepth x = SfAdd ⍀ (deltas ⌷ (braces ⍳ x))                         -- +⍀1 ¯1 0['{}'⍳x]
  where braces = col [cc '{', cc '}']                               -- '{}'
        deltas = col [1, -1, 0]                                     -- 1 ¯1 0

-- PS.apl:43  ⍸2>⌿msk⍪0
tokenEnds :: Col 'Int64 -> Col 'Int64
tokenEnds msk = whereM (next .< msk)                                    -- ⍸2>⌿msk⍪0
  where msk0 = msk ⍪ col [0]                                       -- msk⍪0
        next = C.slice msk0 1 (i64 (C.len msk))                    -- 1↓msk⍪0

-- PS.apl:58  x←' '@{t≠0}IN[pos]
maskSpace :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Col 'Int64
maskSpace inp pos t =
  C.ifElse (t .≠ (n ⍴ 0)) (n ⍴ cc ' ') (inp ⌷ pos)                    -- ' '@{t≠0}IN[pos]
  where n = i64 (C.len t)

-- PS.apl:197  k←2×t∊F
initKind :: Col 'Int64 -> Int64 -> Col 'Int64
initKind t f = (n ⍴ 2) * b2i (t ∊ col [f])                             -- 2×t∊F
  where n = i64 (C.len t)

-- PS.apl:283  d←+⍀(t[x]=M)+-t[x]=-M
nsDepth :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
nsDepth t x m =
  SfAdd ⍀ (b2i (tx .= (n ⍴ m)) - b2i (tx .= (n ⍴ (-m))))              -- +⍀(t[x]=M)+-t[x]=-M
  where tx = t ⌷ x                                                 -- t[x]
        n  = i64 (C.len tx)

-- ---------------------------------------------------------------------------
-- Co-dfns util.apl  P2D (parent→depth)
-- ---------------------------------------------------------------------------

-- APL: P2D←{p←⍵  d←(≢p)⍴0  (x h)←2⍴⊂⍳≢p
--        _←{ph←p[h] m←h≠ph x←m/x d[x]+←1 x(m/ph)}⍣{0=≢⊃⍺}(x h)  d}
-- Note: Co-dfns roots satisfy p[i]=i; ours use p[i]=¯1, so m←ph≠¯1.
computeDepth :: Col 'Int64 -> Int -> Col 'Int64
computeDepth p n = go (i64 n ⍴ 0) (C.iota (i64 n)) (C.iota (i64 n))
  where                                                            -- d←(≢p)⍴0  x h←⍳≢p
    go d x h
      | C.len x' == 0 = d                                         -- 0=≢⊃⍺ → stop
      | otherwise      = go d' x' h'
      where ph   = p ⌷ h                                          -- ph←p[h]
            pn   = i64 (C.len ph)
            m    = ph .≠ (pn ⍴ (-1))                               -- m←ph≠¯1
            x'   = m ⌿ x                                           -- x←m/x
            h'   = m ⌿ ph                                          -- h←m/ph
            new_ = (d ⌷ x') + (i64 (C.len x') ⍴ 1)                -- d[x]+1
            d'   = C.scatter d x' new_                              -- d[x]←d[x]+1
