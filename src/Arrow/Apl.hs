module Arrow.Apl
  ( -- * APL operators
    (⍴), (⌷), (⌿), (⍪), (∊), (⍀), (⍳)
  , (.<), (.>), (.≠), (.=), (.∧), (.∨)
  , whereM, set, b2i, notC, rot, xorScan, pairFind
    -- * Convenience
  , col, cc, ScanFn(..)
    -- * PS.apl translations
  , classifyPrims, tokenStarts, tokenEnds, braceDepth, maskSpace
  , initKind, nsDepth
  , classifyDiamonds, shiftDepth, extendDot, extendHiMinus, extendExp
  , tokenizeRuns, varMask, classifyDoubled, classifyFormals
  , dfnFormalCheck, tradFnMask, tradFnBalance
  , classifyColons, classifySystemVar, structuralFilter
  , classifyAtoms, classifyPrimsOnly, parseTradFn
  , classifyKinds, symbolize
    -- * Co-dfns util.apl
  , computeDepth
  ) where

import Prelude hiding (filter, take, sum, product, concat)

import Arrow.Col (Col(..), ScanFn(..), (==.), (/=.), (<.), (>.), (&&.), (||.), notC)
import qualified Arrow.Col as C
import Arrow.Dtype (Dtype(..), IsOrd)

import Data.Char (ord)
import Data.Int  (Int64)

cc :: Char -> Int64
cc = fromIntegral . ord

col :: [Int64] -> Col 'Int64
col = C.mk @'Int64 . map Just

i64 :: Int -> Int64
i64 = fromIntegral

-- | Bool→Int64 cast (APL's implicit boolean→integer)
b2i :: Col 'Bool -> Col 'Int64
b2i = C.cast Int64

infixl 8 ⍴                          -- n⍴v        fill
infixl 7 ⌷                          -- col[idx]   gather
infixr 5 ⌿                          -- mask/col   compress
infixl 5 ⍪, ∊, ⍳                    -- catenate, membership, index-of
infixl 4 .<, .>, .≠, .=             -- comparison
infixl 3 .∧                         -- logical and
infixl 2 .∨                         -- logical or
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
  where raw = C.cast Int64 (C.indexOf a b); n = C.len a; rn = C.len raw

(⍀) :: ScanFn -> Col 'Int64 -> Col 'Int64                             -- f⍀col  scan
(⍀) = C.scan

set :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64                -- t[i]←v  scatter
set = C.scatterScalar

(.<) :: IsOrd d => Col d -> Col d -> Col 'Bool                         -- ⍺<⍵
(.<) = (<.)
(.>) :: IsOrd d => Col d -> Col d -> Col 'Bool                         -- ⍺>⍵
(.>) = (>.)
(.≠) :: Col d -> Col d -> Col 'Bool                                    -- ⍺≠⍵
(.≠) = (/=.)
(.=) :: Col d -> Col d -> Col 'Bool                                    -- ⍺=⍵
(.=) = (==.)
(.∧) :: Col 'Bool -> Col 'Bool -> Col 'Bool                            -- ⍺∧⍵
(.∧) = (&&.)
(.∨) :: Col 'Bool -> Col 'Bool -> Col 'Bool                            -- ⍺∨⍵
(.∨) = (||.)

-- | n⌽col — rotate (left for positive n, right for negative)
rot :: Int -> Col d -> Col d
rot n c
  | n' == 0   = c
  | otherwise = C.concat (C.slice c (i64 n') (i64 (l - n'))) (C.slice c 0 (i64 n'))
  where l  = C.len c
        n' = n `mod` l

-- | ≠⍀ — running parity scan. Result is true where cumulative count of trues is odd.
xorScan :: Col 'Bool -> Col 'Bool
xorScan m = (s - half * two) .≠ (i64 n ⍴ 0)                           -- cumsum mod 2
  where s    = C.scan SfAdd (b2i m)
        n    = C.len s
        two  = i64 n ⍴ 2
        half = s C./. two

-- | 'ab'⍷x — find positions where the 2-character substring @c1 c2@ starts in @x@.
pairFind :: Int64 -> Int64 -> Col 'Int64 -> Col 'Bool
pairFind c1 c2 x = (x .= (n ⍴ c1)) .∧ shifted                         -- (x[i]=c1)∧(x[i+1]=c2)
  where n       = i64 (C.len x)
        second  = x .= (n ⍴ c2)
        shifted = C.concat (C.slice second 1 (n - 1))
                           (C.mk @'Bool [Just False])

-- PS.apl:89  t[⍸x∊prms]←P  (and line 47: t[⍸'⋄'=IN[pos]]←Z uses the same pattern)
classifyPrims :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyPrims x prms t p =
  set t (whereM (x ∊ prms)) p                                         -- t[⍸x∊prms]←P

-- PS.apl:47  t[⍸'⋄'=IN[pos]]←Z
classifyDiamonds :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyDiamonds inp pos t z =
  set t (whereM ((inp ⌷ pos) ∊ col [cc '⋄'])) z                       -- t[⍸'⋄'=IN[pos]]←Z

-- PS.apl:77  i←⍸2<⌿0⍪dm — rising edges of a boolean mask
tokenStarts :: Col 'Int64 -> Col 'Int64
tokenStarts dm = whereM (prev .< dm)                                   -- ⍸2<⌿0⍪dm
  where dm0  = col [0] ⍪ dm                                           -- 0⍪dm
        prev = C.slice dm0 0 (i64 (C.len dm))                         -- ¯1↓0⍪dm

-- PS.apl:43  ⍸2>⌿msk⍪0 — falling edges of a boolean mask
tokenEnds :: Col 'Int64 -> Col 'Int64
tokenEnds msk = whereM (next .< msk)                                   -- ⍸2>⌿msk⍪0
  where msk0 = msk ⍪ col [0]                                          -- msk⍪0
        next = C.slice msk0 1 (i64 (C.len msk))                       -- 1↓msk⍪0

-- PS.apl:98  d←+⍀1 ¯1 0['{}'⍳x] — brace nesting depth
braceDepth :: Col 'Int64 -> Col 'Int64
braceDepth x = SfAdd ⍀ (deltas ⌷ (braces ⍳ x))                         -- +⍀1 ¯1 0['{}'⍳x]
  where braces = col [cc '{', cc '}']                                 -- '{}'
        deltas = col [1, -1, 0]                                       -- 1 ¯1 0

-- PS.apl:101  d←¯1⌽d — shift depth right by one (so } goes with its child)
shiftDepth :: Col 'Int64 -> Col 'Int64
shiftDepth = rot (-1)                                                  -- ¯1⌽d

-- PS.apl:58  x←' '@{t≠0}IN[pos] — replace chars with space where token type is nonzero
maskSpace :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Col 'Int64
maskSpace inp pos t =
  C.ifElse (t .≠ (n ⍴ 0)) (n ⍴ cc ' ') (inp ⌷ pos)                    -- ' '@{t≠0}IN[pos]
  where n = i64 (C.len t)

-- PS.apl:61  dm∨←('.'=x)∧(¯1⌽dm)∨1⌽dm — extend digit mask to dots between digits
extendDot :: Col 'Int64 -> Col 'Bool -> Col 'Bool
extendDot x dm = dm .∨ ((x .= (n ⍴ cc '.')) .∧ (rot (-1) dm .∨ rot 1 dm))
  where n = i64 (C.len x)                                              -- dm∨←('.'=x)∧(¯1⌽dm)∨1⌽dm

-- PS.apl:63  dm∨←('¯'=x)∧1⌽dm — extend digit mask for leading high-minus
extendHiMinus :: Col 'Int64 -> Col 'Bool -> Col 'Bool
extendHiMinus x dm = dm .∨ ((x .= (n ⍴ cc '¯')) .∧ rot 1 dm)
  where n = i64 (C.len x)                                              -- dm∨←('¯'=x)∧1⌽dm

-- PS.apl:67  dm∨←(x∊'Ee')∧(¯1⌽dm)∧1⌽dm — extend for exponent markers between digits
extendExp :: Col 'Int64 -> Col 'Bool -> Col 'Bool
extendExp x dm = dm .∨ (xE .∧ rot (-1) dm .∧ rot 1 dm)
  where xE = x ∊ col [cc 'E', cc 'e']                                  -- dm∨←(x∊'Ee')∧(¯1⌽dm)∧1⌽dm

-- PS.apl:77/80  tokenize runs in a boolean mask into a token type.
-- Pattern used for numbers, variables, keywords — any contiguous run.
tokenizeRuns :: Col 'Int64 -> Col 'Bool -> Int64 -> Col 'Int64
tokenizeRuns t msk tk = set t (tokenStarts (b2i msk)) tk               -- t[⍸2<⌿0⍪msk]←tk

-- PS.apl:80  msk←dm<(t=0)∧x∊alp,num — variable characters
-- (not in digit mask, token type still 0, character is letter or digit)
varMask :: Col 'Bool -> Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Col 'Bool
varMask dm t x alpNum =
  notC dm .∧ (t .= (i64 (C.len x) ⍴ 0)) .∧ (x ∊ alpNum)                -- dm<(t=0)∧x∊alp,num

-- PS.apl:86  t[⍸msk<(¯1⌽msk)<x∊'⍺⍵']←A — single ⍺/⍵ formals (not doubled ⍺⍺/⍵⍵)
classifyFormals :: Col 'Int64 -> Col 'Bool -> Col 'Int64 -> Int64 -> Col 'Int64
classifyFormals t doubledMsk x a =
  set t (whereM single) a                                              -- t[⍸msk<(¯1⌽msk)<x∊'⍺⍵']←A
  where isFormal = x ∊ col [cc '⍺', cc '⍵']                           -- x∊'⍺⍵'
        single   = notC doubledMsk .∧ notC (rot (-1) doubledMsk) .∧ isFormal

-- PS.apl:85  msk←('⍺⍺'⍷x)∨'⍵⍵'⍷x — doubled formals (⍺⍺ or ⍵⍵)
classifyDoubled :: Col 'Int64 -> Col 'Bool
classifyDoubled x = pairFind (cc '⍺') (cc '⍺') x .∨ pairFind (cc '⍵') (cc '⍵') x

-- PS.apl:104  msk←(d=0)∧(t∊A P)∧x∊'⍺⍵' — formals referenced outside a dfn
dfnFormalCheck :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Col 'Bool
dfnFormalCheck d t x aP =
  (d .= (n ⍴ 0)) .∧ (t ∊ aP) .∧ (x ∊ col [cc '⍺', cc '⍵'])             -- (d=0)∧(t∊A P)∧x∊'⍺⍵'
  where n = i64 (C.len d)

-- PS.apl:108  tm←(d=0)∧'∇'=x — top-level trad-fn markers
tradFnMask :: Col 'Int64 -> Col 'Int64 -> Col 'Bool
tradFnMask d x = (d .= (n ⍴ 0)) .∧ (x .= (n ⍴ cc '∇'))                 -- (d=0)∧'∇'=x
  where n = i64 (C.len d)

-- PS.apl:110  ¯1⌽≠⍀tm — trad-fn balance (nonzero at position 0 means unbalanced)
tradFnBalance :: Col 'Bool -> Col 'Bool
tradFnBalance tm = rot (-1) (xorScan tm)                               -- ¯1⌽≠⍀tm

-- PS.apl:118  t[⍸msk←2<⌿tm⍪0]←T ⋄ d+←msk<tm — parse trad-fn into T type
parseTradFn :: Col 'Int64 -> Col 'Int64 -> Col 'Bool -> Int64 -> (Col 'Int64, Col 'Int64)
parseTradFn t d tm tType = (t', d')
  where n    = i64 (C.len tm)
        tmI  = b2i tm                                                  -- tm as Int64
        next = C.slice (tmI ⍪ col [0]) 1 n                            -- 1↓tm⍪0
        msk  = next .< tmI                                             -- 2<⌿tm⍪0  (falling edge mask)
        t'   = set t (whereM msk) tType                                -- t[⍸msk]←T
        d'   = d + b2i (msk .< tm)                                     -- d+←msk<tm

-- PS.apl:125  t[⍸(':'=x)∧t=0]←K — colon-prefixed keyword start markers
classifyColons :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyColons t x kType =
  set t (whereM ((x .= (n ⍴ cc ':')) .∧ (t .= (n ⍴ 0)))) kType         -- t[⍸(':'=x)∧t=0]←K
  where n = i64 (C.len x)

-- PS.apl:130  si←⍸('⎕'=x)∧1⌽t=V — system variable starts (⎕ followed by a variable)
classifySystemVar :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifySystemVar t x vType =
  whereM ((x .= (n ⍴ cc '⎕')) .∧ (rot 1 (t .= (n ⍴ vType))))           -- ⍸('⎕'=x)∧1⌽t=V
  where n = i64 (C.len x)

-- PS.apl:133  d tm t pos end⌿⍨←⊂(t≠0)∨x∊'()[]{};'
-- Mask used to filter out characters we no longer need from the tree
structuralFilter :: Col 'Int64 -> Col 'Int64 -> Col 'Bool
structuralFilter t x =
  (t .≠ (n ⍴ 0)) .∨ (x ∊ col (map cc "()[]{};"))                       -- (t≠0)∨x∊'()[]{};'
  where n = i64 (C.len t)

-- PS.apl:89a  t[⍸x∊syna]←A — atom classification (first half of line 89)
classifyAtoms :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyAtoms t x syna aType = set t (whereM (x ∊ syna)) aType         -- t[⍸x∊syna]←A

-- PS.apl:89b  t[⍸dm<x∊prms]←P — primitives (only where not a digit-mask char)
classifyPrimsOnly :: Col 'Int64 -> Col 'Bool -> Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
classifyPrimsOnly t dm x prms pType =
  set t (whereM (notC dm .∧ (x ∊ prms))) pType                         -- t[⍸dm<x∊prms]←P

-- PS.apl:197  k←2×t∊F — initialize kind: 2 for functions, 0 otherwise
initKind :: Col 'Int64 -> Int64 -> Col 'Int64
initKind t f = (n ⍴ 2) * b2i (t ∊ col [f])                             -- 2×t∊F
  where n = i64 (C.len t)

-- PS.apl:200-204  kind classification for atoms, functions, operators, etc.
-- k[⍸ condition ]← kindVal  — set kind to kindVal where condition holds.
-- This is the generic pattern; each line picks a different condition.
classifyKinds :: Col 'Int64 -> Col 'Bool -> Int64 -> Col 'Int64
classifyKinds k cond kind = set k (whereM cond) kind                   -- k[⍸cond]←kind

-- PS.apl:211  n←-sym⍳n — rewrite names as negated symbol-table indices
symbolize :: Col 'Int64 -> Col 'Int64 -> Col 'Int64
symbolize sym n = negate (sym ⍳ n)                                     -- -sym⍳n

-- PS.apl:283  d←+⍀(t[x]=M)+-t[x]=-M — namespace nesting depth
nsDepth :: Col 'Int64 -> Col 'Int64 -> Int64 -> Col 'Int64
nsDepth t x m =
  SfAdd ⍀ (b2i (tx .= (n ⍴ m)) - b2i (tx .= (n ⍴ (-m))))               -- +⍀(t[x]=M)+-t[x]=-M
  where tx = t ⌷ x                                                     -- t[x]
        n  = i64 (C.len tx)

-- Co-dfns util.apl  P2D (parent→depth)
-- APL: P2D←{p←⍵  d←(≢p)⍴0  (x h)←2⍴⊂⍳≢p
--        _←{ph←p[h] m←h≠ph x←m/x d[x]+←1 x(m/ph)}⍣{0=≢⊃⍺}(x h)  d}
-- Note: Co-dfns roots satisfy p[i]=i; ours use p[i]=¯1, so m←ph≠¯1.
computeDepth :: Col 'Int64 -> Int -> Col 'Int64
computeDepth p n = go (i64 n ⍴ 0) (C.iota (i64 n)) (C.iota (i64 n))
  where                                                                -- d←(≢p)⍴0  x h←⍳≢p
    go d x h
      | C.len x' == 0 = d                                              -- 0=≢⊃⍺ → stop
      | otherwise      = go d' x' h'
      where ph   = p ⌷ h                                              -- ph←p[h]
            pn   = i64 (C.len ph)
            m    = ph .≠ (pn ⍴ (-1))                                   -- m←ph≠¯1
            x'   = m ⌿ x                                               -- x←m/x
            h'   = m ⌿ ph                                              -- h←m/ph
            new_ = (d ⌷ x') + (i64 (C.len x') ⍴ 1)                     -- d[x]+1
            d'   = C.scatter d x' new_                                  -- d[x]←d[x]+1
