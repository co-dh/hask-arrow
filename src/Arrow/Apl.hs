{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
-- | APL-like operators over Arrow columns, and literal PS.apl translations.
--
-- Demonstrates the one-to-one correspondence between APL primitives
-- and hask-arrow Col operations, following Co-dfns (Aaron Hsu, 2019).
module Arrow.Apl
  ( -- * Implicit IO lifting (mirrors Lean's Colₘ)
    Lft(..)
    -- * APL operators
  , (⍴), (⌷), (⌿), (⍪), (∊), (⍀), (⍳)
  , (.<), (.≠), (.+), (.-), (.×)
  , whereM, set
    -- * Convenience
  , col, cc, ScanFn(..)
    -- * PS.apl translations
  , classifyPrims, tokenStarts, braceDepth
    -- * Co-dfns util.apl
  , computeDepth
  ) where

import Prelude hiding (filter, take, sum, div, abs, product, concat, replicate)

import Arrow.Col (Col(..), ScanFn(..))
import qualified Arrow.Col as C
import Arrow.Dtype (Dtype(..), IsNumeric, IsOrd)

import Data.Char (ord)
import Data.Int  (Int64)

-- ---------------------------------------------------------------------------
-- Implicit IO lifting — Haskell version of Lean's Colₘ.
-- Operators accept both Col d and IO (Col d) transparently.
-- ---------------------------------------------------------------------------

class Lft a (d :: Dtype) | a -> d where lft :: a -> IO (Col d)
instance Lft (Col d)      d where lft = pure
instance Lft (IO (Col d)) d where lft = id

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Char → Int64 code point
cc :: Char -> Int64
cc = fromIntegral . ord

-- | Shorthand: @col [1, -1, 0]@ = @mk \@'Int64 [Just 1, Just (-1), Just 0]@
col :: [Int64] -> IO (Col 'Int64)
col = C.mk @'Int64 . map Just

i64 :: Int -> Int64
i64 = fromIntegral

-- ---------------------------------------------------------------------------
-- APL operators
-- ---------------------------------------------------------------------------

infixl 8 ⍴                          -- n⍴v        fill
infixl 7 ⌷, .×                      -- col[idx]   gather
infixl 6 .+, .-                     -- arithmetic
infixr 5 ⌿                          -- mask/col   compress
infixl 5 ⍪, ∊, ⍳                    -- catenate, membership, index-of
infixl 4 .<, .≠                      -- comparison
infixl 6 ⍀                          -- f⍀col      scan

-- | n⍴v — fill: n copies of v
(⍴) :: Int64 -> Int64 -> IO (Col 'Int64)
n ⍴ v = C.fillInt64 n v

-- | col⌷idx — gather (take by index)
(⌷) :: (Lft a d, Lft b 'Int64) => a -> b -> IO (Col d)
a ⌷ b = do a' <- lft a; b' <- lft b; C.take a' b'

-- | mask⌿col — compress (filter)
(⌿) :: (Lft a 'Bool, Lft b d) => a -> b -> IO (Col d)
m ⌿ c = do m' <- lft m; c' <- lft c; C.filter c' m'

-- | ⍺⍪⍵ — catenate
(⍪) :: (Lft a d, Lft b d) => a -> b -> IO (Col d)
a ⍪ b = do a' <- lft a; b' <- lft b; C.concat a' b'

-- | ⍺∊⍵ — membership
(∊) :: (Lft a d, Lft b d) => a -> b -> IO (Col 'Bool)
a ∊ b = do a' <- lft a; b' <- lft b; C.isIn a' b'

-- | ⍸mask — where (indices of true values)
whereM :: Lft a 'Bool => a -> IO (Col 'Int64)
whereM m = lft m >>= C.where_

-- | ⍺⍳⍵ — dyadic iota (index-of). Returns ≢⍺ for non-matches.
(⍳) :: (Lft a 'Int64, Lft b 'Int64) => a -> b -> IO (Col 'Int64)
a ⍳ b = do
  haystack <- lft a; needles <- lft b
  raw <- C.cast 4 =<< C.indexOf haystack needles  -- int32→int64
  n <- C.len haystack; rn <- C.len raw
  C.fillNull raw =<< C.fillInt64 (i64 rn) (i64 n) -- nulls → ≢⍺

-- | f⍀col — scan
(⍀) :: Lft a 'Int64 => ScanFn -> a -> IO (Col 'Int64)
f ⍀ c = lft c >>= C.scan f

-- | t[i]←v — scatter scalar
set :: Lft a 'Int64 => Col 'Int64 -> a -> Int64 -> IO (Col 'Int64)
set t idx v = do i <- lft idx; C.scatterScalar t i v

-- Arithmetic
(.+) :: (Lft a d, Lft b d, IsNumeric d) => a -> b -> IO (Col d)
a .+ b = do a' <- lft a; b' <- lft b; C.add a' b'
(.-) :: (Lft a d, Lft b d, IsNumeric d) => a -> b -> IO (Col d)
a .- b = do a' <- lft a; b' <- lft b; C.sub a' b'
(.×) :: (Lft a d, Lft b d, IsNumeric d) => a -> b -> IO (Col d)
a .× b = do a' <- lft a; b' <- lft b; C.mul a' b'

-- Comparison
(.<) :: (Lft a d, Lft b d, IsOrd d) => a -> b -> IO (Col 'Bool)
a .< b = do a' <- lft a; b' <- lft b; C.lt a' b'
(.≠) :: (Lft a d, Lft b d) => a -> b -> IO (Col 'Bool)
a .≠ b = do a' <- lft a; b' <- lft b; C.neq a' b'

-- ---------------------------------------------------------------------------
-- Literal PS.apl translations
-- ---------------------------------------------------------------------------

-- PS.apl:89  t[⍸x∊prms]←P
--
-- APL:   t[⍸x∊prms]←P
-- Hask:  set t (whereM (x ∊ prms)) p
classifyPrims :: Col 'Int64 -> Col 'Int64 -> Col 'Int64 -> Int64 -> IO (Col 'Int64)
classifyPrims x prms t p =
  set t (whereM (x ∊ prms)) p                                      -- t[⍸x∊prms]←P

-- PS.apl:77  i←⍸2<⌿0⍪dm
--
-- APL:   i←⍸2<⌿0⍪dm
-- Hask:  whereM (prev .< dm)
tokenStarts :: Col 'Int64 -> IO (Col 'Int64)
tokenStarts dm = do
  dm0  <- col [0] ⍪ dm                                        -- 0⍪dm
  n    <- C.len dm
  prev <- C.slice dm0 0 (i64 n)                               -- ¯1↓0⍪dm
  whereM (prev .< dm)                                              -- ⍸2<⌿0⍪dm

-- PS.apl:98  d←+⍀1 ¯1 0['{}'⍳x]
--
-- APL:   d← +⍀ 1 ¯1 0 ['{}'⍳x]
-- Hask:  SfAdd ⍀ (deltas ⌷ (braces ⍳ x))
braceDepth :: Col 'Int64 -> IO (Col 'Int64)
braceDepth x = do
  let braces = col [cc '{', cc '}']                            -- '{}'
  let deltas = col [1, -1, 0]                                  -- 1 ¯1 0
  SfAdd ⍀ (deltas ⌷ (braces ⍳ x))                             -- +⍀1 ¯1 0['{}'⍳x]

-- ---------------------------------------------------------------------------
-- Co-dfns util.apl  P2D (parent→depth)
-- ---------------------------------------------------------------------------

-- APL: P2D←{p←⍵  d←(≢p)⍴0  (x h)←2⍴⊂⍳≢p
--        _←{ph←p[h] m←h≠ph x←m/x d[x]+←1 x(m/ph)}⍣{0=≢⊃⍺}(x h)  d}
-- Note: Co-dfns roots satisfy p[i]=i; ours use p[i]=¯1, so m←ph≠¯1.
computeDepth :: Col 'Int64 -> Int -> IO (Col 'Int64)
computeDepth p n = go =<< ((,,) <$> (i64 n ⍴ 0) <*> C.iota (i64 n) <*> C.iota (i64 n))
  where                                                        -- d←(≢p)⍴0  x h←⍳≢p
    go (d, x, h) = do
      ph <- p ⌷ h                                             -- ph←p[h]
      pn <- i64 <$> C.len ph
      m  <- ph .≠ (pn ⍴ (-1))                                 -- m←ph≠¯1
      x' <- m ⌿ x                                             -- x←m/x
      h' <- m ⌿ ph                                            -- h←m/ph
      nx <- C.len x'
      if nx == 0 then pure d else do                           -- 0=≢⊃⍺ → stop
        new_ <- (d ⌷ x') .+ (i64 nx ⍴ 1)                     -- d[x]+1
        d'   <- C.scatter d x' new_                            -- d[x]←d[x]+1
        go (d', x', h')
