module System.Random.TF.Random

%default total

-- | A somewhat anemic RandomGen interface
interface RandomGen r where
  next : r -> (Bits32, r)
  split : r -> (r, r)


interface Random a where
  randomR : RandomGen g => (a,a) -> g -> (a,g)

  random  : RandomGen g => g -> (a, g)

mapFst : (a -> c) -> (a, b) -> (c, b)
mapFst g (x, y) = (g x, y)

boundsWrap : (Ord a, Num a) => (a -> g -> (a, g)) -> (a, a) -> g -> (a, g)
boundsWrap f (l, h) rng = if l == h
                             then (l, rng)
                             else if l > h then mapFst (h +) $ f (l - h) rng
                                           else mapFst (l +) $ f (h - l) rng

word32Mask : Bits32 -> Bits32
word32Mask w =
  (f (f (f (f (f w 1) 2) 4) 8) 16)
  where
   f : Bits32 -> Bits32 -> Bits32
   f w n = w `prim__orB32` (w `prim__lshrB32` n)

-- Inspired by Java's java.util.Random.
-- This version avoids division modulo.
{-# INLINE randomWord32' #-}
randomBits32' : RandomGen g => Bits32 -> g -> (Bits32, g)
randomBits32' k =
  if k' == 0 -- Case 1: k is the maxBound.
    then next
    else if (k' `prim__andB32` k) == 0 -- Case 2: k' is a power of two; k is a bit mask.
           then \rng => mapFst (prim__andB32 k) (next rng)
           else loop -- Case 3: The general case. Case 3 subsumes Case 2,
                     -- and Case 2 subsumes Case 1. Cases 1 and 2 are
                     -- there for efficiency.
  where
   k' : Bits32
   k' = k + 1
   mask : Bits32
   mask = word32Mask k
   loop rng =
      let (x, rng') = next rng in
      let x'        = x `prim__andB32` mask in
      if x' <= k then (x', rng') else assert_total $ loop rng'

makeBits64 : Bits32 -> Bits32 -> Bits64
makeBits64 w1 w2 = (prim__shlB64 (prim__zextB32_B64 w1) 32) `prim__orB64` (prim__zextB32_B64 w2)


-- Works similarly to randomWord32'
randomBits64' : RandomGen g => Bits64 -> g -> (Bits64, g)
randomBits64' k =
  if k <= m32   -- Case 1: The range fits in 32 bits.
    then \rng => mapFst prim__zextB32_B64 $ randomBits32' (prim__truncB64_B32 k) rng
    else if k' == 0   -- Case 2: (l,h) is the full range. This case should
                      -- probably be removed
           then \rng =>
             let (x1, rng')  = next rng in
             let (x2, rng'') = next rng' in
             (makeBits64 x1 x2, rng'')
           else if (k' `prim__andB64` k) == 0   -- Case 3: k' is a power of two; k is a bit mask.
                  then \rng =>
                    let (x1, rng')  = next rng in
                    let (x2, rng'') = next rng' in
                    (makeBits64 x1 x2 `prim__andB64` k, rng'')
                  else loop   -- Case 4: The general case. Case 4 subsumes Cases 1 and 3,
                              -- and Case 3 subsumes Case 2. Cases 1, 2 and 3 are
                              -- there for efficiency.

  where
  m32 : Bits64
  m32 = prim__zextB32_B64 maxBound
  k' : Bits64
  k' = k + 1
  mask : Bits32
  mask = word32Mask (prim__truncB64_B32 (prim__lshrB64 k 32))
  loop rng =
    let (x1, rng')  = next rng in
    let (x2, rng'') = next rng' in
    let x = makeBits64 (prim__andB32 x1 mask) x2 in
    if x <= k
      then (x, rng'')
      else assert_total $ loop rng''


randomBits32 : RandomGen g => (Bits32, Bits32) -> g -> (Bits32, g)
randomBits32 (l, h) rng = boundsWrap randomBits32' (l, h) rng


randomBits64 : RandomGen g => (Bits64, Bits64) -> g -> (Bits64, g)
randomBits64 (l, h) rng = boundsWrap randomBits64' (l, h) rng

randomBounded : (RandomGen g, Random a, MinBound a, MaxBound a) => g -> (a, g)
randomBounded = randomR (minBound, maxBound)

implementation Random Bits32 where
  randomR = randomBits32
  random = next

implementation Random Bits64 where
  randomR = randomBits64
  random rng = let (h, rng') = next rng in
               let (l, rng'') = next rng' in
               (makeBits64 h l, rng'')


implementation Random Int where
  randomR (l, h) rng = if h < l
                          then assert_total $ randomR (h, l) rng
                          else let d = h - l in
                               mapFst ((l+) . prim__zextB32_Int) $
                               randomR (the Bits32 0, prim__truncInt_B32 d) rng

  random rng = mapFst prim__truncB64_Int $ random rng

implementation Random Bool where
  randomR (b1, b2) rng = if b1 == b2
                           then (b1, snd (next rng))
                           else mapFst intToBool $ randomR (the Int 0, 1) rng
  random rng = mapFst intToBool $ randomR (the Int 0, 1) rng

implementation Random Char where
  randomR (l, h) rng = mapFst (cast {from=Int} {to=Char}) $
                       randomR (cast {to=Int} l, cast {to=Int} h) rng
  random rng = mapFst (cast {from=Int} {to=Char}) $
               random rng

-- -}
-- -}
-- -}
-- -}
