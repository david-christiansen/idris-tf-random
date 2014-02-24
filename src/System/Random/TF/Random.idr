module System.Random.TF.Random

-- | A somewhat anemic RandomGen class
class RandomGen r where
  next : r -> (Bits32, r)
  split : r -> (r, r)



