module System.Random.TF.Gen

import System.Random.TF.Random
import Data.Vect

%default total

--%include C "threefish_block.c"
%include C "idr_mash.h"
%link C "threefish_block.o"
%link C "idr_mash.o"

%dynamic "threefish_block.so"
%dynamic "idr_mash.so"

export
data Block256 = MkBlock256 Bits64 Bits64 Bits64 Bits64
%name Block256 blk,blk'

export
implementation Show Block256 where
  show (MkBlock256 a b c d) = show a ++ "|" ++ show b ++ "|" ++
                              show c ++ "|" ++ show d

export
data BlockIndex =
  Zero | One | Two | Three | Four | Five | Six | Seven

export
implementation Show BlockIndex where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"

export
implementation Cast BlockIndex Nat where
  cast Zero  = 0
  cast One   = 1
  cast Two   = 2
  cast Three = 3
  cast Four  = 4
  cast Five  = 5
  cast Six   = 6
  cast Seven = 7

export
implementation Cast BlockIndex Bits64 where
  cast Zero  = 0
  cast One   = 1
  cast Two   = 2
  cast Three = 3
  cast Four  = 4
  cast Five  = 5
  cast Six   = 6
  cast Seven = 7


export
record TFGen where
  constructor MkTFGen
  tfgen_key : Block256
  tfgen_level : Bits64
  tfgen_position : Bits64
  tfgen_treeposindex : Bits16
  tfgen_blockindex : BlockIndex
  tfgen_block : Block256

%name TFGen gen, gen'

export
implementation Show TFGen where
  show (MkTFGen k lvl pos tpos blki blk) =
    "MkTFGen (" ++ show k ++ ") " ++ show lvl ++ " " ++ show pos ++ " " ++
    show tpos ++ " " ++ show (cast {to=Nat} blki) ++ " (" ++ show blk ++ ")"

export
record TFGenR where
  constructor MkTFGenR
  tfgenr_key : Block256
  tfgenr_level : Bits64
  tfgenr_position : Bits64
  tfgenr_treeposindex : Bits16
  tfgenr_blockindex : BlockIndex

%name TFGenR gen, gen'

getCBlock : Block256 -> IO Ptr
getCBlock (MkBlock256 a b c d) =
  do blkPtr <- foreign FFI_C "alloc_block" (IO Ptr)
     foreign FFI_C "set_a" (Ptr -> Bits64 -> IO ()) blkPtr a
     foreign FFI_C "set_b" (Ptr -> Bits64 -> IO ()) blkPtr b
     foreign FFI_C "set_c" (Ptr -> Bits64 -> IO ()) blkPtr c
     foreign FFI_C "set_d" (Ptr -> Bits64 -> IO ()) blkPtr d
     pure blkPtr

eatBlock : Ptr -> IO Block256
eatBlock blk = do a <- foreign FFI_C "get_a" (Ptr -> IO Bits64) blk
                  b <- foreign FFI_C "get_b" (Ptr -> IO Bits64) blk
                  c <- foreign FFI_C "get_c" (Ptr -> IO Bits64) blk
                  d <- foreign FFI_C "get_d" (Ptr -> IO Bits64) blk
                  foreign FFI_C "free_block" (Ptr -> IO ()) blk
                  pure $ MkBlock256 a b c d

ioMash : Block256 -> Bits64 -> Bits64 -> Bits64 -> Int -> IO Block256
ioMash k i b m o32 =
  do blk_k <- getCBlock k
     blk_c' <- getCBlock (MkBlock256 b i m 0)
     blk_res <- foreign FFI_C "alloc_block" (IO Ptr)
     foreign FFI_C "idr_Threefish_256_Process_Block" (Ptr -> Ptr -> Ptr -> IO ()) blk_k blk_c' blk_res
     foreign FFI_C "free_block" (Ptr -> IO ()) blk_k
     foreign FFI_C "free_block" (Ptr -> IO ()) blk_c'
     eatBlock blk_res

mash : Block256 -> Bits64 -> Bits64 -> Bits64 -> Int -> Block256
mash k i b m o32 = unsafePerformIO (ioMash k i b m o32)

mash' : TFGen -> Bits64 -> Int -> Block256
mash' (MkTFGen k i b _ _ _) m o32 =
  mash k i b m o32

toTFGenR : TFGen -> TFGenR
toTFGenR (MkTFGen key lvl pos tpos bpos _) = MkTFGenR key lvl pos tpos bpos

export
implementation Cast Bits16 Bits64 where
  cast = prim__zextB16_B64

export
implementation Cast (Fin 8) Bits64 where
  cast                                 FZ        = 0
  cast                             (FS FZ)       = 1
  cast                         (FS (FS FZ))      = 2
  cast                     (FS (FS (FS FZ)))     = 3
  cast                 (FS (FS (FS (FS FZ))))    = 4
  cast             (FS (FS (FS (FS (FS FZ)))))   = 5
  cast         (FS (FS (FS (FS (FS (FS FZ))))))  = 6
  cast     (FS (FS (FS (FS (FS (FS (FS FZ))))))) = 7
  cast (FS (FS (FS (FS (FS (FS (FS (FS no)))))))) = absurd no

fromTFGenR : TFGenR -> Maybe TFGen
fromTFGenR (MkTFGenR k i b bi blki) =
  if bi >= 0 && bi <= 64
     then Just (MkTFGen k i b bi blki (mash k (i `prim__subB64` cast blki) b 0 1))
     else Nothing

higher : Bits64 -> Bits32
higher x = prim__truncB64_B32 $ prim__lshrB64 x 32

lower : Bits64 -> Bits32
lower x = prim__truncB64_B32 x




extract : Block256 -> BlockIndex -> Bits32
extract (MkBlock256 a b c d) Zero  = higher a
extract (MkBlock256 a b c d) One   = lower a
extract (MkBlock256 a b c d) Two   = higher b
extract (MkBlock256 a b c d) Three = lower b
extract (MkBlock256 a b c d) Four  = higher c
extract (MkBlock256 a b c d) Five  = lower c
extract (MkBlock256 a b c d) Six   = higher d
extract (MkBlock256 a b c d) Seven = lower d



inc : BlockIndex -> Maybe BlockIndex
inc Zero  = Just One
inc One   = Just Two
inc Two   = Just Three
inc Three = Just Four
inc Four  = Just Five
inc Five  = Just Six
inc Six   = Just Seven
inc Seven = Nothing




makeTFGen : Block256 -> Bits64 -> Bits64 -> Bits16 -> TFGen
makeTFGen k i b bi = MkTFGen k i b bi Zero (mash k i b 0 1)

setBit64 : Bits64 -> Bits64 -> Bits64
setBit64 a i = prim__orB64 a (prim__shlB64 0x1 4)


tfGenNext : TFGen -> (Bits32, TFGen)
tfGenNext (MkTFGen k i b bi blki blk) =
   let next : TFGen =
         case inc blki of
           Nothing => if i < (maxBound `prim__subB64` 1)
                        then makeTFGen k (i+1) b bi
                        else
                          if bi < 64
                            then makeTFGen k 0 (setBit64 b (prim__zextB16_B64 bi)) (bi+1)
                            else makeTFGen (mash k maxBound b 0 0) 0 0 0
           Just blki' => MkTFGen k (i+1) b bi blki' blk
       val : Bits32 = extract blk blki
   in (val, next)

-- Assumes that Idris's Int is always at most 64 bits
tfGenNext' : TFGen -> (Int, TFGen)
tfGenNext' gen =
  let (left, gen')   = tfGenNext gen
      (right, gen'') = tfGenNext gen'
      i              = prim__shlB64 (prim__zextB32_B64 left) 32 `prim__orB64` prim__zextB32_B64 right
  in
    (prim__truncB64_Int i, gen'')


tfGenSplit : TFGen -> (TFGen, TFGen)
tfGenSplit (MkTFGen k i b bi x y) =
  if bi == 64
    then let k' = mash' (MkTFGen k i b bi x y) 0 0 in
         (makeTFGen k' 0 0 1, makeTFGen k' 0 1 1)
    else let bi' = bi + 1 in
         let b' = setBit64 b (prim__zextB16_B64 bi) in
         (makeTFGen k i b bi', makeTFGen k i b' bi')


seedTFGen : Block256 -> TFGen
seedTFGen blk = makeTFGen blk 0 0 0


mkSeed : IO Block256
mkSeed =  do seed <- foreign FFI_C "seed_block" (IO Ptr)
             eatBlock seed


implementation RandomGen TFGen where
  next = tfGenNext
  split = tfGenSplit


-- -}
