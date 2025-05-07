{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
module Grammar where 

import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

newtype HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: t4 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: t5 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: t10 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: t11 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: t12 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: t13 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: t14 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: t15 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: t16 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: t17 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: t18 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: t19 -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> t19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyInTok :: (Token) -> (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x28\x00\x00\x40\x00\x00\x00\x28\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x94\x02\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x04\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x28\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x20\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x10\x00\x00\x08\x00\x00\x00\x28\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x20\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x01\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x54\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x20\x00\x00\x10\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x20\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Queries","queriesRest","query","condition","selectList","selectListRest","selectItem","task","product","drop","copy","leftMerge","csv","string","columnIndex","filename","SELECT","WHERE","DO","','","GET","COLUMN","OF","'!='","IS","NOT","EMPTYCOL","EMPTY","READ","AND","OR","PRODUCT","TIMES","DROP","IN","PERMUTE","COPY","WITH","LEFT","MERGE","TO","ON","FILENAME","PRINT","int","var","'='","'=='","'!=='","'('","')'","';'","%eof"]
        bit_start = st               Prelude.* 56
        bit_end   = (st Prelude.+ 1) Prelude.* 56
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..55]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\x00\x00\x01\x00\x00\x00\xe2\xff\xff\xff\xf1\xff\xff\xff\x17\x00\x00\x00\x2f\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x1d\x00\x00\x00\x2c\x00\x00\x00\x37\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x2d\x00\x00\x00\x4d\x00\x00\x00\x4c\x00\x00\x00\x3a\x00\x00\x00\x32\x00\x00\x00\x0f\x00\x00\x00\x40\x00\x00\x00\x3b\x00\x00\x00\x0f\x00\x00\x00\x3c\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x41\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x42\x00\x00\x00\x57\x00\x00\x00\x44\x00\x00\x00\x46\x00\x00\x00\x51\x00\x00\x00\x0f\x00\x00\x00\x4b\x00\x00\x00\xff\xff\xff\xff\xf9\xff\xff\xff\x5c\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x48\x00\x00\x00\x49\x00\x00\x00\x62\x00\x00\x00\x62\x00\x00\x00\x54\x00\x00\x00\x4a\x00\x00\x00\x4e\x00\x00\x00\xfe\xff\xff\xff\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x02\x00\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x58\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x59\x00\x00\x00\x5b\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x5f\x00\x00\x00\x0f\x00\x00\x00\x61\x00\x00\x00\x60\x00\x00\x00\x63\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2b\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x00\x00\x75\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x82\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x6b\x00\x00\x00\x7d\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\xe7\xff\xff\xff\xe4\xff\xff\xff\xe6\xff\xff\xff\xe5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\xdd\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xfd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xeb\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\xdf\xff\xff\xff\x00\x00\x00\x00\xe2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xff\xff\xee\xff\xff\xff\xf5\xff\xff\xff\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\xf2\xff\xff\xff\xef\xff\xff\xff\xf1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xff\xff\xe3\xff\xff\xff\xf3\xff\xff\xff\xdc\xff\xff\xff\xf4\xff\xff\xff\x00\x00\x00\x00\xfa\xff\xff\xff\x00\x00\x00\x00\xde\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x08\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x24\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0c\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x19\x00\x00\x00\x22\x00\x00\x00\x0c\x00\x00\x00\x23\x00\x00\x00\x06\x00\x00\x00\x0f\x00\x00\x00\x23\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x00\x00\x04\x00\x00\x00\x12\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x17\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x25\x00\x00\x00\x22\x00\x00\x00\x24\x00\x00\x00\x22\x00\x00\x00\x18\x00\x00\x00\x23\x00\x00\x00\x04\x00\x00\x00\x06\x00\x00\x00\x13\x00\x00\x00\x22\x00\x00\x00\x1b\x00\x00\x00\x06\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x11\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x1b\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x0c\x00\x00\x00\x16\x00\x00\x00\x06\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x22\x00\x00\x00\x22\x00\x00\x00\x19\x00\x00\x00\x19\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x23\x00\x00\x00\x0e\x00\x00\x00\x1b\x00\x00\x00\x22\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x23\x00\x00\x00\x19\x00\x00\x00\x1b\x00\x00\x00\x23\x00\x00\x00\x0e\x00\x00\x00\x23\x00\x00\x00\x1a\x00\x00\x00\x0f\x00\x00\x00\x23\x00\x00\x00\x23\x00\x00\x00\x0e\x00\x00\x00\x06\x00\x00\x00\x0f\x00\x00\x00\x23\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x0d\x00\x00\x00\x0f\x00\x00\x00\x0d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x00\x00\xff\xff\xff\xff\x0e\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x3b\x00\x00\x00\x04\x00\x00\x00\x3c\x00\x00\x00\x05\x00\x00\x00\x3d\x00\x00\x00\x14\x00\x00\x00\x20\x00\x00\x00\x31\x00\x00\x00\x4f\x00\x00\x00\x38\x00\x00\x00\x54\x00\x00\x00\x53\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x28\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x29\x00\x00\x00\x13\x00\x00\x00\x19\x00\x00\x00\x39\x00\x00\x00\x39\x00\x00\x00\x39\x00\x00\x00\x3e\x00\x00\x00\x3f\x00\x00\x00\x40\x00\x00\x00\x4b\x00\x00\x00\x1a\x00\x00\x00\x06\x00\x00\x00\x21\x00\x00\x00\x32\x00\x00\x00\x50\x00\x00\x00\x47\x00\x00\x00\x43\x00\x00\x00\x1e\x00\x00\x00\x29\x00\x00\x00\x64\x00\x00\x00\x39\x00\x00\x00\x0f\x00\x00\x00\x16\x00\x00\x00\x10\x00\x00\x00\x17\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x26\x00\x00\x00\x23\x00\x00\x00\x17\x00\x00\x00\x17\x00\x00\x00\x34\x00\x00\x00\x44\x00\x00\x00\x17\x00\x00\x00\x17\x00\x00\x00\x60\x00\x00\x00\x65\x00\x00\x00\x17\x00\x00\x00\x17\x00\x00\x00\x62\x00\x00\x00\x5e\x00\x00\x00\x5f\x00\x00\x00\x5e\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x2f\x00\x00\x00\x15\x00\x00\x00\x09\x00\x00\x00\xff\xff\xff\xff\x1f\x00\x00\x00\x2f\x00\x00\x00\x1c\x00\x00\x00\x1b\x00\x00\x00\x2e\x00\x00\x00\x2d\x00\x00\x00\x1e\x00\x00\x00\x26\x00\x00\x00\x28\x00\x00\x00\x09\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x09\x00\x00\x00\x34\x00\x00\x00\x2d\x00\x00\x00\x44\x00\x00\x00\x09\x00\x00\x00\x23\x00\x00\x00\x09\x00\x00\x00\x38\x00\x00\x00\x37\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x00\x00\x36\x00\x00\x00\x4b\x00\x00\x00\x5b\x00\x00\x00\x46\x00\x00\x00\x1e\x00\x00\x00\x47\x00\x00\x00\x59\x00\x00\x00\x57\x00\x00\x00\x52\x00\x00\x00\x53\x00\x00\x00\x4d\x00\x00\x00\x1e\x00\x00\x00\x1e\x00\x00\x00\x51\x00\x00\x00\x41\x00\x00\x00\x09\x00\x00\x00\x63\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x2b\x00\x00\x00\x57\x00\x00\x00\x6b\x00\x00\x00\x65\x00\x00\x00\x09\x00\x00\x00\x6a\x00\x00\x00\x1c\x00\x00\x00\x69\x00\x00\x00\x6e\x00\x00\x00\x21\x00\x00\x00\x68\x00\x00\x00\x67\x00\x00\x00\x2a\x00\x00\x00\x32\x00\x00\x00\x49\x00\x00\x00\x6c\x00\x00\x00\x48\x00\x00\x00\x5b\x00\x00\x00\x59\x00\x00\x00\x55\x00\x00\x00\x5f\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 37) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37)
        ]

happy_n_terms = 38 :: Prelude.Int
happy_n_nonterms = 16 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut6 happy_x_1 of { happy_var_1 -> 
        case happyOut5 happy_x_3 of { happy_var_3 -> 
        happyIn4
                 (foldl ANDQUERY happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut6 happy_x_1 of { happy_var_1 -> 
        case happyOut5 happy_x_3 of { happy_var_3 -> 
        happyIn5
                 (happy_var_1 : happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_0  1# happyReduction_3
happyReduction_3  =  happyIn5
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happyReduce 6# 2# happyReduction_4
happyReduction_4 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        case happyOut19 happy_x_6 of { happy_var_6 -> 
        happyIn6
                 (SELECT happy_var_3 happy_var_6
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happyReduce 10# 2# happyReduction_5
happyReduction_5 (happy_x_10 `HappyStk`
        happy_x_9 `HappyStk`
        happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        case happyOut7 happy_x_7 of { happy_var_7 -> 
        case happyOut19 happy_x_10 of { happy_var_10 -> 
        happyIn6
                 (SELECTWHERE happy_var_3 happy_var_7 happy_var_10
        ) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 4# 2# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut11 happy_x_2 of { happy_var_2 -> 
        case happyOut19 happy_x_4 of { happy_var_4 -> 
        happyIn6
                 (DO happy_var_2 happy_var_4
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 8# 2# happyReduction_7
happyReduction_7 (happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut11 happy_x_2 of { happy_var_2 -> 
        case happyOut7 happy_x_5 of { happy_var_5 -> 
        case happyOut19 happy_x_8 of { happy_var_8 -> 
        happyIn6
                 (DOWHERE happy_var_2 happy_var_5 happy_var_8
        ) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_2  2# happyReduction_8
happyReduction_8 happy_x_2
        happy_x_1
         =  case happyOut19 happy_x_2 of { happy_var_2 -> 
        happyIn6
                 (PRINT happy_var_2
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  3# happyReduction_9
happyReduction_9 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOut17 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (CONDITIONEQSTRING happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_3  3# happyReduction_10
happyReduction_10 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOut17 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (CONDITIONNEQSTRING happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happyReduce 5# 3# happyReduction_11
happyReduction_11 (happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOutTok happy_x_4 of { (TokenInt happy_var_4) -> 
        happyIn7
                 (CONDITIONEQSTRING happy_var_1 (STR (show happy_var_4))
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 5# 3# happyReduction_12
happyReduction_12 (happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOutTok happy_x_4 of { (TokenInt happy_var_4) -> 
        happyIn7
                 (CONDITIONNEQSTRING happy_var_1 (STR (show happy_var_4))
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  3# happyReduction_13
happyReduction_13 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOut18 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (CONDITIONEQCOLUMN happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_3  3# happyReduction_14
happyReduction_14 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        case happyOut18 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (CONDITIONNEQCOLUMN happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_2  3# happyReduction_15
happyReduction_15 happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        happyIn7
                 (CONDITIONISEMPTY happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_3  3# happyReduction_16
happyReduction_16 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_1 of { happy_var_1 -> 
        happyIn7
                 (CONDITIONISNOTEMPTY happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_3  3# happyReduction_17
happyReduction_17 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut7 happy_x_1 of { happy_var_1 -> 
        case happyOut7 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (AND happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_3  3# happyReduction_18
happyReduction_18 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut7 happy_x_1 of { happy_var_1 -> 
        case happyOut7 happy_x_3 of { happy_var_3 -> 
        happyIn7
                 (OR happy_var_1 happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_2  4# happyReduction_19
happyReduction_19 happy_x_2
        happy_x_1
         =  case happyOut10 happy_x_1 of { happy_var_1 -> 
        case happyOut9 happy_x_2 of { happy_var_2 -> 
        happyIn8
                 (foldl ANDSELECT happy_var_1 happy_var_2
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_3  5# happyReduction_20
happyReduction_20 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut10 happy_x_2 of { happy_var_2 -> 
        case happyOut9 happy_x_3 of { happy_var_3 -> 
        happyIn9
                 (happy_var_2 : happy_var_3
        )}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_0  5# happyReduction_21
happyReduction_21  =  happyIn9
                 ([]
        )

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happyReduce 4# 6# happyReduction_22
happyReduction_22 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut18 happy_x_2 of { happy_var_2 -> 
        case happyOut16 happy_x_4 of { happy_var_4 -> 
        happyIn10
                 (GET happy_var_2 happy_var_4
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_2  6# happyReduction_23
happyReduction_23 happy_x_2
        happy_x_1
         =  case happyOut16 happy_x_2 of { happy_var_2 -> 
        happyIn10
                 (GETCSV happy_var_2
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_1  7# happyReduction_24
happyReduction_24 happy_x_1
         =  case happyOut12 happy_x_1 of { happy_var_1 -> 
        happyIn11
                 (happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_1  7# happyReduction_25
happyReduction_25 happy_x_1
         =  case happyOut14 happy_x_1 of { happy_var_1 -> 
        happyIn11
                 (happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_1  7# happyReduction_26
happyReduction_26 happy_x_1
         =  case happyOut15 happy_x_1 of { happy_var_1 -> 
        happyIn11
                 (happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_1  7# happyReduction_27
happyReduction_27 happy_x_1
         =  case happyOut13 happy_x_1 of { happy_var_1 -> 
        happyIn11
                 (happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happyReduce 8# 8# happyReduction_28
happyReduction_28 (happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        case happyOut8 happy_x_7 of { happy_var_7 -> 
        happyIn12
                 (PRODUCT happy_var_3 happy_var_7
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happyReduce 6# 9# happyReduction_29
happyReduction_29 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut18 happy_x_2 of { happy_var_2 -> 
        case happyOut8 happy_x_5 of { happy_var_5 -> 
        happyIn13
                 (DROP happy_var_2 happy_var_5
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happyReduce 6# 10# happyReduction_30
happyReduction_30 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        case happyOut17 happy_x_6 of { happy_var_6 -> 
        happyIn14
                 (COPY happy_var_3 happy_var_6
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happyReduce 8# 10# happyReduction_31
happyReduction_31 (happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        case happyOutTok happy_x_7 of { (TokenInt happy_var_7) -> 
        happyIn14
                 (COPY happy_var_3 (STR (show happy_var_7))
        ) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happyReduce 6# 10# happyReduction_32
happyReduction_32 (happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_3 of { happy_var_3 -> 
        happyIn14
                 (COPYEMPTYCOL happy_var_3
        ) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happyReduce 11# 11# happyReduction_33
happyReduction_33 (happy_x_11 `HappyStk`
        happy_x_10 `HappyStk`
        happy_x_9 `HappyStk`
        happy_x_8 `HappyStk`
        happy_x_7 `HappyStk`
        happy_x_6 `HappyStk`
        happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut8 happy_x_4 of { happy_var_4 -> 
        case happyOut8 happy_x_8 of { happy_var_8 -> 
        case happyOut18 happy_x_11 of { happy_var_11 -> 
        happyIn15
                 (LEFTMERGE happy_var_4 happy_var_8 happy_var_11
        ) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_1  12# happyReduction_34
happyReduction_34 happy_x_1
         =  case happyOut19 happy_x_1 of { happy_var_1 -> 
        happyIn16
                 (happy_var_1
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_3  13# happyReduction_35
happyReduction_35 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenVar happy_var_2) -> 
        happyIn17
                 (STR happy_var_2
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_2  14# happyReduction_36
happyReduction_36 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (TokenInt happy_var_2) -> 
        happyIn18
                 (COLUMN happy_var_2
        )}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_1  15# happyReduction_37
happyReduction_37 happy_x_1
         =  case happyOutTok happy_x_1 of { (TokenFileName happy_var_1) -> 
        happyIn19
                 (FILENAME happy_var_1
        )}

happyNewToken action sts stk [] =
        happyDoAction 37# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        TokenSelect -> cont 1#;
        TokenWhere -> cont 2#;
        TokenDo -> cont 3#;
        TokenComma -> cont 4#;
        TokenGet -> cont 5#;
        TokenColumn -> cont 6#;
        TokenOf -> cont 7#;
        TokenNeq -> cont 8#;
        TokenIs -> cont 9#;
        TokenNot -> cont 10#;
        TokenEmptyCol -> cont 11#;
        TokenEmpty -> cont 12#;
        TokenRead -> cont 13#;
        TokenAnd -> cont 14#;
        TokenOr -> cont 15#;
        TokenProduct -> cont 16#;
        TokenTimes -> cont 17#;
        TokenDrop -> cont 18#;
        TokenIn -> cont 19#;
        TokenPermute -> cont 20#;
        TokenCopy -> cont 21#;
        TokenWith -> cont 22#;
        TokenLeft -> cont 23#;
        TokenMerge -> cont 24#;
        TokenTo -> cont 25#;
        TokenOn -> cont 26#;
        TokenFileName happy_dollar_dollar -> cont 27#;
        TokenPrint -> cont 28#;
        TokenInt happy_dollar_dollar -> cont 29#;
        TokenVar happy_dollar_dollar -> cont 30#;
        TokenEq -> cont 31#;
        TokenEqString -> cont 32#;
        TokenNEqString -> cont 33#;
        TokenLParen -> cont 34#;
        TokenRParen -> cont 35#;
        TokenSemiColon -> cont 36#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 37# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut4 x} in x'))

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens = error $ "Parse error. Remaining tokens: " ++ show tokens

data Exp = Let String Exp Exp 
         | Plus Exp Exp 
         | Minus Exp Exp 
         | Times Exp Exp 
         | Div Exp Exp 
         | Negate Exp
         | Int Int 
         | Var String 
         | Expon Exp Exp
         deriving Show 

data Exp2 = SELECT Exp2 Exp2
          | SELECTWHERE Exp2 Exp2 Exp2
          | CONDITIONEQSTRING Exp2 Exp2
          | CONDITIONNEQSTRING Exp2 Exp2
          | CONDITIONNEQITEM Exp2 Exp2
          | CONDITIONEQITEM Exp2 Exp2
          | CONDITIONISEMPTY Exp2 
          | CONDITIONISNOTEMPTY Exp2
          | ANDSELECT Exp2 Exp2
          | AND Exp2 Exp2
          | OR Exp2 Exp2
          | DO Exp2 Exp2
          | GET Exp2 Exp2
          | DOWHERE Exp2 Exp2 Exp2
          | PRODUCT Exp2 Exp2 
          | DROP Exp2 Exp2
          | PERMUTE Exp2 
          | COPY Exp2 Exp2
          | COPYEMPTYCOL Exp2 
          | LEFTMERGE Exp2 Exp2 Exp2
          | FILENAME String 
          | RESULTOF Exp2 
          | CONDITIONEQCOLUMN Exp2 Exp2
          | CONDITIONNEQCOLUMN Exp2 Exp2
          | COLUMN Int 
          | STR String 
          | PRINT Exp2
          | GETCSV Exp2
          | ANDQUERY Exp2 Exp2
          deriving Show
#define HAPPY_COERCE 1
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
