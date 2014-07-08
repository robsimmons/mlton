(* Copyright (C) 2013 Matthew Fluet
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_INT_INF =
   sig
      eqtype int
      type t = int

      val precision: Primitive.Int32.int option

      val maxInt: int option
      val minInt: int option

      val zero: int
      val one: int
      val negOne: int

      datatype rep =
         Big of C_MPLimb.word vector
       | Small of ObjptrInt.int
      val rep: int -> rep
      val fromRep: rep -> int option

      val isSmall: int -> bool
      val areSmall: int * int -> bool

      val abs: int -> int
      val +! : int * int -> int
      val +? : int * int -> int
      val + : int * int -> int
      val divMod: int * int -> int * int
      val div: int * int -> int
      val gcd: int * int -> int
      val mod: int * int -> int
      val *! : int * int -> int
      val *? : int * int -> int
      val * : int * int -> int
      val ~! : int -> int
      val ~? : int -> int
      val ~ : int -> int
      val quotRem: int * int -> int * int
      val quot: int * int -> int
      val rem: int * int -> int
      val -! : int * int -> int
      val -? : int * int -> int
      val - : int * int -> int

      val < : int * int -> bool
      val <= : int * int -> bool
      val > : int * int -> bool
      val >= : int * int -> bool
      val compare: int * int -> Primitive.Order.order
      val min: int * int -> int
      val max: int * int -> int
      val ltu: int * int -> bool
      val leu: int * int -> bool
      val gtu: int * int -> bool
      val geu: int * int -> bool

      val andb: int * int -> int
      val <<? : int * Primitive.Word32.word -> int
      val << : int * Primitive.Word32.word -> int
      val notb: int -> int
      val orb: int * int -> int
      val ~>>? : int * Primitive.Word32.word -> int
      val ~>> : int * Primitive.Word32.word -> int
      val xorb: int * int -> int

      val mkCvt: ({base: Primitive.Int32.int,
                   smallCvt: ObjptrInt.int -> Primitive.String8.string} 
                  -> int -> Primitive.String8.string)
      val mkLog2: ({fromSmall: {smallLog2: Primitive.Int32.int} -> 'a,
                    fromLarge: {mostSigLimbLog2: Primitive.Int32.int,
                                numLimbsMinusOne: SeqIndex.int} -> 'a}
                   -> int -> 'a)

      val zextdFromInt8: Primitive.Int8.int -> int
      val zextdFromInt16: Primitive.Int16.int -> int
      val zextdFromInt32: Primitive.Int32.int -> int
      val zextdFromInt64: Primitive.Int64.int -> int
      val zextdFromIntInf: Primitive.IntInf.int -> int
      val zextdFromWord8: Primitive.Word8.word -> int
      val zextdFromWord16: Primitive.Word16.word -> int
      val zextdFromWord32: Primitive.Word32.word -> int
      val zextdFromWord64: Primitive.Word64.word -> int
      val zextdToInt8: int -> Primitive.Int8.int
      val zextdToInt16: int -> Primitive.Int16.int
      val zextdToInt32: int -> Primitive.Int32.int
      val zextdToInt64: int -> Primitive.Int64.int
      val zextdToIntInf: int -> Primitive.IntInf.int
      val zextdToWord8: int -> Primitive.Word8.word
      val zextdToWord16: int -> Primitive.Word16.word
      val zextdToWord32: int -> Primitive.Word32.word
      val zextdToWord64: int -> Primitive.Word64.word

      val sextdFromInt8: Primitive.Int8.int -> int
      val sextdFromInt16: Primitive.Int16.int -> int
      val sextdFromInt32: Primitive.Int32.int -> int
      val sextdFromInt64: Primitive.Int64.int -> int
      val sextdFromIntInf: Primitive.IntInf.int -> int
      val sextdFromWord8: Primitive.Word8.word -> int
      val sextdFromWord16: Primitive.Word16.word -> int
      val sextdFromWord32: Primitive.Word32.word -> int
      val sextdFromWord64: Primitive.Word64.word -> int
      val sextdToInt8: int -> Primitive.Int8.int
      val sextdToInt16: int -> Primitive.Int16.int
      val sextdToInt32: int -> Primitive.Int32.int
      val sextdToInt64: int -> Primitive.Int64.int
      val sextdToIntInf: int -> Primitive.IntInf.int
      val sextdToWord8: int -> Primitive.Word8.word
      val sextdToWord16: int -> Primitive.Word16.word
      val sextdToWord32: int -> Primitive.Word32.word
      val sextdToWord64: int -> Primitive.Word64.word

      val castFromInt8: Primitive.Int8.int -> int
      val castFromInt16: Primitive.Int16.int -> int
      val castFromInt32: Primitive.Int32.int -> int
      val castFromInt64: Primitive.Int64.int -> int
      val castFromIntInf: Primitive.IntInf.int -> int
      val castFromWord8: Primitive.Word8.word -> int
      val castFromWord16: Primitive.Word16.word -> int
      val castFromWord32: Primitive.Word32.word -> int
      val castFromWord64: Primitive.Word64.word -> int
      val castToInt8: int -> Primitive.Int8.int
      val castToInt16: int -> Primitive.Int16.int
      val castToInt32: int -> Primitive.Int32.int
      val castToInt64: int -> Primitive.Int64.int
      val castToIntInf: int -> Primitive.IntInf.int
      val castToWord8: int -> Primitive.Word8.word
      val castToWord16: int -> Primitive.Word16.word
      val castToWord32: int -> Primitive.Word32.word
      val castToWord64: int -> Primitive.Word64.word

      val zchckFromInt8: Primitive.Int8.int -> int
      val zchckFromInt16: Primitive.Int16.int -> int
      val zchckFromInt32: Primitive.Int32.int -> int
      val zchckFromInt64: Primitive.Int64.int -> int
      val zchckFromIntInf: Primitive.IntInf.int -> int
      val zchckFromWord8: Primitive.Word8.word -> int
      val zchckFromWord16: Primitive.Word16.word -> int
      val zchckFromWord32: Primitive.Word32.word -> int
      val zchckFromWord64: Primitive.Word64.word -> int
      val zchckToInt8: int -> Primitive.Int8.int
      val zchckToInt16: int -> Primitive.Int16.int
      val zchckToInt32: int -> Primitive.Int32.int
      val zchckToInt64: int -> Primitive.Int64.int
      val zchckToIntInf: int -> Primitive.IntInf.int
      val zchckToWord8: int -> Primitive.Word8.word
      val zchckToWord16: int -> Primitive.Word16.word
      val zchckToWord32: int -> Primitive.Word32.word
      val zchckToWord64: int -> Primitive.Word64.word

      val schckFromInt8: Primitive.Int8.int -> int
      val schckFromInt16: Primitive.Int16.int -> int
      val schckFromInt32: Primitive.Int32.int -> int
      val schckFromInt64: Primitive.Int64.int -> int
      val schckFromIntInf: Primitive.IntInf.int -> int
      val schckFromWord8: Primitive.Word8.word -> int
      val schckFromWord16: Primitive.Word16.word -> int
      val schckFromWord32: Primitive.Word32.word -> int
      val schckFromWord64: Primitive.Word64.word -> int
      val schckToInt8: int -> Primitive.Int8.int
      val schckToInt16: int -> Primitive.Int16.int
      val schckToInt32: int -> Primitive.Int32.int
      val schckToInt64: int -> Primitive.Int64.int
      val schckToIntInf: int -> Primitive.IntInf.int
      val schckToWord8: int -> Primitive.Word8.word
      val schckToWord16: int -> Primitive.Word16.word
      val schckToWord32: int -> Primitive.Word32.word
      val schckToWord64: int -> Primitive.Word64.word
   end

signature PRIM_INTWORD_CONV =
   sig
      include PRIM_INTWORD_CONV

      val idFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int

      val zextdFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val zextdFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val zextdFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val zextdFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val zextdFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val zextdFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val zextdFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val zextdFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val zextdFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val zextdFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val zextdFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val zextdFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val zextdFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val zextdFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val zextdFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val zextdFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val zextdFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val sextdFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val sextdFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val sextdFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val sextdFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val sextdFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val sextdFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val sextdFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val sextdFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val sextdFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val sextdFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val sextdFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val sextdFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val sextdFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val sextdFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val sextdFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val sextdFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val sextdFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val castFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val castFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val castFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val castFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val castFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val castFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val castFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val castFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val castFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val castFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val castFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val castFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val castFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val castFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val castFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val castFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val castFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val zchckFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val zchckFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val zchckFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val zchckFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val zchckFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val zchckFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val zchckFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val zchckFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val zchckFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val zchckFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val zchckFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val zchckFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val zchckFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val zchckFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val zchckFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val zchckFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val zchckFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word


      val schckFromInt8ToIntInf: Primitive.Int8.int -> Primitive.IntInf.int
      val schckFromInt16ToIntInf: Primitive.Int16.int -> Primitive.IntInf.int
      val schckFromInt32ToIntInf: Primitive.Int32.int -> Primitive.IntInf.int
      val schckFromInt64ToIntInf: Primitive.Int64.int -> Primitive.IntInf.int
      val schckFromWord8ToIntInf: Primitive.Word8.word -> Primitive.IntInf.int
      val schckFromWord16ToIntInf: Primitive.Word16.word -> Primitive.IntInf.int
      val schckFromWord32ToIntInf: Primitive.Word32.word -> Primitive.IntInf.int
      val schckFromWord64ToIntInf: Primitive.Word64.word -> Primitive.IntInf.int

      val schckFromIntInfToInt8: Primitive.IntInf.int -> Primitive.Int8.int
      val schckFromIntInfToInt16: Primitive.IntInf.int -> Primitive.Int16.int
      val schckFromIntInfToInt32: Primitive.IntInf.int -> Primitive.Int32.int
      val schckFromIntInfToInt64: Primitive.IntInf.int -> Primitive.Int64.int
      val schckFromIntInfToIntInf: Primitive.IntInf.int -> Primitive.IntInf.int
      val schckFromIntInfToWord8: Primitive.IntInf.int -> Primitive.Word8.word
      val schckFromIntInfToWord16: Primitive.IntInf.int -> Primitive.Word16.word
      val schckFromIntInfToWord32: Primitive.IntInf.int -> Primitive.Word32.word
      val schckFromIntInfToWord64: Primitive.IntInf.int -> Primitive.Word64.word
   end
signature PRIM_INTEGER =
   sig
      include PRIM_INTEGER

      val zextdFromIntInf: Primitive.IntInf.int -> int
      val zextdToIntInf: int -> Primitive.IntInf.int

      val sextdFromIntInf: Primitive.IntInf.int -> int
      val sextdToIntInf: int -> Primitive.IntInf.int

      val castFromIntInf: Primitive.IntInf.int -> int
      val castToIntInf: int -> Primitive.IntInf.int

      val zchckFromIntInf: Primitive.IntInf.int -> int
      val zchckToIntInf: int -> Primitive.IntInf.int

      val schckFromIntInf: Primitive.IntInf.int -> int
      val schckToIntInf: int -> Primitive.IntInf.int
   end
signature PRIM_WORD =
   sig
      include PRIM_WORD

      val zextdFromIntInf: Primitive.IntInf.int -> word
      val zextdToIntInf: word -> Primitive.IntInf.int

      val sextdFromIntInf: Primitive.IntInf.int -> word
      val sextdToIntInf: word -> Primitive.IntInf.int

      val castFromIntInf: Primitive.IntInf.int -> word
      val castToIntInf: word -> Primitive.IntInf.int

      val zchckFromIntInf: Primitive.IntInf.int -> word
      val zchckToIntInf: word -> Primitive.IntInf.int

      val schckFromIntInf: Primitive.IntInf.int -> word
      val schckToIntInf: word -> Primitive.IntInf.int
   end
