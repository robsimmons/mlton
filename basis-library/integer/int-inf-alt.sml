structure Primitive = struct

open Primitive

structure IntInfAlt = 
   struct
      structure SmallInt = Int8
      structure SmallWord = Word8 
      structure BigInt = Int16
      structure BigWord = Word16

      val smallPrecision: Primitive.Word32.word = 0wx8
      val base: BigWord.word = BigWord.<<? (0wx1, smallPrecision)
      val smallWordToBigInt = SmallWord.zextdToInt16 
      val smallWordToBigWord = SmallWord.zextdToWord16 
      val bigWordToSmallWordTrunc = SmallWord.zextdFromWord16
      val bigWordToSmallWordCheck = SmallWord.zchckFromWord16
      val bigIntToBigWord = BigWord.zextdFromInt16
      val smallWordToMPLimb = C_MPLimb.zchckFromWord8
      val smallWordFromMPLimb = C_MPLimb.zchckToWord8

      open SeqIndex
      structure V = Vector

      datatype int = 
         Zero
       | Neg of SmallWord.word vector
       | Pos of SmallWord.word vector
      type t = int

      (* We don't use all the bits in a Int *) 
      val minSmall = BigInt.~ (smallWordToBigInt SmallWord.maxWord')
      val maxSmall = smallWordToBigInt SmallWord.maxWord'

      fun is_int_vector v = 
         0 < V.length v
         andalso 0wx0 <> V.sub (v, SeqIndex.- (V.length v, 1))

      fun is_int i = 
         case i of 
            Zero => true
          | Neg v => is_int_vector v
          | Pos v => is_int_vector v

      fun smallVec i = 
         let
            val w = bigIntToBigWord i
            val hi = bigWordToSmallWordCheck (BigWord.>>? (w, smallPrecision))
            val lo = bigWordToSmallWordTrunc w
         in
            Vector.tabulate (2, fn 0 => lo | _ => hi)
         end

      fun compareVec (v1, v2) = 
         case compare (V.length v1, V.length v2) of
            EQUAL => 
            let 
               fun loop i = 
                  case SmallWord.compare (V.sub (v1, i), V.sub (v2, i)) of
                     EQUAL => (if i = 0 then EQUAL else loop (i - 1))
                   | cmp => cmp 
            in 
               loop (V.length v1 - 1)
            end
          | cmp => cmp 

      fun bigAbs i = 
         case i of 
            Zero => Zero
          | Neg v => Pos v
          | Pos v => Pos v

      fun bigNeg i = 
         case i of 
            Zero => Zero
          | Neg v => Pos v
          | Pos v => Neg v 

      local
         fun bigLT (i, j) = 
            case (i, j) of 
               (Zero, Zero) => true
             | (Zero, Neg _) => false
             | (Zero, Pos _) => true
             | (Neg i, Neg j) => GREATER = compareVec (i, j)
             | (Neg _, _) => true
             | (Pos i, Pos j) => LESS = compareVec (i, j)
             | (Pos _, _) => false
         structure S = IntegralComparisons(type t = int
                                           val op < = bigLT)
      in
         val bigCompare = S.compare
         val bigLT = S.<
         val bigLTE = S.<=
         val bigGT = S.>
         val bigGTE = S.>=
         val bigMin = S.min
         val bigMax = S.max
      end

      local
         fun bigLTU (lhs, rhs) =
            case (bigCompare (lhs, Zero), bigCompare (rhs, Zero)) of
               (LESS, LESS) => bigLT (lhs, rhs)
             | (LESS, GREATER) => false
             | (_, EQUAL) => false
             | (EQUAL, _) => true
             | (GREATER, LESS) => true
             | (GREATER, GREATER) => bigLT (lhs, rhs)
         structure S = IntegralComparisons(type t = int
                                           val op < = bigLTU)
      in
         val bigLTU = S.<
         val bigLEU = S.<=
         val bigGTU = S.>
         val bigGEU = S.>=
      end

      (* Natural number arithmetic functions *)

      fun bigWordSub (vec, i) = smallWordToBigWord (V.sub (vec, i))

      fun drop0s (v: SmallWord.t vector): SmallWord.t vector = 
         V.Slice.sequence 
            (V.Slice.dropr (fn 0wx0 => true | _ => false) (V.Slice.full v))

      (* Requires: 0 <= carry <= maxSmall *) 
      fun add_perform_carry (src, len) (i, carry) =  
         if i = len 
            then (bigWordToSmallWordCheck carry, 0wx0)
         else let
                 val result = BigWord.+ (src i, carry)
                 val carry = BigWord.>>? (result, smallPrecision)
              in
                 (bigWordToSmallWordTrunc result, carry)
              end

      fun addVecs (v1, v2) = 
         let
            val len = max (V.length v1, V.length v2)
            fun mkSub vec = 
               if V.length vec = len 
                  then (fn i => bigWordSub (vec, i))
               else (fn i => if i >= (V.length vec) 
                                then 0wx0
                             else bigWordSub (vec, i))
            val (sub1, sub2) = (mkSub v1, mkSub v2)
            fun sub i = BigWord.+ (sub1 i, sub2 i)
         in
            #1 (V.unfoldi (len + 1, 0wx0, add_perform_carry (sub, len)))
         end
 
      (* Requires: 0 <= carry <= maxSmall *) 
      fun sub_perform_borrow (src, len) (i, carry) = 
         let
            val result = BigWord.- (src i, carry)
            val (result, carry) =
               if BigWord.< (result, base)
                  then (result, 0wx0) (* No overflow *)
               else (BigWord.+ (result, base), 0wx1) (* Overflow *)
         in
            (bigWordToSmallWordCheck result, carry)
         end

      (* Requires: GREATER = compareVecs (v1, v2) *)
      fun subVecs (v1, v2) = 
         let
            val len = V.length v1
            fun sub1 i = bigWordSub (v1, i)
            fun sub2 i = if i < V.length v2 then bigWordSub (v2, i) else 0wx0
            fun sub i = BigWord.- (sub1 i, sub2 i)
         in
            #1 (V.unfoldi (len, 0wx0, sub_perform_borrow (sub, len)))
         end

      fun bigAdd (i, j) = 
         case (i, j) of
            (Zero, j) => j
          | (i, Zero) => i
          | (Neg v1, Neg v2) => Neg (drop0s (addVecs (v1, v2)))
          | (Pos v1, Pos v2) => Pos (drop0s (addVecs (v1, v2)))
          | (Neg v1, Pos v2) => 
            (case compareVec (v1, v2) of
                EQUAL => Zero
              | LESS => Pos (drop0s (subVecs (v2, v1)))
              | GREATER => Neg (drop0s (subVecs (v1, v2))))
          | (Pos v1, Neg v2) =>
            (case compareVec (v1, v2) of
                EQUAL => Zero
              | LESS => Neg (drop0s (subVecs (v2, v1)))
              | GREATER => Pos (drop0s (subVecs (v1, v2)))) 

      fun bigSub (i, j) = bigAdd (i, bigNeg j)

      (* These should all actually be the identity! *)
      local 
         val base = BigWord.castToIntInf base
         fun vecToIntInf v = 
            V.foldr (fn (digit, accum) =>
                        IntInf.+ (SmallWord.castToIntInf digit, 
                                  IntInf.* (accum, base))) 
                    0 v
         fun toIntInf i = 
            case i of 
               Zero => IntInf.zero
             | Pos v => vecToIntInf v
             | Neg v => IntInf.~ (vecToIntInf v)
         exception Whoops
         fun vecFromIntInf i =
            let
               fun calc 0 = (0, [])
                 | calc i = 
                   let
                      val (thediv, themod) = IntInf.divMod (i, base)
                      val (n, list) = calc thediv
                   in
                      (n + 1, SmallWord.castFromIntInf themod :: list)
                   end
               val (n, list) = calc i
            in
               #1 (V.unfold (n, list, fn [] => raise Whoops
                                       | x :: xs => (x, xs)))
            end

         fun fromIntInf i = 
            case IntInf.compare (i, 0) of
               EQUAL => Zero
             | LESS => Neg (vecFromIntInf (IntInf.~ i))
             | GREATER => Pos (vecFromIntInf i)  
      in
         val castFromIntInf = fromIntInf
         val castToIntInf = toIntInf
         val zextdFromIntInf = fromIntInf
         val zextdToIntInf = toIntInf
         val sextdFromIntInf = fromIntInf
         val sextdToIntInf = toIntInf
         val zchckFromIntInf = fromIntInf
         val zchckToIntInf = toIntInf
         val schckFromIntInf = fromIntInf
         val schckToIntInf = toIntInf
      end

      fun unimp x = let exception Unimplemented in raise Unimplemented end

      val precision = NONE
      val maxInt = NONE
      val minInt = NONE
      val zero = Zero
      val one = Pos (V.new (1, 0wx1))
      val negOne = Neg (V.new (1, 0wx1))
      datatype rep = 
         Big of C_MPLimb.t vector (* Assumes MPLimb.t can hold a SmallWord.t *)
       | Small of ObjptrInt.int
      fun rep i = 
         let 
            fun sub (v, sign) i =  
               if i = 0 
                  then sign
               else smallWordToMPLimb (V.sub (v, i - 1))
         in 
            case i of 
               Zero => Small 0
             | Neg v => Big (V.tabulate (V.length v + 1, sub (v, 0wx1)))
             | Pos v => Big (V.tabulate (V.length v + 1, sub (v, 0wx0)))
         end
      fun fromRep i = 
         let
            fun sub v i = smallWordFromMPLimb (V.sub (v, i + 1))
            fun chk i = if is_int i then SOME i else NONE
         in 
            case i of 
               Big v =>
                  (case Vector.sub (v, 0) of
                      0wx1 => chk (Neg (V.tabulate (V.length v - 1, sub v)))
                    | 0wx0 => chk (Pos (V.tabulate (V.length v - 1, sub v)))
                    | _ => NONE) 
             | Small 0 => SOME Zero
             | _ => NONE 
         end handle Subscript => NONE
                  | Overflow => NONE 
      fun isSmall i = case i of Zero => true | _ => false 
      fun areSmall (i, j) = case (i, j) of (Zero, Zero) => true | _ => false 
      val abs = bigAbs
      val op +! = bigAdd
      val op +? = bigAdd
      val op + = bigAdd 
      val divMod = unimp
      val op div = unimp
      val gcd = unimp
      val op mod = unimp
      val op *! = unimp
      val op *? = unimp
      val op * = unimp 
      val op ~! = bigNeg
      val op ~? = bigNeg
      val op ~ = bigNeg
      val quotRem = unimp
      val quot = unimp
      val rem = unimp
      val op -! = bigSub
      val op -? = bigSub
      val op - = bigSub
      val op < = bigLT
      val op <= = bigLTE
      val op > = bigGT
      val op >= = bigGTE
      val compare = bigCompare
      val min = bigMin
      val max = bigMax
      val ltu = bigLTU
      val leu = bigLEU
      val gtu = bigGTU
      val geu = bigGEU
      val andb = unimp
      val op <<? = unimp
      val op << = unimp
      val notb = unimp
      val orb = unimp
      val op ~>>? = unimp
      val op ~>> = unimp
      val xorb = unimp
      val mkCvt = unimp
      val mkLog2 = unimp

      val castFromInt8 = unimp
      val castFromWord8 = unimp
      val castToInt8 = unimp
      val castToWord8 = unimp
      val zextdFromInt8 = unimp
      val zextdFromWord8 = unimp
      val zextdToInt8 = unimp
      val zextdToWord8 = unimp
      val sextdFromInt8 = unimp
      val sextdFromWord8 = unimp
      val sextdToInt8 = unimp
      val sextdToWord8 = unimp
      val zchckFromInt8 = unimp
      val zchckFromWord8 = unimp
      val zchckToInt8 = unimp
      val zchckToWord8 = unimp
      val schckFromInt8 = unimp
      val schckFromWord8 = unimp
      val schckToInt8 = unimp
      val schckToWord8 = unimp 

      val castFromInt16 = unimp
      val castFromWord16 = unimp
      val castToInt16 = unimp
      val castToWord16 = unimp
      val zextdFromInt16 = unimp
      val zextdFromWord16 = unimp
      val zextdToInt16 = unimp
      val zextdToWord16 = unimp
      val sextdFromInt16 = unimp
      val sextdFromWord16 = unimp
      val sextdToInt16 = unimp
      val sextdToWord16 = unimp
      val zchckFromInt16 = unimp
      val zchckFromWord16 = unimp
      val zchckToInt16 = unimp
      val zchckToWord16 = unimp
      val schckFromInt16 = unimp
      val schckFromWord16 = unimp
      val schckToInt16 = unimp
      val schckToWord16 = unimp

      val castFromInt32 = unimp
      val castFromWord32 = unimp
      val castToInt32 = unimp
      val castToWord32 = unimp
      val zextdFromInt32 = unimp
      val zextdFromWord32 = unimp
      val zextdToInt32 = unimp
      val zextdToWord32 = unimp
      val sextdFromInt32 = unimp
      val sextdFromWord32 = unimp
      val sextdToInt32 = unimp
      val sextdToWord32 = unimp
      val zchckFromInt32 = unimp
      val zchckFromWord32 = unimp
      val zchckToInt32 = unimp
      val zchckToWord32 = unimp
      val schckFromInt32 = unimp
      val schckFromWord32 = unimp
      val schckToInt32 = unimp
      val schckToWord32 = unimp

      val castFromInt64 = unimp
      val castFromWord64 = unimp
      val castToInt64 = unimp
      val castToWord64 = unimp
      val zextdFromInt64 = unimp
      val zextdFromWord64 = unimp
      val zextdToInt64 = unimp
      val zextdToWord64 = unimp
      val sextdFromInt64 = unimp
      val sextdFromWord64 = unimp
      val sextdToInt64 = unimp
      val sextdToWord64 = unimp
      val zchckFromInt64 = unimp
      val zchckFromWord64 = unimp
      val zchckToInt64 = unimp
      val zchckToWord64 = unimp
      val schckFromInt64 = unimp
      val schckFromWord64 = unimp
      val schckToInt64 = unimp
      val schckToWord64 = unimp
  end

structure IntInfAlt: PRIM_INT_INF = IntInfAlt

end 
