(* Copyright (C) 2014 Rob Simmons
 * Copyright (C) 2013 Matthew Fluet
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive = struct

open Primitive

structure IntInfAlt =
   struct
      structure A = Array
      structure V = Vector
      structure S = SeqIndex
      structure W = struct
                       open Word8
                       val toBig = Word8.zextToWord16
                       val toSmall = Word16.zchckToWord8
                    end
      structure B = Word8
      structure I = Int9

      datatype sign = NEG | POS
      datatype bigInt =
         Big of bool * V.vector 
       | Small of I.int

      fun small (s, x: W.word) = 
         case s of 
            POS => 
          | NEG => 

      (* Requires: 0 <= i <= \length(vec) *)
      (* Requires: vec[i,\length(vec)) = 0wx0 *)
      fun finiResLoop (s: sign, vec: W.word V.vector, i: I.t) =
         if 0 = i then Small (
         if 0wx0 = V.subUnsafe (vec, i)
      
   end

(* structure IntInfAlt : PRIM_INT_INF = IntInfAlt *)

end
