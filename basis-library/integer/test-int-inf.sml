structure I = Primitive.IntInfAlt

val toString = IntInf.fmt StringCvt.HEX

fun make opstr defop testop (i, j) = 
   let
      val res = defop (i, j)
      val x = I.castFromIntInf i
      val y = I.castFromIntInf j
      val calculated = I.castToIntInf (testop (x, y))
   in
      if (res = calculated)
         then ()
      else (print ("Error: "^toString i^" "^opstr^" "^toString j^" is "^toString res^" but library reported "^toString calculated^"\n"))
   end

val add = make "+" IntInf.+ I.+
val sub = make "-" IntInf.- I.-

fun loop n =
   if IntInf.> (n, 1000000)
      then print "All tests succeeded!\n"
   else
     ( add (n, 100)
     ; sub (n, 100)
     ; add (n, 1000)
     ; sub (n, 1000)
     ; add (n, 100000)
     ; sub (n, 100000)
     ; add (n, 1000000)
     ; sub (n, 1000000)
     ; add (100, n)
     ; sub (100, n)
     ; add (1000, n)
     ; sub (1000, n)
     ; add (100000, n)
     ; sub (100000, n)
     ; add (1000000, n)
     ; sub (1000000, n)
     ; add (n, n)
     ; sub (n, n)
     ; add (n, 0)
     ; sub (n, 0)
     ; loop (IntInf.+ (n, 100)))

val () = loop ~1000000
      
