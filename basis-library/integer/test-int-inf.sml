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
val mul = make "*" IntInf.* I.*

fun loop n =
   if IntInf.> (n, 1000000)
      then print "All tests succeeded!\n"
   else
     ( mul (n, 1)
     ; mul (n, ~1)
     ; mul (n, 0)
     ; mul (n, 2)
     ; mul (n, ~2)
     ; mul (n, 3)
     ; mul (n, 16)
     ; mul (n, 100)
     ; mul (n, 255)
     ; mul (n, 256)  
     ; mul (n, 10000)
     ; add (n, 100)
     ; sub (n, 100)
     ; add (n, 1000)
     ; sub (n, 1000)
     ; add (n, 100000)
     ; sub (n, 100000)
     ; add (n, 1000000)
     ; sub (n, 1000000)
     ; mul (1, n)
     ; mul (~1, n)
     ; mul (0, n)
     ; mul (2, n)
     ; mul (~2, n)
     ; mul (3, n)
     ; mul (16, n)
     ; mul (100, n)
     ; mul (255, n)
     ; mul (256, n)  
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
     ; mul (n, n)
     ; add (n, 0)
     ; sub (n, 0)
     ; mul (n, 0)
     ; loop (IntInf.+ (n, 100)))

val () = loop ~1000000
      
