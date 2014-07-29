minTime="30.0"

bench="barnes-hut boyer checksum count-graphs DLXSimulator even-odd fft fib flat-array hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc model-elimination mpuz nucleic output1 peek psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern"

cd tests
for prog in $bench; do
    case "$prog" in
        "fxp")
            continue ;;
        "model-elimination")
            echo "(\"model-elimination\", 0):: (* ??? sec *)"
            continue ;;
    esac

    ( cat $prog.sml ; echo "val _ = Main.doit (valOf (Int.fromString (hd (CommandLine.arguments ()))))" ) > $prog.main.sml
    mlton -output $prog $prog.main.sml 1>/dev/null 2>/dev/null

    n=1;
    m=1;
    k=1;
    t=$(/usr/bin/time -o $prog.time --format "%U + %S" ./$prog $n 1>/dev/null 2>/dev/null; cat $prog.time | grep -v "Command exited" | bc)
    while [ "$(echo "$t < $minTime" | bc)" = "1" ]; do
        if [ $n -lt $m ]; then
            n=$(($n+$k))
        else
            m=$((2*$m))
            if [ $m -lt 8 ]; then
                n=$m
            else
                k=$(( ($m-$n) / 2 ))
                n=$(($n+$k))
            fi
        fi
        t=$(/usr/bin/time -o $prog.time --format "%U + %S" ./$prog $n 1>/dev/null 2>/dev/null; cat $prog.time | grep -v "Command exited" | bc)
    done

    rm $prog $prog.main.sml $prog.time

    echo "(\"$prog\", $n):: (* $t sec *)"
done
