(setf print-f (import "libc.so.6" "printf"))
(setf lib "libtestlib.so")

(import lib "staticTest")
(import lib "dynamicTest")

(staticTest)
(setf r (dynamicTest (int 20)))

(printf "newLISP int(%d) received from C/C++ library (%s)\n" r lib)

(exit)