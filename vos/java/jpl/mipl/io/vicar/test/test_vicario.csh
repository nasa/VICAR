#!/bin/csh
set echo

# Run this with no arguments to test the Java VICAR I/O subsystem.  Capture
# the output and compare with the test log.  The Thread test at the end prints
# things in a completely random order, but as long as there are no errors
# reported, it's okay.  There should be no other diffs.  Also, "difpic"
# should always report 0 differences, and there should be no Java exceptions
# or error prints.
#
# Tested:  writing in Java, reading and comparing via VICAR.  Writing in
# VICAR, reading and comparing in Java.  (thus they cross-check each other).
# Heavily threaded reads.  Labels are tested to a limited extent.  "File"
# interface (single physical files with potentially multiple bands).  Both
# line-oriented and tile-oriented I/O.  All data types except complex.
# All organizations (BSQ, BIL, BIP).  Several different pixel stride settings.
#
# Not tested:  Binary labels.  Extensive label testing.  "Image" interface
# (virtual image made up of multiple physical files, e.g. 3 separate files
# for 3 color bands).  Complex data type (not fully implemented).  Codec
# (higher-level) interfaces.
#
# As always, the best test is a regression test on users of the package.
#
# rgd 10/10/00

# Byte, bsq

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bsq line 2 3 5 12 21 1
$R2LIB/label -list a
$R2LIB/label -list a -dump
$R2LIB/gen b 100 100 3 -byte -bsq linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bsq line 2 3 5 12 21 3

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bsq tile 2 3 5 12 21 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bsq tile 2 3 5 12 21 1

# Byte, bil

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bil line 2 3 5 12 21 2
$R2LIB/gen b 100 100 3 -byte -bil linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bil line 2 3 5 12 21 3

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bil tile 2 3 5 12 21 4
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bil tile 2 3 5 12 21 1

# Byte, bip

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bip line 2 3 5 12 21 1
$R2LIB/gen b 100 100 3 -byte -bip linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bip line 2 3 5 12 21 1

java jpl.mipl.io.vicar.test.TestGen a 100 100 3 byte bip tile 2 3 5 12 21 4
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 100 3 byte bip tile 2 3 5 12 21 3

######

# half, bsq

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bsq line 2 3 5 37 11 1
$R2LIB/gen b 234 432 1 -half -bsq linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bsq line 2 3 5 37 11 2

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bsq tile 2 3 5 37 11 3
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bsq tile 2 3 5 37 11 1

# half, bil

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bil line 2 3 5 37 11 4
$R2LIB/gen b 234 432 1 -half -bil linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bil line 2 3 5 37 11 1

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bil tile 2 3 5 37 11 1
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bil tile 2 3 5 37 11 3

# half, bip

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bip line 2 3 5 37 11 2
$R2LIB/gen b 234 432 1 -half -bip linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bip line 2 3 5 37 11 1

java jpl.mipl.io.vicar.test.TestGen a 234 432 1 half bip tile 2 3 5 37 11 1
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 234 432 1 half bip tile 2 3 5 37 11 5

######

# full, bsq

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bsq line 2 3 5 100 10 4
$R2LIB/gen b 1500 50 2 -full -bsq linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bsq line 2 3 5 100 10 1

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bsq tile 2 3 5 100 10 1
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bsq tile 2 3 5 100 10 2

# full, bil

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bil line 2 3 5 100 10 1
$R2LIB/gen b 1500 50 2 -full -bil linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bil line 2 3 5 100 10 3

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bil tile 2 3 5 100 10 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bil tile 2 3 5 100 10 1

# full, bip

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bip line 2 3 5 100 10 1
$R2LIB/gen b 1500 50 2 -full -bip linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bip line 2 3 5 100 10 4

java jpl.mipl.io.vicar.test.TestGen a 1500 50 2 full bip tile 2 3 5 100 10 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 1500 50 2 full bip tile 2 3 5 100 10 1

######

# real, bsq

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bsq line 2 3 5 10 10 5
$R2LIB/gen b 100 120 8 -real -bsq linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bsq line 2 3 5 10 10 1

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bsq tile 2 3 5 10 10 1
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bsq tile 2 3 5 10 10 3

# real, bil

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bil line 2 3 5 10 10 4
$R2LIB/gen b 100 120 8 -real -bil linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bil line 2 3 5 10 10 1

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bil tile 2 3 5 10 10 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bil tile 2 3 5 10 10 3

# real, bip

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bip line 2 3 5 10 10 2
$R2LIB/gen b 100 120 8 -real -bip linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bip line 2 3 5 10 10 1

java jpl.mipl.io.vicar.test.TestGen a 100 120 8 real bip tile 2 3 5 10 10 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 100 120 8 real bip tile 2 3 5 10 10 3

######

# doub, bsq

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bsq line 2 3 5 70 160 1
$R2LIB/gen b 130 310 1 -doub -bsq linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bsq line 2 3 5 70 160 1

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bsq tile 2 3 5 70 160 2
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bsq tile 2 3 5 70 160 3

# doub, bil

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bil line 2 3 5 70 160 2
$R2LIB/gen b 130 310 1 -doub -bil linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bil line 2 3 5 70 160 1

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bil tile 2 3 5 70 160 1
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bil tile 2 3 5 70 160 3

# doub, bip

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bip line 2 3 5 70 160 1
$R2LIB/gen b 130 310 1 -doub -bip linc=2 sinc=3 binc=5
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bip line 2 3 5 70 160 2

java jpl.mipl.io.vicar.test.TestGen a 130 310 1 doub bip tile 2 3 5 70 160 3
$R2LIB/label -list a
$R2LIB/label -list a -dump
$R2LIB/difpic \(a b\)
java jpl.mipl.io.vicar.test.TestGenRead b 130 310 1 doub bip tile 2 3 5 70 160 1

########

# Now test thread reading

$R2LIB/gen a 200 200 3
java jpl.mipl.io.vicar.test.TestThreadRead a 200 200 3 17 59 10 15

########

# Clean up

rm a b

