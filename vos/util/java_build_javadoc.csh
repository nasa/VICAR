#!/bin/tcsh
# Note: tcsh is used instead of csh due to restrictions on the length of an
# env var in Solaris 2.7.  $CLASSPATH needs to be more than 1024 characters.
# The above line may need to be changed to the correct path for tcsh on your
# system.  If csh is ever fixed (as it is on Linux), change this back to csh.
#
# This script builds all the javadocs for packages in $V2JAVA (results are
# put in $V2HTML/javadoc).  Note that $V2HTML/packagelist.tmp must
# exist, and it must be a list of all packages that should be built by this
# routine.  This file is automatically created by the various calls to
# java_build_dir.csh.
#
#
# Links are included to the standard Sun things:  Java core, JAI, and Java3D.
# This list may be augmented in the future.

echo "##########"
echo "# Building javadoc"
echo "##########"

cd $V2HTML

# Make sure we don't repeat packages (could happen for incremental builds)

sort $V2HTML/packagelist.tmp | uniq >$V2HTML/packagelist.tmp2

echo "Package list:"
cat $V2HTML/packagelist.tmp2

# Run javadoc


echo javadoc -J-Xmx2048m -sourcepath ${V2JAVA} -d $V2HTML/javadoc -author -version -link http://java.sun.com/j2se/1.4/docs/api -link http://java.sun.com/products/java-media/jai/forDevelopers/jai-apidocs -link http://java.sun.com/products/java-media/3D/forDevelopers/J3D_1_2_API/j3dapi @$V2HTML/packagelist.tmp2
javadoc -Xdoclint:none -J-Xmx2048m -sourcepath ${V2JAVA} -d $V2HTML/javadoc -author -version -link http://java.sun.com/j2se/1.4/docs/api -link http://java.sun.com/products/java-media/jai/forDevelopers/jai-apidocs -link http://java.sun.com/products/java-media/3D/forDevelopers/J3D_1_2_API/j3dapi @$V2HTML/packagelist.tmp2


