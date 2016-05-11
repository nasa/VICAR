#!/bin/tcsh
# Note: tcsh is used instead of csh due to restrictions on the length of an
# env var in Solaris 2.7.  $CLASSPATH needs to be more than 1024 characters.
# The above line may need to be changed to the correct path for tcsh on your
# system.  If csh is ever fixed (as it is on Linux), change this back to csh.
#
# This script builds the contents of the java directory.
#  

# For compilation only, we want CLASSPATH to point at the source

if ($?CLASSPATH != 0) then
   setenv CLASSPATH ${V2JAVA}:"${CLASSPATH}"
else
   setenv CLASSPATH ${V2JAVA}
endif

# Create output product directories if necessary

if ( ! -e $V2HTML ) then
   mkdir $V2HTML
endif
if ( ! -e $V2HTML/jars ) then
   mkdir $V2HTML/jars
endif
if ( ! -e $V2HTML/javadoc ) then
   mkdir $V2HTML/javadoc
endif

# Create a softlink between $V2HTML/jars and $V2TOP/jars to
# move the jars directory out of html.  Needed to do this to
# avoid getting "Word too long".
#

#cd $V2TOP 
#ln -s html/jars j
# Fetch all the externals

$V2UTIL/java_fetch_externals.csh

#
# Now add all of those jars into $CLASSPATH.  This is basically what vicset1
# does, but at the time vicset1 is run, these jars are not present... yet
# they will be needed for the build process.  So we must do it again here.
# We could simply source vicset1 again, but that would introduce a dependency
# on $V2TOP which we would like to avoid here, so developers can use this
# script in private directories by setting $V2HTML and $V2JAVA.
#
# First, expunge old references

if ($?CLASSPATH != 0) then
   setenv CLASSPATH `$V2TOP/util/remove_element.csh "$CLASSPATH" $V2TOP/\*`
endif

# Now, set up $CLASSPATH again, just like vicset1 does.

if ("$CLASSPATH" == "") then
#    setenv CLASSPATH ${V2TOP}/j/\*:${V2TOP}/j
    setenv CLASSPATH ${V2HTML}/jars/\*:${V2HTML}/jars
else
#    setenv CLASSPATH "${CLASSPATH}":${V2TOP}/j/\*:${V2TOP}/j
    setenv CLASSPATH "${CLASSPATH}":${V2HTML}/jars/\*:${V2HTML}/jars
endif

# The packagelist.tmp file is accumulated during the build and used for
# running javadoc on all packages at once at the end.  Non-packaged files
# have javadoc run on them during their individual build.

if ( -e $V2HTML/packagelist.tmp ) then
   /bin/rm -f $V2HTML/packagelist.tmp
endif

# Recursively go through each directory and build it.  Accumulates
# packagelist.tmp.  Ignores any directory named "test".

# Note that we are explicitly listing the top-level directories (TLD)
# in the java tree.  If you add another TLD, add to this list!  In two places.

# Phase 1 generates all source code (e.g. from IDL or JAXB)

echo "*********************************************"
echo "**** CODE GENERATION PHASE OF JAVA BUILD ****"
echo "*********************************************"

cd $V2JAVA
foreach tld (jpl com gov one the)
  find $tld -name test -prune -o -type d -exec $V2UTIL/java_build_dir.csh {} generate \;
end


# Phase 2 compiles the code and does everything else

echo "*************************************"
echo "**** COMPILE PHASE OF JAVA BUILD ****"
echo "*************************************"

cd $V2JAVA
foreach tld (jpl com gov one the)
  find $tld -name test -prune -o -type d -exec $V2UTIL/java_build_dir.csh {} compile \;
end

# Now run javadoc on all the generated packages.

$V2UTIL/java_build_javadoc.csh

# Generate doxygen documentation

$V2UTIL/java_build_doxygen_doc.csh

# The build is done...

echo "THE BUILD IS FINISHED"
