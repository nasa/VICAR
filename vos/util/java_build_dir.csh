#!/bin/tcsh
# Note: tcsh is used instead of csh due to restrictions on the length of an
# env var in Solaris 2.7.  $CLASSPATH needs to be more than 1024 characters.
# The above line may need to be changed to the correct path for tcsh on your
# system.  If csh is ever fixed (as it is on Linux), change this back to csh.
#
# This utility builds a single directory's worth of Java files.
#
# Usage:
# $V2UTIL/java_build_dir.csh rel_dir_path [phase]
# where rel_dir_path is the pathname to build relative to the $V2JAVA
# directory and phase is the optional build phase (see below).
#
# All build products go into the $V2HTML directory.  Jars go in $V2HTML/jars,
# javadoc in $V2HTML/javadoc, and non-packaged .class files go in $V2HTML
# in the subdirectory specified by rel_dir_path (argument 1 to the script).
#
# For package files, javadoc is not run immediately; rather a list of package
# names is built up and the driver of this is expected to run javadoc.  Non-
# package files are javadoc'd here.
#
# It is recommended that each (package) directory contain a "package.html"
# file, which will be automatically picked up by javadoc to describe the
# overall function of that package.
#
# Each directory may have a 0build.jmake file in it, which specifies options
# to build that directory.  The file must be in a format acceptable to v2param
# (basically, keyword=value lines).  Lists of more than one item must be
# enclosed in parentheses and separated by commas, with no spaces.  Valid
# commands for this file are shown below.  The default value is shown.
#
# BTW, the odd name of 0build.jmake is because 0 sorts at the top
# alphabetically, so the file is easy to find, and the file is conceptually
# at least loosely based in vimake (thus jmake).
#
# The build occurs in multiple "phases" (currently two).  In the first phase,
# code is generated from non-code sources, such as IDL or JAXB.  In the
# second phase, everything else is done (compile, jar, etc).  The "phase"
# argument says what to do:
#
# generate : Generate java code
# compile : Everything else (compile, jar, etc.)
# all : both of the above
#
# If the phase name is not given, "all" is assumed.
#
# Commands for 0build.jmake:
#
# JDK_version = 1.3
#     Specifies which version of the JDK to compile with.  Currently allowed:
#     1.3, 1.2, 1.1.  Note:  This should ONLY be specified if an old version
#     of the JDK is needed for some reason.  Most code should leave it
#     unspecified, which means to use the current default version of the JDK.
#     Note that if this is specified, the directory /usr/java${JDK_version}
#     will be *explicitly* used, rather than the normal JDK setup in vicset1
#     via V2JDK.  This could cause problems at external sites.
#
# packaged = 1
#     0 means there is no package, so javadoc is created independently
#     (in $V2HTML/javadoc/nopackage).  This should be avoided; most code
#     should be in a package.  1 means there is a package, so javadoc is
#     generated with the rest of the packages (at the end of the build process).
#
# jar_file = mipl.jar
#     Specifies the name of the jar file for the files in this directory.
#     All jar files go in $V2HTML/jars.  Note that the jar files are always
#     created with a 1.2 (or later) jar, even if JDK_version is 1.1.
#
# extra_jar_files =
#     A list of filename patterns that will be added to the jar in addition
#     to the *.class files.  Note that .class files are always added to the
#     specified jar file.  Multiple patterns should be enclosed in
#     parentheses and comma-separated, e.g.
#     extra_jar_files = (*.gif,xyz.xml,*.jpeg)
#
# non_jar_files =
#     A list of filename patterns that will be put into $V2HTML in a
#     subdirectory that matches the $1 parameter (the package tree structure).
#     Note that the same files can go here *and* in a jar, if desired.
#     This follows the same format as extra_jar_files.
#
# no_java = 0
#     1 means there are no Java files to compile in this directory.  An
#     example might be an image subdirectory (in which case you'd want to
#     make use of extra_jar_files or non_jar_files).  If no 0build.jmake
#     file exists *and* there are no *.java files present, then the directory's
#     build is skipped (because without a jmake and with no .java files, there
#     is nothing to do).
#
# manifest_file =
#     Specifies the manifest file to include in the jar.  If none is specified
#     (the default), jar will create one for you.  Specifying more than
#     one manifest (in different subdirectories) for the same jar may produce
#     undefined results (it depends on how jar itself deals with it, although
#     it appears that jar simply merges the entries, so it may be okay).  Note
#     that there is no guarantee of the order in which subdirectories are
#     processed during the build, so there must be no order dependencies among
#     multiple manifest files, either.
#
# jni_classes =
#     Specifies a list of class names for which to run "javah -jni" to
#     generate native includes.  Includes are put in the $V2HTML/inc
#     directory.  Note that this is a list of CLASS names, not FILE names.
#     The package specified in $1 is automatically prepended to each element,
#     so you should list only the base class name with no package or
#     extension (such as .java or .class).
#
# idl_files =
#     Specifies a list of files to be run through an IDL->Java compiler.
#     (this is Interface Definition Language for CORBA, et al, not Research
#     Systems' IDL).  The default compiler is jidl from Orbacus 
#     (reached via $J2_ORBACUS/bin/$VICCPU/jidl), 
#     but additional flags may specify a different
#     compiler if necessary in the future.  All files are generated with the
#     current package name.  If an IDL file is shared with the C/C++ world,
#     it should be delivered in a .com file into the appropriate place in the
#     VICAR C/C++ directory structure.  The post-fetch fixup script,
#     ??????????????????, will extract and copy (or softlink?????) each relevant
#     idl file to the Java directories on a case-by-case basis.  This means
#     the script itself must be modified to add or remove shared IDL files;
#     the 0build.jmake file itself is not sufficient.
#
# rmi_classes =
#     Specifies a list of class names to be run through the rmic compiler
#     in order to generate Stub and Skeleton class files.  Note that this
#     is a list of CLASS names, not FILE names.  The package specified in
#     $1 is automatically prepended to each element, so you should list only
#     the base class name with no package or extension (such as .java or
#     .class).  Source is not generated because it is not needed during a
#     build; developers can run rmic manually if they wish to inspect the
#     source.  Generation of IDL files from rmic is not currently supported;
#     contact CM if you need this capability.
#
# services_files =
#     Specifies a list of files to go into the META-INF/services directory.
#     These files typically list services included in the jar, such as
#     JAI operators, IIO plugins, or MICA tools.  The filenames by convention
#     use a reverse domain naming scheme (e.g. jpl.mipl.mica.ToolPalette) but
#     the build script will accept any filename.  These files are put in the
#     META-INF/services directory rather than the directory in which they
#     reside.  The result of two different directories contributing the same
#     file to the same jar are undefined; whoever happened to get added last
#     wins (i.e., don't do that).
#
# jaxb_schema =
#     Specifies a (single) file containing an XML Schema.  This file is
#     run through JAXB to create interface and implementation files for JAXB.
#     Currently the Sun reference implementation of JAXB is used, but this
#     may change in the future.  The generated code will be in the package
#     implied by the directory currently being built (i.e. specified by the
#     argument to this script).  Note that subpackages (subdirectories) are
#     also created.  These are NOT built by default.  Each subdir must have
#     a pre-existing 0build.jmake file in it, telling how to build the
#     generated code.  So the subdirs in the CM system should be empty except
#     for the build file.  Note that JAXB sometimes generates "*.ser" or
#     "jaxb.properties" files; these MUST be listed in extra_jar_files if
#     they are to be included in the jar; this is not done automatically.
#     It is recommended that the specified schema file exist in the directory
#     being built, in which case only the filename is specified.  However,
#     it is possible for the schema file to reside in a different directory
#     (e.g. if there was some runtime reason for it to be in a different
#     path).  In that case, a directory spec relative to $V2JAVA may be
#     specified for jaxb_schema. (e.g. jaxb_schema=jpl/mipl/mdms/schema/x.xsd).
#     Note that only one schema may be built per directory.
#
# wsdl_files =
#     Specifies a list of WSDL (Web Services Description Language) files
#     to be run through the "wsimport" code generator/compiler.

# ejb_descriptor = 0
#     If set to 1 or 2, then EJB deployment descriptors will be generated for
#     all code in this directory.  If set to 1, subdirs are recursively
#     included; if set to 2, only the current dir is processed.  The files
#     "ejb-jar.xml" and "jboss.xml" will be put in a subdirectory named
#     "META-INF" below this one.  Nothing else is done with the files by
#     the build process; they just sit there.  Because it is likely that
#     deployment descriptors will need to be hand-edited, it is up to the
#     user to put these files in the appropriate jar/war/ear at deployment
#     time.
#
# dependency =
#     List the dependencies (jar files) upon which this build depends.
#     This includes both external jars and internal software that is used
#     by the code in this directory.  Do not list the jar in which this
#     code resides (given by jar_file); that is implied.  Setting a
#     dependency will cause classpath to be reset so that ONLY the jars
#     specified here (plus jar_file) will be used.  The dependency should
#     be listed without a version or ".jar"; a "*.jar" is automatically
#     appended to each entry.  This allows versions to be upgraded without
#     requiring every dependency line in every build file to change.  It is
#     acceptable to list a version number if it is absolutely required, but
#     this should in general not be done.  Note that the pattern match
#     allows multiple jars to be included in each line; for example just
#     listing "jaxb" would match all of "jaxb-api.jar", "jaxb-impl.jar",
#     and "jaxb-libs.jar".
#
#

cd $V2JAVA/$1
set package = `echo $1 | sed 's/\//\./g'`

set phase_generate = 1
set phase_compile = 1

if ($#argv == 2) then
   if ("$2" == "generate") then
      set phase_compile = 0
   endif
   if ("$2" == "compile") then
      set phase_generate = 0
   endif
endif

echo "##########"
echo "# Java package " $package
echo "##########"

# For compilation only, we want CLASSPATH to point at the source

setenv CLASSPATH ${V2JAVA}:"${CLASSPATH}"

# Deal with some users who actually like this... messes up ">>" at least.
unset noclobber

# Property defaults

### set JDK_version = 1.3   # defaults to not set
set packaged = 1
set jar_file = mipl.jar
set extra_jar_files =
set non_jar_files =
set no_java = 0
set manifest_file =
set jni_classes =
set idl_files =
set rmi_classes =
set services_files =
set jaxb_schema =
set wsdl_files =
set ejb_descriptor = 0
set dependency =

# Property overrides from 0build.jmake

if ( -e 0build.jmake ) then
   setenv V2PARAM_FILE 0build.jmake

   if (`v2param -test JDK_version` != 0) \
      set JDK_version = `v2param JDK_version`
   if (`v2param -test packaged` != 0) \
      set packaged = `v2param packaged`
   if (`v2param -test jar_file` != 0) \
      set jar_file = `v2param jar_file`
   if (`v2param -test extra_jar_files` != 0) \
      set extra_jar_files = `v2param extra_jar_files`
   if (`v2param -test non_jar_files` != 0) \
      set non_jar_files = `v2param non_jar_files`
   if (`v2param -test no_java` != 0) \
      set no_java = `v2param no_java`
   if (`v2param -test manifest_file` != 0) \
      set manifest_file = `v2param manifest_file`
   if (`v2param -test jni_classes` != 0) \
      set jni_classes = `v2param jni_classes`
   if (`v2param -test idl_files` != 0) \
      set idl_files = `v2param idl_files`
   if (`v2param -test rmi_classes` != 0) \
      set rmi_classes = `v2param rmi_classes`
   if (`v2param -test services_files` != 0) \
      set services_files = `v2param services_files`
   if (`v2param -test jaxb_schema` != 0) \
      set jaxb_schema = `v2param jaxb_schema`
   if (`v2param -test wsdl_files` != 0) \
      set wsdl_files = `v2param wsdl_files`
   if (`v2param -test ejb_descriptor` != 0) \
      set ejb_descriptor = `v2param ejb_descriptor`
   if (`v2param -test dependency` != 0) \
      set dependency = `v2param dependency`
else

   # If no *.java files are present, we can completely skip this directory
   # since, with no .jmake file, no work can possibly be specified.

   ls -1 | grep \.java >/dev/null
   if ($status == 1) \
      exit

endif

# Set JDK version.  May be platform dependent!!!

set oldpath = ($path)
if ($?JDK_version != 0) then
   set path = ( /usr/java$JDK_version/bin $path )
endif

# Set up CLASSPATH, if dependency is given, to just those dependencies.
# There are two options here.  First, if you want to use (empty) dependencies
# *everywhere*, in order to see what fails, then enable the temp_garbage line
# in the first if.  This should be done only during development of the
# dependency lists themselves.  Second, if you want to look only at external
# dependencies, include ${V2JAVA} in $CLASSPATH.  This *must* be done during
# a full-system build, because the dependant internal jars may not be built
# yet.  Leaving it out is useful for internal dependency checks against an
# already-built system.

### enable the second, disable the first, for forcing on dependencies
if ("$dependency" != "") then
### if ("$dependency" != "wwefefetewrewewwet_temp_garbage") then

###    enable the second, disable the first, for internal dependency checking
   setenv CLASSPATH ${V2JAVA}:$V2HTML/jars/$jar_file
###   setenv CLASSPATH $V2HTML/jars/$jar_file
   foreach i ($dependency)
      set nonomatch
      set jar = $V2HTML/jars/${i}*.jar
      unset nonomatch
      if ("$jar" != "$V2HTML/jars/${i}*.jar") then
         foreach j ($jar)
         setenv CLASSPATH "${CLASSPATH}":$j
         end
      endif
   end
   echo "setenv CLASSPATH ${CLASSPATH}"
endif

# Do IDL, JAXB, and WSDL generation only if generate phase is on

if ($phase_generate == 1) then

   # Generate Java files from IDL, if needed

   if ("$idl_files" != "") then
      if (-e $J2_ORBACUS/lib/$VICCPU) then
         setenv LD_LIBRARY_PATH $J2_ORBACUS/lib/${VICCPU}:$LD_LIBRARY_PATH
      endif
      foreach idl ($idl_files)

         echo $J2_ORBACUS/bin/$VICCPU/jidl --output-dir $V2JAVA $idl
         $J2_ORBACUS/bin/$VICCPU/jidl --output-dir $V2JAVA $idl

      end
   endif

   # Generate Java files from JAXB, if needed
   # Note that subdirs are created too; they should already exist with their
   # own build files.  We do not here descend into these subdirs, relying
   # instead on the caller to do so.

   if ("$jaxb_schema" != "") then

      set schema_file = `pwd`/$jaxb_schema
      if ( ! -e $schema_file ) then
         set schema_file = $V2JAVA/$jaxb_schema
      endif

      echo setenv JAVA_HOME $V2JDK
      setenv JAVA_HOME $V2JDK
      echo $J2_JAXB/bin/xjc.sh -p $package $schema_file -d $V2JAVA \</dev/null
      $J2_JAXB/bin/xjc.sh -p $package $schema_file -d $V2JAVA </dev/null

   endif

   # Generate Java files from WSDL, if needed
   # Note that wsimport compiles the files as well; they are re-compiled
   # during the compilation phase from the generated source.

   if ("$wsdl_files" != "") then
      foreach wsdl ($wsdl_files)
          
         setenv JAVA_HOME $V2JDK
         echo $J2_JAXWS/bin/wsimport.sh -s $V2JAVA -d $V2JAVA -p $package -keep -verbose $wsdl
         $J2_JAXWS/bin/wsimport.sh -s $V2JAVA -d $V2JAVA -p $package -keep -verbose $wsdl
         unsetenv JAVA_HOME
      end
   endif

endif

# If we aren't compiling, just bail out now

if ($phase_compile == 0) then
   exit
endif

# Compile the Java

if ($no_java == 0) then

   echo javac *.java | fold -80
   javac *.java

   # Generate javadoc (or the package list for later generation)

   if ( ! -e $V2HTML/javadoc ) then
      mkdir -p $V2HTML/javadoc
   endif

   if ($packaged == 0 ) then
      if ( ! -e $V2HTML/javadoc/nopackage ) then
         mkdir -p $V2HTML/javadoc/nopackage
      endif
      echo javadoc -J-Xmx2048 -d $V2HTML/javadoc/nopackage -author -version *.java | fold -80
      javadoc -J-Xm2048m -d $V2HTML/javadoc/nopackage -author -version *.java 
   else   
      echo $package >> $V2HTML/packagelist.tmp
   endif

endif   

# Generate stubs and skeletons via rmic
 
if ("$rmi_classes" != "") then
   foreach class ($rmi_classes)
      echo rmic -d $V2JAVA ${package}.${class} | fold -80
      rmic -d $V2JAVA ${package}.${class}
   end
endif

# Generate EJB deployment descriptors
# Note: Ant is no longer maintained in VICAR external. 
# The System version is being used.
#
# Recursive
if ( $ejb_descriptor == 1 ) then
   setenv JAVA_HOME $V2JDK
   ant -f $V2UTIL/java_build_ejb_descriptor.xml recursive
endif

# Non-recursive
if ( $ejb_descriptor == 2 ) then
   setenv JAVA_HOME $V2JDK
   ant -f $V2UTIL/java_build_ejb_descriptor.xml package-only
endif

# Update jar file.  We reset $path because only jar 1.2 can handle update
# mode, so we use it even if the user specified 1.1.

set path = ($oldpath)

if ( ! -e $V2HTML/jars ) then
   mkdir -p $V2HTML/jars
endif

if ( -e $V2HTML/jars/$jar_file ) then
   set jar_opt = uf
else
   set jar_opt = cf
endif
if ("$manifest_file" != "") then
   set jar_opt = ${jar_opt}m
endif

# Since we want to maintain the package directory structure in the jar file,
# jar requires that the given name include the package directory (ugh!).
# So, we must go through the extra_jar_files list and prepend $1 to each of
# the entries.

set extra_jar_files2 =
if ("$extra_jar_files" != "") then
   foreach f ($extra_jar_files)
      set extra_jar_files2 = ($extra_jar_files2 $1/$f)
   end
endif

set manifest_file2 =
if ("$manifest_file" != "") then
   set manifest_file2 = $1/$manifest_file
endif

# If there are no java files, don't add *.class.

cd $V2JAVA

if ($no_java == 0) then
   echo jar $jar_opt $V2HTML/jars/$jar_file $manifest_file2 $1/*.class $extra_jar_files2 | fold -80
   jar $jar_opt $V2HTML/jars/$jar_file $manifest_file2 $1/*.class $extra_jar_files2
else
   echo jar $jar_opt $V2HTML/jars/$jar_file $manifest_file2 $extra_jar_files2 | fold -80
   jar $jar_opt $V2HTML/jars/$jar_file $manifest_file2 $extra_jar_files2
endif

cd $V2JAVA/$1

# Add the services files to the jar.  This can't be done above because the
# META-INF/services directory has to exist off of the current directory, which
# in the above case is $V2JAVA.  Putting META-INF/services files there would
# lead to possible confusion between jars (since different jars will often
# have the same services filename) and prevent any possibility of building
# more than one directory at a time.

if ("$services_files" != "") then
    echo mkdir META-INF
    mkdir META-INF
    echo mkdir META-INF/services
    mkdir META-INF/services
    echo cp $services_files META-INF/services
    cp $services_files META-INF/services
    echo jar uf $V2HTML/jars/$jar_file META-INF/services/*
    jar uf $V2HTML/jars/$jar_file META-INF/services/*
    echo rm -fr META-INF
    rm -fr META-INF
endif

# Copy non-jar files to the right spot

if ("$non_jar_files" != "") then
   if ( ! -e $V2HTML/$1 ) then
      mkdir -p $V2HTML/$1
   endif

   echo cp $non_jar_files $V2HTML/$1 | fold -80
   cp $non_jar_files $V2HTML/$1
endif

# Generate JNI includes

if ("$jni_classes" != "") then
   if ( ! -e $V2HTML/inc ) then
      mkdir -p $V2HTML/inc
   endif

# Formerly we had to add jars from the JDK extensions directory to classpath
# explicitly (see JDC Bug #4349304).  However, this seems to have been fixed
# somewhere in 1.5, and is no longer needed.  The code to find these jars is
# available in Harvest history, if it is necessary to resurrect it.
# rgd 5/12/08

   foreach class ($jni_classes)
      echo javah -jni -d $V2HTML/inc ${package}.${class}
      javah -jni -d $V2HTML/inc ${package}.${class}
   end
endif

