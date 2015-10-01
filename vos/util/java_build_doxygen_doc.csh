#!/bin/tcsh
#
# This script builds doxygen documentation for all packages listed in the
# INPUT keyword of $V2HTML/java_doxygen_config.cfg.
#
#
if ( -e $V2HTML/doxygenpackagelist.tmp && $?DOXYGEN_EXE_NAME != 0 ) then

  echo "##########"
  echo "# Building Doxygendoc"
  echo "##########"

  set localConfigFile = $V2HTML/java_doxygen_config.cfg
  #remove old doc
  if ( -e $V2HTML/doxygendoc ) then
    chmod -R u+rwx $V2HTML/doxygendoc
    /bin/rm -fr $V2HTML/doxygendoc
  endif

  if ( -e $localConfigFile) then
    chmod u+rwx $localConfigFile
    /bin/rm -f $localConfigFile
  endif

  #create new doxygendoc directory
  mkdir -p  $V2HTML/doxygendoc
  chmod -R u+rwx $V2HTML/doxygendoc

  #copy and prepare doxygen configuration file
  cp $V2UTIL/java_doxygen_base_config.cfg $localConfigFile
  chmod u+rw $localConfigFile
  echo "" >> $V2HTML/doxygenpackagelist.tmp
  cat $V2HTML/doxygenpackagelist.tmp >> $localConfigFile

  #run doxygen

  $DOXYGEN_EXE_NAME $localConfigFile

  # Remove doxygen input package list since one will be generated
  # by java_build_dir.csh the system is rebuild

  if ( -e $V2HTML/doxygenpackagelist.tmp ) then
    /bin/rm -f $V2HTML/doxygenpackagelist.tmp
  endif
  echo "##########"
  echo "# Done Building Doxygendoc"
  echo "##########"

endif
