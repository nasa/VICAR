#-----------------------------------------------------------------------
#
# This utility, and the companion vicset1, will set up VICAR for use.
# vicset1 must be run first.
#
# This file must be source'd from csh, since it sets environment variables.
#
# vicset1 sets up items that are inherited by subshells or subprocesses
# (mostly environment variables).  vicset2 sets up items that are not
# inherited (mostly aliases).  This means that only vicset2 needs to be
# rerun if a subshell is created.
#
#-----------------------------------------------------------------------

#
# Define a temporary directory (this should be on a local disk, not network!)
#
setenv VTMP /tmp/$USER
#
# Check for TAE version 4 or 5
if ($?TAE != 0) then
  if (-d $TAE/csh) then
# version 4 - the first three are set up by usrassign but are reset here in
# case this is run from a subshell.
    alias runtm $TAE/csh/tmstart
    alias tae $TAE/csh/tmstart
    alias runwb $TUTIL/wb
    alias vicar 'runtm'
  else
# version 5
    alias vicar 'taetm'
  endif
else
  unalias runtm
  unalias tae
  unalias runwb
  unalias vicar
endif

# If there's no RTL then there's no need for vimake
if ($?V2INC != 0) then
  if ($VICCPU == 'sun-solr') then
    alias vimake 'imake -D__sparc -DSOLARIS -I$V2INC -I$V2UTIL -T$V2UTIL/imake_unix.tmpl -f \!*.imake -s \!*.make' 
  else
    if ($?V2_FORCE_32 != 0) then
      set force = -DV2_FORCE_32
    else
      set force = ""
    endif
    if ($?V2_COVERAGE) then
      set cover = -DV2_COVERAGE
      echo "CODE COVERAGE BUILD ENABLED"
    else
      set cover = ""
    endif
    alias vimake 'imake '$force' '$cover' -I$V2INC -I$V2UTIL -T$V2UTIL/imake_unix.tmpl -f \!*.imake -s \!*.make'
    endif
  endif
else
  unalias vimake
endif

alias vpack $V2UTILEXE/vpack
alias vunpack $V2UTILEXE/vunpack
if ($?VRDILIB != 0) then
  alias use $VRDILIB/usedisp a
  alias free $VRDILIB/usedisp d
  alias showdisp $VRDILIB/usedisp s
  alias helpdisp $VRDILIB/usedisp h
else
  unalias use
  unalias free
  unalias showdisp
  unalias helpdisp
endif

exit

