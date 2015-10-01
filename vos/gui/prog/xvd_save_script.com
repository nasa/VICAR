$!****************************************************************************
$!
$! Build proc for MIPL module xvd_save_script
$! VPACK Version 1.9, Thursday, March 01, 2001, 15:20:32
$!
$! Execute by entering:		$ @xvd_save_script
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module xvd_save_script ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvd_save_script.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xvd_save_script.imake") .nes. ""
$   then
$      vimake xvd_save_script
$      purge xvd_save_script.bld
$   else
$      if F$SEARCH("xvd_save_script.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvd_save_script
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvd_save_script.bld "STD"
$   else
$      @xvd_save_script.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvd_save_script.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvd_save_script.com -mixed -
	-s xvd_save_script.sh -
	-i xvd_save_script.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xvd_save_script.sh
$ DECK/DOLLARS="$ VOKAGLEVE"
#!/bin/sh

########################################################################
# Shell script to save or print a file from xvd.
#
# The single argument is a file output by SiSaveAsCmd or SiPrintCmd (see the
# source for the contents).  Interpret the file and save as requested.  The key
# parameters for saving are:
# saveFilename:		self-explanatory
# saveImageExtent:	file (full image), display (disp only), roi (not impl)
# saveLutType:		raw, stretch, pseudo, psuedo_only
# saveAsByte:		0 = no (preserve data type, no stretches if not byte)
#			1 = yes (convert to byte, stretches allowed)
# saveFileFormat:	Currently must be VICAR or TIFF
#
# Note that there's some weirdness when running VICAR programs.  If xvd
# is in the background, running a VICAR program causes the process to
# suspend for TTY *output*!!  Duuh, the script has been outputting all
# along!  I really don't understand this.  But, redirecting the output
# to a file, then cat'ing the file, seems to work around this.
#
# Due to weirdnesses in the VTIFF program, VTIFF2 is used for Linux and
# VTIFF for everything else.  If this ever gets fixed (to have a consistent
# program everywhere), change the program name in the TIFF code.
#
# The key parameters for printing are:
# printTo:		file or printer
# printCommand:		command to use for printing (normally lpr, may contain
#			printer queue options, etc.)
# printWidth:		width in inches of the printed image
# printHeight:		height in inches of the printed image
# orientation:		tall or wide (i.e. portrait or landscape)
# printTitle:		title to print with the image.  Blank for no title.
#
# Printing uses the same process as saving, except that the VICAR program
# PSCRIPT is run at the end.
########################################################################

# Make v2param program read our file (convenient that the formats match ;-}  )
# No need to save/restore the value since we're running in a subshell anyway...

V2PARAM_FILE=$1; export V2PARAM_FILE

# Check for printing or saving

PRINTING="`v2param -test printTo`"

# Check file format, bail if bad

if [ "`v2param saveFileFormat`" != "VICAR" -a "`v2param saveFileFormat`" != "TIFF" ]; then
  echo >&2 "ERROR: Output format not VICAR or TIFF"
  exit
fi

# Get the extent, set up some NL/NS parameters for the command line.
# If we're doing the whole file, these parameters are blank.
# Note that we are using the unrotatedDisplayBounds because the first
# thing we do is flot the image with these bounds.  If there's no rotation,
# it's the same as displayBounds.
# imageWidth and imageHeight are of the *rotated* image, so we must swap
# them in some cases to get the h/w of the file itself (unrotated h/w).

if [ "`v2param saveImageExtent`" = "display" ]; then
  height=`v2param imageHeight`
  width=`v2param imageWidth`
  mode="`v2param rotateMode`"
  if [ "$mode" != "none" -a "$mode" != "180" ]; then
#   Swap height/width
    height=`v2param imageWidth`
    width=`v2param imageHeight`
  fi
  ss=`v2param -i 0 unrotatedDisplayBounds`
  ss=`expr $ss + 1`
  sl=`v2param -i 1 unrotatedDisplayBounds`
  sl=`expr $sl + 1`
  es=`v2param -i 2 unrotatedDisplayBounds`
  if [ $es -ge $width ]; then
    es=$width
    es=`expr $es - 1`
  fi
  ns=`expr $es - $ss + 2`
  el=`v2param -i 3 unrotatedDisplayBounds`
  if [ $el -ge $height ]; then
    el=$height
    el=`expr $el - 1`
  fi
  nl=`expr $el - $sl + 2`

  NLNS="ss=$ss sl=$sl ns=$ns nl=$nl"
else
  NLNS=""
fi

NLNS_ORIG="$NLNS"

########################################################################
# Check to see if FLOT is needed to rotate the image.  See the FLOT section
# for the meaning of FLOT2_NEEDED.

FLOT_NEEDED=0
if [ "`v2param rotateMode`" != "none" ]; then
  FLOT_NEEDED=1
fi
FLOT2_NEEDED=0
if [ "`v2param rotateMode`" = "flip_nw_se" ]; then
  FLOT2_NEEDED=1
fi

########################################################################
# Check to see if CFORM is needed to convert data types.

CFORM_NEEDED=0
if [ "`v2param saveAsByte`" = "1" -a "`v2param dataType`" != "byte" ]; then
  CFORM_NEEDED=1
fi
if [ "$PRINTING" = "1" -a "`v2param dataType`" != "byte" ]; then
  CFORM_NEEDED=1
fi
if [ "`v2param saveAsByte`" = "0" -a "`v2param dataType`" != "byte" -a \
	"`v2param saveFileFormat`" = "TIFF" ]; then
  echo "WARNING: TIFF files may be saved in BYTE only."
  echo "         Automatically enabling Convert to Byte option."
  CFORM_NEEDED=1
fi

save_lut_type="`v2param saveLutType`"

if [ "`v2param saveAsByte`" = "0" -a "`v2param dataType`" != "byte" -a \
	"$save_lut_type" != "raw" ]; then
  echo >&2 "ERROR: Can't apply Stretch or Pseudo to non-byte data!"
  exit
fi

########################################################################
# Check to see if VLOOKUP is needed, once or twice (twice: str + pseudo).

STRETCH_NEEDED=0
PSEUDO_NEEDED=0
if [ "$save_lut_type" = "stretch" ]; then
  STRETCH_NEEDED=1
fi
if [ "$save_lut_type" = "pseudo" ]; then
  STRETCH_NEEDED=1
  PSEUDO_NEEDED=1
fi
if [ "$save_lut_type" = "pseudo_only" ]; then
  PSEUDO_NEEDED=1
fi

if [ "$PSEUDO_NEEDED" = 1 -a "`v2param imageMode`" != "bw" ]; then
  echo >&2 "ERROR: Can't apply Pseudocolor tables to non-BW image!"
  exit
fi

if [ "$PSEUDO_NEEDED" = 1 -a "`v2param -test pseudoFile`" = 0 ]; then
  echo >&2 "ERROR: Can't save Pseudocolor when not in Pseudocolor mode!"
  exit
fi

########################################################################
# Get input filename(s)

orig_filename=`v2param filename`

# Remove any band specifiers (BAND IS IGNORED AT THIS TIME!!!!)

filename=`echo $orig_filename | sed 's#([^)]*)##g'`
if [ $filename != $orig_filename ]; then
  echo >&2 "WARNING:  Band specifiers in the input files are ignored!"
fi

# Split into separate filenames

if [ "`v2param imageMode`" = "color" ]; then

  # Make sure there are three files with commas (not a complete check!)
  tmp=`echo $filename | sed 's/,[^,]/XX/'`
  if [ $tmp = $filename ]; then
    echo >&2 "ERROR:  Color output must have three separate input files"
    exit
  fi

  INP_1=`echo $filename | sed 's#\([^, ]*\)[, ].*#\1#'`
  INP_2=`echo $filename | sed 's#[^, ]*[, ]\([^, ]*\)[, ].*#\1#'`
  INP_3=`echo $filename | sed 's#[^, ]*[, ][^, ]*[, ]\([^, ]*\)#\1#'`

# convert .grn to file.grn

  if [ "`echo $INP_2 | cut -c1`" = "." ]; then
    INP_2=`echo $INP_1 | sed "s/\.[^.]*\$/$INP_2/"`
  fi
  if [ "`echo $INP_3 | cut -c1`" = "." ]; then
    INP_3=`echo $INP_1 | sed "s/\.[^.]*\$/$INP_3/"`
  fi

else					# bw input

  INP_1=`echo $filename | sed 's/,//g'`
  INP_2=""
  INP_3=""
fi

########################################################################
# Get output filename(s).  This is a bit easier because we can use
# v2param to split the values.  For TIFF, only one filename is needed...
# but we simulate 3 for color so the VICAR processing can use them.
# For printing, we generate a temporary name unless printing to a file.

if [ "$PRINTING" = "0" ]; then
  if [ "`v2param saveFileFormat`" = "VICAR" ]; then

#
# VICAR
#
    if [ "`v2param imageMode`" = "color" -o "$PSEUDO_NEEDED" = 1 ]; then
      if [ "`v2param -n saveFilename`" != "3" ]; then
        echo >&2 "ERROR: Must have 3 output files for Color or Pseudocolor!"
        exit
      fi
      OUT_1="`v2param -i 0 saveFilename`"
      OUT_2="`v2param -i 1 saveFilename`"
      OUT_3="`v2param -i 2 saveFilename`"

# convert .grn to file.grn

      if [ "`echo $OUT_2 | cut -c1`" = "." ]; then
        OUT_2=`echo $OUT_1 | sed "s/\.[^.]*\$/$OUT_2/"`
      fi
      if [ "`echo $OUT_3 | cut -c1`" = "." ]; then
        OUT_3=`echo $OUT_1 | sed "s/\.[^.]*\$/$OUT_3/"`
      fi

# Translate any ~'s in the names

      OUT_1=`csh -fc "echo $OUT_1"`
      OUT_2=`csh -fc "echo $OUT_2"`
      OUT_3=`csh -fc "echo $OUT_3"`

    else
      if [ "`v2param -n saveFileName`" != "1" ]; then
        echo >&2 "WARNING:  Only 1 output file needed for BW.  Other two ignored."
      fi
      OUT_1="`v2param -i 0 saveFileName`"
      OUT_2=""
      OUT_3=""

# Translate any ~'s in the name

      OUT_1=`csh -fc "echo $OUT_1"`

    fi
  fi

  if [ "`v2param saveFileFormat`" = "TIFF" ]; then
#
# TIFF
#
    if [ "`v2param -n saveFilename`" != "1" ]; then
      echo >&2 "WARNING:  Only 1 output file needed for TIFF.  Other two ignored."
    fi

    OUT_TIFF="`v2param -i 0 saveFilename`"

# Translate any ~'s in the name

    OUT_TIFF=`csh -fc "echo $OUT_TIFF"`

# Create VICAR output file names based on the TIFF name for the rest of the
# processing steps.  The TIFF conversion is the last step performed.

    if [ "`v2param imageMode`" = "color" -o "$PSEUDO_NEEDED" = 1 ]; then
      OUT_1="$OUT_TIFF"_$$_`date +%H%M%S`_red
      OUT_2="$OUT_TIFF"_$$_`date +%H%M%S`_grn
      OUT_3="$OUT_TIFF"_$$_`date +%H%M%S`_blu
    else
      OUT_1="$OUT_TIFF"_$$_`date +%H%M%S`_vic
      OUT_2=""
      OUT_3=""
    fi

  fi

else
#
# PRINTING
#
  if [ "`v2param printTo`" = "file" ]; then
    OUT_PRINT="`v2param -i 0 saveFilename`"
  else
    OUT_PRINT=/tmp/print_$$_`date +%H%M%S`.ps
  fi

# Translate any ~'s in the name

  OUT_PRINT=`csh -fc "echo $OUT_PRINT"`

# Create VICAR output file names based on the print name for the rest of the
# processing steps.  The postscript conversion is the last step performed.

  if [ "`v2param imageMode`" = "color" -o "$PSEUDO_NEEDED" = 1 ]; then
    OUT_1="$OUT_PRINT"_$$_`date +%H%M%S`_red
    OUT_2="$OUT_PRINT"_$$_`date +%H%M%S`_grn
    OUT_3="$OUT_PRINT"_$$_`date +%H%M%S`_blu
  else
    OUT_1="$OUT_PRINT"_$$_`date +%H%M%S`_vic
    OUT_2=""
    OUT_3=""
  fi

fi

if [ "$OUT_1" = "" ]; then
  echo >&2 "ERROR: No output filename given!"
  exit
fi

########################################################################
# Now go through and process each step.

# Remember the current filename in "CURR_x".  This is modified when there's
# an intermediate step.

CUR_1="$INP_1"
CUR_2="$INP_2"
CUR_3="$INP_3"
CUR_IS_INP="1"

# Get a temporary file name for VICAR stdout/stderr output (see comments
# at top)

TMPOUT="$OUT_1"_$$_`date +%H%M%S`_out

error=0

########################################################################
# FLOT
#
# All rotation modes are support by FLOT except, unfortunately, flip_nw_se.
# For this one, we need to do 2 flot's, the first -trans (like flip_ne_sw)
# and the second a 180-degree rotation.

if [ "$FLOT_NEEDED" = "1" ]; then
  O1="$OUT_1"
  O2="$OUT_2"
  O3="$OUT_3"
  if [ "`expr $FLOT2_NEEDED + $CFORM_NEEDED + $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    # there is a follow-on step
    O1="$OUT_1"_$$_`date +%H%M%S`_fl
    O2="$OUT_2"_$$_`date +%H%M%S`_fl
    O3="$OUT_3"_$$_`date +%H%M%S`_fl
  fi

  FLOT_MODE=""
  if [ "`v2param rotateMode`" = "cw" ]; then
    FLOT_MODE="-clock"
  fi
  if [ "`v2param rotateMode`" = "ccw" ]; then
    FLOT_MODE="-counter"
  fi
  if [ "`v2param rotateMode`" = "180" ]; then
    FLOT_MODE="-rot180"
  fi
  if [ "`v2param rotateMode`" = "flip_nw_se" ]; then
    FLOT_MODE="-trans"
  fi
  if [ "`v2param rotateMode`" = "flip_ne_sw" ]; then
    FLOT_MODE="-trans"
  fi

  echo $R2LIB/flot inp=$CUR_1 out=$O1 $NLNS $FLOT_MODE
  $R2LIB/flot inp=$CUR_1 out=$O1 $NLNS $FLOT_MODE >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  if [ "$INP_2" != "" ]; then
    echo $R2LIB/flot inp=$CUR_2 out=$O2 $NLNS $FLOT_MODE
    $R2LIB/flot inp=$CUR_2 out=$O2 $NLNS $FLOT_MODE >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi
  if [ "$INP_3" != "" ]; then
    echo $R2LIB/flot inp=$CUR_3 out=$O3 $NLNS $FLOT_MODE
    $R2LIB/flot inp=$CUR_3 out=$O3 $NLNS $FLOT_MODE >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi

  NLNS=""

  if [ "`expr $FLOT2_NEEDED + $CFORM_NEEDED + $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    CUR_1="$O1"
    CUR_2="$O2"
    CUR_3="$O3"
    CUR_IS_INP=0
    if [ "$INP_2" = "" ]; then
      CUR_2=""
    fi
    if [ "$INP_3" = "" ]; then
      CUR_3=""
    fi
  fi
fi

########################################################################
# FLOT2
#
# The second flot for flip_nw_se.

if [ "$FLOT2_NEEDED" = "1" ]; then
  O1="$OUT_1"
  O2="$OUT_2"
  O3="$OUT_3"
  if [ "`expr $CFORM_NEEDED + $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    # there is a follow-on step
    O1="$OUT_1"_$$_`date +%H%M%S`_fl2
    O2="$OUT_2"_$$_`date +%H%M%S`_fl2
    O3="$OUT_3"_$$_`date +%H%M%S`_fl2
  fi

  echo $R2LIB/flot inp=$CUR_1 out=$O1 $NLNS -rot180
  $R2LIB/flot inp=$CUR_1 out=$O1 $NLNS -rot180 >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  if [ "$INP_2" != "" ]; then
    echo $R2LIB/flot inp=$CUR_2 out=$O2 $NLNS -rot180
    $R2LIB/flot inp=$CUR_2 out=$O2 $NLNS -rot180 >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi
  if [ "$INP_3" != "" ]; then
    echo $R2LIB/flot inp=$CUR_3 out=$O3 $NLNS -rot180
    $R2LIB/flot inp=$CUR_3 out=$O3 $NLNS -rot180 >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi

  NLNS=""

  # Remove temp files

  if [ "$CUR_IS_INP" = 0 ]; then
    echo rm $CUR_1 $CUR_2 $CUR_3
    rm $CUR_1 $CUR_2 $CUR_3
  fi

  if [ "`expr $CFORM_NEEDED + $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    CUR_1="$O1"
    CUR_2="$O2"
    CUR_3="$O3"
    CUR_IS_INP=0
    if [ "$INP_2" = "" ]; then
      CUR_2=""
    fi
    if [ "$INP_3" = "" ]; then
      CUR_3=""
    fi
  fi
fi

########################################################################
# CFORM

if [ "$CFORM_NEEDED" = "1" ]; then
  O1="$OUT_1"
  O2="$OUT_2"
  O3="$OUT_3"
  if [ "`expr $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    # there is a follow-on step
    O1="$OUT_1"_$$_`date +%H%M%S`_cf
    O2="$OUT_2"_$$_`date +%H%M%S`_cf
    O3="$OUT_3"_$$_`date +%H%M%S`_cf
  fi

  echo $R2LIB/cform inp=$CUR_1 out=$O1 $NLNS oform=byte orange=\(0,255\) \\
  echo 	irange=\(`v2param rawDataMin` `v2param rawDataMax`\)
  $R2LIB/cform inp=$CUR_1 out=$O1 $NLNS oform=byte orange=\(0,255\) \
	irange=\(`v2param rawDataMin` `v2param rawDataMax`\) >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  if [ "$INP_2" != "" ]; then
    echo $R2LIB/cform inp=$CUR_2 out=$O2 $NLNS oform=byte orange=\(0,255\) \\
    echo	  irange=\(`v2param rawDataMin` `v2param rawDataMax`\)
    $R2LIB/cform inp=$CUR_2 out=$O2 $NLNS oform=byte orange=\(0,255\) \
	irange=\(`v2param rawDataMin` `v2param rawDataMax`\) >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi
  if [ "$INP_3" != "" ]; then
    echo $R2LIB/cform inp=$CUR_3 out=$O3 $NLNS oform=byte orange=\(0,255\) \\
    echo	  irange=\(`v2param rawDataMin` `v2param rawDataMax`\)
    $R2LIB/cform inp=$CUR_3 out=$O3 $NLNS oform=byte orange=\(0,255\) \
	irange=\(`v2param rawDataMin` `v2param rawDataMax`\) >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi

  NLNS=""

  # Remove temp files

  if [ "$CUR_IS_INP" = 0 ]; then
    echo rm $CUR_1 $CUR_2 $CUR_3
    rm $CUR_1 $CUR_2 $CUR_3
  fi

  if [ "`expr $STRETCH_NEEDED + $PSEUDO_NEEDED`" != "0" ]; then
    CUR_1="$O1"
    CUR_2="$O2"
    CUR_3="$O3"
    CUR_IS_INP=0
    if [ "$INP_2" = "" ]; then
      CUR_2=""
    fi
    if [ "$INP_3" = "" ]; then
      CUR_3=""
    fi
  fi
fi

########################################################################
# STRETCH

if [ "$STRETCH_NEEDED" = "1" ]; then
  O1="$OUT_1"
  O2="$OUT_2"
  O3="$OUT_3"
  if [ "`expr $PSEUDO_NEEDED`" != "0" ]; then
    # there is a follow-on step
    O1="$OUT_1"_$$_`date +%H%M%S`_str
    O2="$OUT_2"_$$_`date +%H%M%S`_str
    O3="$OUT_3"_$$_`date +%H%M%S`_str
  fi

  if [ "$CUR_2" = "" ]; then
    O2=""
  fi
  if [ "$CUR_3" = "" ]; then
    O3=""
  fi

  # vlookup can do all 3 at once.  If the files are blank, no harm

  echo $R2LIB/vlookup inp=\($CUR_1 $CUR_2 $CUR_3\) out=\($O1 $O2 $O3\) \\
  echo	$NLNS lutfile=`v2param stretchFile`
  $R2LIB/vlookup inp=\($CUR_1 $CUR_2 $CUR_3\) out=\($O1 $O2 $O3\) \
	$NLNS lutfile=`v2param stretchFile` >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT

  # Remove temp files

  if [ "$CUR_IS_INP" = 0 ]; then
    echo rm $CUR_1 $CUR_2 $CUR_3
    rm $CUR_1 $CUR_2 $CUR_3
  fi

  NLNS=""

  if [ "`expr $PSEUDO_NEEDED`" != "0" ]; then
    CUR_1="$O1"
    CUR_2="$O2"
    CUR_3="$O3"
    CUR_IS_INP=0
  fi
fi

########################################################################
# PSEUDO

if [ "$PSEUDO_NEEDED" = "1" ]; then
  O1="$OUT_1"
  O2="$OUT_2"
  O3="$OUT_3"

  # vlookup can do all 3 at once.  If the input files 2,3 are blank, no harm

  echo $R2LIB/vlookup inp=\($CUR_1 $CUR_2 $CUR_3\) out=\($O1 $O2 $O3\) \\
  echo	$NLNS lutfile=`v2param pseudoFile` column=\(1,2,3\)
  $R2LIB/vlookup inp=\($CUR_1 $CUR_2 $CUR_3\) out=\($O1 $O2 $O3\) \
	$NLNS lutfile=`v2param pseudoFile` column=\(1,2,3\) >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT

  # Remove temp files

  if [ "$CUR_IS_INP" = 0 ]; then
    echo rm $CUR_1 $CUR_2 $CUR_3
    rm $CUR_1 $CUR_2 $CUR_3
  fi

fi

########################################################################
# None of the above.  Just copy out the subsection, or the whole file
# if that's what they really want!

if [ "`expr $FLOT_NEEDED + $FLOT2_NEEDED + $CFORM_NEEDED + $STRETCH_NEEDED + $PSEUDO_NEEDED`" = "0" ]; then

  echo $R2LIB/copy inp=$INP_1 out=$OUT_1 $NLNS
  $R2LIB/copy inp=$INP_1 out=$OUT_1 $NLNS >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  if [ "$INP_2" != "" ]; then
    echo $R2LIB/copy inp=$INP_2 out=$OUT_2 $NLNS
    $R2LIB/copy inp=$INP_2 out=$OUT_2 $NLNS >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi
  if [ "$INP_3" != "" ]; then
    echo $R2LIB/copy inp=$INP_3 out=$OUT_3 $NLNS
    $R2LIB/copy inp=$INP_3 out=$OUT_3 $NLNS >$TMPOUT 2>&1
    if [ "$?" != "1" ]; then
      error=1
    fi
    cat $TMPOUT
  fi
fi

########################################################################
# This is a placeholder for a label-update program, to update e.g. map
# projection labels to handle the new size of the image.  It is assumed
# that there are seven parameters:
# inp, sl, ss, nl, ns, orig_nl, orig_ns
# where inp is modified in-place, sl/ss/nl/ns are the standard VICAR
# size params of the output file (what was used to cut out the piece),
# and orig_nl/orig_ns are the original size of the file.  The parameter
# names are easy to change, of course...

##if [ "`v2param saveImageExtent`" = "display" ]; then
## 
##  # First file
##
##  echo $R2LIB/map_label_program inp=$OUT_1 $NLNS_ORIG \\
##  echo	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth`
##  $R2LIB/map_label_program inp=$OUT_1 $NLNS_ORIG \
##	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth` >$TMPOUT 2>&1
##  if [ "$?" != "1" ]; then
##    error=1
##  fi
##  cat $TMPOUT
##
##  if [ "`v2param imageMode`" = "color" -o "$PSEUDO_NEEDED" = 1 ]; then
##
##    # Second and third files only if needed
##
##    echo $R2LIB/map_label_program inp=$OUT_2 $NLNS_ORIG \\
##    echo	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth`
##    $R2LIB/map_label_program inp=$OUT_2 $NLNS_ORIG \
##	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth` >$TMPOUT 2>&1
##    if [ "$?" != "1" ]; then
##      error=1
##    fi
##    cat $TMPOUT
##
##    echo $R2LIB/map_label_program inp=$OUT_3 $NLNS_ORIG \\
##    echo	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth`
##    $R2LIB/map_label_program inp=$OUT_3 $NLNS_ORIG \
##	orig_nl=`v2param imageHeight` orig_ns=`v2param imageWidth` >$TMPOUT 2>&1
##    if [ "$?" != "1" ]; then
##      error=1
##    fi
##    cat $TMPOUT
##
##  fi
##fi

########################################################################
# Convert the output to TIFF, if desired.  We set up $OUT_1/2/3 to be
# temporary names above, so we can safely delete them after converting to
# the TIFF name.
#
# Parameters to vtiff can be set via the $VTIFF_PARAM environment variable,
# if desired.

if [ "`v2param saveFileFormat`" = "TIFF" ]; then

# This is Very Bad.  vtiff2 runs only under linux, and vtiff does not run
# under linux.  So, pick the right program name based on the OS.
# !!!! THIS SHOULD BE FIXED !!!!
  TIFF_PGM="$R2LIB/vtiff"
  if [ "$VICCPU" = "x86-linux" ] || [ "$VICCPU" = "x86-64-linx" ]; then
    TIFF_PGM=$R2LIB/vtiff2
  fi

# If not set, set it to empty string
  VTIFF_PARAM="${VTIFF_PARAM:-}"

  echo $TIFF_PGM -fromvic inp=\($OUT_1 $OUT_2 $OUT_3\) out=$OUT_TIFF $VTIFF_PARAM
  $TIFF_PGM -fromvic inp=\($OUT_1 $OUT_2 $OUT_3\) out=$OUT_TIFF $VTIFF_PARAM >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  echo rm $OUT_1 $OUT_2 $OUT_3
  rm $OUT_1 $OUT_2 $OUT_3

  echo ""
  echo "Note:  Parameters to VTIFF may be set via the VTIFF_PARAM environment variable."
  echo ""

fi

########################################################################
# Convert the output to PostScript, if we're printing.  We set up $OUT_1/2/3
# to be temporary names above, so we can safely delete them after converting
# to PostScript.
#

if [ "$PRINTING" = 1 ]; then

  TITLE="`v2param printTitle`"
  if [ "$TITLE" = "" ]; then
    TITLE_CMD="-notitle"
  else
    TITLE_CMD="-title title=\"$TITLE\""
  fi

  WIDTH=""
  if [ "`v2param printWidth`" != "" ]; then
    WIDTH="width=`v2param printWidth`"
  fi
  HEIGHT=""
  if [ "`v2param printHeight`" != "" ]; then
    HEIGHT="height=`v2param printHeight`"
  fi

  echo $R2LIB/pscript inp=\($OUT_1 $OUT_2 $OUT_3\) out=$OUT_PRINT $WIDTH $HEIGHT orient=`v2param orientation` $TITLE_CMD
  $R2LIB/pscript inp=\($OUT_1 $OUT_2 $OUT_3\) out=$OUT_PRINT $WIDTH $HEIGHT orient=`v2param orientation` $TITLE_CMD >$TMPOUT 2>&1
  if [ "$?" != "1" ]; then
    error=1
  fi
  cat $TMPOUT
  echo rm $OUT_1 $OUT_2 $OUT_3
  rm $OUT_1 $OUT_2 $OUT_3

# If we're actually printing, send it to the print command

  if [ "`v2param printTo`" != "file" ]; then
    PRINT_CMD="`v2param printCommand`"
    if [ "$PRINT_CMD" = "" ]; then
      PRINT_CMD="lpr"
    fi

    echo $PRINT_CMD $OUT_PRINT
    $PRINT_CMD $OUT_PRINT

# OUT_PRINT is a dummy name set up above so we can delete it if printing

    echo rm $OUT_PRINT
    rm $OUT_PRINT
  fi

fi

########################################################################
# We're done!!

echo "rm $TMPOUT"
rm $TMPOUT

if [ "$error" = "1" ]; then
  echo >&2 "ERROR:  VICAR error, see output above!"
  exit
fi

if [ "$PRINTING" = "1" ]; then
  echo "Print Complete!"
else
  echo "Save Complete!"
fi

exit

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvd_save_script.imake
#define PROCEDURE xvd_save_script

#define MODULE_LIST xvd_save_script.sh

#define USES_SH

#define GUILIB

$ Return
$!#############################################################################
