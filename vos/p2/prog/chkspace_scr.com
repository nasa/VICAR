$!****************************************************************************
$!
$! Build proc for MIPL module chkspace_scr
$! VPACK Version 1.9, Thursday, February 17, 2011, 15:54:40
$!
$! Execute by entering:		$ @chkspace_scr
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
$ write sys$output "*** module chkspace_scr ***"
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
$ write sys$output "Invalid argument given to chkspace_scr.com file -- ", primary
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
$   if F$SEARCH("chkspace_scr.imake") .nes. ""
$   then
$      vimake chkspace_scr
$      purge chkspace_scr.bld
$   else
$      if F$SEARCH("chkspace_scr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake chkspace_scr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @chkspace_scr.bld "STD"
$   else
$      @chkspace_scr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chkspace_scr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chkspace_scr.com -mixed -
	-s chkspace_scr.sh -
	-i chkspace_scr.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create chkspace_scr.sh
$ DECK/DOLLARS="$ VOKAGLEVE"
:
DISK_NAME=$1
TEMP_FILE_NAME=$2
#echo "1= "$1"  2= " $2 "  0= " $0 "  arg" $# 

#check if standard error file exists and remove if so
rm -f errors*

#check number of parameters input - UNIX file name expansion adds parameters
if [ $# != 2 ]
  then echo "CHKSPACE: DISK name not valid" > errors
  exit 0
fi

#use "df" command to check disk space and pipe to nawk 
#if disk name was not a disk output error message to standard error file
#nawk will get the available disk space and put it in temp file
df -k "$DISK_NAME"  2>errors | awk ' {  
  if (NR == 2 && $4 != "") print $4 ; #if sys name is long avail on diff line 
  if (NR > 2 && $3 != "") print $3 ;}' > $TEMP_FILE_NAME

if [ -s errors ]
  then a=1
  else rm errors
fi
exit 0

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create chkspace_scr.imake
/***********************************************************************

                     IMAKE FILE FOR PROCEDURE chkspace_scr

   To Create the build file give the command:

	$ vimake chkspace                     (VMS)
   or
	% vimake chkspace                     (Unix)


*************************************************************************/

#define PROCEDURE chkspace_scr
#define MODULE_LIST chkspace_scr.sh

#define USES_SH
$ Return
$!#############################################################################
