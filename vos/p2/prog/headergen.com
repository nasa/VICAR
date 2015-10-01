$!****************************************************************************
$!
$! Build proc for MIPL module headergen
$! VPACK Version 1.8, Wednesday, July 16, 1997, 12:37:36
$!
$! Execute by entering:		$ @headergen
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module headergen ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to headergen.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("headergen.imake") .nes. ""
$   then
$      vimake headergen
$      purge headergen.bld
$   else
$      if F$SEARCH("headergen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake headergen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @headergen.bld "STD"
$   else
$      @headergen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create headergen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack headergen.com -
	-s headergen.c -
	-i headergen.imake -
	-p headergen.pdf -
	-t tstheadergen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create headergen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Headergen takes an ascii file of records and combines them into a
   single record in outfile. */
/* Revision History */
/* 7-97 RRD Combined modules headergen and headers and ported to UNIX */

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    250              /* buffer length for file name      */
#define MAXR2    2*MAXR1          /* buffer length for full file name */   

   int   count;                   /* used by zvp to get parameters    */
   int   len, status;
   FILE  *infile, *outfile;       /* pointers to input/output files   */
   char  inbuf[132];              /* buffer for text from infile      */
   char  outbuf[32767];           /* buffer for text to outfile       */
   char  iname[MAXR1], oname[MAXR1];         /* file names            */
   char full_in[MAXR2+1];         /* full file path name for infile   */
/*  ================================================================= */

/* Retrieve the user specified filenames */

   zvp("INFILE", iname, &count);
   zvfilename(iname,full_in,MAXR2);
   zvp("OUTFILE", oname, &count);


/* Open files */

   infile = fopen(full_in, "r");
   if (infile == NULL) {
       sprintf(outbuf, "ERROR OPENING text FILE '%s'", iname);
       zmabend(outbuf);
   }
   outfile = fopen(oname, "w");

/* Read infile into outbuf */

   outbuf[0] = '\0';
   while (fgets(inbuf, sizeof(inbuf), infile) != NULL) {
      len = strlen(inbuf);
      if ( len>0 && inbuf[len-1] == '\n')
         inbuf[len-1]  =  '\0';
      strcat(outbuf,inbuf);
   }

/* Write outbuf to outfile */

   fprintf(outfile,"%s",outbuf);

   status= fclose(infile);
   status= fclose(outfile);
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create headergen.imake
#define PROGRAM headergen

#define MODULE_LIST headergen.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create headergen.pdf
PROCESS help=*
	parm INFILE	type=string
	parm OUTFILE	type=string

!# annot function="VICAR utilities"
!# annot keywords=(header)
END-PROC
.title
Combines multiple records of an ASCII file into a single record
.help
PURPOSE
	Headergen is a VICAR program that takes an ascii file of records and 
	combines them into one record in outfile.
EXECUTION
	headergen INFILE OUTFILE
REVISION HISTORY
	7-97 RRD Combined modules headergen and headers, rewrote in C,
		 added pdf file and tstheadergen, and ported to UNIX 
END-PROC
$ Return
$!#############################################################################
$Test_File:
$ create tstheadergen.pdf
procedure
!
!
refgbl $echo
body
let _onfail="continue"
let $echo=("yes","no","no") ! echo only top level.
!
createfile add1.dat
addtofile  add1.dat "MISSION_NAME='CASSINI'     "     
addtofile  add1.dat "MISSION_PHASE_TYPE='BENCH'     "
addtofile  add1.dat "INSTRUMENT_ID='ISSNA'     "
addtofile  add1.dat "IMAGE_NUMBER=763839162,     "
addtofile  add1.dat "IMAGE_TIME='1994-075T09:32:42.000'     "
addtofile  add1.dat "SOFTWARE_VERSION_ID='IMAGE_PROC'     "
addtofile  add1.dat "INSTRUMENT_MODE_ID='SUM2'     "
addtofile  add1.dat "FILTER1_NAME='CL1'     "
addtofile  add1.dat "FILTER2_NAME='CL2'     "
addtofile  add1.dat "EXPOSURE_DURATION=1000.0     "
addtofile  add1.dat "GAIN_MODE_ID='24K'     "
addtofile  add1.dat "ENCODING_TYPE='NOTCOMP'     "
addtofile  add1.dat "CONVERSION_TYPE='12BIT'     "
addtofile  add1.dat "DETECTOR_TEMPERATURE=-238.00     "
addtofile  add1.dat "OPTICS_TEMPERATURE=-230.00     "
addtofile  add1.dat "FILTER_TEMPERATURE=-999.00     "
addtofile  add1.dat "LIGHT_FLOOD_STATE_FLAG='OFF'     "
addtofile  add1.dat "ANTIBLOOMING_STATE_FLAG='OFF'     "
addtofile  add1.dat "CALIB_LAMP_STATE_FLAG='OFF'     "
addtofile  add1.dat "OFFSET=380     "
addtofile  add1.dat "DARK_CURRENT=381     "
addtofile  add1.dat "COMPRESSION_RATIO=-999.000     "
addtofile  add1.dat "TARGET_NAME='ISS_LAB'     "
addtofile  add1.dat "OBSERVATION_ID='ISS_TEST'     "
addtofile  add1.dat "ILLUMINANT='XENON'     "
addtofile  add1.dat "MISSING_LINES=10     "
addtofile  add1.dat "GROUP_BLOCKS=128     "
addtofile  add1.dat "ALGORITHM=0     "
addtofile  add1.dat "BLOCK_TYPE=1     "
addtofile  add1.dat "RADIANCE=12.345     "
addtofile  add1.dat "QUANTIZATION_FACTOR_INDEX=8     "
typetext add1.dat
headergen add1.dat add2.dat
typetext add2.dat 
!
end-proc
$ Return
$!#############################################################################
