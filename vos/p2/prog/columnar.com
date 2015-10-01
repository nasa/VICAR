$!****************************************************************************
$!
$! Build proc for MIPL module columnar
$! VPACK Version 1.8, Monday, July 21, 1997, 07:41:15
$!
$! Execute by entering:		$ @columnar
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
$ write sys$output "*** module columnar ***"
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
$ write sys$output "Invalid argument given to columnar.com file -- ", primary
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
$   if F$SEARCH("columnar.imake") .nes. ""
$   then
$      vimake columnar
$      purge columnar.bld
$   else
$      if F$SEARCH("columnar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake columnar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @columnar.bld "STD"
$   else
$      @columnar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create columnar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack columnar.com -
	-s columnar.c -
	-i columnar.imake -
	-p columnar.pdf -
	-t tstcolumnar.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create columnar.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Columnar takes in 2 ascii files of records and concatenates respective
   records side by side separated by 1 space.  If SKIP1 is set then one
   column (through 1st tab) of data is skipped from 2nd file. */
/* Revision History */
/* 7-97 RRD Combined modules columnar, columns, and columns2 */

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    255              /* buffer length for file name      */
#define MAXR2    2*MAXR1          /* buffer length for full file name */
#define BLEN     32767            /* buffer length for text           */
#define NUMFILES 2                /* number of input files            */

   int   count, def;              /* used to get parameters           */
   char  *buf_ptr, *buf_ptr2;     /* text pointers                    */
   FILE  *outfile;                /* out file                         */
   FILE  *infiles[NUMFILES];      /* array of infiles                 */
   char  inbufs[NUMFILES][BLEN];  /* array of text buffers            */
   char  outbuf[NUMFILES*BLEN];   /* output text buffer               */
   char  inames[NUMFILES][MAXR1]; /* array of input files             */
   char  oname[MAXR1];            /* output file name                 */
   char  full_out[MAXR2+1];       /* full file path name              */
   char  full_in[NUMFILES][MAXR2+1]; /* full file path names          */
   int   i, status, exit, zvpt;
/*  ================================================================= */

/* Retrieve and open user specified filenames */

   zvparm("IN", inames, &count, &def, NUMFILES, MAXR1);
   for (i=0;i<NUMFILES;++i) {
     zvfilename(inames[i],full_in[i],MAXR2);     
     infiles[i] = fopen(full_in[i], "r");
     if (infiles[i] == NULL)
       file_error(inames[i], "OPENING");
   }

   zvp("OUT", oname, &count);
   zvfilename(oname,full_out,MAXR2);
   outfile = fopen(full_out, "w");
   if (outfile == NULL)
     file_error(oname, "OPENING");

/* Read infiles into buffers and write to outfile */

   exit = 0;
   zvpt = zvptst("SKIP1");
   while (1) {
     outbuf[0]='\0';
     for (i=0;i<NUMFILES;++i) {
       if (fgets(inbufs[i], sizeof(inbufs[i]), infiles[i]) == NULL) {
	 exit = 1;
	 break;
       }
       buf_ptr = inbufs[i];
       if (zvpt && i != 0) {
	 buf_ptr2 = strchr(inbufs[i], '\t');
         if (buf_ptr2 != NULL)
	   buf_ptr = buf_ptr2;
	   ++buf_ptr;
       }
       if (i != NUMFILES-1) {
	 buf_ptr2 = strchr(inbufs[i], '\n');
	 if (buf_ptr2 != NULL)
	   *buf_ptr2 = ' ';
	 else {
	   buf_ptr2 = strchr(inbufs[i], '\0');
	   *buf_ptr2 = ' ';
	   ++buf_ptr2;
	   *buf_ptr2 = '\0';
	 }
       }
       strcat(outbuf, buf_ptr);
     }
     if (exit) break;
     status = fprintf(outfile,"%s",outbuf);
     if (status==0)
       file_error(oname, "WRITING");
   }


   for (i=0;i<NUMFILES;++i) {
     status= fclose(infiles[i]);
     if (status!=0) 
       file_error(inames[i], "CLOSING");
   }
   status= fclose(outfile);
   if (status!=0)
     file_error(oname, "CLOSING");
   return;
}


/* handles file errors */
file_error(fname, msg)
char fname[], msg[];
{
  char buf[MAXR1+25];

  sprintf(buf, "ERROR %s TEXT FILE '%s'", msg, fname);
  zmabend(buf);
  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create columnar.imake
#define PROGRAM columnar

#define MODULE_LIST columnar.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create columnar.pdf
PROCESS help=*
	parm IN		string			count=2
	parm OUT	string
	parm SKIP1	keyword	valid=SKIP1	count=0:1	default=--
END-PROC
.TITLE
VICAR program columnar
.HELP
PURPOSE
	Columnar takes in 2 ascii files of records and concatenates respective
	records side by side separated by 1 space.  If SKIP1 is set then one
	column of data (through 1st tab) is skipped from 2nd file.
EXECUTION
	columnar IN=(infile1, infile2) OUT=outfile [SKIP1=SKIP1]
REVISION HISTORY
	7-97 RRD Combined modules columnar, columns, and columns2, rewrote
                 in C, added pdf file and tstcolumnar, and ported to UNIX 
.LEVEL1
.VARIABLE IN
Two input file names.
.VARIABLE OUT
Output file name.
.VARIABLE SKIP1
Keyword to skip first
column of 2nd file.
.LEVEL2
.VARIABLE IN
STRING
Two input file names.
.VARIABLE OUT
STRING
Output file name.
.VARIABLE SKIP1
KEYWORD
If set then the first column of the 2nd file is skipped.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcolumnar.pdf
procedure
!
!
refgbl $echo
body
let _onfail="continue"
let $echo=("yes","no","no") ! echo only top level.
!
createfile add1.dat
addtofile  add1.dat "MISSION_NAME=	'CASSINI'"     
addtofile  add1.dat "MISSION_PHASE_TYPE=	'BENCH'"
addtofile  add1.dat "INSTRUMENT_ID=	'ISSNA'"
addtofile  add1.dat "IMAGE_NUMBER=	763839162,"
addtofile  add1.dat "IMAGE_TIME=	'1994-075T09:32:42.000'"
addtofile  add1.dat "SOFTWARE_VERSION_ID=	'IMAGE_PROC'"
addtofile  add1.dat "INSTRUMENT_MODE_ID=	'SUM2'"
addtofile  add1.dat "FILTER1_NAME=	'CL1'"
addtofile  add1.dat "FILTER2_NAME=	'CL2'"
addtofile  add1.dat "EXPOSURE_DURATION=	1000.0"
addtofile  add1.dat "GAIN_MODE_ID=	'24K'"
addtofile  add1.dat "ENCODING_TYPE=	'NOTCOMP'"
addtofile  add1.dat "CONVERSION_TYPE=	'12BIT'"
addtofile  add1.dat "DETECTOR_TEMPERATURE=	-238.00"
addtofile  add1.dat "OPTICS_TEMPERATURE=	-230.00"
addtofile  add1.dat "FILTER_TEMPERATURE=	-999.00"
addtofile  add1.dat "LIGHT_FLOOD_STATE_FLAG=	'OFF'"
addtofile  add1.dat "ANTIBLOOMING_STATE_FLAG=	'OFF'"
addtofile  add1.dat "CALIB_LAMP_STATE_FLAG=	'OFF'"
addtofile  add1.dat "OFFSET=	380"
addtofile  add1.dat "DARK_CURRENT=	381"
addtofile  add1.dat "COMPRESSION_RATIO=	-999.000"
addtofile  add1.dat "TARGET_NAME=	'ISS_LAB'"
addtofile  add1.dat "OBSERVATION_ID=	'ISS_TEST'"
addtofile  add1.dat "ILLUMINANT=	'XENON'"
addtofile  add1.dat "MISSING_LINES=	10"
addtofile  add1.dat "GROUP_BLOCKS=	128"
addtofile  add1.dat "ALGORITHM=	0"
addtofile  add1.dat "BLOCK_TYPE=	1"
addtofile  add1.dat "RADIANCE=	12.345"
addtofile  add1.dat "QUANTIZATION_FACTOR_INDEX=	8"
typetext add1.dat
columnar in=(add1.dat, add1.dat) out=add2.dat skip1=skip1
typetext add2.dat 
!
end-proc









$ Return
$!#############################################################################
