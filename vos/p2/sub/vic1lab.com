$!****************************************************************************
$!
$! Build proc for MIPL module vic1lab
$! VPACK Version 1.9, Monday, December 07, 2009, 16:41:29
$!
$! Execute by entering:		$ @vic1lab
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
$ write sys$output "*** module vic1lab ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vic1lab.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("vic1lab.imake") .nes. ""
$   then
$      vimake vic1lab
$      purge vic1lab.bld
$   else
$      if F$SEARCH("vic1lab.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vic1lab
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vic1lab.bld "STD"
$   else
$      @vic1lab.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vic1lab.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vic1lab.com -mixed -
	-s vic1lab.c -
	-i vic1lab.imake -
	-t tstvic1lab.imake tstvic1lab.pdf tvic1lab.f tvic1lab.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vic1lab.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include <string.h>
#include <stdio.h>

/* Returns old Vicar 1 72-byte labels in a buffer.  The buffer is a	*/
/* single string with each item 72 bytes long.  There is no separator	*/
/* between the labels.							*/
/* NOTE:  For vic1lab, the buffer is a Fortran character*n variable.	*/
/* For vic1labx, the buffer is not.					*/
/* For zvic1lab, the buffer may not be null terminated.			*/

/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

void FTN_NAME2(vic1lab, VIC1LAB) (int *unit, int *status, int *nlabs, char *buf,
						int *maxlabs, ZFORSTR_PARAM)
#if 0
int *unit;			/* In: unit number */
int *status;			/* Out: status value */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: character*n buffer to contain labels */
int *maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for string size max) */
#endif
{
   ZFORSTR_BLOCK
   int len, max;

   max = *maxlabs;
   zsfor2len(len, buf, &unit, 5, 4, 1, maxlabs);
   if (*maxlabs == 0 || len < *maxlabs*72)
      max = len/72;

   memset(zsfor2ptr(buf), ' ', len);
   *status = zvic1lab(*unit, nlabs, zsfor2ptr(buf), max);
   return;
}

void FTN_NAME2(vic1labx, VIC1LABX) (unit, status, nlabs, buf, maxlabs)
int *unit;			/* In: unit number */
int *status;			/* Out: status value */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: byte buffer to contain labels */
int *maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for no max) */
{
   *status = zvic1lab(*unit, nlabs, buf, *maxlabs);
   return;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvic1lab(unit, nlabs, buf, maxlabs)
int unit;			/* In: unit number */
int *nlabs;			/* Out: number of 72-byte labels returned */
char *buf;			/* Out: buffer to contain labels */
int maxlabs;			/* In: max # of 72-byte labels to return */
				/*     (0 for no max) */
{
   int status, i;
   char task[16];
   int instance, ninst;
   char key[10];
   char label[256];

/* Get the name of the first task */

   ninst = 1;
   status = zlhinfo(unit, task, &instance, &ninst, NULL);
   if (status != SUCCESS)
      return status;

/* Get the number of Vicar1 labels present */

   status = zlget(unit, "HISTORY", "NLABS", nlabs, "HIST", task,
			"INSTANCE", 1, NULL);
   if (status != SUCCESS) {
      *nlabs = 0;	/* Assume if error that no Vicar1 labels are present */
      return SUCCESS;
   }

   if (maxlabs != 0 && maxlabs < *nlabs)	/* check for max # of labels */
      *nlabs = maxlabs;

/* Now get NLABS 72 byte labels */

   for (i = 0; i < *nlabs; i++) {
      sprintf(key, "LAB%02d", i+1);
      status = zlget(unit, "HISTORY", key, label, "HIST", task,
				"INSTANCE", 1, NULL);
      strncpy(buf+i*72, label, 72);
      if (status != SUCCESS) {
         *nlabs = i;
         return status;
      }
   }

   return SUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vic1lab.imake
/* Imake file for VICAR subroutine VIC1LAB */

#define SUBROUTINE vic1lab

#define MODULE_LIST vic1lab.c

#define P2_SUBLIB

#define USES_ANSI_C
#define FTN_STRING

$ Return
$!#############################################################################
$Test_File:
$ create tstvic1lab.imake
/* Imake file for Test of VICAR subroutine VIC1LAB */

#define PROGRAM tvic1lab

#define MODULE_LIST tvic1lab.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL

$!-----------------------------------------------------------------------------
$ create tstvic1lab.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
TVIC1LAB MIPLDISK:[MIPL.VGR]F1636832.RAW
end-proc

$!-----------------------------------------------------------------------------
$ create tvic1lab.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      BYTE BUF(1440)
      CHARACTER*1440 CBUF
      CHARACTER*72 LINE
      INTEGER UNIT,STATUS,NLABS,I

      CALL XVUNIT(UNIT,'INP',1,STATUS, ' ')
      CALL XVOPEN(UNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA', ' ')

      CALL XVMESSAGE('Labels from input by BYTE buffer:', ' ')
      CALL VIC1LABX(UNIT,STATUS,NLABS,BUF,20)
      DO I = 1,NLABS
         CALL MVLC(BUF((I-1)*72+1), LINE, 72)
         CALL XVMESSAGE(LINE, ' ')
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('Labels from input by CHAR*n buffer:', ' ')
      CALL VIC1LAB(UNIT,STATUS,NLABS,CBUF,20)
      DO I = 1,NLABS
         CALL XVMESSAGE(CBUF((I-1)*72+1:I*72), ' ')
      ENDDO

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tvic1lab.pdf
PROCESS
  PARM INP TYPE=STRING
END-PROC

$ Return
$!#############################################################################
