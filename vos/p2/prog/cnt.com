$!****************************************************************************
$!
$! Build proc for MIPL module cnt
$! VPACK Version 1.9, Monday, April 27, 1998, 17:24:36
$!
$! Execute by entering:		$ @cnt
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
$ write sys$output "*** module cnt ***"
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
$ write sys$output "Invalid argument given to cnt.com file -- ", primary
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
$   if F$SEARCH("cnt.imake") .nes. ""
$   then
$      vimake cnt
$      purge cnt.bld
$   else
$      if F$SEARCH("cnt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cnt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cnt.bld "STD"
$   else
$      @cnt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cnt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cnt.com -
	-s cnt.c -
	-i cnt.imake -
	-p cnt.pdf -
	-t tstcnt.pdf tstcnt.dat tstcnt2.dat
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cnt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>
#include "taeconf.inp"
#include "vicmain_c"
#include "parblk.inc"
#include "pgminc.inc"

/*  Revision History                                                    */
/*    2-87   SP   Added code to handle  NO RECORDS in SRCH file.        */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/
/*    2-96   FFM  Made TOTAL & NXTFIL as optional keywords to obsolete  */
/*           procedure count                                            */

void main44(void)
{

#define MAXR1       80          /* buffer length for first record.      */
#define MAX_LEN     80


    int count,def;		/* COUNT, DEF for XVPARM		*/
    struct PARBLK out_parb;	/* Local parameter block for XQ calls	*/
    int next_file;		/* Next file to be retrieved		*/
    int stat,i;			/* Return status ind, increment var	*/
    int total;			/* Total number of files in list	*/
    int maxlen;			/* dummy parameter to satisfy zvparm	*/
    char filename[40];	        /* Name of ASCII file to be read	*/
    char new_name[44];	        /* filename with unique extension	*/
    char output_name[MAX_LEN];	/* Name of file for output		*/
    char msg[80];		/* Message buffer for zvmessage		*/
    char rec1[MAXR1];           /* buffer for first record in file.     */
    FILE *unit;			/* C I/O unit control block		*/

    zvparm("INPUT",filename,&count,&def,1,0);
    zvfilename(filename,new_name,43);

    unit = fopen(new_name,"r+");	/* Open the file		*/
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }


 /*   Load first record into rec1 buffer.                                */

    for (i=0; (rec1[i] = getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;

    rewind(unit);

    if ( strncmp(rec1,"NO RECORDS",10) == 0 )
    {
       next_file = 0;                   /* case of no records found by SRCH */
       total     = 0;
    }
    else
    {
    stat = fscanf(unit,"NEXT FILE = %5d",&next_file);
    if (stat != 1)
    {
	zvmessage(" Error reading number of next file","");
	zvmessage(" First line of input file must be of the form:","");
	zvmessage(" NEXT FILE = file_number","");
	zabend();
    }

    if (next_file <= 0) next_file = 1;	/* Protect from user error	*/

    for (total = 0; 1; total++)
    {
	stat = fscanf(unit,"%s",output_name);
	if (stat == EOF) break;
	if (stat != 1)
	{
	    zvmessage(" Read error on input","");
	    zabend();
	}
    }
    }
    sprintf(msg," File %d out of %d total",next_file,total);
    zvmessage(msg,"");
	
    q_init(&out_parb,500,P_ABORT);	/* Initialize a local par block	*/

/* output variables TOTAL, and NXTFIL to vblock				*/
    q_intg(&out_parb,"TOTAL",1,&total,P_ADD);
    q_intg(&out_parb,"NXTFIL",1,&next_file,P_ADD);

    zvq_out(&out_parb);		/* Output vblock to TM		*/

    fclose(unit);
}

make_file_process_specific(old_name,new_name)

    char old_name[];		/* User supplied file name		*/
    char new_name[];		/* Output: new process specific name	*/

{
    strcpy(new_name,old_name);

    return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cnt.imake
#define PROGRAM cnt

#define MODULE_LIST cnt.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create cnt.pdf
PROCESS HELP=*
  LOCAL DUMMY TYPE=INTEGER
  LOCAL DMY   TYPE=STRING
  PARM INPUT  TYPE=(STRING,40)
  PARM TOTAL  TYPE=NAME DEFAULT=DUMMY
  PARM NXTFIL TYPE=NAME DEFAULT=DMY

!# annot function="VICAR Procedure Generation"
!# annot keywords-(CNT,proc,SRCH,TCL,COUNT,"ASCII file",TOTAL,INPUT,NXTFIL,+
!#  NXT,"Output parameter","TAE variable")

END-PROC
.title
Returns number of files in a list created by SRCH
.HELP
CNT returns the total number of files in a list created by the proc
SRCH, and also the number of the next file in that list.  It does
not modify the file in any way.

CNT is intended to be used in a proc to provide the calling proc with
information about the file.  For interactive use, use the TCL procedure
COUNT.
.level1
.vari input
Input -- File name
.vari total
Output -- total number of
names in list.
.vari nxtfil
Output -- Number of next
file in list.
.level2
.vari input
The input to CNT is a file which consists of a list of file names
(or perhaps other strings) created by the proc SRCH.  It is an ASCII
file which may be edited using a text editor if desired.
.vari total
Upon completion of CNT, TOTAL contains the total number of members 
contained in the INPUT list.
.vari nxtfil
Upon completion of CNT, the parameter given for NXTFIL contains the
number of the next file to be referenced in the INPUT list, but
does not modify the input in any way (see program NXT).
.end


$ Return
$!#############################################################################
$Test_File:
$ create tstcnt.pdf
procedure
refgbl $echo
local total type=int
local next type=int
body
let _onfail="continue"
let $echo="yes"
cnt tstcnt.dat total next
disp total
disp next
! TEST CASE OF NO RECORDS
cnt tstcnt2.dat total next
disp total
disp next
end-proc
$!-----------------------------------------------------------------------------
$ create tstcnt.dat
NEXT FILE =     2
A.DAT
B.DAT
C.DAT
A.BCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789
$!-----------------------------------------------------------------------------
$ create tstcnt2.dat
NO RECORDS FOUND
$ Return
$!#############################################################################
