$!****************************************************************************
$!
$! Build proc for MIPL module aggrg
$! VPACK Version 1.9, Friday, December 05, 2014, 13:28:31
$!
$! Execute by entering:		$ @aggrg
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
$ write sys$output "*** module aggrg ***"
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
$ write sys$output "Invalid argument given to aggrg.com file -- ", primary
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
$   if F$SEARCH("aggrg.imake") .nes. ""
$   then
$      vimake aggrg
$      purge aggrg.bld
$   else
$      if F$SEARCH("aggrg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aggrg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aggrg.bld "STD"
$   else
$      @aggrg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aggrg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aggrg.com -mixed -
	-s aggrg.c -
	-i aggrg.imake -
	-p aggrg.pdf -
	-t tstaggrg.pdf tstaggrg.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create aggrg.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
/*#include "cartoVicarProtos.h"*/

/************************************************************************/
/* program aggrg                                                        */
/************************************************************************/
/* 00-06 ...alz... initial version                                      */
/* see pdf for history continuagion                                     */
/************************************************************************/

void main44(void)
{
   int i,j,sumcol[40],tocol[40],sumcount,tocount,dummy;
   int unit,ibis,status,clen,agcol,indexcol,indx,icol,outptr;
   int *ixdat,agcolisnum,agcolwid=0,rst,il,iu,outcol;
   short int *conbuf;
   double sum,*agdat,*iodat;
   char *p,cformat[7],*agdatstr;
           
   zifmessage("aggrg version Fri Dec  5 2014");
   
   /* get the basic parameters */
   
   status = zvp("agcol",&agcol,&dummy);
   status = zvp("indexcol",&indexcol,&dummy);
   zvparm("sumcol",sumcol,&sumcount,&dummy,40,0);
   zvparm("tocol",tocol,&tocount,&dummy,40,0);
   if (tocol[0]==0) tocount = 0;
   
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&conbuf,clen,2);
   if (indexcol>0) mz_alloc1((unsigned char **)&ixdat,clen,4);
   
   /* control column can be numeric or alpha */
   
   status = IBISColumnGet(ibis,"FORMAT",cformat,agcol);
   if (status!=1) IBISSignal(ibis,status,1);
   if (cformat[0]=='A')
      {
      agcolisnum = 0;
      agcolwid = ms_num(&cformat[1])+1;
      mz_alloc1((unsigned char **)&agdatstr,clen,agcolwid);
      status = IBISColumnRead(ibis,agdatstr,agcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   else
      {
      agcolisnum = 1;
      mz_alloc1((unsigned char **)&agdat,clen,8);
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",agcol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*) agdat,agcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
      
   /* calculate the control column */
   
   for (i=0;i<clen;i++) conbuf[i] = 0;
   if (agcolisnum)
      {
      rst = agdat[0];
      for (i=0;i<clen;i++)
         {
         if (rst!=agdat[i])
	    {
	    rst = agdat[i];
	    conbuf[i-1] = 1;
	    }
         }
      conbuf[clen-1] = 1;
      free(agdat);
      }
   else
      {
      p = &agdatstr[0];
      for (i=0;i<clen;i++)
         {
         if (strcmp(p,&agdatstr[i*agcolwid])!=0)
	    {
	    p = &agdatstr[i*agcolwid];
	    conbuf[i-1] = 1;
	    }
         }
      conbuf[clen-1] = 1;
      free(agdatstr);
      }
   mz_alloc1((unsigned char **)&iodat,clen,8);
   
   /* Read, sum, and output columns to the ibis interface file */
   
   for (indx=0;indx<sumcount;indx++)
      {
      icol = sumcol[indx];
      status = IBISColumnGet(ibis,"FORMAT",cformat,icol);
      if (status!=1) IBISSignal(ibis,status,1);
      if (cformat[0]=='A') zmabend("Can't sum an alpha column");
      
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",icol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*) iodat,icol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
   
      /* sum the column*/
      
      iu = -1;
      for (outptr=0;outptr<clen;outptr++)
         {
         il = iu+1;
         sum = 0.0;
         for (i=il;i<clen;i++)
            {
            sum += iodat[i];
            if (conbuf[i])
               {
               iu = i;
               for (j=il;j<=iu;j++) iodat[j] = sum;
               if (indx==0&&indexcol>0)
                  for (j=il;j<=iu;j++) ixdat[j] = outptr+1;
               break;
               }
            }
         }
      if (indx<tocount)
         {
         outcol = tocol[indx];
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",outcol);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else outcol = icol;
      status = IBISColumnWrite(ibis,(char*) iodat,outcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   if (indexcol>0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","FULL",indexcol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*) ixdat,indexcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
  
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create aggrg.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM aggrg

   To Create the build file give the command:

		$ vimake aggrg			(VMS)
   or
		% vimake aggrg			(Unix)


************************************************************************/


#define PROGRAM	aggrg
#define R2LIB

#define MODULE_LIST aggrg.c

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create aggrg.pdf
PROCESS		HELP=*
 PARM INP      TYPE=(STRING)
 PARM AGCOL    TYPE=INTEGER DEFAULT=1
 PARM SUMCOL   TYPE=INTEGER COUNT=(1:40)
 PARM TOCOL    TYPE=INTEGER COUNT=(0:40) DEFAULT=0
 PARM INDEXCOL TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program "aggrg"
.HELP
PURPOSE

    "aggrg" aggregates columns in an IBIS tabular file using any designated 
column in the file as a control column.  A column is aggregated by adding 
together items which have the same number contiguously in the control column.
The columns of aggregated data either replaces the original column or is 
placed in a columns specified by the user.  Optionally an index column may
be specified to receive the row index for each group.  The input file is 
modified instead of having an output file; the file retains its original 
length (unlike "aggrg2").

A simple example of this process is shown using four columns:

.PAGE
CONTROL COLUMN	 AGGREGATION COLUMN   RESULTS COLUMN   INDEX COLUMN
	1	   10	(10+10)		20		1
	1	   10	(10+10)		20		2
	2	   11	(11+13+14)	38		1
	2	   13	(11+13+14)	38		2
	2	   14	(11+13+14)	38		3
	3	   17	(17)		17		1
	9	   10	(10+11)		21		1
	9	   11	(10+11)		21		2
	8	   11	(11+12)		23		1
	8	   12	(11+12)		23		2
	1	   15	(15)		15		1

.PAGE
TAE COMMAND LINE FORMAT

	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z) TOCOL=(A,B,C)
	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z)
	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z) TOCOL=(A,B,C) INDEXCOL=K

	FILE is the IBIS interface file, N is the control column, 
X,Y,and Z are the columns to be aggregated, A,B, and C are the columns where 
the results are stored, and K is the column where the index numbers are stored.

EXAMPLE

	aggrg INP=A AGCOL=1 SUMCOL=(5,6,7) TOCOL=(8,9,10)

	This execution indexes on column 1, summing columns 5, 6 and 7; and
replacing columns 8, 9, and 10 with the output.


RESTRICTIONS

    There may be at most 40 columns specified in SUMCOL.
    The maximum column length is 250,000.


.PAGE
WRITTEN BY		A. L. Zobrist		15 Dec 1976

COGNIZANT PROGRAMMER	B. McGuffie

REVISIONS:

    AS (CRI)         Jan 1995 - Made portable for UNIX
    A. L. Zobrist 08 Jun 2000 - Converted to C  
    W. Bunch      29 Dec 2007 - switched to USES_ANSI_C AND LIB_CARTO; misc cleanup
    W. Bunch      05 Dec 2014 - fiddled includes to build in mipl
	
.LEVEL1
.VARIABLE INP
Standard IBIS interface file
.VARIABLE AGCOL
Indexing column; usually numeric
but can be alphabetic
.VARIABLE SUMCOL
Columns to aggregate(S1,S2,..Sk)
.VARIABLE TOCOL
Columns for output  (T1,T2,..Tk)
Default output columns equal the
input aggregate columns.
.VARIABLE INDEXCOL
Optional column to hold the
index numbers for each group.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaggrg.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

! Aug 28, 2013 - RJB
! TEST SCRIPT FOR AGGRG 
! Vicar Programs:
!   translog ibis-gen ibis-list acopin
!
! Parameters:
!   <none>
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="stop"
!check to see if mipl or cartlab for test images
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
!MIPL
    ush ln -s /project/test_work/testdata/carto ct
else
!CARTLAB
    if (aftestdata = "")
        ush ln -s /raid1/vicar_test_images/testdata/carto ct
    else 
        ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/carto ct
    end-if
end-if
let _onfail="goto rm"


let $echo="yes"

! basic case with tocol

ibis-gen xxxagg1 nc=5 nr=5 datacol=(1,2,3,4,5) +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
ibis-list xxxagg1
aggrg xxxagg1 AGCOL=3 SUMCOL=(4,5) TOCOL=(1,2)
ibis-list xxxagg1

! basic case with indexcol

ibis-gen xxxagg2 nc=5 nr=5 datacol=(1,2,3,4,5) +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
ibis-list xxxagg2
aggrg xxxagg2 AGCOL=3 SUMCOL=5 TOCOL=2 INDEXCOL=1
ibis-list xxxagg2

! basic case without tocol

ibis-gen xxxagg3 nc=5 nr=5 datacol=(1,2,3,4,5) +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
ibis-list xxxagg3
aggrg xxxagg3 AGCOL=3 SUMCOL=(4,5)
ibis-list xxxagg3

! test alphabetic control column, also test double precision

ibis-gen xxxagg4 nc=2 nr=1 format=("FULL","FULL","A5","FULL","DOUB")
acopin (ct/aggrg.dat,xxxagg4) cols=(1,2,3,4,5) lead_rm=2
ibis-list xxxagg4 cfor="%5d %5d %6s %5d %18.12f"
aggrg xxxagg4 AGCOL=3 SUMCOL=(4,5)
ibis-list xxxagg4 cfor="%5d %5d %6s %5d %18.12f"

let $echo="no"
rm>
ush rm ct
end-proc
$!-----------------------------------------------------------------------------
$ create tstaggrg.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

ibis-gen xxxagg1 nc=5 nr=5 datacol=(1,2,3,4,5)  +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
Beginning VICAR task ibis
ibis-list xxxagg1
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        9.00        8.00        7.00        6.00        5.00
        9.00        8.00        7.00        6.00        5.00
aggrg xxxagg1 AGCOL=3 SUMCOL=(4,5) TOCOL=(1,2)
Beginning VICAR task aggrg
aggrg version Fri Dec  5 2014
ibis-list xxxagg1
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       12.00       15.00        3.00        4.00        5.00
       12.00       15.00        3.00        4.00        5.00
       12.00       15.00        3.00        4.00        5.00
       12.00       10.00        7.00        6.00        5.00
       12.00       10.00        7.00        6.00        5.00
ibis-gen xxxagg2 nc=5 nr=5 datacol=(1,2,3,4,5)  +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
Beginning VICAR task ibis
ibis-list xxxagg2
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        9.00        8.00        7.00        6.00        5.00
        9.00        8.00        7.00        6.00        5.00
aggrg xxxagg2 AGCOL=3 SUMCOL=5 TOCOL=2 INDEXCOL=1
Beginning VICAR task aggrg
aggrg version Fri Dec  5 2014
ibis-list xxxagg2
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       15.00        3.00        4.00        5.00
        1.00       15.00        3.00        4.00        5.00
        1.00       15.00        3.00        4.00        5.00
        2.00       10.00        7.00        6.00        5.00
        2.00       10.00        7.00        6.00        5.00
ibis-gen xxxagg3 nc=5 nr=5 datacol=(1,2,3,4,5)  +
   data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
Beginning VICAR task ibis
ibis-list xxxagg3
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        1.00        2.00        3.00        4.00        5.00
        9.00        8.00        7.00        6.00        5.00
        9.00        8.00        7.00        6.00        5.00
aggrg xxxagg3 AGCOL=3 SUMCOL=(4,5)
Beginning VICAR task aggrg
aggrg version Fri Dec  5 2014
ibis-list xxxagg3
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00       12.00       15.00
        1.00        2.00        3.00       12.00       15.00
        1.00        2.00        3.00       12.00       15.00
        9.00        8.00        7.00       12.00       10.00
        9.00        8.00        7.00       12.00       10.00
ibis-gen xxxagg4 nc=2 nr=1 format=("FULL","FULL","A5","FULL","DOUB")
Beginning VICAR task ibis
acopin (ct/aggrg.dat,xxxagg4) cols=(1,2,3,4,5) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 5
    Col 3 is ASCII width = 6
Output length is 5 records


ibis-list xxxagg4 cfor="%5d %5d %6s %5d %18.12f"
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+------+-----+-----------------
   C:1   C:2    C:3   C:4               C:5
+-----+-----+------+-----+-----------------
    1     2  aaaaa    12    15.123456789010
    1     2  aaaaa    12    15.000000000002
    1     2  aaaaa    12    15.000000000000
    9     8  bbbbb    12    10.000000000000
    9     8  bbbbb    12    10.000000000000
aggrg xxxagg4 AGCOL=3 SUMCOL=(4,5)
Beginning VICAR task aggrg
aggrg version Fri Dec  5 2014
ibis-list xxxagg4 cfor="%5d %5d %6s %5d %18.12f"
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+------+-----+-----------------
   C:1   C:2    C:3   C:4               C:5
+-----+-----+------+-----+-----------------
    1     2  aaaaa    36    45.123456789012
    1     2  aaaaa    36    45.123456789012
    1     2  aaaaa    36    45.123456789012
    9     8  bbbbb    24    20.000000000000
    9     8  bbbbb    24    20.000000000000
let $echo="no"
$ Return
$!#############################################################################
