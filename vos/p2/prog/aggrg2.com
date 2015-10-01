$!****************************************************************************
$!
$! Build proc for MIPL module aggrg2
$! VPACK Version 1.9, Wednesday, December 17, 2014, 13:02:47
$!
$! Execute by entering:		$ @aggrg2
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
$ write sys$output "*** module aggrg2 ***"
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
$ write sys$output "Invalid argument given to aggrg2.com file -- ", primary
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
$   if F$SEARCH("aggrg2.imake") .nes. ""
$   then
$      vimake aggrg2
$      purge aggrg2.bld
$   else
$      if F$SEARCH("aggrg2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aggrg2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aggrg2.bld "STD"
$   else
$      @aggrg2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aggrg2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aggrg2.com -mixed -
	-s aggrg2.c -
	-i aggrg2.imake -
	-p aggrg2.pdf -
	-t tstaggrg2.pdf tstaggrg2.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create aggrg2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoVicarProtos.h"*/
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MAXCOLS 500
#define MAXSUMCOL 200
#define MAXBYARCOL 100

/************************************************************************/
/* program aggrg2                                                       */
/************************************************************************/
/* 00-06 ...alz... initial version                                      */
/* see pdf for history continuation                                     */
/************************************************************************/

void main44(void)
{
   int i,icol,il,iu,sumcount,byarcount,dummy,unit1,unit2,ibis1,ibis2;
   int agcol,count,area,sumcol[MAXSUMCOL],byar[MAXBYARCOL],swtch;
   int clen1,clen2,ncol1,ncol2,status,uptr,sptr,agcase,outptr;
   int agcolisnum,areacolisnum,agcolwid=0,areacolwid=0,colwid;
   short int *conbuf,x2;
   double *agdat,*areadat,*iodat,val=0,valmax=0,rst;
   char *p=0,*q,cformat[MAXCOLS][6],*agdatstr,*areadatstr,*iodatstr;
  
   zifmessage("aggrg2 version Thu Jan  3 2008");
   
   zvp("agcol",&agcol,&count);
   zvp("area",&area,&count);
   zvparm("sumcol",sumcol,&sumcount,&dummy,20,0);
   zvparm("byar",byar,&byarcount,&dummy,20,0);
   
   /* open the input file and read in the control columns */
   
   status = zvunit(&unit1,"inp",1, NULL);
   status = IBISFileOpen(unit1,&ibis1,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",cformat,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&agdat,clen1,8);
   mz_alloc1((unsigned char **)&areadat,clen1,8);
   mz_alloc1((unsigned char **)&conbuf,clen1,2);
   
   /* control column can be numeric or alpha */
   
   status = IBISColumnGet(ibis1,"FORMAT",cformat[0],agcol);
   if (status!=1) IBISSignal(ibis1,status,1);
   if (cformat[0][0]=='A')
      {
      agcolisnum = 0;
      agcolwid = ms_num(&cformat[0][1])+1;
      mz_alloc1((unsigned char **)&agdatstr,clen1,agcolwid);
      status = IBISColumnRead(ibis1,agdatstr,agcol,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   else
      {
      agcolisnum = 1;
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",agcol);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char*)agdat,agcol,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   
   /* area column can be numeric or alpha */
    
   status = IBISColumnGet(ibis1,"FORMAT",cformat[0],area);
   if (status!=1) IBISSignal(ibis1,status,1);
   if (cformat[0][0]=='A')
      {
      areacolisnum = 0;
      areacolwid = ms_num(&cformat[0][1])+1;
      mz_alloc1((unsigned char **)&areadatstr,clen1,areacolwid);
      status = IBISColumnRead(ibis1,areadatstr,area,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   else
      {
      areacolisnum = 1;
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",area);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char*)areadat,area,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   
   /* set up conbuf with breaks in data */
   
   if (agcolisnum)
      {
      swtch = 3;
      rst = agdat[0];
      for (i=0;i<clen1;i++)
         {
         if (rst!=agdat[i])
	    {
	    rst = agdat[i];
	    swtch = 7-swtch;
	    }
         conbuf[i] = swtch;
         }
      }
   else
      {
      swtch = 3;
      p = &agdatstr[0];
      for (i=0;i<clen1;i++)
         {
         if (strcmp(p,&agdatstr[i*agcolwid])!=0)
	    {
	    p = &agdatstr[i*agcolwid];
	    swtch = 7-swtch;
	    }
         conbuf[i] = swtch;
         }
      free(agdatstr);
      }
   
   /* change conbuf to 2 bit at end of each group, 1 bit for the
      maximum in each group */

   clen2 = 0;
   for (i=0;i<clen1;)
      {
      clen2 +=1;
      uptr = i; sptr = i;
      x2 = conbuf[i];
      conbuf[i] = 0;
      if (areacolisnum) valmax = areadat[i];
         else p = &areadatstr[i*areacolwid];
      while (uptr<clen1-1 && conbuf[uptr+1]==x2)
	 {
	 uptr += 1;
	 conbuf[uptr] = 0;
	 if (areacolisnum)
	    {
	    val = areadat[uptr];
	    if (val>valmax) { valmax = val; sptr = uptr; }
	    }
	 else
	    {
	    q = &areadatstr[uptr*areacolwid];
	    if (strcmp(q,p)>0) { q = p; sptr = uptr; }
	    }
	 }
      conbuf[uptr] +=2;
      conbuf[sptr] +=1;
      i = uptr+1;
      }
   
   free(agdat);
   free(areadat);
   mz_alloc1((unsigned char **)&iodat,clen1,8);
   
   /* open the output file */
   
   ncol2 = ncol1;
   for (icol=1;icol<=ncol1;icol++)
      {
      status = IBISColumnGet(ibis1,"FORMAT",cformat[icol-1],icol);
      if (status!=1) IBISSignal(ibis1,status,1);
     }
   status = zvunit(&unit2,"out",1, NULL);
   status = IBISFileUnit(unit2,&ibis2,"write",ncol2,clen2,(char*)cformat,"column");
   status = IBISFileUnitOpen(ibis2);
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')
         {
         status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis1,status,1);
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      }
   /*status = IBISFileOpen(ibis2,&ibis,"write",4,clen,0,0);*/
   
   /* read each selected column, aggregate using conbuf control,
      then write out the column */
   
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')  /* numeric case */
         {
         status = IBISColumnRead(ibis1,(char*)iodat,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         agcase = 1;
         for (i=0;i<sumcount;i++) if (sumcol[i]==icol) agcase = 2;
         for (i=0;i<byarcount;i++) if (byar[i]==icol) agcase = 3;
         
         iu = -1;
         for (outptr=0;outptr<clen2;outptr++)
            {
            il = iu+1;
            switch(agcase)
               {
               case 1: val = iodat[il]; break;
               case 2: val = 0.0; break;
               case 3: val = 0.0; break;
               }
            for (i=il;i<clen1;i++)
               {
               switch(agcase)
                  {
                  case 1: val = MAX(val,iodat[i]); break;
                  case 2: val += iodat[i]; break;
                  case 3: if (conbuf[i]%2==1) val = iodat[i]; break;
                  }
               if (conbuf[i]>1)
                  {
                  iodat[outptr] = val;
                  iu = i;
                  break;
                  }
               }
            }
         
         status = IBISColumnWrite(ibis2,(char*)iodat,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      else     /* alphabetic case */
         {
         colwid = ms_num(&cformat[icol-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen1,colwid);
         status = IBISColumnRead(ibis1,iodatstr,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         agcase = 1;
         for (i=0;i<sumcount;i++) if (sumcol[i]==icol) agcase = 2;
         for (i=0;i<byarcount;i++) if (byar[i]==icol) agcase = 3;
         if (agcase==2) zmabend("Can't sum an alpha column");
         
         iu = -1;
         for (outptr=0;outptr<clen2;outptr++)
            {
            il = iu+1;
            switch(agcase)
               {
               case 1: p = &iodatstr[il*colwid];
               }
            for (i=il;i<clen1;i++)
               {
               switch(agcase)
                  {
                  case 1: if (strcmp(p,&iodatstr[i*colwid])<=0)
                             p = &iodatstr[i*colwid]; break;
                  case 3: if (conbuf[i]%2==1) p = &iodatstr[i*colwid]; break;
                  }
               if (conbuf[i]>1)
                  {
                  strcpy(&iodatstr[outptr*colwid],p);
                  iu = i;
                  break;
                  }
               }
            }
         
         status = IBISColumnWrite(ibis2,iodatstr,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         
         free(iodatstr);
         }
      }
         
   
   /* print in/out file lengths */
   
   printf("%d records in, %d records out\n",clen1,clen2);
   
   /* close files */

   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
  
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create aggrg2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM aggrg2

   To Create the build file give the command:

		$ vimake aggrg2			(VMS)
   or
		% vimake aggrg2			(Unix)


************************************************************************/


#define PROGRAM	aggrg2
#define R2LIB

#define MODULE_LIST aggrg2.c

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
$ create aggrg2.pdf
PROCESS		HELP=*
! "aggrg2" PDF/HELP - VICAR/IBIS MOSAIC SOFTWARE
PARM INP TYPE=(STRING)
PARM OUT TYPE=(STRING)
PARM AGCOL TYPE=INTEGER,DEFAULT=1
PARM AREA TYPE=INTEGER,DEFAULT=1
PARM BYAR TYPE=INTEGER,COUNT=(1:20),DEFAULT=0
PARM SUMCOL TYPE=INTEGER,COUNT=(1:20),DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program "aggrg2"
.HELP
PURPOSE

	"aggrg2" collapses columns of numbers in an IBIS interface file into
smaller columns using a designated column as the control. Within the control
column each number which is repeated will define one row. A second key or
index, the area key, selects the maximum value from an index group and stores
is row position in the column. The row position is a pointer from which a
sample can be selected for other columns.

	There are three alternative methods of collapsing columns:

1. The first option selects a sample item from a column based on the row
position of the maximum value from the key column. One value is selected
for each group of control values.

2. The second option aggregates all the values in a column based on the
limits defined by the control values.

3. The third option selects a maximum value from the column based on the
limits defined by the control values (default).

.PAGE
	To illustrate the options a sample case is shown:

CONTROL AREA	SAMPLE		CONTROL	OPTION	OPTION	OPTION
COLUMN	KEY	COLUMN		COLUMN	ONE	TWO	THREE
1	10	 2		1	 4	12	 6
1	20	 4		2	10	18	10
1	15	 6		3	16	42	16
2	24	 8		4	18	18	18
2	31	10		5	20	20	20
3	16	12
3	31	14
3	53	16
4	10	18
5	20	20

.PAGE
TAE COMMAND LINE FORMAT

	aggrg2 INP=FILE OUT=FILE2 AGCOL=N AREA=A BYAR=(X,Y,Z) SUMCOL=(A,B,C)
	aggrg2 INP=FILE OUT=FILE2 AGCOL=N SUMCOL=(X,Y,Z)

	FILE represents the interface file, N the control column,
	A the keyword area, X,Y, and Z the columns to be collapsed,
	and A,B, and C the columns to be collapsed by aggregating
	all the values for an index group.

EXAMPLE

	sort INP=A SORTCOL=1
	aggrg2 INP=A OUT=B AGCOL=1

	This example collapses the interface file by replacing multiple
rows with the same value in column 1 by a single row and choosing the
largest value in the other rows to be retained.

RESTRICTIONS

The interface file can be sorted on the AGCOL, but it does not have to
be.  It will be collapsed on each set of contiguous values, using
the breaks between them to identify the sets.  For example if the 
agcol contains (0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,0) then 7 sets of 
contiguous values will be recognized.

Dynamic allocation is used so the columns can be of any length.

	WRITTEN BY		A. L. Zobrist		 1 Dec 1975
	COGNIZANT PROGRAMMER	K. F. Evans
	REVISION		1			26 Jul 1978
        Made portable for UNIX  A. Scop (CRI)            2 Jan 1995
        rewritten in C          A. L. Zobrist            7 Jun 2000
Thu Jan  3 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  

.LEVEL1
.VARIABLE INP
Standard IBIS input file.
.VARIABLE OUT
Standard IBIS output file.
.VARIABLE AGCOL
Control column number.
.VARIABLE AREA
Area maximum key.
.VARIABLE BYAR
Columns to be collapsed by the
first option.
.VARIABLE SUMCOL
Columns to be collapsed by
aggregating all values for
an index group.
.LEVEL2
.VARIABLE INP
Standard IBIS interface file
of 512 samples by N lines.
.VARIABLE OUT
Standard IBIS interface file
of 512 samples by N lines.
.VARIABLE AGCOL
This column number is used as
a control for the columns to
be compressed. Each number
repeated in this column will
form 1 row on the output file.
THE COLUMN DOES NOT HAVE TO BE
SORTED, IT WORKS ON "BREAKS" IN
THE DATA.  FOR EXAMPLE YOU CAN
USE
    0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,0
TO GET SEVEN RECORDS OUT.
.VARIABLE AREA
This column is used as an area
maximum key. The maximum value
is found in each index group
and stored by row position.
This row position is a pointer
to the same positions in other
columns.
.VARIABLE BYAR
These are the columns to be
collapsed by the first option,
that is select one sample row
position for each index group
by the maximum key area.
.VARIABLE SUMCOL
These are the columns to be
collapsed by aggregating all
values for an index group.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaggrg2.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

! Aug 28, 2013 - RJB
! TEST SCRIPT FOR AGGRG2 
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


! test max default on all columns

ibis-gen xxxagg1 nc=5 nr=5 datacol=(1,2,3,4,5) +
    data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
aggrg2 xxxagg1 xxxaggo1 AGCOL=1
ibis-list xxxagg1
ibis-list xxxaggo1

! test sum and maxarea functions

ibis-gen xxxagg2 nc=5 nr=5 datacol=(1,2,3,4,5) +
    format=("REAL","DOUB","DOUB","DOUB","DOUB") +
    data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
aggrg2 xxxagg2 xxxaggo2 AGCOL=1 AREA=4 BYAR=(2,3) SUMCOL=(5)
ibis-list xxxagg2
ibis-list xxxaggo2

! case showing mix of types

ibis-gen xxxa nc=2 nr=1 format=("A5","A5","A5","A5","A5","DOUB")
acopin (ct/aggrg2.dat,xxxa) cols=(1,2,3,4,5,6) lead_rm=2
aggrg2 xxxa xxxa2 AGCOL=1 AREA=4 BYAR=(2,3) SUMCOL=6
ibis-list xxxa 'format
ibis-list xxxa2 'format

rm>
let $echo="no"
ush rm ct
end-proc
$!-----------------------------------------------------------------------------
$ create tstaggrg2.log
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
    data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
Beginning VICAR task ibis
aggrg2 xxxagg1 xxxaggo1 AGCOL=1
Beginning VICAR task aggrg2
aggrg2 version Thu Jan  3 2008
5 records in, 2 records out
ibis-list xxxagg1
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00        4.00        5.00
        1.00       12.00       13.00        7.00        4.00
        1.00        5.00        4.00        3.00       15.00
        2.00        8.00        4.00        6.00       15.00
        2.00        2.00       17.00        9.00        5.00
ibis-list xxxaggo1
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       12.00       13.00        7.00       15.00
        2.00        8.00       17.00        9.00       15.00
ibis-gen xxxagg2 nc=5 nr=5 datacol=(1,2,3,4,5)  +
    format=("REAL","DOUB","DOUB","DOUB","DOUB")  +
    data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
Beginning VICAR task ibis
aggrg2 xxxagg2 xxxaggo2 AGCOL=1 AREA=4 BYAR=(2,3) SUMCOL=(5)
Beginning VICAR task aggrg2
aggrg2 version Thu Jan  3 2008
5 records in, 2 records out
ibis-list xxxagg2
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        2.00        3.00        4.00        5.00
        1.00       12.00       13.00        7.00        4.00
        1.00        5.00        4.00        3.00       15.00
        2.00        8.00        4.00        6.00       15.00
        2.00        2.00       17.00        9.00        5.00
ibis-list xxxaggo2
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00       12.00       13.00        7.00       24.00
        2.00        2.00       17.00        9.00       20.00
ibis-gen xxxa nc=2 nr=1 format=("A5","A5","A5","A5","A5","DOUB")
Beginning VICAR task ibis
acopin (ct/aggrg2.dat,xxxa) cols=(1,2,3,4,5,6) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 6
    Col 1 is ASCII width = 6
    Col 2 is ASCII width = 6
    Col 3 is ASCII width = 6
    Col 4 is ASCII width = 6
    Col 5 is ASCII width = 6
Output length is 5 records


aggrg2 xxxa xxxa2 AGCOL=1 AREA=4 BYAR=(2,3) SUMCOL=6
Beginning VICAR task aggrg2
aggrg2 version Thu Jan  3 2008
5 records in, 2 records out
ibis-list xxxa 'format
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
          A5          A5          A5          A5          A5        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
       aaaaa       bbbbb       ccccc       ddddd       eeeee        5.00
       aaaaa       lllll       mmmmm       ggggg       ddddd        4.00
       aaaaa       eeeee       ddddd       ccccc       ooooo       15.00
       bbbbb       hhhhh       ddddd       fffff       ooooo       15.00
       bbbbb       bbbbb       qqqqq       iiiii       eeeee        5.00
ibis-list xxxa2 'format
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
          A5          A5          A5          A5          A5        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
       aaaaa       lllll       mmmmm       ggggg       ooooo       24.00
       bbbbb       bbbbb       qqqqq       iiiii       ooooo       20.00
let $echo="no"
$ Return
$!#############################################################################
