$!****************************************************************************
$!
$! Build proc for MIPL module sort
$! VPACK Version 1.9, Thursday, May 28, 2015, 19:26:09
$!
$! Execute by entering:		$ @sort
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
$ write sys$output "*** module sort ***"
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
$ write sys$output "Invalid argument given to sort.com file -- ", primary
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
$   if F$SEARCH("sort.imake") .nes. ""
$   then
$      vimake sort
$      purge sort.bld
$   else
$      if F$SEARCH("sort.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sort
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sort.bld "STD"
$   else
$      @sort.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sort.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sort.com -mixed -
	-s sort.c -
	-i sort.imake -
	-p sort.pdf -
	-t tstsort.pdf tstsort.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sort.c
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
#include "cartoSortUtils.h"

#define MAXCOLS 200

/************************************************************************/
/* program sort                                                      */
/************************************************************************/
/*  00-09 ...alz... initial version                     */
/************************************************************************/

void sorta(buf,wid,ptr,n)
     char *buf;
     int wid,*ptr,n;
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      char *ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;
      if ((ibuf=(char *)malloc(wid))==NULL) zmabend("malloc failed");
      
 l10: k = m;
      strcpy(ibuf,&buf[k*wid]);
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&strcmp(&buf[(j+1)*wid],&buf[j*wid])>0) j++;
      if (strcmp(&buf[j*wid],ibuf)<1) goto l25;
      strcpy(&buf[k*wid],&buf[j*wid]);
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: strcpy(&buf[k*wid],ibuf);
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      strcpy(ibuf,&buf[k*wid]);
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&strcmp(&buf[(j+1)*wid],&buf[j*wid])>0) j++;
      if (strcmp(&buf[j*wid],ibuf)<1) goto l45;
      strcpy(&buf[k*wid],&buf[j*wid]);
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: strcpy(&buf[k*wid],ibuf);
      ptr[k] = iptr;
      strcpy(ibuf,&buf[0]);
      iptr = ptr[0];
      strcpy(&buf[0],&buf[l*wid]);
      ptr[0] = ptr[l];
      strcpy(&buf[l*wid],ibuf);
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;
      
      free(ibuf);
      return;
}

void sortreca(key,wid,ptr,len)
   char *key;
   int wid,*ptr,len;
{
   char *temp;
   int i;
   
   if (len<2) return;
   if ((temp=(char *)malloc(wid*len))==NULL) zmabend("malloc failed");
   for (i=0;i<len;i++) strncpy(&temp[i*wid],&key[i*wid],wid);
   for (i=0;i<len;i++) strncpy(&key[i*wid],&temp[(ptr[i]-1)*wid],wid);
   free(temp);
   return;
}

void main44(void)
{
   int i,icol,k,ascend,sortcol[20],sortcount,dummy,indexcol,unit;
   int ibis,status,clen,ncol,pu,tcs,ksv,oldcs,filwid,tcx,ku;
   int *cx,*cs;
   double *iodat;
   char coltype[MAXCOLS][6],*iodatstr;
 
   zifmessage("sort version Wed Jan  2 2008");
   
   /* get the basic parameters */
   
   ascend = zvptst("ascend");
   zvparm("sortcol",sortcol,&sortcount,&dummy,MAXCOLS,0);
   zvp("indexcol",&indexcol,&dummy);
   
   /* open the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"formats",coltype,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&iodat,clen,8);
   mz_alloc1((unsigned char **)&cx,clen,4);
   mz_alloc1((unsigned char **)&cs,clen,4);
   
   /* read each sort column and sort, keep the result in cx
      cs keeps track of the identical key groups */

   for (i=0;i<clen;i++) { cx[i] = i+1; cs[i] = 1; }
   for (icol=0;icol<sortcount;icol++)
      {
      if (coltype[sortcol[icol]-1][0]!='A')     /* numeric column */
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",sortcol[icol]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)iodat,sortcol[icol],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         if (icol>0) sortrec8(iodat,cx,clen);
         
         for (k=0;k<clen;)
	    {
	    pu = k; tcs = cs[k];
	    while (cs[pu+1]==tcs&&pu<clen-1) pu++;
	    sort8(&iodat[k],&cx[k],pu-k+1);
	    k = pu+1;
	    }
         ksv = 1; oldcs = cs[0];
         for (k=1;k<clen;k++)
	    {
	    if (iodat[k]!=iodat[k-1])
	       {
	       ksv += 1;
	       goto nxtcs;
	       }
	    if (cs[k]!=oldcs) ksv += 1;
	    nxtcs: oldcs = cs[k]; cs[k] = ksv;
	    }
	 } /* end of numeric case */
      else     /* alpha column */
         {
         filwid = ms_num(&coltype[sortcol[icol]-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen,filwid);
         
         status = IBISColumnRead(ibis,iodatstr,sortcol[icol],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         if (icol>0) sortreca(iodatstr,filwid,cx,clen);
                  
         for (k=0;k<clen;)
	    {
	    pu = k; tcs = cs[k];
	    while (cs[pu+1]==tcs&&pu<clen-1) pu++;
	    sorta(&iodatstr[k*filwid],filwid,&cx[k],pu-k+1);
	    k = pu+1;
	    }
         ksv = 1; oldcs = cs[0];
         for (k=1;k<clen;k++)
	    {
	    if (strcmp(&iodatstr[k*filwid],&iodatstr[(k-1)*filwid])!=0)
	       {
	       ksv += 1;
	       goto nxtcs2;
	       }
	    if (cs[k]!=oldcs) ksv += 1;
	    nxtcs2: oldcs = cs[k]; cs[k] = ksv;
	    }
	 free(iodatstr);
         } /* end of alpha case */
      } /* end of loop over columns */
  
   /* not ascending, reverse cs and cx */
   
   if (!ascend)
      {
      for (k=0;k<clen/2;k++)
	 {
	 ku = clen-k-1;
	 tcs = cs[k]; cs[k] = cs[ku]; cs[ku] = tcs;
	 tcx = cx[k]; cx[k] = cx[ku]; cx[ku] = tcx;
	 }
      }

   /* move the entire file by the pointers cx, place cs in the indexcol
      if selected */

   for (icol=0;icol<ncol;icol++)
      {
      if (coltype[icol][0]!='A')     /* numeric column */
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",icol+1);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)iodat,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         
         if (icol+1!=indexcol) sortrec8(iodat,cx,clen);
         else for (i=0;i<clen;i++) iodat[i] = (double)cs[i];
         
         status = IBISColumnWrite(ibis,(char*)iodat,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else     /* alpha column */
         {
         filwid = ms_num(&coltype[icol][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen,filwid);
         
         status = IBISColumnRead(ibis,iodatstr,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         
         sortreca(iodatstr,filwid,cx,clen);
         
         status = IBISColumnWrite(ibis,iodatstr,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         free(iodatstr);
         }
      }
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sort.imake
#define  PROGRAM   sort

#define MODULE_LIST sort.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create sort.pdf
PROCESS		HELP=*
! VERSION 1
! SORT PDF - VICAR/IBIS MOSAIC SOFTWARE
PARM INP TYPE=STRING
PARM SORTCOL TYPE=INTEGER COUNT=(1:20)
PARM INDEXCOL TYPE=INTEGER COUNT=1,DEFAULT=0
PARM DESCEND TYPE=KEYWORD VALID=(DESCEND,ASCEND) DEFAULT=ASCEND
END-PROC
.TITLE
VICAR/IBIS Program sort
.HELP
PURPOSE

	SORT performs a multiple column sort on an IBIS interface file. The
program starts with the column defined as primary and performs an incore sort.
Subsequent columns are sorted only within limits established by equal values
in previously sorted columns. The program incorporates two temporary fields as
sorting and indexing columns. The index column can be retained for later use
if specified.

TAE COMMAND LINE FORMAT

	sort INP=A SORTCOL=(B,C,D,E,...F) INDEXCOL=G
	sort INP=A SORTCOL=(B,C,D,E,...F) DESCEND INDEXCOL=G

	INP is a random access file which is read from or written to depending
on the parameters. SORTCOL specifies the columns to be sorted in the order in
which the sort is to occur. DESCEND is a keyword whose presence indicates that
all the columns are to be sorted in descending order. INDEXCOL is an integer
which designates the column to recieve the index numbers assigned to each
unique combination of letters or numbers in the sorted columns created by the
sorting process. The columns can be numdric or alphabetic, or a mixture of
the two in the case of multiple column sorts.

EXAMPLE

	sort INP=A SORTCOL=(1,2) INDEXCOL=3

	In this example an 8 character name is stored in the first 2 columns.
The file is sorted into alphabetic order. The index stored in column 3 can be
used in place of the names for other operations such as aggregation.

OPERATION

	SORT uses two temporary arrays. The first holds the SORT data and the
second defines the boundary limits. Limits are defined using an index number
which indicates the portion of the file to be sorted. The boundary index is
initialized to 1 which indicates that the entire column is to be sorted. As
subsequent sorts are made on the file the index number is broken into groupings
of the same number which indicate the sort limits. The holding array is sorted
each time the index changes to achieve a multiple column sort.

	At the completion of the sort process, the reordering is in the form
of pointers. The pointers are used to move all the columns of the file one at
a time. The limit array serves as an index.

WRITTEN BY		A. L. Zobrist		15 Dec 1976
COGNIZANT PROGRAMMER	K. F. Evans
DOCUMENTED BY		R. Wayne Bannister
REVISION HISTORY
  1995-03-06 C. Randy Schenk (CRI) - Ported to UNIX
  2000-09-04 A. Zobrist - rewritten in C
  2008-01-02 WLB - switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  

.LEVEL1
.VARIABLE INP
File to be sorted.
.VARIABLE SORTCOL
Columns to be sorted in order.
.VARIABLE INDEXCOL
Column to get index numbers.
.VARIABLE DESCEND
Descending order keyword.
.LEVEL2
.VARIABLE INP
INP is a random access file
which is read from and written
to depending on the parameters
.VARIABLE SORTCOL
SORTCOL specifies 1 or more
columns to be sorted in the
order in which it is to occur.
.VARIABLE INDEXCOL
INDEXCOL specifies the column
to recieve the index numbers
assigned to each unique com-
bination of letters or numbers
in the sorted columns created
by the sorting process.
.VARIABLE DESCEND
DESCEND is a keyword whose
presence indicates that all
the columns are to be
sorted in descending order.
.VARIABLE ALPHA
ALPHA is a keyword indicating
that the sorted columns are
alphabetic.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstsort.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"


! numeric cases


ibis-gen xxc version=ibis-2 org=column nc=4 nr=22 deffmt=DOUB
mf3 xxc func="c1=@int(@rand*8)$c2=@int(@rand*8)"
ibis-list xxc
sort xxc sortcol=(1,2)  indexcol=3
ibis-list xxc

!sort xxc sortcol=(1,2)  indexcol=3 'descend
!ibis-list xxc



!In this example an 8 character name is stored in the first 2 columns.
!The file is sorted into alphabetic order. The index stored in column 3 can be
!used in place of the names for other operations such as aggregation.

  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-1 +
     index=4 strcol=(1,2) +
     string=(zzzz,zzzz,+
	     zzzz,yyyy, +
	     yyyy,yyyy, +
	     xxxx,xxxx, +
	     aaaa,bbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2)  indexcol=3
   ibis-list a a4col=(1,2)

  ibis-gen a nc=3 nr=5 datacol=(1,2) 'ibis-1 +
    data=(1,3, +
 	  5,3,+
	  1,4,+
	  5,4,+
	  2,5)
  ibis-list a
  sort a sortcol=(1,2) indexcol=3
  ibis-list a

  sort a sortcol=(1,2) indexcol=3 'descend
  ibis-list a
!

  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-2 +
     index=4 strcol=(1,2) +
     string=(zzzz,zzzz,+
	     zzzz,yyyy, +
	     yyyy,yyyy, +
	     xxxx,xxxx, +
	     aaaa,bbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2)  indexcol=3
   ibis-list a a4col=(1,2)

  ibis-gen a nc=4 nr=10 format=(a12,a12,real,real) 'ibis-2 +
     index=4 strcol=(1,2) +
     string=(zzzzzzzzzz,zzzzzzzzzz,+
	     zzzzzzzzzz,yyyyyyyyyy, +
	     zzzzzzzzzc,yyyyyyyyyy, +
	     zzzzzzzzzd,yyyyyyyyyy, +
	     zzzzzzzzza,yyyyyyyyyy, +
	     zzzzzzzzzb,yyyyyyyyyb, +
	     zzzzzzzzzb,yyyyyyyyya, +
	     yyyyyyyyyy,yyyyyyyyyy, +
	     xxxxxxxxxx,xxxxxxxxxx, +
	     aaaaaaaaaa,bbbbbbbbbb) 
   ibis-list a a4col=(1,2)
   sort a sortcol=(1,2)  indexcol=3
   ibis-list a a4col=(1,2)
   
!  the last test is a mixed alphabetic-numeric sort
   
   sort a sortcol=(1,4)  indexcol=3
   ibis-list a a4col=(1,2)

   ush rm a xxc

end-proc
$!-----------------------------------------------------------------------------
$ create tstsort.log
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

let $autousage="none"
ibis-gen xxc version=ibis-2 org=column nc=4 nr=22 deffmt=DOUB
Beginning VICAR task ibis
mf3 xxc func="c1=@int(@rand*8)$c2=@int(@rand*8)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
function string = c1=@int(@rand*8)$c2=@int(@rand*8)
22 records in
ibis-list xxc
Beginning VICAR task ibis
 
Number of Rows:22  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:22
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        1.00        4.00        0.00        0.00
        5.00        3.00        0.00        0.00
        0.00        4.00        0.00        0.00
        6.00        7.00        0.00        0.00
        4.00        6.00        0.00        0.00
        6.00        1.00        0.00        0.00
        5.00        2.00        0.00        0.00
        2.00        2.00        0.00        0.00
        6.00        7.00        0.00        0.00
        5.00        3.00        0.00        0.00
        3.00        6.00        0.00        0.00
        2.00        1.00        0.00        0.00
        5.00        0.00        0.00        0.00
        2.00        7.00        0.00        0.00
        3.00        0.00        0.00        0.00
        6.00        1.00        0.00        0.00
        6.00        3.00        0.00        0.00
        4.00        1.00        0.00        0.00
        5.00        4.00        0.00        0.00
        0.00        7.00        0.00        0.00
        3.00        3.00        0.00        0.00
        0.00        2.00        0.00        0.00
sort xxc sortcol=(1,2)  indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
ibis-list xxc
Beginning VICAR task ibis
 
Number of Rows:22  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:22
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        0.00        2.00        1.00        0.00
        0.00        4.00        2.00        0.00
        0.00        7.00        3.00        0.00
        1.00        4.00        4.00        0.00
        2.00        1.00        5.00        0.00
        2.00        2.00        6.00        0.00
        2.00        7.00        7.00        0.00
        3.00        0.00        8.00        0.00
        3.00        3.00        9.00        0.00
        3.00        6.00       10.00        0.00
        4.00        1.00       11.00        0.00
        4.00        6.00       12.00        0.00
        5.00        0.00       13.00        0.00
        5.00        2.00       14.00        0.00
        5.00        3.00       15.00        0.00
        5.00        3.00       15.00        0.00
        5.00        4.00       16.00        0.00
        6.00        1.00       17.00        0.00
        6.00        1.00       17.00        0.00
        6.00        3.00       18.00        0.00
        6.00        7.00       19.00        0.00
        6.00        7.00       19.00        0.00
  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-1  +
     index=4 strcol=(1,2)  +
     string=(zzzz,zzzz, +
	     zzzz,yyyy,  +
	     yyyy,yyyy,  +
	     xxxx,xxxx,  +
	     aaaa,bbbb)
Beginning VICAR task ibis
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 4       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        zzzz        zzzz        0.00        1.00
        zzzz        yyyy        0.00        2.00
        yyyy        yyyy        0.00        3.00
        xxxx        xxxx        0.00        4.00
        aaaa        bbbb        0.00        5.00
   sort a sortcol=(1,2)  indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 4       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        aaaa        bbbb        1.00        5.00
        xxxx        xxxx        2.00        4.00
        yyyy        yyyy        3.00        3.00
        zzzz        yyyy        4.00        2.00
        zzzz        zzzz        5.00        1.00
  ibis-gen a nc=3 nr=5 datacol=(1,2) 'ibis-1  +
    data=(1,3,  +
 	  5,3, +
	  1,4, +
	  5,4, +
	  2,5)
Beginning VICAR task ibis
  ibis-list a
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 3       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        3.00        0.00
        5.00        3.00        0.00
        1.00        4.00        0.00
        5.00        4.00        0.00
        2.00        5.00        0.00
  sort a sortcol=(1,2) indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
  ibis-list a
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 3       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        1.00        3.00        1.00
        1.00        4.00        2.00
        2.00        5.00        3.00
        5.00        3.00        4.00
        5.00        4.00        5.00
  sort a sortcol=(1,2) indexcol=3 'descend
Beginning VICAR task sort
sort version Wed Jan  2 2008
  ibis-list a
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 3       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------
         C:1         C:2         C:3
+-----------+-----------+-----------
        5.00        4.00        5.00
        5.00        3.00        4.00
        2.00        5.00        3.00
        1.00        4.00        2.00
        1.00        3.00        1.00
  ibis-gen a nc=4 nr=5 format=(a4,a4,real,real) 'ibis-2  +
     index=4 strcol=(1,2)  +
     string=(zzzz,zzzz, +
	     zzzz,yyyy,  +
	     yyyy,yyyy,  +
	     xxxx,xxxx,  +
	     aaaa,bbbb)
Beginning VICAR task ibis
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        zzzz        zzzz        0.00        1.00
        zzzz        yyyy        0.00        2.00
        yyyy        yyyy        0.00        3.00
        xxxx        xxxx        0.00        4.00
        aaaa        bbbb        0.00        5.00
   sort a sortcol=(1,2)  indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        aaaa        bbbb        1.00        5.00
        xxxx        xxxx        2.00        4.00
        yyyy        yyyy        3.00        3.00
        zzzz        yyyy        4.00        2.00
        zzzz        zzzz        5.00        1.00
  ibis-gen a nc=4 nr=10 format=(a12,a12,real,real) 'ibis-2  +
     index=4 strcol=(1,2)  +
     string=(zzzzzzzzzz,zzzzzzzzzz, +
	     zzzzzzzzzz,yyyyyyyyyy,  +
	     zzzzzzzzzc,yyyyyyyyyy,  +
	     zzzzzzzzzd,yyyyyyyyyy,  +
	     zzzzzzzzza,yyyyyyyyyy,  +
	     zzzzzzzzzb,yyyyyyyyyb,  +
	     zzzzzzzzzb,yyyyyyyyya,  +
	     yyyyyyyyyy,yyyyyyyyyy,  +
	     xxxxxxxxxx,xxxxxxxxxx,  +
	     aaaaaaaaaa,bbbbbbbbbb)
Beginning VICAR task ibis
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
  zzzzzzzzzz  zzzzzzzzzz        0.00        1.00
  zzzzzzzzzz  yyyyyyyyyy        0.00        2.00
  zzzzzzzzzc  yyyyyyyyyy        0.00        3.00
  zzzzzzzzzd  yyyyyyyyyy        0.00        4.00
  zzzzzzzzza  yyyyyyyyyy        0.00        5.00
  zzzzzzzzzb  yyyyyyyyyb        0.00        6.00
  zzzzzzzzzb  yyyyyyyyya        0.00        7.00
  yyyyyyyyyy  yyyyyyyyyy        0.00        8.00
  xxxxxxxxxx  xxxxxxxxxx        0.00        9.00
  aaaaaaaaaa  bbbbbbbbbb        0.00       10.00
   sort a sortcol=(1,2)  indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
  aaaaaaaaaa  bbbbbbbbbb        1.00       10.00
  xxxxxxxxxx  xxxxxxxxxx        2.00        9.00
  yyyyyyyyyy  yyyyyyyyyy        3.00        8.00
  zzzzzzzzza  yyyyyyyyyy        4.00        5.00
  zzzzzzzzzb  yyyyyyyyya        5.00        7.00
  zzzzzzzzzb  yyyyyyyyyb        6.00        6.00
  zzzzzzzzzc  yyyyyyyyyy        7.00        3.00
  zzzzzzzzzd  yyyyyyyyyy        8.00        4.00
  zzzzzzzzzz  yyyyyyyyyy        9.00        2.00
  zzzzzzzzzz  zzzzzzzzzz       10.00        1.00
   sort a sortcol=(1,4)  indexcol=3
Beginning VICAR task sort
sort version Wed Jan  2 2008
   ibis-list a a4col=(1,2)
Beginning VICAR task ibis
 
Number of Rows:10  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:10
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
  aaaaaaaaaa  bbbbbbbbbb        1.00       10.00
  xxxxxxxxxx  xxxxxxxxxx        2.00        9.00
  yyyyyyyyyy  yyyyyyyyyy        3.00        8.00
  zzzzzzzzza  yyyyyyyyyy        4.00        5.00
  zzzzzzzzzb  yyyyyyyyyb        5.00        6.00
  zzzzzzzzzb  yyyyyyyyya        6.00        7.00
  zzzzzzzzzc  yyyyyyyyyy        7.00        3.00
  zzzzzzzzzd  yyyyyyyyyy        8.00        4.00
  zzzzzzzzzz  zzzzzzzzzz        9.00        1.00
  zzzzzzzzzz  yyyyyyyyyy       10.00        2.00
   ush rm a xxc
end-proc
$ Return
$!#############################################################################
