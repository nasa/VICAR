$!****************************************************************************
$!
$! Build proc for MIPL module acopin
$! VPACK Version 1.9, Thursday, January 29, 2015, 12:20:02
$!
$! Execute by entering:		$ @acopin
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
$ write sys$output "*** module acopin ***"
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
$ write sys$output "Invalid argument given to acopin.com file -- ", primary
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
$   if F$SEARCH("acopin.imake") .nes. ""
$   then
$      vimake acopin
$      purge acopin.bld
$   else
$      if F$SEARCH("acopin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake acopin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @acopin.bld "STD"
$   else
$      @acopin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create acopin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack acopin.com -mixed -
	-p acopin.pdf -
	-i acopin.imake -
	-s acopin.c -
	-t tstacopin.pdf tstacopin.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create acopin.pdf
process help=*
parm inp        type=(string) count=2
parm cols       type=int count=0:100
parm rtjust     type=keyword valid=rtjust count=(0:1) default=--
parm lead_rm    type=int count=1 default=0
end-proc
.title
VICAR Program acopin - converts an ASCII file into a IBIS table
.help
PURPOSE

    ACOPIN converts an ascii file (that is organized in columns)
    into corresponding columns of an IBIS table file.

    ACOPIN requires that the ascii data be imported into a
    preexisting ibis file. Therefore, acopin is almost always
    preceeded by an ibis-gen command.

.PAGE
OPERATION

    The ASCII columns can be either numeric or ascii. Formats
    are controlled by the IBIS file column format. However,
    acopin only operates on ascii and DOUB data so on input 
    automatic numeric conversions of BYTE, HALF, FULL, REAL, 
    or COMP to DOUB will take place. ASCII text will be 
    imported into whatever the original format (A1-A255) that
    is in the preexisting IBIS file.

    NOTE: If the ASCII text contains embedded blanks, there
    will be problems since it is assumed that the column
    separator is one or more "blank" characters. 

    So 123 in the ascii file can become a numeric (DOUB) or
    or "123" alphabetic in the IBIS table.  Conversion errors,
    such as a letter in a numeric field (except for e, d, or E
    or D, as exponents) will abort the program.

    Only one execution of acopin is required to input both
    numeric and alphabetic data.

.PAGE
PARAMETERS

    Two input files are required, an ascii column table and
    a preformatted IBIS table.

    COLS are values given in integer format and  place the
    data into the IBIS file columns in the same order as the 
    data in the ascii file. The columns can be named using 
    the parameter COLS, or if COLS is omitted, all of the 
    columns will be used in order.
     
    Columns that are not listed in the COLS format will be 
    will be filled with zero's (Numeric) or blanks (ASCII).
     
    The last record of the ascii column file can end with 
    a newline, or can be abruptly ended at the last input field.

    Normally, the a column that is to be imported as an ASCII
    text column is left-justified. The keyword parameter, 
    RTJUST allows this convention to be reversed.

    It is also common for ascii column files to have one or
    more header records describing the table and/or its
    contents. The number in the parameter LEAD_RM allows you
    prevent these header records from being imported.
     
    Internally, the ascii column data is read into a giant buffer 
    and then written out to columns of the IBIS file.
    Conversions to DOUB and ascii are performed according to the
    type (Case 0 is ASCII) and Case 8 is Numeric) for the IBIS column.


.PAGE
TAE COMMAND LINE FORMAT

     acopin INP=(A,B) COLS=(c1,c2,...,cn) OPTIONS
     
     where

     A           is an ascii file of data in columns
     B		 is an update IBIS file, containing no data
     COLS        are the IBIS file columns (integers) that
                 receive the data in the same order as the
                 data in the ascii file.  
     OPTIONS     parameters for right justification of alphabetic
                 data and removal of header lines.
   
.PAGE

TIMING

     Should be able to read millions if IBIS records in less than
     five minutes.
     
RESTRICTIONS

   The maximum number of records is limited by IBIS table
   size (currently about 10 million?).  Internal to the program,
   dynamic memory allocation is used.  The number of columns
   is limited to 100.  The total virtual memory size (number of 
   columns times number of records times bytes per element)
   cannot exceed virtual memory size.

    The number of columns is limited to 20 in DOUB format.
    If you try to inport more than 20 columns you get
    the error message:

    acopin version 17-jun-00
    Partial record at end
    ** ABEND called **

    This occurs if you put in more than 20 cols in the cols
    parameter or if you try to import only 20 cols in the cols
    parameter and there are more than 20 columns in the ascii
    file.

    Thus, if you have to import more than 20 columns you must
    divide the ascii columnar file into 20 column parcels.
    Import each of the 20-column parcels into an IBIS file.
    At the end join each of the IBIS 20-column files using
    the IBIS program icat with the 'h keyword.

    Columns in the ascii column file that are to be imported
    into an ASCII formatted column must not contain embedded
    blanks, since a blank or multiple blanks, signify a
    column separator.


    If the last column is an ascii text column, there must be
    an entry and not just a blank field. If the first few
    records have an ascii string and then one record is missing
    it, then you will get an error message of the form:

    Number of input cols = 6
        Col 6 is ASCII width = 41
        ??E character = Cloud in numeric col 0  record 258
        ??E Are there blanks in ASCII field?
    ??E Case 8 (DOUBLE): Non-numeric data in a numeric field
     ** ABEND called **

    What is actually happening here is that this is the first record
    beyond the missing last ascii column and it is trying to interpret
    your text as a number.

    Example:
    256  23.0 40.0 15.6 Cloud
    257  10.0 38.1 14.2                 <---missing ascii text
    258  14.2 33.7 19.6 Cloud 

Note: 1-28-2010

If you have a non-existent IBIS file, it does not report File not found.
Instead it crashes with the following:

[TAE-PRCSTRM] Abnormal process termination; process status code = 10.

This is an error in the IBISFileOpen routine that has to be fixed.


.PAGE
WRITTEN BY:            A. L. Zobrist, 29 may 2000

COGNIZANT PROGRAMMER:  R. J. Bambery

REVSIONS:
        A. L. Zobrist, 14 Nov 2001 - starting version
        R. J. Bambery, 9 Sep 2007 - Documentation of limitations;
        R. J. Bambery, 21 Sep 2007 - Updated documentation and error
                            messages in code; 
        W. Bunch, 3 Jan 2008 - Modifications to fix ANSI Std C 
                            warnings on Linux, added to svn 
        R. J. Bambery, 26 Feb 2008 - Integrated Walt Bunch's changes
                            with my changes 
        R. J. Bambery, 28 Jul 2008 - Made compatible with Linux afids 5b
        R. J. Bambery, 03 Dec 2009 - Made compatible with new 64-bit afids
                            (removed cartoVicarProtos.h) (Makefile.acopin)
        R. J. Bambery, 28 Jan 2010 - Made compatible with 64-bit afids Build 793
                            Linux, MacOSX (both Intel/PowerPC)
                            removed 99 character limit on inp 
                            both the preexisting program and this modification still give   
                            [TAE-PRCSTRM] Abnormal process termination; process status code = 10.
                            when no ibis file exists.
        R. J. Bambery, 08 Feb 2010 - fixed call to IBISFileClose(ibis,NULL);
        R. J. Bambery, 23 Jul 2010 - Improved error detection and messaging after
                            problems with files which gave 
                            [TAE-PRCSTRM] Abnormal process termination; process status code = 11.
        R. J. Bambery, 13 Aug 2010 - Improved error detection for failure to report missing ASCII files
                            gives [TAE-PRCSTRM] Abnormal process termination; process status code = 11.

        R. J. Bambery, 29 Apr 2011 - Fixed prototype declaration warning with gcc44 compiler
        R. J. Bambery, 19 Jun 2011 - Fixed "warning: array subscript is below array bounds" and
                            other warnings with gcc4.4.4                           
	W. Bunch, 5 Dec 2014 - Added imake and fiddled includes to build in mipl.
.level1
.var inp
ascii input file, IBIS
update file
.var cols
table columns to receive
the data; place names in
order of the data in the
ascii file
.var rtjust
keyword to right justify
alphabetic  column data
.var lead_rm
number of text lines to
remove at front of file
.end
$ Return
$!#############################################################################
$Imake_File:
$ create acopin.imake
#define PROGRAM acopin

#define MODULE_LIST acopin.c

#define MAIN_LANG_C
#define R2LIB
#define USES_ANSI_C
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
$ Return
$!#############################################################################
$Source_File:
$ create acopin.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>         //64-bit edit for NULL
#include <string.h>

#include "ms_defines.h" 
#include "applic.h" 
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "vicmain_c.h"
#include "zifmessage.h"
#include "zmabend.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MXDDWID 2000

/* prototypes  added for 64-bit */

unsigned char ct1 (unsigned char *s);
short int ct2(short int *s) ;
int ct4(int *s);
float ct7(float *s);
double ct8(double *s);

void st1(unsigned char v,unsigned char *s);
void st2(short int v,short int *s);
void st4(int v,int *s);
void st7(float v,float *s);
void st8(double v,double *s);
void lflush(FILE *infile);

/************************************************************************/
/* program acopin                                                       */
/************************************************************************/
/* 00-05 ...alz... initial version                                      */
/* see pdf for history continuation                                     */
/************************************************************************/

int bufsizdefv[6] = {240000,2000000,8000000,20000000,60000000,99000000};

void lflush(infile)
   FILE *infile;
{
   int i; char ch;
   for (i=0;i<10000;i++)
      {
      fscanf(infile,"%c",&ch);
      if (ch=='\n'||ch=='\r') break;
      }
   return;
}

/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1 (unsigned char *s) {
  return(*s);
}

short int ct2(short int *s) {
  return(*s);
}

int ct4(int *s) {
  return(*s);
}

float ct7(float *s) {
  return(*s);
}

double ct8(double *s) {
  return(*s);
}

void st1(unsigned char v,unsigned char *s) {
  *s = v;
  return;
}

void st2(short int v,short int *s) {
  *s = v;
  return;
}

void st4(int v,int *s) {
  *s = v;
  return;
}

void st7(float v,float *s) {
  *s = v;
  return;
}

void st8(double v,double *s) {
  *s = v;
  return;
}

void main44(void)
{
   int      nincol,rt,lead_rm;
   char     infilename[2][255];             //64-bit
   
   double *numoutcol;

   unsigned char *buf=NULL,*nbuf;
   
   int ncol,tablen=0,totrec=0,i,j,k;
   int datcols[100],typ[100],wid[100],totwid[101],ibig;
   int krt,klen,kcp,bufsiz,ototwid[101],ototrec;
   int coldef,unit,ibis,status,parmct,parmdf,irec,rstlen;
   int ind;
   double tmp8;
   char *p,fmtstring[10],rst[MXDDWID];
   char msg[150];
   FILE *infile;

   nincol = 101;
   zifmessage("acopin Fri Dec  5 2014 - wlb");
   
   /* open tae, fetch params */
   
   zvparm("inp",infilename,&parmct,&parmdf,2,254);
   if ((infile = fopen(infilename[0],"r")) == NULL) {
        sprintf (msg,"??E - Cannot find file = %s\n",infilename[0]);
        zmabend(msg);
    }
   for (i=0;i<100;i++) datcols[i] = i+1;
   zvparm("cols",datcols,&nincol,&coldef,100,0);            //20,99);
   rt = zvptst("rtjust");
   zvp("lead_rm",&lead_rm,&parmct);

   for (i=0;i<lead_rm;i++) lflush(infile);
   /* open table */
   status = zvunit(&unit,"inp",2,NULL);                     //64-bit edit
   status = IBISFileOpen(unit,&ibis,IMODE_UPDATE,0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (coldef) nincol = ncol;
    
   /* process widths of fields */
   sprintf (msg,"Number of input cols = %d",nincol);
	zvmessage(msg," ");
   totwid[0] = 0;
   for (i=0;i<nincol;i++) {
        status = IBISColumnGet(ibis,"FORMAT",fmtstring,datcols[i]);
        if (status!=1) IBISSignal(ibis,status,1);
        if (fmtstring[0]=='A') {
            wid[i] = ms_num(&fmtstring[1])+1;
            typ[i] = 0;
	        sprintf (msg,"    Col %d is ASCII width = %d",i+1,wid[i]);
	        zvmessage (msg," ");
        } else {
        	/* If not ascii, force DOUB format */
            status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datcols[i]);
            if (status!=1) IBISSignal(ibis,status,1);
            wid[i] = 8;
            typ[i] = 8;
        }
        totwid[i+1] = totwid[i]+wid[i];
    } //for (i=0;i<nincol;i++)
   /* read all input into contiguous columns */

   ototrec = 0;
   for (ibig=0;ibig<6;ibig++) {
        bufsiz = bufsizdefv[ibig];
        mz_alloc1((unsigned char **)&nbuf,bufsiz,1);
        if (ibig==0) {
	        totrec = bufsiz/totwid[nincol];
	        totrec = (totrec/8)*8;
	        for (i=0;i<=nincol;i++) totwid[i] *= totrec;
	    } else {
	        ototrec = totrec;
	        for (i=0;i<=nincol;i++) {
	            ototwid[i] = totwid[i];
	            totwid[i] /= totrec;
	        }
	        totrec = bufsiz/totwid[i];      /* changed from totrec = bufsiz/totwid[nincol]; */
                                            /* to prevent  "warning: array subscript is below array bounds" */
	        totrec = (totrec/8)*8;
	        for (i=0;i<=nincol;i++) totwid[i] *= totrec;
	        for (j=nincol-1;j>=0;j--) {
	    /*bcopy(&buf[ototwid[j]],&nbuf[totwid[j]],wid[j]*ototrec);*/
	            zmve(1,wid[j]*ototrec,&buf[ototwid[j]],&nbuf[totwid[j]],1,1);
	        }
	    free (buf);
	    }
        buf = nbuf;

        for (i=ototrec;i<totrec;i++)
	        for (j=0;j<nincol;j++) {
	            switch(typ[j])
	            {
	            case 0: {					/* ASCII case */
//                    printf ("case 0\n");
	                fscanf(infile,"%s",rst);
	                if (feof(infile)&&j==0) { tablen = i-1; goto endread; }
	                if (feof(infile)&&j<(nincol-1)) {
			            sprintf (msg,"Case 0 (ASCII): record %d col %d",i,j);
			            zvmessage (msg," ");
			            zmabend("??E Partial record at end");
		            }
	                klen = (int)strlen(rst); krt = 0;
	                if (rt) krt = wid[j]-klen-1;
	                if (krt<0) krt = 0;
	                kcp = wid[j]-1; if (klen<kcp) kcp = klen;
	                for (k=0;k<wid[j]-1;k++)
		                buf[totwid[j]+i*wid[j]+k] = ' ';
	                    buf[totwid[j]+(i+1)*wid[j]-1] = (char)0;
	                    for (k=0;k<kcp;k++)
		                    buf[totwid[j]+i*wid[j]+k+krt] = (unsigned char)rst[k];
	                    if (feof(infile)) { tablen = i; goto endread; }
	                break;
	            }
	            case 8:	{				/* DOUB case */
//                    printf ("case 8 --->\n");
	                ind = fscanf(infile,"%s",rst);
                    if (ind == 1 || ind == -1) {
                    } else {
                        sprintf (msg,"ind on fscanf of %s = %d\n",infilename[0],ind);
                        zvmessage (msg," ");
                        zmabend("??E Input file error");
                    }
	                if (feof(infile)&&j==0) { 
                        tablen = i-1; goto endread; 
                    }
	                if (feof(infile)&&j<(nincol-1)) {
			            sprintf (msg,"Case 8 (DOUBLE): record %d col %d",i,j);
			            zvmessage (msg," ");
			            zmabend("??E Partial record at end");
		            }
	                rstlen = (int)strlen(rst);
	                for (p=rst,k=0;k<rstlen;p++,k++)
	                {
	                    if (*p>='0'&&*p<='9') continue;
	                    if (*p=='.'||*p=='+'||*p=='-'||*p=='e') continue;
	                    if (*p=='E'||*p=='d'||*p=='D') continue;
/*			sprintf (msg,"typ[%d] = %d",j,typ[j]);
			zvmessage (msg," ");
*/
			            sprintf (msg,"     ??E character = %s in numeric col %d  record %d",p,j,i);
			            zvmessage (msg," ");
			            zvmessage ("     ??E Are there blanks in ASCII field?"," ");
	                    zmabend("??E Case 8 (DOUBLE): Non-numeric data in a numeric field");
	                }   
	                sscanf(rst,"%lf",&tmp8);
	                st8(tmp8,(double*)&buf[totwid[j]+i*wid[j]]);             //64-bit edit
	                if (feof(infile)) { tablen = i; goto endread; }
	                break;
                }
                default: {
                    zvmessage ("     ??E Unexpected Case for first switch(typ[j])"," ");
                    zmabend("??E Case Default");
                }
	        } //end switch
	     } // for (j=0;j<nincol;j++)
      } // for (ibig=0;ibig<6;ibig++)   
      endread: tablen++;

   /* write out data */

    sprintf (msg,"Output length is %d records\n\n",tablen);
    zvmessage (msg," ");
//   mz_alloc1((unsigned char **)&numoutcol,tablen,8);      //warning: dereferencing type-punned pointer will break strict-aliasing rules
   mz_alloc1((unsigned char **)&numoutcol,tablen,8);
   status = IBISFileSet(ibis,"nr",(char*)((long)tablen),0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   for (i=1;i<=ncol;i++) {
      k = -1;
      for (j=0;j<nincol;j++) if (datcols[j]==i) k=j;
      if (k>=0) {
	    switch (typ[k])
	        {
	            case 0: {
	                status = IBISColumnWrite(ibis,(char*)&buf[totwid[k]],i,1,tablen);       //64-bit
                    if (status!=1) IBISSignal(ibis,status,1);
                    break;
                }
                case 8: {
	                for (irec=0;irec<tablen;irec++)
	                    numoutcol[irec] = ct8((double*)&buf[totwid[k]+irec*wid[k]]);        //64-bit
	                status = IBISColumnWrite(ibis,(char*)numoutcol,i,1,tablen);
                    if (status!=1) IBISSignal(ibis,status,1);
                    break;
                }
                default: {
                    zvmessage ("     ??E Unexpected Case for second switch(typ[j])"," ");
                    zmabend("??E Case Default");
                }
            }
        } else {
	        status=7;
	    }
     }
   
   /* IBISFileClose required here because nr is changed */
   
   status = IBISFileClose(ibis,NULL);                                   //64-bit
   if (status!=1) IBISSignalU(unit,status,1);
   return;

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Test_File:
$ create tstacopin.pdf
procedure
parm version string def="ibis-2"
parm org string def="column"
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

! Jun 19, 2012 - RJB
! TEST SCRIPT FOR ACOPIN
! Vicar Programs:
!   translog ibis-gen ibis-list
!
! Parameters:
!   version: ibis-1, ibis-2
!   org: column, row
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
refgbl $echo
refgbl $autousage

body
let _onfail="stop"
let $autousage="none"
let $echo="yes"

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! case showing mix of types

ibis-gen xxa version=&version org=&org nc=2 nr=1 +
         format=("FULL","DOUB","A5")

acopin (ct/acopin.dat,xxa) cols=(2,1,3) lead_rm=2

ibis-list xxa 'format


! same data, right justify case

ibis-gen xxa version=&version org=&org nc=2 nr=1 +
         format=("FULL","DOUB","A5")

acopin (ct/acopin.dat,xxa) cols=(2,1,3) lead_rm=2 'rtjust

ibis-list xxa 'format

! case with abrupt end at last line

ibis-gen xxa version=&version org=&org nc=2 nr=1 +
         format=("FULL","DOUB","A5")

acopin (ct/acopin.dat3,xxa) cols=(2,1,3) lead_rm=2

ibis-list xxa 'format

! case with abrupt end at last line, also ends numeric

ibis-gen xxa version=&version org=&org nc=2 nr=1 +
         format=("FULL","DOUB","A5")

acopin (ct/acopin.dat2,xxa) cols=(2,3,1) lead_rm=2

ibis-list xxa 'format

! Report error for missing filea
let $echo="no"
write "*******************************************"
write "-----> SHOULD REPORT acopin.dat4 is missing"
write "*******************************************"
let $echo="yes"
acopin (ct/acopin.dat4,xxa) cols=(2,3,1) lead_rm=2

rm>
ush rm ct
let $echo="no"

end-proc
$!-----------------------------------------------------------------------------
$ create tstacopin.log
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

translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
    ush ln -s /project/test_work/testdata/carto ct
else
    if (aftestdata = "")
    else
    end-if
end-if
let _onfail="goto rm"
ibis-gen xxa version=ibis-2 org=column nc=2 nr=1  +
         format=("FULL","DOUB","A5")
Beginning VICAR task ibis
acopin (ct/acopin.dat,xxa) cols=(2,1,3) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 3
    Col 3 is ASCII width = 6
Output length is 3 records


ibis-list xxa 'format
Beginning VICAR task ibis
 
Number of Rows:3  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:3
+-----------+-----------+-----------
         C:1         C:2         C:3
        FULL        DOUB          A5
+-----------+-----------+-----------
           2        1.10       a    
           4        3.30       bcdef
           6        5.50       wxyz 
ibis-gen xxa version=ibis-2 org=column nc=2 nr=1  +
         format=("FULL","DOUB","A5")
Beginning VICAR task ibis
acopin (ct/acopin.dat,xxa) cols=(2,1,3) lead_rm=2 'rtjust
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 3
    Col 3 is ASCII width = 6
Output length is 3 records


ibis-list xxa 'format
Beginning VICAR task ibis
 
Number of Rows:3  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:3
+-----------+-----------+-----------
         C:1         C:2         C:3
        FULL        DOUB          A5
+-----------+-----------+-----------
           2        1.10           a
           4        3.30       bcdef
           6        5.50        wxyz
ibis-gen xxa version=ibis-2 org=column nc=2 nr=1  +
         format=("FULL","DOUB","A5")
Beginning VICAR task ibis
acopin (ct/acopin.dat3,xxa) cols=(2,1,3) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 3
    Col 3 is ASCII width = 6
Output length is 3 records


ibis-list xxa 'format
Beginning VICAR task ibis
 
Number of Rows:3  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:3
+-----------+-----------+-----------
         C:1         C:2         C:3
        FULL        DOUB          A5
+-----------+-----------+-----------
           2        1.10       a    
           4        3.30       bcdef
           6        5.50       wxyz 
ibis-gen xxa version=ibis-2 org=column nc=2 nr=1  +
         format=("FULL","DOUB","A5")
Beginning VICAR task ibis
acopin (ct/acopin.dat2,xxa) cols=(2,3,1) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
Number of input cols = 3
    Col 2 is ASCII width = 6
Output length is 3 records


ibis-list xxa 'format
Beginning VICAR task ibis
 
Number of Rows:3  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:3
+-----------+-----------+-----------
         C:1         C:2         C:3
        FULL        DOUB          A5
+-----------+-----------+-----------
           5        1.10       a    
           7        3.30       bcdef
           9        5.50       wxyz 
let $echo="no"
*******************************************
-----> SHOULD REPORT acopin.dat4 is missing
*******************************************
acopin (ct/acopin.dat4,xxa) cols=(2,3,1) lead_rm=2
Beginning VICAR task acopin
acopin Fri Dec  5 2014 - wlb
??E - Cannot find file = ct/acopin.dat4

 ** ABEND called **
goto rm
ush rm ct
let $echo="no"
$ Return
$!#############################################################################
