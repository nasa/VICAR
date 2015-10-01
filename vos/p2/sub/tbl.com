$!****************************************************************************
$!
$! Build proc for MIPL module tbl
$! VPACK Version 1.9, Monday, December 07, 2009, 16:37:43
$!
$! Execute by entering:		$ @tbl
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
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module tbl ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tbl.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("tbl.imake") .nes. ""
$   then
$      vimake tbl
$      purge tbl.bld
$   else
$      if F$SEARCH("tbl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tbl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tbl.bld "STD"
$   else
$      @tbl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tbl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tbl.com -mixed -
	-s tbl.mar ztbl.c tbl.c -
	-i tbl.imake -
	-t tztbl.c ttbl.f ttbl.imake ttbl.pdf tsttbl.pdf -
	-o tbl.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tbl.mar
$ DECK/DOLLARS="$ VOKAGLEVE"
	.psect tbl
	.title tbl
;
;  83-7-14 ...LWK..
;
;  Vicar1 subroutine, replaces equivalent VICAR routine.
;
;  Fortran call:   CALL TBL(BUF,TAB,N)
;   BUF = byte buffer to be translated
;   TAB = 256-byte lookup table
;   N   = # of elements in BUF
;
	.entry	tbl,^m<r2,r3,r4,r5>
;
	movtc	@12(ap),@4(ap),#0,@8(ap),@12(ap),@4(ap)
;
	ret
	.end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ztbl.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztbl - replaces values in an array by means of   */
/* a table lookup.                                                      */
/************************************************************************/

void ztbl( buf, tab, n)
unsigned char *buf;                   /* array of values to be modified */
unsigned char *tab;	              /* lookup table of 256 elements */
int n;                                /* number of bytes in buf */

{
FTN_NAME(tbl)( buf, tab, &n);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tbl.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This program replaces the values in an array by means of a table    */
/*  lookup.  This program is used on the UNIX system because the        */
/*  original subroutine was written in assembly language and is not     */
/*  compilable in UNIX.                                                 */    

/*  Original Programmer: Damon D. Knight  1993-08-11                    */
/*  Current Cog. Progr.: Damon D. Knight  1993-08-11                    */

#include "xvmaininc.h"
#include "ftnbridge.h"

static void ztbl(unsigned char *buf, unsigned char *tab, int n );

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(tbl, TBL) ( buf, tab, n )
unsigned char *buf, *tab;
int *n;
{
     ztbl(buf, tab, *n);
     return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

static void ztbl(unsigned char *buf, unsigned char *tab, int n )
{
     int i, val;
     for (i=0; i<n; i++)
     {
          val= *buf;
          *buf = *(tab+val);
          buf++;
     }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tbl.imake
/* Imake file for VICAR subroutine TBL */

#define SUBROUTINE tbl

#if VMS_OS
#define MODULE_LIST tbl.mar ztbl.c
#define CLEAN_OTHER_LIST tbl.c 
#else 
#define MODULE_LIST tbl.c 
#define CLEAN_OTHER_LIST tbl.mar ztbl.c 
#endif 

#define P2_SUBLIB

#define USES_C
#if VMS_OS
#define USES_MACRO
#endif 
$ Return
$!#############################################################################
$Test_File:
$ create tztbl.c
/* tztbl is the C program that test the tbl subroutine.  */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     unsigned char buf[10], tab[256];
     char  ms1[100];
     int k,n;

     sprintf(ms1,"Test the C interface");
     zvmessage(ms1," ");

     for (k=0; k<256; k++)
     {
          tab[k]=k;
     }

     tab[75]=20;      /*  75 => 20 */
     tab[125]=50;     /* 125 => 50 */
     tab[250]=90;     /* 250 => 90 */

     for (k=0; k<10; k++)
     {
          buf[k] = (k+1)*25;
     }

     n=10;

     ztbl(buf, tab, n);

     sprintf(ms1,"  %X  %X  %X  %X  %X  %X  %X  %X  %X  %X",buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6],buf[7],buf[8],buf[9]);
     zvmessage(ms1," ");

     sprintf(ms1,"Test the FORTRAN interface");
     zvmessage(ms1," ");

     FTN_NAME(ttbl)();

}
$!-----------------------------------------------------------------------------
$ create ttbl.f
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE TAB                                  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE TTBL()
      INCLUDE 'fortport'

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B(10)               !BYTE ARRAY
      BYTE       TAB(256)            !LOOKUP TABLE
      INTEGER*4  I,K                 !INITIALIZATION VARIABLES

      DO I=1,256          !INITIALIZE THE LOOKUP TABLE
         K=I-1
         TAB(I) = INT2BYTE(K)
      ENDDO

      TAB(76)  = INT2BYTE(20)    !DN = 75 => 20
      TAB(126) = INT2BYTE(50)    !DN = 125 => 50
      TAB(251) = INT2BYTE(90)    !DN = 250 => 90

      DO I=1,10           !INITIALIZE THE DATA ARRAY
         K=I*25
         B(I) = INT2BYTE(K)
      ENDDO

      K=10

      CALL TBL(B, TAB, K)
      WRITE(CARD, 100) (B(I),I=1,10)
      CALL XVMESSAGE(CARD,' ')

      RETURN
100   FORMAT(10Z4.2)
      END
$!-----------------------------------------------------------------------------
$ create ttbl.imake
/* Imake file for test of VICAR subroutine tbl */

#define PROGRAM ttbl

#define MODULE_LIST tztbl.c ttbl.f

#define MAIN_LANG_C
#define TEST

#define FTNINC_LIST fortport

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create ttbl.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tsttbl.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
ttbl
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create tbl.hlp
1 TBL

PURPOSE: Replaces the values in an array by means of a table lookup

FORTRAN Calling Sequence:  
                           BYTE BUF[DIM], TAB[256]
                           INTEGER*4 N
                           CALL TBL(BUF, TAB, N)

      C Calling Sequence:  
                           unsigned char *buf, tab[256];
                           int n;
                           ztbl(buf, tab, n);

PARAMETERS:

   BUF  = (in/out) array of values to be modified.
   TAB  = (input)  lookup table of 256 elements.
   N    = (input)  Number of bytes in BUF. 

2 NOTES

HISTORY

  Original Programmer: G.M. Yagi    1977-01-16
  Current Cog Progr:    D.D. Knight
  Converted to Vax by:  L.W. Kamp   1983-07-14
  Ported to Unix: D.D. Knight  1993-8-6

DESCRIPTION

  This MACRO routine provides a function to replace the value in a 
  byte array BUF with a new value as determined by the table TAB. 
$ Return
$!#############################################################################
