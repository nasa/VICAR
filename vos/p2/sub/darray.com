$!****************************************************************************
$!
$! Build proc for MIPL module darray
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:57
$!
$! Execute by entering:		$ @darray
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
$ write sys$output "*** module darray ***"
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
$ write sys$output "Invalid argument given to darray.com file -- ", primary
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
$   if F$SEARCH("darray.imake") .nes. ""
$   then
$      vimake darray
$      purge darray.bld
$   else
$      if F$SEARCH("darray.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake darray
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @darray.bld "STD"
$   else
$      @darray.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create darray.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack darray.com -mixed -
	-s darray.f zdarray.c -
	-i darray.imake -
	-t tdarray.f tzdarray.c tdarray.imake tdarray.pdf tstdarray.pdf -
	-o darray.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create darray.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE DARRAY(MODE,I,J,N,M,S,D)
C   30 JULY 1993...DDK...PORTED TO UNIX
C   21 OCTOBER 1975...JBS...CHANGES FOR CONVERSION TO 360/OS
C   6/6/71 JBS61     FROM SCIENTIFIC SUBROUTINE PACKAGE
C
C     PURPOSE . . .
C
C     CONVERT DOUBLE PRECISION DATA ARRAY FROM SINGLE TO
C     DOUBLE DIMENSION OR VICE VERSA.  THIS PROGRAM ALSO HAS
C     THE CAPABILITY OF PACKING AND UNPACKING DOUBLE PRECISION
C     DATA THAT ARE DOUBLE DIMENSION.
C     THIS SUBR. IS THE DOUBLE PRECISION ANALOG OF THE
C     IBM SSP ROUTINE ARRAY.  THE DARRAY CALLING SEQUENCE AND
C     VARIABLES ARE IDENTICAL TO THOSE OF ARRAY EXCEPT THAT
C     THE VARIABLES S AND D IN DARRAY ARE TYPE DOUBLE PRECISION.
C
      INTEGER*4 MODE,I,J,N,M,K,L
      REAL*8 S(I,J),D(N,M)
C
C  TEST TYPE OF CONVERSION
C
      IF (MODE .EQ. 1) GO TO 100
      IF (MODE .GT. 1) GO TO 120
C
C  CONVERT FROM PACKED TO UNPACKED STORAGE
C
100      DO 110 L=1,J
         DO 115 K=1,I
            D(I-K+1,J-L+1)=S(I-K+1,J-L+1)
115      CONTINUE
110      CONTINUE
         GO TO 140
C
C  CONVERT FROM UNPACKED TO PACKED STORAGE
C
120      DO 130 L=1,J
         DO 135 K=1,I
            S(K,L)=D(K,L)
135      CONTINUE
130      CONTINUE
140   RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zdarray.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the C Callable Subroutine for the darray.f program */
/*  darray converts a double precision data array from single  */
/*  to double dimension or vice versa.                         */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zdarray(mode,i,j,n,m,s,d)

int mode,i,j,n,m;
double *s, *d;
{
     FTN_NAME2(darray, DARRAY) (&mode,&i,&j,&n,&m,s,d); 
     return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create darray.imake
/* Imake file for VICAR subroutine DARRAY */

#define SUBROUTINE darray

#define MODULE_LIST darray.f zdarray.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tdarray.f
C TDARRAY IS A PROGRAM TO TEST THE SUBROUTINE
C DARRAY.
C
        SUBROUTINE TDARRAY()

	INTEGER*4 MODE,I,J,M,N,K,ID,IS
        REAL*8 S1(5,5), D1(10,10)
        CHARACTER*35 MS1
        CHARACTER*100 MS2,MS3
C FIRST TEST MODE 1 WITH 10 X 10
	IS = 5
        ID = 10
	K=0
	DO 5 I = 1,IS
	DO 6 J = 1,IS
	S1(I,J) = (J+I)/1.0
6       CONTINUE
5       CONTINUE
	DO 7 I = 1,ID
	DO 8 J = 1,ID
	D1(I,J) = 0.0
8       CONTINUE
7       CONTINUE
	MODE = 1
	J=IS
	I = IS
	N = ID 
	M = ID
	CALL DARRAY(MODE,I,J,N,M,S1,D1)
        WRITE(MS1,2000)
2000	FORMAT(' MODE 1 :.')
        CALL XVMESSAGE(MS1, ' ')
	DO 15 K = 1,IS
	WRITE(MS2,100) S1(1,K),S1(2,K),S1(3,K),S1(4,K),S1(5,K)
        CALL XVMESSAGE(MS2, ' ')
15      CONTINUE
        WRITE(MS1,6000)
        CALL XVMESSAGE(MS1, ' ')
	DO 25 K = 1,ID
	WRITE(MS2,100) D1(1,K),D1(2,K),D1(3,K),D1(4,K),D1(5,K)
        WRITE(MS3,100) D1(6,K),D1(7,K),D1(8,K),D1(9,K),D1(10,K)
        CALL XVMESSAGE(MS2, ' ')
        CALL XVMESSAGE(MS3, ' ')
25      CONTINUE
100     FORMAT(5E12.4)
C NEXT MODE 2 WITH A 10 X 10
	IS = 5
        ID = 10
	K=0
	DO 105 I = 1,IS
	DO 106 J = 1,IS
	S1(I,J) = 0.0
106     CONTINUE
105     CONTINUE
	DO 107 I = 1,ID
	DO 108 J = 1,ID
	D1(I,J) = (J+I)/1.0
108     CONTINUE
107     CONTINUE
	MODE = 2
	J=IS
	I = IS
	N = ID 
	M = ID
	CALL DARRAY(MODE,I,J,N,M,S1,D1)
        WRITE(MS1,3000)
3000	FORMAT(' MODE 2: ')
        CALL XVMESSAGE(MS1, ' ')
	DO 115 K = 1,IS
	WRITE(MS2,100) S1(1,K),S1(2,K),S1(3,K),S1(4,K),S1(5,K)
        CALL XVMESSAGE(MS2, ' ')
115     CONTINUE
        WRITE(MS1,6000)
        CALL XVMESSAGE(MS1, ' ')
	DO 125 K = 1,ID
	WRITE(MS2,100) D1(1,K),D1(2,K),D1(3,K),D1(4,K),D1(5,K)
        WRITE(MS3,100) D1(6,K),D1(7,K),D1(8,K),D1(9,K),D1(10,K)
        CALL XVMESSAGE(MS2, ' ')
        CALL XVMESSAGE(MS3, ' ')
125     CONTINUE
6000    FORMAT(' D ARRAY: ')
        END
$!-----------------------------------------------------------------------------
$ create tzdarray.c
/* tzdarray is a program to test the C Callable Subroutine for DARRAY */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     int mode, i, j, m, n, is, id, k, a, b;
     double s1[5][5],d1[10][10];
     char ms1[400];

sprintf(ms1,"Test the C interface");
zvmessage(ms1," ");

/* Test Mode 1 */

     id=10;
     is=5;
     k=0;

     for (i=0; i<is; i++)
     {
          for (j=0; j<is; j++)
          {
               s1[j][i]=(double)(j+i+2.0);
          }
     }

     for (n=0; n<id; n++)
     {
          for (m=0; m<id; m++)
          {
               d1[m][n]=(double)(0.0);
          }
     }

     mode=1;
     j=is;
     i=is;
     n=id;
     m=id;

     zdarray(mode,i,j,n,m,&s1[0][0],&d1[0][0]);
     sprintf(ms1," mode 1: ");
     zvmessage(ms1," ");

     for (a=0; a<is; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",s1[0][a],s1[1][a],s1[2][a],s1[3][a],s1[4][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"d array: ");
     zvmessage(ms1," ");
     for (a=0; a<id; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[0][a],d1[1][a],d1[2][a],d1[3][a],d1[4][a]);
         zvmessage(ms1," ");      
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[5][a],d1[6][a],d1[7][a],d1[8][a],d1[9][a]);
         zvmessage(ms1," ");      
     }


/* Test Mode 2 */

     id=10;
     is=5;
     k=0;

     for (i=0; i<is; i++)
     {
          for (j=0; j<is; j++)
          {
               s1[j][i]=(double)(0.0);
          }
     }

     for (n=0; n<id; n++)
     {
          for (m=0; m<id; m++)
          {
               d1[m][n]=(double)(m+n+2.0);
          }
     }

     mode=2;
     j=is;
     i=is;
     n=id;
     m=id;

     zdarray(mode,i,j,n,m,&s1[0][0],&d1[0][0]);
     sprintf(ms1," mode 2: ");
     zvmessage(ms1," ");

     for (a=0; a<is; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",s1[0][a],s1[1][a],s1[2][a],s1[3][a],s1[4][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"d array: ");
     zvmessage(ms1," ");
     for (a=0; a<id; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[0][a],d1[1][a],d1[2][a],d1[3][a],d1[4][a]);
         zvmessage(ms1," ");      
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[5][a],d1[6][a],d1[7][a],d1[8][a],d1[9][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"Test the FORTRAN interface");
     zvmessage(ms1," ");

     FTN_NAME(tdarray)();

}
$!-----------------------------------------------------------------------------
$ create tdarray.imake

/* Imake file for Test of VICAR subroutine TDARRAY */

#define PROGRAM tdarray

#define MODULE_LIST tdarray.f tzdarray.c

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_FORTRAN
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tdarray.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstdarray.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "The test programs outputs differ because of the format restrictions."
tdarray
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create darray.hlp
1 DARRAY

  DARRAY packs and unpacks double precision data arrays.

  FORTRAN Calling Sequence: 
                          INTEGER*4 MODE,I,J,N,M
                          REAL*8 S(DIMX,DIMY), D(DIMX,DIMY)
                          CALL DARRAY(MODE,I,J,N,M,S,D)
  C Calling Sequence: 
                          int mode,i,j,n,m
                          double s(dimx,dimy), d(dimx,dimy)
                          darray(mode,i,j,n,m,s,d)


             mode - specifies whether to pack or unpack the arrays
                s - the array that is used for packing
                d - the array that is used for unpacking
              i,j - specifies the dimension for the s array
              n,m - specifies the dimension for the d array
           
             Note:  FORTRAN treats things in column-major order, while C
                    uses row-major order.  The C subscripts in the array
                    needs to be reversed prior to to calling the sequence.

                    The dimensions of the arrays should be specified by
                    the user.

             

History

  Original Programmer: JBS October 21, 1975
  Current Cognizant Programmer: Damon D. Knight August 5, 1993
  Source Language: FORTRAN
  Ported to Unix: August 5,1993

2 Operation

  DARRAY is the double precision analog of the IBM SSP routine ARRAY.
  The DARRAY calling sequence and variables are identical to those of
  ARRAY except that the variables S and D in DARRAY are type double
  precision.  It can be used to pack and unpack data in the arrays.
$ Return
$!#############################################################################
