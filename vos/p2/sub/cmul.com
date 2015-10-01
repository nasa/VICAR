$!****************************************************************************
$!
$! Build proc for MIPL module cmul
$! VPACK Version 1.7, Tuesday, May 31, 1994, 15:35:49
$!
$! Execute by entering:		$ @cmul
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
$ write sys$output "*** module cmul ***"
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
$ write sys$output "Invalid argument given to cmul.com file -- ", primary
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
$   if F$SEARCH("cmul.imake") .nes. ""
$   then
$      vimake cmul
$      purge cmul.bld
$   else
$      if F$SEARCH("cmul.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cmul
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cmul.bld "STD"
$   else
$      @cmul.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cmul.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cmul.com -
	-s cmul.f zcmul.c -
	-i cmul.imake -
	-t tcmul.f tzcmul.c tcmul.imake tcmul.pdf tstcmul.pdf -
	-o cmul.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cmul.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE CMUL(N,R1,I,R2,J,ROUT)
C-------THIS ROUTINE REPLACES THE OLD BAL VERSION FROM 1970
C-------IT DOES A COMPLEX MULTIPLICATION OF ARRAYS
	COMPLEX*8 R1(N),R2(N),ROUT(N),A,B
C
	DO 10 L=1,N
           A = R1(L)
	   B = R2(L)
	   IF (I.EQ.1) THEN
              A = CONJG(A)
           END IF
	   IF (J.EQ.1) THEN
              B = CONJG(B)
           END IF
	   ROUT (L) = A*B
10      continue

	RETURN
	END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zcmul.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/****************************************************************************
     C-Callable Version: zcmul - perform complex multiplication of arrays
*****************************************************************************/

/* ***************************************************************************
    This function performs the same processing as the Fortran subroutine
    'CMUL'.  Since 'C' does not support complex math, the 'C' function
    'zcmul' emulates the fortran subroutine.  The original fortran 
    subroutine listing is duplicated in the following lines of code.
     
	FORTRAN SUBROUTINE CMUL(N,R1,I,R2,J,ROUT)

C-------CMUL PERFORMS A COMPLEX MULTIPLICATION OF ARRAYS

	COMPLEX*8 R1(N),R2(N),ROUT(N),A,B

	DO 10 L=1,N
           A = R1(L)
	   B = R2(L)
	   IF (I.EQ.1) A = CONJG(A)
	   IF (J.EQ.1) B = CONJG(B)
	   ROUT (L) = A*B
10      continue
	RETURN
	END

   The following logic and 'C' code were developed to emulate the complex
   math function for 'multiply'.

        if   z1 = (x1,y1)
        and  z2 = (x2,y2)

then 

        z1 + z2 = (x1+x2,y1+y2)

        z1 - z2 = (x1-x2,y1-y2)

        z1 * z2 = (x1*x2-y1*y2,x1*y2+x2*y1)

                  (x1*x2+y1*y2,x2*y1-x1*y2
        z1 / z2 =  ----------- -----------
                  (x2*x2+y2*y2 x2*x2+y2*y2)   

*************************************************************************** */

typedef struct {
      float real;
      float imaginary;
}complex;

void zcmul( n, r1, i, r2, j, results)
   complex *r1[1], *r2[1], *results[1];
   int      n, i, j;
{
complex a, b, c;
void    *r1_ptr, *r2_ptr, *r3_ptr;
int     ii;

   r1_ptr = (complex *)r1;
   r2_ptr = (complex *)r2;
   r3_ptr = (complex *)results;

   for (ii = 0; ii < n; ii++) {
      a = *((complex *)r1_ptr + ii);
      b = *((complex *)r2_ptr + ii);
      if (i == 1) {
         a.imaginary = a.imaginary * -1.0;
      }
      if (j == 1) {
         b.imaginary = b.imaginary * -1.0;
      }
      c.real = (a.real * b.real) - (a.imaginary * b.imaginary);
      c.imaginary = (a.real * b.imaginary) + (b.real * a.imaginary);
      *((complex *)r3_ptr + ii) = c;
   }
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cmul.imake
/* Imake file for VICAR subroutine cmul */

#define SUBROUTINE cmul

#define MODULE_LIST cmul.f zcmul.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tcmul.f
	include 'VICMAIN_FOR'

	subroutine main44


	complex*8 c1(3)/(1.,5.),(2.,2.),(3.,.5)/
	complex*8 c2(3)/(.5,2.),(1.,5.),(2.,1.)/,c3(3)
        integer n, i, j
        character*132 string
        data n /0/, i /0/, j /0/

        call xvmessage (' ',' ')
        call xvmessage 
     &   ('Perform test cases in C to test c interface: zcmul', ' ')
        call xvmessage (' ',' ')
        call tzcmul

        call xvmessage (' ',' ')
        call xvmessage 
     &   ('Repeat test cases using fortran interface: cmul', ' ')
        call xvmessage (' ',' ')

	n=3
	i=0
	j=0
        write (string,100) c1(1),c1(2),c1(3)
  100   format ('inl    '6E12.4)
        call xvmessage (string, ' ')

        write (string,110) c2(1),c2(2),c2(3)
  110   format ('in2    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for both not conjugated',' ')

	call cmul(n,c1,i,c2,j,c3)

        write (string,120) c3(1),c3(2),c3(3)
  120   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for first conjugated',' ')

	i = 1
	call cmul(n,c1,i,c2,j,c3)

        write (string,130) c3(1),c3(2),c3(3)
  130   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for second conjugated',' ')

	i=0
	j=1
	call cmul(n,c1,i,c2,j,c3)

        write (string,140) c3(1),c3(2),c3(3)
  140   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for both conjugated',' ')
	i=1

	call cmul(n,c1,i,c2,j,c3)

        write (string,150) c3(1),c3(2),c3(3)
  150   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	return
	end
$!-----------------------------------------------------------------------------
$ create tzcmul.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/****************************************************************************
    Test 'C' Version of cmul ... perform complex multiplication of arrays
*****************************************************************************/

typedef struct  {
      float real;
      float imaginary;
}complex;

void FTN_NAME(tzcmul)() 
{
   complex c1[3], c2[3], result[3];
   int     n, i, j;
   char    string[132];

   c1[0].real = 1.0;
   c1[1].real = 2.0;
   c1[2].real = 3.0;
   c1[0].imaginary = 5.0;
   c1[1].imaginary = 2.0;
   c1[2].imaginary = 0.5;

   c2[0].real = 0.5;
   c2[1].real = 1.0;
   c2[2].real = 2.0;
   c2[0].imaginary = 2.0;
   c2[1].imaginary = 5.0;
   c2[2].imaginary = 1.0;

   result[0].real = 0.0;
   result[1].real = 0.0;
   result[2].real = 0.0;
   result[0].imaginary = 0.0;
   result[1].imaginary = 0.0;
   result[2].imaginary = 0.0;

   n = 3;
   i = 0;
   j = 0;

   (void) sprintf (string,"in1    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             c1[0].real, c1[0].imaginary,
             c1[1].real, c1[1].imaginary,
             c1[2].real, c1[2].imaginary);
   zvmessage (string, 0);

   (void) sprintf (string,"in2    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             c2[0].real, c2[0].imaginary,
             c2[1].real, c2[1].imaginary,
             c2[2].real, c2[2].imaginary);
   zvmessage (string, 0);

/* #1 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for both not conjugated");
   zvmessage (string,0);

   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #2 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for first conjugated");
   zvmessage (string,0);

   i = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #3 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for second conjugated");
   zvmessage (string,0);

   i = 0;
   j = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #4 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for both conjugated");
   zvmessage (string,0);

   i = 1;
   j = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

}
$!-----------------------------------------------------------------------------
$ create tcmul.imake
/* Imake file for Test of VICAR subroutine cmul */

#define PROGRAM tcmul

#define MODULE_LIST tcmul.f tzcmul.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
/* Reference to LIB_LOCAL commented-out for delivery */
/* #define LIB_LOCAL */  
#define LIB_MATH77
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tcmul.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstcmul.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write "four tests will be run which will each print"
write "results based on the same input data"
tcmul
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cmul.hlp
1 CMUL

  CMUL multiplies two complex arrays together. 

  CMUL provides an interface for both FORTRAN and for C programs.  The 
  Fortran interface multiplies two complex*8 arrays together; the C interface 
  multiplies two complex structures together, where each structure contains
  a real and an imaginary part.
  

  Calling Sequence:

    FORTRAN calling sequence:  CALL CMUL(NUM,C1,I,C2,J,C3)

    Arguments: NUM - INPUT - NUMBER OF ELEMENTS TO MULTIPLY
               C1  - INPUT - COMPLEX*8 ARRAY
               I   - INPUT - =1 means CONJUGATE C1
                             =0 means don't
               C2  - INPUT - COMPLEX*8 ARRAY
               J   - INPUT - =1 means CONJUGATE C2
                             =0 means don't
               C3  - OUTPUT- RESULT COMPLEX*8 ARRAY

    C calling sequence:  cmul (NUM,C1,I,C2,J,C3)

    Arguments: Num - INPUT - Number of elements to multiply
               C1  - INPUT - Complex structure 
                             typedef struct {
                               float real;
                               float imaginary;
                             } COMPLEX;
               I   - INPUT - =1 means CONJUGATE C1
                             =0 means don't
               C2  - INPUT - Complex structure 
                             typedef struct {
                               float real;
                               float imaginary;
                             } COMPLEX;
               J   - INPUT - =1 means CONJUGATE C2
                             =0 means don't
               C3  - OUTPUT- Result complex structure 
                             typedef struct {
                               float real;
                               float imaginary;
                             } COMPLEX;

2 History

  Original Programmer: T. RINDFLEISCH, 24 JAN 1970
  Current Cognizant Programmer: C AVIS
  Source Language: FORTRAN & C
  Made portable for UNIX ... J. Turner (CRI) 05 Sep 1994

2 Operation

  CMUL will multiply two complex arrays using the rules
  of complex arithmetic.  Either of the input arrays may
  be conjugated before multiplication.

  Depending on the two flags I and J, the subroutine performs:
               c3 = c1 * c2    or
               c3 = conjg(c1) * c2  or
               c3 = c1 * conjg(c2)  or
               c3 = conjg(c1) * conjg(c2)

2 Arguments
  
  Fortran Arguments:
    NUM - INTEGER*4 - NUMBER OF ARRAY ELEMENTS TO MULTIPLY
    C1  - COMPLEX*8 - ARRAY TO BE MULTIPLIED
    I   - INTEGER*4 - FLAG INDICATING CONJUGATION OF C1
    C2  - COMPLEX*8 - ARRAY TO BE MULTIPLIED
    J   - INTEGER*4 - FLAG INDICATING CONJUGATION OF C2
    C3  - COMPLEX*8 - RESULTING PRODUCT ARRAY

  C Arguments:
    NUM - int       - NUMBER OF ARRAY ELEMENTS TO MULTIPLY
    C1  - Complex   - Structure containing real & imaginary parts
          Structure   to be multiplied.
    I   - int       - FLAG INDICATING CONJUGATION OF C1
    C2  - Complex   - Structure containing real & imaginary parts
          Structure   to be multiplied.
    J   - int       - FLAG INDICATING CONJUGATION OF C2
    C3  - Complex   - Structure containing results of the multiplied 
          Structure   real & imaginary parts.

$ Return
$!#############################################################################
