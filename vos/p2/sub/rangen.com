$!****************************************************************************
$!
$! Build proc for MIPL module rangen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:32:49
$!
$! Execute by entering:		$ @rangen
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
$ write sys$output "*** module rangen ***"
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
$ write sys$output "Invalid argument given to rangen.com file -- ", primary
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
$   if F$SEARCH("rangen.imake") .nes. ""
$   then
$      vimake rangen
$      purge rangen.bld
$   else
$      if F$SEARCH("rangen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rangen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rangen.bld "STD"
$   else
$      @rangen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rangen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rangen.com -mixed -
	-s rangen.c -
	-i rangen.imake -
	-t trangen.f tzrangen.c trangen.imake tzrangen.imake trangen.pdf -
	   tzrangen.pdf tstrangen.pdf -
	-o rangen.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rangen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*------------------------------------------------------------------
 * C version of rangen
 *
 * this subroutine is to be called by fortran using:
 *     CALL rangen(seed,rand_num)
 * where seed is an integer:
 *          0 > seed     < 714025
 *   and    0 > rand_num < 1.0
 *
 *---------------------------------------------------------------- */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <math.h>
#include <stdio.h>

#define M 714025
#define IA 1366
#define IC 150889

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2 (rangen, RANGEN) (idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
 zrangen(idum,rand_num);
  return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


zrangen(idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
  static long iy,ir[98];
  static int iff=0;
  int j;
  char msg[80];

  j=*idum;
  *idum%=(1<<24); /* Take modulo 24, to obtain seed between 0 and 2**24-1 */
                  /* Since mod function, %, in C behaves like a reminder  */
                  /* function, negative value of idum will still cause    */
                  /* mod function to return abs value between the range.  */
  if(abs(j) > ((1<<24)-1)) {
    sprintf(msg,"RANGEN::: Random Seed value of %d adjusted to %d",j,*idum);
    zvmessage(msg,0);
  }
  if(*idum<0 || iff==0)
  {
    iff=1;
    if ((*idum=(IC - (*idum)) % M) < 0) *idum= -(*idum);
    for (j=1;j<97;j++)
    {
      *idum=(IA*(*idum)+IC) % M;
      ir[j]=(*idum);
    }
    *idum=(IA*(*idum)+IC) % M;
    iy=(*idum);
  }
  j=1 + 97.0*iy/M;
  if (j>97||j<1) 
  {
     sprintf(msg,"RANGEN: This cannot happen.");
     zvmessage(msg,"");
  }
  iy=ir[j];
  *idum=(IA*(*idum)+IC) % M;
  iy=(*idum);
  *rand_num= (float) iy/M; 
}
  
  
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rangen.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY rangen

   To Create the build file give the command:

	$ vimake rangen                     (VMS)
   or
	% vimake rangen                     (Unix)


*************************************************************************/

#define SUBROUTINE rangen

#define MODULE_LIST rangen.c

#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
$Test_File:
$ create trangen.f
C This is the Fortran routine to test the subroutine rangen

        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 seed
        character*100 msg
        real*4 rand_num

        seed = 1073741969
	do 1001 j=1,25
          call rangen(seed, rand_num )
          write(msg,100) seed, rand_num
          call xvmessage(msg,' ')
 100      format(I8, '   ',F16.14)
 1001   continue
	end
$!-----------------------------------------------------------------------------
$ create tzrangen.c
#include <stdio.h>
#include "vicmain_c"

/* "C" test routine to test the rangen subroutine. */
void main44()
{
 unsigned long seed;  /* input and returned seed */ 
 float rand_num;        /* returned random number */
 char string[80];
 int i;
 seed = 1073741969;
 for (i=1; i<25 ; i++ )
 { 
    zrangen(&seed,&rand_num);
    sprintf(string,"%8d    %15.14f\n",seed,rand_num);
    if ( ( i< 51) || (i> 49974) )
      { 
        sprintf(string, "%8d    %15.14f", seed,rand_num);
        zvmessage(string,"");
      } 
 } 
}
$!-----------------------------------------------------------------------------
$ create trangen.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM trangen

   To Create the build file give the command:

	$ vimake trangen                     (VMS)
   or
	% vimake trangen                     (Unix)

*************************************************************************/

#define PROGRAM trangen

#define MODULE_LIST trangen.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL FOR TESTING WITH GET_SECONDS ONLY DISABLE DURING DELIVER. */
$!-----------------------------------------------------------------------------
$ create tzrangen.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM tzrangen

   To Create the build file give the command:

	$ vimake tzrangen                     (VMS)
   or
	% vimake tzrangen                     (Unix)

*************************************************************************/

#define PROGRAM tzrangen

#define MODULE_LIST tzrangen.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL FOR TESTING WITH GET_SECONDS ONLY DISABLE DURING DELIVER. */

$!-----------------------------------------------------------------------------
$ create trangen.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tzrangen.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstrangen.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
trangen
tzrangen
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create rangen.hlp
1 rangen
 
  PURPOSE 

       Subroutine to generate sequences  of random numbers.
       The program is not machine dependent and will always
       generate  the same random number sequence from the same
       starting seed.  

  Fortran Calling Sequence:  CALL RANGEN(SEED,RANDOM_NUMBER)
  C Calling sequence:          zrangen(&seed,&random_number)

2 History

  Original Programer: Randy Schenk (CRI) (1-July-1994)
  Current Cognizant Programmer: 
  Revisions:
             Removed LIB_LOCAL as per FR85768

2 Operation
  
  Gereates a random number form a given seed


2 Arguments

  SEED is an input integer and an output random number integer:
  The value of SEED will be moded with 2**24 to adjust it in
  the following range.
			0 < seed < 16777215

  RANDOM_NUMBER is an ouput random number:
                        0 < random_number < 1

$ Return
$!#############################################################################
