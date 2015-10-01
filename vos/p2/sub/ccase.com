$!****************************************************************************
$!
$! Build proc for MIPL module ccase
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:32
$!
$! Execute by entering:		$ @ccase
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
$ write sys$output "*** module ccase ***"
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
$ write sys$output "Invalid argument given to ccase.com file -- ", primary
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
$   if F$SEARCH("ccase.imake") .nes. ""
$   then
$      vimake ccase
$      purge ccase.bld
$   else
$      if F$SEARCH("ccase.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccase
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccase.bld "STD"
$   else
$      @ccase.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccase.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccase.com -mixed -
	-s ccase.c uprcase.f -
	-i ccase.imake -
	-t tccase.f tzccase.c tccase.imake tccase.pdf tstccase.pdf -
	-o ccase.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccase.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

/*  Values of flag - type of case conversion to perform */

#define UPPER  1
#define LOWER -1
#define SWAP   0
void zccase(char* str, int flag, int max);
void zuprcase(char *str);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(ccase, CCASE) (char *str, int *flag, int *max, ZFORSTR_PARAM)

#if 0
char *str;     /* string on which to do case conversion */
int *flag;     /* type of case conversion to perform    */
int *max;      /* maximum number of characters in str. However,
                  processing will cease on encoutering a 0 (null)
                  or after the Fortran length of the CHARACTER string
                  is reached.
                  If max < 0, processing continues until a 0 (null) is
                  encountered or until the Fortran length of the 
                  CHARACTER string is reached.
                  Thus, max may be set to -1 for a Fortran CHARACTER string.*/
#endif
{
   ZFORSTR_BLOCK
   char *c_string;
   int length, maxl;

   zsfor2len(length, str, &str, 3, 1, 1, max);       /* 3 args for ccase   */
   c_string = (char *)calloc(1,(length+1));	     /* str is 1st arg,  */
   zsfor2c(c_string, length, str, &str, 3, 1, 1,max);/* str is 1st string  */

   maxl = *max;
   if (length < maxl)    maxl = length;

   zccase( c_string, *flag, maxl );

   zsc2for( c_string, maxl, str, &str, 3, 1, 1, max); /* 3 args for ccase   */
   free (c_string);

}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zccase(char* str, int flag, int max)
#if 0
  char *str;	/* string on which to do case conversion */
  int flag;	/* type of case conversion to perform */
  int max;	/* maximum number of characters in str.  However, */
		/* processing will stop if a NULL is encountered. */
#endif
{
  int i, maxl;

/*  ==================================================================  */

 maxl = max;   /*  Negative max means keep going until NULL  */
 if (maxl < 0)  maxl = strlen(str);

 switch( flag) {
 case UPPER:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) 
		     str[i] = toupper(str[i]);
		break;

 case LOWER:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) 
		     str[i] = tolower(str[i]);
		break;

 case SWAP:	
 default:	for ( i = 0; i < maxl && str[i] != '\0'; ++i) {
		     if ( islower(str[i]) )         str[i] = toupper(str[i]);
		     else if ( isupper(str[i]) )    str[i] = tolower(str[i]);
		}
		break;                  
 }
}
/************************************************************************/
/* C-Callable short cut for converting null-terminated string to upper case.*/
/************************************************************************/

void zuprcase(char *str)
{
  zccase( str, UPPER, -1);   /*  -1 means convert the whole string  */
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create uprcase.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE UPRCASE(STR)
c   Short cut for converting whole argument string to upper case.

      CHARACTER*(*) STR

      CALL CCASE( STR, 1, -1)  ! -1 means convert the whole string
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccase.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY ccase

   To Create the build file give the command:

	$ vimake ccase                     (VMS)
   or
	% vimake ccase                     (Unix)


*************************************************************************/

#define SUBROUTINE ccase

#define MODULE_LIST ccase.c uprcase.f

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tccase.f
      INCLUDE 'VICMAIN_FOR'
c  test subroutine CCASE

        SUBROUTINE MAIN44
	implicit integer (a-z)

	character*12 c
        data c/'a#*defgHIJKL'/
	character*8  s
        data s/'a2 defgh'/

	call xvmessage('Original string:'//c, ' ')
	call ccase(c,0,-1)
	call xvmessage('After reversing case:'//c, ' ')
	call ccase(c,1,12)
	call xvmessage('In upper case:'//c, ' ')
	call ccase(c,-1,-1)
	call xvmessage('In lower case:'//c, ' ')
	call uprcase(c)
	call xvmessage('In upper case again:'//c, ' ')

	call xvmessage('Second string:'//s, ' ')
	call ccase(s,0,5)
	call xvmessage('After reversing case of first five chars:'//s,
     .                  ' ')
	call ccase(s,1,12)      ! Make sure ccase can handles max > length.
	call xvmessage('In upper case:'//s, ' ')
	call ccase(s,-1,-1)
	call xvmessage('In lower case:'//s, ' ')
	call uprcase(s)
	call xvmessage('In upper case again:'//s, ' ')

c  Repeat test cases in C to test C interface: zccase

      CALL XVMESSAGE('REPEAT SOME TESTS FROM C LANGUAGE',' ')
      call tzccase

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzccase.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzccase)()

{
 static char c[]= "a#*defgHIJKL";

	zvmessage("Original string:", "");
	zvmessage(c, "");
	zccase(c,0,-1);
	zvmessage("After reversing case:", "");
	zvmessage(c, "");
	zccase(c,1,12);
	zvmessage("In upper case:", "");
	zvmessage(c, "");
	zccase(c,-1,-1);
	zvmessage("In lower case:", "");
	zvmessage(c, "");
	zuprcase(c);
	zvmessage("In upper case again:", "");
	zvmessage(c, "");

}
$!-----------------------------------------------------------------------------
$ create tccase.imake
/* Imake file for Test of VICAR subroutine ccase */

#define PROGRAM tccase

#define MODULE_LIST tccase.f tzccase.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create tccase.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstccase.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tccase
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ccase.hlp
1 CCASE
  Perform case conversion on a string.  CCASE converts according to FLAG
  parameter.  Routine UPRCASE converts to upper case.  It is available so that
  application code may be simpler when converting to upper case.


  FORTRAN Calling Sequence:  CALL CCASE( STR, FLAG, MAX )
                  or to convert to UPPER case
                             CALL UPRCASE(STR)
  C Calling Sequence:        zccase( str, flag, max );
                  or to convert to UPPER case
                             zuprcase(str);

  Arguments:   STR  -  input/output, string to be converted:  CHARACTER data 
                       type for FORTRAN (may be a substring); for C, str does
                       not need to be null-terminated for CCASE if max >= 0,
                       but str needs to be null-terminated for zuprcase.
               FLAG  - input, integer  -  conversion flag
                         if = 1: convert lower to upper case
                         if = -1: convert upper to lower case
                         if 0 or otherwise: change case of all ascii characters
               MAX  -  input, integer  -   maximum number of characters in str.
                       However, processing will cease on encoutering a 0 (null)
                       or after the Fortran length of the CHARACTER string
                       is reached.
                       If max < 0, processing continues until a 0 (null) is
                       encountered or until the Fortran length of the 
                       CHARACTER string is reached.
                       Thus, max may be set to -1 for a Fortran CHARACTER 
                       string or a null-terminated C string.
  MAX is a REQUIRED argument. (flag and max are passed by value for zsumv.)

2 History

  Original Programmer: L. W. Kamp, 7 Febr. 1984
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: C
  Ported to UNIX by Steve Pohorsky

  Revisions:
   92-12-4  ..SP....  Made portable for UNIX - converted from Fortran
                      to C to handle CHARACTER strings from both FORTRAN
                      and C.  Added zccase for C.  Since all calls to CCASE
                      were to convert to UPPER, added UPRCASE and (zuprcase for
                      C) to provide simpler interface for most cases.
2 OPERATION

  CCASE will perform the specified case conversion on bytes that are ascii
  representations of letters, starting at the location specified by STR.
  CCASE calls zccase, which uses the C run-time routines toupper and
  tolower.  (Thus it might work on non-ASCII machines.)  Writing a lower case
  companion to UPRCASE would make a good training experience for a new
  programmer.
$ Return
$!#############################################################################
