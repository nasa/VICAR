$!****************************************************************************
$!
$! Build proc for MIPL module bits
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:58
$!
$! Execute by entering:		$ @bits
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
$ write sys$output "*** module bits ***"
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
$ write sys$output "Invalid argument given to bits.com file -- ", primary
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
$   if F$SEARCH("bits.imake") .nes. ""
$   then
$      vimake bits
$      purge bits.bld
$   else
$      if F$SEARCH("bits.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bits
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bits.bld "STD"
$   else
$      @bits.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bits.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bits.com -mixed -
	-s bits.c -
	-i bits.imake -
	-t tbits.f tzbits.c tzbits.imake tzbits.pdf tstbits.pdf -
	-o bits.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bits.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*									*/
/*				bits.c					*/
/*		Originally changed to C by Steve Hwan			*/
/*									*/
/*			CREATION DATE: 15 October 1993			*/
/*			LAST MODIFIED: 19 October 1993			*/
/*									*/
/*	These routines extract bits lowb through highb from IN and	*/
/* put them into OUT.  One routine is written for full words		*/
/* (long ints) and one is written for half words (short ints).		*/
/*									*/
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>

#define HIGH_BIT_L	(sizeof(long int) * 8 -1)
#define HIGH_BIT_S	(sizeof(short int) * 8 -1)

void zbits(),zhbits();

void FTN_NAME2(bits, BITS) (in,lowb,highb,out)
long int *in;
int *lowb,*highb;
long int *out;
{
  zbits(*in,*lowb,*highb,out);
}

void FTN_NAME2(hbits, HBITS) (in,lowb,highb,out)
short int *in;
int *lowb,*highb;
long int *out;
{
  zhbits(*in,*lowb,*highb,out);
}


void zbits(in,lowb,highb,out)
long int in;
int lowb,highb;				/* uses 0 to N-1 */
long int *out;
{
  unsigned long int mask= 0;

  mask = ~mask;				/* Initialize mask with all 1's */

  if ((lowb<0) || (highb<0)		/* negative bit numbers specified */
      ||  (highb<lowb)			/* highb lower than lowb */
      ||  (lowb>HIGH_BIT_L)		/* bit numbers beyond top */
      || (highb>HIGH_BIT_L))  {		/* of word. */
    zvmessage("Routine bits called with invalid parameters.","");
    zabend();
  }

  mask = mask >>  (HIGH_BIT_L-highb );
  *out = ((unsigned long int)in & mask) >> lowb;

 return;
}


void zhbits(in,lowb,highb,out)
short int in;
int lowb,highb;				/* uses 0 to N-1 */
long int *out;
{
  unsigned short int mask= 0;

  mask = ~mask;				/* Initialize mask with all 1's */

  if ((lowb<0) || (highb<0)		/* negative bit numbers specified */
      ||  (highb<lowb)			/* highb lower than lowb */
      ||  (lowb>HIGH_BIT_S)		/* bit numbers beyond top */
      || (highb>HIGH_BIT_S)) {		/* of word. */
    zvmessage("Routine bits called with invalid parameters.","");
    zabend();
  }

  mask = mask >> (HIGH_BIT_S-highb );
  *out = ( (unsigned short int) ((unsigned short int)in & mask) >> lowb);

 return;
}

	
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bits.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY bits

   To Create the build file give the command:

	$ vimake bits                     (VMS)
   or
	% vimake bits                     (Unix)


*************************************************************************/

#define SUBROUTINE bits

#define MODULE_LIST bits.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tbits.f
      SUBROUTINE TBITS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE BITS                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER*2  H   /-21846/        !HALFWORD INTEGER HEX AAAA
      INTEGER*4  F   /-1431655766/   !FULLWORD INTEGER HEX AAAAAAAA
      INTEGER*4  A(0:31)             !INTEGER ARRAY
      CHARACTER  CARD*80             !PRINT BUFFER

      CALL XVMESSAGE(' ',' ')

      DO I=15,0,-1
         CALL HBITS(H, I, I, A(I))
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' **HALF DATA** HARDWIRED TO 0xAAAA',' ')
      WRITE(CARD, 100) (A(15-I),I=0,15)
      CALL XVMESSAGE(CARD,' ')

      DO I=31,0,-1
         CALL BITS(F, I, I, A(I))
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' **FULL DATA** HARDWIRED TO 0xAAAAAAAA',' ')
      WRITE(CARD, 100) (A(31-I),I=0,31)
      CALL XVMESSAGE(CARD,' ')

      RETURN
 100  FORMAT(32I1)
      END
$!-----------------------------------------------------------------------------
$ create tzbits.c
/************************************************************************/
/*									*/
/*			tzbits.c					*/
/*		C Implementation by: Steve Hwan				*/
/*									*/
/*		Creation Date: 16 October 1993				*/
/*		Last Modified: 22 October 1993				*/
/*									*/
/*	This is a driver program to test the routines zbits.c and	*/
/* zhbits.c.								*/
/* 	This program can be called without any parameters and it	*/
/* should look just like the output of the old bits program.  Option-	*/
/* ally, it can be called with the following parameters in the		*/
/* following order:							*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vicmain_c"
#include "ftnbridge.h"

#define SUCCESS		1
#define LAST_BIT_SHORT	(8* sizeof(short int) -1)
#define LAST_BIT_LONG	(8* sizeof(long int) -1)

void ztbits()
{
  FTN_NAME(tbits)();
}

/* tzbits_help just prints out the parameters to use the test program */
void tzbits_help(hbit)
int hbit;
{
  char mymessage[80];

  zvmessage("Optional parameters are:","");
  zvmessage("HWORD   half word input(hex)    default: 0x05AF","");
  zvmessage("FWORD   full word input(hex)    default: 0x0055AAFF","");
  zvmessage("LOBIT   low(starting) bit(dec)  default: 0","");
  sprintf(mymessage,"HIBIT  high(ending) bit(dec)   default: %d\n",hbit);
  zvmessage("Or type tzbits help to get this message\n","");
  zvmessage(mymessage,"");

  return;
}



main44()
{
  int i;			/* dummy index */
  short int half_in;		/* half word input */
  long  int word_in;		/* full word input */
  int lbit,hbit;		/* starting and ending of bits to extract */
  long int output_word;		/* return value */
  char card[132];		/* dummy string to pass to zvmessage */
  char temp[132];			/* temporary string to deal with zv routines */
  int count;			/* used in zvp */

  /*** GET PARAMETERS LOBIT,HIBIT ***/
  zvp("LOBIT", &lbit, &count);
  zvp("HIBIT", &hbit, &count);
  if (hbit==-1) hbit=LAST_BIT_LONG;

  /*** GET PARAMETER HWORD ***/
  zvp("HWORD", temp, &count);
  if (count<=0)
    half_in=0x05AF;
  else {
    if (!strcmp(temp,"help")) {
      tzbits_help(hbit);
      return;
    }
    count=sscanf(temp,"%hx",&half_in);
    if (count<=0)
      half_in=0x05AF;
  } /* else */

  /*** GET PARAMETER FWORD ***/
  zvp("FWORD", temp, &count);
  if (count<=0)
    word_in=0x0055AAFF;
  else {
    count=sscanf(temp,"%x",&word_in);
    if (count<=0)
      word_in=0x0055AAFF;
  }

  /* arbitrary part - lets user select which bits to look at in addition */
  /* to normal test. */
  zvmessage("Running user selected lbit and hbit.(Defaults to whole word)","");
  zbits(word_in, lbit,hbit, &output_word);
  sprintf(temp,"\t %x\n",output_word);
  zvmessage(temp,"");

  /* Test the half word routine, looking bit by bit */
  zvmessage("*****Half word tests*****","");
  card[0]=0;
  for(i=LAST_BIT_SHORT; i>=0; i--) {
    zhbits(half_in, i,i,&output_word);
    sprintf(temp,"%x",output_word);
    strcat(card,temp);
  }
  zvmessage(card,"");

  /* Test the full word routine, looking bit by bit */
  zvmessage("*****Full word tests*****","");
  card[0]=0;
  for(i=LAST_BIT_LONG; i>=0; i--) {
    zbits(word_in, i,i,&output_word);
    sprintf(temp,"%x",output_word);
    strcat(card,temp);
  }
  zvmessage(card,"");

  zvmessage("Calling FORTRAN version....","");
  ztbits();

}
$!-----------------------------------------------------------------------------
$ create tzbits.imake
/* Imake file for Test of VICAR subroutine bits */

#define PROGRAM tzbits

#define MODULE_LIST tzbits.c tbits.f

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
$!-----------------------------------------------------------------------------
$ create tzbits.pdf
PROCESS
PARM HWORD TYPE=STRING DEFAULT=-- COUNT=0:1
PARM FWORD TYPE=STRING DEFAULT=-- COUNT=0:1
PARM LOBIT TYPE=INTEGER DEFAULT=0
PARM HIBIT TYPE=INTEGER DEFAULT=-1
END-PROC
$!-----------------------------------------------------------------------------
$ create tstbits.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
! First, test the internal tzbits help mode.  Should display a list
! of optional parameters
tzbits help
! Test with half word=aaaa, full word=aaaaaaaa  The point of this call
! is to get the C and Fortran versions to look at the same words.  Fortran
! is hardwired to these values because the old Fortran tester was, and
! the requirements say we don't have to upgrade the test programs.
! However, I didn't think these tests were that great, so I made the
! C tester more powerful and more flexible.
tzbits aaaa aaaaaaaa
! Now use it with the C hardwired parameters - I think it tests
! the bits a little better by giving a different value to each
! byte.
tzbits
! Extract the bottom half of the full word
tzbits lobit=0 hibit=15
! Extract an arbitrary portion of the full word
tzbits lobit=3 hibit=23
let $echo="no"
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create bits.hlp
1 BITS

PURPOSE:   Extracts a bit string from IN and places it, right justified,
           into fullword OUT.

USAGE:
FORTRAN:   CALL BITS(IN, LOBIT, HIBIT, OUT)
           CALL HBITS(IN, LOBIT, HIBIT, OUT)

C:         void zbits(in,lowb,highb,out)
             long int in;
             int lowb,highb;
             long int *out;
           void zhbits(in,lowb,highb,out)
             short int in;
             int lowb,highb;                         /* uses 0 to N-1 */
             long int *out;

PARAMETERS:

     IN    =  Is a halfword(zhbits,HBITS) or fullword(zbits,BITS) integer
              variable containing the bits to be moved into OUT.

     LOBIT =  Is the (integer) start bit of the string to be extracted.
 
     HIBIT =  Is the (integer) end   bit of the string to be extracted.

     OUT   =  Is the (fullword) integer variable containing the right
              justified, extracted bits.

    Note that in numbering bits, the least significant(low order) bit
is bit 0.


2 NOTES

DESCRIPTION

These routines provides the C or Fortran programmer with a function
to extract an arbitrary bit string from a halfword or fullword variable.
This facilitates further manipulation such as printing, testing, comparing
etc.

HISTORY

  Original Programmer: H.J. Frieden  06/12/74
  Converted to Vax:    L.W. Kamp     04/20/83
  Converted to C:      S.V. Hwan     10/22/93
  Current Cog Progr:   S.V. Hwan
$ Return
$!#############################################################################
