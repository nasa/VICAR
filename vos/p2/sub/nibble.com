$!****************************************************************************
$!
$! Build proc for MIPL module nibble
$! VPACK Version 1.9, Monday, December 07, 2009, 16:30:42
$!
$! Execute by entering:		$ @nibble
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
$ write sys$output "*** module nibble ***"
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
$ write sys$output "Invalid argument given to nibble.com file -- ", primary
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
$   if F$SEARCH("nibble.imake") .nes. ""
$   then
$      vimake nibble
$      purge nibble.bld
$   else
$      if F$SEARCH("nibble.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake nibble
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @nibble.bld "STD"
$   else
$      @nibble.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create nibble.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack nibble.com -mixed -
	-s nibble.c -
	-i nibble.imake -
	-t tnibble.f tznibble.c tnibble.imake tnibble.pdf tstnibble.pdf -
	-o nibble.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create nibble.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*---------------------------  nibble     ------------------------
 * NIBBLE  (Nibbles to Bytes)
 *
 *  REVISION HISTORY
 *    5-94 CRI MSTP S/W Conversion (VICAR Porting), changed to use C
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                           nibble

	General routine for converting an array of nibbles
          into an array of bytes

	Fortran format of call:

          CALL NIBBLE( IN,OUT,NBYTES_OUT)

        "C" format of call:

          znibble(in, out, nbytes_out);
 
	Parameters:-

	IN - byte declared array containing the nibbles
        OUT - byte declared array to hold the unpacked bytes
        NBYTES_OUT - is the number of resultant bytes

--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#define NIB_MASK 15      /* mask for right nibble */
/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(nibble, NIBBLE) (in, out, nbytes_out)
     unsigned char *in, *out;
     int *nbytes_out;
{
   znibble( in, out, *nbytes_out);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

znibble( in, out, nbytes_out)
   unsigned char *in, *out;
   register int nbytes_out;
{
  register unsigned char nibb;
  register int i, j;

  for (i=0; i < nbytes_out/2; i++)         /* two nibbles to a byte */
    {
      j = 2 * i;
      nibb = *(in + i);                    /* get byte with nibbles */
      *(out + (j + 1)) = nibb & NIB_MASK;  /* mask and store second nibble */ 
      *(out + j) = nibb >> 4;              /* shift and store first nibble */ 
    }
  if (nbytes_out%2)                        /* if odd number of bytes */     
    {
      nibb = *(in + i);                    /* get last byte with nibble */  
      *(out + (2 * i)) = nibb >> 4;        /* shift and store last nibble */
    }
  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create nibble.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY nibble

   To Create the build file give the command:

	$ vimake nibble                     (VMS)
   or
	% vimake nibble                     (Unix)


*************************************************************************/

#define SUBROUTINE nibble

#define MODULE_LIST nibble.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tnibble.f
C  This is the Fortran routine to test the subroutine nibble.



  	include 'VICMAIN_FOR'
        subroutine main44
	byte in(10),ot(10)
        integer*4 num
	num = 9
        do i=1,10
	in(i)=-3+i
	ot(i)=0
	end do
	call prnt(4,1,num,'nbytes_out.')
	call prnt(1,5,in,'in in dec.')
	call nibble(in,ot,9)
	call prnt(1,9,ot,'ot in dec.')
        call tznibble  ! test the "C" interface
	return
	end
$!-----------------------------------------------------------------------------
$ create tznibble.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/* "C" test routine to test the nibble subroutine.  This routine builds
 *  an array of nibbles and then calls the znibble subroutine to parse
 *  the array of nibbles and build an array of bytes, each containing
 *  a single nibble. 
 */ 



void FTN_NAME(tznibble)() 

{
  char in[10], ot[10];
  int i, num;

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      for (i=0; i<10; i++)
      {
         in[i] = -2 + i;
         ot[i] = 0;
      }
        num =9; 
	zprnt(4,1,&num,"nbytes_out");
	zprnt(1,5,in,"in in dec");
	znibble(in,ot,9);
	zprnt(1,9,ot,"ot in dec");
}
$!-----------------------------------------------------------------------------
$ create tnibble.imake
/* Imake file for Test of VICAR subroutine nibble */

#define PROGRAM tnibble

#define MODULE_LIST tnibble.f tznibble.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tnibble.pdf
process help=*
END-PROC
$!-----------------------------------------------------------------------------
$ create tstnibble.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
tnibble
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create nibble.hlp
1 NIBBLE

    PURPOSE
 
         Subroutine to unpack nibbles to bytes.
 
  Fortran calling sequence:  CALL NIBBLE(IN,OUT,NBYTES_OUT)
  C Calling Sequence:        znibble(in, out, nbytes_out);
  
2 History

  Original Programmer: Budak Barkan (Dec 12, 1984)
  Current Cognizant Programmer: Budak Barkan
  Source Language: Macro
  Made portable for UNIX RNR(CRI) (May 25, 1994) Rewrote in "C"

2 Operation

  Unpacks nibbles to bytes. A nibble is a 4-bit unsigned quantity.
  Sucessive nibbles are contiguous in input.

2 Arguments

  IN is a byte declared array containing the nibbles
  OUT is a byte decleared array to hold the unpacked bytes
  NBYTES_OUT is the number of resultant bytes
$ Return
$!#############################################################################
