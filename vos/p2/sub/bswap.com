$!****************************************************************************
$!
$! Build proc for MIPL module bswap
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:03
$!
$! Execute by entering:		$ @bswap
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
$ write sys$output "*** module bswap ***"
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
$ write sys$output "Invalid argument given to bswap.com file -- ", primary
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
$   if F$SEARCH("bswap.imake") .nes. ""
$   then
$      vimake bswap
$      purge bswap.bld
$   else
$      if F$SEARCH("bswap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake bswap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @bswap.bld "STD"
$   else
$      @bswap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create bswap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack bswap.com -mixed -
	-s bswap.mar zbswap.c bswap.c -
	-i bswap.imake -
	-t tzbswap.c tbswap.f tbswap.imake tbswap.pdf tstbswap.pdf -
	-o bswap.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create bswap.mar
$ DECK/DOLLARS="$ VOKAGLEVE"
.TITLE	BSWAP
.PSECT	BSWAP
.ENTRY	BSWAP,^M<R2,R3>
;	IN	R2
;	N	R3
	MOVL     4(AP),R2
        MOVL    @8(AP),R3
        MOVL	#2,R1
;
LOOP:	MOVB	1(R2),R0
	MOVB	(R2),1(R2)
	MOVB	R0,(R2)
	ADDL2   R1,R2
	SOBGTR	R3,LOOP
	RET
.END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zbswap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zbswap( in, n )
unsigned char *in;        /* array to be swapped */
int n;                    /* number of pairs of bytes in array IN */
{
FTN_NAME2(bswap, BSWAP) (in, &n);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create bswap.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

void zbswap(unsigned char *in, int n);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/
void FTN_NAME2(bswap, BSWAP) ( in , n )
unsigned char *in;
int *n;
{
     zbswap(in,*n);
     return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zbswap(unsigned char *in, int n)
{
  register int i, indx;
  register unsigned char temp;

  for (i=0; i<n;i++) {
    indx = i * 2;
    temp = in[indx];
    in[indx] = in[indx+1];
    in[indx+1] = temp;
  }
}
    
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create bswap.imake
/* Imake file for VICAR subroutine BSWAP */

#define SUBROUTINE bswap

#if VMS_OS
#define MODULE_LIST bswap.mar zbswap.c
#define CLEAN_OTHER_LIST bswap.c 
#else
#define MODULE_LIST bswap.c 
#define CLEAN_OTHER_LIST bswap.mar zbswap.c 
#endif

#define P2_SUBLIB

#define USES_C
#if VMS_OS
#define USES_MACRO
#endif
$ Return
$!#############################################################################
$Test_File:
$ create tzbswap.c
/* tzbswap is the C program that test the bswap subroutine.  */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{

  unsigned char in[10];
  int i;
  char msg[132];

  for (i=0; i<10; i++)
    {
      in[i] = i - 2;
    }

  zvmessage("nbyte_pairs 5","");
  sprintf(msg,"input in decimal %u %u %u %u %u %u %u %u %u %u",in[0],in[1],in[2],in[3],in[4],in[5],in[6],in[7],in[8],in[9]);
  zvmessage(msg,"");
  zbswap(in,5);
  sprintf(msg,"output in decimal %u %u %u %u %u %u %u %u %u %u",in[0],in[1],in[2],in[3],in[4],in[5],in[6],in[7],in[8],in[9]);
  zvmessage(msg,"");

  zvmessage("***********Testing the FORTRAN interface....******","");

  FTN_NAME(tbswap)();

}
  
$!-----------------------------------------------------------------------------
$ create tbswap.f
        subroutine tbswap()

        integer*4 j
	byte in(10)

	do j=1,10
           in(j)= j - 3
	end do

	call xvmessage('nbyte_pairs 5',' ')
	call prnt(1,10,in,'input in decimal')

	call BSWAP(in,5)

	call prnt(1,10,in,'output in decimal')

        return
	end


$!-----------------------------------------------------------------------------
$ create tbswap.imake
/* Imake file for test of VICAR subroutine bswap */

#define PROGRAM tbswap

#define MODULE_LIST tzbswap.c tbswap.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tbswap.pdf
! pdf for test pgm for subroutine BSWAP
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstbswap.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
! test for subroutine BSWAP
tbswap
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create bswap.hlp
1 BSWAP

  PURPOSE: swaps bytes in pairs.  Can be used to convert an array of halfword
           data to the format of a machine with the opposite ordering of bytes.

  FORTRAN Calling Sequence:  
		
	BYTE IN(2*NBYTE_PAIRS) or INTEGER*2 IN(NBYTE_PAIRS)
	INTEGER*4 NBYTE_PAIRS

	CALL BSWAP(IN,NBYTE_PAIRS)

  C Calling Sequenc:

	unsigned char in[2*nbyte_pairs];  or    short int in[nbyte_pairs];
	int nbyte_pairs;

	zbswap(in,n);

  Arguments: BSWAP(IN,NBYTE_PAIRS)

2 History

  Original Programmer: Budak Barkan (Dec 12, 1984)
  Current Cognizant Programmer: Budak Barkan
  Source Language: Macro
  Revision history: 10-10-94  TLT  Made portable.

2 Operation

  Swaps byte-pairs.(#1 #2 #3 #4 ... becomes #2 #1 #4 #3 ...).
  Useful in IBM to VAX conversion of data formats.

2 Arguments

  IN is a byte or halfword declared array containing the input
  IN gets the results also (ie. done in place)
  NBYTE_PAIRS is the number of byte-pairs to be swapped





$ Return
$!#############################################################################
