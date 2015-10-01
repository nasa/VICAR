$!****************************************************************************
$!
$! Build proc for MIPL module pu75
$! VPACK Version 1.7, Monday, May 02, 1994, 16:41:35
$!
$! Execute by entering:		$ @pu75
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
$ write sys$output "*** module pu75 ***"
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
$ write sys$output "Invalid argument given to pu75.com file -- ", primary
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
$   if F$SEARCH("pu75.imake") .nes. ""
$   then
$      vimake pu75
$      purge pu75.bld
$   else
$      if F$SEARCH("pu75.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pu75
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pu75.bld "STD"
$   else
$      @pu75.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pu75.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pu75.com -
	-s pu75.f zpu75.c -
	-i pu75.imake -
	-t tpu75.f tzpu75.c tpu75.imake tpu75.pdf tstpu75.pdf -
	-o pu75.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pu75.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------
C                        VICAR SUBROUTINE pu75
C
C Edit History:
C      4-94        CRI    MSTP S/W CONVERSION (VICAR PORTING)
C------------------------------------------------------------------


      SUBROUTINE PU75(U,N)

      REAL*4 U(N,2,103)
      CHARACTER*132 RBUF
      CHARACTER*132 QBUF
      CHARACTER*132 PBUF

C  U IS REAL*4 IF N=1, REAL*8 IF N=2
C
      J = 1
C
      DO II = 1, 5
         PBUF(1:132) = ' '
         QBUF(1:132) = ' '
         RBUF(1:132) = ' '
         K = 7
C
         DO I = 1, 11
            WRITE (RBUF(K-2-2:K-2),'(I3)') J
            WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
            WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
            J = J + 1
            K = K + 12
         end do
C
         CALL XVMESSAGE(RBUF(2:132),' ')
         CALL XVMESSAGE(PBUF(2:132),' ')
         CALL XVMESSAGE(QBUF(2:132),' ')
         CALL XVMESSAGE(' ',' ')
         IF (II .EQ. 5) RETURN
         PBUF(1:132) = ' '
         QBUF(1:132) = ' '
         RBUF(1:132) = ' '
         K = 7
C
         DO I = 1, 12
            WRITE (RBUF(K-2-2:K-2),'(I3)') J
            WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
            WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
            J = J + 1
            IF (I .EQ. 1) K = K - 6
            IF (I .EQ. 11) K = K - 6
            K = K + 12
         END DO
C
         CALL XVMESSAGE(RBUF(2:132),' ')
         CALL XVMESSAGE(PBUF(2:132),' ')
         CALL XVMESSAGE(QBUF(2:132),' ')
         CALL XVMESSAGE(' ',' ')
      END DO
C
      return
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpu75.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************/
/* C-Callable Version : zpu75 - print grid coordinate information */
/******************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"


void zpu75 ( u, n)
int n;                     /* integer specifying real*4 or real*8 */ 
void *u;                   /* array for grid information          */

{
FTN_NAME(pu75) ( u, &n);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pu75.imake
/* Imake file for VICAR subroutine pu75 */

#define SUBROUTINE pu75

#define MODULE_LIST pu75.f zpu75.c 

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tpu75.f

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'
      REAL*4 LOC(2,1000)

C
      CALL IFMESSAGE('TPU75 version 01-JULY-1994')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(inunit,'INP',1,status,' ')
      CALL XVOPEN(inunit,status,' ')
      CALL XVREAD(inunit,loc,status,' ')
C          SCAN FOR SPECIAL END OF RECORD MARK
      CALL PU75(LOC,1)
      CALL XVCLOSE(inunit,status,' ')
C
      
      CALL XVMESSAGE(
     X 'Repeat test cases in C to test C interface: zpu75',' ')


      call tzpu75  

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzpu75.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzpu75) ()
{
      float loc[2][1000];
      int   status, inunit, ii, count;

      ii = 1;
      zveaction("SA","");
      zifmessage("TZPU75 version 02-MAY-1994");
 
      status = zvunit(&inunit,"INP",ii,"");
      status = zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA","");
      status = zvread(inunit,loc,"");

      zpu75 (loc, 1);

      status = zvclose(inunit,"");     
}

$!-----------------------------------------------------------------------------
$ create tpu75.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tpu75

   To Create the build file give the command:

		$ vimake tpu75			(VMS)
   or
		% vimake tpu75			(Unix)


************************************************************************/


#define PROGRAM	tpu75
#define R2LIB

#define MODULE_LIST tpu75.f tzpu75.c

#define MAIN_LANG_FORTRAN

#define TEST

#define USES_FORTRAN
#define USES_C

#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB  
/*   The LIB_LOCAL is removed for delivery  */
/* #define LIB_LOCAL  */
/************************* End of Imake file ***************************/
$!-----------------------------------------------------------------------------
$ create tpu75.pdf
process
parm inp type=string
end-proc
$!-----------------------------------------------------------------------------
$ create tstpu75.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
!
Write " "
Write " The Test Data are handled for both VMS and UNIX in this PDF. "
Write " At present (May 1994) for the UNIX, in order to run the program, "
Write " the data file (MIPLDISK:[MIPL.VGR].reslfil) which is approx. "
Write " 28000 blocks MUST be copied to the LOCAL directory where the "
Write " program resides. "
Write " This UNIX restriction on the data will be changed eventually. "
Write " "
refgbl $syschar
local resl_fil
!
!  First,  VMS
!
if ($syschar(1) = "VAX_VMS")
   let resl_fil = "MIPLDISK:[MIPL.VGR]resl.fil"
!
!  Now,    UNIX
!
else
   let resl_fil = "resl.fil"
end-if


tpu75 @resl_fil

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create pu75.hlp
1 pu75

      Subroutine to print grid coordinates information.

  FORTRAN calling Sequence:  CALL pu75(u,n)
  C Calling Sequence:        zpu75(u,n)
			

  Arguments:
		U (input) : Array containing grid coordinate information.
		N (input) : Integer specifying whether grid information is in
				real*4 or real*8 format.

2 History

  Original Programmer: ...<NAME & DATE>
  Current Cognizant Programmer: Helen De Rueda		July 6, 1984
  Source Language:  Fortran
  Made portable for UNIX        Meredith Cox (CRI)      14 Apr 1994

2 Operation

     "pu75" takes the grid coordinate values and outputs them in either rows
     of 11 or 12 values each, which are the VO reseau pattern. It will output
     only 103 coordinate values.

2 Arguments

	U (input) : Array containing grid coordinate information.

	N (input) : Integer specifying whether grid information is in
			real*4 or real*8 format.

$ Return
$!#############################################################################
