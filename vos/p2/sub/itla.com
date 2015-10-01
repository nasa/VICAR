$!****************************************************************************
$!
$! Build proc for MIPL module itla
$! VPACK Version 1.5, Monday, October 12, 1992, 11:35:52
$!
$! Execute by entering:		$ @itla
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
$ write sys$output "*** module itla ***"
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
$   if F$SEARCH("itla.imake") .nes. ""
$   then
$      vimake itla
$      purge itla.bld
$   else
$      if F$SEARCH("itla.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake itla
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @itla.bld "STD"
$   else
$      @itla.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create itla.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack itla.com -
	-s itla.f -
	-i itla.imake -
	-t titla.f titla.imake titla.pdf tstitla.pdf -
	-o itla.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create itla.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE ITLA(INT, B, N)
      include  'fortport'  ! defines INT2BYTE.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  83-3-1  ...  LWK                                               C
C  86-3-26 ...  JRS  ADD HELP AND TEST FILES                      C
C  92-9-18      SP   made portable for UNIX.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER      INT,  N
      BYTE         B(N), B1
C==================================================================
      IF (INT .LT. 0 .OR. INT .GT. 255) CALL MABEND('ITLA: PROGRAM ERR')
      B1 = INT2BYTE(INT)
      DO I=1,N
	B(I) = B1
      ENDDO

      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create itla.imake
/* Imake file for VICAR subroutine itla */

#define SUBROUTINE itla

#define MODULE_LIST itla.f

#define P2_SUBLIB

#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create titla.f
C TEST SUBROUTINE itla
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE ITLA     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BYTE     B(25)                !THE BYTE BUCKET

      DO I=1,16                     !LOOP OVER 16 FILL VALUES
         CALL ITLA(I*16, B, 25)        !FILL THE BYTE ARRAY WITH (I)
                                       ! last time tests error handling
                                       ! Should abend.
         CALL PRNT(1,25,B,' ARRAY B:.')
      ENDDO
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create titla.imake
/* Imake file for Test of VICAR subroutine itla */

#define PROGRAM titla

#define MODULE_LIST titla.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create titla.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstitla.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "THIS IS A TEST OF MODULE ITL"
WRITE "FILL A BYTE ARRAY WITH NUMBERS FROM 1*16 to 16*16"
write "should print values twenty five 16s... twentyfive 240s"
write "should abend on 256 because it is not in range (0,255)"
titla
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create itla.hlp
1 ITLA

PURPOSE:  Fills a byte array with a value.

FORTRAN CALL:    CALL ITLA(INT, B, N)
From C, use ANSI C routine memset.

PARAMETERS:

     INT =  A fullword integer containing the input fill value.

     B   =  A byte array, of dimension N, into which the value 
            INT is to be output.
  
     N   =  The number of bytes to fill.


2 NOTES

DESCRIPTION

  This FORTRAN routine provides the FORTRAN programmer with a 
  function to fill a byte array with a value in the range (0,255).
  If INT is not in the range (0,255) inclusive,ITLA abends with the
  message 'ITLA: PROGRAM ERR'.

HISTORY

  Original Programmer: H.J. Frieden  06/12/74
  Converted to Vax by: L.W. Kamp     03/01/83
  Ported for UNIX by:  S. Pohorsky   10/12/92
  Current Cog Progr:   S. Pohorsky

$ Return
$!#############################################################################
