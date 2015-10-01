$!****************************************************************************
$!
$! Compile+link proc for MIPL program chkstat
$! VPACK Version 1.2, Wednesday, April 29, 1992, 13:59:49
$!
$! Execute by entering:		$ @chkstat
$!
$! Primary options are:
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$!   COMPile     Compile the program modules
$!   ALL         Build a private version (COMPile and LINK)
$!   SYStem      Build the system version (COMPile, LINK, and CLEAN-OBJ&SRC)
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        only the test files are created.
$!   IMAKE       only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       only the "other" files are created.
$!
$!   The default is to use the SYS parameter if none is provided.
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
$ write sys$output "*** Program chkstat ***"
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Default_Options
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
$Run_Make_File:
$   if F$SEARCH("chkstat.imake") .nes. ""
$   then
$      vimake chkstat
$      purge chkstat.bld
$   else
$      if F$SEARCH("chkstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake chkstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @chkstat.bld "SYS"
$   else
$      @chkstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create chkstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack chkstat.com -
	-s chkstat.f -
	-i chkstat.imake -
	-t tchkstat.f tchkstat.imake tchkstat.pdf tstchkstat.pdf -
	-o chkstat.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create chkstat.f
      SUBROUTINE CHKSTAT(STATUS, MSG, ABFLAG, DATA, NDAT)
c
C  All 5 ARGS are required Input parameters 
c
C  STATUS - Return if 0 or 1, else print MSG 
C  MSG    - Error Message
C  ABFLAG - Return If 0, Abend if NON-0
C  DATA   - Additional Data  (Integer*4 Array)
C  NDAT   - # DATA Words TO PRINT 

      Implicit Integer (A-Z)
      Character*(*)  MSG
      Integer  DATA(1)
c
c   No action whatsoever if status is 0 or 1
c
      If (Status.Eq.0 .OR. Status.Eq.1) Return
c
c   Print message
c
c      Call Xvmessage(' ', ' ')
c      Call Xvmessage('Porting !! ', ' ')
      Call Xvmessage(MSG, ' ')
c
c   Process Additional DATA 
c
      N = NDAT
      If (N .GE. 1)  Call PRNT(4,N,DATA,' .')

      Call PRNT(4,1,STATUS,' Status =.')
c
c   Abend if instructed to do so
c
      If (ABFLAG .NE. 0) Call Abend

      Return
      End
$ Return
$!#############################################################################
$Imake_File:
$ create chkstat.imake
/* Imake file for VICAR subroutine CHKSTAT  */

#define SUBROUTINE  chkstat

#define MODULE_LIST chkstat.f  

#define P1_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tchkstat.f
c test CHKSTAT

c	Include 'r2lib:main.fin/nolist'
        Include 'VICMAIN_FOR'
	Subroutine main44
	Implicit Integer (a-z)
	Integer s(2)/100,200/
c
c   First, if status = 1, the message will NOT be printed
c
	Call Chkstat( 1, 'Error in X',        0, 0, 0)
c
c   2nd,  the message will be printed since status = 2
c
	Call Chkstat( 2, 'Error in X',        0, 0, 0)
c
c   Cases 3, 4, and 5:  Make sure additional DATA is printed according to NDAT
c
	Call Chkstat( 3, 'Error in X, More Data:', 0, s, 0)
	Call Chkstat( 4, 'Error in X, More Data:', 0, s, 1)
	Call Chkstat( 5, 'Error in X, More Data:', 0, s, 2)
c
c   Case 6:  Check ABEND flag functionality
c
	Call Chkstat( 6, 'Terminal Error',    1, 0, 0)
 
	Return
	End
$!-----------------------------------------------------------------------------
$ create tchkstat.imake
/* IMAKE file for Test of VICAR subroutine CHKSTAT   */

#define PROGRAM tchkstat

#define MODULE_LIST tchkstat.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL
#define   LIB_TAE
/*  #define   LIB_LOCAL */    /*  Disable during delivery   */
#define LIB_P2SUB             /*  Enable during delivery  */
$!-----------------------------------------------------------------------------
$ create tchkstat.pdf
Process
End-proc
$!-----------------------------------------------------------------------------
$ create tstchkstat.pdf
Procedure
Refgbl $echo
Body
Let _onfail="continue"
Let $echo="yes"
TCHKSTAT
End-proc
$ Return
$!#############################################################################
$Other_File:
$ create chkstat.hlp
1 CHKSTAT

  Routine to check returned error status.

  Calling Sequence:  CALL CHKSTAT(STATUS, MSG, ABFLAG, DATA, NDAT)


2 Arguments & Operations

  STATUS -  Input, Integer  -  Return if 0 or 1, else print MSG
  MSG    -  Input, Character String -  Error Message to print
  ABFLAG -  Input, Integer  -  Abend flag
  DATA   -  Input, (Integer Array)  -  Additional data to print
  NDAT   -  Input, Integer  -  # DATA words to print

  E.g.:  Call CHKSTAT(IND, 'Read Error. Line # =', 0, REC, 1)
 
     will cause the following message to be printed if IND=4, REC=20:

                Read Error. Line # =
                              20
                Status =               4


2 History

  Original Programmer: L. W. Kamp, 23 Apr. 1984
  Current Cognizant Programmer: L. W. Kamp
  Source Language:   Fortran


  04-30-92.....W.P. Lee...... UNIX/VMS  Portable
$ Return
$!#############################################################################
