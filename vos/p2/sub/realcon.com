$!****************************************************************************
$!
$! Build proc for MIPL module realcon
$! VPACK Version 1.5, Monday, November 23, 1992, 09:32:33
$!
$! Execute by entering:		$ @realcon
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
$ write sys$output "*** module realcon ***"
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
$   if F$SEARCH("realcon.imake") .nes. ""
$   then
$      vimake realcon
$      purge realcon.bld
$   else
$      if F$SEARCH("realcon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake realcon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @realcon.bld "STD"
$   else
$      @realcon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create realcon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack realcon.com -
	-s realcon.f -
	-i realcon.imake -
	-t trealcon.f trealcon.imake trealcon.pdf tstrealcon.pdf -
	-o realcon.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create realcon.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	subroutine realcon( r, buf, n)

c Convert REAL*4 to N-character output format in BUF.
c  Uses Fixed-point format unless not enough characters available.

	logical neg
        character*(*) buf
        CHARACTER*80  RUNTIME

c  intitialize to "**..*"
	do i=1,n
	  buf(i:i) = '*'
	enddo

	ra = r
	neg = .FALSE.
	if (r.lt. 0.) then
	  neg = .TRUE.
	  ra = -r
	endif
	if (r.eq.0.) ra=1.
	rlog = alog10( ra)

	if (rlog.ge.0) then	! |r| >= 1.0 
	  is = ifix(rlog+1.)	! # digits to left of "."
	  if (neg) is = is+1	! one for "-"
	  id = n-is-1		! # digits to right of "."
	  if (id.lt.0) then	! can't use F format
	    if (rlog.eq.0.) rlog = 1.
	    ie = alog10(rlog)+1	! "E+yy"
	    id = n-4-ie		! # digits between "x." and "E+yy"
	    if (neg) id = id-1	! "-"
	    if (id.lt.0) return
            WRITE (RUNTIME,9020) n, id, ie
            WRITE (BUF(1:n), RUNTIME) r
	  else
            WRITE (RUNTIME,9010) n, id
            WRITE (BUF(1:n), RUNTIME) r
	  endif

	else			! |r| < 1.0
	  id = n-2		! # digits to right of "."
	  if (neg) id = id-1	! one for "-"
	  idm = ifix(-rlog)+1	! minimum # required
	  if (id.lt.idm) then	! can't use F format
	    rlog = -rlog        ! if r=2.e-10, then 10>rlog>9 after this line.
	    if (rlog.eq.0.) then
                rlog = 1.
            else                ! so need to add 1 to make ie come out right.
                if (rlog .ne. aint(rlog) )  rlog = rlog + 1.0
            end if
	    ie = alog10(rlog)+1	! "E-yy"
	    id = n-4-ie		! # digits between "x." and "E-yy"
	    if (neg) id = id-1	! "-"
	    if (id.lt.0) return
            WRITE (RUNTIME,9020) n, id, ie
            WRITE (BUF(1:n), RUNTIME) r
	  else
            WRITE (RUNTIME,9010) n, id
            WRITE (BUF(1:n), RUNTIME) r
	  endif

	endif
	return

9010    FORMAT( '(F',   I3.3, '.', I3.3, ')' )
9020    FORMAT( '(1PE', I3.3, '.', I3.3,'E', I3.3, ')' )
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create realcon.imake
/* Imake file for VICAR subroutine realcon */

#define SUBROUTINE realcon

#define MODULE_LIST realcon.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create trealcon.f
C TEST SUBROUTINE realcon
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE REALCON     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	real*4 r(10)
        data r / 5.5e20, -3.e15, 1.7e8, 1.0, 0.9, -0.665,
     .	 0.0, 10.0E-10, 1.0711E-10, -5.67e7/
	character*20 cbuf
        data cbuf/' R =XXXXXXXXXXXXXXXX'/

c first try all numbers in field of 8 chars:
	do i=1,10
	  call realcon( r(i), cbuf(5:), 8)
          call xvmessage( cbuf(1:13), ' ')
	enddo

c now try field of 15:
	do i=1,10
	  call realcon( r(i), cbuf(5:), 15)
          call xvmessage( cbuf, ' ')
	enddo
        return
	end
$!-----------------------------------------------------------------------------
$ create trealcon.imake
/* Imake file for Test of VICAR subroutine realcon */

#define PROGRAM trealcon

#define MODULE_LIST trealcon.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create trealcon.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstrealcon.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "THIS IS A TEST OF MODULE REALCON"
WRITE "should generate 20 values, each with 1 trailing X."
trealcon
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create realcon.hlp
1 REALCON

 This routine will convert a REAL*4 number, R, to exactly fill an
 N-character field in BUF in external format. It will convert R to
 Fixed-point format unless not enough characters are available, in
 which case Floating point (exponential) format is used.

  Fortran Calling Sequence:  CALL REALCON( R, BUF, N)

  Arguments: R (real*4) -   number to be converted
             BUF -          character variable to contain external 
                            representation of R.
             N (integer*4) -number of characters in BUF to contain 
                             the representation of R.
2 History

  Original Programmer: L.W.Kamp,  17 April 1985
  Ported for UNIX by:  S. Pohorsky   11/17/92
  Current Cognizant Programmer: L.W.Kamp
  Source Language: Fortran

  REVISIONS
   11-18-92  SP  Fixed bug in computing number of exponent digits.

2 Operation

 REALCON uses the Fortran WRITE statement with a run-time format.
 (REALCON is callable from Fortran.)  The routine will convert a
 number to fixed-point format if the field size is large enough,
 else it will convert to floating point format.


 E.g., the number 1.285E+06 will be converted to:

                1285000.00

 if N=10 is specified, but to:

                1.28E+6

 if N=7 is specified.

 The routine executes a number of instructions for each number, including
 up to two logarithms, plus whatever is inside the WRITEs, so it is
 not recommended for heavy-duty processing.

$ Return
$!#############################################################################
