$!****************************************************************************
$!
$! Build proc for MIPL module lut
$! VPACK Version 1.5, Monday, December 07, 1992, 10:20:32
$!
$! Execute by entering:		$ @lut
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
$ write sys$output "*** module lut ***"
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
$   if F$SEARCH("lut.imake") .nes. ""
$   then
$      vimake lut
$      purge lut.bld
$   else
$      if F$SEARCH("lut.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lut
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lut.bld "STD"
$   else
$      @lut.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lut.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lut.com -
	-s lut.f -
	-i lut.imake -
	-t tlut.f tlut.imake tlut.pdf tstlut.pdf -
	-o lut.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lut.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c*****************************************************************************
c LUT.F - Subroutine to translate NS bytes in buffer 'BUF' using look-up 
c         table 'TAB'
c
c 12-7-1992 M. O'Shaughnessy     Converted LUT back to Fortran from assembler
c                                code and ported it to UNIX. Removed optional
c                                number of parameters. No C-bridge.
c*****************************************************************************
	subroutine lut( ns, buf, tab, obuf)
        include 'fortport'

        integer*4 ns,i,wor
	byte      buf(*),tab(256), obuf(*)

	do i=1,ns
           wor = byte2int(buf(i))
	   obuf(i) = tab(wor+1)
        enddo

	return
	end
c*****************************************************************************
c end module
c*****************************************************************************
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lut.imake
/* Imake file for VICAR subroutine LUT */

#define SUBROUTINE lut

#define MODULE_LIST lut.f

#define P2_SUBLIB

#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tlut.f
C*****************************************************************************
C Unit test program TLUT.F for subroutine LUT
c Ported to UNIX 12/7/1992.
C*****************************************************************************
	include 'VICMAIN_FOR'
	subroutine main44
        include 'fortport'
c        
        integer*4     i,x
	byte          buf(256), tab(256), obuf(256), bx
c
	call xvmessage('construct complement lookup table',' ')
	do i=1,256
	  x = 256-i
          bx = int2byte(x)
	  tab(i) = bx

	  x = i - 1
          bx = int2byte(x)
	  buf(i) = bx
	enddo

	call prnt(1, 256, buf, ' input buffer =.')

	call lut( 256, buf, tab, obuf)
	call prnt(1, 256, obuf, ' output buffer =.')

	call lut( 256, buf, tab, obuf)
        call xvmessage('next buffer should = input buffer',' ')
	call prnt(1, 256, buf, ' output buffer =.')

	return
	end





$!-----------------------------------------------------------------------------
$ create tlut.imake
/* Imake file for Test of VICAR subroutine lut */

#define PROGRAM tlut

#define MODULE_LIST tlut.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 

$!-----------------------------------------------------------------------------
$ create tlut.pdf
!*****************************************************************************
! TLUT.PDF - pdf for test program TLUT.F for the subroutine LUT
!*****************************************************************************
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstlut.pdf
!****************************************************************************
! TSTLUT.PDF, unit test procedure for subroutine LUT.F
!****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "Test subroutine LUT"
TLUT
end-proc
.title TSTLUT.PDF - unit test for subroutine LUT
.help
This unit test creates no files and uses little CPU. Just run it in batch,
with no parameters, and verify the output.
.end
$ Return
$!#############################################################################
$Other_File:
$ create lut.hlp
1 LUT

  LUT - byte translation using lookup table.  Primary application is
    linear contrast enhancement (stretching) of byte images.

  Calling sequence:   CALL LUT( NS, IBUF, TAB, OBUF)
  All arguments are required.

  Arguments:  

	integer*4  NS      Number of bytes to be translated
	byte       IBUF    Buffer containing bytes to be translated
	byte       TAB     Lookup table (must contain 256 byte entries)
	byte       OBUF    Buffer to contain translated bytes. 

2 History

  Original Programmer:  L.W.Kamp,  25 June 1985
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN

  This routine was written as partial support for the IBM
  subroutine LUT (by J.J.Lorre), which was in Assembler and
  was able to process halfword data as well as byte.  The
  current implementation will be upgraded when time permits.

  The routine was first coverted to FORTRAN by LWK and later re-written
  in MACRO by GMY.

  12/7/1992 LUT was reconverted to FORTRAN by M. O'Shaughnessy and 
  ported to the UNIX platform.  LUT now runs on both VAX/VMS and UNIX
  platforms, but is currently only callable by FORTRAN.
$ Return
$!#############################################################################
