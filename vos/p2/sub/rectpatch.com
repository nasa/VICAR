$!****************************************************************************
$!
$! Build proc for MIPL module rectpatch
$! VPACK Version 1.5, Monday, February 08, 1993, 15:55:03
$!
$! Execute by entering:		$ @rectpatch
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
$ write sys$output "*** module rectpatch ***"
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
$   if F$SEARCH("rectpatch.imake") .nes. ""
$   then
$      vimake rectpatch
$      purge rectpatch.bld
$   else
$      if F$SEARCH("rectpatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rectpatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rectpatch.bld "STD"
$   else
$      @rectpatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rectpatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rectpatch.com -
	-s rectpatch.f -
	-i rectpatch.imake -
	-t trectpatch.f trectpatch.imake trectpatch.pdf tstrectpatch.pdf -
	-o rectpatch.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rectpatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine rectpatch(rdata)

C     RETURNS CALCULATED LINE OF LATITUDE 0. AND LONGITUDE AT SAMPLE 1

      REAL*8 PI,CONST,REQ,SCALE,RSAMP,RLAT
      real rdata(40)
      DATA pi/3.141592653589793D0/
C==================================================================
      call mve(4,1,rdata(39),l,1,1)
      if(l.ne.10)return
      SCALE=rdata(7)   ! KM/PIXEL
      req=rdata(26)
      flag=-999
      RLINE=rdata(2)
      RSAMP=rdata(1)
      RLAT=rdata(3)
      RLON=rdata(6)
      IF(RLAT.EQ.0..AND.RSAMP.EQ.1.)RETURN
      CONST=SCALE/REQ/PI*180.  !DEGREES/PIXEL

C     CALCULATE LONGITUDE
      RLON=RLON+CONST*(RSAMP-1.)
      if(rlon.eq.0.)rlon=360.
      IF(RLON .GT. 360.) RLON=AMOD(360.+RLON,360.)

C     CALCULATE LINE OF EQUATOR
      RLINE=RLINE+RLAT/CONST
      rdata(1)=1.
      rdata(2)=RLINE
      rdata(3)=0.
      rdata(6)=RLON
      if(rdata(6).eq.0.)rdata(6)=360.
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rectpatch.imake
/* Imake file for VICAR subroutine rectpatch */

#define SUBROUTINE rectpatch

#define MODULE_LIST rectpatch.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create trectpatch.f
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real*4 rdata(40)
      integer idata(40)
      equivalence(idata,rdata)
      idata(39)=10      !simple cylindrical
      rdata(1)=49.99999      !sample  (changed from 50 to make output prettier.)
      rdata(2)=50.      !line
      rdata(3)=-64.6828 !lat
      rdata(6)=205.3172 !west long
      rdata(7)=100.     !scale
      rdata(25)=1815.   !io radii
      rdata(26)=rdata(25)
      call prnt(7,26,rdata,'input data=.')
      CALL XVMESSAGE('AT LINE=50,SAMP=50,LAT=-64.6828,LONG=205.3172',
     .               ' ')
      call rectpatch(rdata)
      CALL prnt(7,1,RDATA(2),'LINE=.')
      CALL prnt(7,1,RDATA(1),'SAMPLE=.')
      CALL prnt(7,1,RDATA(3),'LAT=.')
      CALL prnt(7,1,RDATA(6),'LONG=.')
      CALL XVMESSAGE('SHOULD BE: LINE=29.51,SAMP=1.,LATI=0,LONG=360',
     .               ' ')
      call prnt(7,26,rdata,'output data=.')
c     line=1,samp=1,lati=50.,long=360. (or 0.)
      return
      end
$!-----------------------------------------------------------------------------
$ create trectpatch.imake
/* Imake file for Test of VICAR subroutine rectpatch */

#define PROGRAM trectpatch

#define MODULE_LIST trectpatch.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create trectpatch.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstrectpatch.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
trectpatch
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create rectpatch.hlp
1 RECTPATCH
  For simple cylindrical projection, returns line of equator, longitude
of sample 1.

  FORTRAN Calling Sequence: 

  CALL RECTPATCH(RDATA)

     where:

  RDATA is 40 word real*4 array containing geometry data as specified in
  subroutine CONVEV.  Elements 1,2,3 and 6 will be changed.
2 History
  Original Programmer: Joel Mosher 19-APR-1986
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: FORTRAN
  Ported to UNIX: Steve Pohorsky

  REVISION HISTORY
    2-4-93   SP   Minor cleanup: Changed (RLON.gE.360.0001) to (RLON .GT. 360.0)
                  and changed 3.14159265358 to 3.141592653589793D0
2 Operation
 RECTPATCH uses data in RDATA to calculate line of equator and longitude of 
 sample 1.  Elements 1,2,3 and 6 will be changed accordingly.

 RECTPATCH is intended for use only on rectangular projections, so if 
 RDATA(39) is not 10, then it returns without any processing.  (Please note that
 RDATA(39) is stored in INTEGER format and thus is not equal to 10.0.)
$ Return
$!#############################################################################
