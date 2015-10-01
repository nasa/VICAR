$!****************************************************************************
$!
$! Build proc for MIPL module mercpatch
$! VPACK Version 1.5, Monday, February 08, 1993, 15:54:52
$!
$! Execute by entering:		$ @mercpatch
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
$ write sys$output "*** module mercpatch ***"
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
$   if F$SEARCH("mercpatch.imake") .nes. ""
$   then
$      vimake mercpatch
$      purge mercpatch.bld
$   else
$      if F$SEARCH("mercpatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mercpatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mercpatch.bld "STD"
$   else
$      @mercpatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mercpatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mercpatch.com -
	-s mercpatch.f -
	-i mercpatch.imake -
	-t tmercpatch.f tmercpatch.imake tmercpatch.pdf tstmercpatch.pdf -
	-o mercpatch.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mercpatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine mercpatch(rdata)
c subroutine to calulate lat,long at line 1,sample 1 for mercator projection
c It updates rdata buffer elements 1 and 2.

      real scl,rdata(40),rad,req,leq,pi,smp,alog,atan
      data pi/3.1415926/
C==================================================================
      call mve(4,1,rdata(39),l,1,1)
      if(l.ne.6)return
      SCL=RDATA(7)
      RAD=57.29578
      REQ=RDATA(26)
      IF(RDATA(2).NE.1.)THEN
C        FIND LINE OF EQUATOR
         LEQ=(REQ/SCL)*ALOG(TAN(PI/4.+RDATA(3)/(2.*RAD)))+RDATA(2)
C        FIND LATITUDE OF LINE 1.
         RDATA(3)=(2.*(ATAN(EXP(SCL*(LEQ-1.)/REQ))-PI/4.)*RAD)
         RDATA(2)=1.
      ENDIF
      IF(RDATA(1).NE.1.)THEN
C        FIND SAMPLE OF PRIME MERIDIAN
         SMP=(REQ/SCL)*(RDATA(6)/RAD)+RDATA(1)
C        FIND LONGITUDE OF SAMPLE 1.
         RDATA(6)=((SMP-1.)*SCL*RAD)/REQ
         RDATA(1)=1.
      ENDIF
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mercpatch.imake
/* Imake file for VICAR subroutine mercpatch */

#define SUBROUTINE mercpatch

#define MODULE_LIST mercpatch.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tmercpatch.f
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real    rdata(40)
      integer idata(40)
      equivalence(idata,rdata)
      idata(39)=6       !mercator
      rdata(1)=50.      !sample
      rdata(2)=19.34389 !line
      rdata(3)=0.       !lat
      rdata(6)=205.3172 !west long
      rdata(7)=100.     !scale
      rdata(25)=1815.   !io radii
      rdata(26)=rdata(25)
      call prnt(7,26,rdata,'input data=.')
      call XVMESSAGE
     *('input lat=0.,long=205.3172,line=19.34389,sample=50.',' ')
      call XVMESSAGE('input is mercator, scale=100.,target is io',' ')
      call mercpatch(rdata)
      call prnt(7,1,rdata(1),'sample  = .')
      call prnt(7,1,rdata(2),'line    = .')
      call prnt(7,1,rdata(3),'latitude= .')
      call prnt(7,1,rdata(6),'longitude=.')
      call prnt(7,26,rdata,'output data=.')
      call XVMESSAGE
     *('output should be line=1.,samp=1.,lati=50.,long=360.',' ')
c     line=1,samp=1,lati=50.,long=360. (or 0.)
      return
      end
$!-----------------------------------------------------------------------------
$ create tmercpatch.imake
/* Imake file for Test of VICAR subroutine mercpatch */

#define PROGRAM tmercpatch

#define MODULE_LIST tmercpatch.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tmercpatch.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstmercpatch.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tmercpatch
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mercpatch.hlp
1 MERCPATCH
  Corrects data buffer for Mercator projections.

  Fortran Calling sequence:  CALL MERCPATCH(DATA)

  where DATA(40) is a real*4 buffer defined in the SEARCV2 help file.

  Note: elements 1,2,3,and 6 of DATA may be changed after the call.
2 History
  Original Programmer: Joel Mosher,  13 April 1986
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: FORTRAN
  Ported to UNIX: Steve Pohorsky

2 Operation
  Map2 labels require that the latitude and longitude values are for
line=1, sample=1. Sometimes programs obtain latitudes and longitudes
for other lines/samples. Passing these values to MERCPATCH via the DATA
buffer changes these values to line 1, sample 1 and the corresponding
latitudes and longitudes.

$ Return
$!#############################################################################
