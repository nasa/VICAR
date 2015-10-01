$!****************************************************************************
$!
$! Build proc for MIPL module fomclv
$! VPACK Version 1.5, Friday, January 15, 1993, 10:34:53
$!
$! Execute by entering:		$ @fomclv
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
$ write sys$output "*** module fomclv ***"
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
$   if F$SEARCH("fomclv.imake") .nes. ""
$   then
$      vimake fomclv
$      purge fomclv.bld
$   else
$      if F$SEARCH("fomclv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fomclv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fomclv.bld "STD"
$   else
$      @fomclv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fomclv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fomclv.com -
	-s fomclv.f -
	-i fomclv.imake -
	-t tfomclv.f tfomclv.imake tfomclv.pdf tstfomclv.pdf -
	-o fomclv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fomclv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
      Subroutine  FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
c
C     11 JAN 78   ...JJL...       INITIAL RELEASE
c     15 Jan 93   ...W.P. Lee...  Ported for UNIX Conversion
C
C     SET UP RETICLE DESCRIPTIVE ARRAY A AND CALL FOMCAL
C
C     AA CONTAINS NPOINT ENTRYS OF
C          AA(1,J) LINE VALUE OF JTH RETICLE
C          AA(2,J) SAMPLE VALUE
C          AA(3,J) LATITUDE
C          AA(4,J) LONGITUDE
C
      Real*4  AA(4,20),A(5,20),B(6)
      Real*8  OM(3,3)
      Real*8  RS(3)
      Real*4  CL, CS
c
      Do 100 I = 1, NPOINT
       A(1,I) = AA(1,I)
       A(2,I) = AA(2,I)
       A(3,I) =  B(3)
       A(4,I) = AA(3,I)
       A(5,I) = AA(4,I)
100   Continue
C
      Call FOMCAV(IND,NPOINT,A,B,OM,RS,CL,CS)
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fomclv.imake
/* Imake file for VICAR subroutine  FOMCLV  */

#define SUBROUTINE  fomclv

#define MODULE_LIST  fomclv.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tfomclv.f
        Include  'VICMAIN_FOR'
c
        Subroutine  Main44
c
C  THIS IS A TEST PROGRAM FOR SUBROUTINES FOMCLV AND FOMCAV.
C  FOMCAV COMPUTES THE OMMATRIX, THE PLANET TO CAMERA TRANSFORMATION
C  MATRIX, AND THE RSVECTOR, THE PLANET TO CAMERA POSITION VECTOR
C  EXPRESSED IN THE PLANET COORDINATE SYSTEM.  FOMCAV IS ACCESSED
C  THROUGH FOMCLV, WHICH MERELY SETS UP THE RETICLE DESCRIPTIVE
C  ARRAY FROM THE INPUT ARRAYS AA AND B, AND THEN CALLS FOMCAV.
C  THE CONSTANTS FOR THE PLANET AND SPACECRAFT DESCRIPTIVE ARRAY,
C  B, REFER TO THE JUPITER MOON, IO.  
c
        Real*4     AA(4,20)
c   , A(5,20), 
        Real*4     B(6), CL, CS
        Real*8     OM(3,3), RS(3)
        Integer*4  IND, NPOINT
c 
C  PLANET AND SPACECRAFT DESCRIPTIVE ARRAY
c
        B(1) = 1500.19 * 84.821
        B(2) = 0.
	B(3) = 806061.
	B(4) = -.02
	B(5) = 155.07
	B(6) = 1815. 
c 
C  LINE/SAMPLE OF CAMERA AXIS
c      	
	CL = 500.
	CS = 500.
c
C  IMAGE POINTS (LINE, SAMPLE, LATITUTE, LONGITUDE)
c
	AA(1,1) = 381.86
        AA(2,1) = 382.64
	AA(3,1) = 19.35
	AA(4,1) = 229.21 
c
	AA(1,2) = 382.17
	AA(2,2) = 498.94
	AA(3,2) = 25.83
	AA(4,2) = 190.00
c 
	AA(1,3) = 381.98
	AA(2,3) = 615.31
	AA(3,3) = 32.86
	AA(4,3) = 163.58
c 
        NPOINT = 3
c  
        CALL QPRINT(' TEST USING THREE IMAGE POINTS',30)
	CALL QPRINT('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',46)
        CALL PRNT(7, 12, AA, '.')
        CALL QPRINT('  ',2)
c
	CALL FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	CALL PRNT(8,9,OM,' OMMATRIX = .')
	CALL PRNT(8,3,RS,' RSVECTOR = .')
        CALL PRNT(4,1,IND,' IND =.')
        CALL QPRINT('  ',2)
	CALL QPRINT(' ********************',21)
c
        CALL QPRINT('  ',2)
        AA(1,4) = 498.40
  	AA(2,4) = 498.49
	AA(3,4) = 2.539
	AA(4,4) = 179.32
c
        AA(1,5) = 498.53
        AA(2,5) = 615.11
        AA(3,5) = 8.65
        AA(4,5) = 156.13
c
        AA(1,6) = 497.52
      	AA(2,6) = 732.24
    	AA(3,6) = 15.14
	AA(4,6) = 132.25
c
  	NPOINT = 6
c   
        CALL QPRINT('  ',2)
        CALL QPRINT(' TEST USING SIX IMAGE POINTS',28)
	CALL QPRINT('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',46)
        CALL PRNT(7, 24, AA, '.')
        CALL QPRINT('  ',2)
c
	CALL FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	CALL PRNT(8,9,OM,' OMMATRIX = .')
	CALL PRNT(8,3,RS,' RSVECTOR = .')
        CALL PRNT(4,1,IND,' IND =.')
        CALL QPRINT('  ',2)
c  
  	AA(1,7) = 613.50
    	AA(2,7) = 380.84
	AA(3,7) = -26.74
	AA(4,7) = 206.78
c
 	AA(1,8) = 613.95
	AA(2,8) = 497.92
	AA(3,8) = -20.18
	AA(4,8) = 174.26
c
	AA(1,9) = 614.02
	AA(2,9) = 614.04
	AA(3,9) = -13.80
	AA(4,9) = 150.12
c
	AA(1,10) = 613.13
	AA(2,10) = 731.88
	AA(3,10) = -7.33
	AA(4,10) = 126.19
c	
	AA(1,11) = 729.89
	AA(2,11) = 496.11
	AA(3,11) = -47.37
	AA(4,11) = 172.84
c	
	AA(1,12) = 729.79
	AA(2,12) = 613.04
	AA(3,12) = -38.93
	AA(4,12) = 141.21
c
	AA(1,13) = 728.78
	AA(2,13) = 730.46
	AA(3,13) = -31.16
	AA(4,13) = 112.36
c
  	NPOINT = 13
c   
	CALL QPRINT(' ********************',21)
        CALL QPRINT('  ',2)
        CALL QPRINT(' TEST USING THIRTEEN IMAGE POINTS',33)
	CALL QPRINT('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',46)
        CALL PRNT(7, 52, AA, '.')
        CALL QPRINT('  ',2)
c
	CALL FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	CALL PRNT(8,9,OM,' OMMATRIX = .')
	CALL PRNT(8,3,RS,' RSVECTOR = .')
        CALL PRNT(4,1,IND,' IND = .')
        CALL QPRINT('  ',2)
	CALL QPRINT(' ********************',21)
        CALL QPRINT('  ',2)
c
	CALL QPRINT(' TEST ERROR RETURN (IND = 3):',29)
        CALL QPRINT('  ',2)
c
	AA(1,1) = 346
	AA(2,1) = 432
	AA(3,1) = 45
	AA(4,1) = 63
c
	AA(1,2) = 479
	AA(2,2) = 316
	AA(3,2) = 120
	AA(4,2) = 90
c	
	AA(1,3) = 723
	AA(2,3) = 529
	AA(3,3) = 80
	AA(4,3) = 62
c 
	CALL QPRINT('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',46)
        CALL PRNT(7, 12, AA, '.')
        CALL QPRINT('  ',2)
c
	NPOINT = 3
c
	CALL FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	CALL PRNT(8,9,OM,' OMMATRIX =.')
	CALL PRNT(8,3,RS,' RSVECTOR =.')
	CALL PRNT(4,1,IND,' IND = .')
c
	CALL QPRINT(' ********************',21)
        CALL QPRINT('  ',2)
        CALL ZIA(AA, 80)
	CALL QPRINT('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',46)
        CALL PRNT(7,12,AA, '.')
        CALL QPRINT('  ',2)
	CALL FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	CALL PRNT(8,9,OM,' OMMATRIX = .')
	CALL PRNT(8,3,RS,' RSVECTOR = .')
	CALL PRNT(4,1,IND,' IND = .')
c 
        Return
	End
$!-----------------------------------------------------------------------------
$ create tfomclv.imake
/* IMAKE file for Test of VICAR subroutine  FOMCLV  */

#define PROGRAM  tfomclv

#define MODULE_LIST tfomclv.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/* #define   LIB_LOCAL */     /*  Disable during delivery   */
#define   LIB_P2SUB         
#define   LIB_MATH77  
$!-----------------------------------------------------------------------------
$ create tfomclv.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstfomclv.pdf
Procedure
Refgbl $Echo
Body
Let _onfail="Continue"
Let $Echo="Yes"
WRITE "THIS IS A SCRIPT FOR TESTING SUBROUTINES FOMCLV AND"
WRITE "FOMCAV, USING THE TEST PROGRAM TFOMCLV.  FOMCAV" 
WRITE "COMPUTES THE OMMATRIX, THE PLANET TO CAMERA TRANSFORMATION"
WRITE "MATRIX, AND THE RSVECTOR, THE PLANET TO CAMERA POSITION"
WRITE "VECTOR EXPRESSED IN THE PLANET COORDNATE SYSTEM.  FOMCAV" 
WRITE "IS ACCESSED THROUGH FOMCLV, WHICH MERELY SETS UP THE" 
WRITE "RETICLE DESCRIPTIVE ARRAY A FROM THE INPUT ARRAYS AA AND"
WRITE "B, AND THEN CALLS FOMCAV.  THE CONSTANTS FOR THE" 
WRITE "PLANET AND SPACECRAFT DESCRIPTIVE ARRAY B REFER TO THE" 
WRITE "JUPITER MOON, IO.  AFTER EACH CALL TO FOMCLV, THE VALUES"
WRITE "FOR THE INPUT ARRAY, AA, THE COMPUTED OMMATRIX, THE RSVECTOR,"
WRITE "AND THE RETURN CODE, WHICH IS 0 FOR NORMAL RETURN, AND 1"
WRITE "OR 3 FOR ERROR RETURNS, ARE PRINTED."
TFOMCLV
Let $Echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create fomclv.hlp
1 FOMCLV

        Subroutine which sets up array for and calls FOMCAV.

2 USAGE

  Calling Sequence:  call fomclv(ind,npoint,aa,b,om,rs,cl,cs)

  Arguments:  IND     (i*4,return)  0 for normal return, 1 and 3 for error return
              NPOINT  (i*4,entry)  number of sets of line, sample,
                                   lat, long in buffer AA
              AA(100) (r*4,entry)  buffer of points
              B(6)    (r*4,entry)  buffer of constants defined below
                   B(1) = camera focal length, pixels
                   B(2) = planet radius at equator - radius at pole, km
                   B(3) = distance planet center to spacecraft, km
                   B(4) = latitude of sub spacecraft point, degrees
                   B(5) = longitude of sub spacecraft point, degrees west
                   B(6) = equatorial radius, km
              OM(9)   (r*8,return)  OMMATRIX
              RS(3)   (r*8,return)  RSVECTOR
              CL      (r*4,entry)  Optical axis line, object space pixels
              CS      (r*4,entry)  Optical axis sample, object space pixels

2 HISTORY

  Original Programmer:  J. J. Lorre, 16 June 1977
  Current Cognizant Programmer: ...
  Source Language: Fortran

  Ported for UNIX Conversion:  W.P. Lee,  Jan-15-1993 

2 OPERATION

	AA contains NPOINT entries of
           AA(1,J) LINE VALUE OF JTH RETICLE
           AA(2,J) SAMPLE VALUE
           AA(3,J) LATITUDE
           AA(4,J) LONGITUDE
 	The reticle descriptive array A is set up from 
	values in AA and B(3) and FOMCAV is called.

$ Return
$!#############################################################################
