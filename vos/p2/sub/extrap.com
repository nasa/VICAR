$!****************************************************************************
$!
$! Build proc for MIPL module extrap
$! VPACK Version 1.7, Saturday, July 10, 1993, 19:15:30
$!
$! Execute by entering:		$ @extrap
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
$ write sys$output "*** module extrap ***"
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
$ write sys$output "Invalid argument given to extrap.com file -- ", primary
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
$   if F$SEARCH("extrap.imake") .nes. ""
$   then
$      vimake extrap
$      purge extrap.bld
$   else
$      if F$SEARCH("extrap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake extrap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @extrap.bld "STD"
$   else
$      @extrap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create extrap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack extrap.com -
	-s extrap.f -
	-i extrap.imake -
	-t textrap.f textrap.imake textrap.pdf tstextrap.pdf -
	-o extrap.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create extrap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
      SUBROUTINE EXTRAP(N,L,SS,ES,PTS,BUF,RM)
C
C-----THIS IS A FORTRANIZED VERSION OF EXTRAP
      IMPLICIT INTEGER (A-Z)
      REAL*4 R,R1,R2,NOOM,DENO
      INTEGER*2 PTS(3,N),BUF(1)
C
      RMAX = RM
      K = 0
C
      DO 10 J=SS,ES
      K = K + 1
      BUF(K) = 0
      NOOM = 0.0
      DENO = 0.0
        DO 5 I=1,N
        S = I
        R1 = J - PTS(1,I)
        R2 = L - PTS(2,I)
        R = R1**2 + R2**2
        IF(R .EQ. 0.0) GO TO 6
        IF(R .GT. RMAX) GO TO 5
        NOOM = NOOM + PTS(3,I) / R
        DENO = DENO + 1./R
    5   CONTINUE
C
      IF(NOOM .EQ. 0.0 .OR. DENO .EQ. 0.0) GO TO 10
      BUF(K) = NOOM / DENO + 0.5
      GO TO 10
    6 BUF(K) = PTS(3,S)
   10 CONTINUE
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create extrap.imake
/* Imake file for VICAR subroutine EXTRAP */

#define SUBROUTINE extrap

#define MODULE_LIST extrap.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create textrap.f
!
!     This is the test program for EXTRAP
!
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C-----THIS IS A TEST PROGRAM FOR MODULE  EXTRAP
C-----EXTRAP WILL CALCULATE VALUES FOR THE DNS OF A LINE SEGMENT
C-----BASED ON THE VALUES OF OTHER POINTS IN THE PICTURE.
C-----THESE OTHER POINTS ARE STORED IN ARRAY PTS.
C-----THIS PROGRAM ASSUMES THAT AN 8 X 8 IMAGE IS INPUT.
C-----EXTRAP IS BEING TOLD TO INTERPOLATE OVER THE AREA
C-----(4,4,3,3)
        INTEGER*4 IUNIT,OUNIT1,OUNIT2,NL,NS
	INTEGER*2 LINE(20)
	INTEGER*2 PTS(48)/3,3,5,4,3, 6,5,3, 7,6,3, 8,7,3, 9,
     .			  3,4,6,                     7,4,10,
     .			  3,5,7,                     7,5,11,
     .			  3,6,8,                     7,6,12,
     .			  3,7,9,4,7,10,5,7,11,6,7,12,7,7,13/
C
	MAX1 = 25
	MAX2 = 10000000

C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
      CALL XVGET(IUNIT,STAT,'NL',NL,'NS',NS,' ')
C        OPEN OUTPUT DATA SETS
      CALL XVUNIT(OUNIT1,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT1,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'O_FORMAT','BYTE',' ')
      CALL XVUNIT(OUNIT2,'OUT',2,STAT,' ')
      CALL XVOPEN(OUNIT2,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'O_FORMAT','BYTE',' ')

      DO L=1,NL
          CALL XVREAD(IUNIT,LINE,STAT,'LINE',L,' ')
          CALL XVWRIT(OUNIT1,LINE,STAT,'LINE',L,'NSAMPS',NS,' ')
          CALL XVWRIT(OUNIT2,LINE,STAT,'LINE',L,'NSAMPS',NS,' ')
      END DO

C        CLOSE OUTPUT DATA SETS AND RE-OPEN FOR UPDATE
      CALL XVCLOSE(OUNIT1,STAT,' ')
      CALL XVCLOSE(OUNIT2,STAT,' ')
      CALL XVOPEN(OUNIT1,STAT,'OP','UPDATE','U_FORMAT','HALF',
     &            'O_FORMAT','BYTE',' ')
      CALL XVOPEN(OUNIT2,STAT,'OP','UPDATE','U_FORMAT','HALF',
     &            'O_FORMAT','BYTE',' ')
C
      DO 20 L=4,6
	   CALL EXTRAP(16,L,4,6,PTS,LINE,MAX1)
           CALL XVWRIT(OUNIT1,LINE,STAT,'LINE',L,
     &                 'SAMP',4,'NSAMPS',3,' ')
	   CALL EXTRAP(16,L,4,6,PTS,LINE,MAX2)
           CALL XVWRIT(OUNIT2,LINE,STAT,'LINE',L,
     &                 'SAMP',4,'NSAMPS',3,' ')
 20   CONTINUE

      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT1,STAT,' ')
      CALL XVCLOSE(OUNIT2,STAT,' ')

      STOP
      END
$!-----------------------------------------------------------------------------
$ create textrap.imake
/* Imake file for Test of VICAR subroutine TEXTRAP */

#define PROGRAM textrap

#define MODULE_LIST textrap.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create textrap.pdf
PROCESS
PARM INP TYPE=STRING
PARM OUT TYPE=STRING COUNT=2
END-PROC
$!-----------------------------------------------------------------------------
$ create tstextrap.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!This is a test of module EXTRAP (subroutine)
!the input to the test program will be a picture with
!a square hole. The output should have had the hole
!interpolated over.

!Generate image b with the shown DN values
genthis out=b nl=8 ns=8 +
	dn=( 1, 2, 3, 4, 5, 6, 7, 8, +
	     2, 3, 4, 5, 6, 7, 8, 9, +
	     3, 4, 5, 6, 7, 8, 9,10, +
	     4, 5, 6, 0, 0, 0,10,11, +
	     5, 6, 7, 0, 0, 0,11,12, +
	     6, 7, 8, 0, 0, 0,12,13, +
	     7, 8, 9,10,11,12,13,14, +
	     8, 9,10,11,12,13,14,15 )

list b

!create outputs using no radius and using large radius.
textrap inp=b out=(e1,e2)

!print result which used no radius
list e1

!print result which used large radius (should be same)
list e2

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create extrap.hlp
1 EXTRAP

  EXTRAP is an interpolation routine which may be used to generate an
  image, or an area of an image, from a set of randomly spaced data
  points.

2 CALLING SEQUENCE 

  CALL EXTRAP(N,L,SS,ES,PT,BUFF,RAD2)

2 ARGUMENTS

  PT	   is an INTEGER*2 input array containing the data points.
	   Each data point is stored as a triplet of the form
	   (S(i),L(i),D(i)) consisting of its sample and line 
	   coordinates and DN value, respectively.

  N	   is the number of data points in PT.

  BUF	   is an INTEGER*2 output array.  Upon return, BUF will
	   contain a line segment generated by interpolating over
	   the elements of PT.

  L,SS,ES  are the line, starting sample, and ending sammple coor-
	   dinates of the line segment returned in BUF.

  RAD2     specifies the square of the radius of an area about each
	   output pixel within which tiepoints will be used for
	   purposes of interpolation.  RAD2 is required.

  The arguments L, SS, ES, and RAD2 are input as INTEGER*4 quantities.



2 OPERATION

  Each Call to EXTRAP will generate an array consisting of the inter-
  polated grey values for the line segment specified by L,SS, and ES.
  Let (L,S) be the coordinates of a point of the line segment
  (SS<=S<=ES) and let D represent its interpolated grey value.  Then

                      __ D(i)
                      \  ___
                      /_ R^2(i)
                     ----------
                      __  1
                      \  ---
                      /_ R^2(i)

  where R(i) is the distance from (L(i),S(i)) to (L,S), and the 
  summation is performed over all data points for which R^2(i) <= RAD2.

2 EXAMPLES

  For examples of usage, consult the source listings for the VICAR
  application programs STERMAP, RAMSAR, and RESSAR77.

2 RESTRICTIONS

  Core requirements: 250 bytes.  
   
2 HISTORY

  Original Programmer: Bill Benton
  Current Cognizant Programmer: Gary Yagi
  Source Language: Fortran
  Revision:  New, 27 August 1980

	09 July 1993	JFM	Made to run on both UNIX and VMS systems.
				RAD2 is made a required parameter.
				Test file slightly revised.
$ Return
$!#############################################################################
