$!****************************************************************************
$!
$! Build proc for MIPL module momatv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:28:40
$!
$! Execute by entering:		$ @momatv
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
$ write sys$output "*** module momatv ***"
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
$ write sys$output "Invalid argument given to momatv.com file -- ", primary
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
$   if F$SEARCH("momatv.imake") .nes. ""
$   then
$      vimake momatv
$      purge momatv.bld
$   else
$      if F$SEARCH("momatv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momatv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momatv.bld "STD"
$   else
$      @momatv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momatv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momatv.com -mixed -
	-s momatv.f -
	-i momatv.imake -
	-t tmomatv.f tmomatv.imake tmomatv.pdf tstmomatv.pdf -
	-o momatv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create momatv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c/*      Oct 92   ...WPL...   Convert to UNIX */
C/*   10 AUG 83   ...CCA...   CONVERT TO VAX */
C/*   11 JAN 78   ...JJL...   INITIAL RELEASE */
c
      SUBROUTINE MOMATV(L0,S0,LSSP,SSSP,PIXPMM,FOC,BL,PHI,THT,VABS,
     #MATRIX,VECTOR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 VECTOR(3)
      REAL*8 MATRIX(3,3),LL,L0,LSSP
c     LOGICAL*1 IO(36)/' ','I','N','P','U','T',' ','T','O',' ','M','0',
c    &'M','A','T','-','-','S','S','C','P','T','=','(',' ',' ',' ',' ',
c    &' ',',',' ',' ',' ',' ',' ',')'/
      Character*80 IO
      Data  IO /'INPUT TO MOMAT--SSCPT=(     ,     )'/
C
C   ********************************************************************
C   *                                                                  *
C   * THIS ROUTINE COMPUTES THE PLANET TO CAMERA ROTATION MATRIX AND   *
C   *    THE VECTOR FROM PLANET CENTER TO SPACECRAFT, EXPRESSED IN THE *
C   *    PLANET SYSTEM.  A VECTOR IN THE CAMERA SYSTEM, VPR, IS THEN   *
C   *    GIVEN IN TERMS OF THE SAME VECTOR IN THE PLANET SYSTEM, V, BY *
C   *                  VPR = (MATRIX)*(V-VECTOR)                       *
C   *                                                                  *
C   *                           INPUT VARIABLES                        *
C   *   NAME             TYPE                MEANING                   *
C   *                                                                  *
C   *    L0               R*8   LINE COORDINATE OF CAMERA AXIS         *
C   *    S0               R*8   SAMPLE COORDINATE OF CAMERA AXIS       *
C   *   LSSP              R*8   LINE COORDINATE OF SUBSPACECRAFT POINT *
C   *   SSSP              R*8   SAMPLE COORD.   OF SUBSPACECRAFT POINT *
C   *       (ALL OF THE ABOVE FOR DISTORTION-CORRECTED PICTURES)       *
C   *   PIXPMM            R*8   NUMBER OF PIXELS PER MM                *
C   *   FOC               R*8   CAMERA FOCAL LENGTH, MM                *
C   *    BL               R*8   WEST LONGITUDE OF SSP--DEGREES         *
C   *   PHI               R*8   LATITUDE OF SSP--DEGREES               *
C   *   THT (SEE SPECIAL  R*8   ANGLE OF PLANETARY SPIN AXIS (NORTH),  *
C   *        CASE LIST)         MEASURED IN PICTURE PLANE AT SSP,      *
C   *                           CLOCKWISE FROM UP--DEGREES             *
C   *   VABS              R*8   DISTANCE TO PLANET FROM S/C--KM        *
C   *                                                                  *
C   *                    OUTPUT VARIABLES                              *
C   *   MATRIX(3,3)       R*8   ROTATION MATRIX FROM PLANET TO CAMERA  *
C   *   VECTOR(3)         R*8   TRANSLATION VECTOR, IN PLANET SYSTEM
C   *                                                                  *
C   * THE FOLLOWING SPECIAL CASES CAN BE HANDLED BY THE PROGRAM IF THE *
C   * INPUT VARIABLES ARE PROPERLY SET                                 *
C   *               CASE 1 THE SSP IS A POLE, NOT IN THE PICTURE CENTER*
C   *          PROCEDURE   BL IS MEANINGLESS AND NOT LOOKED AT.  THT AS*
C   *                      DEFINED ABOVE IS ALSO MEANINGLESS.  IT WILL *
C   *                      BE ASSUMED TO CONTAIN A POSITIVE NUMBER     *
C   *                      WHICH EQUALS THE WEST LOMGITUDE OF THE      *
C   *                      PICTURE CENTER.                             *
C   *                                                                  *
C   *               CASE 2 THE SSP IS BOTH A POLE AND THE PICTURE      *
C   *                      CENTER                                      *
C   *          PROCEDURE   THE ROTATION IS TRIVIAL, BUT BL & THT ARE   *
C   *                      AGAIN MEANINGLESS, SO ADDITIONAL INFORMATION*
C   *                      IS REQUIRED.  SET THT (NORANG) TO A POSITIVE*
C   *                      NUMBER EQUALLING THE W LONGITUDE OF THE UP- *
C   *                      GOING MERIDIAN IN THE INPUT PICTURE.        *
C   *                                                                  *
C   *                                                                  *
C   * NOTE--CASES SUCH AS THE SSP BEING IN PICTURE CENTER BUT NOT A    *
C   *      POLE, OR POLE BEING IN PICTURE CENTER BUT NOT THE SSP, DO   *
C   *       NOT NEED SPECIAL HANDLING.                                 *
C   * NOTE--IN ALL CASES, VARIABLES OTHER THAN THT AND BL MUST BE      *
C   *       PROPERLY SET.                                              *
C   *                                                                  *
C   ********************************************************************
C
C
c     CALL OUTCON(SNGL(LSSP),IO(29),5,0)
c
      II = NINT(LSSP)
      JJ = NINT(SSSP)
      Write (IO(24:28),  '(I5)')  II
      Write (IO(30:34),  '(I5)') JJ
c     CALL OUTCON(SNGL(SSSP),IO(35),5,0)
c     CALL QPRINT(IO,36)
      Call Xvmessage(IO, ' ')
c
C     INITIALIZE, AND GET ALL ANGLES IN RADIANS
C
      PI=4.D0*DATAN(1.D0)
      DEGRAD=PI/180.D0
      BL=BL*DEGRAD
      PHI=PHI*DEGRAD
      THT=THT*DEGRAD
C
C     HERE COMPUTE LL, ANGLE BETWEEN ORIGINAL X-AXIS AND PROJECTION OF S
C     IN IMAGE PLANE, AND R, ANGLE AT CAMERA OBJECTIVE BETWEEN SSP AND O
C     AXIS.  BRANCH TO 100 IF OPTICAL AXIS POINTS TO SSP
C
      CONRAD = 1.D0/FOC/PIXPMM
      QLDIF=LSSP-L0
      QSDIF=SSSP-S0
      IF(QSDIF.EQ.0.D0) GO TO 13
      LL=DATAN2(QLDIF,QSDIF)
      GO TO 14
13    IF(QLDIF.EQ.0.D0) GO TO 100
      LL=DSIGN(PI/2.D0,QLDIF)
      GO TO 14
100   LL=0.D0
14    R=CONRAD*DSQRT(QLDIF*QLDIF+QSDIF*QSDIF)
      IF(DABS(PHI).EQ.PI/2.D0.AND.R.EQ.0.D0) GO TO 107
C
C     COMPUTE SINES AND COSINES OF ALL ANGLES, TEST FOR CASE 3
C
      SINLL=DSIN(LL)
      COSLL=DCOS(LL)
      SINR=DSIN(R)
      COSR=DCOS(R)
      IF(DABS(PHI).EQ.PI/2.D0) GO TO 101
      SINPHI=DSIN(PHI)
      COSPHI=DCOS(PHI)
      SINBL=DSIN(BL)
      COSBL=DCOS(BL)
C
C     IF PROGRAM GETS HERE, THE SSP IS NOT A POLE.  IN SUCH A CASE FIVE
C     SEPARATE ROTATIONS FROM CAMERA TO PLANET SYSTEM ARE REQUIRED.  THE
C
C     #1) +LL ABOUT Z-AXIS (+ MEANS COUNTERCLOCKWISE VIEWED FROM POSITIV
C                                END OF AXIS OF ROTATION)
C     #2) +R ABOUT Y-AXIS
C     #3) -PSI ABOUT Z-AXIS
C     #4) 90 + PHI ABOUT X-AXIS
C     #5) 90 + BL ABOUT Z-AXIS
C
C     ALL ANGLES HAVE BEEN PREVIOUSLY EXPLAINED EXCEPT PSI.  PSI IS THE
C     ANGLE BETWEEN UP AND NORTH MEASURED AT THE SSP IN THE IMAGE PLANE
C     AS IT STANDS AFTER THE SECOND ROTATION.  IF IT WEREN'T FOR R2,
C     PSI WOULD = (THT-LL), BUT NOW THAT RELATION IS ONLY APPROXIMATE,
C     WITH THE ERROR THE ORDER OF R.  THE NEXT ORDER OF BUSINESS IS TO
C     COMPUTE PSI.
C
      IF(R.EQ.0.D0) GO TO 400
      IF(DMOD(LL-THT+2.D0*PI,2.D0*PI).EQ.1.5D0*PI) GO TO 200
      DEL=DASIN(-SINR*DCOS(THT-LL)/COSPHI)
      A=DSIN((R+PI/2.D0-PHI)/2.D0)
      B=DSIN((R-PI/2.D0+PHI)/2.D0)
      C=DTAN((PI/2.D0+DEL-THT+LL)/2.D0)
      IF(A*C.EQ.0.D0) GO TO 1
      PSI=PI/2.D0+DATAN(B/A/C)*2.D0
      GO TO 2
C     NP VECTOR ANTI-PARALLEL TO CENTER-SSP LINE
1     PSI=PI/2.D0
      GO TO 2
C     NP VECTOR PARALLEL TO CENTER-SSP LINE
200   PSI=1.5D0*PI
      GO TO 2
C     SSP IN PICTURE CENTER
400   PSI=-THT
2     SINPSI=DSIN(PSI)
      COSPSI=DCOS(PSI)
C
C     FILL THE OMMATRIX.  CONVENTIONS: (I,J) = I ROW, J COLUMN.  GIVEN M
C     MULTIPLIES FROM LEFT ON COLUMN VECTORS, GOES FROM PLANET TO CAMERA
C     SYSTEMS.
C
      MATRIX(1,1)=+COSLL*COSR*COSPSI*SINBL+COSLL*COSR*SINPSI*SINPHI*COSB
     #L-COSLL*SINR*COSPHI*COSBL+SINLL*SINPSI*SINBL-SINLL*COSPSI*SINPHI*
     #COSBL
C
      MATRIX(2,1)=+SINLL*COSR*COSPSI*SINBL+SINLL*COSR*SINPSI*SINPHI*COSB
     #L-SINLL*SINR*COSPHI*COSBL-COSLL*SINPSI*SINBL+COSLL*COSPSI*SINPHI*
     #COSBL
C
      MATRIX(3,1)=-SINR*COSPSI*SINBL-SINR*SINPSI*SINPHI*COSBL-COSR*COSPH
     #I*COSBL
C
      MATRIX(1,2)=+COSLL*COSR*COSPSI*COSBL-COSLL*COSR*SINPSI*SINPHI*SINB
     #L+COSLL*SINR*COSPHI*SINBL+SINLL*SINPSI*COSBL+SINLL*COSPSI*SINPHI*
     #SINBL
C
      MATRIX(2,2)=+SINLL*COSR*COSPSI*COSBL-SINLL*COSR*SINPSI*SINPHI*SINB
     #L+SINLL*SINR*COSPHI*SINBL-COSLL*SINPSI*COSBL-COSLL*COSPSI*SINPHI*
     #SINBL
C
      MATRIX(3,2)=-SINR*COSPSI*COSBL+SINR*SINPSI*SINPHI*SINBL+COSR*COSPH
     #I*SINBL
C
      MATRIX(1,3)=-COSLL*COSR*SINPSI*COSPHI-COSLL*SINR*SINPHI+SINLL*COSP
     #SI*COSPHI
C
      MATRIX(2,3)=-SINLL*COSR*SINPSI*COSPHI-SINLL*SINR*SINPHI-COSLL*COSP
     #SI*COSPHI
C
      MATRIX(3,3)=+SINR*SINPSI*COSPHI-COSR*SINPHI
C
C     COMPUTE VECTOR FROM PLANET CENTER TO S/C, IN CAMERA SYSTEM
C
3     VECTOR(1)=-VABS*COSLL*SINR
      VECTOR(2)=-VABS*SINLL*SINR
      VECTOR(3)=-VABS*COSR
C
C     ROTATE VECTOR TO PLANET SYSTEM
C
40    A=VECTOR(1)
      B=VECTOR(2)
      C=VECTOR(3)
      VECTOR(1)=MATRIX(1,1)*A+MATRIX(2,1)*B+MATRIX(3,1)*C
      VECTOR(2)=MATRIX(1,2)*A+MATRIX(2,2)*B+MATRIX(3,2)*C
      VECTOR(3)=MATRIX(1,3)*A+MATRIX(2,3)*B+MATRIX(3,3)*C
      RETURN
C
C     COME HERE IF SSP IS A PLANET POLE NOT AT PICTURE CENTER.
C
101   SINTHT=DSIN(THT)
      COSTHT=DCOS(THT)
      MATRIX(1,1)=-COSLL*COSR*COSTHT-SINLL*SINTHT
      MATRIX(1,2)=+COSLL*COSR*SINTHT-SINLL*COSTHT
      MATRIX(1,3)=-COSLL*SINR
      MATRIX(2,1)=-SINLL*COSR*COSTHT+COSLL*SINTHT
      MATRIX(2,2)=+SINLL*COSR*SINTHT+COSLL*COSTHT
      MATRIX(2,3)=-SINLL*SINR
      MATRIX(3,1)=+SINR*COSTHT
      MATRIX(3,2)=-SINR*SINTHT
      MATRIX(3,3)=-COSR
      IF(PHI.GT.0.D0) GO TO 3
C
C     IF SSP WAS SOUTH POLE, MATRIX NEEDS REVISION
C
      DO 4 I=1,3
      DO 5 J=2,3
5     MATRIX(I,J)=-MATRIX(I,J)
4     CONTINUE
      GO TO 3
C
C     COME HERE IF SSP IS BOTH A POLE AND PICTURE CENTER
C
107   SINTHT=DSIN(THT)
      COSTHT=DCOS(THT)
      MATRIX(1,1)=SINTHT
      MATRIX(1,2)=-COSTHT
      MATRIX(1,3)=0.D0
      MATRIX(2,1)=COSTHT
      MATRIX(2,2)=SINTHT
      MATRIX(2,3)=0.D0
      MATRIX(3,1)=0.D0
      MATRIX(3,2)=0.D0
      MATRIX(3,3)=1.D0
      VECTOR(1)=0.
      VECTOR(2)=0.
      VECTOR(3)=-VABS
      IF(PHI.LT.0.D0) GO TO 40
      MATRIX(1,1)=-MATRIX(1,1)
      MATRIX(2,1)=-MATRIX(2,1)
      MATRIX(3,3)=-1.D0
      GO TO 40
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momatv.imake
/* Imake file for VICAR subroutine MOMATV  */

#define SUBROUTINE  momatv

#define MODULE_LIST  momatv.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tmomatv.f
        Include 'VICMAIN_FOR'
c
C-----THIS PROGRAM TESTS SUBROUTINE MOMATV
c
        Subroutine  Main44
	implicit real*8 (a-z)
	common/c/fl,oal,oas,scl,slat,slon,lss,sss,na,rng
	real*8 om(9),rs(3)
        character*80   header
        Data Header /'           fl   oal   oas   scl   slat   slon   lss   sss
     1  na   rng'/

C
C-----SET UP THE TEST INPUT BUFFER
	fl = 1500.1904
	oal = 500.
	oas = 500.
	scl = 84.821431
	slat = 3.4825339
	slon = 116.72441
	lss = -121.600
	sss = 1662.700
	na = 160.8923
 	rng = 14967727.
c
c-----print the input parameters
c
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss  
c     1  na   rng',67)

        Call Xvmessage(Header, ' ')  
	call prnt(8,10,fl,'.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	CALL momatv(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	CALL PRNT(8,9,oM,' OM MATRIX.')
	CALL PRNT(8,3,rs,' RS VECTOR.')

        Return  
	END
$!-----------------------------------------------------------------------------
$ create tmomatv.imake
/* IMAKE file for Test of VICAR subroutine  MOMATV  */

#define PROGRAM  tmomatv

#define MODULE_LIST tmomatv.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/*  #define   LIB_LOCAL  */       /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tmomatv.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstmomatv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
TMOMATV
!THIS IS A TEST OF MODULE MOMATV
!THE TEST PROGRAM WILL SET UP A SET OF ARGUEMENTS TO INPUT
!TO MOMATV.  THEY ARE PRINTED OUT INITIALLY.  THEN THE OUTPUT
!DATA IS PRINTED : A 9 ELEMENT OM-MATRIX AND A 3 ELEMENT
! RS-VECTOR.
TMOMATV
!THE VALUES FROM AN IBM RUN ARE:
! OM -.83108 .315447 .458028 .450869 -.100005 .886969958
!    .325597 .9436588 -.0591128 
! RS  -6718550.879  -13344185.05  909203.4494
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create momatv.hlp
1  MOMATV

        To compute the OM matrix and the RS vector in the MAP2 FARENC mode.

2  CALLING SEQUENCE:

        CALL MOMATV(LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,VABS,MATRIX,VECTOR) 

2  ARGUMENTS:

                                 INPUT VARIABLES                        
      NAME             TYPE                MEANING                   
                                                                     
       L0               R*8   LINE COORDINATE OF CAMERA AXIS         
       S0               R*8   SAMPLE COORDINATE OF CAMERA AXIS       
      LSSP              R*8   LINE COORDINATE OF SUBSPACECRAFT POINT 
      SSSP              R*8   SAMPLE COORD.   OF SUBSPACECRAFT POINT 
          (ALL OF THE ABOVE FOR DISTORTION-CORRECTED PICTURES)       
      PIXPMM            R*8   NUMBER OF PIXELS PER MM                
      FOC               R*8   CAMERA FOCAL LENGTH, MM                
       BL               R*8   WEST LONGITUDE OF SSP--DEGREES         
      PHI               R*8   LATITUDE OF SSP--DEGREES               
      THT (SEE SPECIAL  R*8   ANGLE OF PLANETARY SPIN AXIS (NORTH),  
           CASE LIST)         MEASURED IN PICTURE PLANE AT SSP,      
                              CLOCKWISE FROM UP--DEGREES             
      VABS              R*8   DISTANCE TO PLANET FROM S/C--KM        
                                                                     
                       OUTPUT VARIABLES                              
      MATRIX(3,3)       R*8   ROTATION MATRIX FROM PLANET TO CAMERA  
      VECTOR(3)         R*8   TRANSLATION VECTOR, IN PLANET SYSTEM
                                                                     

2 OPERATION:

    THIS ROUTINE COMPUTES THE PLANET TO CAMERA ROTATION MATRIX AND   
    THE VECTOR FROM PLANET CENTER TO SPACECRAFT, EXPRESSED IN THE 
    PLANET SYSTEM.  A VECTOR IN THE CAMERA SYSTEM, VPR, IS THEN   
    GIVEN IN TERMS OF THE SAME VECTOR IN THE PLANET SYSTEM, V, BY 
    VPR = (MATRIX)*(V-VECTOR).                       
   

    THE FOLLOWING SPECIAL CASES CAN BE HANDLED BY THE PROGRAM IF THE 
    INPUT VARIABLES ARE PROPERLY SET                                 
                  CASE 1 THE SSP IS A POLE, NOT IN THE PICTURE CENTER
             PROCEDURE   BL IS MEANINGLESS AND NOT LOOKED AT.  THT AS
                         DEFINED ABOVE IS ALSO MEANINGLESS.  IT WILL 
                         BE ASSUMED TO CONTAIN A POSITIVE NUMBER     
                         WHICH EQUALS THE WEST LOMGITUDE OF THE      
                         PICTURE CENTER.                             
                                                                     
                  CASE 2 THE SSP IS BOTH A POLE AND THE PICTURE      
                         CENTER                                      
             PROCEDURE   THE ROTATION IS TRIVIAL, BUT BL & THT ARE   
                         AGAIN MEANINGLESS, SO ADDITIONAL INFORMATION
                         IS REQUIRED.  SET THT (NORANG) TO A POSITIVE
                         NUMBER EQUALLING THE W LONGITUDE OF THE UP- 
                         GOING MERIDIAN IN THE INPUT PICTURE.        
                                                                     
                                                                     
    NOTE--CASES SUCH AS THE SSP BEING IN PICTURE CENTER BUT NOT A    
          POLE, OR POLE BEING IN PICTURE CENTER BUT NOT THE SSP, DO   
          NOT NEED SPECIAL HANDLING.                                 
    NOTE--IN ALL CASES, VARIABLES OTHER THAN THT AND BL MUST BE      
          PROPERLY SET.                                              


2  HISTORY:

    Ported for UNIX Conversion:   W.P. Lee, October-1992
    Original Programmer: J. J. Lorre, 16 June 1977
    Current Cognizant Programmer: J. J. Lorre
    Source Language: Revision
    Latest Revision: New


$ Return
$!#############################################################################
