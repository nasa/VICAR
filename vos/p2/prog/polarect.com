$!****************************************************************************
$!
$! Build proc for MIPL module polarect
$! VPACK Version 1.9, Thursday, October 28, 2010, 16:09:25
$!
$! Execute by entering:		$ @polarect
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module polarect ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to polarect.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("polarect.imake") .nes. ""
$   then
$      vimake polarect
$      purge polarect.bld
$   else
$      if F$SEARCH("polarect.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polarect
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polarect.bld "STD"
$   else
$      @polarect.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polarect.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polarect.com -mixed -
	-s polarect.f -
	-i polarect.imake -
	-p polarect.pdf -
	-t tstpolarect.pdf tstpolarect.log_solos tstpolarect.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polarect.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      EXTERNAL PLRECT
      EXTERNAL TEST
      INTEGER  IBLK, JBLK

      CALL IFMESSAGE('POLARECT version 27-Oct-2010')
      IBLK = 0
      JBLK = 0
c     CALL STACKA(7,TEST,4,10000,(IBLK+1)/2*2,(JBLK+1)/2*2,1000000,IGOT)
c     CALL STACKA(7,PLRECT,4,10000,(IBLK+1)/2*2,(JBLK+1)/2*2,IGOT,*900)
c (lwk:) above calls caused a crash on Sun-Solaris (AR-116326) -- but not
c when compiled with debug!  revised to:
      ii = (IBLK+1)/2*2
      jj = (JBLK+1)/2*2
c (note that both ii and jj are 0, but stacka changes this to 1)
c the call to subr.test seems completely pointless -- replacing it with
c 'igot=1000000-2048' gives the same result;  but it does no harm, so is
c retained ...
      CALL STACKA(7,TEST,4,10000,ii,jj,1000000,IGOT)
      CALL STACKA(7,PLRECT,4,10000,ii,jj,IGOT,*900)
  900 CONTINUE
      RETURN
      END
C*****************************************************************
      SUBROUTINE TEST(BUF1,I,BUF2,J,BUF3,K,BUF4,L,IGOT)
      IMPLICIT INTEGER(A-Z)
      INTEGER BUF1(I),BUF2(J),BUF3(K),BUF4(L)
      IGOT=L-2048
      RETURN
      END
C*****************************************************************
      SUBROUTINE PLRECT(OUT,KX,ABUF,IALT,BBUF,JALT,IN,IBFSIZ,*)
C
C----  VICAR PROGRAM POLARECT 
C
C     PURPOSE... COORDINATE TRANSFORMATION PROGRAM FROM RECTANGULAR TO
C        POLAR COORDINATES (DEFAULT) OR INVERSE.
C
C     PARAMETERS...INVERSE---TRANSFORMATION FROM POLAR TO RECTANGULAR
C                             COORDINATES IS TO BE PERFORMED.
C                  XCEN,X---SAMPLE COORDINATE FOR RECTANGULAR PICTURE
C                             ORIGIN (REAL).
C                  YCEN,Y---LINE COORDINATE FOR RECTANGULAR PICTURE
C                             ORIGIN (REAL).
C                  RANGE,A1,A2---MINIMUM A1 AND MAXIMUM A2 ANGLES TO BE
C                             INCLUDED IN COORDINATE TRANSFORMATION (REA
C                  RMIN,R1---MINIMUM RADIUS FROM RECTANGULAR ORIGIN TO
C                             BE INCLUDED (REAL).
C                  RMAX,R2---MAXIMUM RADIUS FROM RECTANGULAR ORIGIN TO
C                             BE INCLUDED (REAL)
C                  RESOLUTION,A3---ANGULAR RESOLUTION BETWEEN POLAR
C                             PICTURE LINES (REAL).
C                  RSCALE,S---RADIAL COMPRESSION PARAMETER
C                             RPOLAR=S*RRECT (REAL).
C                  BACK,I---BACKGROUND DN LEVEL WHERE INPUT PICTURE IS
C                             NON-EXISTENT FOR OUTPUT PICTURE(INTEGER).
C                  NOINTERP---NO INTERPOLATION IS TO BE PERFORMED, NEAREST
C                             INPUT PIXEL IS USED FOR OUTPUT.
C                  NOLABEL---FOR THE POLAR TO RECTANGULAR MODE, NO
C                             LABELS ARE TO BE WRITTEN ONTO THE OUTPUT
C                             DATA SET SPECIFYING THE TRANSFORMATION
C                             PARAMETERS.  FOR THE INVERSE MODE, THIS
C                             PARAMETER SPECIFIES THAT THERE IS NO
C                             LABEL ON THE INPUT DATA SET SPECIFYING
C                             THE TRANSFORMATION PARAMETERS.  DEFAULT
C                             IS THAT LABELS ARE ASSUMED TO BE WRITTEN
C                             AND THAT THEY EXIST FOR THE INVERSE MODE.
C
C***********************************************************************
C
C     VARIABLE MEANINGS...
C        SL,SS,NLO,NSO,NL,NS...SIZE FIELD PARAMETERS.
C        HALF...0 IF BYTE, 1 IF HALFWORD.
C        XCEN,YCEN...SPECIFIED RECTANGULAR ORIGIN OF COORDINATE TRANSFOR
C        RMIN,RMAX...MIN & MAX RADIUS FROM ORIGIN IN INPUT PICTURE.
C        PHIMIN,PHIMAX...MIN & MAX ANGLE IN POLAR PICTURE AS MEASURED
C              CLOCKWISE FROM +X-DIRECTION IN RECTANGULAR PICTURE.
C        RESO...ANGULAR RESOLUTION IN DEGREES/PIXEL (RADIANS/PIXEL).
C        SCALE...RPOLAR=SCALE*RRECT
C        INVER...0 FOR RECTANGULAR TO POLAR, 1 FOR INVERSE
C        IFLAG...INTERPOLATION FLAG. 0 FOR INTERPOLATION, 1 FOR NOINTERP.
C        NBL...NUMBER OF INPUT PICTURE LINES WHICH WILL FIT INTO CORE.
C
C***********************************************************************
C***********************************************************************
      INTEGER*4 XCENDF,YCENDF,RMAXDF,RMINDF,RESODF,RANGDF,RSCALEDF,
     *          BACKDF
      INTEGER SL,SS,NL,NS,NLO,NSO,HALF,BUFSIZ
      REAL XCEN,YCEN,RMIN,RMAX,RESO,PHIMIN,PHIMAX,TPI
C      BYTE IN(1)
      INTEGER*2 OUT(1)
      INTEGER*4 BACK,RANG(2)
      BYTE ABUF(1),BBUF(1),IN(1)
      INTEGER*4 STATUS
      CHARACTER*132 MG1
      CHARACTER*132 MG2
      CHARACTER*132 MG3
      CHARACTER*132 MG4
      CHARACTER*132 MG6
      CHARACTER*132 MG5
      CHARACTER*132 MCMPLT
      CHARACTER*132 MSIZE
      CHARACTER*4 FBYTE,FHALF,CSFORMAT

      LOGICAL INVERSE,NOINTERP,NOLABEL,AUTO,XVPTST

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE
      COMMON/DEFS/XCENDF,YCENDF,RMAXDF,RMINDF,RESODF,RANGDF,RSCALEDF,
     *          BACKDF
     

      DATA IAUTO,INVER,IFLAG,HALF,BACK/5*0/
      DATA RMIN,RMAX,RESO,PHIMIN,PHIMAX,SCALE,XCEN,YCEN/8*0./
      DATA LAB/1/
      DATA TPI/6.2831852/

C
C*** INITIALIZATION SECTION ***
C
      FHALF =  'HALF'
      FBYTE =  'BYTE'

      MG1(1:57) = '  INPUT IMAGE STATISTICS FOR XCEN=        , YCEN='
      MG2(1:24) = '   SIZE: NL=     NS=    '
      MG3(1:50) = '   ANGULAR RANGE: PHIMIN=        , PHIMAX='
      MG4(1:45) = '   RADIAL RANGE: RMIN=        , RMAX='
      MG5(1:56) = 
     + '   OPTIMAL ANGULAR RESOLUTION=         YIELDS      LINES'
      MG6(1:54) =
     + '   OPTIMAL RADIAL SCALE=   1.000 YIELDS       SAMPLES'
      MCMPLT(1:26) = '      INPUT LINES COMPLETE'
      WRITE (MSIZE,9900) IBFSIZ
9900  FORMAT (' WORK CORE AVAILABLE=',I7,' BYTES')
      CALL XVMESSAGE(MSIZE(2:34),' ')
      BUFSIZ=IBFSIZ
C
C     SET UP TRANSLATION BUFFER 
C


C
C*** PARAMETER PROCESSOR ***
C
      CALL XVPARM('XCEN',XCEN,NXCEN,XCENDF,1)
      CALL XVPARM('YCEN',YCEN,NYCEN,YCENDF,1)
      CALL XVPARM('RMAX',RMAX,NRMAX,RMAXDF,1)
      CALL XVPARM('RMIN',RMIN,NRMIN,RMINDF,1)
      CALL XVPARM('RESO',RESO,NRESO,RESODF,1)
      CALL XVPARM('RANGE',RANG,NRANG,RANGDF,1)
      CALL XVPARM('RSCALE',RSCALE,NRSCALE,RSCALEDF,1)
      CALL XVPARM('BACK',BACK,NBACK,BACKDF,1)
      NOINTERP=XVPTST('NOINTERP')
      INVERSE = XVPTST('INVERSE')
      NOLABEL = XVPTST('NOLABEL')
      AUTO = XVPTST('AUTO')
      RESO=RESO*TPI/360.
      PHIMIN=RANG(1)*TPI/360.
      PHIMAX=RANG(2)*TPI/360.
      IF(NOINTERP) IFLAG=1
      SCALE=RSCALE
      IF(INVERSE) INVER=1
      IF(NOLABEL) LAB=0
      IF(AUTO) IAUTO=1
C
C---- OPEN DATA SETS FOR PROCESSING
C
      CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(RUNIT,STATUS,'OP','READ','OPEN_ACT','SA',' ')
      CALL XVSIZE(SL,SS,NLO,NSO,NL,NS)
      CALL XVGET(RUNIT,STATUS,'FORMAT',CSFORMAT,' ')
      IF(STATUS.NE.1) THEN
	CALL XVMESSAGE('TEST POINT: BAD STATUS FROM XVGET',' ')
        CALL ABEND
      ENDIF

      IF(CSFORMAT.EQ.FHALF) HALF=1
      IF(CSFORMAT.EQ.FBYTE) HALF=0

      NBYTES = (1+HALF)*NS
      SL = SL-1
      SS = SS-1
      NL = NL-SL
      NS = NS-SS
      CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(WUNIT,STATUS,'OP','WRITE','U_NL',NLO,'U_NS',NSO,
     * 'O_FORMAT',CSFORMAT,'U_FORMAT',FHALF,'OPEN_ACT','SA',' ')
C
C     WRITE BACKGROUND DN ONTO OUTPUT PICTURE.
C
      CALL MVE(-5-HALF,NSO,BACK,IN,0,1)
      DO 21 I=1,NLO
         CALL XVWRIT(WUNIT,IN,STATUS,' ')
   21 CONTINUE
      CALL XVCLOSE(WUNIT,STATUS,' ')
      CALL XVOPEN(WUNIT,STATUS,'OP','UPDATE',
     * 'O_FORMAT',CSFORMAT,'U_FORMAT',FHALF,'OPEN_ACT','SA',' ')
C
C---- BEGIN EXECUTION
C
      IF(INVER .EQ. 1) GO TO 200
C
C***** RECTANGULAR TO POLAR SECTION *****
C
C
C     SET PHIMIN,PHIMAX,RESO,RMIN,RMAX,SCALE IF PREVIOUSLY UNSPECIFIED.
C
      IF(XCENDF .EQ. 1) XCEN=NS/2.
      IF(YCENDF .EQ. 1) YCEN=NL/2.
      WRITE (MG1(35:42),'(F8.3)') XCEN
      WRITE (MG1(50:57),'(F8.3)') YCEN
      CALL XVMESSAGE(MG1(2:57),' ')
      WRITE (MG2(13:16),'(I4)') NL
      WRITE (MG2(21:24),'(I4)') NS
      CALL XVMESSAGE(MG2(2:24),' ')
      CALL LIMITS(A,B,XCEN,YCEN,NS,NL)
      WRITE (MG3(26:33),'(F8.4)') A*360./TPI
      WRITE (MG3(43:50),'(F8.4)') B*360./TPI
      CALL XVMESSAGE(MG3(2:50),' ')
      IF(RANGDF .EQ. 1) PHIMIN=A
      IF(RANGDF .EQ. 1) PHIMAX=B
      CALL MAXRAD(C,D,XCEN,YCEN,NS,NL)
      WRITE (MG4(23:30),'(F8.3)') D
      WRITE (MG4(38:45),'(F8.3)') C
      CALL XVMESSAGE(MG4(2:45),' ')
      IF(RMINDF .EQ. 1) RMIN=D
      IF(RMAXDF .EQ. 1) RMAX=C
      CALL RESCIR(E,C)
      WRITE (MG5(31:38),'(F8.5)') E*360./TPI
      WRITE (MG5(47:50),'(I4)') NINT((B-A)/E+1.)
      CALL XVMESSAGE(MG5(2:56),' ')
      IF(RESODF .EQ. 1) RESO=E
      WRITE (MG6(41:44),'(I4)') NINT(C-D+1.)
      CALL XVMESSAGE(MG6(2:52),' ')
      IF(RSCALEDF .EQ. 1) SCALE=1.
      IF(IAUTO .EQ. 0 .OR. NLO .EQ. 1 .OR. NSO .EQ. 1) GO TO 23
      RESO=(PHIMAX-PHIMIN)/(NLO-1.)
      SCALE=(RMAX-RMIN)/(NSO-1.)
23    IF((PHIMAX-PHIMIN)/RESO .GT. NLO-1.)
     *   PHIMAX=PHIMIN+RESO*(NLO-1.)
      IF((RMAX-RMIN)/SCALE .GT. NSO-1.)
     *   RMAX=RMIN+SCALE*(NSO-1.)
      MG1(2:7) = 'OUTPUT'
      CALL XVMESSAGE(MG1(2:24),' ')
      WRITE (MG2(13:16),'(I4)') NLO
      WRITE (MG2(21:24),'(I4)') NSO
      CALL XVMESSAGE(MG2(2:24),' ')
      WRITE (MG3(26:33),'(F8.4)') PHIMIN*360./TPI
      WRITE (MG3(43:50),'(F8.4)') PHIMAX*360./TPI
      CALL XVMESSAGE(MG3(2:50),' ')
      WRITE (MG4(23:30),'(F8.3)') RMIN
      WRITE (MG4(38:45),'(F8.3)') RMAX
      CALL XVMESSAGE(MG4(2:45),' ')
      MG5(1:10) = ' '
      WRITE (MG5(31:38),'(F8.5)') RESO*360./TPI
      CALL XVMESSAGE(MG5(2:38),' ')
      MG6(1:10) = ' '
      WRITE (MG6(25:32),'(F8.5)') SCALE
      CALL XVMESSAGE(MG6(2:32),' ')
C     CALL QPRINT(MG6(9),24)
C
C     PERFORM LABEL PROCESSING IF DESIRED.
C
      IF(LAB.EQ.1) CALL ALABEL(0)
C
C     FIND NUMBER OF LINES OF INPUT WHICH WILL FIT INTO CORE, DECREMENT
C     BY 1 FOR OVERLAP, AND CALL POLAR FOR EACH SECTION OF PICTURE.
C
      NBL=BUFSIZ/NBYTES
      N=NBL-1
      DO 100 I=1,NL,N
	 CALL POLAR(I,NBL,NBYTES,IN,OUT)
         WRITE (MCMPLT(2:5),'(I4)') MIN0(I+N-1,NL)
         CALL XVMESSAGE(MCMPLT(2:26),' ')
100   CONTINUE
      GO TO 300
C
C***** POLAR TO RECTANGULAR SECTION *****
C
200   CONTINUE
      CALL QPRINT(' INVERSE MODE SPECIFIED',23)
C
C     FIND NUMBER OF INPUT LINES WHICH WILL FIT INTO THE CORE BUFFER,
C     DECREMENT BY 1 FOR OVERLAP, AND CALL RECT FOR EACH SECTION OF
C     PICTURE.
C
      NBL=BUFSIZ/NBYTES
      N=NBL-1
C
C     PERFORM LABEL PROCESSING IF SPECIFIED (OBTAIN NECESSARY PARAMS
C     FROM INPUT PICTURE LABEL).
C
      IF(LAB.EQ.1) THEN
        CALL ALABEL(1)
      ELSE
C
C----    MAKE ANY NECESSARY DEFAULT VALUES
C
         IF(RANGDF.EQ. 1) PHIMIN=0.
         IF(RANGDF .EQ. 1) PHIMAX=TPI
         IF(RESODF .EQ. 1) RESO=(PHIMAX-PHIMIN)/(NL-1)
         IF(RMINDF .EQ. 1) RMIN=0.
         IF(RMAXDF .EQ. 1) RMAX=RMIN+NS-1.
         IF(RSCALEDF .EQ. 1) SCALE=(RMAX-RMIN)/(NS-1.)
         IF(XCENDF .EQ. 1) XCEN=NSO/2.
         IF(YCENDF .EQ. 1) YCEN=NLO/2.
      ENDIF
      DO 202 I=1,NL,N
         CALL RECT(I,NBL,NBYTES,IN,OUT)
         WRITE (MCMPLT(2:5),'(I4)') MIN0(I+N-1,NL)
         CALL XVMESSAGE(MCMPLT(2:26),' ')
202   CONTINUE
C
C---- CLOSE FILES AND EXIT.
C
300   CONTINUE
      CALL XVCLOSE(RUNIT,STATUS,' ')
      CALL XVCLOSE(WUNIT,STATUS,' ')
      RETURN
      END
C***********************************************************
      SUBROUTINE LIMITS(PHIMIN,PHIMAX,X,Y,NS,NL)
C
C        SUBROUTINE LIMITS FINDS THE MAXIMUM & MINIMUM ANGLES WHICH
C        CONTAIN THE INPUT PICTURE WITH RESPECT TO THE ORIGIN (X,Y)
C
      REAL PHIMIN,PHIMAX
      TPI=6.2831852
      A=NS
      B=NL
      IF(X .LT. 1) GO TO 1
      IF(X .GT. NS) GO TO 2
      IF(Y .LT. 1) GO TO 40
      IF(Y .GT. NL) GO TO 60
      IF(X .EQ. 1..AND. Y .EQ. 1.) GO TO 100
      IF(X .EQ. 1..AND. Y .EQ. NL) GO TO 101
      IF(X .EQ. NS .AND. Y .EQ. 1.) GO TO 102
      IF(X .EQ. NS .AND. Y .EQ. NL) GO TO 103
      GO TO 50
1     IF(Y .LT. 1.) GO TO 10
      IF(Y .GT. NL) GO TO 30
      GO TO 20
2     IF(Y .LT. 1.) GO TO 70
      IF(Y .GT. NL) GO TO 90
      GO TO 80
C
C     CASE 1... X .LT. 1, Y .LT. 1
C
10    PHIMIN=ATAN2(1.-Y,A-X)
      PHIMAX=ATAN2(B-Y,1.-X)
      GO TO 200
C
C     CASE 2... X .LT. 1, 1 .LE. Y .LE. NL
C
20    PHIMIN=ATAN2(1.-Y,1.-X)
      PHIMAX=ATAN2(B-Y,1.-X)
      GO TO 200
C
C     CASE 3... X .LT. 1, Y .GT. NL
C
30    PHIMIN=ATAN2(1.-Y,1.-X)+TPI
      PHIMAX=ATAN2(B-Y,A-X)+TPI
      GO TO 200
C
C     CASE 4... 1 .LE. X .LE. NS, Y .LT. 1
C
40    PHIMIN=ATAN2(1.-Y,A-X)
      PHIMAX=ATAN2(1.-Y,1.-X)
      GO TO 200
C
C     CASE 5... 1 .LE. X .LE. NS, 1 .LE. Y .LE. NL
C
50    PHIMIN=0.
      PHIMAX=TPI
      GO TO 200
C
C     CASE 6... 1 .LE. X .LE. NS, Y .GT. NL
C
60    PHIMIN=ATAN2(B-Y,1.-X)+TPI
      PHIMAX=ATAN2(B-Y,A-X)+TPI
      GO TO 200
C
C     CASE 7... X .GT. NS, Y .LT. 1
C
70    PHIMIN=ATAN2(B-Y,A-X)
      PHIMAX=ATAN2(1.-Y,1.-X)
      GO TO 200
C
C     CASE 8... X .GT. NS, 1 .LE. Y .LE. NL
C
80    PHIMIN=ATAN2(B-Y,A-X)
      PHIMAX=ATAN2(1.-Y,A-X)+TPI
      GO TO 200
C
C     CASE 9... X .GT. NS, Y .GT. NL
C
90    PHIMIN=ATAN2(B-Y,1.-X)+TPI
      PHIMAX=ATAN2(1.-Y,A-X)+TPI
      GO TO 200
C
C     CASE 10A... X .EQ. 1, Y .EQ. 1
C
100   PHIMIN=0.
      PHIMAX=TPI/4.
      GO TO 200
C
C     CASE10B... X .EQ. 1 Y .EQ. NL
C
101   PHIMIN=TPI*3./4.
      PHIMAX=TPI
      GO TO 200
C
C     CASE 10C... X .EQ. NS, Y .EQ. 1
C
102   PHIMIN=TPI/4.
      PHIMAX=TPI/2.
      GO TO 200
C
C     CASE 10D... X .EQ. NS, Y .EQ. NL
C
103   PHIMIN=TPI/2.
      PHIMAX=TPI*3./4.
      GO TO 200
C
C     RETURN ANSWERS...
C
200   CONTINUE
      RETURN
      END
C******************************************************************
      SUBROUTINE MAXRAD(RMAX,RMIN,X,Y,NS,NL)
C
C        SUBROUTINE MAXRAD FINDS THE MINIMUM & MAXIMUM RADII WHICH
C        CONTAINS THE INPUT PICTURE WITH RESPECT TO THE ORIGIN (X,Y).
C
      A=NS
      B=NL
      IF(X .LE. A/2.) GO TO 10
      IF(Y .LE. B/2.) GO TO 5
C
C     MAXIMUM DISTANCE IS TO (X,Y)=(1,1)
      C=1.-X
      D=1.-Y
      GO TO 100
C
C     MAXIMUM DISTANCE IS TO (X,Y)=(1,NL)
5     C=1.-X
      D=B-Y
      GO TO 100
10    IF(Y .LE. B/2.) GO TO 15
C
C     MAXIMUM DISTANCE IS TO (X,Y)=(NS,1)
      C=A-X
      D=1.-Y
      GO TO 100
C
C     MAXIMUM DISTANCE IS TO (X,Y)=(NS,NL)
15    C=A-X
      D=B-Y
      GO TO 100
C
C     CALCULATE MAXIMUM DISTANCE
C
100   RMAX=SQRT(C*C+D*D)
C     CALCULATE RMIN
      IF(X .LT. 1.) GO TO 20
      IF(X .GT. A) GO TO 35
      IF(Y .LT. 1.) GO TO 50
      IF(Y .GT. B) GO TO 55
      RMIN=0.
      GO TO 103
20    IF(Y .LT. 1.) GOTO 25
      IF(Y .GT. B) GO TO 30
C
C        MINIMUM DISTANCE IS TO COLUMN 1.
      RMIN=1.-X
      GO TO 103
C
C        MINIMUM DISTANCE IS TO (X,Y)=(1,1)
25    C=1.-X
      D=1.-Y
      GO TO 102
C
C        MINIMUM DISTANCE IS TO (X,Y)=(1,NL)
30    C=1.-X
      D=B-Y
      GO TO 102
35    IF(Y .LT. 1) GO TO 40
      IF(Y .GT. B) GO TO 45
C
C        MINIMUM DISTANCE IS TO COLUMN NS.
      RMIN=X-A
      GO TO 103
C
C        MINIMUM DISTANCE IS TO (X,Y)=(NS,1).
40    C=A-X
      D=1.-Y
      GO TO 102
C
C        MINIMUM DISTANCE IS TO (X,Y)=(NS,NL).
45    C=A-X
      D=B-Y
      GO TO 102
C
C        MINIMUM DISTANCE IS TO LINE 1.
50    RMIN=1.-Y
      GO TO 103
C
C        MINIMUM DISTANCE IS TO LINE NL.
55    RMIN=Y-B
      GO TO 103
C
C        CALCULATE MINIMUM DISTANCE
102   RMIN=SQRT(C*C+D*D)
103   CONTINUE
      RETURN
      END
C************************************************************
      SUBROUTINE RESCIR(RESO,RMAX)
C        SUBROUTINE RESCIR FINDS THE OPTIMUM ANGULAR RESOLUTION FOR A
C        POLAR COORDINATE PICTURE.
      REAL RESO,RMAX
      RESO=ATAN(1./RMAX)
      RETURN
      END
C************************************************************
      SUBROUTINE POLAR(LN,NBL,NBYTES,IN,OUT)
C        SUBROUTINE POLAR PERFORMS THE RECTANGULAR TO POLAR XFORM.
      INTEGER SL,SS,HALF,BACK
      INTEGER*2 OUT(1)
      BYTE IN(NBYTES,NBL)
      REAL RESO,PHIMIN,PHIMAX,XCEN,YCEN

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE

      LINE=LN-1+SL
      NLN=NBL
      IF(NLN+LN .GT. NL) THEN
	 NLN=NL-LN+1
      ENDIF
C
C        READ SECTION OF INPUT INTO CORE
      DO 1 I=1,NLN
         CALL XVREAD(RUNIT,IN(1,I),STATUS,'LINE',LINE+I,'SAMP',SS+1,
     *               'NSAMPS',NS,' ')
    1 CONTINUE
C
C        PROCESS EACH OUTPUT LINE FOR EACH SECTION OF INPUT
      DO 2 I=1,NLO
         CALL XVREAD(WUNIT,OUT,STATUS,'LINE',I,'NSAMPS',NSO,' ')
         PHI=PHIMIN+(I-1)*RESO
         IF(PHI .GT. PHIMAX) GO TO 2
         COSPHI=COS(PHI)
         SINPHI=SIN(PHI)
         CALL POLA(NLN,IN,OUT,LN,COSPHI,SINPHI)
C
C----    WRITE NEW OUTPUT LINE
C
         CALL XVWRIT(WUNIT,OUT,STATUS,'LINE',I,'NSAMPS',NSO,' ')

2     CONTINUE
      RETURN
      END
C************************************************************
      SUBROUTINE RECT(LN,NBL,NBYTES,IN,OUT)
C        SUBROUTINE RECT PERFORMS AN INVERSE POLAR TRANSFORMATION, I.E.,
C        IT TRANSFORMS A POLAR COORDINATE PICTURE TO A RECTANGULAR
C        COORDINATE PICTURE.
C
      INTEGER SL,SS,HALF,BACK
      INTEGER*2 OUT(1)
      BYTE IN(NBYTES,NBL)
      REAL RESO,PHIMIN,PHIMAX,XCEN,YCEN,RMIN,RMAX,SCALE

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE

      LINE=LN-1+SL
      NLN=NBL
      IF(NLN+LN .GT. NL) NLN=NL-LN+1
      DO 1 I=1,NLN
         CALL XVREAD(RUNIT,IN(1,I),STATUS,'LINE',LINE+I,'SAMP',SS+1,
     *               'NSAMPS',NS,' ')
    1 CONTINUE
      DO 2 I=1,NLO
         CALL XVREAD(WUNIT,OUT,STATUS,'LINE',I,'NSAMPS',NSO,' ')
         CALL RECTA(NLN,IN,OUT,LN,I)
         CALL XVWRIT(WUNIT,OUT,STATUS,'LINE',I,'NSAMPS',NSO,' ')
    2 CONTINUE
      RETURN
      END
C************************************************************
      SUBROUTINE ALABEL(IND)
C        SUBROUTINE ALABEL EITHER WRITES OUT THE TRANSFORMATION PARAMS
C        TO THE OUTPUT DATA SET, OR OBTAINS THEM FROM THE INPUT DATA
C        SET LABEL, DEPENDING ON THE MODE.
C
      INTEGER*4 XCENDF,YCENDF,RMAXDF,RMINDF,RESODF,RANGDF,RSCALEDF,
     *          BACKDF
      INTEGER*4 STATUS
      CHARACTER*80 MG1
      CHARACTER*80 MG3
      CHARACTER*80 MG2

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE
      COMMON/DEFS/XCENDF,YCENDF,RMAXDF,RMINDF,RESODF,RANGDF,RSCALEDF,
     *          BACKDF

      MG1= 
     + '         PHIMIN=           AT LINE 1, PHIMAX=           AT LINE'
      MG2=
     + '         RMIN=           AT SAMP 1, RMAX=           AT SAMP'
      MG3=
     + '         CENTER OF TRANSFORMATION XCEN=         , YCEN='

C      CHARACTER*72 M1
C      M1 =
C    + '         PHIMIN=XXXXXXXXXX AT LINE 1, PHIMAX=XXXXXXXXXX AT LINE'
C      CHARACTER*72 M2
C      M2 =
C    + '         RMIN=XXXXXXXXXX AT SAMP 1, RMAX=XXXXXXXXXX AT SAMP'
C      CHARACTER*72 M3
C      M3 =
C     + '         CENTER OF TRANSFORMATION XCEN=XXXXX.XXX, YCEN=XXXXX.'
      TPI=6.2831852 

      IF(IND.EQ.0) THEN
C
C---- WRITE LABEL (FORWARD TRANSFORM)
C
      CALL XLADD(WUNIT,'HISTORY','PHIMIN',PHIMIN,STATUS,'FORMAT',
     *       'REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','PHIMAX',PHIMAX,STATUS,'FORMAT',
     *       'REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','RMIN',RMIN,STATUS,'FORMAT','REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','RMAX',RMAX,STATUS,'FORMAT','REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','XCEN',XCEN,STATUS,'FORMAT','REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','YCEN',YCEN,STATUS,'FORMAT','REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','RESO',RESO,STATUS,'FORMAT','REAL',' ')
      CALL XLADD(WUNIT,'HISTORY','RSCALE',SCALE,STATUS,'FORMAT',
     *       'REAL',' ')
      CALL XVMESSAGE('VICAR HISTORY LABEL WRITTEN:',' ')
      WRITE (MG1(17:26),'(F10.5)') PHIMIN*360./TPI
      WRITE (MG1(46:55),'(F10.5)') PHIMAX*360./TPI
      WRITE (MG1(64:67),'(I4)') NINT((PHIMAX-PHIMIN)/RESO+1.)
C 9910  FORMAT ('                ',F10.5,'                   ',F10.5,
C     +'        ',I4,'     ')
      CALL XVMESSAGE(MG1,' ')
C      WRITE (MG2,9920) RMIN,RMAX,NINT((RMAX-RMIN)/SCALE+1.)
      WRITE (MG2(15:24),'(F10.5)') RMIN
      WRITE (MG2(42:51),'(F10.5)') RMAX
      WRITE (MG2(60:63),'(I4)') NINT((RMAX-RMIN)/SCALE+1.)
C 99920  FORMAT ('              ',F10.5,'                 ',F10.5,
C     +'        ',I4,'         ')
      CALL XVMESSAGE(MG2,' ')
      WRITE (MG3(40:48),'(F9.3)') XCEN
      WRITE (MG3(56:64),'(F9.3)') YCEN
C 9930  FORMAT ('                                       ',F9.3,'       ',
C     +F9.3,'        ')
      CALL XVMESSAGE(MG3,' ')
      ELSE
C
C---- READ LABEL (INVERSE TRANSFORM)
C
        CALL XVMESSAGE(' VICAR HISTORY LABEL READ:',' ')
      IF(RANGDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','PHIMIN',PHIMIN,STATUS,
     *    'FORMAT','REAL',' ')
      MG1= ' PHIMIN = '
      WRITE(MG1(11:20),'(F10.5)') PHIMIN
      CALL XVMESSAGE(MG1,' ')
      ENDIF
      IF(RANGDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','PHIMAX',PHIMAX,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG2= ' PHIMAX = '
      WRITE(MG2(11:20),'(F10.5)') PHIMAX
      CALL XVMESSAGE(MG2,' ')
      ENDIF
      IF(RMINDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','RMIN',RMIN,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG3= ' RMIN = '
      WRITE(MG3(9:18),'(F10.5)') RMIN
      CALL XVMESSAGE(MG3,' ')
      ENDIF
      IF(RMAXDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','RMAX',RMAX,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG1= ' RMAX = '
      WRITE(MG1(9:18),'(F10.5)') RMAX
      CALL XVMESSAGE(MG1,' ')
      ENDIF
      IF(XCENDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','XCEN',XCEN,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG2= ' XCEN = '
      WRITE(MG2(9:18),'(F10.5)') XCEN
      CALL XVMESSAGE(MG2,' ')
      ENDIF
      IF(YCENDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','YCEN',YCEN,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG3= ' YCEN = '
      WRITE(MG3(9:18),'(F10.5)') YCEN
      CALL XVMESSAGE(MG3,' ')
      ENDIF
      IF(RESODF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','RESO',RESO,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG1= ' RESO = '
      WRITE(MG1(9:18),'(F10.5)') RESO
      CALL XVMESSAGE(MG1,' ')
      ENDIF
      IF(RSCALEDF.EQ.1) THEN
        CALL XLGET(RUNIT,'HISTORY','RSCALE',SCALE,STATUS,
     *          'FORMAT','REAL','HIST','POLARECT',' ')
      MG2= ' SCALE = '
      WRITE(MG2(10:19),'(F10.5)') SCALE
      CALL XVMESSAGE(MG2,' ')
      ENDIF
      ENDIF
      RETURN
      END
c*******************************************************
	SUBROUTINE POLA(NLN,IN,OUT,LN,COSPHI,SINPHI)
      
	INTEGER*4 BACK
	INTEGER SL,SS,HALF
        INTEGER*2 OUT(1),IOUT
	BYTE IN(1)
	REAL RNS,RNLN,RLN

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE

        RNS=NS
        RNLN=NLN
 	RLN=LN
	ISTOP=0
	R=RMIN
	I=1

	DO 100 J=1,NSO
C
C   GET NEXT VALUE FROM OUTPUT AREA AND CHECK BACKGROUND
C        I.E. IS THIS IN THE AREA OF INTEREST
C
	IOUT=OUT(I)
	IF (IOUT.NE.BACK) GO TO 400
	IF (R.GT.RMAX) RETURN
	X=R*COSPHI+XCEN
	IF (X.LT.1..OR. X.GT.RNS) GO TO 300
	Y=R*SINPHI+YCEN+1.-RLN
	IF (Y.LT.1..OR.Y.GT.RNLN) GO TO 300
	RAD=R
	OUT(I)=IFP(X,Y,NS,NLN,IN,IFLAG,HALF)

	ISTOP=1
	R=RAD
	GO TO 400
300	CONTINUE
	IF (ISTOP.NE.0) RETURN
400	CONTINUE
	I=I+1
	R=R+SCALE
100	CONTINUE

	RETURN
	END
C********************************************************
	SUBROUTINE RECTA(NLN,IN,OUT,LN,LINE)

	INTEGER*4 BACK
	INTEGER SL,SS,HALF
        INTEGER*2 OUT(1),IOUT
	BYTE IN(1)
	REAL RLINE,RNS,RNLN,RLN

      COMMON /C1/ RUNIT,WUNIT,SL,SS,NL,NS,NLO,NSO,HALF,BACK
      COMMON /C2/ RESO,PHIMIN,PHIMAX,XCEN,YCEN,IFLAG,RMIN,RMAX,SCALE

	RLINE=LINE
 	Y=RLINE-YCEN
	Y2=Y*Y
	X=1.-XCEN

	TWOPI=2.*4.*ATAN(1.)
	RNS=NS
	RNLN=NLN
	RLN=LN
	I=1
	DO 100 J=1,NSO
C
C   GET NEXT VALUE FROM OUTPUT AREA AND CHECK BACKGROUND
C        I.E. IS THIS IN THE AREA OF INTEREST
C
	IOUT=OUT(I)
	IF (IOUT.NE.BACK) GO TO 400
	R=X*X+Y2
	E0=SQRT(R)-RMIN
	IF(E0.LT.0.) GO TO 400
	R=E0/SCALE+1.
	IF(R.GT.RNS) GO TO 400
	IF (X.NE.0.) THEN
            PHI=ATAN2(Y,X)
          ELSEIF (Y.LT.0.) THEN
            PHI=TWOPI*.75
          ELSEIF (Y.EQ.0.) THEN
            PHI=PHIMIN
          ELSEIF (Y.GT.0.) THEN
            PHI=TWOPI*.25
        ENDIF
	IF (PHI.LT.PHIMIN) PHI=PHI+TWOPI
        IF (PHI.GT.PHIMAX) PHI=PHI-TWOPI
        IF (PHI-PHIMIN.LT.0.) GO TO 400
	PHI=(PHI-PHIMIN)/RESO-RLN+2.
	IF (PHI.LT.1. .OR. PHI.GT.RNLN) GO TO 400
	OUT(I)=IFP(R,PHI,NS,NLN,IN,IFLAG,HALF)

400	CONTINUE
	I=I+1
	X=X+1.
100	CONTINUE

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create polarect.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polarect

   To Create the build file give the command:

		$ vimake polarect			(VMS)
   or
		% vimake polarect			(Unix)


************************************************************************/


#define PROGRAM	polarect
#define R2LIB

#define MODULE_LIST polarect.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create polarect.pdf
process help=*
!
!    VICAR PROGRAM POLARECT
!
PARM INP TYPE=(STRING,72)
PARM OUT TYPE=(STRING,72)
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM INVERSE TYPE=KEYWORD COUNT=(0:1) VALID=INVERSE DEFAULT=--
PARM BACK TYPE=INTEGER DEFAULT=0
PARM NOINTERP TYPE=KEYWORD COUNT=(0:1) VALID=NOINTERP DEFAULT=--
PARM XCEN TYPE=REAL DEFAULT=0.
PARM YCEN TYPE=REAL DEFAULT=0.
PARM RANGE TYPE=REAL COUNT=2 DEFAULT=(0.,0.)
PARM RMIN TYPE=REAL DEFAULT=0.
PARM RMAX TYPE=REAL DEFAULT=0.
PARM RESO TYPE=REAL DEFAULT=0.
PARM RSCALE TYPE=REAL DEFAULT=0.
PARM NOLABEL TYPE=KEYWORD COUNT=(0:1) VALID=NOLABEL DEFAULT=--
PARM AUTO TYPE=KEYWORD COUNT=(0:1) VALID=AUTO DEFAULT=--
END-PROC
.TITLE
VICAR PROGRAM POLARECT
.HELP
PURPOSE
     POLARECT is a VICAR applications program which performs 
     a  rectangular-to-polar  coordinate transformation  (or 
     inverse)  on  the input image with respect to  a  given 
     origin.   The  purpose of this program is to  geometri-
     cally transform an image to polar coordinates,  where R 
     and    are  the orthogonal axes rather than  x  and  y, 
     thereby allowing radial and circular characteristics in 
     the  image  to  be subjected to  existing  enhancement, 
     analysis  and transformation routines in a polar  coor-
     dinate sense.   After polar processing,  the  resultant 
     image may then be transformed back to rectangular coor-
     dinates. The program will operate on BYTE and HALF images,
     type is determined from the VICAR label.
.PAGE
TAE COMMAND LINE FORMAT

     polarect INP=A OUT=B SIZE=(SL,SS,NL,NS) PARAMS

          where

     INP and OUT       are the input and output data sets

     SIZE             is the standard VICAR size field.   SL 
                      and  SS refer to starting offsets with 
                      respect to the input image.  NL and NS 
                      refer  to the desired  output  picture 
                      size.

     PARAMS           is the VICAR parameter field.
.PAGE
OPERATION
  
     POLARECT  transforms the input image from the  standard 
     rectangular  coordinate  system to a  polar  coordinate 
     system  based on the orthogonal axes R and THETA. These 
     orthogonal axes are represented in the polar image by R 
     in the sample direction, and THETA in the direction  of 
     increasing  line  count as shown  in  Figure  1.   This 
     convention  was  used  in order to permit  the  use  an 
     angular  resolution as small as desired,  even though a 
     large  angular coverage may be desired,  the number  of 
     lines in the polar image becomes large. In general, the 
     radial  coverage  represented by the  sample  direction 
     remains relatively small,  thereby adhering to physical 
     record length constraints.
.PAGE
            FIGURE 1.  POLARECT PARAMETER CONVENTIONS
                                                          
               R=R1          R--->         R-R2           
              _________________________________           
         Th=A1|                               |           
              |                               |           
              |                               |      
          Th  |                               |      
           |  |                               |           
           |  |                               |           
           |  |                               |           
              |                               |      
              |                               |      
              |                               |           
              |                               |           
         Th=A2|_______________________________|           
                                              (nl,ns)
                    POLAR COORDINATE IMAGE                 
.PAGE 
     The variables defined by the parameters RMIN (R1), RMAX 
     (R2), and RANGE (A1 and A2) are also shown in Figure 1.

     The  rectangular image coordinate system was  based  on 
     the  standard  convention of the +X and  +Y  directions 
     corresponding  to  increasing sample  and  line  counts 
     respectively. For this reason, the (Th=0)  direction is 
     defined  as the +X direction,  which is directed toward 
     the  right  side  of  the  rectangular   image.   These 
     conventions are shown in Figure 2. Also shown in Figure 
     2  are  pictorial  rtepresentations for  the  parameter 
     variables.

.PAGE                                                
         FIGURE 2.  POLARECT PARAMETER CONVENTIONS   
                                                          
              (1,1)          X--->                        
              _________________________________           
              |                      Th=A1    |           
              |                       .       |           
              |                         .     |      
           Y  |                .          .   |      
           |  |             R=r1 .         .  |     o     
           |  |       ___._______._________._>| Th=0. 
           |  | (xcen,ycen)      .     R=r2.  |           
              |                .          .   |      
              |                         .     |      
              |                       .       |           
              |                      Th=A2    |           
              |_______________________________|           
                                              (nl,ns)
                RECTANGULAR COORDINATE IMAGE           
.PAGE
     Once  the  parameters have  been  determined,  POLARECT 
     performs  the  geometric  transformation  in  a  manner 
     similar to GEOMA,  i.e.,  every output pixel is  mapped 
     back  to its location in the input picture,  and either 
     four-point   bilinear  interpolation  or  the   nearest 
     neighbor method is used to determine the pixel DN.  The 
     mapping transformations are as follows:

     FORWARD MODE: For known R and THETA in the output image,

                     X (INPUT)  =  R cos(Theta) + XCEN
                     Y (INPUT)  =  R sin(Theta) + YCEN

.PAGE
     INVERSE MODE: For known X and Y in the output image,
                                      2          2
              R(INPUT) = sqrt((X-XCEN) + (Y-YCEN) )
              
                                -1
              THETA(OUTPUT)= tan  ((Y-YCEN)/(X-XCEN))

     Dynamic  allocation is used for the mapping  operation, 
     with  as  much of the input picture being  loaded  into 
     core as the partition allows.   For this reason,  it is 
     recommended that for optimum time utilization, the user 
     use POLARECT in the largest region possible when trans-
     forming larger pictures (NL&NX>1000).
     All  parameters in the INVERSE mode are expressible  in 
     terms of the forward mode parameters with the exception 
     of  the AUTO parameter which is NOT applicable  to  the 
     INVERSE  mode.   All  GENERAL parameters apply  to  the 
     INVERSE mode and unnecessary parameters are ignored.
.PAGE
RESTRICTIONS

     There  are  no  restrictions on image  size  or  record 
     lengths,  however,  the  user is advised of  the  rapid 
     execution  time  increase as input and output  pictures 
     sizes increase.

TIMING

     POLARECT execution time (on IBM) is approximately:

     a) 16 minutes for 1350 line by 1250 sample input image,

     b) 2 minutes for a 400 line by 400 sample input image,

     c) 1 minute for a 325 line by 325 sample input image.
.PAGE
EXAMPLES
A.   This  example represents the simplest form of  POLARECT 
     usage.   The  user  specifies the origin of  the  polar 
     coordinate system,  and allows the program to calculate 
     scaling  factors for the polar  coordinate  image.  The 
     output  picture size is defaulted to that of the input, 
     so that 1250 lines = 360 ,  and 1250 samples = RMAX  of 
     input   picture   (881.25   in   this   case).    After 
     transforming   to   polar   coordinates,    the   VICAR 
     applications  programs  FILTERAA and PIXH are  applied, 
     and  the  resulting image is transformed  back  to  the 
     rectangular coordinate format. See Figure 3A.

          COPY IN A SIZE=(1,1,1250,1250)
          polarect INP=A OUT=B 'AUTO XCEN=625. YCEN=625.
          FILTER B C FILTWTS
          PISH INP=(C,B) OUT=A PIXHPAR
          polarect A B 'INVERSE
.PAGE
     Note  that defaulting picture size may cause a loss  of 
     detail  because  there is usually insufficient  angular 
     resolution in the defaulted number of lines. This tends 
     to  cause a wiggling of straight lines at large  radius 
     when  inversely transformed.  For this  reason,  it  is 
     recommended  that the user make the number of lines  of 
     the polar coordinate image at least 3 times the maximum 
     radius RMAX.

B.   This  example  represents  the most  complex  usage  of 
     parameters necessary for POLARECT.  The user desires an 
     annular ring with center at (X,Y) = (-1000,500),  width 
     500 pixels,  and minimum radius of 1250. (Corresponding 
     to the circle passing through pixel (X,Y) = (250.,500.) 
     to  be transformed to polar coordinates with an angular 
     resolution of 0.01 degree per line (output).
.PAGE
          COPY TAPE A SIZE=(1,1,1000,1000)
          polarect A B NL=300 NS=500 XCEN=-1000. YCEN=500. +
                   RANGE=(-15.,+15.) RMIN=1250. RMAX=1750. +
                   RESO=(0.01) RSCALE=1.0
          polarect B A NL=1000 'INVERSE

     Written by:  W. D. Benton, 23 August 1977
     Cognizant Programmer:  B. Gokhman
     Revision:  C.R. Schenk 1-July-1994
.LEVEL1
.VARIABLE INP
Input image
.VARIABLE OUT
Output image
.VARIABLE SIZE
Size field
.VARIABLE INVERSE
Polar to rectangular
.VARIABLE BACK
Background DN for output.
.VARIABLE NOINTERP
No bilinear interpolation
.VARIABLE XCEN
X-coord. for origin of polar sys
.VARIABLE YCEN
Y-coord. for origin of polar sys
.VARIABLE RANGE
Angular limits of the sector
.VARIABLE RMIN
Min radius of the sector
.VARIABLE RMAX
Max radius of the sector
.VARIABLE RESO
Angular resolution
.VARIABLE RSCALE
Radial scaling factor
.VARIABLE NOLABEL
Label info not used for default par
.VARIABLE AUTO
Fits input sector to output size
.LEVEL2
.VARIABLE INP
     INP=a            Input image: rectangular for forward
                      transformation, polar for inverse.
.VARIABLE OUT
     OUT=b            Output image: polar for forward
                      transformation, rectangular for inverse.
.VARIABLE SIZE
     SIZE=(SL,SS,NLO,NSO)
                      is the standard VICAR size field.   SL 
                      and  SS refer to starting offsets with 
                      respect to the input image.NLO and NSO 
                      refer  to the desired  output  picture 
                      size.
.VARIABLE INVERSE
    'INVERSE          Input  is in polar coordinate  format, 
                      and   is   to   be   transformed    to 
                      rectangular coordinate format (default 
                      is rectangular to polar transformation).
.VARIABLE BACK
     BACK=I           I is the DN value of all points in the 
                      output  image which are not  contained 
                      in the input.

.VARIABLE NOINTERP
     'NOINTERP        This  keyword specifies that there  is 
                      to  be  no bilinear interpolation  for 
                      output  pixels  which  correspond   to 
                      inter-pixel  area in the input.   The 
                      nearest  pixel value is used  (default 
                      is to perform bilinear interpolation).
.VARIABLE XCEN
FORWARD:
     XCEN,X           X(REAL)  is  the  rectangular   sample 
                      coordinate for the origin of the polar 
                      coordinate system.  Default is NSI/2., 
                      where  NSI  is the input picture  line 
                      length in pixels.
INVERSE:
     XCEN,X           X  (REAL)  is the  rectangular  sample 
                      coordinate to which the points defined 
                      by  R=0.  are mapped.   Default is  to 
                      obtain X from the history labels.

.VARIABLE YCEN
FORWARD:
     YCEN,Y           Y(REAL)   is  the   rectangular   line 
                      coordinate for the origin of the polar 
                      coordinate system.  Default is NLI/2., 
                      where  NLI  is the number of lines  in 
                      the input picture.
INVERSE:
     YCEN,Y           Y   (REAL)  is  the  rectangular  line 
                      coordinate to which the points defined 
                      by  R=0.  are mapped.  Default  is  to 
                      obtain Y from the history labels.
.VARIABLE RANGE
FORWARD:
     RANGE,A1,A2      A1  and  A2  (REAL)  are  the  angular 
                      limits  of  the sector  in  the  input 
                      picture  which is to be transformed to 
                      polar  coordinates  when  taken   with 
                      respect to the polar origin defined by 
                      XCEN  and YCEN.  Default is to set  A1 
                      and   A2,   to  include  entire  input 
                      picture.   A1 and A2 are expressed  in 
                      degrees.   See parameter RESO.  (A2-A1 
                       360.,A1 -90.)
INVERSE:
     RANGE,A1,A2      A1  and  A2  (REAL)  are  the  angular 
                      limits,   in  degrees,  of  the  polar 
                      coordinate picture.  A1 corresponds to 
                      the input angle    at line 1 and A2 to 
                      the   at line NL(INPUT). Default is to 
                      obtain  A1  and  A2 from  the  history 
                      labels.  (A2-A1 360.,A1 -90.)
.VARIABLE RMIN
FORWARD:
     RMIN,R1          R1(REAL)  is the  minimum  radius,  in 
                      pixels  from the polar origin,  of the 
                      sector to be included in the transfor-
                      mation.  Default is R1=0. if the polar 
                      origin is within the input  image,  or 
                      the   radius   corresponding  to   the 
                      closest  point in the input  image  if 
                      the  polar  origin is not  within  the 
                      input image.
INVERSE:
     RMIN,R1          R1  (REAL) is the radius in the  polar 
                      input  corresponding to the  first  to 
                      the  first  pixel of each input  line. 
                      Default  is  to  obtain  R1  from  the 
                      history labels.
.VARIABLE RMAX
FORWARD:
     RMAX,R2          R2  (REAL) is the maximum  radius,  in 
                      pixels  from the polar origin,  of the 
                      sector to be included in the transfor-
                      mation.   Default  is to set R2 to  be 
                      the radius corresponding to the  point 
                      in  the input sector which is furthest 
                      from the polar origin.   See parameter 
                      RSCALE.
INVERSE:
     RMAX,R2          R2  (REAL) is the radius in the  polar 
                      input corresponding to the last  pixel 
                      of  each  input line.  Default  is  to 
                      obtain R2 from the history labels.
.VARIABLE RESO
FORWARD:
     RESO=B           B (REAL) is the angular resolution, in 
                      degrees   of  the   polar   coordinate 
                      picture. One line spacing in the polar 
                      coordinate   image  corresponds  to  B 
                      degrees. Default is B=arctan(1./R2).
                      Note  that the default for  B  retains 
                      all     pixel     separation,      and 
                      interpolation  is  between pixels  for 
                      all radii less than R2.  The number of 
                      output lines necessary to contain  the 
                      entire sector of input is NL=(A2-A1)/B+1,
                      so   that   B   may  be  considered  a 
                      scaling  factor in the line  direction 
                      for  fitting  the output image to  the 
                      output picture size  constraints.  See 
                      parameter AUTO.
.PAGE
INVERSE:
     RESO=B           B (REAL) is the angular change in each 
                      line  of the polar input specified  in 
                      degrees and is defined as:

                              B=(A2-A1)/(NL  -1).
                                           IN

                      Default is to calculate B based on the 
                      history labels.
.VARIABLE RSCALE
FORWARD:
     RSCALE=S         S  (REAL) is the radial scaling factor 
                      (as   RESO  is  the  angular   scaling 
                      factor)  for compression or  expansion 
                      of  the output picture in  the  radial 
                      direction.  Default is S=1.. For S>1., 
                      the output picture is compressed,  and 
                      for  0.<S<1.,  the  output picture  is 
                      expanded. See parameter AUTO.

INVERSE:
     RSCALE=S         S  (REAL) is the radial scaling factor 
                      and is defined as:

                       S=(R2-R1)/(NS  -1) (NS  =PIXELS)
                                    IN       IN

                      Default is to calculate S based on the 
                      history labels.
.VARIABLE NOLABEL
FORWARD:
     'NOLABEL         This parameter specifies that there is 
                      to  be no history  label  update.  The 
                      default  is to perform a history label 
                      update on the output picture,  quanti-
                      tatively   describing   the    forward 
                      transformation  for future use by  the 
                      INVERSE   mode.    The  use  of  label 
                      updating is recommended because of the 
                      cumbersome  calculations  required  of 
                      the   user  for  the  "INVERSE   MODE" 
                      parameters.
INVERSE:
     'NOLABEL         This  parameter  specifies that  there 
                      are  NO  history labels on  the  input 
                      data set specifying the parameter used 
                      by  the forward mode.  If the  history 
                      labels  created  by the  forward  mode 
                      still exist on the input data set, and 
                      the  user  desires  to  transform  the 
                      polar coordinate input to  rectangular 
                      coordinate  output,  with EXACTLY  the 
                      reverse  transformation of the forward 
                      mode,  the only parameter necessary is 
                      INVERSE.   Default  is  that  POLARECT 
                      history labels are assumed to exist on 
                      the  input  data set,  and  all  input 
                      parameters   not  specified   in   the 
                      parameter  list are obtained from  the 
                      history labels.
.VARIABLE AUTO
     'AUTO            This  parameter fits the input  sector 
                      to  the output picture size  specified 
                      in  the VICAR size field by  adjusting 
                      the angular resolution parameter B and 
                      the   radial  scaling  factor   S   as 
                      follows:

                      S=(R2-R1)/(NSout-1) (NSout=PIXELS)

                      B=(A2-A1)/(NLout-1).

                      By the use of this parameter, the user 
                      is  assured that the output picture is 
                      filled with the desired  sector,  with 
                      no  loss of data due to truncation  of 
                      data at output picture boundaries.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpolarect.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
! Generate BYTE test image and test BYTE mode.
spot a nl=100 ns=100 
list a size=(51 51 20 20)
polarect a b (1 1 100 50) 'AUTO XCEN=50 YCEN=50
list b size=(1 1 20 20)
polarect b c (1 1 100 100) 'INVERSE 
list c size=(51 51 20 20)
! Generate HALF test image and test HALF mode.
cform a ah oform=half
list ah size=(51 51 20 20)
polarect ah bh (1 1 100 50) 'AUTO XCEN=50 YCEN=50
list bh size=(1 1 20 20)
polarect bh ch (1 1 100 100) 'INVERSE
list ch size=(51 51 20 20)
end-proc
$!-----------------------------------------------------------------------------
$ create tstpolarect.log_solos
tstpolarect
spot a nl=100 ns=100
Beginning VICAR task spot
SPOT version 02-MAY-94
****GAUSSIAN PATTERN GENERATED
list a size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
polarect a b (1 1 100 50) 'AUTO XCEN=50 YCEN=50
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
 INPUT IMAGE STATISTICS FOR XCEN=  50.000, YCEN=  50.000
  SIZE: NL=1024 NS=1024
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
  OPTIMAL ANGULAR RESOLUTION= 0.04160 YIELDS 8656 LINES
  OPTIMAL RADIAL SCALE=   1.000 YIELDS 1378  SAMPLE
OUTPUT IMAGE STATISTICS
  SIZE: NL= 100 NS=  50
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
          ANGULAR RESOLUTION= 3.63636
          RADIAL SCALE=28.11110
VICAR HISTORY LABEL WRITTEN:
         PHIMIN=   0.00000 AT LINE 1, PHIMAX= 360.00000 AT LINE 100
         RMIN=   0.00000 AT SAMP 1, RMAX=1377.44397 AT SAMP  50
         CENTER OF TRANSFORMATION XCEN=   50.000, YCEN=   50.000
 973 INPUT LINES COMPLETE
1024 INPUT LINES COMPLETE
list b size=(1 1 20 20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Wed Oct 27 17:39:12 2010
 Task:POLARECT  User:lwk       Date_Time:Wed Oct 27 17:39:13 2010
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1   1
      4       0   0   0   0   0   0   0   0   0   0   0   0   1   1   2   2   3   3   4   4
      5       0   0   0   0   0   0   0   0   0   0   0   1   1   2   3   4   6   7   8   8
      6       0   0   0   0   0   0   0   0   0   0   1   1   2   4   6   8  10  12  15  16
      7       0   0   0   0   0   0   0   0   0   0   1   2   4   6   9  12  16  21  25  29
      8       0   0   0   0   0   0   0   0   0   1   1   3   5   8  13  18  25  33  40  48
      9       0   0   0   0   0   0   0   0   0   1   2   4   7  11  17  26  36  47  60  73
     10       0   0   0   0   0   0   0   0   0   1   2   5   8  14  22  33  47  64  83 101
     11       0   0   0   0   0   0   0   0   0   1   3   5  10  17  27  41  58  80 104 130
     12       0   0   0   0   0   0   0   0   0   1   3   6  11  19  30  46  67  92 121 152
     13       0   0   0   0   0   0   0   0   0   1   3   6  12  20  32  49  71  99 130 164
     14       0   0   0   0   0   0   0   0   0   1   3   6  12  19  32  48  70  97 129 162
     15       0   0   0   0   0   0   0   0   0   1   3   6  11  18  29  45  65  89 118 147
     16       0   0   0   0   0   0   0   0   0   1   3   5  10  16  26  39  56  76  99 123
     17       0   0   0   0   0   0   0   0   0   1   2   4   8  13  21  31  44  60  77  94
     18       0   0   0   0   0   0   0   0   0   1   2   4   6  10  16  24  33  44  55  66
     19       0   0   0   0   0   0   0   0   0   1   1   3   5   8  11  17  23  30  36  43
     20       0   0   0   0   0   0   0   0   0   0   1   2   3   5   8  11  15  19  22  26
polarect b c (1 1 100 100) 'INVERSE
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
INVERSE MODE SPECIFIED
 VICAR HISTORY LABEL READ:
 PHIMIN =    0.00000
 PHIMAX =    6.28318
 RMIN =    0.00000
 RMAX = 1377.43994
 XCEN =   50.00000
 YCEN =   50.00000
 RESO =    0.06347
 SCALE =   28.11110
 100 INPUT LINES COMPLETE
list c size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
cform a ah oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
list ah size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
polarect ah bh (1 1 100 50) 'AUTO XCEN=50 YCEN=50
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
 INPUT IMAGE STATISTICS FOR XCEN=  50.000, YCEN=  50.000
  SIZE: NL=1024 NS=1024
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
  OPTIMAL ANGULAR RESOLUTION= 0.04160 YIELDS 8656 LINES
  OPTIMAL RADIAL SCALE=   1.000 YIELDS 1378  SAMPLE
OUTPUT IMAGE STATISTICS
  SIZE: NL= 100 NS=  50
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
          ANGULAR RESOLUTION= 3.63636
          RADIAL SCALE=28.11110
VICAR HISTORY LABEL WRITTEN:
         PHIMIN=   0.00000 AT LINE 1, PHIMAX= 360.00000 AT LINE 100
         RMIN=   0.00000 AT SAMP 1, RMAX=1377.44397 AT SAMP  50
         CENTER OF TRANSFORMATION XCEN=   50.000, YCEN=   50.000
 486 INPUT LINES COMPLETE
 972 INPUT LINES COMPLETE
1024 INPUT LINES COMPLETE
list bh size=(1 1 20 20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:SPOT      User:lwk       Date_Time:Wed Oct 27 17:39:12 2010
 Task:POLARECT  User:lwk       Date_Time:Wed Oct 27 17:39:14 2010
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line

      3         0     0     0     0     0     0     0     0     0     0     0     0     0     0     1
      4         0     0     0     0     0     0     0     0     0     0     0     0     1     1     2
      5         0     0     0     0     0     0     0     0     0     0     0     1     1     2     3
      6         0     0     0     0     0     0     0     0     0     0     1     1     2     4     6
      7         0     0     0     0     0     0     0     0     0     0     1     2     4     6     9
      8         0     0     0     0     0     0     0     0     0     1     1     3     5     8    13
      9         0     0     0     0     0     0     0     0     0     1     2     4     7    11    17
     10         0     0     0     0     0     0     0     0     0     1     2     5     8    14    22
     11         0     0     0     0     0     0     0     0     0     1     3     5    10    17    27
     12         0     0     0     0     0     0     0     0     0     1     3     6    11    19    30
     13         0     0     0     0     0     0     0     0     0     1     3     6    12    20    32
     14         0     0     0     0     0     0     0     0     0     1     3     6    12    19    32
     15         0     0     0     0     0     0     0     0     0     1     3     6    11    18    29
     16         0     0     0     0     0     0     0     0     0     1     3     5    10    16    26
     17         0     0     0     0     0     0     0     0     0     1     2     4     8    13    21
     18         0     0     0     0     0     0     0     0     0     1     2     4     6    10    16
     19         0     0     0     0     0     0     0     0     0     1     1     3     5     8    11
     20         0     0     0     0     0     0     0     0     0     0     1     2     3     5     8

   HALF     samples are interpreted as HALFWORD data
 Task:SPOT      User:lwk       Date_Time:Wed Oct 27 17:39:12 2010
 Task:POLARECT  User:lwk       Date_Time:Wed Oct 27 17:39:14 2010
     Samp      16    17    18    19    20
   Line

      3         1     1     1     1     1
      4         2     3     3     4     4
      5         4     6     7     8     8
      6         8    10    12    15    16
      7        12    16    21    25    29
      8        18    25    33    40    48
      9        26    36    47    60    73
     10        33    47    64    83   101
     11        41    58    80   104   130
     12        46    67    92   121   152
     13        49    71    99   130   164
     14        48    70    97   129   162
     15        45    65    89   118   147
     16        39    56    76    99   123
     17        31    44    60    77    94
     18        24    33    44    55    66
     19        17    23    30    36    43
     20        11    15    19    22    26
polarect bh ch (1 1 100 100) 'INVERSE
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
INVERSE MODE SPECIFIED
 VICAR HISTORY LABEL READ:
 PHIMIN =    0.00000
 PHIMAX =    6.28318
 RMIN =    0.00000
 RMAX = 1377.43994
 XCEN =   50.00000
 YCEN =   50.00000
 RESO =    0.06347
 SCALE =   28.11110
 100 INPUT LINES COMPLETE
list ch size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
end-proc
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpolarect.log_linux
tstpolarect
spot a nl=100 ns=100
Beginning VICAR task spot
SPOT version 02-MAY-94
****GAUSSIAN PATTERN GENERATED
list a size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
polarect a b (1 1 100 50) 'AUTO XCEN=50 YCEN=50
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
 INPUT IMAGE STATISTICS FOR XCEN=  50.000, YCEN=  50.000
  SIZE: NL=1024 NS=1024
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
  OPTIMAL ANGULAR RESOLUTION= 0.04160 YIELDS 8656 LINES
  OPTIMAL RADIAL SCALE=   1.000 YIELDS 1378  SAMPLE
OUTPUT IMAGE STATISTICS
  SIZE: NL= 100 NS=  50
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
          ANGULAR RESOLUTION= 3.63636
          RADIAL SCALE=28.11110
VICAR HISTORY LABEL WRITTEN:
         PHIMIN=   0.00000 AT LINE 1, PHIMAX= 359.99997 AT LINE 100
         RMIN=   0.00000 AT SAMP 1, RMAX=1377.44397 AT SAMP  50
         CENTER OF TRANSFORMATION XCEN=   50.000, YCEN=   50.000
 973 INPUT LINES COMPLETE
1024 INPUT LINES COMPLETE
list b size=(1 1 20 20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:SPOT      User:lwk       Date_Time:Thu Oct 28 16:07:13 2010
 Task:POLARECT  User:lwk       Date_Time:Thu Oct 28 16:07:14 2010
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1   1
      4       0   0   0   0   0   0   0   0   0   0   0   0   1   1   2   2   3   3   4   4
      5       0   0   0   0   0   0   0   0   0   0   0   1   1   2   3   4   6   7   8   8
      6       0   0   0   0   0   0   0   0   0   0   1   1   2   4   6   8  10  12  15  16
      7       0   0   0   0   0   0   0   0   0   0   1   2   4   6   9  12  16  21  25  29
      8       0   0   0   0   0   0   0   0   0   1   1   3   5   8  13  18  25  33  40  48
      9       0   0   0   0   0   0   0   0   0   1   2   4   7  11  17  26  36  47  60  73
     10       0   0   0   0   0   0   0   0   0   1   2   5   8  14  22  33  47  64  83 101
     11       0   0   0   0   0   0   0   0   0   1   3   5  10  17  27  41  58  80 104 130
     12       0   0   0   0   0   0   0   0   0   1   3   6  11  19  30  46  67  92 121 152
     13       0   0   0   0   0   0   0   0   0   1   3   6  12  20  32  49  71  99 130 164
     14       0   0   0   0   0   0   0   0   0   1   3   6  12  19  32  48  70  97 129 162
     15       0   0   0   0   0   0   0   0   0   1   3   6  11  18  29  45  65  89 118 147
     16       0   0   0   0   0   0   0   0   0   1   3   5  10  16  26  39  56  76  99 123
     17       0   0   0   0   0   0   0   0   0   1   2   4   8  13  21  31  44  60  77  94
     18       0   0   0   0   0   0   0   0   0   1   2   4   6  10  16  24  33  44  55  66
     19       0   0   0   0   0   0   0   0   0   1   1   3   5   8  11  17  23  30  36  43
     20       0   0   0   0   0   0   0   0   0   0   1   2   3   5   8  11  15  19  22  26
polarect b c (1 1 100 100) 'INVERSE
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
INVERSE MODE SPECIFIED
 VICAR HISTORY LABEL READ:
 PHIMIN =    0.00000
 PHIMAX =    6.28318
 RMIN =    0.00000
 RMAX = 1377.43994
 XCEN =   50.00000
 YCEN =   50.00000
 RESO =    0.06347
 SCALE =   28.11110
 100 INPUT LINES COMPLETE
list c size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
cform a ah oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
list ah size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
polarect ah bh (1 1 100 50) 'AUTO XCEN=50 YCEN=50
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
 INPUT IMAGE STATISTICS FOR XCEN=  50.000, YCEN=  50.000
  SIZE: NL=1024 NS=1024
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
  OPTIMAL ANGULAR RESOLUTION= 0.04160 YIELDS 8656 LINES
  OPTIMAL RADIAL SCALE=   1.000 YIELDS 1378  SAMPLE
OUTPUT IMAGE STATISTICS
  SIZE: NL= 100 NS=  50
  ANGULAR RANGE: PHIMIN=  0.0000, PHIMAX=360.0000
  RADIAL RANGE: RMIN=   0.000, RMAX=1377.444
          ANGULAR RESOLUTION= 3.63636
          RADIAL SCALE=28.11110
VICAR HISTORY LABEL WRITTEN:
         PHIMIN=   0.00000 AT LINE 1, PHIMAX= 359.99997 AT LINE 100
         RMIN=   0.00000 AT SAMP 1, RMAX=1377.44397 AT SAMP  50
         CENTER OF TRANSFORMATION XCEN=   50.000, YCEN=   50.000
 486 INPUT LINES COMPLETE
 972 INPUT LINES COMPLETE
1024 INPUT LINES COMPLETE
list bh size=(1 1 20 20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:SPOT      User:lwk       Date_Time:Thu Oct 28 16:07:13 2010
 Task:POLARECT  User:lwk       Date_Time:Thu Oct 28 16:07:14 2010
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line

      3         0     0     0     0     0     0     0     0     0     0     0     0     0     0     1
      4         0     0     0     0     0     0     0     0     0     0     0     0     1     1     2
      5         0     0     0     0     0     0     0     0     0     0     0     1     1     2     3
      6         0     0     0     0     0     0     0     0     0     0     1     1     2     4     6
      7         0     0     0     0     0     0     0     0     0     0     1     2     4     6     9
      8         0     0     0     0     0     0     0     0     0     1     1     3     5     8    13
      9         0     0     0     0     0     0     0     0     0     1     2     4     7    11    17
     10         0     0     0     0     0     0     0     0     0     1     2     5     8    14    22
     11         0     0     0     0     0     0     0     0     0     1     3     5    10    17    27
     12         0     0     0     0     0     0     0     0     0     1     3     6    11    19    30
     13         0     0     0     0     0     0     0     0     0     1     3     6    12    20    32
     14         0     0     0     0     0     0     0     0     0     1     3     6    12    19    32
     15         0     0     0     0     0     0     0     0     0     1     3     6    11    18    29
     16         0     0     0     0     0     0     0     0     0     1     3     5    10    16    26
     17         0     0     0     0     0     0     0     0     0     1     2     4     8    13    21
     18         0     0     0     0     0     0     0     0     0     1     2     4     6    10    16
     19         0     0     0     0     0     0     0     0     0     1     1     3     5     8    11
     20         0     0     0     0     0     0     0     0     0     0     1     2     3     5     8

   HALF     samples are interpreted as HALFWORD data
 Task:SPOT      User:lwk       Date_Time:Thu Oct 28 16:07:13 2010
 Task:POLARECT  User:lwk       Date_Time:Thu Oct 28 16:07:14 2010
     Samp      16    17    18    19    20
   Line

      3         1     1     1     1     1
      4         2     3     3     4     4
      5         4     6     7     8     8
      6         8    10    12    15    16
      7        12    16    21    25    29
      8        18    25    33    40    48
      9        26    36    47    60    73
     10        33    47    64    83   101
     11        41    58    80   104   130
     12        46    67    92   121   152
     13        49    71    99   130   164
     14        48    70    97   129   162
     15        45    65    89   118   147
     16        39    56    76    99   123
     17        31    44    60    77    94
     18        24    33    44    55    66
     19        17    23    30    36    43
     20        11    15    19    22    26
polarect bh ch (1 1 100 100) 'INVERSE
Beginning VICAR task polarect
POLARECT version 27-Oct-2010
WORK CORE AVAILABLE= 997952 BYTES
INVERSE MODE SPECIFIED
 VICAR HISTORY LABEL READ:
 PHIMIN =    0.00000
 PHIMAX =    6.28318
 RMIN =    0.00000
 RMAX = 1377.43994
 XCEN =   50.00000
 YCEN =   50.00000
 RESO =    0.06347
 SCALE =   28.11110
 100 INPUT LINES COMPLETE
list ch size=(51 51 20 20)
Beginning VICAR task list
 ** The specified window is all zero.
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
