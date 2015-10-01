$!****************************************************************************
$!
$! Build proc for MIPL module ptp
$! VPACK Version 1.9, Thursday, December 29, 2011, 15:22:05
$!
$! Execute by entering:		$ @ptp
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
$ write sys$output "*** module ptp ***"
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
$ write sys$output "Invalid argument given to ptp.com file -- ", primary
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
$   if F$SEARCH("ptp.imake") .nes. ""
$   then
$      vimake ptp
$      purge ptp.bld
$   else
$      if F$SEARCH("ptp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ptp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ptp.bld "STD"
$   else
$      @ptp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ptp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ptp.com -mixed -
	-s ptp.f open_images.f get_inp_nav.f get_ref_nav.f fix_pointing.f -
	   icproject.f project_quad.f project_quad2.f check_gridsize.f psub.f -
	   old_ptp_sub.f -
	-p ptp.pdf -
	-i ptp.imake -
	-t tstptp.pdf tstptp.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ptp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR PROGRAM PTP:  Perspective-to-perspective projection.
C
      include 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      COMMON/CIO/IUNIT,OUNIT,DCODE
      INTEGER*4 IUNIT,OUNIT,DCODE

      COMMON/CINP/SL,SS,NLO,NSO,NLI,NSI,INCo
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,INCo

C     ....Navigation data for input image
      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

C     ....Navigation data for reference image
      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      COMMON/PCONST/RA,RB,RC
      REAL*8 RA,RB,RC

      REAL*8 SBUF(100),SBUF2(100)
      INTEGER*4 IBUF(200),IBUF2(200)
      EQUIVALENCE (SBUF,IBUF),(SBUF2,IBUF2)

      REAL*8 T,T2,SROT
      INTEGER*4 N1,N2
      CHARACTER*12 TARGET
      EXTERNAL ICPROJECT		!For STACKA

      CALL XVMESSAGE(' PTP Version 29-Dec-2011',' ')

C     ...Open input and output images
      CALL OPEN_IMAGES(iunit,ounit,dcode,sl,ss,nlo,nso,nli,nsi)

C     ....Get navigation data for input and reference images
      CALL GET_INP_NAV(IUNIT,sbuf,ibuf,t)		!Store data in C1
      CALL PBNAME(IBUF(9),target,*994)			!Get target name
      CALL GET_RADII(TARGET,ra,rb,rc,srot)		!Get target radii
      CALL GET_REF_NAV(PROJECT,TARGET,ITYPE,sbuf2,ibuf2,t2) !Store data in C2

C     ....Process parameters PC, RPC, and ROT to correct navigation
      CALL FIX_POINTING(SBUF,SBUF2,TARGET,T,T2,SROT)

      RX2(1) = RS2(1)/RA
      RX2(2) = RS2(2)/RB
      RX2(3) = RS2(3)/RC
      RX2MAG = RX2(1)*RX2(1) + RX2(2)*RX2(2) + RX2(3)*RX2(3)

C     ....Compute size of input and output buffers
      INCo = 32			!Size of default grid (bytes)
      N1 = NLI*NSI		!Size of input picture buffer (bytes)
      N2 = INCo*NSO		!Size of output picture buffer (bytes)
      IF (DCODE.EQ.2) THEN	!If input is halfword,
         N1 = 2*N1		!buffers must be twice as large.
         N2 = 2*N2
      ENDIF

C     ....Project the input image
      CALL STACKA(5,ICPROJECT,2,N1,N2,INCo)
      CALL XVMESSAGE(' ***PTP task completed',' ')
      RETURN

  994 CALL XVMESSAGE('***Invalid target name',' ')
      CALL MABEND('***PTP task cancelled')
      END
C------------------------------------------------------------------	  
C Convert SCET into Days
C          T = days since Jan 1, 1950.
C          
      SUBROUTINE GET_TIME(IBUF,t)
      IMPLICIT NONE
      INTEGER*4 IBUF(8)
      REAL*8 T

      INTEGER*4 IDATE,ITIME,IYEAR,IDAY,IHOUR,IMIN,ISEC,JTIME
      REAL*8 HOUR

      IDATE = 1000*MOD(IBUF(3),100) + IBUF(4)	! IDATE = YYDDD
      ITIME = 10000000*IBUF(5) + 100000*IBUF(6)
     &         + 1000*IBUF(7) + IBUF(8)		! ITIME = HHMMSSMMM
C         Time since 1950 (days)
      IYEAR = IBUF(3) - 1900
      IDAY = 365.25*IYEAR - 18262.125 + IBUF(4) !days since 1950
      JTIME = ITIME/1000
      ISEC = MOD(JTIME,100)
      JTIME = JTIME/100
      IMIN = MOD(JTIME,100)
      IHOUR = JTIME/100
      HOUR = IHOUR + (ISEC/60.D0 + IMIN)/60.0D0
      T = IDAY + HOUR/24.0D0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get target radii and rotation rate (deg/day)
C
      SUBROUTINE GET_RADII(TARGET,ra,rb,rc,srot)
      IMPLICIT NONE
      CHARACTER*8 TARGET
      REAL*8 RA,RB,RC,SROT

      REAL*8 R(3)
      REAL*4 DATA(20)
      INTEGER*4 DEF,NUM

      call init_spice

      CALL PBDATA(TARGET,data,*994)
      RA = DATA(1)			!Equatorial radius, long axis
      RB = DATA(2)			!Equatorial radius, short axis
      RC = DATA(3)			!Polar radius
      SROT = 360.D0/DATA(5)		!rotation rate in degrees/day

      CALL XVPARMD('RADII',r,num,def,1)
      IF (DEF.NE.1) THEN
         RA = r(1)			!Equatorial radius, long axis
         RB = r(2)			!Equatorial radius, short axis
         RC = r(3)			!Polar radius
      ENDIF
      RETURN

  994 CALL XVMESSAGE('***Invalid target name',' ')
      CALL XVMESSAGE('***PTP task cancelled',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute approximate transformation:
C    u = a1 + a2*x + a3*y + a4*x*y
C    v = b1 + b2*x + b3*y + b4*x*y
C where (x,y) and (u,v) are the (line,samp) coordinates of the reference
C and input images.  The a's and b's are solved for such that the above
C equation is true for the four corners.
C
      SUBROUTINE FINDT (IX,IY,U,V,a,b,ind)
      IMPLICIT NONE
      INTEGER*4 IX(4),IY(4)	!Location of four corners in ref image
      REAL*8 U(4),V(4)		!Location of four corners in input image
      REAL*4 a(4),b(4)		!Transformation coefficients (output)
      INTEGER*4 IND		!Return status (1=singular matrix)

      REAL*4 X(4),Y(4),M(4,4)
      INTEGER I,N

      DO I=1,4
        X(I) = IX(I)
        Y(I) = IY(I)
        A(I) = U(I)
        B(I) = V(I)
      ENDDO

      DO I=1,4
        M(I,1) = 1
        M(I,2) = X(I)
        M(I,3) = Y(I)
        M(I,4) = X(I)*Y(I)
      ENDDO

      N = 4
      CALL SIMQ(M,a,N,IND)

      DO I=1,4
        M(I,1) = 1
        M(I,2) = X(I)
        M(I,3) = Y(I)
        M(I,4) = X(I)*Y(I)
      ENDDO

      CALL SIMQ(M,b,N,IND)

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create open_images.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input and output images.
C
      SUBROUTINE OPEN_IMAGES(iunit,ounit,dcode,sl,ss,nlo,nso,nli,nsi)
      IMPLICIT NONE
      INTEGER*4 IUNIT,OUNIT,DCODE,SL,SS,NLO,NSO,NLI,NSI

      INTEGER STATUS
      CHARACTER*8 FORMAT

C     ...Open input image
      CALL XVUNIT(iunit,'INP',1,status,' ')
      CALL XVOPEN(IUNIT,status,'OPEN_ACT','SA','IO_ACT','SA',' ')
      IF (STATUS.NE.1) CALL XVSIGNAL(IUNIT,STATUS,1) 

C     ...Get data format
      CALL XVGET(IUNIT,status,'FORMAT',format,' ')
      IF (FORMAT.EQ.'BYTE') THEN
         DCODE = 1				!Byte data format
      ELSE IF (FORMAT.EQ.'HALF' .OR. FORMAT.EQ.'WORD') THEN
         DCODE = 2				!Halfword data format
      ELSE
         CALL XVMESSAGE('***Invalid data format',' ')
         CALL MABEND('***PTP task cancelled')
      ENDIF

      CALL XVSIZE(sl,ss,nlo,nso,nli,nsi)

C     ...Open output image
      CALL XVUNIT(OUNIT,'OUT',1,status,' ')
      CALL XVOPEN(OUNIT,status,'U_NL',NLO,'U_NS',NSO,
     +      'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_inp_nav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------
C Get navigation data for the input image and load into C1.
C
      SUBROUTINE GET_INP_NAV(IUNIT,sbuf,ibuf,t)
      IMPLICIT NONE
      INTEGER*4 IUNIT		!Input image logical unit number
      REAL*8 SBUF(100)		!Returned SPICE/SEDR buffer
      INTEGER*4 IBUF(200)	!    "      "    "     "
      REAL*8 T			!Return time in days

      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

      REAL*4 XFL,XOAL,XOAS,XSCALE
      INTEGER*4 IFDSC,IND,LAB(80),DATA(40)
      INTEGER MP,STATUS,ISOURCE

C     ...Get project, camera S/N, and SCLK
      CALL GETPROJ(IUNIT,project,icam,ifdsc,ind)
      IF (IND.NE.0) CALL MABEND('***Invalid input image project ID')
      CALL XVMESSAGE('Input image spacecraft is '//PROJECT,' ')

C     ...Determine if input image is geometrically corrected
      CALL MP_INIT(mp,status)
      CALL MP_LABEL_READ(MP,IUNIT,status)
      IF (STATUS.GE.0) THEN	!Map labels found
	 CALL MP_MPO2BUF(MP,data,status)
         ITYPE = DATA(39)
      ELSE
	 CALL SEARC_DISTOR(IUNIT,status)
          ITYPE = 7			!Geometrically uncorrected (image-space)
	  IF (STATUS.EQ.1) ITYPE=8	!Geometrically corrected (object_space)
      ENDIF
      IF (ITYPE.EQ.7 .AND. (PROJECT.EQ.'VIKOR'.OR.
     +		PROJECT.EQ.'VGR-1'.OR.PROJECT.EQ.'VGR-2'))
     +	CALL MABEND('Input image must be geometricaly corrected')
      IF (ITYPE.EQ.7) THEN
         CALL MVCL(PROJECT,conv,5) 
         CONV(3) = ICAM
      ENDIF

C     ....Get label buffer
      CALL GETLABCON(IUNIT,PROJECT,lab,ind)
      IF (IND.GT.1) CALL MABEND('***Err reading input image label')

C     ....Get SPICE buffer
      CALL GETSPICE4(PROJECT,1,LAB,sbuf,ind)
      IF (IND.NE.1) CALL MABEND('***SPICE err for input image')
      CALL CMSOURCE(SBUF,isource)	!Print source of C matrix

C     ....Get camera constants
      CALL GETCAMCON(PROJECT,ICAM,xfl,xoal,xoas,xscale,ind)
      IF (IND.NE.0) CALL MABEND('***Invalid input image camera S/N')
      FL = XFL					!Camera focal length (mm)
      OAL = XOAL				!Optical axis line
      OAS = XOAS				!Optical axis sample
      SCALE = XSCALE				!Picture scale (pixels/mm)
      ZSCALE = FL*SCALE

C     ....Get camera pointing and target-center-to-spacecraft vector
      CALL MVE(8,9,SBUF(59),om,1,1)		!OM matrix
      CALL MVE(8,3,SBUF(22),rs,1,1)		!RS vector
      CALL GET_TIME(SBUF,t)			!Days since 1950
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_ref_nav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------
C Get navigation data for reference image.
C
      SUBROUTINE GET_REF_NAV(PROJECT,TARGET,ITYPE,sbuf2,ibuf2,t2)
      IMPLICIT NONE
      CHARACTER*5 PROJECT
      CHARACTER*12 TARGET
      REAL*8 SBUF2(100)
      INTEGER*4 ITYPE,IBUF2(200)
      REAL*8 T2

      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      INTEGER*4 LAB(80),DATA(40),DEF,RUNIT,NUM,IND,IFDSC,I,ISOURCE
      REAL*4 XFL,XOAL,XOAS,XSCALE
      REAL*8 MP

      CHARACTER*128 REFFILE

C     ....Get info for the reference image (if given)
      CALL XVPARM('REF',reffile,num,def,1)
      IF (NUM.NE.1) GOTO 10
      CALL XVUNIT(runit,'N/A',1,ind,'U_NAME',REFFILE,' ')
      CALL XVOPEN(RUNIT,ind, 'OPEN_ACT', 'SA', 'IO_ACT','SA',' ')
      CALL GETPROJ(RUNIT,project2,icam2,ifdsc,ind)
      IF (IND.NE.0) CALL MABEND('***Invalid ref image project ID')
      CALL XVMESSAGE('Reference image spacecraft is '//PROJECT2,' ')
      CALL GETLABCON(RUNIT,project2,lab,ind)
      IF (IND.GT.1) CALL MABEND('***Err reading ref image label')
      ICAM2 = LAB(6)

C     ...Determine if input image is geometrically corrected
      CALL MP_INIT(mp,ind)
      CALL MP_LABEL_READ(MP,RUNIT,ind)
      IF (IND.GE.0) THEN     !Map labels found
         CALL MP_MPO2BUF(MP,data,ind)
         ITYPE = DATA(39)
      ELSE
         CALL SEARC_DISTOR(RUNIT,ind)
         ITYPE2 = 7             !Geometrically uncorrected (image-space)
         IF (IND.EQ.1) ITYPE2=8	!Geometrically corrected (object_space)
      ENDIF
      IF (ITYPE2.EQ.7 .AND. (PROJECT2.EQ.'VIKOR'.OR.
     +          PROJECT2.EQ.'VGR-1'.OR.PROJECT2.EQ.'VGR-2'))
     +  CALL MABEND('Reference image must be geometricaly corrected')
      GOTO 30
C
C     ....Here if reference frame is not specified
   10 CALL XVMESSAGE('No reference image provided',' ')
      DO I=1,80
         LAB(I) = -999		!Initialize dummy label buffer
      ENDDO
      CALL MVCL(TARGET,lab(25),12)
      CALL XVPARM('RMISSION',project2,num,def,1)
      IF (DEF.EQ.1) PROJECT2=PROJECT
      CALL XVPARM('RSCET',lab(8),num,def,6)
      IF (DEF.EQ.1) CALL MABEND('***RSCET parameter required')
      CALL XVPARM('RCAM',icam2,num,def,1)	!reference camera S/N
      IF (DEF.EQ.1) CALL MABEND('***RCAM parameter required')
      LAB(6) = ICAM2
      ITYPE2 = ITYPE

C     ....Get camera focal length, optical axis line,samp, picture scale
   30 CALL GETCAMCON(PROJECT2,ICAM2,xfl,xoal,xoas,xscale,ind)
      IF (IND.NE.0) CALL MABEND('***Invalid camera S/N for ref image')
      FL2 = XFL
      OAL2 = XOAL
      OAS2 = XOAS
      SCALE2 = XSCALE
      ZSCALE2 = FL2*SCALE2
      IF (ITYPE2.EQ.7) THEN
         CALL MVCL(PROJECT2,conv2,5) 
         CONV2(3) = ICAM2
      ENDIF

C     ....Get geometry data for reference image
      CALL GETSPICE4(PROJECT2,.TRUE.,LAB,sbuf2,ind)
      IF (IND.NE.1) CALL MABEND('***Err getting SPICE for ref image')
      CALL CMSOURCE(SBUF2,isource)	!Print C matrix source
      CALL MVE(8,9,SBUF2(59),om2,1,1)		!OM matrix
      CALL MVE(8,3,SBUF2(22),rs2,1,1)		!RS vector
      CALL GET_TIME(SBUF2,t2)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fix_pointing.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------
C If PC or RPC is specified, use them to fix the camera pointing for
C the input or reference image.  The OM matrix is updated.
C If rotation rate is specified, adjust SCLON to match rotation over
C time inverval T-T2.  The OM matrix is updated.
C
      SUBROUTINE FIX_POINTING(SBUF,SBUF2,TARGET,T,T2,SROT)
      IMPLICIT NONE
      REAL*8 SBUF(100),SBUF2(100)
      CHARACTER*12 TARGET	!Target name
      REAL*8 T,T2		!Time (days) for input and ref images
      REAL*8 SROT		!Rotation rate (degrees/day) from SEDR

      COMMON/CINC/DL,DS,INCLUDE
      REAL*8 DL,DS

      COMMON/SCALING/SCALE_FACTOR
      REAL*8 SCALE_FACTOR      !RRP FR89270
      INTEGER*4 INCLUDE

      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      LOGICAL XVPTST
      REAL*8 SCLINE,SCSAMP,SCLAT,SCLON,ANGLN,RANGE
      REAL*8 SCLINE2,SCSAMP2,SCLAT2,SCLON2,ANGLN2,RANGE2
      REAL*8 ROT		!Rotation rate specified by user
      REAL*8 SCLON0		!Spacecraft longitude from SEDR
      REAL*4 PAR(2)
      INTEGER*4 NUM,DEF
      CHARACTER*128 MSG
  110 FORMAT(' OS target center=(',F10.2,',',F10.2,')')
  112 FORMAT(' OS target center for ref image=(',F10.2,',',F10.2,')')
 1004 FORMAT(' SEDR rotation rate is',F9.4,' degrees/day')
 1003 FORMAT(' New rotation rate is',F9.4,' degrees/day')
 1002 FORMAT(' Spacecraft longitude adjusted by',F8.3,' degrees')

      SCLINE = SBUF(69)				!Target center line
      SCSAMP = SBUF(70)				!Target center sample
      SCLAT = SBUF(30)				!Spacecraft latitude
      SCLON = DMOD(SBUF(31)+360.d0,360.d0)	!Spacecraft longitude
      ANGLN = DMOD(SBUF(68)+90.d0,360.d0)	!North angle
      RANGE = SBUF(27)				!Target range (km)

      CALL XVPARM('PC',par,num,def,2)
      IF (DEF.NE.1) THEN			!If PC is specified, then
         SCLINE = PAR(1)			!compute the OM-matrix from it.
         SCSAMP = PAR(2)
         CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,SCALE,FL,SCLON,SCLAT,ANGLN,
     &      RANGE,om,rs)
      ENDIF

      WRITE(MSG,110) SCLINE,SCSAMP
      CALL XVMESSAGE(MSG,' ')

      SCLINE2 = SBUF2(69)			!Target center line
      SCSAMP2 = SBUF2(70)			!Target center sample

      CALL XVPARM('RPC',par,num,def,2)
      IF (DEF.NE.1) THEN			!If RPC is specified, then
         SCLINE2 = PAR(1)
         SCSAMP2 = PAR(2)
         SCLAT2 = SBUF2(30)			!Spacecraft latitude
         SCLON2 = DMOD(SBUF2(31)+360.d0,360.d0)	!Spacecraft longitude
         ANGLN2 = DMOD(SBUF2(68)+90.d0,360.d0)	!North angle
         RANGE2 = SBUF2(27)			!Target range (km)
         CALL MOMATI(OAL2,OAS2,SCLINE2,SCSAMP2,SCALE2,FL2,SCLON2,
     &      SCLAT2,ANGLN2,RANGE2,OM2,RS2)
      ENDIF

      WRITE(MSG,112) SCLINE2,SCSAMP2
      CALL XVMESSAGE(MSG,' ')

      CALL XVPARMD('ROT',rot,num,def,1)
      IF (DEF.NE.1) THEN
         WRITE(MSG,1004) SROT
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,1003) ROT
         CALL XVMESSAGE(MSG,' ')
         SCLON0 = SCLON
         SCLON = SCLON + (T-T2)*(ROT-SROT)
         CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,SCALE,FL,SCLON,SCLAT,ANGLN,
     &      RANGE,om,rs)
         WRITE(MSG,1002) SCLON-SCLON0
         CALL XVMESSAGE(MSG,' ')
      ENDIF

C     ....Compute offset for sky background

      SCALE_FACTOR = SCALE/SCALE2 !RRP FR89270
      IF (XVPTST('INCLUDE')) THEN
         INCLUDE = 1
         DL = SCLINE/SCALE_FACTOR - SCLINE2 !RRP FR89270
         DS = SCSAMP/SCALE_FACTOR - SCSAMP2 !RRP FR89270
      ELSE
         INCLUDE = 0
      ENDIF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create icproject.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------	  
C  Project input image onto output image.  This version maintains
C  the entire input image in-core (IC).
C
      SUBROUTINE ICPROJECT(PIC,N1,BUF,N2)
      IMPLICIT NONE
      BYTE PIC(*),BUF(*)
      INTEGER*4 N1,N2

      COMMON/CINP/SL,SS,NLO,NSO,NLI,NSI,INCo
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,INCo

      COMMON/CINC/DL,DS,INCLUDE
      REAL*8 DL,DS
      INTEGER*4 INCLUDE

      COMMON/CIO/IUNIT,OUNIT,DCODE
      INTEGER*4 IUNIT,OUNIT,DCODE

      INTEGER LBEG,SBEG,ROW,COL,I,L
      INTEGER*4 INC,ADDRESS,INTERP,IND
      REAL*4 A(4),B(4)
      LOGICAL XVPTST,EXACT

      IF (N1.LT.NLI*NSI*DCODE) GOTO 990
      IF (N2.LT.NSO*INCo*DCODE) GOTO 990
C           Read in input image
      I = 1
      DO L=1,NLI
         CALL XVREAD(IUNIT,PIC(I),ind,' ')
         I = I + NSI*DCODE
      ENDDO

      EXACT = XVPTST('EXACT')

C     ....Process image in INCo X INCo blocks
      DO 110 ROW=1,NLO,INCo             !Row loop
      DO 100 COL=1,NSO,INCo             !Column loop
      INC = INCo
      ADDRESS = 0
      LBEG = ROW
      SBEG = COL

   20 IF (LBEG.GT.NLO .OR. SBEG.GT.NSO) GOTO 40
      IF (EXACT) THEN		!If EXACT is specified
         INTERP = 1		!project using exact equations.
      ELSE			!Else, see if grid size is appropriate.
         CALL CHECK_GRIDSIZE(INCLUDE,LBEG,SBEG,
     &		a,b,inc,address,interp)
      ENDIF
      IF (DCODE.EQ.1) THEN
         CALL PROJECT_QUAD(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      ELSE
         CALL PROJECT_QUAD2(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      ENDIF

   40 CALL UPDATE_ADDRESS(address,inc,lbeg,sbeg)
      IF (ADDRESS.GT.0) GOTO 20
  100 CONTINUE

      I = 1
      DO L=1,INCo
         CALL XVWRIT(OUNIT,BUF(I),ind,' ') !Write completed lines
         I = I + NSO*DCODE
      ENDDO
  110 CONTINUE
  
      RETURN

  990 CALL XVMESSAGE('***Not enough memory',' ')
      CALL XVMESSAGE('***PTP task cancelled',' ')
      CALL ABEND
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create project_quad.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Project quadrant from input PIC to output BUF (for byte data).
C
      SUBROUTINE PROJECT_QUAD(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      BYTE PIC(NSI,NLI),BUF(NSO,INCo)
      INTEGER*4 INTERP,INCLUDE,INC,INCo,LBEG,SBEG,NLI,NSI,NLO,NSO,ROW
      REAL*8 DL,DS

      COMMON/SCALING/SCALE_FACTOR
      REAL*8 SCALE_FACTOR

      REAL*8 X,Y,RLINE,RSAMP
      REAL*4 A(4),B(4)
      INTEGER*4 L,S,IL,IS,LEND,SEND,LL
      INTEGER*4 D1,D2,D3,D4,IVAL,IND

      INCLUDE 'fortport'

      LEND = LBEG + INC - 1
      SEND = SBEG + INC - 1
      IF (LEND.GT.NLO) LEND=NLO
      IF (SEND.GT.NSO) SEND=NSO
      IND = 1

      DO 30 L=LBEG,LEND
      LL = L - ROW + 1

      DO 30 S=SBEG,SEND
      IVAL=0				!Clear DN
      IF (INTERP.EQ.1) THEN
        CALL PSUB(INCLUDE,L,S,rline,rsamp,ind)
        IF (IND.EQ.0) THEN
           IF (INCLUDE.EQ.0) GOTO 30
           RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
           RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
        ENDIF        
      ELSEIF (INTERP.EQ.2) THEN
        RSAMP = A(1) + A(2)*S + A(3)*L + A(4)*L*S
        RLINE = B(1) + B(2)*S + B(3)*L + B(4)*L*S
      ELSEIF (INTERP.EQ.3) THEN
        RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
        RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
      ELSE 
        GOTO 30
      ENDIF

      IS = RSAMP
      IL = RLINE
      IF (IL.LT.1.OR.IS.LT.1) GOTO 30           !Point off image
      IF (IL.GE.NLI.OR.IS.GE.NSI) GOTO 30

C     ....Compute pixel using bilinear interpolation
      D1 = BYTE2INT(PIC(IS,IL))         !upper left pixel
      D2 = BYTE2INT(PIC(IS+1,IL))       !upper right pixel
      D3 = BYTE2INT(PIC(IS,IL+1))       !lower left pixel
      D4 = BYTE2INT(PIC(IS+1,IL+1))     !lower right pixel
      X = RSAMP - IS
      Y = RLINE - IL
      IVAL = D1 + (D2-D1)*X + (D3-D1)*Y + (D1-D2-D3+D4)*X*Y
CCC      IVAL = INTERP
CCC      IF (IND.EQ.0) IVAL=3
CCC      IF (INC.EQ.32) IVAL=32
CCC      IF (INC.EQ.16) IVAL=64
CCC      IF (INC.EQ.8) IVAL=128
CCC      IF (INC.EQ.4) IVAL=196
CCC      IF (INTERP.EQ.1) IVAL=255
   30 BUF(S,LL) = int2byte(IVAL)

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create project_quad2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Project quadrant from input PIC to output BUF (halfword version).
C
      SUBROUTINE PROJECT_QUAD2(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      INTEGER*2 PIC(NSI,NLI),BUF(NSO,INCo)
      INTEGER*4 INTERP,INCLUDE,INC,INCo,LBEG,SBEG,NLI,NSI,NLO,NSO,ROW
      REAL*8 DL,DS

      COMMON/SCALING/SCALE_FACTOR
      REAL*8 SCALE_FACTOR

      REAL*8 X,Y,RLINE,RSAMP
      REAL*4 A(4),B(4)
      INTEGER*4 L,S,IL,IS,LEND,SEND,LL
      INTEGER*4 D1,D2,D3,D4,IVAL,IND

      LEND = LBEG + INC - 1
      SEND = SBEG + INC - 1
      IF (LEND.GT.NLO) LEND=NLO
      IF (SEND.GT.NSO) SEND=NSO

      DO 30 L=LBEG,LEND
      LL = L - ROW + 1

      DO 30 S=SBEG,SEND
      IVAL=0				!Clear DN
      IF (INTERP.EQ.1) THEN
        CALL PSUB(INCLUDE,L,S,rline,rsamp,ind)
        IF (IND.EQ.0) THEN
           IF (INCLUDE.EQ.0) GOTO 30
           RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
           RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
        ENDIF        
      ELSEIF (INTERP.EQ.2) THEN
        RSAMP = A(1) + A(2)*S + A(3)*L + A(4)*L*S
        RLINE = B(1) + B(2)*S + B(3)*L + B(4)*L*S
      ELSEIF (INTERP.EQ.3) THEN
        RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
        RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
      ELSE  
        GOTO 30
      ENDIF

      IS = RSAMP
      IL = RLINE
      IF (IL.LT.1.OR.IS.LT.1) GOTO 30           !Point off image
      IF (IL.GE.NLI.OR.IS.GE.NSI) GOTO 30

C     ....Compute pixel using bilinear interpolation
      D1 = PIC(IS,IL)		        !upper left pixel
      D2 = PIC(IS+1,IL)			!upper right pixel
      D3 = PIC(IS,IL+1)			!lower left pixel
      D4 = PIC(IS+1,IL+1)		!lower right pixel
      X = RSAMP - IS
      Y = RLINE - IL
      IVAL = D1 + (D2-D1)*X + (D3-D1)*Y + (D1-D2-D3+D4)*X*Y
   30 BUF(S,LL) = IVAL

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update quadrant address
C
      SUBROUTINE UPDATE_ADDRESS(address,inc,lbeg,sbeg)
      IMPLICIT NONE
      INTEGER*4 ADDRESS,INC,LBEG,SBEG

      INTEGER*4 QUAD

C     ....If we are in 4th quadrant, step up to next larger grid size
   40 QUAD = MOD(ADDRESS,10)
      IF (QUAD.NE.4) GOTO 50
      ADDRESS = ADDRESS/10
      LBEG = LBEG - INC
      INC = 2*INC 
      GOTO 40

   50 IF (ADDRESS.EQ.0) RETURN
      IF (QUAD.EQ.1) THEN          !Shift to next quadrant
         SBEG = SBEG + INC
      ELSEIF (QUAD.EQ.2) THEN
         LBEG = LBEG + INC
      ELSEIF (QUAD.EQ.3) THEN
         SBEG = SBEG - INC
      ENDIF
      ADDRESS = ADDRESS + 1
      RETURN
      END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create check_gridsize.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check the grid size (INC) by computing the approximate transformation (a,b)
C using the four corner points (x,y).  The type of interpolation chosen
C is as follows:
C  INTERP=0  Zero out this quadrant
C  INTERP=1  Use exact formula for entire quadrant
C  INTERP=2  Use approximate formula for entire quadrant  
C  INTERP=3  Use fixed offsed (DL,DS)
C
      SUBROUTINE CHECK_GRIDSIZE(INCLUDE,LBEG,SBEG,
     &		a,b,inc,address,interp)
      IMPLICIT NONE
      INTEGER*4 INCLUDE,LBEG,SBEG,INC,INTERP,ADDRESS
      REAL*4 A(4),B(4)

      INTEGER*4 IND,INDS(4),IERR,I,Xo,Yo,X(4),Y(4)
      REAL*8 U(4),V(4),Uo,Vo,Uop,Vop,ERR

      X(1) = SBEG
      Y(1) = LBEG

   20 X(2) = X(1) + INC - 1
      Y(2) = Y(1)
      X(3) = X(2)
      Y(3) = Y(2) + INC - 1
      X(4) = X(1)
      Y(4) = Y(3)
      IERR = 0

C     ....Project 4 corners using exact equations
      DO 10 I=1,4
      CALL PSUB(INCLUDE,Y(I),X(I),v(i),u(i),inds(i))
      IF (INDS(I).NE.1) THEN		!If point is off the target,
         IF (INC.GT.4) THEN		!reduce grid size if possible.
            INC = INC/2
            ADDRESS = 10*ADDRESS + 1
            GOTO 20
         ENDIF
         IERR = IERR + 1
      ENDIF
   10 CONTINUE

      IF (IERR.GT.0) GOTO 980

C     ....Compute approximate transformation (a,b) by fitting four corners
   25 CALL FINDT (X,Y,U,V,a,b,ind)

C     ....Calculate midpoint of square using exact and approx transform
      Xo = 0.5*(X(1) + X(2))
      Yo = 0.5*(Y(1) + Y(4))
      CALL PSUB(INCLUDE,Yo,Xo,vo,uo,ind)		!Exact

      Uop = A(1) + A(2)*Xo + A(3)*Yo + A(4)*Xo*Yo	!Approximate
      Vop = B(1) + B(2)*Xo + B(3)*Yo + B(4)*Xo*Yo
 
C     ....Difference between exact and approximate location of midpoint
      ERR = SQRT((Vo-Vop)**2 + (Uo-Uop)**2)
      IF (ERR.GT.0.5) GOTO 970
      INTERP = 2
      RETURN

C     ...Here if grid size is too large
  970 IF (INC.GT.4) THEN	!Reduce grid size if possible
         INC = INC/2
         ADDRESS = 10*ADDRESS + 1
         GOTO 20
      ELSE     
         INTERP = 1		!Otherwise, use exact equations
      ENDIF
      RETURN
C
C     ....Here if INC=4 and some points are off the planet
  980 IF (IERR.LT.4) THEN		!If at least one point is on the planet
         INTERP = 1			!use exact equation for entire quadrant
      ELSE				!If completely off target body and
         INTERP = 0			!background not included, skip square
         IF (INCLUDE.EQ.1) INTERP=3	!Else, use fixed offset
      ENDIF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create psub.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Convert from output (L,S) to input (RLINE,RSAMP)
C
      SUBROUTINE PSUB(INCLUDE,L,S,rline,rsamp,ind)
      IMPLICIT NONE
      INTEGER*4 INCLUDE,L,S,IND
      REAL*8 RLINE,RSAMP

      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      COMMON/PCONST/RA,RB,RC
      REAL*8 RA,RB,RC

      INTEGER*4 NPH,NPV		!dummy variables for CONVISOS
      REAL*8 A,B,C,D,R,SCL
      REAL*8 X,Y,Z,X0,Y0,Z0,UX0,UY0,UZ0,UX3,UY3,UZ3
      REAL*4 L4,S4

      IND = 0
C     ....Vector from ref S/C to point on planet in camera coordinates
      IF (ITYPE2.EQ.7) THEN
         L4 = L
         S4 = S
         CALL CONVISOS(PROJECT2,ICAM2,L4,S4,l4,s4,1,CONV2,NPH,NPV,ind)
         ux0 = S4 - OAS2
         uy0 = L4 - OAL2
      ELSE
         ux0 = S - OAS2
         uy0 = L - OAL2
      ENDIF
      uz0 = ZSCALE2

C     ....Convert vector to planet coordinate system (x3,y3,z3)
      ux3 = OM2(1,1)*ux0 + OM2(2,1)*uy0 + OM2(3,1)*uz0
      uy3 = OM2(1,2)*ux0 + OM2(2,2)*uy0 + OM2(3,2)*uz0
      uz3 = OM2(1,3)*ux0 + OM2(2,3)*uy0 + OM2(3,3)*uz0

C     ....Find where vector intersects planet
      x0 = ux3/ra
      y0 = uy3/rb
      z0 = uz3/rc

      a = x0*x0 + y0*y0 + z0*z0
      b = x0*rx2(1) + y0*rx2(2) + z0*rx2(3)
      c = rx2mag - 1.
      d = b*b - a*c
      if (d.lt.0.) return       !Point off the planet

      r = (-b-dsqrt(d))/a       !Choose smaller root for point in front
      x = r*ux3 + RS2(1)        !(x,y,z) is point on planet
      y = r*uy3 + RS2(2)
      z = r*uz3 + RS2(3)

      ux3 = x - RS(1)           !vector from S/C to surface point
      uy3 = y - RS(2)
      uz3 = z - RS(3)

C     ....Back-of-planet test
      if (include.eq.1) goto 50 !Skip test if sky is included
      d = ux3*(x/ra) + uy3*(y/rb) + uz3*(z/rc)
      if (d.gt.0) return

C     ....Rotate vector into camera coordinates
   50 ux0 = OM(1,1)*ux3 + OM(1,2)*uy3 + OM(1,3)*uz3
      uy0 = OM(2,1)*ux3 + OM(2,2)*uy3 + OM(2,3)*uz3
      uz0 = OM(3,1)*ux3 + OM(3,2)*uy3 + OM(3,3)*uz3
C     ....Scale vector into pixels
      SCL = ZSCALE/uz0
      IF (ITYPE.EQ.7) THEN
         L4 = SCL*uy0 + OAL
         S4 = SCL*ux0 + OAS
         CALL CONVISOS(PROJECT,ICAM,l4,s4,L4,S4,0,CONV,NPH,NPV,ind)
         RLINE = L4
         RSAMP = S4
      ELSE
         RLINE = SCL*uy0 + OAL
         RSAMP = SCL*ux0 + OAS
      ENDIF
      IND = 1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create old_ptp_sub.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C------------------------------------------------------------------	  
C   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE:   NOTE: 
C
C    None of the code below is called anymore, but it preserved
C    in case someone needs to run PTP on a very large image that
C    cannot be read into memory. The routine PROJECT is supposed
C    to read in 32 x 32 blocks into IBUF, and process only parts
C    of the image at a time. The current implementation just
C    reads in the whole file and is no better than the ICPROJECT
C    code (in fact, it is slower). These routines will need to
C    be hacked for 32 x 32 block buffering to work. See LGEOM for
C    a guide on how it is done there.
C
C     -- Niles Ritter.
C------------------------------------------------------------------	  

	  
C------------------------------------------------------------------	  
C ROUTINE PROJECT - part of PTP program
C  Project input image onto output image
C------------------------------------------------------------------	  

      SUBROUTINE PROJECT(PIC,NPIX2,IBUF,NBI2,BUF,NSO2,
     &     NLI,NSI,NLO,NLW,NSW,NSB,NR,NC,NBLK,DL,DS,INCLUDE)
      IMPLICIT NONE
      
      INTEGER I, I0, I1, I2, I3, I4, ICAMERA, ICAMERA2, IFDSC
      INTEGER IFDSC2, II, IIP, IL, IMOD, INCLUDE, IS, IUNIT, IVAL
      INTEGER IX, J, JMOD, L, NBI, NBI2, NBLK, NC, NLI, NLO, NLW
      INTEGER NPIX, NPIX2, NR, NSB, NSI, NSO, NSO2, NSW
      INTEGER*2 PIC(NSI,NLW),IBUF(NSB,NBLK),BUF(NSO2/2)
      INTEGER*4 OUNIT
      INTEGER S

      CHARACTER*80 MSG      
      
      REAL*8 A, ANGLN, ANGLN2, B, D, DEGRAD, DL, DS, DSQRT, EPSLN
      REAL*8 EPSLN2, EPSLN22, FL, FL2		, OAL, OAL2, OAS, OAS2
      REAL*8 OM, OM2, PI, R, RADDEG, RANGE, RANGE2, RE, RLINE, RP
      REAL*8 RS, RS2, RSAMP, SCALE, SCALE2, SCLAT, SCLAT2, SCLINE
      REAL*8 SCLINE2, SCLON, SCLON2, SCSAMP, SCSAMP2, STATUS, UX0
      REAL*8 UX3, UY0, UY3, UZ0, UZ3, X, Y, Z, ZSCALE, ZSCALE2

      COMMON/CIO/OUNIT,IUNIT

      COMMON/CONST/PI,DEGRAD,RADDEG,RE,RP,EPSLN

      COMMON/CMAP/IFDSC,ICAMERA,FL,OAL,OAS,SCALE,ZSCALE
      COMMON/CMAP/SCLINE,SCSAMP,SCLAT,SCLON,ANGLN,RANGE
      COMMON/CMAP/OM(3,3),RS(3),EPSLN2      
      COMMON/CMAP/IFDSC2,ICAMERA2,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/CMAP/SCLINE2,SCSAMP2,SCLAT2,SCLON2,ANGLN2,RANGE2
      COMMON/CMAP/OM2(3,3),RS2(3),EPSLN22

 1000 FORMAT(' ***Increase virtual memory size by',I4,'K')


      call init_spice
      npix=npix2/2
      nbi=nbi2/2
      nso=nso2/2

      IF (NBI .LT. (NBLK*NSB)) THEN		!IF not enough core...
	    CALL xvmessage(' ***Virtual memory too small',' ')
	    WRITE(MSG,1000) (NBLK*NSB-NBI)/1024+1
        CALL xvmessage(MSG,' ')
	    CALL ABEND
      ENDIF
C           Read in input image and segment into IBUF
      CALL PICSEG(IUNIT,PIC,IBUF,NLI,NSI,NBLK,NSB,NLW,NSW)


      DO L=1,NLO			!Image line loop

          DO  S=1,NSO			!Pixel loop
              IVAL = 0				!clear output dn
C         Vector from S/C to point on planet in camera coordinates
          ux0 = S - OAS2
          uy0 = L - OAL2
          uz0 = ZSCALE2
C         Convert vector to planet coordinate system (x3,y3,z3)
               ux3 = OM2(1,1)*ux0 + OM2(2,1)*uy0 + OM2(3,1)*uz0
               uy3 = OM2(1,2)*ux0 + OM2(2,2)*uy0 + OM2(3,2)*uz0
               uz3 = OM2(1,3)*ux0 + OM2(2,3)*uy0 + OM2(3,3)*uz0
C          Find where vector intersects planet
               A = ux3**2     + uy3**2     + EPSLN*uz3**2
               B = ux3*RS2(1) + uy3*RS2(2) + EPSLN*uz3*RS2(3)
               D = B*B - A*EPSLN22

               IF (D.LT.0.) THEN		!Point off the planet
                   IF (INCLUDE.EQ.0) CONTINUE
                   RLINE = L + DL	!If sky is to be included
                   RSAMP = S + DS        !just use constant offset
                   GOTO 60
               ENDIF

               r = (-B-DSQRT(D))/A	!Choose smaller root for point in front

               X = r*ux3 + RS2(1)		!(X,Y,Z) is point on planet
               Y = r*uy3 + RS2(2)
               Z = r*uz3 + RS2(3)

               ux3 = X - RS(1)		!vector from S/C to point on planet
               uy3 = Y - RS(2)
               uz3 = Z - RS(3)
               IF (INCLUDE.EQ.1) GOTO 50	!Skip test if sky is included
C          Back-of-planet test
               A = ux3**2    + uy3**2    + EPSLN*uz3**2
               B = ux3*RS(1) + uy3*RS(2) + EPSLN*uz3*RS(3)
               D = B*B - A*EPSLN2
               IF(D.GT.0.) THEN
                   r = (-B-DSQRT(D))/A
                   IF (r.LT.0.99999D0) CONTINUE		!Point behind the planet
               ENDIF
C          Rotate vector into camera coordinates
   50          ux0 = OM(1,1)*ux3 + OM(1,2)*uy3 + OM(1,3)*uz3
               uy0 = OM(2,1)*ux3 + OM(2,2)*uy3 + OM(2,3)*uz3
               uz0 = OM(3,1)*ux3 + OM(3,2)*uy3 + OM(3,3)*uz3
C          Scale vector into pixels
               SCALE = ZSCALE/uz0
               RLINE = SCALE*uy0 + OAL
               RSAMP = SCALE*ux0 + OAS

   60          IS = RSAMP
               IL = RLINE
               IF (IL.LT.1.OR.IS.LT.1) CONTINUE		!Point off image
               IF (IL.GE.NLI.OR.IS.GE.NSI) CONTINUE

C           Determine which block contains (IS,IL)
               I = (IS-1)/32
               J = (IL-1)/32
               IX = NC*J + I + 1
               IMOD = IS - 32*I
               JMOD = IL - 32*J
               I0 = 32*(JMOD-1)
               II = I0 + IMOD
               i1 = IBUF(II,IX)			!upper left pixel

               IF (IMOD .EQ. 32) THEN		!right margin of block
                    IIP = I0 + 1

                    i2 = IBUF(IIP,IX+1)		!upper right pixel

                    IF (JMOD .EQ. 32) THEN       !lower right corner of block
                         IX = IX + NC
                         i3 = IBUF(32,IX)	!lower left pixel
                         i4 = IBUF(1,IX+1)	!lower right pixel
                     ELSE
                         i3 = IBUF(II+32,IX)
                         i4 = IBUF(IIP+32,IX+1)
                     ENDIF

               ELSE				!not on right margin
                     i2 = IBUF(II+1,IX)
                     IF (JMOD .EQ. 32) THEN	!lower margin
                         IX = IX + NC
                         i3 = IBUF(IMOD,IX)
                         i4 = IBUF(IMOD+1,IX)
                     ELSE			!interior pixel
                         II = II + 32
                         i3 = IBUF(II,IX)
                         i4 = IBUF(II+1,IX)
                     ENDIF
               ENDIF

               X = RSAMP - IS
               Y = RLINE - IL
               IVAL = I1 + (I2-I1)*X + (I3-I1)*Y + (I1-I2-I3+I4)*X*Y
               BUF(S) = IVAL
          ENDDO  !Pixel Loop

          CALL xvwrit(OUNIT,BUF,status,' ')	!Write completed line to output image
          IF (status.ne.1) call xvsignal(ounit,status,1) 
      ENDDO   !Line Loop
      RETURN
      END
C------------------------------------------------------------------	  
C Segments input image into 32x32 areas and store in PIC
C
      SUBROUTINE PICSEG(IUNIT,PIC,IBUF,NLI,NSI,NBLK,NSB,NLW,NSW)
      integer*2 PIC(NSI,NLW),IBUF(NSB,NBLK)

      LINE = 1
      IBLK = 1

      DO 100 L=1,NLI,NLW

      DO LL=1,NLW			!Read in next NLW lines...
          IF (LINE .GT. NLI) THEN
               CALL MVE(2,NSI,0,PIC(1,LL),0,1)  !all zeroes
          ELSE
               CALL XVREAD(IUNIT,PIC(1,LL),status,' ')
	           IF (status.ne.1) call xvsignal(iunit,status,1) 
          ENDIF
          LINE = LINE + 1
      ENDDO

      DO I=1,NSI,NSW			!and carve them up into
          J = 1				!NSWxNLW areas

          DO LL=1,NLW
               CALL MVE(2,nsw,PIC(I,LL),IBUF(J,IBLK),1,1)
               J = J + NSW
          ENDDO
          IBLK = IBLK + 1
      ENDDO

  100 CONTINUE

      CALL XVCLOSE(IUNIT,status,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ptp.pdf
process help=*
PARM INP     TYPE=STRING  COUNT=1	
PARM OUT     TYPE=STRING  COUNT=1
PARM SIZE    TYPE=INTEGER COUNT=4			DEFAULT=(1,1,0,0)
PARM SL	     TYPE=INTEGER				DEFAULT=1
PARM SS	     TYPE=INTEGER				DEFAULT=1
PARM REF     TYPE=STRING  COUNT=(0:1)                   DEFAULT=--
PARM RSCET   TYPE=INTEGER COUNT=(0,6)                   DEFAULT=--
PARM RCAM    TYPE=INTEGER COUNT=(0:1)			DEFAULT=--
PARM RMISSION   KEYWORD   COUNT=0:1              DEFAULT=-- +
  VALID=(VGR-1,VGR-2,GLL,CASSI,VIKOR)
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM INCLUDE TYPE=KEYWORD COUNT=(0:1) VALID=INCLUDE     DEFAULT=--
PARM EXACT   TYPE=KEYWORD COUNT=(0:1) VALID=EXACT       DEFAULT=--
PARM TARGET  TYPE=(STRING,12) COUNT=0:1                 DEFAULT=--
PARM RADII   TYPE=REAL    COUNT=(0,3)                   DEFAULT=--
PARM PC      TYPE=REAL 	  COUNT=(0:2)			DEFAULT=--
PARM RPC     TYPE=REAL    COUNT=(0:2)			DEFAULT=--
PARM ROT     TYPE=REAL 	  COUNT=(0:1)			DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1                DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1                DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1              DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1              DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                DEFAULT=NONE
END-PROC
.TITLE
VICAR Program PTP:  Perspective-To-Perspective projection
.HELP
PURPOSE

PTP is a VICAR applications program which projects an image of a planet or
satellite so that the resulting image is that of the planet as seen from a
different perspective.  PTP's primary application is to register color triplets
in cases where the time between frames is large enough that noticeable changes
in perspective occur as a result of planet rotation and spacecraft motion.

.page
EXECUTION:

     PTP  INP=PIC  OUT=OPIC  REF=RPIC
where 
   PIC  is the input image to be projected (byte or halfword), and
   OPIC is the output image
   REF  is the reference image,

PIC may be from any mission for which navigation data is available via the
MIPS SPICE server.  If PIC is from Voyager or an earlier mission, it must be a
geometrically corrected image.  If PIC is from Galileo or a later mission, it
must be a raw (uncorrected) image.  OPIC will be output in the same data format
as PIC.

In cases where the reference image is not available, it may be replaced by
supplying the program with enough information to identify it to the SPICE
server:

   PTP  INP=PIC  OUT=OPIC  RSCET=(yr,day,month,hr,min,sec,msec) +
          RCAM=n  TARGET=aaaaaaa
where
   RSCET is the SpaceCraft Event Time of the reference image,
   RCAM is the camera serial number of the reference image, and
   TARGET is the target name.

Note that the input and reference images may be from different missions.  I.e.,
it is possible to project a Cassini image to the same scale and orientation as
a Voyager image (for comparison).

.page
OPERATION

Before running PTP, the input and reference images should be navigated to
correct the camera pointing (see programs NAV, NAV2, FARENC, MANMATCH,
and AUTOMATCH).

PTP reads the VICAR labels of the input and reference images (see INP and REF
parameters) to identify the project, camera ID, target ID, and Spacecraft-Event
Time.  Note that the target is not available in the Voyager label and must be
supplied via the TARGET parameter.

PTP retrieves the navigation data for the input and reference images from the
MIPS SPICE server using SCET, camera, and target to access the appropriate data.
If the reference image is not available, it may be replaced by identifying it
via the RSCET, RCAM, and TARGET parameters.

The camera pointing retrieved from SPICE may be overridden by supplying the
planet center via the PC and RPC parameters.

PTP uses a triaxial ellipsoid model of the planet, defined by three radii
(equatorial long axis, equatorial short axis, and polar).  The values are
obtained via a call to subroutine PBDATA.  This may be overridden by using the
RADII parameter.

The ROT parameter may be used to change the rotation rate or the planet.

.page
GENERATING THE OUTPUT PROJECTION:

PTP projects the image by performing a geometric transformation similar to that
in MAP3.  If the keyword EXACT is specified, the projection is computed exactly
at every pixel.  Otherwise, the projection is calculated on a grid of tiepoints,
and bilinear interpolation is used to compute the projection at intermediate
points.

The initial grid spacing is 32 x 32 pixels.  In areas of the image where this
interpolation generates errors larger than 1/2 pixel (e.g. near the limb), the
grid spacing is reduced in successive steps (16x16, 8x8, 4x4) until a 1/2 pixel
accuracy is achieved.

In general, an output pixel will not project onto an input pixel, but will
project to a point between four input pixels.  The output sample value is
computed via bilinear interpolation over the these four pixels:

        DN = DN1 + (DN2-DN1)*ds + (DN1-DN2-DN3+DN4)*dl

If the 'INCLUDE keyword is specified, all output pixels not on the planet are
generated by offsetting the sky background of the input image by a constant
(the difference between the planet centers).  No perspective geometry is used,
and the planet ring system is treated as though no rotation has occured.
Also, there will be areas of the planet visible from the perspective of the
output image but not visible in the input image (e.g. the area may have rotated
behind the planet).  These areas are generated by using the corresponding
points in front of the planet.  Note that this is a "cludgy" solution.
Beware of odd side effects if the rotation is large.

If INCLUDE is not specified, the sky background is deleted (output
as 0 dn's).

.page
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels (only CKNAME is applicable for Voyager):

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

CDATE specifies the date and time the camera pointing was created.
REQNUM identifies the request number associated with the camera pointing.
PURPOSE identifies the purpose for creating the camera pointing.
PROGRAM identifies the program which created the camera pointing.
SPKID identifies the SP-kernel used to create the camera pointing.
USERID identifies the user who created the camera pointing.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

A complete list of CK and SPK IDs are located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.page
EXAMPLES

   (1)  PTP INP=1550856.IMG  OUT=OUT.IMG REF=1550854.IMG TARGET=GANYMEDE

        The input image (1550856.IMG) is projected to the same perspective as
        the reference image (1550854.IMG).  The output image is OUT.IMG.
        Since these are Voyager images, the target must be specified.

   (2)  PTP INP=1550856.IMG OUT=OUT.IMG RSCET=(1996,223,14,59,58,123) +
            RCAM=2  TARGET=GANYMEDE

        In lieu of the REF parameter, the reference image is identified by
        time (RSCET) and camera ID.

   (3)  ptp INP=n1354897340.1  OUT=cassini.img  REF=f1634146.img TARGET=JUPITER

	The input is a Cassini image of Jupiter while the reference image is
	a Voyager image taken at approximately the same longitude.  Since the
	reference image is in object space, the output image will be in object
	space, even though the input image is in image space.

.page
PROGRAM HISTORY

ORIGINAL PROGRAMMER: Gary Yagi, January 4, 1986
CURRENT COGNIZANT PROGRAMMER: Gary Yagi
REVISION:
 29 Dec 11  LWK  Added call to init_spice, which is required for the new PBDATA.
 27 Feb 03  GMY  Upgraded to support Cassini.  Updated to reference SPICE
		 instead of SEDR for Voyager.  Delete RFRAME parameter.
		 Fix bug introduced in Y2000 date checking by initializing
		 ERT in label buffer.  Add RMISSION parameter.  Fix time
		 calculation in GET_TIME.
		 Use tri-axial ellipsoid model (replace RADIUS, RE,RP paramters
		 by RADII parameter.
 29 Jul 02  LWK  AR-AR-107379.  Cleaned up help text; also replaced SEARCV3
		 call (obsolete) with MP and SEARC_DISTOR.
 18 May 98  RRP  AR-9064. The icam2 check should be for camera number 1 and
                 2 and not 0 and 1.
 08 Aug 97  RRP  Added support for input image being Summastion Mode and
                 Reference being Full frame or vise-versa. (FR 89270)
 08 Oct 96  GMY  Implemented Flagstaff tiepoint algorithm (dynamically adjust-
                 able grid spacing).  Added SSI geometric correction (FR 89818)
 27 Aug 96  SMC  ...Added Support for GLL Summation Mode processing (DFR)
 10 Jul 96  SMC  ...Added old parameters RSCET, RFRAME, RCAM back in, use
                    TARGET to replace PLANET, CKNAME to replace SEDRSRC
 10 Jun 96  SMC  ...Consolidated Voyager processing method to be same as that
                    of the Gailileo's, so both will call getspice2.  At this
                    stage, getspice2 will call getspice if it detects Voyager
                    images.  Took out parameters RSCET, RFRAME, RCAM, PLANET,
                    SEDRSRC
 24 May 96  SMC  ...Added REMOTE Spice function for Gailileo image processing
 23 Aug 95  NDR  ...Ported to Unix
 1  Jun 90  JJL  ...Converted to GLL
 1  Jan 91  JJL  ...Halfword arithmetic throughout.
 20 Aug 89  GMY  ...Fix bug in ROT parameter
 17 Aug 89  GMY  ...Add check to insure input is byte image
 20 Jan 86  GMY  ...Add PLANET & ROT parameters
 16 May 88  GMY  ...Incorporate 'SEDRSRC keyword
 26 Jul 89  GMY  ...Make PC and RPC parameters optional
 04 Jan 86  GMY  ...Original Program.

.LEVEL1

.VARI INP
Input image (byte or halfword)

.VARI OUT
Output image (byte or halfword, 
depending on INP)

.VARI SIZE
Vicar size field for the output 
image (optional)
(SL,SS,NLO,NSO)

.VARI SL
INTEGER - Input starting
 line (ignored)

.VARI SS
INTEGER - Input starting
 sample (ignored)

.VARI REF
STRING - File Name
Reference image file containing 
the desired projection 
attributes

.VARI RSCET
6 INTEGERS
Reference SCET information 
required for GLL project if REF 
is not specified

.VARI RCAM
INTEGER - Camera Number
Reference camera, required if 
REF is not specified

.VARI RMISSION
OPTIONAL KEYWORD
Spacecraft ID of reference image

.VARI RADII
REAL - three radii (km) of target
 body (optional)

.VARI ROT
REAL - Target body rotation
 rate (deg/day) (optional)

.VARI PC
REAL - Planet-center of input
 image (line,sample) (optional)

.VARI RPC
REAL - Planet center of 
reference image (line,sample) 
(optional)

.VARI INCLUDE
KEYWORD - Include sky
 background (optional)

.VARI EXACT 
KEYWORD - compute the projection
using the exact equations at 
each pixel (rather than via 
interpolation).

.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE, no effect on 
VGR data as of (6/12/96))
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera 
pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera 
pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera 
pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera 
pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created 
camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing 
was created

.LEVEL2

.VARI INP
STRING - Input image.
INP must be in byte or halfword format.

If INP is from Voyager or any earlier mission it must be a geometrically
corrected image.

If INP is from Galileo or any later mission, it must be a raw (uncorrected)
image.

.VARI OUT
STRING - Output image
The output image format will be in the same data format as the input image
(byte or halfword).

.VARI REF
STRING - Reference image
The input image (INP) is projected to the same perspective as the reference
image. 

PTP extracts SCET, camera serial number, and target-id from the VICAR label
and used these data to retrieve the navigation data from the SPICE server.

If the reference image is not available, it may be replaced by specifying the
RSCET, RCAM, and TARGET parameters.

.VARI RSCET
6 integers
(SCETYear, SCETDay, SCETHour, SCETMin, SCETSec, SCETMSec)
SpaceCraft Event Time of reference frame.

.VARI RCAM
OPTIONAL INTEGER
Camera serial number for reference image.  If defaulted, the camera ID is
obtained from the VICAR label of the reference image.

The camera serial number is used to retrieve the focal length, line and sample
of the optical-axis intercept point, and the picture scale from built-in
tables.  The current values in these tables are:

		      CAMERA	 FOCAL	  LAXIS  SAXIS  PSCALE (pixels/mm)
        CASSI NAC       1       2000.00    512    512   83.333333
	CASSI WAC       2        200.736    "      "      "
        CASSI NAC 2x2  21       2000.00    256    256   41.666665
	CASSI WAC 2x2  22        200.736    "      "      "
        CASSI NAC 4x4  41       2000.00    128    128   20.833333
	CASSI WAC 4x4  42        200.736    "      "      "

        GLL             1       1501.039   400    400   65.6167979 
        GLL 2x2 sum     2	1501.039   200    200   32.8083990

	VGR-2 WA        4	 200.770   500    500   84.821428
	VGR-2 NA        5	1503.49     "      "      "
	VGR-1 WA        6	 200.465    "      "      "
	VGR-1 NA        7	1500.19     "      "      "

        VIKOR 1A        7	474.610    575    625   85.0
        VIKOR 1B        4	474.398     "      "      "
        VIKOR 2A        8	474.101     "      "      "
        VIKOR 2B        6	474.448     "      "      "

        MAR10 A         1      1495.66     400    475   74.78
        MAR10 B         1      1503.69     400    475   74.78

        MAR-9           1	 52.267    400    475   75.0     
        MAR-9		2	500.636     "      "      "

Note: These tables are obtained via a call to VICAR subroutine GETCAMCON.  For
active missions, these values may be updated as they are more accurately
determined.

.VARI RMISSION
OPTIONAL KEYWORD
Spacecraft ID of reference image
     e.g.  RMISSION=CASSI
VALID=(VGR-1,VGR-2,GLL,CASSI,VIKOR)
If defaulted, RMISSION is assumed to be the same as the input image.

.VARI RADII
OPTIONAL REAL

PTP uses a triaxial ellipsoid model of the planet, defined by three radii
(equatorial long axis, equatorial short axis, and polar) in km:

    EX:  RADII=(1811.3, 1805.2, 1793.1)

If defaulted, the values are obtained via a call to subroutine PBDATA.

.VARI RP
OPTIONAL REAL
polar radius of oblate spheroid target body (km)

.VARI RE
OPTIONAL REAL
equatorial radius of oblate spheroid target body (km)

.VARI ROT
OPTIONAL REAL
Target body rotation rate (deg/day)
E.g: ROT=870.536

.VARI PC
OPTIONAL REAL
Planet center of input image specified as (line,sample).
 Example: PC=(123.23,432.34)

.VARI RPC
OPTIONAL REAL
Planet center of reference image specified as (line,sample).
 Example: PC=(123.23,432.34)

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

The processing method is fixed by GETSPICE2 to REMOTE for VGR data as of
6/12/96.  This is only a temporary.

.VARI INCLUDE
If the 'INCLUDE keyword is specified, all output pixels not on the planet are
generated by offsetting the sky background of the input image by a constant
(the difference between the planet centers).  No perspective geometry is used,
and the planet ring system is treated as though no rotation has occured.
Also, there will be areas of the planet visible from the perspective of the
output image but not visible in the input image (e.g. the area may have rotated
behind the planet).  These areas are generated by using the corresponding
points in front of the planet.  Note that this is a "cludgy" solution.
Beware of odd side effects if the rotation is large.

If INCLUDE is not specified, the sky background is deleted (output
as 0 dn's).  This is the default.

.VARI EXACT
By default, the projection is calculated for a grid of tiepoints (the grid 
spacing is 32 x 32 pixels).  All output pixels within the square area 
defined by any four neighboring grid points are projected via interpolation.
In areas of the image where this interpolation generates errors larger than 
1/2 pixel (e.g. the limb), the grid spacing is reduced in successive steps 
(16x16, 8x8, 4x4) until a 1/2 pixel accuracy is achieved.

If the keyword 'EXACT is specified, the projection is computed using the exact
equations at each pixel (rather than via interpolation).

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.

.END
$ Return
$!#############################################################################
$Imake_File:
$ create ptp.imake
/*  Imake file for PTP  */
#define PROGRAM ptp
#define R2LIB
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define MODULE_LIST ptp.f open_images.f get_inp_nav.f get_ref_nav.f \
		fix_pointing.f icproject.f project_quad.f project_quad2.f \
		check_gridsize.f psub.f old_ptp_sub.f
#define FTNINC_LIST fortport
#define LIB_NETWORK
#define LIB_P1SUB
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
#define LIB_SPICE
#define LIB_MATH77
$ Return
$!#############################################################################
$Test_File:
$ create tstptp.pdf
Procedure	
  Refgbl $Echo
  RefGbl $SysChar
Body

  Local path  String
  Local mgll  String
  Local mvgr  String
  Local mcas  String

  IF ($SysChar(1)="VAX_VMS")
    LET path = "wms_test_work:[testdata.gll]"
    LET mgll = "wms_test_work:[testdata.mipl.gll]"
    LET mvgr = "wms_test_work:[testdata.mipl.vgr]"
    LET mcas = "wms_test_work:[testdata.cassini.cas$i$ss]"
  ELSE
    LET path = "/project/test_work/testdata/gll/"
    LET mgll = "/project/test_work/testdata/mipl/gll/"
    LET mvgr = "/project/test_work/testdata/mipl/vgr/"
    LET mcas = "/project/test_work/testdata/cassini/casIss/"
  END-IF

let $echo="no"
write "=================================================================="
write "========================== VGR Test =============================="
write "=================================================================="
let $echo="yes"

!....Project RED image 2 ways: 
!....(1) specifying REF
ptp INP=&"mvgr"ptp_vgr_red.img OUT=vgr_red.img TARGET=JUPITER +
    REF=&"mvgr"ptp_vgr_blu.img
!....(2) specifying RSCET and RCAM
ptp INP=&"mvgr"ptp_vgr_red.img OUT=vgr_grn.img TARGET=JUPITER +
    RSCET=(1979,62,21,57,22,910) rcam=6
difpic (vgr_red.img vgr_grn.img)	!difference should be 0

!....Project GREEN image
ptp INP=&"mvgr"ptp_vgr_grn.img OUT=vgr_grn.img TARGET=JUPITER +
    REF=&"mvgr"ptp_vgr_blu.img

!....Print a portion of the limb
list vgr_red.img (365,278,20,10)
list vgr_grn.img (365,278,20,10)

!....Copy BLUE image (for displaying only)
copy &"mvgr"ptp_vgr_blu.img vgr_blu.img

!....Use XVD to display vgr_red.img, vgr_grn.img, vgr_blu.img
!....Note: registration is not all that hot

Let $Echo="No"
write "=================================================================="
write "======================= Galileo test ============================="
write "=================================================================="
let $echo="yes"

!....Project red image 2 ways:
! (1): specify reference image....
ptp INP=&"mgll"ptp_gll_red.img OUT=red.img +
	REF=&"mgll"ptp_gll_grn.img
! (2) specifying RSCET, RCAM and TARGET....
ptp INP=&"mgll"ptp_gll_red.img OUT=grn.img +
	RSCET=(1992,346,6,9,57,358) RCAM=1 TARGET=EARTH
difpic (red.img,grn.img)	!Difference should be 0

!....Project blue image
ptp INP=&"mgll"ptp_gll_blu.img OUT=blu.img +
    REF=&"mgll"ptp_gll_grn.img 'REMOTE

list red.img (203,265,20,10)
list grn.img (203,265,20,10)

!....Copy green image (for display only)
copy &"mgll"ptp_gll_grn.img grn.img
!....Use XVD to display red.img, grn.img, blu.img

WRITE "----------------------------------------------------------------------"
WRITE "TEST 2: Input is summation and reference is full frame."
WRITE ""

ptp INP=&"path"s0401863200.1 OUT=sf.img REF=&"path"s0401863178.1 'INCLUDE
list sf.img (267,185,10,10)

let $echo="no"
write "=================================================================="
write "========================= Cassini Test ==========================="
write "=================================================================="
let $echo="yes"

!Note that input image is from Cassini, but the reference image is from VGR.
!Also note that the images differ by a rotation of approximately 90 degrees.
!These factors make this an extreme test of the programs capabilities.

ptp INP=&"mcas"n1354897340.1 OUT=cas_red.img TARGET=JUPITER +
    REF=&"mvgr"ptp_vgr_blu.img
list cas_red.img (190,468,10,10)
End-Proc
$!-----------------------------------------------------------------------------
$ create tstptp.log_solos
tstptp
==================================================================
========================== VGR Test ==============================
==================================================================
ptp INP=/project/test_work/testdata/mipl/vgr/ptp_vgr_red.img OU+
T=vgr_red.img TARGET=JUPITER      REF=/project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
Reference image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
 OS target center=(    707.78,    542.33)
 OS target center for ref image=(    601.77,    709.68)
 ***PTP task completed
ptp INP=/project/test_work/testdata/mipl/vgr/ptp_vgr_red.img OU+
T=vgr_grn.img TARGET=JUPITER      RSCET=(1979,62,21,57,22,910) rcam=6
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
No reference image provided
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
 OS target center=(    707.78,    542.33)
 OS target center for ref image=(    601.77,    709.68)
 ***PTP task completed
difpic (vgr_red.img vgr_grn.img)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
ptp INP=/project/test_work/testdata/mipl/vgr/ptp_vgr_grn.img OU+
T=vgr_grn.img TARGET=JUPITER      REF=/project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
Reference image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
 OS target center=(    598.31,    637.46)
 OS target center for ref image=(    601.77,    709.68)
 ***PTP task completed
list vgr_red.img (365,278,20,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:jry       Date_Time:Fri Jun  7 12:19:47 1996
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:20:48 2011
     Samp     278   279   280   281   282   283   284   285   286   287
   Line

    369         0     0     0     0     0     0     0     0     0   622
    370         0     0     0     0     0     0     0     0     0   812
    371         0     0     0     0     0     0     0     0   684   929
    372         0     0     0     0     0     0     0     0   872  1009
    373         0     0     0     0     0     0     0   739   993  1063
    374         0     0     0     0     0     0   585   893  1073  1108
    375         0     0     0     0     0     0   802  1028  1120  1151
    376         0     0     0     0     0   667   954  1080  1159  1199
    377         0     0     0     0     0   843  1023  1112  1191  1242
    378         0     0     0     0   723   969  1082  1154  1219  1253
    379         0     0     0     0   884  1044  1134  1184  1217  1262
    380         0     0     0   772   984  1093  1167  1196  1229  1290
    381         0     0     0   927  1056  1131  1183  1212  1275  1329
    382         0     0   839  1039  1116  1167  1205  1243  1299  1352
    383         0   639   965  1097  1159  1193  1218  1276  1319  1368
    384         0   844  1053  1144  1190  1217  1247  1303  1356  1406
list vgr_grn.img (365,278,20,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:jry       Date_Time:Fri Jun  7 12:21:31 1996
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:20:54 2011
     Samp     278   279   280   281   282   283   284   285   286   287
   Line

    369         0     0     0     0     0     0     0     0     0  1685
    370         0     0     0     0     0     0     0     0     0  1719
    371         0     0     0     0     0     0     0     0  1654  1715
    372         0     0     0     0     0     0     0     0  1697  1795
    373         0     0     0     0     0     0     0  1671  1752  1824
    374         0     0     0     0     0     0  1701  1816  1844  1859
    375         0     0     0     0     0     0  1795  1882  1883  1900
    376         0     0     0     0     0  1727  1783  1847  1903  1974
    377         0     0     0     0     0  1779  1812  1853  1924  2008
    378         0     0     0     0  1728  1779  1823  1879  1969  2074
    379         0     0     0     0  1779  1816  1856  1922  2009  2092
    380         0     0     0  1730  1781  1834  1888  1955  2025  2089
    381         0     0     0  1748  1806  1864  1924  1984  2046  2112
    382         0     0  1735  1770  1841  1918  1979  2021  2087  2147
    383         0  1718  1759  1822  1893  1968  2035  2094  2136  2168
    384         0  1733  1801  1880  1931  1970  2025  2117  2164  2190
copy /project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img vgr_blu.img
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Let $Echo="No"
==================================================================
======================= Galileo test =============================
==================================================================
ptp INP=/project/test_work/testdata/mipl/gll+
/ptp_gll_red.img OUT=red.img  	REF=/project/test_work/testdata/mipl/gll/ptp_gll_grn.img
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is GLL
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
Reference image spacecraft is GLL
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
 OS target center=(    382.56,    475.63)
 OS target center for ref image=(    384.22,    518.68)
 ***PTP task completed
ptp INP=/project/test_work/testdata/mipl/gll+
/ptp_gll_red.img OUT=grn.img  	RSCET=(1992,346,6,9,57,358) RCAM=1 TARGET=EARTH
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is GLL
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
No reference image provided
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
 OS target center=(    382.56,    475.63)
 OS target center for ref image=(    384.22,    518.68)
 ***PTP task completed
difpic (red.img,grn.img)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
ptp INP=/project/test_work/testdata/mipl/gll+
/ptp_gll_blu.img OUT=blu.img      REF=/project/test_work/testdata/mipl/gll/ptp_gll_grn.img 'REMOTE
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is GLL
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
Reference image spacecraft is GLL
CKNAME=NAV   SPKID=N083  PROGRAM=  1919  
 OS target center=(    381.74,    501.73)
 OS target center for ref image=(    384.22,    518.68)
 ***PTP task completed
list red.img (203,265,20,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CATLABEL  User:HBM320    Date_Time:Tue Aug  3 16:13:51 1993
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:20:57 2011
     Samp   265     267     269     271     273
   Line
    203       0   0   0   0   0   0  12  13  14  18
    204       0   0   0   0   0  12  14  14  19  22
    205       0   0   0   0  11  13  14  16  21  37
    206       0   0   0  11  12  14  15  19  26  66
    207       0   0   0  12  13  14  19  23  50 105
    208       0   0  10  11  14  18  18  34  81 136
    209       0  13  13  14  16  19  26  60 115 159
    210       0  12  13  13  18  21  44  96 146 183
    211      12  13  16  18  18  32  80 128 171 192
    212      12  16  13  18  25  60 116 160 184 208
    213      14  17  17  20  42 100 150 181 202 233
    214      13  17  20  33  78 141 171 192 220 246
    215      16  19  25  58 123 170 191 213 238 252
    216      16  23  42  99 157 190 209 235 251 255
    217      19  30  74 143 183 202 226 249 255 255
    218      25  55 121 176 196 212 242 253 255 255
    219      38  90 158 193 203 222 241 252 255 255
    220      68 135 178 195 214 235 251 255 255 255
    221     109 165 186 201 232 251 255 255 255 255
    222     153 188 204 221 243 253 255 255 255 255
list grn.img (203,265,20,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CATLABEL  User:HBM320    Date_Time:Tue Aug  3 16:13:51 1993
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:21:01 2011
     Samp   265     267     269     271     273
   Line
    203       0   0   0   0   0   0  12  13  14  18
    204       0   0   0   0   0  12  14  14  19  22
    205       0   0   0   0  11  13  14  16  21  37
    206       0   0   0  11  12  14  15  19  26  66
    207       0   0   0  12  13  14  19  23  50 105
    208       0   0  10  11  14  18  18  34  81 136
    209       0  13  13  14  16  19  26  60 115 159
    210       0  12  13  13  18  21  44  96 146 183
    211      12  13  16  18  18  32  80 128 171 192
    212      12  16  13  18  25  60 116 160 184 208
    213      14  17  17  20  42 100 150 181 202 233
    214      13  17  20  33  78 141 171 192 220 246
    215      16  19  25  58 123 170 191 213 238 252
    216      16  23  42  99 157 190 209 235 251 255
    217      19  30  74 143 183 202 226 249 255 255
    218      25  55 121 176 196 212 242 253 255 255
    219      38  90 158 193 203 222 241 252 255 255
    220      68 135 178 195 214 235 251 255 255 255
    221     109 165 186 201 232 251 255 255 255 255
    222     153 188 204 221 243 253 255 255 255 255
copy /project/test_work/testdata/mipl/gll/ptp_gll_grn.img grn.img
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
WRITE "----------------------------------------------------------------------"
----------------------------------------------------------------------
WRITE "TEST 2: Input is summation and reference is full frame."
TEST 2: Input is summation and reference is full frame.
WRITE ""

ptp INP=/project/test_work/testdata/gll/s0401863200.1 OUT=sf.img REF=/project/test_work/testdata/gll/s0401863178.1 'INCLUDE
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is GLL
CKNAME=NAV   SPKID=N126  PROGRAM=NAV     TFT
Reference image spacecraft is GLL
CKNAME=NAV   SPKID=N126  PROGRAM=NAV     TFT
 OS target center=(    194.93,    231.37)
 OS target center for ref image=(    382.99,    462.28)
 ***PTP task completed
list sf.img (267,185,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:TASK      User:RTO040    Date_Time:Tue Jul 15 09:56:18 1997
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:21:08 2011
     Samp   185     187     189     191     193
   Line
    267      10  11  11  21  40  87 167 208 208 206
    268       8  10  12  31  68 119 183 215 212 207
    269       6   8  13  42  98 151 198 220 214 208
    270       7  11  21  59 130 180 207 218 213 207
    271       8  15  31  78 162 209 215 216 211 206
    272       9  23  51 103 183 223 218 214 208 205
    273      10  32  73 130 203 234 221 211 206 203
    274      14  45 105 163 217 237 220 210 206 203
    275      18  59 140 198 230 238 219 208 206 203
    276      31  79 164 215 230 230 216 207 205 203
let $echo="no"
==================================================================
========================= Cassini Test ===========================
==================================================================
ptp INP=/project/test_work/testdata/cassini/casIss/n135489734+
0.1 OUT=cas_red.img TARGET=JUPITER      REF=/project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img
Beginning VICAR task ptp
 PTP Version 29-Dec-2011
Input image spacecraft is CASSI
CKNAME=NAV   SPKID=N009  PROGRAM=NAV     GMY059  NONE  02/08/03
Reference image spacecraft is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
 OS target center=(    138.69,    840.44)
 OS target center for ref image=(    601.77,    709.68)
 ***PTP task completed
list cas_red.img (190,468,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:TASK      User:casrt     Date_Time:Fri Dec  8 10:00:23 2000
 Task:PTP       User:lwk       Date_Time:Thu Dec 29 15:21:12 2011
     Samp   468     470     472     474     476
   Line
    190       0   0   0   0   0   0   0  26  26  27
    191       0   0   0   0   0  25  26  26  26  26
    192       0   0   0  25  26  26  26  25  26  27
    193       0  27  27  27  26  26  27  28  28  27
    194      27  27  27  26  27  28  28  28  28  29
    195      27  27  26  27  28  28  29  30  31  30
    196      26  27  27  28  27  30  31  31  31  32
    197      28  27  28  29  29  31  31  32  33  33
    198      28  29  30  30  30  32  33  33  34  34
    199      29  30  31  31  32  33  33  34  36  35
End-Proc
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
