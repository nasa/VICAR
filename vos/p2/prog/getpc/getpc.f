C------------------------------------------------------------------------------
C PROGRAM GETPC
C Purpose: Retrieves PC from SPICE kernal and assign to TAE variable.
C      For GLL and CASSI, also assigns SCLAT, SCLON, and SCALE to TAE variable.
C------------------------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      INTEGER IND
      REAL*4 C(2),DATA(40),SCL, CONV(3)
      CHARACTER*4 CKNReturn
      CHARACTER*5 Project
      INTEGER     Camera

      CALL xvmessage ('GETPC Version Jan 30, 2003',' ')
      
      CALL MSEDR(IND,Project,Camera,CKNReturn,DATA,SCL)
      IF (IND .LT. 0) THEN
        CALL ABEND
      END IF

C-----LAT LON to LINE SAMP Conversion
      IF (project.EQ.'GLL  ' .OR. project.EQ.'CASSI') THEN
        CALL mvcl (PROJECT, conv, 5)
        CALL mve (4,1,CAMERA, conv(3),1,1)
      END IF
      CALL CONVEV(IND,DATA,DATA,C(1),C(2),DATA(31),DATA(32),1,CONV)

      CALL PPC(Project,C,CKNReturn,data(31),data(32),SCL)
      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine MSEDR
C Input: none
C Output: ind => integer, 0 -> all successful
C                         1 -> non-fatal error, all fine but CK source unknown
C                        -1 -> fatal error, getspice2 failed
C                        -2 -> fatal error, getcamcon failed
C                        -3 -> fatel error, unable to determine project/camera
C         Project => CHARACTER*5, The type of project of the file specified
C                    by the INP VICAR user parameter (e.g. GLL)
C         Camera => INTEGER, the camera serial number of the file INP
C         CKReturn => CHARACTER*4, The source of C-Kernel buffer used to
C                     process DATA (e.g. DAVI, NAV, NAV2..etc)
C         DATA => 40xREAL*4, MP Buffer
C         SCL => REAL*4, Calculated Scale in KM/Pixel
C------------------------------------------------------------------------------
      	SUBROUTINE MSEDR(IND,Project,Camera,CKNReturn,DATA,SCL)
	INTEGER IND
        CHARACTER*5 Project
        CHARACTER*4 CKNReturn
        REAL*4 DATA(40)
        REAL*4 SCL

      	REAL*8 PI,R2,DTOR,GBUF8(100)
        REAL*4 GBUF4(200)
	CHARACTER*4 CKNRequest, CKID
        INTEGER*4 IntBuf
        INTEGER   Camera

        EQUIVALENCE (GBUF4, GBUF8) ! Spice Buffer, in 4 byte and 8 byte format

        Ind = 0

C-------Open input file for getspice2 and getproj
	CALL XVUNIT(IUN,'INP',1,IST, ' ')
	CALL XVOPEN(IUN,ind, ' ')
        IF (Ind.NE.1) call xvsignal(iunit,ind,1)

C-------Get Project and Camera
        CALL getproj(IUN, Project,Camera,IntBuf,ind) ! IntBuf will not be used
        IF (Ind.NE.0) THEN
          CALL xvmessage ('GETPROJ failed',' ')
          Ind = -3
          RETURN
        END IF

C-------Get Spice buffer
        call getspice2(IUN, .TRUE., GBUF8, ind)
	CALL XVCLOSE(IUN,IST, ' ')
        IF (ind.NE.1) THEN
          CALL xvmessage ('GETSPICE2 Failed', ' ')
          CALL xvmessage ('GETPC cannot continue', ' ')
          ind = -1
          RETURN
        END IF

C-------Check if desired source was found.
        CALL mvlc(GBUF4(11), CKNRequest,4)
        IF (CKNRequest.EQ.'    ') CKNRequest='NAIF'
        CALL MVLC (GBUF4(172), CKID, 4)
        CALL CKID2CKName (CKID, CKNReturn)
        IF (CKNRequest.NE.CKNReturn) THEN
          CALL XVMESSAGE ('Source '//CKNRequest//' not available',' ')
          CALL XVMESSAGE ('Retrieving from '//CKNReturn,' ')
        END IF
       
C-------Process SPICE Buffer
   50   PI = 3.141592653589793D0
	dtor = pi/180.D0
        IntBuf=8       ! need this for MVE
	CALL MVE(4, 1, IntBuf, DATA(39), 1, 1)
	CALL GETCAMCON(Project,Camera,DATA(27),DATA(28),DATA(29),
     +                 DATA(30),IND)
        IF (ind.NE.0) THEN
          CALL xvmessage ('GETCAMCON error', ' ')
          ind = -2
          RETURN
        END IF

C          PLANET RADII
      DATA(25) = GBUF8(15)
	rp2=data(25)*data(25)
      DATA(26) = GBUF8(13)
	re2=data(26)*data(26)
	r2=1.0D0*rp2/re2
C          RANGE
      DATA(38) = GBUF8(27)
C          SUBSPACECRAFT (LAT,LON)
      DATA(31) = GBUF8(30)
      DATA(32) = DMOD(GBUF8(31)+360.D0,360.D0)

C-----NORTH ANGLE
      DATA(35) = DMOD(GBUF8(68)+90.D0,360.D0)
C-----OM MATRIX
       CALL MVE(8,9,GBUF8(59),DATA, 1, 1)
C-----RS VECTOR
      CALL MVE(8,3,GBUF8(22),DATA(19), 1, 1)
C-----SCALE IN KM/PX
      SCL = GBUF8(27)/(DATA(27)*DATA(30))

      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine PPC
C Purpose: Print and output PC; if GLL or CAS also output SCLAT/LON and SCALE
C------------------------------------------------------------------------------
      SUBROUTINE PPC(Project,PC,Source,LAT,LON,SCL)
      CHARACTER*5 Project       ! Input: GLL, CASSI, VGR-1 or VGR-2
      CHARACTER*4 Source        ! Input: DAVI, NAV, NAV2...etc
      REAL*4 PC(2),LAT,LON,SCL  ! Input: SCLAT,SCLON and SCALE

      INTEGER*4 PARB(500)
      CHARACTER*80 MSG
 
      CALL XVMESSAGE(' ', ' ')
      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
 	CALL XVMESSAGE('IMAGE SPACE PLANET CENTER (LINE, SAMP)',' ')
      ELSE
        CALL xvmessage('OBJECT SPACE PLANET CENTER (LINE, SAMP)',' ')
      END IF

      WRITE (msg, '(a4, '' PC = '', f9.2, ''  '', f9.2)') Source,
     +              PC(1), PC(2)
      CALL XVMESSAGE(MSG,' ')

      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
        CALL PRNT(7,1,LAT,' SCLAT =.')
	CALL PRNT(7,1,LON,' SCLON =.')
	CALL PRNT(7,1,SCL,' SCALE (KM/PX) =.')
      END IF

      CALL XQINI(PARB,500,XABORT)
      CALL XQREAL(PARB,'PCL',1,PC(1),XADD,IST)
      CALL XQREAL(PARB,'PCS',1,PC(2),XADD,IST)

      IF (Project.EQ.'GLL  ' .or. Project.EQ.'CASSI') THEN
        CALL XQREAL(PARB,'SCLAT',1,LAT,XADD,IST)
        CALL XQREAL(PARB,'SCLON',1,LON,XADD,IST)
        CALL XQREAL(PARB,'SCALE',1,SCL,XADD,IST)
      END IF
      CALL XVQOUT(PARB,IST)

      RETURN
      END

C------------------------------------------------------------------------------
C Subroutine CKID2CKName
C Purpose: Converts a C-Kernel ID (CKID) to its common Name format (CKName)
C
C Usage  : This procedure is useful in converting the 172nd element of the 4x200
C          SPICE buffer to common recognized SOURCE name (DAVI, NAV, NAIF, etc)
C
C Input: CKID    => CHARACTER*4, CKID as returned by the 172nd element of the
C                   4x200 SPICE buffer.
C Output: CKName => CHARACTER*4, Common recognized SPICE source name such as:
C                     NAIF, AMOS, NEAR, NAV2, FAR, NAV, DAVI, etc.
C------------------------------------------------------------------------------
      SUBROUTINE CKID2CKName (CKID, CKName)
      IMPLICIT NONE
      CHARACTER*4 CKID
      CHARACTER*4 CKName

      CKNAME='NAIF'     ! default to NAIF
      IF (CKID.EQ.'M901') CKNAME='AMOS'
      IF (CKID.EQ.'M902') CKNAME='NEAR'
      IF (CKID.EQ.'M903') CKNAME='NAV2'
      IF (CKID.EQ.'M904') CKNAME='FARE'
      IF (CKID.EQ.'M905') CKNAME='NAV'
      IF (CKID.EQ.'M906') CKNAME='DAVI'
      RETURN
      END
