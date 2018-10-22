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
