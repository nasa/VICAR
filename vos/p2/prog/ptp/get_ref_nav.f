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
