c  test subroutine HSUB
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C test pgm for subroutine HSUB
      IMPLICIT INTEGER (A-Z)
      BYTE BUF(1000000)
      DIMENSION HIST(65536)
      CHARACTER*8 FORMAT
C==================================================================
      CALL XVUNIT(IUNIT,'INP',1,STAT, ' ')
      CALL XVOPEN(IUNIT,STAT, ' ')
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT, ' ')
      DCODE=0
      IF(FORMAT.EQ.'BYTE') DCODE=1
      IF(FORMAT.EQ.'HALF') DCODE=2
      IF (DCODE.LT.1.OR.DCODE.GT.2) THEN
	CALL XVMESSAGE(' ** ILLEGAL FORMAT **',' ')
	CALL ABEND
      ENDIF
      CALL XVPARM('IHI',IHI,ICOUNT,IDEF,1)
      CALL XVPARM('ILO',ILO,ICOUNT,IDEF,1)
      IF (DCODE .EQ. 2 .AND. IHI .EQ. 0) IHI=32767

      call zia(HIST, 65536)   ! INITIALIZE HISTOGRAM
      DO I = 1,NLO
         CALL XVREAD(IUNIT,BUF,STAT,'LINE',I,'NSAMPS',NSO, ' ')
         IF ( MOD(I,2) .EQ. 1)  THEN
            CALL HSUB(DCODE,NSO,BUF,HIST,ILO,IHI)   ! test from Fortran
         ELSE
            call tzhsub(DCODE,NSO,BUF,HIST,ILO,IHI) ! and C.
         END IF
      ENDDO

      IF (DCODE.EQ.1) THEN
	MAX = 1
	DO I=1,256
	  IF (HIST(I).NE.0) MAX=I
	ENDDO
	CALL PRNT(4,MAX,HIST,' HIST:.')
      ELSE
        IF (IHI .LT. ILO) THEN
            CALL PRNT(4,ILO-IHI+1,HIST,' HIST:.')
        ELSE
	    CALL PRNT(4,IHI-ILO+1,HIST,' HIST:.')
        END IF
      ENDIF

      CALL XVCLOSE(IUNIT,STAT, ' ')
      RETURN
      END
