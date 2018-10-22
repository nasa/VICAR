C   ADAPTED FROM STRETCH by   Ron Alley           3/2/94
C   REVISION HISTORY
C
C    3/94  rea  Initial release
C    3/01  rea  Fix calls to knuth & xknuth to make compatible with the
C		new calling sequence
C
      INCLUDE 'VICMAIN_FOR'
 
      SUBROUTINE MAIN44
      EXTERNAL STRMAIN
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,DNMIN,DNMAX
      INTEGER*4 OUNIT,STAT,SL,SS,DNMIN,DNMAX,LUTSIZ
      CHARACTER*8 FORMAT
C
      CALL XVMESSAGE('STRETCHF Version 3-30-01',' ')
C                                                       OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C                                                    GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT .NE. 'FULL') CALL MYABORT (
     +				' STRETCHF accepts FULLWORD data only')
C                                               GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI)
     +    CALL MYABORT(' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      IF(SS+NSO-1 .GT. NSI)
     +    CALL MYABORT(
     +		' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
C                                                        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     &            'U_NL',NLO,'U_NS',NSO,' ')
C                                                 'DNMIN' - MINIMUM DN VALUE
      CALL XVPARM('DNMIN',DNMIN,ICOUNT,IDEF,1)
C                                                  'DNMAX' - MAXIMUM DN VALUE
      CALL XVPARM('DNMAX',DNMAX,ICOUNT,IDEF,1)
      IF(DNMIN.GT.DNMAX) 
     +    CALL MYABORT(' *** ERROR - DNMIN EXCEEDS DNMAX ***')
C                                                 DYNAMIC ALLOCATION OF BUFFERS
      NBI = 4*NSI
      NLEV=DNMAX-DNMIN+1
      LUTSIZ=4*NLEV
      CALL STACKA(4,STRMAIN,2,NBI,LUTSIZ)
C                                                               CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,0)
      CALL XVCLOSE(OUNIT,STAT,0)
      RETURN
      END
C***********************************************************************
      SUBROUTINE STRMAIN(BUF,NBI,LUT,LUTSIZ)
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,DNMIN,DNMAX
      REAL*4 RPARM(1000),TABBUF(200)
      INTEGER*4 OUNIT,DNMIN,DNMAX,STAT,LUTSIZ
      INTEGER*4 IPARM(1000)
      INTEGER*4 DNVAL,BCKGND,CONINC,SL,SS,HVAL,ALRBUF(100)
      LOGICAL XVPTST
      INTEGER*4 BUF(NBI),LUT(DNMIN:DNMAX)
      CHARACTER*1024 FNCBUF  
      EQUIVALENCE (RPARM,IPARM)
      CHARACTER*80 PRT,PRT2
C                                                                initialize
      ICHK=0
      NCHAR2=0
      NLEV=DNMAX-DNMIN+1
C              *** PROCESS STRETCH SPECIFICATION PARAMETERS ***
C                                                  'COMP' - COMPLEMENT IMAGE
      IF(XVPTST('COMP')) THEN
         IMODE=1
         ICHK=ICHK+1
      END IF
C                                                  'LINEAR' - LINEAR STRETCH
      CALL XVPARM('LINEAR',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .EQ. 2) THEN
         IMODE=2
         ICHK=ICHK+1
         NMIN = IPARM(1)
         NMAX = IPARM(2)
      END IF
C                                                  'CONTOUR'
      CALL XVPARM('CONTOUR',CONINC,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=4
         ICHK=ICHK+1
         IF(CONINC .EQ. 0) CALL MYABORT(
     +			' *** INVALID CONTOUR VALUE ***')
      ENDIF
C                                            'ALARM' - ALARM SPECIFIED DN VALUES
      CALL XVPARM('ALARM',ALRBUF,NALARM,IDEF,100)
      IF(NALARM .GE. 1) THEN
         IMODE=5
         ICHK=ICHK+1
      END IF
C                                                  'TABLE' - TABLE STRETCH
      CALL XVPARM('TABLE',TABBUF,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=6
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT(
     +      ' INVALID COUNT FOR PARAMETER "TABLE"')
      ENDIF
C                                      'ITABLE' - INDIVIDUAL DN TABLE STRETCH
      CALL XVPARM('ITABLE',IPARM,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=7
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT( 
     +      ' INVALID COUNT FOR PARAMETER "ITABLE"')
         DO I=1,ICOUNT
            TABBUF(I)=IPARM(I)
         ENDDO
      ENDIF
C                                                  'PSTRETCH' - PERIODIC STRETCH
      IF(XVPTST('PSTRETCH')) THEN
         IMODE=8
         ICHK=ICHK+1
      END IF
C                                      'FUNCTION' - USER SPECIFIED FUNCTION 
      CALL XVP('FUNCTION',FNCBUF,ICOUNT)
      CALL XVSPTR(FNCBUF,1,I,NCFUNC)
      IF (NCFUNC.GT.1) THEN
          IMODE=9
          ICHK=ICHK+1
      END IF
C                                                  'GAMMA' - GAMMA STRETCH
      CALL XVPARM('GAMMA',GAMMA,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=10
         ICHK=ICHK+1
      ENDIF
C                                CHECK THAT ONLY ONE STRETCH WAS SPECIFIED
      IF(ICHK.GT.1) THEN
         CALL XVMESSAGE(' *** MULTIPLE STRETCHES SPECIFIED ***',' ')
         CALL MYABORT('  ONLY ONE STRETCH MAY BE SPECIFIED')
      END IF
C        *** END OF GENERAL PARAMETER PROCESSING ***
C**********************************************************************
C        *** GENERATE LOOKUP TABLE FOR SPECIFIED STRETCH ***
      IF(IMODE.LT.1 .OR. IMODE.GT.10) CALL MYABORT(
     +   ' *** ILLEGAL STRETCH MODE ***')
      GO TO (110,120,110,140,150,160,170,180,190,200) IMODE
C                                                  --- COMPLEMENT MODE ---
110   CALL XVMESSAGE(' *** COMPLEMENT MODE ***',' ')
      DO I=DNMIN,DNMAX
         LUT(I) = DNMAX - (I-DNMIN)
      ENDDO
      WRITE (PRT,115) DNMIN,DNMAX,DNMAX,DNMIN
  115 FORMAT (' Complement Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR = 58
      GO TO 800
C                                               --- LINEAR STRETCH MODE ---
120   CALL XVMESSAGE(' *** LINEAR CONTRAST STRETCH MODE ***',' ')
      IF(NMIN.EQ.NMAX) NMAX = NMAX+1
C                                                 COMPUTE STRETCH TABLE
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO I = DNMIN,DNMAX
         LUT(I) = MAX(DNMIN,MIN(DNMAX,NINT(A*I + B)))
      ENDDO
      WRITE (PRT,125) NMIN,DNMIN,NMAX,DNMAX
  125 FORMAT (' Linear Stretch:',I9,' to',I9,' and',I9,' to',I9)
      NCHAR = 62
      GO TO 800
C                                                   --- CONTOUR MODE ---
140   CALL XVMESSAGE(' *** CONTOUR MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      CONINC=IABS(CONINC)
      DO I=DNMIN,DNMAX,CONINC
         LUT(I) = DNVAL
      ENDDO
      WRITE (PRT,145) CONINC,DNVAL
  145 FORMAT (' Contour Stretch: Interval =',I5,'  DNvalue =',I7)
      NCHAR=51
      GO TO 800
C                                                    --- ALARM MODE ---
150   CALL XVMESSAGE(' *** ALARM MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      DO I=1,NALARM
         IDN=ALRBUF(I)
         IF (IDN.GE.DNMIN .AND. IDN.LE.DNMAX) LUT(IDN)=DNVAL
      ENDDO
      WRITE (PRT,155) DNVAL
  155 FORMAT (' Alarm Stretch: DNvalue =',I7)
      NCHAR=32
      GO TO 800
C                                                 --- TABLE STRETCH MODE ---
160   CALL XVMESSAGE(' *** TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      NINTRV=NPAIRS-1
      DO J=1,NINTRV
         INDN1  = NINT( TABBUF(2*(J-1)+1) )
         OUTDN1 = TABBUF(2*(J-1)+2)
         INDN2  = NINT( TABBUF(2*J+1) )
         OUTDN2 = TABBUF(2*J+2)
         IF(INDN1.LT.DNMIN.OR.INDN2.LE.INDN1.OR.INDN2.GT.DNMAX)
     +      CALL MYABORT(' *** TABLE STRETCH PARAMETER ERROR')
         A=(OUTDN2-OUTDN1)/(INDN2-INDN1)
         B=OUTDN1-A*INDN1
         DO I = INDN1,INDN2
            LUT(I) = NINT(A*I + B)
         ENDDO
      ENDDO
      PRT = ' Table Stretch'
      NCHAR=14
      GO TO 800
C                                                  --- ITABLE MODE ---
170   CALL XVMESSAGE(' *** INDIVIDUAL DN TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      DO I=1,NPAIRS
         INDN  = TABBUF(2*(I-1)+1)
         IF(INDN.LT.DNMIN .OR. INDN.GT.DNMAX) CALL MYABORT(
     +      ' *** ERROR IN ITABLE PARAMETER')
         OUTDN = TABBUF(2*(I-1)+2)
         IF(OUTDN.LT.DNMIN) OUTDN=DNMIN
         IF(OUTDN.GT.DNMAX) OUTDN=DNMAX
         LUT(INDN)=OUTDN
      ENDDO
      PRT = ' Individual Table Stretch'
      NCHAR=25
      GO TO 800
C                                                     --- PSTRETCH MODE ---
180   CALL XVMESSAGE(' *** PERIODIC STRETCH MODE ***',' ')
C                                                    get related parameters 
      CALL XVPARM('FREQ',FREQ,ICOUNT,IDEF,1)
      CALL XVPARM('PHI',PHI,ICOUNT,IDEF,1)
      CALL XVPARM('DC',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DC = RPARM(1)
      ELSE
         DC = (DNMAX+DNMIN)/2.0
      END IF
      CALL XVPARM('AMPL',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         AMPL = RPARM(1)/2.0
      ELSE
         AMPL = (DNMAX-DNMIN)/2.0
      END IF
      W=2.0*3.14159*FREQ/(DNMAX-DNMIN)
      DO I=DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(AMPL*SIN(W*I+PHI) + DC)))
      ENDDO
      WRITE (PRT,185) FREQ,DC,2.0*AMPL,PHI
  185 FORMAT (' Periodic Stretch: FREQ=',F6.2,'  DC=',F8.1,
     +        '  AMPL=',F8.1,'  PHI=',F6.2)
      NCHAR=70
      GO TO 800
C                                                 --- FUNCTION MODE ---
190   CALL XVMESSAGE(' *** USER SPECIFIED FUNCTION MODE ***',' ')
      J = MIN(NCFUNC,50)
      NCHAR=30+J
      PRT = ' Function Stretch: FUNCTION = ' // FNCBUF(1:J)
C
      CALL ZIA(RPARM,1000)
      CALL KNUTH(FNCBUF,RPARM,IER)
      IF(IER.NE.0) THEN
         CALL XVMESSAGE(' *** ERROR IN PARSING FUNCTION WITH KNUTH',' ')
         CALL MYABORT(FNCBUF)
      END IF
      DO I=DNMIN,DNMAX
         RPARM(1)=I
         CALL XKNUTH(RPARM,DN)
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(DN)))
      ENDDO
      GO TO 800
C                                                      --- GAMMA OPTION ---
200   CONTINUE
      WRITE(PRT,205) GAMMA
205   FORMAT(' *** Gamma Stretch, Gamma =',F6.3)
      DNRANGE = DNMAX - DNMIN
      DO I=DNMIN,DNMAX
          LUT(I) = NINT(DNRANGE*((I-DNMIN)/DNRANGE)**(1.0/GAMMA)+DNMIN)
      END DO
C                                               *** LOOKUP TABLE COMPLETED ***
C*****************************************************************************
  800 CONTINUE
      CALL XVMESSAGE(PRT,' ')
      CALL XVPARM('POST',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
C                                          *** PERFORM POST-STRETCH ***
C                                   CALCULATE LINEAR TRANSFORMATION SO THAT
C                                   DNMIN GOES TO LVAL AND DNMAX GOES TO HVAL.
C                                   MODIFY LOOKUP TABLE TO INCLUDE THIS STRETCH.
        CALL XVMESSAGE(' ',' ')
        CALL XVMESSAGE(' *** POST-STRETCH OPTION ***',' ')
        LVAL=IPARM(1)
        HVAL=IPARM(2)
        IF(LVAL.LT.DNMIN .OR. HVAL.LT.DNMIN) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER LESS THAN DNMIN',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMIN',' ')
           IF(LVAL.LT.DNMIN) LVAL=DNMIN
           IF(HVAL.LT.DNMIN) HVAL=DNMIN
        END IF
        IF(LVAL.GT.DNMAX .OR. HVAL.GT.DNMAX) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER EXCEEDS DNMAX',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMAX',' ')
           IF(LVAL.GT.DNMAX) LVAL=DNMAX
           IF(HVAL.GT.DNMAX) HVAL=DNMAX
        END IF
        IF(LVAL.EQ.HVAL) CALL MYABORT(
     +     ' *** ERROR - POST VALUES SPECIFIED ARE EQUAL')
C                                           COMPUTE COMPOSITE LOOKUP TABLE
        A = FLOAT(HVAL-LVAL)/FLOAT(DNMAX-DNMIN)
        B = -A*DNMIN+LVAL
        DO I = DNMIN,DNMAX
           LUT(I) = NINT( A*LUT(I) + B )
        ENDDO
C                                            SET UP SECOND ADDED LABEL
        WRITE (PRT2,810) DNMIN,LVAL,DNMAX,HVAL
  810   FORMAT (' Post-stretch:',I7,' to',I7,' and',I7,' to',I7)
        NCHAR2=52
        CALL XVMESSAGE(PRT2,' ')
      END IF
C                                        *** LOOKUP TABLE GENERATED ***
C ***********************************************************************
C                                              *** PERFORM STRETCH ***
C                                                   UPDATE LABEL
      CALL XLADD(OUNIT,'HISTORY','PARMS',PRT,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR,0)
      IF(NCHAR2.GT.0) CALL XLADD(OUNIT,'HISTORY','PARMS2',PRT2,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR2,0)
C                                              APPLY STRETCH TABLE TO THE DATA
      DO L=SL,SL+NLO-1
         CALL XVREAD(IUNIT,BUF,STAT,'LINE',L,'SAMP',SS,'NSAMPS',NSO,0)
         DO I=1,NSO
            IF (BUF(I) .GE. DNMAX) THEN
               BUF(I) = LUT(DNMAX)
            ELSE IF (BUF(I) .LE. DNMIN) THEN
               BUF(I) = LUT(DNMIN)
            ELSE
               BUF(I) = LUT(BUF(I))
	    END IF
         END DO
         CALL XVWRIT(OUNIT,BUF,STAT,0)
      END DO
      RETURN
      END
C*************************************************************************
        subroutine MYABORT(msg)
        character*(*) msg
        call xvmessage(msg, ' ')
        call abend
        return
        end

