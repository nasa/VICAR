      INCLUDE 'VICMAIN_FOR'
C
C VICAR PROGRAM FIT -- Automatic stretch program
C Performs a histogram (ends-in) stretch.  See also program ASTRCH
C
C          FIT  INP  OUT  user-parameters
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      CHARACTER*5 FMT
      CHARACTER*3 ORGIN
      LOGICAL XVPTST
      INTEGER*4 HIST(65536)
      INTEGER*4 NLT,NST
      CHARACTER*40 MSG
      CHARACTER*30 MSG1,MSG2,MSG3,MSG4
      DATA  MSG1/'INVALID VALUE OF SL SET TO SL='/,
     *      MSG2/'INVALID VALUE OF NL SET TO NL='/,
     *      MSG3/'INVALID VALUE OF SS SET TO SS='/,
     *      MSG4/'INVALID VALUE OF NS SET TO NS='/
 111  FORMAT(A30,I3)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('FIT version 5 August, 2003',' ')
 
      CALL XVUNIT(IUN,'INP',1,IND,' ')
      CALL XVSIGNAL(IUN,IND,.TRUE.)
      CALL XVOPEN(IUN,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(IUN,IND,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      CALL XVBANDS(isb,nb,nbi)

      IF ( isb .GT. nbi ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( isb + nb - 1 .GT. nbi) THEN 
         CALL XVMESSAGE('***Number of bands truncated', ' ') 
         nb = nbi + 1 - isb
      ENDIF


      CALL XVGET(IUN,IND,'NL',NLT,'NS',NST,' ')
      IF (IND .NE. 1) THEN
        CALL XVMESSAGE('CANNOT GET INPUT INAGE SIZE.',' ')
        CALL XVMESSAGE('CANNOT VERIFY USER INPUT SIZE.',' ')
        CALL XVMESSAGE('*******ABANDING*********',' ')
        CALL ABEND
      ENDIF 
      IF (SL.LT.1) THEN
        SL=1
        WRITE(MSG,111) MSG1,SL
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (NL.GT.NLT) THEN
        NL=NLT
        WRITE(MSG,111) MSG2,NL
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (SS.LT.1) THEN
        SS=1
        WRITE(MSG,111) MSG3,SS
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (NS.GT.NST) THEN
        NS=NST
        WRITE(MSG,111) MSG4,NL
        CALL XVMESSAGE(MSG,0)
      ENDIF

C     ....Check data format of input image
      CALL XVGET(IUN,ind,'FORMAT',fmt,' ')
C     ....Format must be halfword, otherwise ABEND with message.
      IF (FMT.NE.'HALF'.AND.FMT.NE.'WORD') GOTO 992	

C     ....Generate the histogram
      CALL HISTGEN2(hist,nfreq)
      IF (NFREQ.EQ.0) GOTO 990

C     ....Print the histogram (optional)
      CALL PRINTHIS(HIST,NFREQ)

      CALL XVPCNT('OUT',NO)
      IF (NO.EQ.0) GOTO 100

C     ....Open output image
      IF (XVPTST('BYTE')) THEN
         OCODE=1
         FMT = 'BYTE'
      ELSE
         OCODE = 2
         FMT = 'HALF'
      ENDIF
      CALL XVUNIT(OUN,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUN,IND,.TRUE.)
      CALL XVOPEN(OUN,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &	'OP','WRITE','U_FORMAT',FMT,'U_NB', nb, 'O_FORMAT',FMT,' ')

C     ....Determine stretch limits
      CALL SLIMITS(OUN,OCODE,HIST,NFREQ,min,max,lval,hval)

C     ....Generate the output image
      CALL FITPIC(OCODE,MIN,MAX,LVAL,HVAL)

  100 CALL XVMESSAGE('FIT task completed',' ')
      RETURN

C     ....Error message handling
  990 CALL XVMESSAGE('***Histogram contains zero elements',' ')
      GOTO 999
  992 CALL XVMESSAGE('***Invalid input data format',' ')
      CALL XVMESSAGE('***Input image must be in halfword format',' ')
  999 CALL XVMESSAGE('***FIT task cancelled',' ')
      CALL ABEND
      END

C Routine to generate a histogram of 16-bit data
C Outputs:  HIST = 65536 grey-level histogram
C           NFREQ = total number of pixels in histogram
C Variable: BUF = temporary work area to hold input image lines
C
      SUBROUTINE HISTGEN2(hist,nfreq)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 HIST(-32768:32767)

      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      COMMON/AREA/LSL,LSS,LNL,LNS
      INTEGER*2 BUF(10000)

      CALL ZIA(hist,65536)		   !Zero out the histogram

      CALL XVPARM('AREA',lsl,idef,icnt,4)	!Requested image area
      IF (LSL.EQ.0) THEN 
	CALL MVE(4,4,SL,lsl,1,1)   	   !Default is the entire picture
      ENDIF

      CALL XVPARM('SPEED',linc,idef,icnt,1)
      LEL = LSL + LNL - 1
      eb = isb + nb - 1
      
C     ....Accumulate the histogram
      DO ib=isb,eb
         DO L=LSL,LEL,LINC
            CALL XVREAD(IUN,buf,ind,'LINE',L,'SAMP',LSS,'NSAMPS',LNS, 
     +                  'BAND',ib, ' ')
            DO I=1,LNS
               IDN = BUF(I)
               HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDDO
      ENDDO

      NFREQ = (LNL/LINC)*LNS*nb
      RETURN
      END

C Routine to print raw and excluded histograms.  Also processes EXCLUDE
C and INCLUDE parameters.  Excluded DNs are zeroed out in HIST.
C
      SUBROUTINE PRINTHIS(HIST,NFREQ)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 HIST(-32768:32767),EXCL(2,101)
      LOGICAL XVPTST
      CHARACTER*1  FORMFEED
      CHARACTER*33 MSG
      DATA MSG/'     RAW HISTOGRAM STATISTICS...'/

C     Form feed string for passing to XVMESSAGE
      FORMFEED = CHAR(12)

      CALL XVP('HINC',INC,ICNT)
      CALL XVP('NLINES',NLIN,ICNT)
      CALL XVP('SPIKES',NSPIKE,ICNT)

      IF (XVPTST('LOG')) LOG=1
      PHIS = 0
      IF (XVPTST('PHIST')) THEN
	CALL XVMESSAGE(FORMFEED,' ')
	PHIS = 1
      ELSE
	CALL XVMESSAGE(' ',' ')
      ENDIF
      CALL XVMESSAGE(MSG,' ')

      CALL PHIST2(HIST,NFREQ,NLIN,INC,NSPIKE,LOG,PHIS)

      CALL XVPARM('EXCLUDE',EXCL,NEXCL,IDEF,202)
      IF (IDEF.EQ.1) THEN
         IF (XVPTST('INCLUDE')) RETURN
      ELSE
         IF (MOD(NEXCL,2).NE.0) THEN
             CALL XVMESSAGE('***EXCLUDE must have pairs of numbers **',
     .		' ')
	     CALL ABEND
         ENDIF
         NEXCL = NEXCL/2		! # OF PAIRS

         DO K=1,NEXCL
            J1 = EXCL(1,K)      
            J2 = EXCL(2,K)     
            J1 = MAX0(J1,-32768) 
            J2 = MIN0(J2,32767)
            DO J=J1,J2
               NFREQ = NFREQ - HIST(J)
               HIST(J) = 0
            ENDDO            
         ENDDO
      ENDIF

C     ....Check for exclusion of lower saturation DN
      IF (.NOT.XVPTST('INCLUDE')) THEN
         DO J=-1,-32768,-1
            IF (HIST(J).GT.0) THEN
               NFREQ = NFREQ - HIST(-32768)
               HIST(-32768) = 0
               GOTO 50
            ENDIF
         ENDDO
         NFREQ = NFREQ - HIST(0)
         HIST(0) = 0
      ENDIF

   50 MSG(1:8) = 'EXCLUDED'
      IF (NFREQ.EQ.0) THEN
         CALL XVMESSAGE('***All pixels have been excluded',' ')
	 RETURN
      ENDIF
      PEHIS = 0
      IF (XVPTST('EHIST')) THEN
	CALL XVMESSAGE(FORMFEED,' ')
	PEHIS = 1
      ELSE
	CALL XVMESSAGE(' ',' ')
      ENDIF
      CALL XVMESSAGE(MSG,' ')
      CALL PHIST2(HIST,NFREQ,NLIN,INC,NSPIKE,LOG,PEHIS)
      RETURN
      END

C Print out a histogram containing 65536 grey-levels.
C
      SUBROUTINE PHIST2(HIST,NFREQ,NLIN,INC0,NSPIKE,LOG,IPRNT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 MAXT/2.147E09/,HIST(-32768:32767)

      CHARACTER*100 MSG1
      CHARACTER*550 MSG2
      CHARACTER*150 LISTO
      CHARACTER*150 LIST1

C     ....Initialize character arrays with blanks.
      DO I=1,150
	LISTO(I:I) = ' '
	LIST1(I:I) = ' '
      ENDDO
      DO I=1,532
	MSG2(I:I) = ' '
      ENDDO

C     ....Write header line for table of histogram
      MSG2(1:4) = 'GRAY'
      MSG2(12:24) = 'FREQ  PERCENT'
      DO I=1,10
	J = (I-1)*10
        K = 27 + J
	L = K + 1
	WRITE (MSG2(K:L), '(I2)') J
      ENDDO
      J = 100
      WRITE (MSG2(126:128), '(I3)') J	

C     ....Compute and print out mean and standard deviation
      S = 0.D0
      S2 = 0.D0

      DO I=-32768,32767
         IFREQ = HIST(I) 
         IF (IFREQ.GT.0) THEN
            R = I
            RMOM = IFREQ*R
            S = S + RMOM
            S2 = S2 + RMOM*R
         ENDIF
      ENDDO

      AVG = S/NFREQ
      SIGMA = S2/NFREQ - AVG*AVG
      SIGMA = DSQRT(SIGMA)
      WRITE(MSG1,101) AVG,SIGMA,NFREQ
  101 FORMAT('AVERAGE GRAY LEVEL=',F10.3,' STANDARD DEVIATION=',F10.3,
     +  ' NUMBER OF ELEMENTS=',I8)
      CALL XVMESSAGE(MSG1,' ')
C
C     ....Now print out the histogram
      IF (NLIN*IPRNT.EQ.0) RETURN
      INC = INC0        !Determine histogram increment
      IF (INC.EQ.0) CALL PINC(HIST,NLIN,inc)
      IEND = 32767 - INC + 1
      MAXS = MAXT

      DO J=1,NSPIKE	!Determine maximum frequency (ignoring spikes)
         MAX = 0
         DO I=-32768,IEND,INC
            CALL SUMV(4,INC,HIST(I),ifreq,1)
            IF (IFREQ.GT.MAX.AND.IFREQ.LT.MAXS) MAX=IFREQ
         ENDDO
         IF (MAX.LE.0) GOTO 20
         MAXS = MAX
      ENDDO

   20 Z0 = MAX0(MAX,1)
      IF (LOG.EQ.1) Z0=DLOG(Z0)
      R = 100.D0/NFREQ
      IF (LOG.EQ.1) THEN
	CALL XVMESSAGE('Horizontal scale is logarithmic',' ')
      ENDIF
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2,' ')

      NLOUT = 0		!Count of number of histogram lines printed
C
C     ....Print out the histogram
      DO 100 I=-32768,IEND,INC
      CALL SUMV(4,INC,HIST(I),IFREQ,1)
      IF (IFREQ.EQ.0.) THEN
         IF (IFLAG.EQ.0) CALL XVMESSAGE(' ',' ') !Print space to indicate data gap
         IFLAG = 1
      ELSE
         IF (NLOUT.GE.NLIN) THEN
            N = (32767-I)/INC		!See how close we are to end
            IF (N.GT.0.5*NLIN) GOTO 990 !If not close, truncate listing
            NLOUT = 0			!If close, finish it.
         ENDIF
         IFLAG = 0
         PERCEN = IFREQ*R
         NCHAR = 129
         Z = MAX0(IFREQ,0)
         IF (LOG.EQ.1.AND.IFREQ.GT.0) Z=DLOG(Z)
         IVAL = 100.D0*Z/Z0 + 1
         IF (IVAL.GT.101) THEN		!Truncate values greater than 100
            IVAL = 101
            NCHAR = 131
         ENDIF

	 WRITE (LISTO(1:6),'(I6)') I
	 WRITE (LISTO(9:15),'(I7)') IFREQ
	 WRITE (LISTO(17:24),'(F8.3)') PERCEN
         WRITE (LISTO(28:127), '(A100)') LIST1
	 DO J=38,138,10
		LISTO(J:J) = '+'
	 ENDDO
	 DO J=1,IVAL
		LISTO(27+J:27+J) = '*'
	 ENDDO
         CALL XVMESSAGE(LISTO,' ')
         NLOUT = NLOUT + 1
      ENDIF
  100 CONTINUE
      RETURN

  990 CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('***Maximum number of lines (NLIN) exceeded',' ')
      CALL XVMESSAGE('***Histogram listing truncated',' ')
      RETURN
      END
C Find a suitable DN increment for printing out a histogram.
C
      SUBROUTINE PINC(HIST,NLIN,inc)
      INTEGER*4 HIST(-32768:32767)
      N1 = 0
      N2 = 0
      N4 = 0
      N8 = 0
      N16 = 0
      N32 = 0
      N64 = 0
      N128 = 0
      N256 = 0
      I = -32769
C
      DO I512=1,65536,256
	M128 = N128
	DO I256=1,2
          M64 = N64
          DO I128=1,2  
            M32 = N32
            DO I64=1,2   
              M16 = N16
              DO I32=1,2  
                M8 = N8
                DO I16=1,2
                  M4 = N4
                  DO I8=1,2 
                    M2 = N2
                    DO I4=1,2  
                      M1 = N1
                      DO I2=1,2 
                        I = I + 1
                        IF(HIST(I).GT.0) N1=N1+1    
                      ENDDO                          
                      IF(M1.LT.N1) N2=N2+1
                    ENDDO                            
                    IF(M2.LT.N2) N4=N4+1
                  ENDDO                              
                  IF(M4.LT.N4) N8=N8+1
                ENDDO                                
                IF(M8.LT.N8) N16=N16+1
              ENDDO                                   
              IF(M16.LT.N16) N32=N32+1
            ENDDO     
            IF(M32.LT.N32) N64=N64+1
          ENDDO         
          IF(M64.LT.N64) N128=N128+1
        ENDDO 
        IF(M128.LT.N128) N256=N256+1
      ENDDO

      INC = 1
      IF(N1.LE.NLIN.AND.N1.GT.N2) RETURN
      INC = 2
      IF(N2.LE.NLIN.AND.N2.GT.N4) RETURN
      INC = 4
      IF(N4.LE.NLIN.AND.N4.GT.N8) RETURN
      INC = 8
      IF(N8.LE.NLIN.AND.N8.GT.N16) RETURN
      INC = 16
      IF(N16.LE.NLIN.AND.N16.GT.N32) RETURN
      INC = 32
      IF(N32.LE.NLIN.AND.N32.GT.N64) RETURN
      INC = 64
      IF(N64.LE.NLIN.AND.N64.GT.N128) RETURN
      INC = 128
      IF(N128.LE.NLIN.AND.N128.GT.N256)RETURN
      INC=256
      IF(N256.GT.NLIN) INC=512
      RETURN
      END
C Determine stretch limits
C
      SUBROUTINE SLIMITS(OUN,OCODE,HIST,NFREQ,min,max,lval,hval)
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 RPER,LPER,HPER
      CHARACTER*39 MSG1
      CHARACTER*39 MSG2
      CHARACTER*43 MSG5
      DATA MSG1/'MINIMUM DN OF          SCALED TO XXXXX'/
      DATA MSG2/'MAXIMUM DN OF          SCALED TO XXXXX'/
      DATA MSG5/'STRETCH        TO XXXXX AND        TO      '/
      CALL XVPARM('PERCENT',RPER,ICNT,IDEF,1)
      LPER = RPER/2.
      HPER = RPER/2.
      IF (IDEF.EQ.1) THEN
	CALL XVPARM('LPERCENT',LPER,ICNT,IDEF,1)
	CALL XVPARM('HPERCENT',HPER,ICNT,IDEF,1)
      ENDIF

      CALL XVPARM('LVALUE',LVAL,ICNT,IDEF,1)
      CALL XVPARM('HVALUE',HVAL,ICNT,IDEF,1)
      IF (OCODE.EQ.1.AND.IDEF.EQ.1) HVAL=255

C     ....Compute percentage limits (MIN,MAX)
      CALL ASTRC2(HIST,NFREQ,LPER,HPER,min,max)

C     ....Print stretch limits
      WRITE (MSG1(15:20),'(I6)') MIN
      WRITE (MSG1(34:38),'(I5)') LVAL
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG1,' ')
      WRITE (MSG2(15:20),'(I6)') MAX
      WRITE (MSG2(34:38),'(I5)') HVAL
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2,' ')

C     ....Put stretch limits in label
      WRITE (MSG5(8:13),'(I6)') MIN
      WRITE (MSG5(18:22),'(I5)') LVAL
      WRITE (MSG5(28:33),'(I6)') MAX
      WRITE (MSG5(38:42),'(I5)') HVAL
      CALL XLADD(OUN,'HISTORY','LIMITS',MSG5,STAT,'ULEN',43,
     &		'FORMAT','STRING',' ')
      RETURN
      END
C Determine stretch limits from the histogram
C
      SUBROUTINE ASTRC2(HIST,NFREQ,LPER,HPER,min,max)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 LPER,HPER

      MIN = -32768
      MAX = 32767

      IF (NFREQ.EQ.0) RETURN

      ISUM = HIST(MIN)
      KOUNT = NFREQ*.01*LPER + 1.5
      DO WHILE (ISUM.LT.KOUNT)
         MIN = MIN + 1
         ISUM = ISUM + HIST(MIN)
      ENDDO

      ISUM = HIST(MAX)
      KOUNT = NFREQ*.01*HPER + 1.5
      DO WHILE(ISUM.LT.KOUNT)
         MAX = MAX - 1
         ISUM = ISUM + HIST(MAX)
      ENDDO

      IF (MAX.LT.-32767) MAX=-32767
      IF (MIN.GE.MAX) MIN=MAX-1
      RETURN
      END
C Generate the stretched output image
C Output image code: 	OCODE ( =1 is byte, =2 is halfword )
C 			BUF(NS)  = input and/or output image-line buffer
C             		OBUF(NS) = output image-line buffer
C
      SUBROUTINE FITPIC(OCODE,MIN,MAX,LVAL,HVAL)
      IMPLICIT INTEGER(A-Z)

      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      INTEGER*2 ITBL(-32768:32767),BUF(10000)
      BYTE TBL(-32768:32767),OBUF(20000)

      IF (OCODE.EQ.1) THEN
C     ....Generate a halfword-to-byte lookup table
	      CALL MVE(-5,MIN+32769,LVAL,TBL,0,1)
	      N = MAX - MIN + 1
	      CALL INTRPA(1,N,TBL(MIN),LVAL,HVAL)
	      CALL MVE(-5,32768-MAX,HVAL,TBL(MAX),0,1)

	      EL = SL + NL - 1
              eb = isb + nb - 1
C          ....Generate byte output image
           BANDOUT=0
           DO ib=isb,eb
              BANDOUT = BANDOUT + 1
              LINEOUT = 0
              DO L=SL,EL
                 LINEOUT = LINEOUT + 1
                 CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',SS,
     .                       'NSAMPS',NS, 'BAND', ib, ' ')
                 DO I=1,NS
                    OBUF(I) = TBL(BUF(I))
                 ENDDO
                 CALL XVWRIT(OUN,OBUF,STAT,'LINE',LINEOUT,
     +            'BAND',BANDOUT,' ')
              ENDDO
           ENDDO
      ELSE
C     ....Generate a halfword lookup table
	      CALL MVE(-6,MIN+32769,LVAL,ITBL,0,1)
	      N = MAX - MIN + 1
	      CALL INTRPA(2,N,ITBL(MIN),LVAL,HVAL)
	      CALL MVE(-6,32768-MAX,HVAL,ITBL(MAX),0,1)

	      EL = SL + NL - 1
           eb = isb + nb - 1
C          ....Generate halfword output image
           BANDOUT = 0
  	      DO ib=isb,eb
              BANDOUT = BANDOUT + 1
              LINEOUT = 0
              DO L=SL,EL
                 LINEOUT = LINEOUT + 1
                 CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',SS,
     .                'NSAMPS',NS, 'BAND', ib, ' ')
                 DO I=1,NS
                    BUF(I) = ITBL(BUF(I))
                 ENDDO
                 CALL XVWRIT(OUN,BUF,STAT,'LINE',LINEOUT,
     +                       'BAND',BANDOUT,' ')
              ENDDO
           ENDDO
      ENDIF

      RETURN
      END
