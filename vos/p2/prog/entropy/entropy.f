      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

         IMPLICIT NONE

         INTEGER*2 BUF(10000)
         INTEGER HIST(-4095:4095)
         INTEGER PARB(500),IND,ICNT,IDEF
         INTEGER S_IND,E_IND,NLI,NSI,SL,SS,NL,NS,JST,ISPIKE
         INTEGER IAREA(4),IST,S_PLUS_E,INPUT,IEL,LINE,ZERO,NPRP,J 

         CHARACTER*4   FORMAT
         CHARACTER*80  PROP
         CHARACTER*160 MSG
         CHARACTER*255 DISTFILE

         REAL*4 LOG2,ENTRPY,XSUM,XSUM2,AREA,XABORT,XADD,PJ,XMEAN
         REAL*4 SIGMADELDN

         LOGICAL XVPTST,DIST_FLAG

         MSG = ' '
         DIST_FLAG = .FALSE.

C        max values for histogram
         S_IND = -4095
         E_IND = 4095
         S_PLUS_E = E_IND + ABS(S_IND) + 1

         CALL IFMESSAGE ('ENTROPY version April 22, 2000')
         CALL XVUNIT(INPUT,'INP',1,IND,' ')

C     check to see whether to put the entropy value into the frame's label.
         IF (XVPTST('LABEL')) THEN
            CALL XVOPEN(INPUT,ind,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF','OP','UPDATE',' ')
         ELSE
            CALL XVOPEN(INPUT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF',' ')
         ENDIF

C        check for output normalized difference histogram file name.
         CALL XVPARM('DISTFILE',DISTFILE,ICNT,IDEF,' ')
         IF (ICNT .GE. 1) THEN
            OPEN (13, FILE=DISTFILE, STATUS='UNKNOWN',
     &            IOSTAT=JST,ERR=994)
            DIST_FLAG = .TRUE.
         ENDIF
 
         CALL XVGET(INPUT,IND,'NL',NLI,'NS',NSI,' ')
         CALL XVGET(INPUT,IND,'FORMAT',FORMAT,' ')
         IF ((FORMAT .NE. 'BYTE') .AND. (FORMAT .NE.'HALF')) GOTO 992

         CALL XVPARM('AREA',IAREA,ICNT,IDEF,' ')
         IF(ICNT .EQ. 0)THEN
            SL=1
            SS=1
            NL=NLI
            NS=NSI
         ELSE
            SL=IAREA(1)
            SS=IAREA(2)
            NL=IAREA(3)
            NS=IAREA(4)
         ENDIF

         IF((SL .GT. NLI) .OR. (SL+NL-1 .GT. NLI) .OR.
     &      (SS .GT. NSI) .OR. (SS+NS-1 .GT. NSI)) GOTO 995

         CALL ZIA(HIST,S_PLUS_E)

         XSUM = 0.0
         XSUM2 = 0.0
         IEL = SL + NL - 1
         DO LINE=SL, IEL
            CALL XVREAD(INPUT,BUF,IND,'LINE',LINE,'SAMP',SS,
     &                  'NSAMPS',NS,' ')
            CALL DIFFHIST(BUF,NS,HIST,XSUM,XSUM2,S_IND,E_IND,*993)
         ENDDO

         ZERO = 0
         ISPIKE = 1
         IF (XVPTST('ZEROES')) ZERO = 1
         IF (XVPTST('PHIST')) THEN 
            CALL PHIST(HIST,S_PLUS_E,S_IND,E_IND,ISPIKE,ZERO)
         ELSE IF (ZERO .EQ. 1) THEN
            WRITE (MSG,'(A30,A32)')
     &         'The ZEROES keyword is ignored,',
     &         ' because PHIST is not specified.'
            CALL XVMESSAGE (MSG,' ')
         END IF

C      ....Use the formula:          H  =  - >  p log p    to find entropy. 
C                                            -   j   2 j
         LOG2 = ALOG(2.)
         AREA = NL*(NS-1)
         ENTRPY = 0.0

         DO J=S_IND, E_IND
            PJ = HIST(J)/AREA
            IF (HIST(J).GT.0.) THEN
C               PJ = HIST(J)/AREA
               ENTRPY = ENTRPY - PJ*ALOG(PJ)/LOG2
            ENDIF
            IF (DIST_FLAG) THEN
               WRITE (13, 1001) J, PJ
1001           FORMAT (1x, I5, 3x, F7.5)
            ENDIF
         ENDDO

         IF (DIST_FLAG) CLOSE (13)

         XMEAN = XSUM/AREA
         SIGMADELDN=SQRT((XSUM2-AREA*XMEAN**2)/(AREA-1)) 
 
         WRITE(MSG,'(A9,F8.5,2x,A18,F9.5,2x,A15,I8)')
     &      'ENTROPY = ',ENTRPY,' SIGMA_DELTA_DN = ',SIGMADELDN,
     &      ' #_OF_PIXELS = ',IFIX(AREA)
         CALL XVMESSAGE(MSG,' ')

C        output value to TCL local variable
         CALL XQINI(PARB,500,XABORT)
         CALL XQREAL(PARB,'VALUE',1,ENTRPY,XADD,IST)
         CALL XVQOUT(PARB,IST)

         IF (XVPTST('NOLABEL')) RETURN

C        add a label item
C        update label only if AREA is equal to size of the image
         IF ((NL .NE. NLI) .OR. (NS .NE. NSI) .OR. 
     &       (SL .NE. 1) .OR. (SS .NE.1)) THEN
            CALL XVMESSAGE
     &      ('AREA not equal to image size.  ITEM NOT ADDED',' ')
            RETURN
         ENDIF

         CALL XLADD(INPUT,'HISTORY','ENTROPY',ENTRPY,IST,
     &         'MODE','REPLACE','FORMAT','REAL',' ')
         IF (IST .NE. 1) GOTO 991
         CALL XVMESSAGE ('ENTROPY ADDED TO HISTORY LABEL',' ')

         RETURN

991	 CALL XVMESSAGE
     &      ('***Error writing history label item',' ')
	 GOTO 999
992	 CALL XVMESSAGE
     &      ('***Input image must be byte or halfword format',' ')
	 GOTO 999
993	 CALL XVMESSAGE ('***Invalid DN values',' ')
         GOTO 999
994      CALL XVMESSAGE('***Error opening distribution output file',' ')
         GOTO 999
995      CALL XVMESSAGE ('***Selected area is outside of image',' ')
999	 CALL XVMESSAGE ('***Entropy task cancelled',' ')
         CALL ABEND
      END 


C Compute difference histogram (HIST).
      SUBROUTINE DIFFHIST(BUF,NPTS,HIST,XSUM,XSUM2,S_IND,E_IND,*)

         INTEGER S_IND,E_IND,NPTS,J,IDEL,I0,I1
         INTEGER HIST(S_IND:E_IND)
         INTEGER*2 BUF(NPTS)
         
         REAL*4 XSUM, XSUM2

	 I0 = BUF(1)
         IF ((I0 .GT. E_IND) .OR. (I0 .LT. 0)) GOTO 5

         DO J=2,NPTS  
            I1 = BUF(J)
            IF ((I1 .GT. E_IND) .OR. (I1 .LT. 0)) GOTO 5
            IDEL = I1 - I0
            XSUM = XSUM + IDEL
            XSUM2 = XSUM2 + IDEL * IDEL
            HIST(IDEL) = HIST(IDEL) + 1
            I0 = I1
         ENDDO
         RETURN
5        RETURN 1
      END
