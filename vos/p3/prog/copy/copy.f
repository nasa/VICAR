      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
      CHARACTER*25 MSG
      CHARACTER*7 FORMAT
      CHARACTER*3 ORG,O_ORG/'BSQ'/
      INTEGER SL,SS,SB,NL,NS,NB,NLI,NSI,NBI,COUNT,DEF,INPUT,OUTPUT,STAT
      COMMON /INSZ/ NL,NS,NB
      INTEGER BUFFER_SIZE
      PARAMETER (BUFFER_SIZE=200000)
      INTEGER EB,BAND,EL,LINE,ES,SAMP,BUF(BUFFER_SIZE),NLIN,NSIN,NBIN
      LOGICAL XVPTST
      INTEGER PIXSIZE
      LOGICAL BINARY
      INTEGER NLBI, NSBI
      INTEGER REC, NRECS

      NLBI = 0
      NSBI = 0

      BINARY = XVPTST('BINARY')

      CALL XVUNIT(INPUT,'INP',1,STAT, ' ')
      CALL XVUNIT(OUTPUT,'OUT',1,STAT, ' ')
      IF ( BINARY ) THEN 
           CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA',
     *                          'COND','BINARY', ' ')
      ELSE 
           CALL XVOPEN(INPUT,STAT,'OPEN_ACT',' ','IO_ACT','SA', ' ')
      ENDIF
      
      IF (STAT .NE. 1) THEN
C
C	NO LABELS
C
	  CALL XVPARM('FORMAT',FORMAT,COUNT,DEF,0)
	  IF ( DEF.EQ.0) THEN
	      CALL XVADD(OUTPUT,STAT,'U_FORMAT',FORMAT,
     2                   'O_FORMAT',FORMAT, ' ')
	      CALL XVADD(INPUT,STAT,'U_FORMAT',FORMAT,
     2        		 'I_FORMAT',FORMAT, ' ')
	  ENDIF


	  CALL XVPARM('INSIZE',NL,COUNT,DEF,0)
	  IF (DEF.EQ.0) CALL XVADD(INPUT,STAT,'U_NL',NL,
     2                              'U_NS',NS,'U_NB',NB, ' ')

	  CALL XVPARM('ORG',ORG,COUNT,DEF,0)
	  IF ( DEF.EQ.0) CALL XVADD(INPUT,STAT,'U_ORG',ORG, ' ')
	  CALL XVOPEN(INPUT,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     2                    'COND','NOLABELS', ' ')
	  CALL XVADD(OUTPUT,STAT,'COND','NOLABELS', ' ')
      ENDIF

      IF (XVPTST('NOBLOCK'))
     2    CALL XVADD(OUTPUT,STAT,'COND','NOBLOCK', ' ')

      CALL XVGET(INPUT,STAT,'ORG',ORG, ' ')
      CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN)
      CALL XVGET(INPUT,STAT,'NL',NLI,'NS',NSI,'NB',NBI,
     *           'PIX_SIZE',PIXSIZE, ' ')
      CALL XVGET(INPUT,STAT,'NLB',NLBI,'NBB',NSBI, ' ')
      IF (SL.GT.NLI .OR. SS.GT.NSI) THEN
          CALL XVMESSAGE('SL/SS cannot exceed NL/NS of input image',' ')
          CALL ABEND()
      ENDIF
      IF ((SL+NL-1).GT.NLI .OR. (SS+NS-1).GT.NSI .OR. 
     2(SB+NB-1).GT.NBI) THEN
          CALL XVMESSAGE(
     2        'Copy window too large, truncating output image...', ' ')
          NL = NLI-SL+1
          NS = NSI-SS+1
          NB = NBI-SB+1
      ENDIF
      CALL XVBANDS(SB,NB,NBIN)

      IF ( (( SL .NE. 1 ) .OR. ( SS .NE. 1 ) .OR. ( SB .NE. 1 ) .OR.
     *	   ( NL .NE. NLI) .OR. ( NS .NE. NSI) .OR. ( NB .NE. NBI ))
     *     .AND. BINARY ) THEN

         NLBI = 0
         NSBI = 0
         BINARY = .FALSE.

C        CAN NOT COPY BINARY PARTS UNAMBIGUOUSLY IF OUTPUT IS A
C        SUB-WINDOW OF INPUT, SO CANCEL ANY BINARY PART COPY.

         CALL XVMESSAGE('Output is a sub-window of input, so', ' ')
         CALL XVMESSAGE('binary parts can not be copied.', ' ')
         CALL XVMESSAGE('Continuing with copy of non-binary parts.',' ')

      ENDIF

      IF ((ORG .NE. 'BIP' .AND. NS*PIXSIZE .GT. BUFFER_SIZE) .OR.
     *    (ORG .EQ. 'BIP' .AND. NB*PIXSIZE .GT. BUFFER_SIZE)) THEN
          CALL XVMESSAGE('Record is too big for internal buffer', ' ')
          CALL XVMESSAGE('Please notify cognizant programmer for COPY',
     *                   ' ')
          CALL ABEND
      END IF

      IF (NB .NE. 1) O_ORG = ORG
      IF ( BINARY ) THEN
           CALL XVOPEN(OUTPUT,STAT,'OP','WRITE',
     2		       'U_NL',NL,'U_NS',NS,'U_NB',NB,
     3		       'U_ORG',O_ORG,'OPEN_ACT','SA','IO_ACT','SA',
     4                 'COND','BINARY',
     5		       'U_NBB',NSBI,'U_NLB',NLBI, ' ')
      ELSE 
           CALL XVOPEN(OUTPUT,STAT,'OP','WRITE',
     2		      'U_NL',NL,'U_NS',NS,'U_NB',NB,
     3		      'U_ORG',O_ORG,'OPEN_ACT','SA','IO_ACT','SA', ' ')
      ENDIF
C
      IF (NL.NE.NLIN .OR. NS.NE.NSIN) THEN
	  WRITE(MSG,100) SL,SS,NL,NS
  100	  FORMAT('(',I5,',',I5,',',I5,',',I5,')')
	  CALL XLADD(OUTPUT,'HISTORY','SUBAREA',MSG,STAT,
     +		     'FORMAT','STRING',' ')
      END IF
      IF (NB .NE. NBIN) THEN
	  WRITE(MSG,200) SB,NB
  200	  FORMAT('(',I4,',',I4,')')
	  CALL XLADD(OUTPUT,'HISTORY','SB_NB',MSG,STAT,
     +		     'FORMAT','STRING',' ')
      END IF

C	IF BINARY COPY REQUESTED THEN COPY THE BINARY
C	HEADERS AND DATA IN A SPECIAL SIMPLE COPY LOOP.
C	THIS WORKS BECAUSE WE HAVE ALREADY REJECTED A
C	BINARY COPY WHEN WINDOWING IS SELECTED SO THAT
C	WE DON'T HAVE TO WORRY ABOUT KEEPING TRACK OF
C	LINES, BANDS, AND SAMPLES AS IS DONE BELOW.
C
      IF ( BINARY ) THEN
      
        IF ( ORG .EQ. 'BSQ' ) NRECS = NLBI + NL*NB
        IF ( ORG .EQ. 'BIL' ) NRECS = NLBI + NL*NB
        IF ( ORG .EQ. 'BIP' ) NRECS = NLBI + NL*NS

        DO REC = 1,NRECS
            CALL XVREAD(INPUT,BUF,STAT, ' ')
            CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
         ENDDO 

        CALL XVCLOSE(INPUT,STAT, ' ')
        CALL XVCLOSE(OUTPUT,STAT, ' ')
        RETURN

      ENDIF

      IF (ORG .EQ. 'BSQ') THEN
	  EB = SB+NB-1
	  EL = SL+NL-1
	  DO BAND=SB,EB
	      DO LINE=SL,EL
	  	  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     2			      'LINE',LINE,'BAND',BAND, ' ')
		  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ELSE IF (ORG .EQ. 'BIL') THEN
	  EL = SL+NL-1
	  EB = SB+NB-1
	  DO LINE=SL,EL
	      DO BAND=SB,EB
		  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     2			      'BAND',BAND,'LINE',LINE, ' ')
		  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ELSE
C					ORG .EQ. 'BIP'
	  EL = SL+NL-1
	  ES = SS+NS-1
	  DO LINE=SL,EL
	      DO SAMP=SS,ES
	 	  CALL XVREAD(INPUT,BUF,STAT,'BAND',SB,'NBANDS',NB,
     2			      'SAMP',SAMP,'LINE',LINE, ' ')
	    	  CALL XVWRIT(OUTPUT,BUF,STAT, ' ')
	      ENDDO
	  ENDDO
      ENDIF

      CALL XVCLOSE(INPUT,STAT, ' ')
      CALL XVCLOSE(OUTPUT,STAT, ' ')
      RETURN
      END
