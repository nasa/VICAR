      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     
C  REVISION HISTORY
C    05-96  REA  PORTED TO UNIX
C    07-85  JHR  CONVERTED TO VICAR2
C    11-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    11-83  SP   MODIFIED SO THAT RDCHEK AND WRCHEK USED FOR I/O ERROR MESSAGES
C    11-83  SP   CORRECTED SPELLING OF QVAL TO QVALUE IN STACKA CALL.
C    11-83  SP   CORRECTED LENGTHS BY A FACTOR OF TWO IN FIVE ITLA CALLS.
C    11-83  SP   CHANGED TO SKIP CHECK OF   MAX .GT. IHIST(OUT(J))  IF
C                OUT(J) IS 0.
C    12-83  SP   ADDED CHECK FOR WINDOW SIZE BEING ODD.
C     2-82  REA  NEW VERSION - FASTER, WINDOW SIZE RESTRICTION REMOVED
C     1-80  SZF  INITIAL RELEASE
C
C  PURPOSE: ELIMINATE ISOLATED PIXELS IN A CLASSIFIED IMAGE.
C  USER PARAMETERS:
C
C  WINDOW=N      THE ODD INTEGER N SPECIFIES A WINDOW SIZE.  DEFAULT IS 3
C  THRESHLD=M    IF THE MAJORITY CLASS IN A WINDOW IS LARGER THAN M
C                THEN THE CENTER PIXEL MAY BE CHANGED TO THE MAJORITY
C                CLASS
C  NCLASS,K      THE INTEGER K SPECIFIES THE NUMBER OF CLASSES IN THE
C                IMAGE.  THE CLASSES MUST BE NUMBERED FROM 1 TO K.
C  REPLACE       THIS KEYWORD CAUSES ALL 0 PIXELS TO BE REPLACED BY THE
C                VALUE IN THE WINDOW ABOUT THAT PIXEL.
C  VALUE,Q       THIS CHECKS FOR REPLACEMENT  ONLY IF THE CENTRAL DN
C                IS EQUAL TO Q
C
      EXTERNAL WORK
C
      INTEGER*4 OUNIT,SL,SS,LEN,LIMIT
      LOGICAL*4 XVPTST
      LOGICAL QZERO,QVALUE
      CHARACTER*8 FORMAT
C							     open input data set
      CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(IUNIT,ISTAT,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
C						       get data format and check
      CALL XVGET(IUNIT,ISTAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE') THEN
	 CALL XVMESSAGE('SIMPLIFY accepts BYTE data only',' ')
	 CALL ABEND
      ENDIF
C
C						  get size information and check
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF (SL+NL-1 .GT. NLI) THEN
	 CALL XVMESSAGE(
     +		' number of lines requested exceeds input size',' ')
	 CALL ABEND
      ENDIF
      IF (SS+NS-1 .GT. NSI) THEN
	 CALL XVMESSAGE(
     +		  ' number of samples requested exceeds input size',' ')
	 CALL ABEND
      ENDIF
C
C							    open output data set
      CALL XVUNIT(OUNIT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUNIT,ISTAT,'OP','WRITE','U_FORMAT','HALF',' ')
C
C							      process parameters
      CALL XVPARM('WINDOW',IWIND,ICOUNT,IDEF,1)
      IF (MOD(IWIND,2) .EQ. 0)  THEN
	 CALL XVMESSAGE(' error:window size not an odd number',' ')
	 CALL ABEND
      ENDIF
C
      CALL XVPARM('THRESHLD',LIMIT,ICOUNT,IDEF,1)
      CALL XVPARM('NCLASS',NCLASS,ICOUNT,IDEF,1)
      QZERO = XVPTST('REPLACE')
      CALL XVPARM('VALUE',IVAL,ICOUNT,IDEF,1)
      QVALUE = IDEF.EQ.0
C
      IREC = SL
C
C  Call special library subroutine STACKA to allocate the necessary buffers
C  and to call subroutine WORK. (We allocate IWIND line buffers together in a
C  two-dimensional array with LEN*IWIND halfwords.)
C
      LEN = NS+IWIND
      I = 2*LEN*IWIND
      J = 2*NS
      K = 1024
      CALL STACKA(19, WORK, 3, I, J, K, IUNIT, OUNIT, IREC, SS, NL, NS,
     &            NCLASS, LIMIT, IWIND, IVAL, LEN, QZERO, QVALUE)
C
C								 close data sets
      CALL XVCLOSE(IUNIT,ISTAT,' ')
      CALL XVCLOSE(OUNIT,ISTAT,' ')
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WORK(IN,II,OUT,JJ,IHIST,KK,IUNIT,OUNIT,
     &                IREC, SS, NL, NS, NCLASS, LIMIT,
     &                IWIND, IVAL, LEN, QZERO, QVALUE)
C
C  INPUT PARAMETERS 
C      IN(I,IWIND)       - LINE BUFFERS FOR IWIND LINES WHERE IWIND IS THE
C       array              WINDOW SIZE.  THE PIXEL INDEX GOES FROM 1 TO LEN.
C                          THE DATA FROM A LINE OF THE INPUT IMAGE
C                          BEGINS WITH AT IN(2+IWIND/2,L) TO PROVIDE A BORDER
C                          OF ZERO DNS AROUND THE IMAGE.
C      II                - NUMBER OF BYTES ALLOCATED BY STACKA FOR IN.
C      OUT(J) array      - line buffer for output image.
C      JJ                - NUMBER OF BYTES ALLOCATED BY STACKA FOR OUT.
C      IHIST(NUM) array  - BUFFER FOR HISTOGRAM.
C      KK                - NUMBER OF BYTES ALLOCATED BY STACKA FOR IHIST.
C      IREC              - RECORD NUMBER IN INPUT FILE OF STARTING LINE.
C      SS               - STARTING SAMPLE.
C      NL                - NUMBER OF LINES.
C      NS                - NUMBER OF SAMPLES.
C      NCLASS            - VALUE OF NCLASS PARAMETER.
C      LIMIT             - VALUE OF THRESHLD PARAMETER.
C      IWIND             - VALUE OF WINDOW SIZE PARAMETER.
C      IVAL              - VALUE OF VALUE PARAMETER.
C      LEN               - HORIZONTAL DIMENSION OF IN ARRAY.
C      QZERO             - TRUE IF REPLACE PARAMETER SELECTED.
C      QVALUE            - TRUE IF VALUE PARAMETER SELECTED.
C
      CHARACTER*80 MSG
      INTEGER*4 OUNIT,SS
      INTEGER*4 IHIST(256)
      INTEGER*2 IN(LEN,IWIND), OUT(NS)
      LOGICAL   QZERO, QVALUE
C						  set up keys and looping limits
      N = 0
      KEYL = 0
      M = IWIND/2
      M1 = M+1
      M2 = M+2
      NEND = NL-M
      NREPLACED = 0
C
C     Read in the lines needed to fill the initial window.  The top part of
C     the window is left with zeros since it is outside the boundary of the
C     input image.
C
      CALL ITLA(0,IN,2*LEN*IWIND)
C
      DO I=1,M
          N = N+1
          CALL XVREAD(IUNIT,IN(M2,N),ISTAT,'LINE',IREC,'SAMP',SS,
     +                'NSAMPS',NS,' ')
          IREC = IREC + 1
          CALL ITLA(0,IN(1,N),M1*2)
          CALL ITLA(0,IN(NS+M2,N),M*2)
      END DO
C
C     Main loop, looping through each output line.  Since the algorithm does
C     not depend on the geometry of pixels in the window, we do not have to
C     keep the lines of the window in any order.  We just read the next line
C     into the oldest buffer.  KEYL is the line index for the in array of the
C     center line.
C
      DO I=1,NL
          N = N+1
          IF(N.GT.IWIND) N=1
          KEYL = KEYL+1
          IF(KEYL.GT.IWIND) KEYL=1
C
C	      read in a line of input; if at the image bottom, enter a line of 0
C
          IF(I.LE.NEND) THEN
              CALL XVREAD(IUNIT,IN(M2,N),ISTAT,'LINE',IREC,'SAMP',SS,
     +                    'NSAMPS',NS,' ')
              IREC = IREC + 1
              CALL ITLA(0,IN(1,N),M1*2)
              CALL ITLA(0,IN(NS+M2,N),M*2)
          ELSE
              CALL ITLA(0,IN(1,N),LEN*2)
          END IF
C					 set up the initial window for this line
          KEY2 = IWIND
          KEY3 = 0
          CALL ZIA(IHIST,256)
          DO J=1,IWIND
              DO K=M2,KEY2
                  NUM = IN(K,J)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)+1
              END DO
          END DO
C					     loop through for each output sample
          CALL MVE(2,NS,IN(M2,KEYL),OUT,1,1)
          DO J=1,NS
              KEY2 = KEY2+1
              KEY3 = KEY3+1
C
C     Update the histogram:  when we move the window to the right, we lose
C     the leftmost column of the previous window and gain a column on the 
C     right.
C
              DO K=1,IWIND
                  NUM = IN(KEY2,K)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)+1
                  NUM = IN(KEY3,K)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)-1
              END DO
C							    test for replacement
              IF (OUT(J).EQ.IVAL .OR. .NOT.QVALUE) THEN
                  CALL MINMAX(4,NCLASS,IHIST,MIN,MAX,IMIN,IMAX)
                  IF(QZERO.AND.OUT(J).EQ.0) THEN
                      IF (MAX.NE.0) THEN
                          OUT(J) = IMAX
                          NREPLACED = NREPLACED + 1
                      END IF
                  ELSE
                      IF (MAX.GE.LIMIT .AND.
     +                    (MAX.GT.IHIST(OUT(J)) .OR. OUT(J).EQ.0)) THEN
                          OUT(J) = IMAX
                          NREPLACED = NREPLACED + 1
                      END IF
                  END IF
              END IF
          END DO
          CALL XVWRIT(OUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
      END DO
      WRITE (MSG,100) NREPLACED
  100 FORMAT(I10,' values replaced')
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
