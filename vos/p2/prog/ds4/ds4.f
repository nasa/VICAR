	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	INTEGER*2 IBUF(4000,6),TABLE(256,6)
C
	INTEGER SL,SS,NL,NS
	INTEGER PTR,PTR1,PTR2,PTR3,PTR4
	INTEGER PBUF(256,6),CDFBUF(256)
	INTEGER RSEN,IWTS(41)
	LOGICAL QAVERAGE,XVPTST
C
	COMMON IBUF,TABLE,SL,SS,NL,NS,PTR1,PTR2,PTR3,PTR4,PBUF,CDFBUF,
     &         LINE1,LINE2,LINE3,LINE4,RSEN

	CALL XVMESSAGE('DS4 version March 15, 1998',' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN,' ')

	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'OP','WRITE','U_NL',NL,'U_NS',NS,'U_FORMAT','HALF',' ')

	CALL ZIA(TABLE(1,1),768)
	CALL ZIA(PBUF(1,1),1536)
	CALL ZIA(CDFBUF(1),256)

	CALL XVPARM('RSEN',RSEN,ICNT,IDEF,' ')
	CALL XVPARM('GROUP',NSETSG,ICNT,IDEF,' ')
	QAVERAGE = XVPTST('AVERAGE')
	CALL XVPARM('FILTER',IWTS,NWTS,IDEF,' ')
	NSETS = (NL-12)/6
	NGT = NSETS-NSETSG+1

	LINE1 = SL
	LINE2 = LINE1+MOD((RSEN+3),6)
	LINE3 = LINE2
	LINE4 = LINE2

C LINE2 is the starting line of the first sensor
C set such that RSEN becomes sensor #3 within the set
C
C      ....Process the first group of sensor sets
	DO I=1,NSETSG
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE3,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE3 = LINE3+1
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)+1	!Compute histogram PBUF
		END DO
	    END DO
	END DO

C       ....Generate the CDF for RSEN
	IF (QAVERAGE) THEN
	    CDFBUF(1) = (PBUF(1,1)+PBUF(1,2)+PBUF(1,3)+PBUF(1,4)+
     &			 PBUF(1,5)+PBUF(1,6))/6
	    DO I=2,256
		CDFBUF(I) = CDFBUF(I-1)+(PBUF(I,1)+PBUF(I,2)+PBUF(I,3)+
     &			    PBUF(I,4)+PBUF(I,5)+PBUF(I,6))/6
	    END DO
	    CALL PRCDF(CDFBUF(1),NSETSG,NS,IWTS,NWTS)
	ELSE
	    CDFBUF(1)=PBUF(1,3)
	    DO I=2,256
		CDFBUF(I) = PBUF(I,3)+CDFBUF(I-1)
	    END DO
	END IF
C       ....Match cdf's and generate lookup tables
	CALL MATCH
C       ....Transform all lines up through the middle of the first group
	NLT = MOD((RSEN+3),6)+((NSETSG/2)+1)*6
	DO I=1,NLT
	    CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			'SAMP',SS,'NSAMPS',NS,' ')
	    PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
	    CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
	    CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
	    LINE1 = LINE1+1
	END DO
C						  process the rest of the groups
	NGR = NSETS-NSETSG
C
	DO I=1,NGR
C						 add in the data for the end set
C						 in the current group
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE3,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE3 = LINE3+1
	    END DO
	    DO J=1,6
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)+1
		END DO
	    END DO
C						 subtract out the data for the
C						 first set in the previous group
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE4,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE4 = LINE4+1
	    END DO
	    DO J=1,6
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)-1
		END DO
	    END DO
C						      recompute the cdf for RSEN
	    IF (QAVERAGE) THEN
		CDFBUF(1) = (PBUF(1,1)+PBUF(1,2)+PBUF(1,3)+PBUF(1,4)+
     &			     PBUF(1,5)+PBUF(1,6))/6
		DO J=2,256
		    CDFBUF(J) = CDFBUF(J-1)+(PBUF(J,1)+PBUF(J,2)+
     &			      PBUF(J,3)+PBUF(J,4)+PBUF(J,5)+PBUF(J,6))/6
		END DO
	        CALL PRCDF(CDFBUF(1),NSETSG,NS,IWTS,NWTS)
	    ELSE
		CDFBUF(1)=PBUF(1,3)
		DO J=2,256
		    CDFBUF(J) = PBUF(J,3)+CDFBUF(J-1)
		END DO
	    END IF
C					  match CDF's and generate lookup tables
	    CALL MATCH
C							transform the middle set
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
		CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
		CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
		LINE1 = LINE1+1
	    END DO
	END DO
C						   transform the remaining lines
	NLR = SL+NL-LINE1
C
	DO I=1,NLR
	    CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			'SAMP',SS,'NSAMPS',NS,' ')
	    PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
	    CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
	    CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
	    LINE1 = LINE1+1
	END DO
C
	RETURN
	END
C**********************************************************************
C
      SUBROUTINE MATCH
      INTEGER*2 IBUF(4000,6),TABLE(256,6)
      INTEGER SL,SS,NL,NS
      INTEGER PTR1,PTR2,PTR3,PTR4
      INTEGER PBUF(256,6),CDFBUF(256)
      INTEGER RSEN
      LOGICAL EXIT,INCL
      COMMON IBUF,TABLE,SL,SS,NL,NS,PTR1,PTR2,PTR3,PTR4,PBUF,CDFBUF,
     &         LINE1,LINE2,LINE3,LINE4,RSEN
C
C     SCAN CDFBUF TO FIND THE FIRST NONZERO ENTRY
C
              M=1
    5         IF(CDFBUF(M).NE.0) GO TO 10
              M=M+1
              GO TO 5
   10         CONTINUE
              MREF=M
C
              DO 200 I=1,6
C
C     scan pbuf to find the first nonzero entry
C     for the sensor being processed
              L=1
   15         IF(PBUF(L,I).NE.0) GO TO 20
              L=L+1
              GO TO 15
   20         CONTINUE
C
              L=L-1
              IF(L.EQ.0) GO TO 40
              DO 30 J=1,L
                  TABLE(J,I)=0
   30         CONTINUE
   40         CONTINUE
              L=L+1
C
C     MATCH THE CDF'S
C
              M=MREF
              ISUM=PBUF(L,I)
	      EXIT = .FALSE.
	      DO WHILE (.NOT.EXIT)
                  IF(ISUM.GE.CDFBUF(M)) THEN
		      INCL = .TRUE.
                      IF(ISUM.NE.CDFBUF(M)) THEN
	      		  M = M + 1
                          DO WHILE (ISUM.GT.CDFBUF(M))
                              M=M+1
     	  	          END DO
		          IF(ISUM.NE.CDFBUF(M)) THEN
		  	      IDH=ISUM-CDFBUF(M-1)
		              IDL=CDFBUF(M)-ISUM
		              IDD=IDH-IDL
		              IF(IDD.GE.0) TABLE(L,I)=M-1
		              IF(IDD.LT.0) TABLE(L,I)=M-2
	  	          ELSE
		              TABLE(L,I)=M-1
		  	  END IF
		      ELSE
                          TABLE(L,I)=M-1
		      END IF
                  ELSE IF (M.EQ.MREF) THEN
		      INCL = .TRUE.
		      TABLE(L,I)=M-1
                  ELSE
		      INCL = .FALSE.
	              M=M-1
                  END IF
		  IF (INCL) THEN
		      L=L+1
		      IF (L.GT.256) THEN
			  TABLE(1,I) = 0
			  EXIT = .TRUE.
		      ELSE
			  ISUM = ISUM + PBUF(L,I)
		      END IF
		  END IF
              END DO
C
  200         CONTINUE
C
              RETURN
              END
C**********************************************************************
C
      SUBROUTINE PRCDF(CDFBUF,NSETSG,NS,IWTS,NWTS)
      INTEGER CDFBUF(256),WBUF(300),IWTS(41)
      INTEGER NSETSG,NS,NWTS
      INTEGER PTR5,PTR6,SUMWTS
C
C     INSERT CDFBUF INTO WBUF STARTING AT WBUF(21)
C
              CALL ZIA(WBUF(1),300)
              CALL MVE(1,1024,CDFBUF(1),WBUF(21),1,1)
C
C     MIRROR ABOUT EACH END
C
              DO 10 I=1,20
                  J1=20+I
                  J2=21-I
                  K1=277-I
                  K2=276+I
                  WBUF(J2)=WBUF(J1)
                  WBUF(K2)=WBUF(K1)
   10         CONTINUE
C
              ISUM=0
              PTR5=21
              PTR6=PTR5-NWTS/2
              SUMWTS=0
C
C     CALCULATE SUMWTS
C
              DO 15 I=1,NWTS
                  SUMWTS=SUMWTS+IWTS(I)
   15         CONTINUE
C
C     SCALE CDFBUF PROPERLY
C
              RMAX=CDFBUF(256)
              RAMAX=NS*NSETSG
              FACTOR=RAMAX/RMAX
C
              DO 20 I=1,300
                  WBUF(I)=WBUF(I)*FACTOR+0.5
   20         CONTINUE
C
C     APPLY FILTER
C
              DO 40 I=21,276
                  DO 30 J=1,NWTS
                      ISUM=ISUM+IWTS(J)*WBUF(PTR6)
                      PTR6=PTR6+1
   30             CONTINUE
                  CDFBUF(I-20)=ISUM/SUMWTS
                  PTR5=PTR5+1
                  PTR6=PTR5-NWTS/2
                  ISUM=0
   40         CONTINUE
              IF(CDFBUF(256).NE.NS*NSETSG) CDFBUF(256)=NS*NSETSG
C
              RETURN
              END
C**********************************************************************
	      SUBROUTINE LOOKUP(IBUF,TABLE,NS)
	      INTEGER*2 TABLE(0:*),IBUF(1:*)
	      DO I=1,NS
	      	  IBUF(I) = TABLE(IBUF(I))
	      END DO
	      RETURN
	      END
