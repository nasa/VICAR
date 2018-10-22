      INCLUDE 'VICMAIN_FOR'
C     VICAR PROGRAM "zfill"      S.Z. FRIEDMAN    MARCH 1983
C     MODIFIED FOR VAX CONVERSION BY ASM, 6 SEPT 1983
C     MODIFIED TO VICAR2 AND SPARSE/DENSE ALGORITHMS ADDED BY REA, 20 MAY 1986
C     MODIFIED TO BE ABLE TO BE ALSO RUN ON A BIL FILE WITH ANY NUMBER OF
C     BANDS, 6 JULY 1987
C     3-94   CRI   MSTP (S/W CONVERSION) VICAR PORTING
C     1/24/2001 AXC ADDED SUPPORT TO PRESERVE BINARY LABELS.
C**********************************************************************
      SUBROUTINE MAIN44
C
C  PROGRAM "zfill" USES A WINDOW OF NLW LINES BY NSW SAMPLES TO
C  FILL IN VOID AREAS OF AN IMAGE.  VOID AREAS ARE ASSUMED
C  TO BE ZERO DN, BUT CAN BE RESPECIFIED BY PARAMETER.  VOIDS ARE
C  FILLED WITH THE MEAN VALUE OF ALL 'NON-VOID DN' PIXELS IN
C  THE WINDOW.  A MASK SHOWING FILLED PIXELS CAN BE
C  GENERATED UPON REQUEST
C
C
C  TAE STATEMENT FORM:
C     zfill IN OUT SIZE=(SL,SS,NL,NS) PARAMETERS       OR
C     zfill IN (OUT,MASK) SIZE=(SL,SS,NL,NS) PARAMETERS
C
C  PARAMETERS:
C
C     NLW=n            NUMBER OF LINES IN WINDOW (DEFAULT=3)
C
C     NSW=m            NUMBER OF SAMPLES IN WINDOW (DEFAULT=3)
C
C     REPLACE=n        DEFINES DN(N) TO BE THE 'VOID' DN.  ALL PIXELS
C                      OF VALUE N WILL BE REPLACED BY THE MEAN OF ALL
C                      'NON-N' VALUES IN THE WINDOW.  (DEFAULT=0)
C
C     EXCLUDE=n        DN N WILL NOT BE USED IN THE INTERPOLATION
C                      OF VALUE FOR REPLACEMENT DN.  ITS FUNCTION
C                      IS SIMILAR TO AN AREA MASK OR BARRIER.
C                      (NO DEFAULT)
C
C     DENSE	       INVOKES THE ALGORITHM FOR USE WHEN THE INPUT
C		       IS MOSTLY FULL.
C
	EXTERNAL MAIN
	LOGICAL XVPTST
	CHARACTER*4 FORMAT
	CHARACTER*3 ORG
        INTEGER     BUFFER_SIZE
        PARAMETER   (BUFFER_SIZE=200000)
        COMMON /C2/ BUF
        INTEGER*2   BUF(BUFFER_SIZE)
	COMMON /C1/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK,NB,QBINARY,
     +              NLBI,NSBI 
	LOGICAL QDENSE/.FALSE./,QMASK/.FALSE./,QBINARY
C							 call for parameters
        CALL IFMESSAGE('ZFILL version 2016-02-12')
        CALL XVEACTION('SA',' ')
	CALL XVPARM('NLW',NLW,ICNT,IDEF,1)
	CALL XVPARM('NSW',NSW,ICNT,IDEF,1)
	CALL XVPARM('REPLACE',IREPLACE,ICNT,IDEF,1)
	CALL XVPARM('EXCLUDE',IEXCLUDE,ICNT,IDEF,1)
	QDENSE = XVPTST('DENSE')
        QBINARY = XVPTST('BINARY')
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT, ' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')

C                                                     call for binary label
        IF (QBINARY) THEN 
            CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF','OPEN_ACT',
     +                  ' ','IO_ACT','SA','COND','BINARY', ' ')
        ELSE 
            CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF', ' ')
        ENDIF

C end added statement

	CALL XVGET(INUNIT,ISTAT,'NLB',NLBI,'NBB',NSBI,
     +             'NB',NB,'ORG',ORG,'FORMAT',FORMAT,' ')
	IF (NB.GT.1 .AND. ORG.NE.'BIL') THEN
	   CALL XVMESSAGE('ONLY ONE BAND IS ALLOWED FOR A BSQ FILE',' ')
	   CALL ABEND
	ENDIF
        IF (ORG.EQ.'BIP') THEN
           CALL XVMESSAGE('CANNOT BE RUN ON A BIP FILE',' ')
           CALL ABEND
        ENDIF
	IF (FORMAT.NE.'BYTE' .AND. FORMAT.NE.'HALF') THEN
	   CALL XVMESSAGE('MUST BE BYTE OR HALFWORD',' ')
	   CALL ABEND
	ENDIF

	CALL XVPCNT('OUT',NOUT)

	IF (NOUT.EQ.2) THEN
	    IF (NB.GT.1) THEN
	       CALL XVMESSAGE('MASK CAN ONLY BE CREATED FOR A BSQ FILE',
     +                        ' ')
	       CALL XVMESSAGE('NO MASK WILL BE CREATED',' ')
	    ELSE
	       QMASK = .TRUE.
	       CALL XVUNIT(MASKUNIT,'OUT',2,ISTAT,' ')
	       CALL XVOPEN(MASKUNIT,ISTAT,'U_FORMAT','BYTE','O_FORMAT',
     +                'BYTE','OP','WRITE',' ')
	    ENDIF
	END IF

C					     check parameters, adjust if needed
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (ISL+NL-1 .GT. NLIN) NL=NLIN-ISL+1
	IF (ISS+NS-1 .GT. NSIN) NS=NSIN-ISS+1

	CALL 
     +   XVMESSAGE('INPUT IMAGE SIZE AND OUTPUT # LINES AND SAMPLE', 
     +   ' ')
        IF (NL.LE.0 .OR. NS.LE.0) THEN
	    CALL XVMESSAGE('SIZE FIELD ERROR',' ')
	    CALL ABEND
	END IF

        IF ( (( ISL .NE. 1 ) .OR. ( ISS .NE. 1 ) .OR. 
     *       ( NL .NE. NLIN) .OR. ( NS .NE. NSIN))
     *       .AND. QBINARY ) THEN
         NLBI = 0
         NSBI = 0
         QBINARY = .FALSE.
         CALL XVMESSAGE('Output is a sub-window of input, so', ' ')
         CALL XVMESSAGE('binary parts can not be copied.', ' ')
         CALL XVMESSAGE('Continuing with copy of non-binary parts.',' ')
         ENDIF

C                         copy binary label to the output image if requested
        IF (QBINARY) THEN
            CALL XVOPEN(IOUTUNIT,ISTAT,'U_FORMAT','HALF','OP','WRITE',
     +		        'U_NL',NL,'U_NS',NS,'U_NB',NB,'U_ORG',ORG,
     +		        'OPEN_ACT','SA','IO_ACT','SA','COND',
     +                  'BINARY','U_NBB',NSBI,'U_NLB',NLBI, ' ')
        ELSE 
	    CALL XVOPEN(IOUTUNIT,ISTAT,'U_FORMAT','HALF','OP','WRITE',
     +          ' ')
        ENDIF

C Copy binary labels if requested for BSQ image format    

        IF (QBINARY .AND. NLBI .GT.0 .AND. ORG .EQ. 'BSQ') THEN
C                                             read in binary header
            DO REC = 1,NLBI
               CALL XVREAD(INUNIT,BUF,ISTAT, ' ')
               CALL XVWRIT(IOUTUNIT,BUF,ISTAT,' ')
            ENDDO 
        ENDIF

        NLW = 2*(NLW/2) + 1
	NSW = 2*(NSW/2) + 1
C		           determine buffer sizes and call main through stacka
	N1 = NS+NSW-1
	N2 = NLW*NB
	INSIZE = 2*N1*N2
	IOUTSIZE = 2*NS
	IF (QMASK) THEN
	    MSIZE = NS
	ELSE
	    MSIZE = 4
	END IF
	CALL STACKA(8,MAIN,3,MSIZE,INSIZE,IOUTSIZE,N1,N2)
	 	

        CALL XVCLOSE(INUNIT,ISTAT, ' ')
        CALL XVCLOSE(IOUTUNIT,ISTAT, ' ')
        RETURN
        END
C
C**********************************************************************
C
	SUBROUTINE MAIN(MBUF,MSIZE,INBUF,INSIZE,OUTBUF,IOUTSIZE,N1,N2)
C
C  VICAR PROGRAM ZFILL      S.Z. FRIEDMAN    MARCH 1983
C
C  SUBROUTINE MAIN HANDLES ALL I/O, CALLS MEAN ANALYSIS ROUTINE
C
C
C     MBUF    OPTIONAL ARRAY FOR MASK OUTPUT DS FOR LINE N
C     INBUF   INPUT BUFFER
C     OUTBUF  OUTPUT ARRAY FOR LINE N
C
	INTEGER*2 INBUF(N1,N2), OUTBUF(NS)
	INTEGER*4 BN, ISUM(2000), NPTS(2000)
        CHARACTER*80 LBUF
	BYTE      MBUF(MSIZE)
        INTEGER   BUFFER_SIZE
        PARAMETER (BUFFER_SIZE=200000)
        COMMON /C2/ BUF
        INTEGER*2   BUF(BUFFER_SIZE)
	COMMON /C1/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK,NB,QBINARY,
     +              NLBI,NSBI
	LOGICAL   QDENSE,QMASK,QBINARY
C
C						  check if core is available
C
	IF (IOUTSIZE .LT. 2*NS) THEN
	    CALL XVMESSAGE('INSUFFICIENT MEMORY FOR STACKA OPERATION',
     +                     ' ')
	    CALL ABEND
	END IF
C
C							label processing
C
        WRITE(LBUF,1000) NLW, NSW, IREPLACE
1000    FORMAT('ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5)
	IF (IEXCLUDE.NE.-32768) THEN
            WRITE(LBUF,1010) NLW, NSW, IREPLACE, IEXCLUDE
1010        FORMAT('ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5,
     +              ', EXCLUDE =',I5)
	    LENGTH = 59
	ELSE
	    LENGTH = 43
	END IF
	CALL XLADD(IOUTUNIT,'HISTORY','PARMS',LBUF,ISTAT,'FORMAT',
     +		   'STRING','ULEN',LENGTH,' ')
        CALL XVMESSAGE(LBUF,' ')
C	CALL QPRINT(LBUF,LENGTH)
C
	IF(QMASK) THEN
	    CALL XLADD(MASKUNIT,'HISTORY','PARMS',LBUF,ISTAT,'FORMAT',
     +		       'STRING','ULEN',LENGTH,' ')
	    CALL XLADD(MASKUNIT,'HISTORY','MASK',
     +		       ' 0=CHANGED   255=NO CHANGE',ISTAT,'FORMAT',
     +		       'STRING',' ')
	END IF

C
C					Read in initial lines; set pointers
C
	NLW2 = NLW/2
	NSW2 = NSW/2
	LINE1 = MAX(1,ISL-NLW2)				! First line to be read
	LINEN = MIN(NLIN,ISL+NLW2)			! Last line to be read
	ISAMP1 = MAX(1,ISS-NSW2)			! First sample read
	ISAMPN = MIN(NSIN,ISS+NS-1+NSW2)		! Last sample read
	NSAMPS = ISAMPN-ISAMP1+1			! Number of samps read
	LOCL = MAX(1,NLW2-ISL+2)		! Loc of 1st line in INBUF
	LOCS = MAX(1,NSW2-ISS+2)		! Loc of 1st samp in INBUF
C
C					Initialize all values in INBUF to the
C					excluded value
	DO I=1,N2
	    DO J=1,N1
		INBUF(J,I) = IEXCLUDE
	    END DO
	END DO
C						Read input to fill INBUF
C
C   THIS LOOP IS CORRECT ONLY IF NB=1 WHEN ORG IS BSQ
C
        DO I=LINE1,LINEN
	   DO BN = 1,NB
	      NLWBN = NLW*(BN-1)
              IF (QBINARY) THEN 
                  CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),ISTAT,
     +                        'LINE',NLBI+I,'SAMP',NSBI+ISAMP1,
     +                        'NSAMPS',NSAMPS,'BAND',BN,' ')
              ELSE
                  CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),ISTAT,
     +                        'LINE',I,'SAMP',ISAMP1,'NSAMPS', 
     +                        NSAMPS,'BAND',BN,' ')
              ENDIF
           ENDDO
	   LOCL = LOCL+1
	ENDDO
	LINELOC = NLW2
	LINEN = LINEN+1
	LOCL = 1
	IF (QDENSE) THEN
C
C **********************************************************    DENSE ALGORITHM
C
	    DO I=1,NL
		IF (QMASK) CALL ITLA(255,MBUF,NS)
		LINELOC = LINELOC+1
		IF (LINELOC.GT.NLW) LINELOC=1
C							      check each sample
		DO BN = 1,NB
	    	   ISAMPLOC = NSW2
		   NLWBN = NLW*(BN-1)
	    	   DO J=1,NS
		      ISAMPLOC = ISAMPLOC+1
		      IF (INBUF(ISAMPLOC,NLWBN+LINELOC).EQ.IREPLACE) THEN
C
C						      compute replacement value
		         ISUM(BN) = 0
		         NPTS(BN) = 0
		         DO K=1,NLW
			    DO L=ISAMPLOC-NSW2,ISAMPLOC+NSW2
			       IF(INBUF(L,NLWBN+K).NE.IREPLACE .AND.
     +			       	  INBUF(L,NLWBN+K).NE.IEXCLUDE)      THEN
				  ISUM(BN) = ISUM(BN)+INBUF(L,NLWBN+K)
				  NPTS(BN) = NPTS(BN)+1
			       END IF
			    END DO
		    	 END DO
		    	 IF (NPTS(BN).NE.0) THEN
			    OUTBUF(J) = FLOAT(ISUM(BN))/FLOAT(NPTS(BN))+0.5
			    IF (QMASK) CALL ITLA(0,MBUF(J),1)
		    	 ELSE					! nothing in
			    OUTBUF(J) = IREPLACE		! window, leave
		    	 END IF					! as is
		      ELSE
		    	 OUTBUF(J) = INBUF(ISAMPLOC,NLWBN+LINELOC) ! replacement
		      END IF					   ! not needed
	    	   END DO
C							     write out results
                   IF (QBINARY) THEN
C                                                      read in binary prefix
                       CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',NLBI+I,
     +                             'SAMP',1,'NSAMPS',NSBI,'BAND',BN,' ')

C                                                      write out binary prefix 
                       CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'LINE',NLBI+I,
     +                             'NSAMPS',NSBI+NS,'BAND',BN,' ')

C                                                      write out pixel data
	               CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,'LINE',NLBI+I,  
     +                             'SAMP',NSBI+1,'NSAMPS',NSAMPS,
     +                             'BAND',BN,' ')
                   ELSE
	    	       CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')     !write out line
                   ENDIF
	    	   IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')
C								! read new line
	    	   IF (LINEN.LE.NLIN) THEN
                       IF (QBINARY) THEN 
                           CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                 ISTAT,'LINE',NLBI+LINEN, 
     +                                 'SAMP',NSBI+ISAMP1,'NSAMPS', 
     +                                 NSAMPS,'BAND',BN,' ')
                        ELSE
         		   CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL),
     +                                 ISTAT,'LINE',LINEN,'SAMP',
     +                                 ISAMP1,'NSAMPS',NSAMPS,
     +                                 'BAND',BN,' ')
                       ENDIF
 	            ELSE
		      DO J=1,N1
		    	INBUF(J,NLWBN+LOCL) = IEXCLUDE
		      END DO
	    	    END IF
		ENDDO
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO
	ELSE
C
C*********************************************************** SPARSE ALGORITHM
C
	    DO II=1,NL
C						    initialize ISUM and NPTS
  	       DO BN = 1,NB

		  NLWBN = NLW*(BN-1)
		  ISUM(BN) = 0
		  NPTS(BN) = 0
		  DO I=1,NLW
		     DO J=1,NSW
		        IF (INBUF(J,NLWBN+I).NE.IREPLACE .AND.
     +			    INBUF(J,NLWBN+I).NE.IEXCLUDE) THEN
			    ISUM(BN) = ISUM(BN)+INBUF(J,NLWBN+I)
			    NPTS(BN) = NPTS(BN)+1
			END IF
		     END DO
		  END DO
		  ISAMPL = 1
		  ISAMPR = NSW+1
C							set pointers
		  IF (QMASK) CALL ITLA(255,MBUF,NS)
		  IF (BN.EQ.1)  LINELOC = LINELOC+1
		  IF (LINELOC.GT.NLW) LINELOC=1
	    	  ISAMPLOC = NSW2
C							check each sample
	    	  DO J=1,NS
		     ISAMPLOC = ISAMPLOC+1
		     IF (INBUF(ISAMPLOC,NLWBN+LINELOC).EQ.IREPLACE) THEN
		    	IF (NPTS(BN).NE.0) THEN		   ! compute new value
			    OUTBUF(J) = FLOAT(ISUM(BN))/FLOAT(NPTS(BN))+0.5
			    IF (QMASK) CALL ITLA(0,MBUF(J),1)
		    	ELSE
			    OUTBUF(J) = IREPLACE	   ! nothing in window;
		    	END IF				   ! leave as is
		     ELSE
		    	OUTBUF(J) = INBUF(ISAMPLOC,NLWBN+LINELOC) ! don't
		     END IF					  ! replace
C							update ISUM and NPTS
		     IF (J.NE.NS) THEN
			DO K=1,NLW
			    IF(INBUF(ISAMPL,NLWBN+K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPL,NLWBN+K).NE.IEXCLUDE)	THEN
				ISUM(BN) = ISUM(BN)-INBUF(ISAMPL,NLWBN+K)
				NPTS(BN) = NPTS(BN)-1
			    END IF
			    IF(INBUF(ISAMPR,NLWBN+K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPR,NLWBN+K).NE.IEXCLUDE)	THEN
				ISUM(BN) = ISUM(BN)+INBUF(ISAMPR,NLWBN+K)
				NPTS(BN) = NPTS(BN)+1
			    END IF
			END DO
			ISAMPL = ISAMPL+1
			ISAMPR = ISAMPR+1
		     END IF
                  
	    	  END DO
C							     write out results
                  IF (QBINARY) THEN
C                                                       read in binary prefix
                      CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',NLBI+II,
     +                            'SAMP',1,'NSAMPS',NSBI,'BAND',BN,' ')

C                                                       write out binary prefix 
                      CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'LINE',NLBI+II,
     +                            'NSAMPS',NSBI+NS,'BAND',BN,' ')

	              CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,'LINE',NLBI+II, 
     +                            'SAMP',NSBI+1,'NSAMPS',NSAMPS,
     +                            'BAND',BN,' ')
                  ELSE
                      CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')
                  ENDIF
       	          IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')

C							     read new line into
C							     INBUF
	    	  IF (LINEN.LE.NLIN) THEN
                      IF (QBINARY) THEN 
                          CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                ISTAT,'LINE',NLBI+LINEN, 
     +                                'SAMP',NSBI+ISAMP1,'NSAMPS',
     +                                NSAMPS,'BAND',BN,' ')
                      ELSE
                          CALL XVREAD(INUNIT,INBUF(LOCS,NLWBN+LOCL), 
     +                                ISTAT,'LINE',LINEN,'SAMP', 
     +                                ISAMP1,'NSAMPS',NSAMPS, 
     +                                'BAND',BN,' ')
                      ENDIF
	            ELSE
		      DO J=1,N1
		    	INBUF(J,NLWBN+LOCL) = IEXCLUDE
		      END DO
	    	    END IF
		ENDDO
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO	
        END IF
        RETURN
	END
