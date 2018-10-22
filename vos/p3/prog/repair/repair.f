	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	13 OCT 89 ...REA... Added BSQ, BIL capability
C	 1 MAY 91 ...REA... Convert to UNIX/VICAR
C	17 JAN 03 ...REA... MODULO keyword added
C        9 APR 03 ...REA... Fix bug when SL<>1, and search area defaulted
C        9 APR 03 ...REA... Fix bug in rounding, when  input is REAL
C
	EXTERNAL SAR,RPR,SAR_MOD,RPR_MOD
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),MODVAL(2)
	LOGICAL XVPTST,QMV,QZOK
	CHARACTER*3 ORG
	CHARACTER*8 FMT
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	CALL XVMESSAGE('REPAIR Version April 9, 2003',' ')
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBI)
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,'ORG',ORG,' ')
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
	IF (ORG .EQ. 'BIP') THEN
      	    CALL XVMESSAGE(' REPAIR cannot process BIP files',' ')
	    CALL ABEND
	ENDIF
	IF (NB.EQ.1) THEN
	    ORG = 'BSQ'
	ELSE
	    ORG = 'BIL'
	END IF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE','U_ORG',ORG,' ')
C								get parameters
	QZOK = XVPTST('ZOK')
	QMV = XVPTST('MV')
	CALL XVPARM('MODULO',MODVAL,ICNT,IDEF,2)
	MOD1 = MODVAL(1)
	MOD2 = MODVAL(2)
	CALL XVPARM('CORR',CLEVEL,ICNT,IDEF,0)
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,0)
	IF (IDEF.NE.1) QMV=.TRUE.
	CALL XVPARM('VARIANCE',VAR,ICNT,IDEF,0)
	IF (IDEF.NE.1) QMV=.TRUE.
C			    process the 'area', 'badline' & 'lineset' parameters
	CALL PRCESS(ISL,ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA)
C
	II = 4*NS*NB
	JJ = 3*II
	IF (XVPTST('ALL')) THEN
	    IF (MOD2 .GT. 1) THEN
		CALL STACKA(6,SAR_MOD,2,II,JJ,MOD1,MOD2)
	    ELSE
		CALL STACKA(4,SAR,2,II,JJ)
	    END IF
	ELSE 
C								   report limits
	    WRITE (PRT,100) CLEVEL
  100	    FORMAT(' Correlation tolerance =',F6.3)
	    CALL XVMESSAGE(PRT,' ')
	    IF (QMV) THEN
		WRITE (PRT,200) XMEAN
  200		FORMAT(' Mean tolerance =',F10.2)
		CALL XVMESSAGE(PRT,' ')
		WRITE (PRT,300) VAR
  300		FORMAT(' Variance tolerance =',F11.2)
		CALL XVMESSAGE(PRT,' ')
	    END IF
	    IF (MOD2 .GT. 1) THEN
		CALL STACKA(6,RPR_MOD,2,II,JJ,MOD1,MOD2)
	    ELSE
		CALL STACKA(4,RPR,2,II,JJ)
	    END IF
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR(OBUF,II,BUF,JJ)
C
C	This routine replaces all areas specified, without any statistical
C	testing. Its function is the same as the old VICAR program SAR.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
		    END DO
		    LINE = LINE+1
		END DO
		ISLGOOD = ISLBAD+NLBUF(I)
		IF (ISLGOOD.LE.NL) THEN
C							Get the next good line
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,2),ISTAT,'LINE',
     +			   ISLGOOD,'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
		    END DO
		    NLBAD = NLBUF(I)
C							Fix all lines in set
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL AVE(BUF(1,1,1),BUF(1,1,2),OBUF,NS*NB,
     +				     X,ROUND)
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			END DO
		    ELSE
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    DO II=1,NB
				CALL XVREAD(INUNIT,OBUF(1,II),ISTAT,
     +					    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
				CALL AVE(BUF(ISSBUF(I),II,1),
     +				    BUF(ISSBUF(I),II,2),
     +				    OBUF(ISSBUF(I),II),NSBUF(I),X,ROUND)
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+1
			END DO
		    END IF
		ELSE
C						Bad lines go to the bottom of
C						the image, extend last good line
		    NLBAD = NLBUF(I)
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			END DO
		    ELSE
			DO J=1,NLBAD
			    DO II=1,NB
				CALL XVREAD(INUNIT,OBUF(1,II),ISTAT,
     +					    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
				CALL MVE(7,NSBUF(I),BUF(ISSBUF(I),II,1),
     +					 OBUF(ISSBUF(I),II),1,1)
				CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+1
			END DO
		    END IF
		END IF
		LINE = ISLGOOD
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    DO II=1,NB
		CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,'BAND',II,' ')
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    END DO
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR(OBUF,II,BUF,JJ)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBAD(16)
	LOGICAL DIFFER,QBAD
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
C
	LAST = 1		! LAST is the index of last good line
	LINX = 2		! LINX is the index to the test line
	NEXT = 3		! NEXT is the index to the next line
	NBAD = 0
	LASTGOOD = 0
	IAREA = 0
	IELTEST = -1
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,LINX),ISTAT,'LINE',ISL,
     +		        'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'LINE',ISL+1,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	CALL MVE(7,NS*NB,BUF(1,1,NEXT),BUF(1,1,LAST),1,1)
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    IF (LINE .NE. IEL) THEN
		DO II=1,NB
		    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'SAMP',ISS,
     +				'NSAMPS',NS,'BAND',II,'LINE',LINE+1,' ')
		END DO
	    ELSE
		CALL MVE(7,NS*NB,BUF(1,1,LAST),BUF(1,1,NEXT),1,1)
	    END IF
C						Get the next area to be tested
	    IF (LINE.GT.IELTEST) THEN
		IAREA = IAREA+1
		DO WHILE (NLBUF(IAREA).LE.0 .AND. IAREA.LT.NAREA)
		    IAREA = IAREA+1
		END DO
		IF (IAREA.LE.NAREA) THEN
		    ISLTEST = ISLBUF(IAREA)
		    ISSTEST = ISSBUF(IAREA)
		    IELTEST = ISLTEST + NLBUF(IAREA) -1
		    NSTEST = NSBUF(IAREA)
		ELSE
		    ISLTEST = 999999
		    IELTEST = 999999
		END IF
	    END IF
C
	    IF (LINE.GE.ISLTEST) THEN
		QBAD  = DIFFER(BUF(ISSTEST,1,LAST),BUF(ISSTEST,1,LINX),
     +			       BUF(ISSTEST,1,NEXT),NSTEST,NB)
	    ELSE
		QBAD = .FALSE.
	    END IF

C
	    IF (QBAD) THEN
		IF (NBAD .EQ. 0) CALL XVMESSAGE(
     +			 ' The following lines were repaired:',' ')
		NBAD = NBAD + 1
		IF (NBAD .GT. 16) THEN
		    WRITE (PRT,100) (IBAD(I),I=1,16)
  100		    FORMAT(16I5)
		    CALL XVMESSAGE(PRT,' ')
		    NBAD = 1
		END IF
		IBAD(NBAD) = LINE
		N = LINX
		LINX = NEXT
		NEXT = N
	    ELSE
C						If good, write out line and any
C						repaired lines that are needed.
C						Update pointers.
		IF (LASTGOOD.NE.LINE-1) THEN
		    NLBAD = LINE-LASTGOOD-1
		    DO J=1,NLBAD
			X = FLOAT(J)/FLOAT(NLBAD+1)
			CALL AVE(BUF(1,1,LAST),BUF(1,1,LINX),OBUF,NS*NB,
     +				 X,ROUND)
			DO II=1,NB
			    CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					'NSAMPS',NS,' ')
			END DO
		    END DO
		END IF
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LINX),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
		LASTGOOD = LINE
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = IEL-LASTGOOD
	    DO J=1,N
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LAST),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
	    END DO
	END IF
	WRITE (PRT,100) (IBAD(I),I=1,NBAD)
	CALL XVMESSAGE(PRT,' ')
	IF (NBAD .EQ. 0) CALL XVMESSAGE(' No bad lines found',' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR_MOD(OBUF,II,BUF,JJ,MOD1,MOD2)
C
C	This routine replaces all lines where MOD(LINE,MOD2) = MOD1 in the
C	areas specified, without any statistical testing. Its function is 
C	the same as the old VICAR program SAR.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    DO II=1,NB
			CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
		    END DO
		    LINE = LINE+1
		END DO
C							Loop through all lines 
C							of this set
		IELBAD = ISLBAD + NLBUF(I) - 1
		DO WHILE (LINE .LE. IELBAD)
		    IF (MOD(LINE,MOD2) .NE. MOD1) THEN
			DO II=1,NB
			    CALL XVREAD(INUNIT,BUF(1,II,1),ISTAT,
     +				    'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,
     +				    'BAND',II,' ')
			    CALL XVWRIT(IOUTUNIT,BUF(1,II,1),ISTAT,
     +				    'NSAMPS',NS,' ')
			END DO
			LINE = LINE + 1
		    ELSE
C							Bad line encountered
			IF (LINE .LT. IEL) THEN
C						    Read the following good line
			    DO II=1,NB
				CALL XVREAD(INUNIT,BUF(1,II,2),ISTAT,
     +					    'LINE',LINE+1,'SAMP',ISS,
     +					    'NSAMPS',NS,'BAND',II,' ')
			    END DO
C								Fix the bad line
			    IF (NSBUF(I).EQ.NS) THEN
				CALL AVE(BUF(1,1,1),BUF(1,1,2),OBUF,
     +					 NS*NB,0.5,ROUND)
				DO II=1,NB
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    ELSE
				DO II=1,NB
				    CALL XVREAD(INUNIT,OBUF(1,II),
     +					   ISTAT,'LINE',LINE,'SAMP',ISS,
     +					   'NSAMPS',NS,'BAND',II,' ')
				    CALL AVE(BUF(ISSBUF(I),II,1),
     +					     BUF(ISSBUF(I),II,2),
     +					     OBUF(ISSBUF(I),II),
     +					     NSBUF(I),0.5,ROUND)
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    END IF
C							Write out the good line
C							that follows
			    DO II=1,NB
				CALL XVWRIT(IOUTUNIT,BUF(1,II,2),ISTAT,
     +					    'NSAMPS',NS,' ')
			    END DO
			    LINE = LINE+2
			ELSE
C						  Bad line is last line in image
			    IF (NSBUF(I).EQ.NS) THEN
				DO II=1,NB
				    CALL XVWRIT(IOUTUNIT,BUF(1,II,1),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    ELSE
				DO II=1,NB
				    CALL XVREAD(INUNIT,OBUF(1,II),
     +					   ISTAT,'LINE',LINE,'SAMP',ISS,
     +					   'NSAMPS',NS,'BAND',II,' ')
				    CALL MVE(7,NSBUF(I),
     +					     BUF(ISSBUF(I),II,1),
     +					     OBUF(ISSBUF(I),II),1,1)
				    CALL XVWRIT(IOUTUNIT,OBUF(1,II),
     +						ISTAT,'NSAMPS',NS,' ')
				END DO
			    END IF
			    LINE = LINE+1
			END IF
		    END IF
		END DO
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    DO II=1,NB
		CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,'BAND',II,' ')
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    END DO
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR_MOD(OBUF,II,BUF,JJ,MOD1,MOD2)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
	REAL OBUF(NS,NB),BUF(NS,NB,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBAD(16)
	LOGICAL DIFFER,QBAD
	CHARACTER*80 PRT
C
	COMMON /CIO/ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,INUNIT,IOUTUNIT,
     +		    ISL,ISS,NL,NS,NB,ROUND
C
C
	LAST = 1		! LAST is the index of last good line
	LINX = 2		! LINX is the index to the test line
	NEXT = 3		! NEXT is the index to the next line
	NBAD = 0
	LASTGOOD = 0
	IAREA = 0
	IELTEST = -1
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,LINX),ISTAT,'LINE',ISL,
     +		        'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	DO II=1,NB
	    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'LINE',ISL+1,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',II,' ')
	END DO
	CALL MVE(7,NS*NB,BUF(1,1,NEXT),BUF(1,1,LAST),1,1)
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    IF (LINE .NE. IEL) THEN
		DO II=1,NB
		    CALL XVREAD(INUNIT,BUF(1,II,NEXT),ISTAT,'SAMP',ISS,
     +				'NSAMPS',NS,'BAND',II,'LINE',LINE+1,' ')
		END DO
	    ELSE
		CALL MVE(7,NS*NB,BUF(1,1,LAST),BUF(1,1,NEXT),1,1)
	    END IF
C						Get the next area to be tested
	    IF (LINE.GT.IELTEST) THEN
		IAREA = IAREA+1
		DO WHILE (NLBUF(IAREA).LE.0 .AND. IAREA.LT.NAREA)
		    IAREA = IAREA+1
		END DO
		IF (IAREA.LE.NAREA) THEN
		    ISLTEST = ISLBUF(IAREA)
		    ISSTEST = ISSBUF(IAREA)
		    IELTEST = ISLTEST + NLBUF(IAREA) -1
		    NSTEST = NSBUF(IAREA)
		ELSE
		    ISLTEST = 999999
		    IELTEST = 999999
		END IF
	    END IF
C
	    IF (LINE.GE.ISLTEST .AND. MOD(LINE,MOD2).EQ.MOD1) THEN
		QBAD  = DIFFER(BUF(ISSTEST,1,LAST),BUF(ISSTEST,1,LINX),
     +			       BUF(ISSTEST,1,NEXT),NSTEST,NB)
	    ELSE
		QBAD = .FALSE.
	    END IF

C
	    IF (QBAD) THEN
		IF (NBAD .EQ. 0) CALL XVMESSAGE(
     +			 ' The following lines were repaired:',' ')
		NBAD = NBAD + 1
		IF (NBAD .GT. 16) THEN
		    WRITE (PRT,100) (IBAD(I),I=1,16)
  100		    FORMAT(16I5)
		    CALL XVMESSAGE(PRT,' ')
		    NBAD = 1
		END IF
		IBAD(NBAD) = LINE
		N = LINX
		LINX = NEXT
		NEXT = N
	    ELSE
C						If good, write out line and any
C						repaired lines that are needed.
C						Update pointers.
		IF (LASTGOOD.NE.LINE-1) THEN
		    NLBAD = LINE-LASTGOOD-1
		    DO J=1,NLBAD
			X = FLOAT(J)/FLOAT(NLBAD+1)
			CALL AVE(BUF(1,1,LAST),BUF(1,1,LINX),OBUF,NS*NB,
     +				 X,ROUND)
			DO II=1,NB
			    CALL XVWRIT(IOUTUNIT,OBUF(1,II),ISTAT,
     +					'NSAMPS',NS,' ')
			END DO
		    END DO
		END IF
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LINX),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
		LASTGOOD = LINE
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = IEL-LASTGOOD
	    DO J=1,N
		DO II=1,NB
		    CALL XVWRIT(IOUTUNIT,BUF(1,II,LAST),ISTAT,
     +				'NSAMPS',NS,' ')
		END DO
	    END DO
	END IF
	WRITE (PRT,100) (IBAD(I),I=1,NBAD)
	CALL XVMESSAGE(PRT,' ')
	IF (NBAD .EQ. 0) CALL XVMESSAGE(' No bad lines found',' ')
	RETURN
	END
C*************************************************************************
	SUBROUTINE PRCESS(ISL,ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NX)
C
C	Routine to gather and collate the  AREA, LINESET, and BADLINE 
C	parameters. This includes sorting the regions by SL, and combining
C	overlapping regions.
C
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBUF(400)
C
	NX = 0
C						   Get regions specified by AREA
	CALL XVPARM('AREA',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,4).NE.0) THEN
		CALL XVMESSAGE(
     +		' AREA parameter values must be in sets of 4',' ')
		CALL ABEND
	    ENDIF
	    DO I=1,ICNT,4
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = IBUF(I+1)-ISS+1
		NLBUF(NX) = IBUF(I+2)
		NSBUF(NX) = IBUF(I+3)
	    END DO
	END IF
C						  Get lines specified by LINESET
	CALL XVPARM('LINESET',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,2).NE.0) THEN
		CALL XVMESSAGE(
     +		' LINESET parameter values must be in sets of 2',' ')
		CALL ABEND
	    ENDIF
	    DO I=1,ICNT,2
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = IBUF(I+1)
		NSBUF(NX) = NS
	    END DO
	END IF
C						  Get lines specified by BADLINE
	CALL XVPARM('BADLINE',IBUF,ICNT,IDEF,0)
	IF (ICNT.NE.0) THEN
	    DO I=1,ICNT
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = 1
		NSBUF(NX) = NS
	    END DO
	END IF
C
	IF (NX.LE.0) THEN
	    NX = 1
	    ISLBUF(1) = ISL
	    ISSBUF(1) = ISS
	    NLBUF(1) = NL
	    NSBUF(1) = NS
	ELSE IF (NX.GT.1) THEN
C					put the areas into line ascending order
	    DO J=1,NX-1
		DO K=J+1,NX
		    IF(ISLBUF(J).GT.ISLBUF(K))  THEN
			N1 = ISLBUF(J)
			N2 = ISSBUF(J)
			N3 = NLBUF(J)
			N4 = NSBUF(J)
			ISLBUF(J) = ISLBUF(K)
			ISSBUF(J) = ISSBUF(K)
			NLBUF(J) = NLBUF(K)
			NSBUF(J) = NSBUF(K)
			ISLBUF(K) = N1
			ISSBUF(K) = N2
			NLBUF(K)= N3
			NSBUF(K)= N4
		    END IF
		END DO
	    END DO
	END IF
C						  take care of overlapping areas
	DO J=1,NX-1
	    IF (NLBUF(J).NE.0) THEN
		IEL = ISLBUF(J)+NLBUF(J)-1
		K = J+1
		DO WHILE (IEL.GE.ISLBUF(K) .AND. K.LE.NX)
		    IEL = MAX(IEL,ISLBUF(K)+NLBUF(K)-1)
		    NLBUF(J) = IEL-ISLBUF(J)+1
		    NLBUF(K) = 0
		    ISSBUF(J) = MIN(ISSBUF(J),ISSBUF(K))
		    IES1 = MAX(NSBUF(J)+ISSBUF(J),NSBUF(K)+ISSBUF(K))
		    NSBUF(J) = IES1-ISSBUF(J)
		    K = K+1
		END DO
	    END IF
	END DO
C
	RETURN
	END
C***********************************************************************
	SUBROUTINE AVE(BUF1,BUF2,OBUF,NS,X,ROUND)
C
C	This routine does a linear interpolation between BUF1 and BUF2,
C	putting the result in OBUF. X is the fractional weight to be given
C	BUF2.
C
	REAL BUF1(NS),BUF2(NS),OBUF(NS)
C
	Y = 1.0-X
	DO I=1,NS
	    OBUF(I) = Y*BUF1(I) + X*BUF2(I) + ROUND
	END DO
	RETURN
	END
C***********************************************************************
	LOGICAL FUNCTION DIFFER(BUF1,BUF2,BUF3,NS,NB)
C
C	This routine computes the mean and variance of BUF2, and the
C	correlation coefficient between BUF1 and BUF2. It then verifies
C	that the two lines in BUF1 and BUF2 are similar, within the limits
C	specified by the TESTVALS
C		BUF1	Input array, containing the last good line.
C		BUF2	Input array, containing the test line.
C		BUF3	Input array, containing the next line.
C		NS	Input, the number of pixels on each line.
C		NB	Input, the number of channels for each line
	REAL*8 SUMX,SUMY,SUMSQX,SUMSQY,XY
	REAL BUF1(NS,NB),BUF2(NS,NB),BUF3(NS,NB)
	LOGICAL QMV,QZOK
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	PIXELS = NS*NB
	SUMX = 0.0
	SUMY = 0.0
	SUMSQX = 0.0
	SUMSQY = 0.0
	XY = 0.0
C								gather raw stats
	DO II=1,NB
	    DO I=1,NS
		X = BUF2(I,II)
		Y = BUF1(I,II)
		SUMX = SUMX + X
		SUMY = SUMY + Y
		SUMSQX = SUMSQX + X*X
		SUMSQY = SUMSQY + Y*Y
		XY = XY + X*Y
	    END DO
	END DO
C
	AVGX = SUMX/PIXELS
	AVGY = SUMY/PIXELS
	VARX = SUMSQX/PIXELS - AVGX*AVGX
	VARY = SUMSQY/PIXELS - AVGY*AVGY
	IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
	    IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		RHO = 1.0
	    ELSE
		RHO = 0.0
	    END IF
	ELSE
	    RHO = (XY/PIXELS-AVGX*AVGY)/SQRT(VARX*VARY)
	END IF
C						determine whether the test line
C						is unlike the reference line
	IF (RHO.LT.CLEVEL) THEN
	    DIFFER = .TRUE.
	ELSE IF (QMV .AND. (ABS(AVGX-AVGY).GE.XMEAN 
     +			    .OR. ABS(VARX-VARY).GE.VAR)) THEN
	    DIFFER = .TRUE.
	ELSE
	    DIFFER = .FALSE.
	END IF
C
C					If the line is different than the last
C					good line, compare to the average of the
C					next line and the last good line.
	IF (DIFFER) THEN
C
	    SUMY = 0.0
	    SUMSQY = 0.0
	    XY = 0.0
	    DO II=1,NB
		DO I=1,NS
		    Y = (BUF1(I,II)+BUF3(I,II))/2.0
		    SUMY = SUMY + Y
		    SUMSQY = SUMSQY + Y*Y
		    XY = XY + BUF2(I,II)*Y
		END DO
	    END DO
C
	    AVGY = SUMY/PIXELS
	    VARY = SUMSQY/PIXELS - AVGY*AVGY
	    IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
		IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		    RHO = 1.0
		ELSE
		    RHO = 0.0
		END IF
	    ELSE
		RHO = (XY/PIXELS-AVGX*AVGY)/SQRT(VARX*VARY)
	    END IF
C						determine whether the test line
C						is unlike the possible new line
	    IF (RHO.GE.CLEVEL) THEN
		IF (.NOT.QMV .OR. (ABS(AVGX-AVGY).LE.XMEAN .AND. 
     +		    ABS(VARX-VARY).LE.VAR))    DIFFER = .FALSE.
	    END IF
	END IF
	RETURN
	END
