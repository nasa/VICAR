C
C 2-JAN-95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C 8-MAR-96 ...TXH... CORRECTING ARRAY INDEXING PROBLEM ON SUBROUTINE RPR
C                    FR 88239; ON INVALID ARRAY INDEXING FOR THE ARRAY PRT
C

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	EXTERNAL SAR,RPR
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IVAL(4)
	LOGICAL XVPTST,QMV,QZOK
	CHARACTER*80   PRT
        CHARACTER*4 FMT
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C								open datasets
	CALL IFMESSAGE('REPAIR Version 8-MAR-96')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)

C               determine size parameter for abnormal conditions  
        CALL XVPARM ('SIZE',IVAL,ICNT,IDEF,0) 
        IF (IVAL(3).GT.0) THEN
           IF (IVAL(1).GT.IVAL(3) .OR. IVAL(3).GT.NLIN) THEN
              CALL MABEND (
     +         'Invalid SIZE value.  Program terminated.')
           END IF
        END IF

        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      'Invalid SIZE value.  Program terminated.')

        IF (IVAL(4).GT.0) THEN
           IF (IVAL(2).GT.IVAL(4) .OR. IVAL(4).GT.NSIN) THEN
              CALL MABEND (
     +         'Invalid SIZE value.  Program terminated.') 
           END IF
        END IF

        IF (IVAL(2).GT.NSIN) CALL MABEND (
     +      'Invalid SIZE value.  Program terminated.')

        CALL XVPARM ('SL',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      'Invalid SL value.  Program terminated.')

        CALL XVPARM ('SS',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NSIN) CALL MABEND (
     +      'Invalid SS value.  Program terminated.')

        CALL XVPARM ('NL',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NLIN) CALL MABEND (
     +      'Invalid NL value.  Program terminated.')

        CALL XVPARM ('NS',IVAL,ICNT,IDEF,0)
        IF (IVAL(1).GT.NSIN) CALL MABEND (
     +      'Invalid NS value.  Program terminated.')

	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,' ')
        
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
C								get parameters
	QZOK = XVPTST('ZOK')
	QMV = XVPTST('MV')
	CALL XVPARM('CORR',CLEVEL,ICNT,IDEF,1)
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,1)
        IF (XMEAN.LT.0.0) CALL MABEND (
     +      'MEAN parameter value must not be negative') 
	IF (IDEF.NE.1) QMV=.TRUE.
	CALL XVPARM('VARIANCE',VAR,ICNT,IDEF,1)
        IF (VAR.LT.0.0) CALL MABEND (
     +      'VARIANCE parameter value must not be negative') 
	IF (IDEF.NE.1) QMV=.TRUE.

C                           process the 'area', 'badline' & 'lineset' parameters
	CALL PRCESS(ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA)
C
	II = 4*NS
	JJ = 3*II
	IF (XVPTST('ALL')) THEN
	    CALL STACKA(16,SAR,2,II,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,
     +			NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
	ELSE 
C								   report limits
	    PRT='Correlation tolerance = '
	    WRITE(PRT(24:29),'(F6.3)' ) CLEVEL
	    CALL XVMESSAGE(PRT,' ')
	    IF (QMV) THEN
	        PRT='Mean tolerance = '
	        WRITE(PRT(17:22), '(F6.2)') XMEAN
		CALL XVMESSAGE(PRT,' ')
		PRT='Variance tolerance = '
		WRITE(PRT(21:27), '(F7.2)') VAR
		CALL XVMESSAGE(PRT,' ')
	    END IF                         
	    CALL STACKA(16,RPR,2,II,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,
     +			NAREA,INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAR(OBUF,II,BUF,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,
     +			INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
C
C	This routine replaces all areas specified, without any statistical
C	testing. Its function is the same as the old VICAR program SAR.
C
	REAL OBUF(NS),BUF(NS,3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
C
	IEL = ISL+NL-1
	LINE = ISL
	DO I=1,NAREA
	    IF (NLBUF(I).NE.0) THEN		
		ISLBAD = ISLBUF(I)
C							Copy the good lines 
C							preceeding the bad set
		DO WHILE (LINE.LT.ISLBAD)
		    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +				'NSAMPS',NS,' ')
		    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
		    LINE = LINE+1
		END DO
		ISLGOOD = ISLBAD+NLBUF(I)
		IF (ISLGOOD.LE.NL) THEN
C							Get the next good line
		    CALL XVREAD(INUNIT,BUF(1,2),ISTAT,'LINE',ISLGOOD,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    NLBAD = NLBUF(I)
C							Fix all lines in set
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL AVE(BUF(1,1),BUF(1,2),OBUF,NS,X,ROUND)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                  'NSAMPS',NS,' ')
			END DO
		    ELSE
			DO J=1,NLBAD
			    X = FLOAT(J)/FLOAT(NLBAD+1)
			    CALL XVREAD(INUNIT,OBUF,ISTAT,'LINE',LINE,
     +					'SAMP',ISS,'NSAMPS',NS,' ')
			    CALL AVE(BUF(ISSBUF(I),1),BUF(ISSBUF(I),2),
     +				    OBUF(ISSBUF(I)),NSBUF(I),X,ROUND)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			    LINE = LINE+1
			END DO
		    END IF
		ELSE
C						Bad lines go to the bottom of
C						the image, extend last good line
		    NLBAD = NLBUF(I)
		    IF (NSBUF(I).EQ.NS) THEN
			DO J=1,NLBAD
			    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			END DO
		    ELSE
			DO J=1,NLBAD
			    CALL XVREAD(INUNIT,OBUF,ISTAT,'LINE',LINE,
     +					'SAMP',ISS,'NSAMPS',NS,' ')
			    CALL MVE(7,NSBUF(I),BUF(ISSBUF(I),1),
     +				     OBUF(ISSBUF(I)),1,1)
			    CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,
     +                                   'NSAMPS',NS,' ')
			    LINE = LINE+1
			END DO
		    END IF
		END IF
		LINE = ISLGOOD
	    END IF
	END DO
C						   Copy the remaining good lines
	DO WHILE (LINE.LE.IEL)
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',LINE,'SAMP',ISS,
     +			'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    LINE = LINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE RPR(OBUF,II,BUF,JJ,ISLBUF,ISSBUF,NLBUF,NSBUF,NAREA,
     +			INUNIT,IOUTUNIT,ISL,ISS,NL,NS,ROUND)
C
C	This routine searches the areas specified, verifying that each line
C	is similar to its neighbors. Dissimilar lines are replaced by the
C	weighted average of the nearest good lines.
C
	REAL OBUF(NS),BUF(NS,3)
C                                ,AVG(3),VAR(3)
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300)
	LOGICAL QBAD,DIFFER
	LOGICAL QFIRST/.TRUE./
	CHARACTER*80  PRT
C
C       
	DO I=1,NL
	    IF (I.EQ.1) THEN
		LOC = 1			! LOC is the length of the print buffer
		LAST = 1		! LAST is the index of last good line
		LINX = 2		! LINX is the index to the test line
		NEXT = 3		! NEXT is the index to the next line
		LINE = ISL
		LASTGOOD = LINE-1
		IAREA = 0
		IELTEST = -1
	        nstest = ns
		CALL XVREAD(INUNIT,BUF(1,LINX),ISTAT,'LINE',ISL,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		CALL XVREAD(INUNIT,BUF(1,NEXT),ISTAT,'SAMP',ISS,
     +			    'NSAMPS',NS,' ')
		CALL MVE(7,NS,BUF(1,NEXT),BUF(1,LAST),1,1)
	    ELSE IF (I.EQ.NL) THEN
		CALL MVE(7,NS,BUF(1,LAST),BUF(1,NEXT),1,1)
	    ELSE
		CALL XVREAD(INUNIT,BUF(1,NEXT),ISTAT,'SAMP',ISS,
     +			    'NSAMPS',NS,' ')
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
	    QBAD = .FALSE.
	    IF (LINE.GE.ISLTEST) THEN
		QBAD  = DIFFER(BUF(ISSTEST,LAST),BUF(ISSTEST,LINX),
     +			       BUF(ISSTEST,NEXT),NSTEST)
	    END IF

C
	    IF (QBAD) THEN
		IF (QFIRST) THEN
		    CALL XVMESSAGE
     +                   ('The following lines were repaired:',' ')
		    QFIRST = .FALSE.
		END IF
		WRITE(PRT(LOC:LOC+4), '(I5)') LINE
		LOC = LOC+5
		IF (LOC.GE.76) THEN
		    CALL XVMESSAGE(PRT(1:80),' ')
		    LOC = 1 
                ELSE
                    WRITE(PRT(LOC:LOC+4), '(A5)') '     '
		END IF
		LINE = LINE+1
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
			CALL AVE(BUF(1,LAST),BUF(1,LINX),OBUF,NS,X,
     +				 ROUND)
			CALL XVWRIT(IOUTUNIT,OBUF,ISTAT,'NSAMPS',NS,' ')
		    END DO
		END IF
		CALL XVWRIT(IOUTUNIT,BUF(1,LINX),ISTAT,'NSAMPS',NS,' ')
		LASTGOOD = LINE
		LINE = LINE+1
		N = LAST
		LAST = LINX
		LINX = NEXT
		NEXT = N
	    END IF
	END DO
C						If the last line is bad, 
C						replicate the last good line
	IF (QBAD) THEN
	    N = LINE-LASTGOOD-1
	    DO J=1,N
		CALL XVWRIT(IOUTUNIT,BUF(1,LAST),ISTAT,'NSAMPS',NS,' ')
	    END DO
	END IF
	IF (LOC.NE.1) CALL XVMESSAGE(PRT(1:LOC),' ')
	IF (QFIRST) CALL XVMESSAGE('No bad lines found',' ')
	RETURN
	END
C*************************************************************************
	SUBROUTINE PRCESS(ISS,NL,NS,ISLBUF,ISSBUF,NLBUF,NSBUF,NX)
C
C	Routine to gather and collate the  AREA, LINESET, and BADLINE 
C	parameters. This includes sorting the regions by SL, and combining
C	overlapping regions.
C
	INTEGER ISLBUF(300),ISSBUF(300),NLBUF(300),NSBUF(300),IBUF(400)
C
	NX = 0
C						   Get regions specified by AREA
	CALL XVPARM('AREA',IBUF,ICNT,IDEF,400)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,4).NE.0) CALL MABEND( 
     +          ' AREA parameter values must be in sets of 4')
	    DO I=1,ICNT,4
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = IBUF(I+1)-ISS+1
		NLBUF(NX) = IBUF(I+2)
		NSBUF(NX) = IBUF(I+3)

C                                      checking invalid AREA parameters 
                IF ((NL.LT.NLBUF(NX)) .OR. (NS.LT.NSBUF(NX))) THEN
                    CALL MABEND (
     +                 'AREA parameters NL or NS may be out of range')
                ELSEIF ((NL.LT.ISLBUF(NX)) .OR. (NS.LT.ISSBUF(NX))) THEN
                    CALL MABEND (
     +                 'AREA parameters SL or SS may be out of range')
                ELSEIF (NSBUF(NX).LE.0 .OR. NLBUF(NX).LE.0) THEN
                    CALL MABEND (
     +                 'AREA parameters NL and NS must be positive')
                ELSEIF ((ISLBUF(NX).LE.0) .OR. (ISSBUF(NX).LE.0)) THEN
                    CALL MABEND (
     +                 'AREA parameters SL and SS must be positive')

                END IF 
	    END DO
	END IF
C						  Get lines specified by LINESET
	CALL XVPARM('LINESET',IBUF,ICNT,IDEF,200)
	IF (ICNT.NE.0) THEN
	    IF (MOD(ICNT,2).NE.0) CALL MABEND(
     +		' LINESET parameter values must be in sets of 2')
	    DO I=1,ICNT,2
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
		ISSBUF(NX) = 1
		NLBUF(NX) = IBUF(I+1)
                IF (NL.LT.NLBUF(NX) .OR. NL.LT.ISLBUF(NX)) THEN
                   CALL MABEND (
     +                  'LINESET parameters may be out of range')
                ELSE IF (NLBUF(NX).LE.0 .OR. ISLBUF(NX).LE.0) THEN
                   CALL MABEND (
     +                  'LINESET parameters must be positive') 
                END IF 
		NSBUF(NX) = NS
	    END DO
	END IF
C						  Get lines specified by BADLINE
	CALL XVPARM('BADLINE',IBUF,ICNT,IDEF,100)
	IF (ICNT.NE.0) THEN
	    DO I=1,ICNT
		NX = NX+1
		ISLBUF(NX) = IBUF(I)
                IF (NL .LT. ISLBUF(NX)) THEN
                   CALL MABEND (
     +                  'BADLINE parameters may be out of range')
                ELSE IF (ISLBUF(NX).LE.0) THEN
                   CALL MABEND (
     +                  'BADLINE parameters must be positive')
                END IF 
		ISSBUF(NX) = 1
		NLBUF(NX) = 1
		NSBUF(NX) = NS
	    END DO
	END IF
C
	IF (NX.LE.0) THEN
	    NX = 1
	    ISLBUF(1) = 1
	    ISSBUF(1) = 1
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
	LOGICAL FUNCTION DIFFER(BUF1,BUF2,BUF3,NS)
C
C	This routine computes the mean and variance of BUF2, and the
C	correlation coefficient between BUF1 and BUF2. It then verifies
C	that the two lines in BUF1 and BUF2 are similar, within the limits
C	specified by the TESTVALS
C		BUF1	Input array, containing the last good line.
C		BUF2	Input array, containing the test line.
C		BUF3	Input array, containing the next line.
C		NS	Input, the number of pixels on each line.
C
	REAL*8 SUMX,SUMY,SUMSQX,SUMSQY,XY
	REAL BUF1(NS),BUF2(NS),BUF3(NS)
	LOGICAL QMV,QZOK 
	COMMON /TESTVALS/ CLEVEL,XMEAN,VAR,QMV,QZOK
C
	SUMX = 0.0
	SUMY = 0.0
	SUMSQX = 0.0
	SUMSQY = 0.0
	XY = 0.0
C								gather raw stats
	DO I=1,NS
	    X = BUF2(I)
	    Y = BUF1(I)
	    SUMX = SUMX + X
	    SUMY = SUMY + Y
	    SUMSQX = SUMSQX + X*X
	    SUMSQY = SUMSQY + Y*Y
	    XY = XY + X*Y
	END DO
C
	AVGX = SUMX/NS
	AVGY = SUMY/NS
	VARX = SUMSQX/NS - AVGX*AVGX
	VARY = SUMSQY/NS - AVGY*AVGY
	IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
	    IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		RHO = 1.0
	    ELSE
		RHO = 0.0
	    END IF
	ELSE
	    RHO = (XY/NS-AVGX*AVGY)/SQRT(VARX*VARY)
	END IF
C						determine whether the test line
C						is unlike the reference line
        IF (RHO.LT.CLEVEL) THEN
            DIFFER = .TRUE.
        ELSEIF (QMV) THEN 
           IF ((ABS(AVGX-AVGY).GE.XMEAN 
     +                   .OR. ABS(VARX-VARY).GE.VAR)) THEN 
              DIFFER = .TRUE.
           ELSE
              DIFFER = .FALSE.
           ENDIF
	ELSE
	    DIFFER = .FALSE.
        ENDIF
C
C					If the line is different than the last
C					good line, compare to the average of the
C					next line and the last good line.
	IF (DIFFER) THEN
C
	    SUMY = 0.0
	    SUMSQY = 0.0
	    XY = 0.0
	    DO I=1,NS
		Y = (BUF1(I)+BUF3(I))/2.0
		SUMY = SUMY + Y
		SUMSQY = SUMSQY + Y*Y
		XY = XY + BUF2(I)*Y
	    END DO
C
	    AVGY = SUMY/NS
	    VARY = SUMSQY/NS - AVGY*AVGY
	    IF (VARX.LE.0.0 .OR. VARY.LE.0.0) THEN
		IF (QZOK .AND. (AVGX.EQ.0.0 .OR. AVGY.EQ.0.0)) THEN
		    RHO = 1.0
		ELSE
		    RHO = 0.0
		END IF
	    ELSE
		RHO = (XY/NS-AVGX*AVGY)/SQRT(VARX*VARY)
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
