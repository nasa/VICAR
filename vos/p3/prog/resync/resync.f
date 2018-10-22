      INCLUDE 'VICMAIN_FOR'
C
C	11 June 2003    ...rea...  Expand arrays to handle 20,000 lines
C
      SUBROUTINE MAIN44
      REAL BUF(20000,2)
      INTEGER JOFF(20000)
      INTEGER ISET(500)/500*0/
      CHARACTER*80 PRT
      CHARACTER*8 FORMAT
      CHARACTER*3 ORG
C						   open the first input dataset
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		      'U_FORMAT','REAL',' ')
      CALL XVGET(INP,ISTAT,'FORMAT',FORMAT,'ORG',ORG,'NB',NB,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							    get the parameters
      CALL XVPARM('SEARCH',IRANGE,ICNT,IDEF,0)
      CALL XVPARM('IGNORE',IGNORE,ICNT,IDEF,0)
      CALL XVPARM('MODULO',MODULO,ICNT,IDEF,0)
      CALL XVPCNT('INP',NFILES)
C							read in first line
      CALL XVREAD(INP,BUF(1,1),ISTAT,'LINE',ISL,'SAMP',ISS,'NSAMPS',NS,
     +            'BAND',1,' ')
C							loop through each line
      LAST = 1
      NEXT = 2
      IEL = ISL + NL - 1
      NSTRY = NS - 2*IGNORE - IRANGE
      JOFF(ISL) = 0
      JMAX = 0
      JMIN = 0
      DO ILINE=ISL+1,IEL
          CALL XVREAD(INP,BUF(1,NEXT),ISTAT,'LINE',ILINE,'SAMP',ISS,
     +                'NSAMPS',NS,'BAND',1,' ')
C						   loop through possible offsets
          IOFF = 0
          ERR = 1.0E30
          DO IOFFSET=-IRANGE,IRANGE
              IF (IOFFSET .GE. 0) THEN
                  LOCL = IGNORE
                  LOCN = LOCL + IOFFSET
              ELSE
                  LOCN = IGNORE
                  LOCL = LOCN - IOFFSET
              END IF
C						    compute correlation residual
              TEST = 0.0
              DO N=1,NSTRY
                  TEST = TEST + ABS(BUF(LOCN+N,NEXT)-BUF(LOCL+N,LAST))
              END DO
C
              IF (TEST .LT. ERR) THEN
                  IOFF = IOFFSET
                  ERR = TEST
              END IF
          END DO
	  JOFF(ILINE) = IOFF
C					if a shift is found, update ISET array
          IF (IOFF .NE. 0) THEN
	      N = MOD(ILINE,MODULO) + 1
	      ISET(N) = ISET(N) +1
          END IF
          IX = LAST
          LAST = NEXT
          NEXT = IX
      END DO
      CALL XVCLOSE(INP,ISTAT,' ')
C						if MODULO specified...
      IF (MODULO .NE. 1) THEN
C							      find seed location
	  NUM = -1
	  DO I=1,MODULO
	      IF (ISET(I) .GT. NUM) THEN
		  LOC = I
		  NUM = ISET(I)
	      END IF
	  END DO
C						 reject shifts unrelated to seed
	  DO ILINE=ISL+1,IEL
	      IF (JOFF(ILINE) .NE. 0) THEN
		  N = MOD(ILINE,MODULO) + 1
		  IF (N .NE. LOC) JOFF(ILINE)=0
	      END IF
	  END DO
      END IF
C						       compute composite offsets
      JOFF(ISL) = 0
      DO ILINE=ISL+1,IEL
	  IF (JOFF(ILINE) .NE. 0) THEN
              WRITE (PRT,200) JOFF(ILINE),ILINE
  200         FORMAT(' Image shifted by',I5,' at Line',I6)
              CALL XVMESSAGE(PRT,' ')
	  END IF
	  JOFF(ILINE) = JOFF(ILINE-1) + JOFF(ILINE)
      END DO
C					      find the min & max overall shifts
      CALL MINMAX(4,NL,JOFF(ISL),MINOFF,MAXOFF,IMIN,IMAX)
      NSO = NS - MINOFF + MAXOFF
      WRITE (PRT,400) NSO
  400 FORMAT(' *** Output Image contains',I5,' samples')
      CALL XVMESSAGE(PRT,' ')
C
      IF (FORMAT .EQ. 'BYTE') THEN
	  ICODE = 1
      ELSE IF (FORMAT .EQ. 'HALF') THEN
	  ICODE = 2
      ELSE
	  ICODE = 4
      END IF
C							resync all the data
      DO IFILE=1,NFILES
C					     open the input and output datasets
          CALL XVUNIT(INP,'INP',IFILE,ISTAT,' ')
          CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
          CALL XVUNIT(IOUT,'OUT',IFILE,ISTAT,' ')
          CALL XVOPEN(IOUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +                'U_NL',NL,'U_NS',NSO,'OP','WRITE',' ')
C
	  IF (FORMAT .EQ. 'BYTE') THEN
	      CALL FIXB(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE IF (FORMAT .EQ. 'HALF') THEN
	      CALL FIXH(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE IF (FORMAT .EQ. 'FULL') THEN
	      CALL FIXF(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE
	      CALL FIXR(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  END IF
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXB(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      BYTE BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  CALL ITLA(0,BUF,NSO)
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      CALL ITLA(0,BUF,NSO)
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXH(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      INTEGER*2 BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXF(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      INTEGER BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXR(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      REAL BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0.0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0.0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
