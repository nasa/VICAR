      INCLUDE 'VICMAIN_FOR'
C
C     initial release          6/14/95 Ron Alley
C     PC keyword added         2/13/98 Ron Alley
C
C**********************************************************************
      SUBROUTINE MAIN44
C
      CHARACTER*60 INFILE,OUTFILE
      CHARACTER*10 FORMAT,ORG,INTFMT
      CHARACTER*4 FMT(6)/'BYTE','HALF','FULL','REAL','DOUB','COMP'/
      LOGICAL XVPTST
      BYTE CR(2)/13,10/
C								open input
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVP('INP',INFILE,ICOUNT)
C							      get label contents
      CALL XLGET(INP,'SYSTEM','LBLSIZE',LBLSIZE,ISTAT,'FORMAT','INT',
     +		' ')
      CALL XLGET(INP,'SYSTEM','NL',NL,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','NS',NS,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','NB',NB,ISTAT,'FORMAT','INT',' ')
      CALL XLGET(INP,'SYSTEM','FORMAT',FORMAT,ISTAT,'FORMAT','STRING',
     +		' ')
      CALL XLGET(INP,'SYSTEM','ORG',ORG,ISTAT,'FORMAT','STRING',' ')
      CALL XLGET(INP,'SYSTEM','INTFMT',INTFMT,ISTAT,'FORMAT','STRING',
     +		' ')
C							determine data type flag
      IFMT = 0
      I = 1
      DO WHILE (IFMT.EQ.0 .AND. I.LE.6)
	  IF (FORMAT(1:4) .EQ. FMT(I)) IFMT = I
	  I = I+1
      END DO
      IF (IFMT .EQ. 0) IFMT=1
C							determine byte order flg
      IF (INTFMT .EQ. 'HIGH') THEN
	  INTFLAG = 1
      ELSE
	  INTFLAG = 0
      END IF
C							create header file name
      I = 1
      DO WHILE (INFILE(I:I) .NE. ' ')
	  I = I + 1
      END DO
      OUTFILE = INFILE(1:I-1) // '.hdr'
C							write out header file
      OPEN(71,FILE=OUTFILE)
      IF (XVPTST('PC')) THEN
          WRITE(71,200) CR,NS,CR,NL,CR,NB,CR,LBLSIZE,CR,CR,IFMT,CR,ORG,
     +                  CR,INTFLAG
      ELSE
          WRITE(71,100) NS,NL,NB,LBLSIZE,IFMT,ORG,INTFLAG
      END IF
      CLOSE(71)
  100 FORMAT('ENVI',/,'samples = ',I5,/,'lines = ',I5,/,'bands = ',I5,/,
     +       'header offset = ',I5,/,'file type = VICAR',/,
     +       'data type = ',I1,/,'interleave = ',A3,/,'byte order = ',
     +       I1)
  200 FORMAT('ENVI',2A1,'samples = ',I5,2A1,'lines = ',I5,2A1,
     +       'bands = ',I5,2A1,'header offset = ',I5,2A1,
     +       'file type = VICAR',2A1,'data type = ',I1,2A1,
     +       'interleave = ',A3,2A1,'byte order = ',I1)
C
      RETURN
      END
