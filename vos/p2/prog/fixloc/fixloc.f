C      PROGRAM FIXLOC

C  REVISION HISTORY
C     7-97  RRD  MERGED CA'S ADDITIONS INTO PORTED VERSION
C     4-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C     4-94  CA   ADDED READING OF GRID NC,NR FROM LABEL
C    12-93  CA   ADDED DUMMY AND NOPRINT
C     4-86  SP   ADDED OPEN_ACT AND IO_ACT TO XVOPEN CALLS.
C     4-86  SP   DELETED XVADD CALL AND PUT ASSOCIATED PARAMETERS IN XVOPEN
C                CALL.
C     4-86  SP   CONVERTED TO USE XVPARM.
C     4-86  SP   CHANGED MVE CALLS TO USE DCODE OF 7.  DCODE OF 8 DOES  NOT
C                WORK WHEN THE LINE COORDINATE IS 0.0


      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'

      REAL*4 X(2,1000) 
      CHARACTER*132 MSG
      DATA MSG/' ***  (****.*,****.*)    (****.*,****.*)'/
      CHARACTER*132 DEL
      CHARACTER*132 INS
      REAL*4 LOC(2,1000),DL,DS,DMIN,R1,R2
      INTEGER PAR(9000)
      real rpar(9000)
      LOGICAL XVPTST
      EQUIVALENCE (PAR,RPAR),(LOC,X)
C
      CALL IFMESSAGE('FIXLOC version 24-JULY-97')

      CALL XVUNIT(inunit,'INP',1,status,' ')
      CALL XVOPEN(inunit,status,
     .            'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      NLOC = MAX0(NSI,NS)/2
      NC = 0
      NR = 0
      CALL XVREAD(inunit,X,status,'NSAMPS',NSI,' ')

C          SCAN FOR SPECIAL END OF RECORD MARK
      DO 1 I = 1, NLOC
         IF (LOC(1,I).EQ.-999.0 .AND. LOC(2,I).EQ.-999.0) GOTO 1000
    1 CONTINUE
      I = NLOC + 1
 1000 NLOC = I - 1
      NLOCSV = NLOC
C
C-------GET GRID SIZE FROM REFERENCE LABEL EITHER INTERLOC OR GRIDLOCB
        CALL XLGET(INUNIT,'HISTORY','GRID_NROW',NR,IST,'HIST',
     1             'INTERLOC','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XLGET(INUNIT,'HISTORY','GRID_NROW',NR,IST,'HIST',
     2               'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NROW NOT FOUND IN LABEL',' ')

        CALL XLGET(INUNIT,'HISTORY','GRID_NCOL',NC,IST,'HIST',
     1             'INTERLOC','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1	  CALL XLGET(INUNIT,'HISTORY','GRID_NCOL',NC,IST,'HIST',
     2               'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
        IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NCOL NOT FOUND IN LABEL',' ')
C
C....READ PARAMETERS

      CALL XVPARM( 'NC', PAR, ICOUNT, IDEF, 0)
      IF ( ICOUNT .GT. 0 )  THEN
           NC = PAR(1)
      END IF

      CALL XVPARM( 'CHANGE', PAR, ICOUNT, IDEF, 0)
      IF ( ICOUNT .GT. 0 )  THEN
           IF ( MOD( ICOUNT, 3 ) .NE. 0 )  THEN
              CALL XVMESSAGE( 'INVALID COUNT FOR CHANGE PARAM.',' ' )
              CALL ABEND
           END IF

           CALL XVMESSAGE('THE FOLLOWING LOCATIONS ARE CHANGED',' ')
           CALL XVMESSAGE('LOC       INPUT              OUTPUT',' ')
           CALL XVMESSAGE(' ',' ')

           DO II = 1, ICOUNT, 3
             J = RPAR(II)
             WRITE (MSG(2:4),'(I3)') J
             WRITE (MSG(8:13),'(F6.1)') X(1,J)
             WRITE (MSG(15:20),'(F6.1)') X(2,J)
             X(1,J) = RPAR(II+1)
             X(2,J) = RPAR(II+2)
             WRITE (MSG(27:32),'(F6.1)') X(1,J)
             WRITE (MSG(34:39),'(F6.1)') X(2,J)
             CALL XVMESSAGE(MSG(2:40),' ')
           END DO
      END IF

      CALL XVPARM( 'DUMMY', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN

           CALL XVMESSAGE('THE FOLLOWING LOCATIONS ARE CHANGED',' ')
           CALL XVMESSAGE('LOC       INPUT              OUTPUT',' ')
           CALL XVMESSAGE(' ',' ')

           DO II = 1, ICOUNT
             J = PAR(II)
             WRITE (MSG(2:4),'(I3)') J
             WRITE (MSG(8:13),'(F6.1)') X(1,J)
             WRITE (MSG(15:20),'(F6.1)') X(2,J)
	     X(1,J) = -99.
	     X(2,J) = -99.
             WRITE (MSG(27:32),'(F6.1)') X(1,J)
             WRITE (MSG(34:39),'(F6.1)') X(2,J)
             CALL XVMESSAGE(MSG(2:40),' ')
           END DO
      END IF

      CALL XVPARM( 'DELETE', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN
           DO  II = 1, ICOUNT
             J = RPAR(II)
             WRITE (DEL,9900) J,X(1,J),X(2,J)
9900  FORMAT (' LOCATION ',I3,' (',F6.1,',',F6.1,') IS DELETED')
             CALL XVMESSAGE(DEL(2:40),' ')

             IF (NLOC .GT. J) THEN
               CALL MVE(7,2*(NLOC-J),X(1,J+1),X(1,J),1,1)
             END IF
             NLOC = NLOC - 1
           END DO
      END IF

      CALL XVPARM( 'INSERT', PAR, ICOUNT, IDEF, 0 )
      IF ( ICOUNT .GT. 0 )  THEN
           IF ( MOD( ICOUNT, 3 ) .NE. 0 )  THEN
              CALL XVMESSAGE( 'INVALID COUNT FOR INSERT PARAM.',' ' )
              CALL ABEND
           END IF

           DO II = 1, ICOUNT, 3
             J = RPAR(II)
             NLOC = NLOC + 1
             IF (NLOC .GE. J) THEN
                CALL MVE(7, 2*(NLOC-J+1), X(2,NLOC-1),X(2,NLOC),-1,-1)
             END IF
             X(1,J) = RPAR(II+1)
             X(2,J) = RPAR(II+2)
             WRITE (INS,9910) J,X(1,J),X(2,J)
9910  FORMAT (' LOCATION ',I3,' (',F6.1,',',F6.1,') IS INSERTED')
             CALL XVMESSAGE(INS(2:41),' ')
           END DO
      END IF
C
      IF (XVPTST('NOPRINT')) GO TO 100
C
   60 CALL XVMESSAGE('CORRECTED CENTERS',' ')
      IF (NLOC .EQ. 202) GOTO 72
      IF (NLOC .EQ. 103) GOTO 70
C-----ALREADY KNOW GRID NC, NR ?
      IF (NC .NE. 0) GOTO 68
C
C          DETERMINE FORMAT OF DATA
C              NR=NUMBER OF ROWS
C              NC=NUMBER OF COLUMNS
C          FIRST SEE IF SAMPLE COORDINATES INCREASE FASTER THAN LINES

      DL = 0.
      DS = 0.
C
      DO 61 K=2,NLOC
         IF(LOC(1,K).LT.0.) GOTO 61
         DL = DL + ABS(LOC(1,K)-LOC(1,K-1))
         DS = DS + ABS(LOC(2,K)-LOC(2,K-1))
   61 CONTINUE
C          (I1,I2)=(1,2) IF CENTERS ARE ARRANGED ROW BY ROW
C                 =(2,1) IF COLUMN BY COLUMN
      I1 = 1
      IF (DL .GT. DS) I1 = 2
      I2 = 3 - I1
      N = 1
      MINN = 1
C
      DO 62 K=2,NLOC
         IF(LOC(I2,K-1).GT.0.AND.LOC(I2,K).GT.LOC(I2,K-1)) GOTO 62
         IF(N.GT.MINN) MINN=N
         N = 0
   62 N = N + 1
C
      DMIN = 1.0E32
      MAXN = NLOC/2
C
      DO 64 N=MINN,MAXN
         M = NLOC/N
         IF(M*N.NE.NLOC) GOTO 64
         DS = 0.
         KNT = 0
         K = 1
C
         DO 63 J=1,M
            R2 = LOC(I1,K)
            K = K + 1
C
            DO 63 I=2,N
               R1 = R2
               R2 = LOC(I1,K)
               IF(R1.LT.0..OR.R2.LT.0.) GOTO 63
               DS = DS + ABS(R2-R1)
               KNT = KNT + 1
   63    K = K + 1
C
         DS = DS/KNT
         IF (DS .GT. DMIN) GOTO 64
         DMIN =DS
         NC = N
   64 CONTINUE
C
C
   68 IF (NC .LE. 1) NC = 20
      IF (NR .LE. 1) NR = (NLOC-1) / NC + 1
      CALL PGRID(LOC,NR,NC,TRIX,0)
      GOTO 100
C
C          VO RESEAU PATTERN
   70 CALL PU75(LOC,1)
      GOTO 100
C
C          MJS RESEAU PATTERN
   72 CALL PMJS(LOC,1)
C
  100 CALL XVPCNT( 'OUT', NO )  ! NUMBER OF OUTPUT FILES.
      IF (NO .LT. 1) RETURN

      CALL XVUNIT(outunit,'OUT',1,status,' ')

      CALL XVOPEN(outunit,status,'OP','WRITE','U_NL',1,
     .            'U_NS',NLOC*2,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVWRIT(outunit,X,status,'NSAMPS',NLOC*2,' ')
      CALL XVCLOSE(inunit,status,' ')
      CALL XVCLOSE(outunit,status,' ')
C
      RETURN
      END


