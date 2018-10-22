C
C   REVISION HISTORY
C
C     8-26-83    ASM MODIFIED FOR VAX CONVERSION 
C     8-10-84    SP  CONVERTED TO USE VICAR2 CALLS.
C     8-10-84    SP  CHANGED TO HANDLE SL NOT EQUAL TO 1.
C     8-10-84    SP  CHANGED TO HANDLE UP TO 48 INPUT FILES.
C     9-24-87    FFM UPDATED TEST FILE.
C     6-21-93    GAM PORTED TO UNIX
C     10-10-1997 TXH Corrected uninitialized variable problem.  This 
C                    problem caused the program to ABEND under SGI.
C                    The variable was removed, because it is no longer
C                    needed under the current MIPS environment.
C     09-11-2000 AXC Eliminated argument list terminator on RTL calling
C                    sequence XVPCNT to prevent program ABENDing under 
C                    Linux. (AR-104433)

      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
C
C     'MSS'   PICTURE INTERLEAVING PROGRAM
C     MSS will put up to 48 input images side-by-side, left-to-right.
C
      SUBROUTINE MAIN44

         EXTERNAL WORK

         INTEGER IDSN(48), OUTFILE, IPIXSIZE
         INTEGER NS(48),LOC(48)

         CHARACTER*200 BUF
         CHARACTER*8 FMT

         LOGICAL QSIZE

C============START OF EXECUTABLE CODE==========================
         CALL XVEACTION( 'SA', ' ' )

         CALL XVPCNT( 'INP', NIN)
         IF (NIN .GT. 48) 
     &      CALL MABEND('** MAXIMUM # INPUT FILES = 48')

C  OPEN DATA SETS AND GET LENGTH OF LINE FOR EACH FILE.
         DO I = 1,NIN
            CALL XVUNIT(IDSN(I), 'INP', I, IND, ' ' )
            CALL XVOPEN(IDSN(I), IND, 'OP', 'READ', ' ' )
            CALL XVGET(IDSN(I), IND, 'NS', NS(I), ' ' )
         END DO

         CALL XVGET( IDSN(1), IND, 'FORMAT', FMT, ' ' )  ! BYTES PER PIXEL.
         IND = XVPIXSIZEU( IPIXSIZE, FMT, IDSN(1) )

         CALL XVSIZE( ISL, ISSAMP, NL, NS1, NLL, NSL )   ! GET SIZE PARAMETER.
         QSIZE = NS1 .NE. NSL

C     COMPUTE OUTPUT LINE LENGTH
         N = 1
         NSO = 0

         DO 100 I=1,NIN
           IF (QSIZE)   NS(I)=NS1
           LOC(I) = N
           N = N+NS(I)*IPIXSIZE
           NSO = NSO + NS(I)
  100    CONTINUE

C   OPEN OUTPUT FILE
         CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
         CALL XVOPEN
     &      (OUTFILE,IND,'OP','WRITE','U_NL',NL,'U_NS',NSO,' ')


C     REPORT NSO,NIN AND CALL STACKA
         BUF(1:47) = '** OUTPUT CONTAINS    INTERLEAVED DATA SETS **'
         WRITE (BUF(21:22),'(I2)') NIN
         CALL XVMESSAGE(BUF(2:47),' ')
         BUF(1:48) = '** ACTUAL OUTPUT RECORD LENGTH       SAMPLES **'
         WRITE (BUF(33:37),'(I5)') NSO
         CALL XVMESSAGE(BUF(2:48),' ')
         CALL STACKA(11,WORK,1,N,NL,NIN,NS,LOC,IDSN,ISL,ISSAMP,
     &               OUTFILE)
         RETURN
      END
C**********************************************************************


      SUBROUTINE WORK(OUT,NN,NL,NIN,NS,LOC,IDSN,ISL,ISSAMP,OUTFILE)

         INTEGER NS(NIN),LOC(NIN),IDSN(NIN)
         BYTE OUT(*)


C     MAIN LOOP
         DO 500 I= ISL, ISL + NL - 1
            DO 300 J=1,NIN
               CALL XVREAD( IDSN(J), OUT( LOC(J) ), IND, 'LINE', I,
     &                   'SAMP', ISSAMP, 'NSAMPS', NS(J), ' ' )
  300       CONTINUE
            CALL XVWRIT( OUTFILE, OUT, IND, ' ' )
  500    CONTINUE
         RETURN
      END
