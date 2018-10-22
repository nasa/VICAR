      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM BLEMPIC
C
C           BLEMPIC BLEMFILE (PBLEM,SAT,TBLEM) NL=800 NS=800
C
C Create images of the permanent blemishes PBLEM, low-full-well pixels
C SAT, and all the blemishes TBLEM (both permanent blemishes and low-full-well
C pixels).  Output image sizes are NLxNS.
C
C 9-95 AS  ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C 6-97 SP            Modified per Cassini.  Changed 255
C                    to INT2BYTE(255).

      SUBROUTINE MAIN44
      COMMON/C1/BLEM(4,5000),PBLEM(4096),SAT(4096),TBLEM(4096),HIS(4096)
      INTEGER*2 BLEM
      BYTE PBLEM,SAT,TBLEM
      INTEGER*4 SAMP,CLASS,OUNIT(3),HIS,SLO,SSO
      INCLUDE 'fortport'

      CALL IFMESSAGE('BLEMPIC version 9-June-97')
      CALL XVUNIT(IUNIT,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,IND,'NS',NSI,' ')
      CALL XVREAD(IUNIT,BLEM,IND,' ')	!Read in blemishes
      NBLEM = NSI/4			!Number of blemishes in input file

      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      DO I=1,3
          CALL XVUNIT(OUNIT(I),'OUT',I,IND,' ')
          CALL XVOPEN(OUNIT(I),IND,'OP','WRITE','U_NL',NLO,'U_NS',NSO,
     &    'U_FORMAT','BYTE','O_FORMAT','BYTE',
     &    'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDDO
      CALL XLADD(OUNIT(1),'HISTORY','BLEMGEN','PERMANENT BLEMISHES',
     &    IND,'FORMAT','STRING',' ')
      CALL XLADD(OUNIT(2),'HISTORY','BLEMGEN','LOW FULL-WELL PIXELS',
     &    IND,'FORMAT','STRING',' ')
      CALL XLADD(OUNIT(3),'HISTORY','BLEMGEN','TOTAL BLEMISHES',
     &    IND,'FORMAT','STRING',' ')

      CALL ZIA(HIS,4096)		!Zero out histogram of saturated DN
      NSX = (NSO+3)/4
      NPBLEM = 0		!number of permanent blemishes
      NSAT = 0			!number of low full-well pixels
      NUNCLASS = 0		!number of unclassified blemishes
      NDCBLEM = 0		!number of double column blemishes
      N = 1
C
      DO 100 L=1,NLO
      CALL ZIA(PBLEM,NSX)
      CALL ZIA(SAT,NSX)
      CALL ZIA(TBLEM,NSX)

   30 LINE = BLEM(1,N)
      SAMP = BLEM(2,N)
      CLASS = BLEM(3,N)
      ISAT = BLEM(4,N)
      IF (LINE.EQ.L) THEN
          IF (CLASS.EQ.0) THEN
               TBLEM(SAMP) = INT2BYTE(255)
               NUNCLASS = NUNCLASS + 1
          ELSE
               IF (CLASS.GT.15) NDCBLEM=NDCBLEM+1
               TBLEM(SAMP) = INT2BYTE(CLASS)
          ENDIF
          IF (ISAT.GT.0) THEN
               SAT(SAMP)=INT2BYTE(ISAT)
               HIS(ISAT+1) = HIS(ISAT+1) + 1
               NSAT = NSAT + 1
          ELSE
               PBLEM(SAMP) = TBLEM(SAMP)
               NPBLEM = NPBLEM + 1
          ENDIF
          N = N + 1
          IF (N.LE.NBLEM) GOTO 30          
          N = N - 1
      ENDIF

      CALL XVWRIT(OUNIT(1),PBLEM,IND,' ')
      CALL XVWRIT(OUNIT(2),SAT,IND,' ')
      CALL XVWRIT(OUNIT(3),TBLEM,IND,' ')
  100 CONTINUE

      CALL PRNT(4,1,NPBLEM,'Number of permanent blemishes=.')
      CALL PRNT(4,1,NSAT,'Number of low full-well pixels=.')
      CALL PRNT(4,1,NUNCLASS,'Number of unclassified blemishes=.')
      IF (NDCBLEM.GT.0)
     & CALL PRNT(4,1,NDCBLEM,'***Number of double column blemishes=.')
      CALL PRNT(4,1,NBLEM,'Total number of blemishes=.')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE
     &     ('Histogram of saturation DN of low full-well pixels',' ')
      IF (NSAT.GT.0) CALL PHIST(HIS,NSAT,0,255,0,0)
      CALL XVMESSAGE('BLEMPIC task completed',' ')
      RETURN
      END
