CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input and output images.
C
      SUBROUTINE OPEN_IMAGES(iunit,ounit,dcode,sl,ss,nlo,nso,nli,nsi)
      IMPLICIT NONE
      INTEGER*4 IUNIT,OUNIT,DCODE,SL,SS,NLO,NSO,NLI,NSI

      INTEGER STATUS
      CHARACTER*8 FORMAT

C     ...Open input image
      CALL XVUNIT(iunit,'INP',1,status,' ')
      CALL XVOPEN(IUNIT,status,'OPEN_ACT','SA','IO_ACT','SA',' ')
      IF (STATUS.NE.1) CALL XVSIGNAL(IUNIT,STATUS,1) 

C     ...Get data format
      CALL XVGET(IUNIT,status,'FORMAT',format,' ')
      IF (FORMAT.EQ.'BYTE') THEN
         DCODE = 1				!Byte data format
      ELSE IF (FORMAT.EQ.'HALF' .OR. FORMAT.EQ.'WORD') THEN
         DCODE = 2				!Halfword data format
      ELSE
         CALL XVMESSAGE('***Invalid data format',' ')
         CALL MABEND('***PTP task cancelled')
      ENDIF

      CALL XVSIZE(sl,ss,nlo,nso,nli,nsi)

C     ...Open output image
      CALL XVUNIT(OUNIT,'OUT',1,status,' ')
      CALL XVOPEN(OUNIT,status,'U_NL',NLO,'U_NS',NSO,
     +      'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      RETURN
      END
