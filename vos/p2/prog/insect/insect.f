C    29 JUL 1985 ...JHR... allow half,full,real data types
C    02 NOV 1984 ...BXG... converted to VICAR2
C    29 FEB 1984 ...SJR... converted for use on the VAX 11/780 
C    27 JUN 1975 ...DAH... CHANGES FOR CONVERSION TO 360/OS
C    22 MAR 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C     INSERT SECTOR FROM SECONDARY PICTURE INTO PRIMARY PICTURE
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
	implicit none
      INTEGER*4 SL,SS,NL,NS,MAXCNT
      INTEGER*4 IPARM(6),IUNIT1,IUNIT2,OUNIT,SL2,SS2,BN2,STAT
      INTEGER*4 J,L,LN2,ICOUNT,IDEF,IL1,IL2,NL1,NL2,NLI,NSI
      INTEGER*4 NLI1,NLI2,NSI1,NSI2,NS1,NS2,SN2,icode
      REAL*4 BUF(60000)
      CHARACTER*8 FORM1,FORM2
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      DATA MAXCNT/6/ 
C
      CALL IFMESSAGE('INSECT version 7 Jan 2013 (64-bit) - rjb')
      CALL XVEACTION('SA',' ')	
      CALL XVPARM('INSECT',IPARM,ICOUNT,IDEF,MAXCNT)
C
C        OPEN INPUT DATA SETS
      CALL XVUNIT(IUNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT1,STAT,' ')
      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT2,STAT,' ')
C
C        CHECK THAT BOTH FORMATS ARE THE SAME
      CALL XVGET(IUNIT1,STAT,'NL',NLI1,'NS',NSI1,'FORMAT',FORM1,' ')
      CALL XVGET(IUNIT2,STAT,'NL',NLI2,'NS',NSI2,'FORMAT',FORM2,' ')
      IF (FORM1.NE.FORM2) THEN
         CALL XVMESSAGE('??E - Inputs must be in the same format',' ')
         CALL ABEND
      END IF
        icode = 0
        if (form1.eq.'BYTE') icode=1
        if (form1.eq.'HALF'.or.form1.eq.'WORD') icode=2
        if (form1.eq.'FULL') icode=3
        if (form1.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend  
        endif
        call xvclose(iunit1,stat,' ')
	call xvclose(iunit2,stat,' ')
c
         call xvopen( iunit1, stat, 'OP', 'READ', 
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ' )

         call xvopen( iunit2, stat, 'OP', 'READ',
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ' )

c      BYTPIX=0
c      IF(FORM1.EQ.'BYTE') BYTPIX=1
c      IF(FORM1.EQ.'HALF') BYTPIX=2
c      IF(FORM1.EQ.'FULL') BYTPIX=4
c      IF(FORM1.EQ.'REAL') BYTPIX=4
c      IF(BYTPIX.EQ.0) THEN
c         CALL XVMESSAGE('??E - INVALID DATA FORMAT',' ')
c         CALL ABEND
c      END IF
C
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
C
      IF (NS.GT.60000) THEN
         CALL XVMESSAGE('??E - Number of Samples Exceeds 60,000 ',' ')
         CALL ABEND
      END IF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     & 'O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
C
      SL2=IPARM(1)
      SS2=IPARM(2)
      NL2=IPARM(3)
      NS2=IPARM(4)
      LN2=IPARM(5)
      SN2=IPARM(6)
      NL1=NLI-SL+1
      NS1=NSI-SS+1
      IL1=SL
      IL2=SL2
      BN2=(SN2-1)+1
c      NB=BYTPIX*NS
C
      DO L=1,NL
	 do j=1,nsi1
	    buf(j)=0.0
	 enddo
c         CALL ITLA(0,BUF,NB)
         IF(L.LE.NL1) THEN
            CALL XVREAD(IUNIT1,BUF,STAT,'LINE',IL1,'SAMP',SS,
     &                 'NSAMPS',NS1,' ')
            IL1=IL1+1
         END IF
         IF(L.GE.LN2.AND.L.LT.LN2+NL2) THEN
            CALL XVREAD(IUNIT2,BUF(BN2),STAT,'LINE',IL2,
     &                 'SAMP',SS2,'NSAMPS',NS2,' ')
            IL2=IL2+1
         END IF
         CALL XVWRIT(OUNIT,BUF,STAT,'NSAMPS',NS,' ')
      ENDDO

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT1,STAT,' ')
      CALL XVCLOSE(IUNIT2,STAT,' ') 
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
