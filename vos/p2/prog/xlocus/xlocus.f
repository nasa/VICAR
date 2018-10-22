C 8 MAY 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
      INCLUDE 'VICMAIN_FOR'
       SUBROUTINE MAIN44
       IMPLICIT INTEGER(A-Z)
      COMMON/C1/ IN,OUT,FPAR
      REAL*4 R(2,2),O(2),IN(2,1500),OUT(2,1500),FPAR(50)
C********************
C     PARAMETER PROCESSOR HERE
C********************
      CALL IFMESSAGE('XLOCUS VERSION 8-MAY-95')
      CALL XVP('T',FPAR,CNT)
      I = 1
      R(1,1)=FPAR(I)
      R(2,1)=FPAR(I+1)
      R(1,2)=FPAR(I+2)
      R(2,2)=FPAR(I+3)
      O(1)=FPAR(I+4)
      O(2)=FPAR(I+5)
      CALL PRNT(7,1,R(1,1),'0 R(1,1)= .')
      CALL PRNT(7,1,R(2,1),'0 R(2,1)= .')
      CALL PRNT(7,1,R(1,2),'0 R(1,2)= .')
      CALL PRNT(7,1,R(2,2),'0 R(2,2)= .')
      CALL PRNT(7,1,O(1),'0 O(1)= .')
      CALL PRNT(7,1,O(2),'0 O(2)= .')
      CALL XVUNIT(IUNI,'INP',1,STAT,' ')
      CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      NP=NS/2
      CALL XVREAD(IUNI,IN,STAT,'NSAMPS',NS,' ')
      CALL PRNT(7,NP,IN,' DUMP IN.')
C********************
C     NOW APPLY MATRIX TRANSFORMATION TO IN
C********************
      NPTS=NS/2
      DO 11 I=1,NPTS
      OUT(1,I)=R(1,1)*IN(1,I)+R(1,2)*IN(2,I)+O(1)
      OUT(2,I)=R(2,1)*IN(1,I)+R(2,2)*IN(2,I)+O(2)
11    CONTINUE
      CALL PRNT(7,NP,OUT,' DUMP OUT.')
C*********************
C     OK WRITE IT OUT
C*********************
      CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
      IF(STAT.NE.1)GO TO 12
      NLO=1
      NSO=NS
      CALL XVOPEN(OUNI,STAT,'U_NL',NLO,'U_NS',NSO,
     *'OPEN_ACT','SA','IO_ACT','SA','OP','WRITE',' ')
      CALL XVWRIT(OUNI,OUT,STAT,' ')
      CALL XVCLOSE(OUNI,STAT,' ')
12    CONTINUE
      CALL XVCLOSE(IUNI,STAT,' ')
      RETURN
      END
