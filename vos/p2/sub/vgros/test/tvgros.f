C     THIS IS THE TEST PROGRAM FOR THE VGROS MODULE FORTRAN SUROUTINE
      SUBROUTINE TVGROS()
      INTEGER*4 ICAM
      REAL*4 OLOC(2,202)
      CHARACTER MSG*100
C
      DO ICAM=4,7
          CALL VGROS(ICAM,OLOC)
          WRITE(MSG,100)ICAM
          CALL XVMESSAGE(MSG,' ')
 100      FORMAT('Voyager OS reseau for camera=', I3)
          CALL XVMESSAGE(' ',' ')
          DO I=1,199,10
               WRITE(MSG,200) I,I+1,I+2,I+3,I+4,I+5,I+6,I+7,I+8,I+9
               CALL XVMESSAGE(MSG,' ')
 200           FORMAT(10I7)
               WRITE(MSG,300) (OLOC(1,J),J=I,I+9)
               CALL XVMESSAGE(MSG,' ')
 300           FORMAT(10F7.1)
               WRITE(MSG,300) (OLOC(2,J),J=I,I+9)
               CALL XVMESSAGE(MSG,' ')
               CALL XVMESSAGE(' ',' ')
               IF (I.EQ.191) THEN
                    CALL XVMESSAGE('   201   202',' ')
                    WRITE(MSG,500) OLOC(1,201),OLOC(1,202)
                    CALL XVMESSAGE(MSG,' ')
 500           FORMAT(2F7.1)
                    WRITE(MSG,500) OLOC(2,201),OLOC(2,202)
                    CALL XVMESSAGE(MSG,' ')
                    CALL XVMESSAGE(' ',' ')               
               ENDIF
          ENDDO
      ENDDO
      RETURN
      END
