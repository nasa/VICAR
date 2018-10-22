      INCLUDE 'VICMAIN_FOR'
C Create GALGEN test frames...
C         GGEN (DC,D1,D2,D3,D4,D5,D6,D7,D8)
C
      SUBROUTINE MAIN44
         INTEGER*2 BUF(9,9)
         DATA BUF/
     &         10,10,10,10,10,10,10,10,10,
     &         15,20,20,20,20,20,20,20,20,
     &         10,25,30,30,30,30,30,30,30,
     &         10,20,35,40,40,40,40,40,40,
     &         10,15,30,45,50,50,50,50,50,
     &         10,10,25,40,55,60,60,60,60,
     &         10,10,20,35,50,65,70,70,70,
     &         10,10,15,30,45,60,75,80,80,
     &         10,10,10,25,40,55,70,85,90/

         CALL XVEACTION('SA',' ')
         DO I=1,9
            DO J=1,9
               BUF(I,J) = 2*BUF(I,J)
            ENDDO
         ENDDO

         DO I=1,9
            CALL XVUNIT(OUNIT,'OUT',I,IND,' ')
            CALL XVSIGNAL(OUNIT,IND,.TRUE.)
            CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',1,'U_NS',9,
     &                  'U_FORMAT','HALF','O_FORMAT','HALF',' ')
            CALL XLADD(OUNIT,'HISTORY','PICSCALE',2,IND,'FORMAT',
     &                 'INT',' ')
            CALL XVWRIT(OUNIT,BUF(1,I),IND,' ')
            CALL PRNT(2,9,BUF(1,I),' ')
            CALL XVCLOSE(OUNIT,IND,' ')
         ENDDO

         RETURN
      END
