C TVGRIMFMT.F
C Routine to test VGRIMFMT IN FORTAN.....
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      INTEGER*4 SCAN_RATE
      INTEGER*2 SS(10),NS(10)
      CHARACTER*5 FORMAT
      CHARACTER*80 MS1
C
      DO IM=0,31
	  DO I = 1,10
		SS(I)=0
		NS(I)=0
          ENDDO
          CALL VGRIMFMT(IM,FORMAT,SCAN_RATE,SS,NS,IND)
          IF (IND.EQ.1) THEN
              WRITE(MS1,101) IM,FORMAT,SCAN_RATE
	      CALL XVMESSAGE(MS1,' ')
	      WRITE(MS1,102)
	      CALL XVMESSAGE(MS1, ' ')
	      DO I = 1,SCAN_RATE
		WRITE(MS1,103) SS(I),NS(I)
		CALL XVMESSAGE(MS1, ' ')
	      ENDDO
          ENDIF
      ENDDO

C	Repeat test case in C to test C interface: zvgrimfmt
      
      CALL XVMESSAGE( ' ',' ')
      CALL XVMESSAGE( '    REPEAT ABOVE TESTS FROM C LANGUAGE', ' ')
      CALL XVMESSAGE( ' ',' ')
      CALL TZVGRIMFMT()
C
  101 FORMAT('  IMCODE=',I2,'   FORMAT=',A5,'   SCAN_RATE=',I2)
  102 FORMAT('        ** SS **    ** NS **')
  103 FORMAT('	',I5,'       ',I5)
      END
