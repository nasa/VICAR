C  2-87  SP   TEST PROGRAM WAS NOT WORKING IN SOME CASES BECAUSE IT EMBEDDED
C             3 SPACES BETWEEN EVERY CHARACTER OF FORMULA BEFORE CALLING KNUTH.
C             (BOMBED ON .OR.,.AND. ...)  MODIFIED TO NOT ADD BLANKS.
C  9-90  JFM  TEST EXPANDED TO TEST MORE BAD FUNCTION STRINGS
C  2-92  NDR  TEST EXPANDED TO TEST NEW OPTIONS & MORE BAD FUNCTION STRINGS;
C               "IN1.EQ.-4" SHOULD YIELD VALID RESULT: '-' HAS HIGHER PRIORITY.
C               ELIMINATED DEPENDENCE ON NON-UNIX BINBCD,PRNT.
C  4-92  NDR   TEST EXPANDED TO TEST XKNUTH AND KNUTH_LOOKUP, TRUNCATE & ROUND;
C                ELIMINATED NON-PORTABLE EQUIVALENCES
C 11-94  NDR   TEST EXPANDED TO TEST XKNUTH_COMP COMPLEX VALUED OPS
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INCLUDE 'fortport'
      BYTE TABLE(0:255,0:255)
      BYTE BTABLE(-128:127,-128:127)
      EQUIVALENCE(TABLE,BTABLE)
      INTEGER I,J
      BYTE IB,JB
      CHARACTER*250 FCN
      CHARACTER*80 OUTMSG
      INTEGER*2 IBUF(2,150)
      INTEGER COUNT, IER,INDEX
      REAL FBUF(300),RESULT
      EQUIVALENCE(FBUF,IBUF)
      LOGICAL XVPTST


C --- Get the function
      CALL XVP ('FUNCTION', FCN, COUNT)

C--- Test the VARIABLE INDEX subroutine
      IF (XVPTST('KNVAR')) THEN
          CALL KNUTH_VAR(FCN,INDEX)
          WRITE(UNIT=OUTMSG,FMT='(A,A5,A,I3)')
     +           'VARIABLE "',FCN,'" INDEX=',INDEX
          CALL XVMESSAGE(OUTMSG,' ')
      ENDIF

C --- Compile the function
      CALL KNUTH (FCN,IBUF,IER)
      IF (IER.EQ.2) GO TO 400

C ---    Show compiled function
      CALL KNUTH_DUMP(IBUF)

      IF (XVPTST('XKNUTH')) THEN
	  	IF (XVPTST('COMPLEX')) THEN
			CALL TEST_COMPLEX(FCN)
			RETURN
		 ENDIF
C ---   Execute the function on a few REAL values
          CALL XVMESSAGE('FUNCTION VALUES:',' ')
          DO I = 0,3,3
          DO J = 0,2,2
          FBUF(1) = FLOAT(I)
          FBUF(2) = FLOAT(J)
          CALL XKNUTH(FBUF,RESULT)
          WRITE(UNIT=OUTMSG,FMT='(4X,A,F6.1,A,F6.1,A,F6.1)')
     +           'XKNUTH(',FBUF(1),',',FBUF(2),')=',RESULT
          CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
		  FBUF(1)=130.
		  FBUF(2)=255.
          CALL XKNUTH(FBUF,RESULT)
          WRITE(UNIT=OUTMSG,FMT='(4X,A,F6.1,A,F6.1,A,F6.1)')
     +           'XKNUTH(',FBUF(1),',',FBUF(2),')=',RESULT
          CALL XVMESSAGE(OUTMSG,' ')
    
C ---   Use the Byte lookup table method - two variables & truncate
          CALL KNUTH_LOOKUP(IBUF,TABLE,2,0)
          CALL XVMESSAGE('LOOKUP TABLE VALUES (trunc.,int indx):',' ')
          DO I = 0,3,3
          DO J = 0,2,2
                  WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +            'TABLE(',I,',',J,')=',BYTE2INT( TABLE(I,J) )
                  CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
          CALL KNUTH_LOOKUP(IBUF,BTABLE,2,2)
          CALL XVMESSAGE('LOOKUP TABLE VALUE (trunc,byte indx):',' ')
		  IB=-126   ! 130 unsigned
		  JB=-1     ! 255 unsigned
          WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +          'TABLE(',130,',',255,')=',
     +          BYTE2INT(BTABLE(IB,JB))
          CALL XVMESSAGE(OUTMSG,' ')
    
C ---   Use the Byte lookup table method - two variables & round
          CALL KNUTH_LOOKUP(IBUF,TABLE,2,1)
          CALL XVMESSAGE('LOOKUP TABLE VALUES (round):',' ')
          DO I = 0,3,3
          DO J = 0,2,2
                  WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +           'TABLE(',I,',',J,')=',BYTE2INT( TABLE(I,J) )
                  CALL XVMESSAGE(OUTMSG,' ')
          ENDDO
          ENDDO
C --- Added test for values > 127:
          CALL KNUTH_LOOKUP(IBUF,BTABLE,2,3)
          CALL XVMESSAGE('LOOKUP TABLE VALUE (round,byte index):',' ')
		  IB=-126   ! 130 unsigned
		  JB=-1     ! 255 unsigned
          WRITE(UNIT=OUTMSG,FMT='(4X,A,I4,A,I4,A,I4)')
     +          'TABLE(',130,',',255,')=',
     +          BYTE2INT(BTABLE(IB,JB))
          CALL XVMESSAGE(OUTMSG,' ')
      ENDIF
      RETURN
400   CALL XVMESSAGE('BAD FUNCTION STRING',' ')
      CALL ABEND
      RETURN
      END

      SUBROUTINE TEST_COMPLEX(FCN)
	  IMPLICIT NONE
	  CHARACTER*(*) FCN
	  COMPLEX*8 CBUF(300),CRESULT
	  REAL*4 FBUF(300),FRESULT
	  CHARACTER*80 OUTMSG
	  INTEGER COUNT,status
	  EQUIVALENCE(CBUF,FBUF)
	  

	  CALL KNUTH_COMPLEX(FCN,CBUF,status)	  
	  CALL XVP('CVALUES',FBUF,COUNT)
	  CALL XKNUTH_COMPLEX(CBUF,CRESULT)	  
	  WRITE (OUTMSG,'(A,F7.3,F7.3)') 'COMPLEX-RESULT=',CRESULT
	  CALL XVMESSAGE(OUTMSG,' ')
	  
	  CALL KNUTH(FCN,FBUF,status)	  
	  CALL XVP('CVALUES',FBUF,COUNT)
	  CALL XKNUTH(FBUF,FRESULT)	  
	  WRITE (OUTMSG,'(A,F7.3)') 'REAL-RESULT=',FRESULT
          CALL XVMESSAGE(OUTMSG,' ')

	  RETURN
	  END


