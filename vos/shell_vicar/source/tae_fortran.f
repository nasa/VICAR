C
C       FORTRAN INTERFACE TO RUNTIME LIBRARY
C
        SUBROUTINE XZSINIT(PARB, DIM, STDLUN, MODE, STATUS)

        INTEGER PARB(1)
        INTEGER DIM
        INTEGER STDLUN
        INTEGER MODE
        INTEGER STATUS

        CHARACTER*1024 COMMAND
        CHARACTER*8192 CMDLINE

        CALL XZARGINIT(COMMAND,CMDLINE)

        CALL XZPARBINIT(PARB,COMMAND,CMDLINE)

        STATUS = 1
        RETURN
        END
C
C
      SUBROUTINE XZARGINIT(COMMAND,CMDLINE)
      CHARACTER*(*) COMMAND,CMDLINE
      INTEGER MAXLEN
      CHARACTER*1024 NEXTARG
      INTEGER I,SLEN,STRLENF,ENDP
      INTEGER IARGC,ARGC

C--- Get the name of the proc:
      CALL GETARG(0,COMMAND)

C--- Concatenate the arguments:
      MAXLEN = LEN(CMDLINE)
      ENDP=1
      ARGC = IARGC()
      DO I=1,ARGC
         CALL GETARG(I,NEXTARG)
         SLEN = STRLENF(NEXTARG)
         IF (ENDP+SLEN .GT. MAXLEN) THEN
            !Pad with spaces and exit
            DO J=ENDP,MAXLEN
               CMDLINE(I:I) = ' '
            ENDDO
            RETURN
         ENDIF
         CMDLINE(ENDP:ENDP+SLEN) = NEXTARG(1:1+SLEN)
         ENDP = ENDP+SLEN+1
      ENDDO

C--- Pad with spaces:
      DO I=ENDP,MAXLEN
         CMDLINE(I:I) = ' '
      ENDDO

      RETURN
      END



      FUNCTION STRLENF(STRN)
      INTEGER STRLENF
      CHARACTER*(*) STRN
      INTEGER SLEN

      SLEN = LEN(STRN)
      DO I=SLEN,1,-1
        IF (STRN(I:I) .NE. ' ') THEN
           STRLENF = I
           RETURN
        ENDIF
      ENDDO
      STRLENF = 0
      RETURN
      END

