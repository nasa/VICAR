       PROGRAM  MAXMIN
C#######################################################################
C  NAME OF ROUTINE
C      MAXMIN ( find MAXimum and MINimum DNs in an image )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program MAXMIN is a VICAR applications program which is used to 
C      find the minimum and maximum DNs in an image.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   STERLING SOFTWARE        4-88
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C      12-88 SP  CORRECTED EXCLUDE PROCESSING FOR CASE WHERE IMAGE STARTS WITH
C                EXCLUDED VALUE.
C       1-93 SP  Made portable for UNIX.
C       9-00 AS  Corrected core dump on SGI and VMS with 
C                goto for DOUB format. REAL*4 and INTEGER
C                variables were causing core dump on SGI and VMS
C                for DOUB format images. Variables were initialized
C                as REAL*8 to avoid core dump. This also corrected incorrect
C                calculation of maxval for Solaris as well as SGI and
C                VMS.
C
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      MAXMIN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      implicit none
      INCLUDE 'pgminc'            ! FOR XQINI... TAE CONSTANTS & PARAMETERS
	integer*4  vblock(xprdim)
	real*8   vblock8(xprdim)

      CHARACTER*7 FORMAT
      CHARACTER*3 ORG
      INTEGER*4 SL,SS,SB,NL,NS,NB,COUNT,DEF,INPUT,STAT
      COMMON /INSZ/ NL,NS,NB
      CHARACTER*132 PBUF
      INTEGER*4 EB,BAND,EL,LINE,ES,SAMP,BUF(16384)
      LOGICAL*4  FIRST, INTE, EXCL
      INTEGER*4  MINI, MAXI, MMINI, MMAXI, MBANDX, MLINEX, MSAMPX,
     .        MBANDN, MLINEN, MSAMPN, N, IMIN, IMAX, DCODE, LINC,
     .        IEXCLUDE
      REAL*4  RMINI, RMAXI, FMINI, FMAXI, EXCLUDE
      REAL*8  RMINI8, RMAXI8, FMINI8, FMAXI8, EXCLUDE8, 
     .        MINI8, MAXI8, IEXCLUDE8

      EQUIVALENCE (RMINI,MINI), (RMAXI,MAXI), (EXCLUDE,IEXCLUDE)
      EQUIVALENCE (RMINI8,MINI8), (RMAXI8,MAXI8), (EXCLUDE8,IEXCLUDE8)

      integer*4 dummy1,dummy2,dummy3    ! no more optional parameters.
C==================================================================
	inte = .false.
	excl = .false.
	call xvmessage ('*** maxmin - 06-Jul-2012',' ')
      FIRST = .TRUE.                     ! FIRST TIME FLAG.
      CALL XVUNIT(INPUT,'INP',1,STAT,' ')
      CALL XVOPEN(INPUT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')

      CALL XVGET(INPUT,STAT,'ORG',ORG, 'FORMAT', FORMAT,' ')
      CALL XVSIZE(SL,SS,NL,NS,dummy1,dummy2)
      CALL XVBANDS(SB,NB,dummy3)

C CONVERT FORMAT FOR SUBROUTINE MINMAX.

      IF ( FORMAT .EQ. 'BYTE' )  THEN
           DCODE = 1
           INTE  = .TRUE.
      ELSE IF ( FORMAT .EQ. 'HALF' ) THEN
           DCODE = 2
           INTE  = .TRUE.
      ELSE IF ( FORMAT .EQ. 'FULL' ) THEN
           DCODE = 4
           INTE  = .TRUE.
      ELSE IF ( FORMAT .EQ. 'REAL' ) THEN
           DCODE = 7
           INTE  = .FALSE.
      ELSE IF ( FORMAT .EQ. 'DOUB' ) THEN
           DCODE = 8
           INTE  = .FALSE.
           GO TO 200
      ELSE IF ( FORMAT(1:4) .EQ. 'COMP' ) THEN
           DCODE = 10
           INTE  = .FALSE.
      ELSE 
           CALL MABEND( '??E - INVALID DATA FORMAT FROM XVGET')
      END IF

      CALL XVPARM( 'LINC', LINC, COUNT, DEF,0 )
      CALL XVPARM( 'EXCLUDE', EXCLUDE, COUNT, DEF,0 )
c      PRINT *, EXCLUDE
c      PRINT *, COUNT
c      PRINT *, DEF
      IF ( COUNT .GT. 0 )  THEN
         IF (INTE)       IEXCLUDE = NINT( EXCLUDE )
         EXCL = .TRUE.
      ELSE
         EXCL = .FALSE. 
      END IF

C      PRINT *, ORG

      IF (ORG .EQ. 'BSQ') THEN
	  EB = SB+NB-1
	  EL = SL+NL-1
	  DO BAND=SB,EB
	      DO LINE=SL,EL, LINC
	  	  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     .			      'LINE',LINE,'BAND',BAND,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.

                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE, MINI, MAXI, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI, MAXI, IMIN, IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     IF (INTE)  THEN    ! FOR INTEGERS
                        MMINI = MINI
                        MMAXI = MAXI
                     ELSE
                        FMINI = RMINI   ! FOR REAL AND COMPLEX
                        FMAXI = RMAXI
                     END IF
                     MBANDX = BAND      ! SAVE LOCATION OF MIN, MAX.
                     MLINEX = LINE
                     MSAMPX = IMAX+SS-1
                     MBANDN = BAND
                     MLINEN = LINE
                     MSAMPN = IMIN+SS-1
                  ELSE IF (EXCL)  THEN
                     IF (INTE)  THEN
                      IF (MINI .NE. IEXCLUDE) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (MMINI .GT. MINI .OR. 
     .                      MMINI .EQ. IEXCLUDE)  THEN
                            MMINI = MINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (MMAXI .LT. MAXI .OR.
     .                      MMAXI .EQ. IEXCLUDE)  THEN
                            MMAXI = MAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                      END IF
                     ELSE
                      IF (RMINI .NE. EXCLUDE)  THEN
                        IF (FMINI .GT. RMINI .OR.
     .                      FMINI .EQ. EXCLUDE)  THEN
                            FMINI = RMINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI .LT. RMAXI .OR.
     .                      FMAXI .EQ. EXCLUDE)  THEN
                            FMAXI = RMAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                      END IF
                     END IF
                  ELSE
                     IF (INTE)  THEN
                        IF (MMINI .GT. MINI)  THEN
                            MMINI = MINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (MMAXI .LT. MAXI)  THEN
                            MMAXI = MAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     ELSE
                        IF (FMINI .GT. RMINI)  THEN
                            FMINI = RMINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI .LT. RMAXI)  THEN
                            FMAXI = RMAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     END IF
                 END IF
	      ENDDO
	  ENDDO
      ELSE IF (ORG .EQ. 'BIL') THEN
	  EL = SL+NL-1
	  EB = SB+NB-1
	  DO LINE=SL,EL, LINC
	      DO BAND=SB,EB
		  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     .			      'BAND',BAND,'LINE',LINE,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.

                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE, MINI, MAXI, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI, MAXI, IMIN, IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     IF (INTE)  THEN    ! FOR INTEGERS
                        MMINI = MINI
                        MMAXI = MAXI
                     ELSE
                        FMINI = RMINI   ! FOR REAL AND COMPLEX
                        FMAXI = RMAXI
                     END IF
                     MBANDX = BAND      ! SAVE LOCATION OF MIN, MAX.
                     MLINEX = LINE
                     MSAMPX = IMAX+SS-1
                     MBANDN = BAND
                     MLINEN = LINE
                     MSAMPN = IMIN+SS-1
                  ELSE IF (EXCL)  THEN
                     IF (INTE)  THEN
                      IF (MINI .NE. IEXCLUDE) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (MMINI .GT. MINI .OR. 
     .                      MMINI .EQ. IEXCLUDE)  THEN
                            MMINI = MINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (MMAXI .LT. MAXI .OR.
     .                      MMAXI .EQ. IEXCLUDE)  THEN
                            MMAXI = MAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                      END IF
                     ELSE
                      IF (RMINI .NE. EXCLUDE) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (FMINI .GT. RMINI .OR.
     .                      FMINI .EQ. EXCLUDE)  THEN
                            FMINI = RMINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI .LT. RMAXI .OR.
     .                      FMAXI .EQ. EXCLUDE)  THEN
                            FMAXI = RMAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                      END IF
                     END IF
                  ELSE
                     IF (INTE)  THEN
                        IF (MMINI .GT. MINI)  THEN
                            MMINI = MINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (MMAXI .LT. MAXI)  THEN
                            MMAXI = MAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     ELSE
                        IF (FMINI .GT. RMINI)  THEN
                            FMINI = RMINI
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI .LT. RMAXI)  THEN
                            FMAXI = RMAXI
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     END IF
                  END IF
	      ENDDO
	  ENDDO
      ELSE		! ORG .EQ. 'BIP'
	  EL = SL+NL-1
	  ES = SS+NS-1
	  DO LINE=SL,EL, LINC
	      DO SAMP=SS,ES
	 	  CALL XVREAD(INPUT,BUF,STAT,'BAND',SB,'NBANDS',NB,
     .			      'SAMP',SAMP,'LINE',LINE,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.


                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE, MINI, MAXI, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI, MAXI, IMIN, IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     IF (INTE)  THEN    ! FOR INTEGERS
                        MMINI = MINI
                        MMAXI = MAXI
                     ELSE
                        FMINI = RMINI   ! FOR REAL AND COMPLEX
                        FMAXI = RMAXI
                     END IF
                     MBANDX = SB+IMAX-1
                     MLINEX = LINE
                     MSAMPX = SAMP      ! SAVE LOCATION OF MIN, MAX.
                     MBANDN = SB+IMIN-1
                     MLINEN = LINE
                     MSAMPN = SAMP
                  ELSE IF (EXCL)  THEN
                     IF (INTE)  THEN
                      IF (MINI .NE. IEXCLUDE) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (MMINI .GT. MINI .OR.
     .                      MMINI .EQ. IEXCLUDE)  THEN
                            MMINI = MINI
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
c                        PRINT *, 'Use MINI'
                        END IF

                        IF (MMAXI .LT. MAXI .OR.
     .                      MMAXI .EQ. IEXCLUDE)  THEN
                            MMAXI = MAXI
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                      END IF
                     ELSE
                      IF (RMINI .NE. EXCLUDE) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (FMINI .GT. RMINI .OR.
     .                      FMINI .EQ. EXCLUDE)  THEN
                            FMINI = RMINI
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
                        END IF

                        IF (FMAXI .LT. RMAXI .OR.
     .                      FMAXI .EQ. EXCLUDE)  THEN
                            FMAXI = RMAXI
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                      END IF
                     END IF
                  ELSE
                     IF (INTE)  THEN
                        IF (MMINI .GT. MINI)  THEN
                            MMINI = MINI
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
                        END IF

                        IF (MMAXI .LT. MAXI)  THEN
                            MMAXI = MAXI
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                     ELSE
                        IF (FMINI .GT. RMINI)  THEN
                            FMINI = RMINI
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
                        END IF

                        IF (FMAXI .LT. RMAXI)  THEN
                            FMAXI = RMAXI
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                     END IF
                  END IF
	      ENDDO
	  ENDDO
      ENDIF

      CALL XVMESSAGE( ' ',' ')

      PBUF(1:47) = ' Min. value: -1.2345e+111   at  (123456,123456)'
      IF (INTE)  THEN
         WRITE (PBUF(13:25),'(I13)') MMINI
      ELSE
         WRITE (PBUF(13:25),'(1PE13.6)') FMINI
      END IF
      WRITE (PBUF(34:39),'(I6)') MLINEN
      WRITE (PBUF(41:46),'(I6)') MSAMPN

      IF ( NB .GT. 1 )  THEN
         PBUF(48:55) = ' of band'
         WRITE (PBUF(56:61),'(I6)') MBANDN
         N = 61
      ELSE 
         N = 47
      END IF
      CALL XVMESSAGE(PBUF(2:N),' ')

      PBUF(1:47) = ' Max. value: -1.2345e+111   at  (123456,123456)'
      IF (INTE)  THEN
         WRITE (PBUF(13:25),'(I13)') MMAXI
      ELSE
         WRITE (PBUF(13:25),'(1PE13.6)') FMAXI
      END IF
      WRITE (PBUF(34:39),'(I6)') MLINEX
      WRITE (PBUF(41:46),'(I6)') MSAMPX

      IF ( NB .GT. 1 )  THEN
         PBUF(48:55) = ' of band'
         WRITE (PBUF(56:61),'(I6)') MBANDX
         N = 61
      ELSE 
         N = 47
      END IF
      CALL XVMESSAGE(PBUF(2:N),' ')

      CALL XVMESSAGE( ' ',' ')

C		CREATE V-BLOCK TO OUTPUT VALUES TO TCL VARIABLES

	call xqini( vblock, xprdim, xabort)

        IF (INTE)  THEN
           FMINI = MMINI
           FMAXI = MMAXI
        END IF

	call xqreal (vblock, 'MINIVAL', 1, fmini, xadd, stat)
	call xqreal (vblock, 'MAXIVAL', 1, fmaxi, xadd, stat)
        call xqintg (vblock,'MAXLINE',1, mlinex, xadd, stat)
        call xqintg (vblock,'MAXSAMP',1, msampx, xadd, stat)
        call xqintg (vblock,'MINLINE',1, mlinen, xadd, stat)
        call xqintg (vblock,'MINSAMP',1, msampn, xadd, stat)

	Call xvqout( vblock, stat)


 200  IF ( FORMAT .EQ. 'DOUB') THEN      
      CALL XVPARM( 'LINC', LINC, COUNT, DEF,0 )
      CALL XVPARM( 'EXCLUDE', EXCLUDE8, COUNT, DEF,0 )
      IF ( COUNT .GT. 0 )  THEN
           EXCL = .FALSE. 
      END IF

      IF (ORG .EQ. 'BSQ') THEN
	  EB = SB+NB-1
	  EL = SL+NL-1
	  DO BAND=SB,EB
	      DO LINE=SL,EL, LINC
	  	  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     .			      'LINE',LINE,'BAND',BAND,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.

                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE8, MINI8, MAXI8, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI8, MAXI8, IMIN, 
     .                           IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     FMINI8 = RMINI8    ! FOR REAL AND COMPLEX
                     FMAXI8 = RMAXI8
                     MBANDX = BAND      ! SAVE LOCATION OF MIN, MAX.
                     MLINEX = LINE
                     MSAMPX = IMAX+SS-1
                     MBANDN = BAND
                     MLINEN = LINE
                     MSAMPN = IMIN+SS-1
                  ELSE IF (EXCL)  THEN
                      IF (RMINI8 .NE. EXCLUDE8)  THEN
                        IF (FMINI8 .GT. RMINI8 .OR.
     .                      FMINI8 .EQ. EXCLUDE8)  THEN
                            FMINI8 = RMINI8
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI8 .LT. RMAXI8 .OR.
     .                      FMAXI8 .EQ. EXCLUDE8)  THEN
                            FMAXI8 = RMAXI8
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     END IF
                 ELSE
                     IF (FMINI8 .GT. RMINI8)  THEN
                        FMINI8 = RMINI8
                        MBANDN = BAND
                        MLINEN = LINE
                        MSAMPN = IMIN+SS-1
                    END IF

                    IF (FMAXI8 .LT. RMAXI8)  THEN
                        FMAXI8 = RMAXI8
                        MBANDX = BAND
                        MLINEX = LINE
                        MSAMPX = IMAX+SS-1
                    END IF
                 END IF
	      ENDDO
	  ENDDO
      ELSE IF (ORG .EQ. 'BIL') THEN
	  EL = SL+NL-1
	  EB = SB+NB-1
	  DO LINE=SL,EL, LINC
	      DO BAND=SB,EB
		  CALL XVREAD(INPUT,BUF,STAT,'SAMP',SS,'NSAMPS',NS,
     .			      'BAND',BAND,'LINE',LINE,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.

                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE8, MINI8, MAXI8, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI8, MAXI8, IMIN, 
     .                           IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     FMINI8 = RMINI8    ! FOR REAL AND COMPLEX
                     FMAXI8 = RMAXI8
                     MBANDX = BAND      ! SAVE LOCATION OF MIN, MAX.
                     MLINEX = LINE
                     MSAMPX = IMAX+SS-1
                     MBANDN = BAND
                     MLINEN = LINE
                     MSAMPN = IMIN+SS-1
                  ELSE IF (EXCL)  THEN
                      IF (RMINI8 .NE. EXCLUDE8) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (FMINI8 .GT. RMINI8 .OR.
     .                      FMINI8 .EQ. EXCLUDE8)  THEN
                            FMINI8 = RMINI8
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI8 .LT. RMAXI8 .OR.
     .                      FMAXI8 .EQ. EXCLUDE8)  THEN
                            FMAXI8 = RMAXI8
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                     END IF
                  ELSE
                        IF (FMINI8 .GT. RMINI8)  THEN
                            FMINI8 = RMINI8
                            MBANDN = BAND
                            MLINEN = LINE
                            MSAMPN = IMIN+SS-1
                        END IF

                        IF (FMAXI8 .LT. RMAXI8)  THEN
                            FMAXI8 = RMAXI8
                            MBANDX = BAND
                            MLINEX = LINE
                            MSAMPX = IMAX+SS-1
                        END IF
                  END IF
	      ENDDO
	  ENDDO
      ELSE		! ORG .EQ. 'BIP'
	  EL = SL+NL-1
	  ES = SS+NS-1
	  DO LINE=SL,EL, LINC
	      DO SAMP=SS,ES
	 	  CALL XVREAD(INPUT,BUF,STAT,'BAND',SB,'NBANDS',NB,
     .			      'SAMP',SAMP,'LINE',LINE,' ')

C..................FIND MIN & MAX WITH OR WITHOUT EXCLUDE.


                  IF (EXCL)  THEN
                    CALL MINMAXE(DCODE, NS, BUF, EXCLUDE8, MINI8, MAXI8, 
     .                           IMIN, IMAX)
                  ELSE
                    CALL MINMAX(DCODE, NS, BUF, MINI8, MAXI8, IMIN, 
     .                           IMAX)
                  END IF

                  IF (FIRST)  THEN      ! FOR THE FIRST READ.
                     FIRST = .FALSE.
                     FMINI8 = RMINI8   ! FOR REAL AND COMPLEX
                     FMAXI8 = RMAXI8
                     MBANDX = SB+IMAX-1
                     MLINEX = LINE
                     MSAMPX = SAMP      ! SAVE LOCATION OF MIN, MAX.
                     MBANDN = SB+IMIN-1
                     MLINEN = LINE
                     MSAMPN = SAMP
                  ELSE IF (EXCL)  THEN
                      IF (RMINI8 .NE. EXCLUDE8) THEN   !SKIP LINES THAT ARE EXCL.
                        IF (FMINI8 .GT. RMINI8 .OR.
     .                      FMINI8 .EQ. EXCLUDE8)  THEN
                            FMINI8 = RMINI8
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
                        END IF

                        IF (FMAXI8 .LT. RMAXI8 .OR.
     .                      FMAXI8 .EQ. EXCLUDE8)  THEN
                            FMAXI8 = RMAXI8
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                     END IF
                  ELSE
                        IF (FMINI8 .GT. RMINI8)  THEN
                            FMINI8 = RMINI8
                            MBANDN = SB+IMIN-1
                            MLINEN = LINE
                            MSAMPN = SAMP
                        END IF

                        IF (FMAXI8 .LT. RMAXI8)  THEN
                            FMAXI8 = RMAXI8
                            MBANDX = SB+IMAX-1
                            MLINEX = LINE
                            MSAMPX = SAMP
                        END IF
                  END IF
	      ENDDO
	  ENDDO
      ENDIF

      CALL XVMESSAGE( ' ',' ')

      PBUF(1:47) = ' Min. value: -1.2345e+111   at  (123456,123456)'
      WRITE (PBUF(13:25),'(1PE13.6)') FMINI8
      WRITE (PBUF(34:39),'(I6)') MLINEN
      WRITE (PBUF(41:46),'(I6)') MSAMPN

      IF ( NB .GT. 1 )  THEN
         PBUF(48:55) = ' of band'
         WRITE (PBUF(56:61),'(I6)') MBANDN
         N = 61
      ELSE 
         N = 47
      END IF
      CALL XVMESSAGE(PBUF(2:N),' ')

      PBUF(1:47) = ' Max. value: -1.2345e+111   at  (123456,123456)'
      WRITE (PBUF(13:25),'(1PE13.6)') FMAXI8
      WRITE (PBUF(34:39),'(I6)') MLINEX
      WRITE (PBUF(41:46),'(I6)') MSAMPX

      IF ( NB .GT. 1 )  THEN
         PBUF(48:55) = ' of band'
         WRITE (PBUF(56:61),'(I6)') MBANDX
         N = 61
      ELSE 
         N = 47
      END IF
      CALL XVMESSAGE(PBUF(2:N),' ')

      CALL XVMESSAGE( ' ',' ')
C		CREATE V-BLOCK TO OUTPUT VALUES TO TCL VARIABLES

      call xqini( vblock8, xprdim, xabort)

      call xqdble (vblock8, 'MINIVAL', 1, fmini8, xadd, stat)
      call xqdble (vblock8, 'MAXIVAL', 1, fmaxi8, xadd, stat)
	call xqintg (vblock8,'MAXLINE',1, mlinex, xadd, stat)
	call xqintg (vblock8,'MAXSAMP',1, msampx, xadd, stat)
	call xqintg (vblock8,'MINLINE',1, mlinen, xadd, stat)
	call xqintg (vblock8,'MINSAMP',1, msampn, xadd, stat)

      call xvqout( vblock8, stat)
      END IF


      CALL XVCLOSE(INPUT,STAT,' ')
      RETURN
      END
