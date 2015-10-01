$!****************************************************************************
$!
$! Build proc for MIPL module maxmin
$! VPACK Version 1.9, Wednesday, October 17, 2012, 19:51:57
$!
$! Execute by entering:		$ @maxmin
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module maxmin ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to maxmin.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("maxmin.imake") .nes. ""
$   then
$      vimake maxmin
$      purge maxmin.bld
$   else
$      if F$SEARCH("maxmin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maxmin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maxmin.bld "STD"
$   else
$      @maxmin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maxmin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maxmin.com -mixed -
	-s maxmin.f -
	-i maxmin.imake -
	-p maxmin.pdf -
	-t tstmaxmin.pdf tstmaxmin.log_rjb tstmaxmin.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maxmin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create maxmin.imake
#define  PROGRAM   maxmin

#define MODULE_LIST maxmin.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define LIB_LOCAL*/
$ Return
$!#############################################################################
$PDF_File:
$ create maxmin.pdf
process help=*
  LOCAL DUMMY  TYPE=REAL
  LOCAL IDUMMY TYPE=INTEGER
  PARM INP  TYPE=STRING
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM BANDS TYPE=INTEGER COUNT=0:2 DEFAULT=--
  PARM SL INTEGER DEFAULT=1
  PARM SS INTEGER DEFAULT=1
  PARM SB INTEGER DEFAULT=1
  PARM NL INTEGER DEFAULT=0
  PARM NS INTEGER DEFAULT=0
  PARM NB INTEGER DEFAULT=0

  PARM LINC     INTEGER            VALID=(1:99999) DEFAULT=1
  PARM EXCLUDE  REAL     COUNT=(0:1)               DEFAULT=--

  PARM MINIVAL NAME DEFAULT=DUMMY
  PARM MAXIVAL NAME DEFAULT=DUMMY
  PARM MAXLINE NAME DEFAULT=IDUMMY
  PARM MAXSAMP NAME DEFAULT=IDUMMY
  PARM MINLINE NAME DEFAULT=IDUMMY
  PARM MINSAMP NAME DEFAULT=IDUMMY

!# annot function="VICAR Procedure Generation"
!# annot keywords=(minimum,maximum,"DN values",TCL,linc,EXCLUDE,+
!# "Output parameter","TAE variable")
END-PROC
.title
Computes the min and max DN and their first locations and 
copies these to TAE variables
.help
 PURPOSE:

MAXMIN is a VICAR program which finds and prints the minimum and 
maximum DN values in an image.  It optionally returns these values
in TCL variables.
.PAGE
EXECUTION

MAXMIN supports two and three dimensional VICAR images and all valid
VICAR data types.  For complex data, MAXMIN finds the values of minimum 
and maximum magnitude (Fortran CABS function) and reports these values 
as REAL*4 values.  MAXMIN supports all valid VICAR file organizations
(BSQ,BIL,BIP).

MAXMIN prints the location in (line,sample) format for both the minimum
and maximum location.  The band number is also printed if the number 
of bands is greater than 1.  If the minimum or maximum value occur
more than once, the location of the first occurrence will be printed.

The execution time of MAXMIN can be reduced by specifying a LINC parameter
value greater than 1.  In this case, only one out of every LINC lines
in the image is examined.  This may produce acceptable results if the image
data is fairly continuous and if an approximate minimum and maximum are
acceptable.

An EXCLUDE value can be specified to exclude a particular value in the
search for the minimum and maximum.   
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:

      maxmin INP=a
      maxmin INP=a SIZE=(sl,ss,nl,ns) 
      maxmin INP=a SIZE=(sl,ss,nl,ns) BANDS=(sb,nb)
      maxmin a (sl,ss,nl,ns) (sb,nb) opt. parameters MINIVAL=var1 MAXIVAL=var2

       Here 'a' represents the input image file name,
       and 'var1' and 'var2' represent TCL variables of type real
       in which the minimum and maximum values are returned.
.PAGE
EXAMPLES

1.    maxmin INP=A BANDS=(5,2) MINIVAL=MINVALA MAXIVAL=MAXVALA EXCLUDE=0

      In this example MAXMIN finds the minimum and maximum values (excluding 0)
      for the portion of image A contained in bands 5 and 6.
      These values are returned in the TCL variables MINVALA, MAXVALA.

2.    The following example show how to convert a double precision
      image to byte format using appropriate scaling obtained from
      MAXMIN output.  Program CFORM will map the minimum value to 0
      and the maximum value to 255.

        procedure
        refgbl $echo
        LOCAL minvar REAL
        LOCAL maxvar REAL
        body
        let _onfail="continue"
        let $echo="yes"
        gen a 10 10 'REAL8
        maxmin a MINIVAL=minvar MAXIVAL=maxvar
        putmsg "Stretch this image to byte format" ""
        cform a b 'BYTE IRANGE=(&minvar, &maxvar) ORANGE=(0, 255)
        end-proc

3.    The following example show how to get a histogram of a REAL*4
      image using as histogram limits the minimum and maximum obtained from
      MAXMIN output.  Program HIST is used to generate the histogram.

        procedure
        refgbl $echo
        LOCAL minvar REAL
        LOCAL maxvar REAL
        body
        let _onfail="continue"
        let $echo="yes"
        gen a 10 10 ival=-100 sinc= 5500 'REAL
        maxmin a MINIVAL=minvar MAXIVAL=maxvar
        hist a limits=(&minvar, &maxvar) 
        end-proc
.PAGE
 RESTRICTIONS
1. The maximum number of bytes per line is 65536.

 WRITTEN BY:             Steve Pohorsky              15 April 1988

 COGNIZANT PROGRAMMER:   Steve Pohorsky              15 April 1988
 PORTED TO UNIX: Steve Pohorsky

 REVISION HISTORY
  1993-01-08    S. Pohorsky   Made portable for UNIX.  
  2010-12-04    R.J. Bambery  Modifications for 64bit Linux
  2012-07-06    R.J. Bambery  Removed some debugging Print statements

.level1
.vari inp
Input file name
.vari size
Window into input
.vari bands
Window into input
in band dimension
.vari sl
Starting line
= size(1)
.vari ss
Starting sample
= size(2)
.vari sb
Starting band
= bands(1)
.vari nl
Number of lines
= size(3)
.vari ns
Number of samples
= size(4)
.vari nb
Number of bands
= bands(2)
.VARI LINC
Increment for skipping lines.
.VARI EXCLUDE
Value to exclude in search for
minimum and maximum.
.vari minival
Output TCL variable : min. value
.vari maxival
Output TCL variable : max. value
.vari maxline
Output TCL variable : line 
location of 1st occurrence of max
.vari maxsamp
Output TCL variable : samp 
location of 1st occurence of max
.vari minline
Output TCL variable : line 
location of 1st occurence of min
.vari minsamp
Output TCL variable : samp i
location of 1st occurence of min


.level2
.vari inp
Name of a single input file.
.vari out
Name of a single output file.
.vari size
The size parameter determines the boundaries in the input
file from which the MAXMIN is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.vari bands
The bands parameter determines the bands in the input
file from which the MAXMIN is to take place.  It is specified
as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be copied
.vari linc
The execution time of MAXMIN can be reduced by specifying a LINC parameter
value greater than 1.  In this case, only one out of every LINC lines
in the image is examined.  This may produce acceptable results if the image
data is fairly continuous and if an approximate minimum and maximum are
acceptable.  The default for LINC is 1.
.vari exclude
An EXCLUDE value can be specified to exclude a particular value in the
search for the minimum and maximum.   The EXCLUDE parameter is a REAL*4 value,
and is rounded to the nearest integer for images with integral pixel values.
For images of the complex data type, EXCLUDE is REAL*4, and it is the 
magnitudes of the pixel values that are compare against EXCLUDE.

In MAXMIN, the minimum and maximum are set to EXCLUDE if all of
the values in the image are equal to EXCLUDE.  (That would be a flat image.)

The default is to not exclude any values
.vari minival
MAXMIN optionally returns the minimum pixel value (DN) in the image into
a TCL variable (of type real).  The command line syntax for this parameter
is MINIVAL=var1, where 'var1' represents the TCL variable of type real
in which the minimum value is returned.
The minimum value is returned if and only if this parameter is specified.
.vari maxival
MAXMIN optionally returns the maximum pixel value (DN) in the image into
a TCL variable (of type real).  The command line syntax for this parameter
is MAXIVAL=var1, where 'var1' represents the TCL variable of type real
in which the maximum value is returned.
The maximum value is returned if and only if this parameter is specified.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstmaxmin.pdf
procedure
local minvar REAL
local maxvar REAL

refgbl $echo
refgbl $autousage
! Jun 22, 2012 - RJB
! TEST SCRIPT FOR IMGSTAT
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       putmsg gen list disp cform  
! 
! parameters:
!   <none>
!
! Requires NO external test data: 

body
let _onfail="stop"
let $echo="no"
let $autousage="none"
putmsg " " ""
putmsg " TEST FOR BYTE DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen a 10 10
list a 'NOEJECT
maxmin a
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
let $echo="no"
putmsg "3D BSQ files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bsq
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg "3D BIL files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bil
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) sb=2 nb=3
let $echo="no"
putmsg "3D BIP files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bip
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg " " ""
putmsg " TEST FOR HALFWORD DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen a 10 10 'HALF
list a 'NOEJECT
maxmin a
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
let $echo="no"
putmsg "3D BSQ files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bsq 'HALF
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg "3D BIL files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bil 'HALF
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) sb=2 nb=3
let $echo="no"
putmsg "3D BIP files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bip 'HALF
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg " " ""
putmsg " TEST FOR FULLWORD DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen a 10 10 'FULL
list a 'NOEJECT
maxmin a
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
putmsg "3D BSQ files" ""
gen a 10 10 nb=5 org=bsq 'FULL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg "3D BIL files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bil 'FULL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) sb=2 nb=3
let $echo="no"
putmsg "3D BIP files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bip 'FULL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg " " ""
putmsg " TEST FOR REAL*4 DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen a 10 10 'REAL
list a 'NOEJECT
maxmin a
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
let $echo="no"
putmsg "3D BSQ files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bsq 'REAL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg "3D BIL files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bil 'REAL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) sb=2 nb=3
let $echo="no"
putmsg "3D BIP files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bip 'REAL
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg " " ""
putmsg " TEST FOR REAL*8 DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen a 10 10 'REAL8
list a 'NOEJECT
maxmin a MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Stretch this image to byte format" ""
putmsg "Image b should cover the range 0 to 255." ""
let $echo="yes"
cform a b 'BYTE IRANGE=(&minvar, &maxvar) ORANGE=(0, 255)
list b 'NOEJECT
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
let $echo="no"
putmsg "3D BSQ files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bsq 'REAL8
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg "3D BIL files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bil 'REAL8
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) sb=2 nb=3
let $echo="no"
putmsg "3D BIP files" ""
let $echo="yes"
gen a 10 10 nb=5 org=bip 'REAL8
list a 'NOEJECT
maxmin a 
maxmin a (3,3,3,3) (2,3)
let $echo="no"
putmsg " " ""
putmsg " TEST FOR COMPLEX DATA" ""
putmsg " " ""
putmsg " " ""
putmsg "2D files compatibility check" ""
let $echo="yes"
gen aa 10 10 IVAL=-2 'half
cform aa a OFORM=COMPLEX
list a 'NOEJECT
maxmin a
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
let $echo="no"
putmsg "Print minimum and maximum variables" ""
disp minvar
disp maxvar
let $echo="yes"
maxmin a (3,3,3,3)
let $echo="no"
putmsg "Try some miscellaneous EXCLUDE tests." ""
putmsg "Should get min=1, max=9" ""
let $echo="yes"
gen a 10 10 sinc=0
maxmin a EXCLUDE=0
let $echo="no" 
putmsg "Should get min=0, max=8" ""
let $echo="yes"
maxmin a EXCLUDE=9
let $echo="no"
putmsg "Should get min=1, max=18" ""
let $echo="yes"
gen a 10 10 linc=-1 ival=9
maxmin a EXCLUDE=0
let $echo="no" 
putmsg "Should get min=0, max=17" ""
let $echo="yes"
maxmin a EXCLUDE=18
let $echo="no"

! clean up
ush rm a
ush rm aa
ush rm b

end-proc
$!-----------------------------------------------------------------------------
$ create tstmaxmin.log_rjb
tstmaxmin
 
 TEST FOR BYTE DATA
 
 
2D files compatibility check
gen a 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14
      6       6   7   8   9  10  11  12  13  14  15
      7       7   8   9  10  11  12  13  14  15  16
      8       8   9  10  11  12  13  14  15  16  17
      9       9  10  11  12  13  14  15  16  17  18
     10      10  11  12  13  14  15  16  17  18  19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15
      6       7   8   9  10  11  12  13  14  15  16
      7       8   9  10  11  12  13  14  15  16  17
      8       9  10  11  12  13  14  15  16  17  18
      9      10  11  12  13  14  15  16  17  18  19
     10      11  12  13  14  15  16  17  18  19  20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5       7       9
   Line
      1       3   4   5   6   7   8   9  10  11  12
      2       4   5   6   7   8   9  10  11  12  13
      3       5   6   7   8   9  10  11  12  13  14
      4       6   7   8   9  10  11  12  13  14  15
      5       7   8   9  10  11  12  13  14  15  16
      6       8   9  10  11  12  13  14  15  16  17
      7       9  10  11  12  13  14  15  16  17  18
      8      10  11  12  13  14  15  16  17  18  19
      9      11  12  13  14  15  16  17  18  19  20
     10      12  13  14  15  16  17  18  19  20  21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5       7       9
   Line
      1       4   5   6   7   8   9  10  11  12  13
      2       5   6   7   8   9  10  11  12  13  14
      3       6   7   8   9  10  11  12  13  14  15
      4       7   8   9  10  11  12  13  14  15  16
      5       8   9  10  11  12  13  14  15  16  17
      6       9  10  11  12  13  14  15  16  17  18
      7      10  11  12  13  14  15  16  17  18  19
      8      11  12  13  14  15  16  17  18  19  20
      9      12  13  14  15  16  17  18  19  20  21
     10      13  14  15  16  17  18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Samp     1       3       5       7       9
   Band
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Samp     1       3       5       7       9
   Band
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Samp     1       3       5       7       9
   Band
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Samp     1       3       5       7       9
   Band
      1       3   4   5   6   7   8   9  10  11  12
      2       4   5   6   7   8   9  10  11  12  13
      3       5   6   7   8   9  10  11  12  13  14
      4       6   7   8   9  10  11  12  13  14  15
      5       7   8   9  10  11  12  13  14  15  16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Samp     1       3       5       7       9
   Band
      1       4   5   6   7   8   9  10  11  12  13
      2       5   6   7   8   9  10  11  12  13  14
      3       6   7   8   9  10  11  12  13  14  15
      4       7   8   9  10  11  12  13  14  15  16
      5       8   9  10  11  12  13  14  15  16  17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Samp     1       3       5       7       9
   Band
      1       5   6   7   8   9  10  11  12  13  14
      2       6   7   8   9  10  11  12  13  14  15
      3       7   8   9  10  11  12  13  14  15  16
      4       8   9  10  11  12  13  14  15  16  17
      5       9  10  11  12  13  14  15  16  17  18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Samp     1       3       5       7       9
   Band
      1       6   7   8   9  10  11  12  13  14  15
      2       7   8   9  10  11  12  13  14  15  16
      3       8   9  10  11  12  13  14  15  16  17
      4       9  10  11  12  13  14  15  16  17  18
      5      10  11  12  13  14  15  16  17  18  19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Samp     1       3       5       7       9
   Band
      1       7   8   9  10  11  12  13  14  15  16
      2       8   9  10  11  12  13  14  15  16  17
      3       9  10  11  12  13  14  15  16  17  18
      4      10  11  12  13  14  15  16  17  18  19
      5      11  12  13  14  15  16  17  18  19  20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Samp     1       3       5       7       9
   Band
      1       8   9  10  11  12  13  14  15  16  17
      2       9  10  11  12  13  14  15  16  17  18
      3      10  11  12  13  14  15  16  17  18  19
      4      11  12  13  14  15  16  17  18  19  20
      5      12  13  14  15  16  17  18  19  20  21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Samp     1       3       5       7       9
   Band
      1       9  10  11  12  13  14  15  16  17  18
      2      10  11  12  13  14  15  16  17  18  19
      3      11  12  13  14  15  16  17  18  19  20
      4      12  13  14  15  16  17  18  19  20  21
      5      13  14  15  16  17  18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Band     1       3       5
   Samp
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
      6       5   6   7   8   9
      7       6   7   8   9  10
      8       7   8   9  10  11
      9       8   9  10  11  12
     10       9  10  11  12  13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Band     1       3       5
   Samp
      1       1   2   3   4   5
      2       2   3   4   5   6
      3       3   4   5   6   7
      4       4   5   6   7   8
      5       5   6   7   8   9
      6       6   7   8   9  10
      7       7   8   9  10  11
      8       8   9  10  11  12
      9       9  10  11  12  13
     10      10  11  12  13  14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Band     1       3       5
   Samp
      1       2   3   4   5   6
      2       3   4   5   6   7
      3       4   5   6   7   8
      4       5   6   7   8   9
      5       6   7   8   9  10
      6       7   8   9  10  11
      7       8   9  10  11  12
      8       9  10  11  12  13
      9      10  11  12  13  14
     10      11  12  13  14  15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Band     1       3       5
   Samp
      1       3   4   5   6   7
      2       4   5   6   7   8
      3       5   6   7   8   9
      4       6   7   8   9  10
      5       7   8   9  10  11
      6       8   9  10  11  12
      7       9  10  11  12  13
      8      10  11  12  13  14
      9      11  12  13  14  15
     10      12  13  14  15  16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Band     1       3       5
   Samp
      1       4   5   6   7   8
      2       5   6   7   8   9
      3       6   7   8   9  10
      4       7   8   9  10  11
      5       8   9  10  11  12
      6       9  10  11  12  13
      7      10  11  12  13  14
      8      11  12  13  14  15
      9      12  13  14  15  16
     10      13  14  15  16  17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Band     1       3       5
   Samp
      1       5   6   7   8   9
      2       6   7   8   9  10
      3       7   8   9  10  11
      4       8   9  10  11  12
      5       9  10  11  12  13
      6      10  11  12  13  14
      7      11  12  13  14  15
      8      12  13  14  15  16
      9      13  14  15  16  17
     10      14  15  16  17  18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Band     1       3       5
   Samp
      1       6   7   8   9  10
      2       7   8   9  10  11
      3       8   9  10  11  12
      4       9  10  11  12  13
      5      10  11  12  13  14
      6      11  12  13  14  15
      7      12  13  14  15  16
      8      13  14  15  16  17
      9      14  15  16  17  18
     10      15  16  17  18  19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Band     1       3       5
   Samp
      1       7   8   9  10  11
      2       8   9  10  11  12
      3       9  10  11  12  13
      4      10  11  12  13  14
      5      11  12  13  14  15
      6      12  13  14  15  16
      7      13  14  15  16  17
      8      14  15  16  17  18
      9      15  16  17  18  19
     10      16  17  18  19  20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Band     1       3       5
   Samp
      1       8   9  10  11  12
      2       9  10  11  12  13
      3      10  11  12  13  14
      4      11  12  13  14  15
      5      12  13  14  15  16
      6      13  14  15  16  17
      7      14  15  16  17  18
      8      15  16  17  18  19
      9      16  17  18  19  20
     10      17  18  19  20  21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Band     1       3       5
   Samp
      1       9  10  11  12  13
      2      10  11  12  13  14
      3      11  12  13  14  15
      4      12  13  14  15  16
      5      13  14  15  16  17
      6      14  15  16  17  18
      7      15  16  17  18  19
      8      16  17  18  19  20
      9      17  18  19  20  21
     10      18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR HALFWORD DATA
 
 
2D files compatibility check
gen a 10 10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14
      6         6     7     8     9    10    11    12    13    14    15
      7         7     8     9    10    11    12    13    14    15    16
      8         8     9    10    11    12    13    14    15    16    17
      9         9    10    11    12    13    14    15    16    17    18
     10        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15
      6         7     8     9    10    11    12    13    14    15    16
      7         8     9    10    11    12    13    14    15    16    17
      8         9    10    11    12    13    14    15    16    17    18
      9        10    11    12    13    14    15    16    17    18    19
     10        11    12    13    14    15    16    17    18    19    20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     4
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         3     4     5     6     7     8     9    10    11    12
      2         4     5     6     7     8     9    10    11    12    13
      3         5     6     7     8     9    10    11    12    13    14
      4         6     7     8     9    10    11    12    13    14    15
      5         7     8     9    10    11    12    13    14    15    16
      6         8     9    10    11    12    13    14    15    16    17
      7         9    10    11    12    13    14    15    16    17    18
      8        10    11    12    13    14    15    16    17    18    19
      9        11    12    13    14    15    16    17    18    19    20
     10        12    13    14    15    16    17    18    19    20    21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     5
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         4     5     6     7     8     9    10    11    12    13
      2         5     6     7     8     9    10    11    12    13    14
      3         6     7     8     9    10    11    12    13    14    15
      4         7     8     9    10    11    12    13    14    15    16
      5         8     9    10    11    12    13    14    15    16    17
      6         9    10    11    12    13    14    15    16    17    18
      7        10    11    12    13    14    15    16    17    18    19
      8        11    12    13    14    15    16    17    18    19    20
      9        12    13    14    15    16    17    18    19    20    21
     10        13    14    15    16    17    18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         3     4     5     6     7     8     9    10    11    12
      2         4     5     6     7     8     9    10    11    12    13
      3         5     6     7     8     9    10    11    12    13    14
      4         6     7     8     9    10    11    12    13    14    15
      5         7     8     9    10    11    12    13    14    15    16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         4     5     6     7     8     9    10    11    12    13
      2         5     6     7     8     9    10    11    12    13    14
      3         6     7     8     9    10    11    12    13    14    15
      4         7     8     9    10    11    12    13    14    15    16
      5         8     9    10    11    12    13    14    15    16    17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         5     6     7     8     9    10    11    12    13    14
      2         6     7     8     9    10    11    12    13    14    15
      3         7     8     9    10    11    12    13    14    15    16
      4         8     9    10    11    12    13    14    15    16    17
      5         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         6     7     8     9    10    11    12    13    14    15
      2         7     8     9    10    11    12    13    14    15    16
      3         8     9    10    11    12    13    14    15    16    17
      4         9    10    11    12    13    14    15    16    17    18
      5        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         7     8     9    10    11    12    13    14    15    16
      2         8     9    10    11    12    13    14    15    16    17
      3         9    10    11    12    13    14    15    16    17    18
      4        10    11    12    13    14    15    16    17    18    19
      5        11    12    13    14    15    16    17    18    19    20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         8     9    10    11    12    13    14    15    16    17
      2         9    10    11    12    13    14    15    16    17    18
      3        10    11    12    13    14    15    16    17    18    19
      4        11    12    13    14    15    16    17    18    19    20
      5        12    13    14    15    16    17    18    19    20    21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         9    10    11    12    13    14    15    16    17    18
      2        10    11    12    13    14    15    16    17    18    19
      3        11    12    13    14    15    16    17    18    19    20
      4        12    13    14    15    16    17    18    19    20    21
      5        13    14    15    16    17    18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Band       1     2     3     4     5
   Samp
      1         0     1     2     3     4
      2         1     2     3     4     5
      3         2     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
      6         5     6     7     8     9
      7         6     7     8     9    10
      8         7     8     9    10    11
      9         8     9    10    11    12
     10         9    10    11    12    13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Band       1     2     3     4     5
   Samp
      1         1     2     3     4     5
      2         2     3     4     5     6
      3         3     4     5     6     7
      4         4     5     6     7     8
      5         5     6     7     8     9
      6         6     7     8     9    10
      7         7     8     9    10    11
      8         8     9    10    11    12
      9         9    10    11    12    13
     10        10    11    12    13    14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Band       1     2     3     4     5
   Samp
      1         2     3     4     5     6
      2         3     4     5     6     7
      3         4     5     6     7     8
      4         5     6     7     8     9
      5         6     7     8     9    10
      6         7     8     9    10    11
      7         8     9    10    11    12
      8         9    10    11    12    13
      9        10    11    12    13    14
     10        11    12    13    14    15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Band       1     2     3     4     5
   Samp
      1         3     4     5     6     7
      2         4     5     6     7     8
      3         5     6     7     8     9
      4         6     7     8     9    10
      5         7     8     9    10    11
      6         8     9    10    11    12
      7         9    10    11    12    13
      8        10    11    12    13    14
      9        11    12    13    14    15
     10        12    13    14    15    16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Band       1     2     3     4     5
   Samp
      1         4     5     6     7     8
      2         5     6     7     8     9
      3         6     7     8     9    10
      4         7     8     9    10    11
      5         8     9    10    11    12
      6         9    10    11    12    13
      7        10    11    12    13    14
      8        11    12    13    14    15
      9        12    13    14    15    16
     10        13    14    15    16    17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Band       1     2     3     4     5
   Samp
      1         5     6     7     8     9
      2         6     7     8     9    10
      3         7     8     9    10    11
      4         8     9    10    11    12
      5         9    10    11    12    13
      6        10    11    12    13    14
      7        11    12    13    14    15
      8        12    13    14    15    16
      9        13    14    15    16    17
     10        14    15    16    17    18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Band       1     2     3     4     5
   Samp
      1         6     7     8     9    10
      2         7     8     9    10    11
      3         8     9    10    11    12
      4         9    10    11    12    13
      5        10    11    12    13    14
      6        11    12    13    14    15
      7        12    13    14    15    16
      8        13    14    15    16    17
      9        14    15    16    17    18
     10        15    16    17    18    19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Band       1     2     3     4     5
   Samp
      1         7     8     9    10    11
      2         8     9    10    11    12
      3         9    10    11    12    13
      4        10    11    12    13    14
      5        11    12    13    14    15
      6        12    13    14    15    16
      7        13    14    15    16    17
      8        14    15    16    17    18
      9        15    16    17    18    19
     10        16    17    18    19    20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Band       1     2     3     4     5
   Samp
      1         8     9    10    11    12
      2         9    10    11    12    13
      3        10    11    12    13    14
      4        11    12    13    14    15
      5        12    13    14    15    16
      6        13    14    15    16    17
      7        14    15    16    17    18
      8        15    16    17    18    19
      9        16    17    18    19    20
     10        17    18    19    20    21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Band       1     2     3     4     5
   Samp
      1         9    10    11    12    13
      2        10    11    12    13    14
      3        11    12    13    14    15
      4        12    13    14    15    16
      5        13    14    15    16    17
      6        14    15    16    17    18
      7        15    16    17    18    19
      8        16    17    18    19    20
      9        17    18    19    20    21
     10        18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR FULLWORD DATA
 
 
2D files compatibility check
gen a 10 10 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13
      6              5          6          7          8          9         10         11         12         13         14
      7              6          7          8          9         10         11         12         13         14         15
      8              7          8          9         10         11         12         13         14         15         16
      9              8          9         10         11         12         13         14         15         16         17
     10              9         10         11         12         13         14         15         16         17         18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

putmsg "3D BSQ files" ""
3D BSQ files
gen a 10 10 nb=5 org=bsq 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     1
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13
      6              5          6          7          8          9         10         11         12         13         14
      7              6          7          8          9         10         11         12         13         14         15
      8              7          8          9         10         11         12         13         14         15         16
      9              8          9         10         11         12         13         14         15         16         17
     10              9         10         11         12         13         14         15         16         17         18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     2
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              1          2          3          4          5          6          7          8          9         10
      2              2          3          4          5          6          7          8          9         10         11
      3              3          4          5          6          7          8          9         10         11         12
      4              4          5          6          7          8          9         10         11         12         13
      5              5          6          7          8          9         10         11         12         13         14
      6              6          7          8          9         10         11         12         13         14         15
      7              7          8          9         10         11         12         13         14         15         16
      8              8          9         10         11         12         13         14         15         16         17
      9              9         10         11         12         13         14         15         16         17         18
     10             10         11         12         13         14         15         16         17         18         19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     3
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              2          3          4          5          6          7          8          9         10         11
      2              3          4          5          6          7          8          9         10         11         12
      3              4          5          6          7          8          9         10         11         12         13
      4              5          6          7          8          9         10         11         12         13         14
      5              6          7          8          9         10         11         12         13         14         15
      6              7          8          9         10         11         12         13         14         15         16
      7              8          9         10         11         12         13         14         15         16         17
      8              9         10         11         12         13         14         15         16         17         18
      9             10         11         12         13         14         15         16         17         18         19
     10             11         12         13         14         15         16         17         18         19         20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     4
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              3          4          5          6          7          8          9         10         11         12
      2              4          5          6          7          8          9         10         11         12         13
      3              5          6          7          8          9         10         11         12         13         14
      4              6          7          8          9         10         11         12         13         14         15
      5              7          8          9         10         11         12         13         14         15         16
      6              8          9         10         11         12         13         14         15         16         17
      7              9         10         11         12         13         14         15         16         17         18
      8             10         11         12         13         14         15         16         17         18         19
      9             11         12         13         14         15         16         17         18         19         20
     10             12         13         14         15         16         17         18         19         20         21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     5
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              4          5          6          7          8          9         10         11         12         13
      2              5          6          7          8          9         10         11         12         13         14
      3              6          7          8          9         10         11         12         13         14         15
      4              7          8          9         10         11         12         13         14         15         16
      5              8          9         10         11         12         13         14         15         16         17
      6              9         10         11         12         13         14         15         16         17         18
      7             10         11         12         13         14         15         16         17         18         19
      8             11         12         13         14         15         16         17         18         19         20
      9             12         13         14         15         16         17         18         19         20         21
     10             13         14         15         16         17         18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              1          2          3          4          5          6          7          8          9         10
      2              2          3          4          5          6          7          8          9         10         11
      3              3          4          5          6          7          8          9         10         11         12
      4              4          5          6          7          8          9         10         11         12         13
      5              5          6          7          8          9         10         11         12         13         14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              2          3          4          5          6          7          8          9         10         11
      2              3          4          5          6          7          8          9         10         11         12
      3              4          5          6          7          8          9         10         11         12         13
      4              5          6          7          8          9         10         11         12         13         14
      5              6          7          8          9         10         11         12         13         14         15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              3          4          5          6          7          8          9         10         11         12
      2              4          5          6          7          8          9         10         11         12         13
      3              5          6          7          8          9         10         11         12         13         14
      4              6          7          8          9         10         11         12         13         14         15
      5              7          8          9         10         11         12         13         14         15         16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              4          5          6          7          8          9         10         11         12         13
      2              5          6          7          8          9         10         11         12         13         14
      3              6          7          8          9         10         11         12         13         14         15
      4              7          8          9         10         11         12         13         14         15         16
      5              8          9         10         11         12         13         14         15         16         17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              5          6          7          8          9         10         11         12         13         14
      2              6          7          8          9         10         11         12         13         14         15
      3              7          8          9         10         11         12         13         14         15         16
      4              8          9         10         11         12         13         14         15         16         17
      5              9         10         11         12         13         14         15         16         17         18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              6          7          8          9         10         11         12         13         14         15
      2              7          8          9         10         11         12         13         14         15         16
      3              8          9         10         11         12         13         14         15         16         17
      4              9         10         11         12         13         14         15         16         17         18
      5             10         11         12         13         14         15         16         17         18         19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              7          8          9         10         11         12         13         14         15         16
      2              8          9         10         11         12         13         14         15         16         17
      3              9         10         11         12         13         14         15         16         17         18
      4             10         11         12         13         14         15         16         17         18         19
      5             11         12         13         14         15         16         17         18         19         20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              8          9         10         11         12         13         14         15         16         17
      2              9         10         11         12         13         14         15         16         17         18
      3             10         11         12         13         14         15         16         17         18         19
      4             11         12         13         14         15         16         17         18         19         20
      5             12         13         14         15         16         17         18         19         20         21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              9         10         11         12         13         14         15         16         17         18
      2             10         11         12         13         14         15         16         17         18         19
      3             11         12         13         14         15         16         17         18         19         20
      4             12         13         14         15         16         17         18         19         20         21
      5             13         14         15         16         17         18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Band            1          2          3          4          5
   Samp
      1              0          1          2          3          4
      2              1          2          3          4          5
      3              2          3          4          5          6
      4              3          4          5          6          7
      5              4          5          6          7          8
      6              5          6          7          8          9
      7              6          7          8          9         10
      8              7          8          9         10         11
      9              8          9         10         11         12
     10              9         10         11         12         13


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Band            1          2          3          4          5
   Samp
      1              1          2          3          4          5
      2              2          3          4          5          6
      3              3          4          5          6          7
      4              4          5          6          7          8
      5              5          6          7          8          9
      6              6          7          8          9         10
      7              7          8          9         10         11
      8              8          9         10         11         12
      9              9         10         11         12         13
     10             10         11         12         13         14


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Band            1          2          3          4          5
   Samp
      1              2          3          4          5          6
      2              3          4          5          6          7
      3              4          5          6          7          8
      4              5          6          7          8          9
      5              6          7          8          9         10
      6              7          8          9         10         11
      7              8          9         10         11         12
      8              9         10         11         12         13
      9             10         11         12         13         14
     10             11         12         13         14         15


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Band            1          2          3          4          5
   Samp
      1              3          4          5          6          7
      2              4          5          6          7          8
      3              5          6          7          8          9
      4              6          7          8          9         10
      5              7          8          9         10         11
      6              8          9         10         11         12
      7              9         10         11         12         13
      8             10         11         12         13         14
      9             11         12         13         14         15
     10             12         13         14         15         16


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Band            1          2          3          4          5
   Samp
      1              4          5          6          7          8
      2              5          6          7          8          9
      3              6          7          8          9         10
      4              7          8          9         10         11
      5              8          9         10         11         12
      6              9         10         11         12         13
      7             10         11         12         13         14
      8             11         12         13         14         15
      9             12         13         14         15         16
     10             13         14         15         16         17


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Band            1          2          3          4          5
   Samp
      1              5          6          7          8          9
      2              6          7          8          9         10
      3              7          8          9         10         11
      4              8          9         10         11         12
      5              9         10         11         12         13
      6             10         11         12         13         14
      7             11         12         13         14         15
      8             12         13         14         15         16
      9             13         14         15         16         17
     10             14         15         16         17         18


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Band            1          2          3          4          5
   Samp
      1              6          7          8          9         10
      2              7          8          9         10         11
      3              8          9         10         11         12
      4              9         10         11         12         13
      5             10         11         12         13         14
      6             11         12         13         14         15
      7             12         13         14         15         16
      8             13         14         15         16         17
      9             14         15         16         17         18
     10             15         16         17         18         19


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Band            1          2          3          4          5
   Samp
      1              7          8          9         10         11
      2              8          9         10         11         12
      3              9         10         11         12         13
      4             10         11         12         13         14
      5             11         12         13         14         15
      6             12         13         14         15         16
      7             13         14         15         16         17
      8             14         15         16         17         18
      9             15         16         17         18         19
     10             16         17         18         19         20


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Band            1          2          3          4          5
   Samp
      1              8          9         10         11         12
      2              9         10         11         12         13
      3             10         11         12         13         14
      4             11         12         13         14         15
      5             12         13         14         15         16
      6             13         14         15         16         17
      7             14         15         16         17         18
      8             15         16         17         18         19
      9             16         17         18         19         20
     10             17         18         19         20         21


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Band            1          2          3          4          5
   Samp
      1              9         10         11         12         13
      2             10         11         12         13         14
      3             11         12         13         14         15
      4             12         13         14         15         16
      5             13         14         15         16         17
      6             14         15         16         17         18
      7             15         16         17         18         19
      8             16         17         18         19         20
      9             17         18         19         20         21
     10             18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR REAL*4 DATA
 
 
2D files compatibility check
gen a 10 10 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.800000E+01   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 1.000000E+00   at  (     1,     2)
Max. value: 1.700000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 4.000000E+00   at  (     3,     3)
Max. value: 8.000000E+00   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Band             1           2           3           4           5
   Samp
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Band             1           2           3           4           5
   Samp
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Band             1           2           3           4           5
   Samp
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Band             1           2           3           4           5
   Samp
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Band             1           2           3           4           5
   Samp
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Band             1           2           3           4           5
   Samp
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Band             1           2           3           4           5
   Samp
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Band             1           2           3           4           5
   Samp
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Band             1           2           3           4           5
   Samp
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Band             1           2           3           4           5
   Samp
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR REAL*8 DATA
 
 
2D files compatibility check
gen a 10 10 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
maxmin a MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.800000E+01   at  (    10,    10)

let $echo="no"
Stretch this image to byte format
Image b should cover the range 0 to 255.
cform a b 'BYTE IRANGE=(0.000000000000e+00, 1.800000000000e+01) ORANGE=(0, 255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *    14.167+     0.000
INPUT FORMAT = DOUB
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
list b 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 Task:CFORM     User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp     1       3       5       7       9
   Line
      1       0  14  28  43  57  71  85  99 113 128
      2      14  28  43  57  71  85  99 113 128 142
      3      28  43  57  71  85  99 113 128 142 156
      4      43  57  71  85  99 113 128 142 156 170
      5      57  71  85  99 113 128 142 156 170 184
      6      71  85  99 113 128 142 156 170 184 198
      7      85  99 113 128 142 156 170 184 198 213
      8      99 113 128 142 156 170 184 198 213 227
      9     113 128 142 156 170 184 198 213 227 241
     10     128 142 156 170 184 198 213 227 241 255
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.700000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=0.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 4.000000E+00   at  (     3,     3)
Max. value: 8.000000E+00   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Band =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     1
 ***********
     Band             1           2           3           4           5
   Samp
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     2
 ***********
     Band             1           2           3           4           5
   Samp
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     3
 ***********
     Band             1           2           3           4           5
   Samp
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     4
 ***********
     Band             1           2           3           4           5
   Samp
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     5
 ***********
     Band             1           2           3           4           5
   Samp
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     6
 ***********
     Band             1           2           3           4           5
   Samp
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     7
 ***********
     Band             1           2           3           4           5
   Samp
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     8
 ***********
     Band             1           2           3           4           5
   Samp
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =     9
 ***********
     Band             1           2           3           4           5
   Samp
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 ***********
 Line =    10
 ***********
     Band             1           2           3           4           5
   Samp
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR COMPLEX DATA
 
 
2D files compatibility check
gen aa 10 10 IVAL=-2 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
cform aa a OFORM=COMPLEX
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = HALF
OUTPUT FORMAT = COMP
CONVERSION COMPLETE
list a 'NOEJECT
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 Task:CFORM     User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp                         1                       2                       3                       4                       5
   Line
      1      -2.000E+00   0.000E+00  -1.000E+00   0.000E+00   0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00
      2      -1.000E+00   0.000E+00   0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00
      3       0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00
      4       1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00
      5       2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00
      6       3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00
      7       4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00
      8       5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00
      9       6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00
     10       7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
 Task:CFORM     User:rjb       Date_Time:Fri Jul  6 22:42:54 2012
     Samp                         6                       7                       8                       9                      10
   Line
      1       3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00
      2       4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00
      3       5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00
      4       6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00
      5       7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00
      6       8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00
      7       9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00
      8       1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00
      9       1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00   1.500E+01   0.000E+00
     10       1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00   1.500E+01   0.000E+00   1.600E+01   0.000E+00
maxmin a
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 0.000000E+00   at  (     1,     3)
Max. value: 1.600000E+01   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 1.000000E+00   at  (     1,     2)
Max. value: 1.500000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=15.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value: 2.000000E+00   at  (     3,     3)
Max. value: 6.000000E+00   at  (     5,     5)

let $echo="no"
Try some miscellaneous EXCLUDE tests.
Should get min=1, max=9
gen a 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
maxmin a EXCLUDE=0
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            1   at  (     2,     1)
Max. value:            9   at  (    10,     1)

let $echo="no"
Should get min=0, max=8
maxmin a EXCLUDE=9
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (     1,     1)
Max. value:            8   at  (     9,     1)

let $echo="no"
Should get min=1, max=18
gen a 10 10 linc=-1 ival=9
Beginning VICAR task gen
GEN Version 6
GEN task completed
maxmin a EXCLUDE=0
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            1   at  (     9,     1)
Max. value:           18   at  (     1,    10)

let $echo="no"
Should get min=0, max=17
maxmin a EXCLUDE=18
Beginning VICAR task maxmin
*** maxmin - 07/06/2012 - rjb (64-bit)

Min. value:            0   at  (    10,     1)
Max. value:           17   at  (     1,     9)

let $echo="no"
exit
slogoff
$!-----------------------------------------------------------------------------
$ create tstmaxmin.log_solos
tstmaxmin
 
 TEST FOR BYTE DATA
 
 
2D files compatibility check
gen a 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:57 2012
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:59 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:59 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14
      6       6   7   8   9  10  11  12  13  14  15
      7       7   8   9  10  11  12  13  14  15  16
      8       8   9  10  11  12  13  14  15  16  17
      9       9  10  11  12  13  14  15  16  17  18
     10      10  11  12  13  14  15  16  17  18  19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:59 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15
      6       7   8   9  10  11  12  13  14  15  16
      7       8   9  10  11  12  13  14  15  16  17
      8       9  10  11  12  13  14  15  16  17  18
      9      10  11  12  13  14  15  16  17  18  19
     10      11  12  13  14  15  16  17  18  19  20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:59 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5       7       9
   Line
      1       3   4   5   6   7   8   9  10  11  12
      2       4   5   6   7   8   9  10  11  12  13
      3       5   6   7   8   9  10  11  12  13  14
      4       6   7   8   9  10  11  12  13  14  15
      5       7   8   9  10  11  12  13  14  15  16
      6       8   9  10  11  12  13  14  15  16  17
      7       9  10  11  12  13  14  15  16  17  18
      8      10  11  12  13  14  15  16  17  18  19
      9      11  12  13  14  15  16  17  18  19  20
     10      12  13  14  15  16  17  18  19  20  21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:50:59 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5       7       9
   Line
      1       4   5   6   7   8   9  10  11  12  13
      2       5   6   7   8   9  10  11  12  13  14
      3       6   7   8   9  10  11  12  13  14  15
      4       7   8   9  10  11  12  13  14  15  16
      5       8   9  10  11  12  13  14  15  16  17
      6       9  10  11  12  13  14  15  16  17  18
      7      10  11  12  13  14  15  16  17  18  19
      8      11  12  13  14  15  16  17  18  19  20
      9      12  13  14  15  16  17  18  19  20  21
     10      13  14  15  16  17  18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     1
 ***********
     Samp     1       3       5       7       9
   Band
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     2
 ***********
     Samp     1       3       5       7       9
   Band
      1       1   2   3   4   5   6   7   8   9  10
      2       2   3   4   5   6   7   8   9  10  11
      3       3   4   5   6   7   8   9  10  11  12
      4       4   5   6   7   8   9  10  11  12  13
      5       5   6   7   8   9  10  11  12  13  14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     3
 ***********
     Samp     1       3       5       7       9
   Band
      1       2   3   4   5   6   7   8   9  10  11
      2       3   4   5   6   7   8   9  10  11  12
      3       4   5   6   7   8   9  10  11  12  13
      4       5   6   7   8   9  10  11  12  13  14
      5       6   7   8   9  10  11  12  13  14  15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     4
 ***********
     Samp     1       3       5       7       9
   Band
      1       3   4   5   6   7   8   9  10  11  12
      2       4   5   6   7   8   9  10  11  12  13
      3       5   6   7   8   9  10  11  12  13  14
      4       6   7   8   9  10  11  12  13  14  15
      5       7   8   9  10  11  12  13  14  15  16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     5
 ***********
     Samp     1       3       5       7       9
   Band
      1       4   5   6   7   8   9  10  11  12  13
      2       5   6   7   8   9  10  11  12  13  14
      3       6   7   8   9  10  11  12  13  14  15
      4       7   8   9  10  11  12  13  14  15  16
      5       8   9  10  11  12  13  14  15  16  17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     6
 ***********
     Samp     1       3       5       7       9
   Band
      1       5   6   7   8   9  10  11  12  13  14
      2       6   7   8   9  10  11  12  13  14  15
      3       7   8   9  10  11  12  13  14  15  16
      4       8   9  10  11  12  13  14  15  16  17
      5       9  10  11  12  13  14  15  16  17  18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     7
 ***********
     Samp     1       3       5       7       9
   Band
      1       6   7   8   9  10  11  12  13  14  15
      2       7   8   9  10  11  12  13  14  15  16
      3       8   9  10  11  12  13  14  15  16  17
      4       9  10  11  12  13  14  15  16  17  18
      5      10  11  12  13  14  15  16  17  18  19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     8
 ***********
     Samp     1       3       5       7       9
   Band
      1       7   8   9  10  11  12  13  14  15  16
      2       8   9  10  11  12  13  14  15  16  17
      3       9  10  11  12  13  14  15  16  17  18
      4      10  11  12  13  14  15  16  17  18  19
      5      11  12  13  14  15  16  17  18  19  20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =     9
 ***********
     Samp     1       3       5       7       9
   Band
      1       8   9  10  11  12  13  14  15  16  17
      2       9  10  11  12  13  14  15  16  17  18
      3      10  11  12  13  14  15  16  17  18  19
      4      11  12  13  14  15  16  17  18  19  20
      5      12  13  14  15  16  17  18  19  20  21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:00 2012
 ***********
 Line =    10
 ***********
     Samp     1       3       5       7       9
   Band
      1       9  10  11  12  13  14  15  16  17  18
      2      10  11  12  13  14  15  16  17  18  19
      3      11  12  13  14  15  16  17  18  19  20
      4      12  13  14  15  16  17  18  19  20  21
      5      13  14  15  16  17  18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     1
 ***********
     Band     1       3       5
   Samp
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
      6       5   6   7   8   9
      7       6   7   8   9  10
      8       7   8   9  10  11
      9       8   9  10  11  12
     10       9  10  11  12  13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     2
 ***********
     Band     1       3       5
   Samp
      1       1   2   3   4   5
      2       2   3   4   5   6
      3       3   4   5   6   7
      4       4   5   6   7   8
      5       5   6   7   8   9
      6       6   7   8   9  10
      7       7   8   9  10  11
      8       8   9  10  11  12
      9       9  10  11  12  13
     10      10  11  12  13  14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     3
 ***********
     Band     1       3       5
   Samp
      1       2   3   4   5   6
      2       3   4   5   6   7
      3       4   5   6   7   8
      4       5   6   7   8   9
      5       6   7   8   9  10
      6       7   8   9  10  11
      7       8   9  10  11  12
      8       9  10  11  12  13
      9      10  11  12  13  14
     10      11  12  13  14  15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     4
 ***********
     Band     1       3       5
   Samp
      1       3   4   5   6   7
      2       4   5   6   7   8
      3       5   6   7   8   9
      4       6   7   8   9  10
      5       7   8   9  10  11
      6       8   9  10  11  12
      7       9  10  11  12  13
      8      10  11  12  13  14
      9      11  12  13  14  15
     10      12  13  14  15  16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     5
 ***********
     Band     1       3       5
   Samp
      1       4   5   6   7   8
      2       5   6   7   8   9
      3       6   7   8   9  10
      4       7   8   9  10  11
      5       8   9  10  11  12
      6       9  10  11  12  13
      7      10  11  12  13  14
      8      11  12  13  14  15
      9      12  13  14  15  16
     10      13  14  15  16  17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     6
 ***********
     Band     1       3       5
   Samp
      1       5   6   7   8   9
      2       6   7   8   9  10
      3       7   8   9  10  11
      4       8   9  10  11  12
      5       9  10  11  12  13
      6      10  11  12  13  14
      7      11  12  13  14  15
      8      12  13  14  15  16
      9      13  14  15  16  17
     10      14  15  16  17  18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     7
 ***********
     Band     1       3       5
   Samp
      1       6   7   8   9  10
      2       7   8   9  10  11
      3       8   9  10  11  12
      4       9  10  11  12  13
      5      10  11  12  13  14
      6      11  12  13  14  15
      7      12  13  14  15  16
      8      13  14  15  16  17
      9      14  15  16  17  18
     10      15  16  17  18  19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     8
 ***********
     Band     1       3       5
   Samp
      1       7   8   9  10  11
      2       8   9  10  11  12
      3       9  10  11  12  13
      4      10  11  12  13  14
      5      11  12  13  14  15
      6      12  13  14  15  16
      7      13  14  15  16  17
      8      14  15  16  17  18
      9      15  16  17  18  19
     10      16  17  18  19  20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =     9
 ***********
     Band     1       3       5
   Samp
      1       8   9  10  11  12
      2       9  10  11  12  13
      3      10  11  12  13  14
      4      11  12  13  14  15
      5      12  13  14  15  16
      6      13  14  15  16  17
      7      14  15  16  17  18
      8      15  16  17  18  19
      9      16  17  18  19  20
     10      17  18  19  20  21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:02 2012
 ***********
 Line =    10
 ***********
     Band     1       3       5
   Samp
      1       9  10  11  12  13
      2      10  11  12  13  14
      3      11  12  13  14  15
      4      12  13  14  15  16
      5      13  14  15  16  17
      6      14  15  16  17  18
      7      15  16  17  18  19
      8      16  17  18  19  20
      9      17  18  19  20  21
     10      18  19  20  21  22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR HALFWORD DATA
 
 
2D files compatibility check
gen a 10 10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:03 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:05 2012
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:05 2012
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14
      6         6     7     8     9    10    11    12    13    14    15
      7         7     8     9    10    11    12    13    14    15    16
      8         8     9    10    11    12    13    14    15    16    17
      9         9    10    11    12    13    14    15    16    17    18
     10        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:05 2012
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15
      6         7     8     9    10    11    12    13    14    15    16
      7         8     9    10    11    12    13    14    15    16    17
      8         9    10    11    12    13    14    15    16    17    18
      9        10    11    12    13    14    15    16    17    18    19
     10        11    12    13    14    15    16    17    18    19    20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:05 2012
 ***********
 Band =     4
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         3     4     5     6     7     8     9    10    11    12
      2         4     5     6     7     8     9    10    11    12    13
      3         5     6     7     8     9    10    11    12    13    14
      4         6     7     8     9    10    11    12    13    14    15
      5         7     8     9    10    11    12    13    14    15    16
      6         8     9    10    11    12    13    14    15    16    17
      7         9    10    11    12    13    14    15    16    17    18
      8        10    11    12    13    14    15    16    17    18    19
      9        11    12    13    14    15    16    17    18    19    20
     10        12    13    14    15    16    17    18    19    20    21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:05 2012
 ***********
 Band =     5
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         4     5     6     7     8     9    10    11    12    13
      2         5     6     7     8     9    10    11    12    13    14
      3         6     7     8     9    10    11    12    13    14    15
      4         7     8     9    10    11    12    13    14    15    16
      5         8     9    10    11    12    13    14    15    16    17
      6         9    10    11    12    13    14    15    16    17    18
      7        10    11    12    13    14    15    16    17    18    19
      8        11    12    13    14    15    16    17    18    19    20
      9        12    13    14    15    16    17    18    19    20    21
     10        13    14    15    16    17    18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     4
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         3     4     5     6     7     8     9    10    11    12
      2         4     5     6     7     8     9    10    11    12    13
      3         5     6     7     8     9    10    11    12    13    14
      4         6     7     8     9    10    11    12    13    14    15
      5         7     8     9    10    11    12    13    14    15    16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     5
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         4     5     6     7     8     9    10    11    12    13
      2         5     6     7     8     9    10    11    12    13    14
      3         6     7     8     9    10    11    12    13    14    15
      4         7     8     9    10    11    12    13    14    15    16
      5         8     9    10    11    12    13    14    15    16    17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     6
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         5     6     7     8     9    10    11    12    13    14
      2         6     7     8     9    10    11    12    13    14    15
      3         7     8     9    10    11    12    13    14    15    16
      4         8     9    10    11    12    13    14    15    16    17
      5         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     7
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         6     7     8     9    10    11    12    13    14    15
      2         7     8     9    10    11    12    13    14    15    16
      3         8     9    10    11    12    13    14    15    16    17
      4         9    10    11    12    13    14    15    16    17    18
      5        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     8
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         7     8     9    10    11    12    13    14    15    16
      2         8     9    10    11    12    13    14    15    16    17
      3         9    10    11    12    13    14    15    16    17    18
      4        10    11    12    13    14    15    16    17    18    19
      5        11    12    13    14    15    16    17    18    19    20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =     9
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         8     9    10    11    12    13    14    15    16    17
      2         9    10    11    12    13    14    15    16    17    18
      3        10    11    12    13    14    15    16    17    18    19
      4        11    12    13    14    15    16    17    18    19    20
      5        12    13    14    15    16    17    18    19    20    21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:06 2012
 ***********
 Line =    10
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Band
      1         9    10    11    12    13    14    15    16    17    18
      2        10    11    12    13    14    15    16    17    18    19
      3        11    12    13    14    15    16    17    18    19    20
      4        12    13    14    15    16    17    18    19    20    21
      5        13    14    15    16    17    18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     1
 ***********
     Band       1     2     3     4     5
   Samp
      1         0     1     2     3     4
      2         1     2     3     4     5
      3         2     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
      6         5     6     7     8     9
      7         6     7     8     9    10
      8         7     8     9    10    11
      9         8     9    10    11    12
     10         9    10    11    12    13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     2
 ***********
     Band       1     2     3     4     5
   Samp
      1         1     2     3     4     5
      2         2     3     4     5     6
      3         3     4     5     6     7
      4         4     5     6     7     8
      5         5     6     7     8     9
      6         6     7     8     9    10
      7         7     8     9    10    11
      8         8     9    10    11    12
      9         9    10    11    12    13
     10        10    11    12    13    14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     3
 ***********
     Band       1     2     3     4     5
   Samp
      1         2     3     4     5     6
      2         3     4     5     6     7
      3         4     5     6     7     8
      4         5     6     7     8     9
      5         6     7     8     9    10
      6         7     8     9    10    11
      7         8     9    10    11    12
      8         9    10    11    12    13
      9        10    11    12    13    14
     10        11    12    13    14    15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     4
 ***********
     Band       1     2     3     4     5
   Samp
      1         3     4     5     6     7
      2         4     5     6     7     8
      3         5     6     7     8     9
      4         6     7     8     9    10
      5         7     8     9    10    11
      6         8     9    10    11    12
      7         9    10    11    12    13
      8        10    11    12    13    14
      9        11    12    13    14    15
     10        12    13    14    15    16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     5
 ***********
     Band       1     2     3     4     5
   Samp
      1         4     5     6     7     8
      2         5     6     7     8     9
      3         6     7     8     9    10
      4         7     8     9    10    11
      5         8     9    10    11    12
      6         9    10    11    12    13
      7        10    11    12    13    14
      8        11    12    13    14    15
      9        12    13    14    15    16
     10        13    14    15    16    17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     6
 ***********
     Band       1     2     3     4     5
   Samp
      1         5     6     7     8     9
      2         6     7     8     9    10
      3         7     8     9    10    11
      4         8     9    10    11    12
      5         9    10    11    12    13
      6        10    11    12    13    14
      7        11    12    13    14    15
      8        12    13    14    15    16
      9        13    14    15    16    17
     10        14    15    16    17    18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     7
 ***********
     Band       1     2     3     4     5
   Samp
      1         6     7     8     9    10
      2         7     8     9    10    11
      3         8     9    10    11    12
      4         9    10    11    12    13
      5        10    11    12    13    14
      6        11    12    13    14    15
      7        12    13    14    15    16
      8        13    14    15    16    17
      9        14    15    16    17    18
     10        15    16    17    18    19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     8
 ***********
     Band       1     2     3     4     5
   Samp
      1         7     8     9    10    11
      2         8     9    10    11    12
      3         9    10    11    12    13
      4        10    11    12    13    14
      5        11    12    13    14    15
      6        12    13    14    15    16
      7        13    14    15    16    17
      8        14    15    16    17    18
      9        15    16    17    18    19
     10        16    17    18    19    20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =     9
 ***********
     Band       1     2     3     4     5
   Samp
      1         8     9    10    11    12
      2         9    10    11    12    13
      3        10    11    12    13    14
      4        11    12    13    14    15
      5        12    13    14    15    16
      6        13    14    15    16    17
      7        14    15    16    17    18
      8        15    16    17    18    19
      9        16    17    18    19    20
     10        17    18    19    20    21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:07 2012
 ***********
 Line =    10
 ***********
     Band       1     2     3     4     5
   Samp
      1         9    10    11    12    13
      2        10    11    12    13    14
      3        11    12    13    14    15
      4        12    13    14    15    16
      5        13    14    15    16    17
      6        14    15    16    17    18
      7        15    16    17    18    19
      8        16    17    18    19    20
      9        17    18    19    20    21
     10        18    19    20    21    22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR FULLWORD DATA
 
 
2D files compatibility check
gen a 10 10 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:09 2012
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13
      6              5          6          7          8          9         10         11         12         13         14
      7              6          7          8          9         10         11         12         13         14         15
      8              7          8          9         10         11         12         13         14         15         16
      9              8          9         10         11         12         13         14         15         16         17
     10              9         10         11         12         13         14         15         16         17         18
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1)
Max. value:           18   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            1   at  (     1,     2)
Max. value:           17   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            4   at  (     3,     3)
Max. value:            8   at  (     5,     5)

putmsg "3D BSQ files" ""
3D BSQ files
gen a 10 10 nb=5 org=bsq 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:11 2012
 ***********
 Band =     1
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13
      6              5          6          7          8          9         10         11         12         13         14
      7              6          7          8          9         10         11         12         13         14         15
      8              7          8          9         10         11         12         13         14         15         16
      9              8          9         10         11         12         13         14         15         16         17
     10              9         10         11         12         13         14         15         16         17         18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:11 2012
 ***********
 Band =     2
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              1          2          3          4          5          6          7          8          9         10
      2              2          3          4          5          6          7          8          9         10         11
      3              3          4          5          6          7          8          9         10         11         12
      4              4          5          6          7          8          9         10         11         12         13
      5              5          6          7          8          9         10         11         12         13         14
      6              6          7          8          9         10         11         12         13         14         15
      7              7          8          9         10         11         12         13         14         15         16
      8              8          9         10         11         12         13         14         15         16         17
      9              9         10         11         12         13         14         15         16         17         18
     10             10         11         12         13         14         15         16         17         18         19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:11 2012
 ***********
 Band =     3
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              2          3          4          5          6          7          8          9         10         11
      2              3          4          5          6          7          8          9         10         11         12
      3              4          5          6          7          8          9         10         11         12         13
      4              5          6          7          8          9         10         11         12         13         14
      5              6          7          8          9         10         11         12         13         14         15
      6              7          8          9         10         11         12         13         14         15         16
      7              8          9         10         11         12         13         14         15         16         17
      8              9         10         11         12         13         14         15         16         17         18
      9             10         11         12         13         14         15         16         17         18         19
     10             11         12         13         14         15         16         17         18         19         20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:11 2012
 ***********
 Band =     4
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              3          4          5          6          7          8          9         10         11         12
      2              4          5          6          7          8          9         10         11         12         13
      3              5          6          7          8          9         10         11         12         13         14
      4              6          7          8          9         10         11         12         13         14         15
      5              7          8          9         10         11         12         13         14         15         16
      6              8          9         10         11         12         13         14         15         16         17
      7              9         10         11         12         13         14         15         16         17         18
      8             10         11         12         13         14         15         16         17         18         19
      9             11         12         13         14         15         16         17         18         19         20
     10             12         13         14         15         16         17         18         19         20         21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:11 2012
 ***********
 Band =     5
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              4          5          6          7          8          9         10         11         12         13
      2              5          6          7          8          9         10         11         12         13         14
      3              6          7          8          9         10         11         12         13         14         15
      4              7          8          9         10         11         12         13         14         15         16
      5              8          9         10         11         12         13         14         15         16         17
      6              9         10         11         12         13         14         15         16         17         18
      7             10         11         12         13         14         15         16         17         18         19
      8             11         12         13         14         15         16         17         18         19         20
      9             12         13         14         15         16         17         18         19         20         21
     10             13         14         15         16         17         18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     1
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              0          1          2          3          4          5          6          7          8          9
      2              1          2          3          4          5          6          7          8          9         10
      3              2          3          4          5          6          7          8          9         10         11
      4              3          4          5          6          7          8          9         10         11         12
      5              4          5          6          7          8          9         10         11         12         13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     2
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              1          2          3          4          5          6          7          8          9         10
      2              2          3          4          5          6          7          8          9         10         11
      3              3          4          5          6          7          8          9         10         11         12
      4              4          5          6          7          8          9         10         11         12         13
      5              5          6          7          8          9         10         11         12         13         14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     3
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              2          3          4          5          6          7          8          9         10         11
      2              3          4          5          6          7          8          9         10         11         12
      3              4          5          6          7          8          9         10         11         12         13
      4              5          6          7          8          9         10         11         12         13         14
      5              6          7          8          9         10         11         12         13         14         15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     4
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              3          4          5          6          7          8          9         10         11         12
      2              4          5          6          7          8          9         10         11         12         13
      3              5          6          7          8          9         10         11         12         13         14
      4              6          7          8          9         10         11         12         13         14         15
      5              7          8          9         10         11         12         13         14         15         16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     5
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              4          5          6          7          8          9         10         11         12         13
      2              5          6          7          8          9         10         11         12         13         14
      3              6          7          8          9         10         11         12         13         14         15
      4              7          8          9         10         11         12         13         14         15         16
      5              8          9         10         11         12         13         14         15         16         17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     6
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              5          6          7          8          9         10         11         12         13         14
      2              6          7          8          9         10         11         12         13         14         15
      3              7          8          9         10         11         12         13         14         15         16
      4              8          9         10         11         12         13         14         15         16         17
      5              9         10         11         12         13         14         15         16         17         18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     7
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              6          7          8          9         10         11         12         13         14         15
      2              7          8          9         10         11         12         13         14         15         16
      3              8          9         10         11         12         13         14         15         16         17
      4              9         10         11         12         13         14         15         16         17         18
      5             10         11         12         13         14         15         16         17         18         19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     8
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              7          8          9         10         11         12         13         14         15         16
      2              8          9         10         11         12         13         14         15         16         17
      3              9         10         11         12         13         14         15         16         17         18
      4             10         11         12         13         14         15         16         17         18         19
      5             11         12         13         14         15         16         17         18         19         20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =     9
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              8          9         10         11         12         13         14         15         16         17
      2              9         10         11         12         13         14         15         16         17         18
      3             10         11         12         13         14         15         16         17         18         19
      4             11         12         13         14         15         16         17         18         19         20
      5             12         13         14         15         16         17         18         19         20         21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:12 2012
 ***********
 Line =    10
 ***********
     Samp            1          2          3          4          5          6          7          8          9         10
   Band
      1              9         10         11         12         13         14         15         16         17         18
      2             10         11         12         13         14         15         16         17         18         19
      3             11         12         13         14         15         16         17         18         19         20
      4             12         13         14         15         16         17         18         19         20         21
      5             13         14         15         16         17         18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     1
 ***********
     Band            1          2          3          4          5
   Samp
      1              0          1          2          3          4
      2              1          2          3          4          5
      3              2          3          4          5          6
      4              3          4          5          6          7
      5              4          5          6          7          8
      6              5          6          7          8          9
      7              6          7          8          9         10
      8              7          8          9         10         11
      9              8          9         10         11         12
     10              9         10         11         12         13


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     2
 ***********
     Band            1          2          3          4          5
   Samp
      1              1          2          3          4          5
      2              2          3          4          5          6
      3              3          4          5          6          7
      4              4          5          6          7          8
      5              5          6          7          8          9
      6              6          7          8          9         10
      7              7          8          9         10         11
      8              8          9         10         11         12
      9              9         10         11         12         13
     10             10         11         12         13         14


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     3
 ***********
     Band            1          2          3          4          5
   Samp
      1              2          3          4          5          6
      2              3          4          5          6          7
      3              4          5          6          7          8
      4              5          6          7          8          9
      5              6          7          8          9         10
      6              7          8          9         10         11
      7              8          9         10         11         12
      8              9         10         11         12         13
      9             10         11         12         13         14
     10             11         12         13         14         15


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     4
 ***********
     Band            1          2          3          4          5
   Samp
      1              3          4          5          6          7
      2              4          5          6          7          8
      3              5          6          7          8          9
      4              6          7          8          9         10
      5              7          8          9         10         11
      6              8          9         10         11         12
      7              9         10         11         12         13
      8             10         11         12         13         14
      9             11         12         13         14         15
     10             12         13         14         15         16


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     5
 ***********
     Band            1          2          3          4          5
   Samp
      1              4          5          6          7          8
      2              5          6          7          8          9
      3              6          7          8          9         10
      4              7          8          9         10         11
      5              8          9         10         11         12
      6              9         10         11         12         13
      7             10         11         12         13         14
      8             11         12         13         14         15
      9             12         13         14         15         16
     10             13         14         15         16         17


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     6
 ***********
     Band            1          2          3          4          5
   Samp
      1              5          6          7          8          9
      2              6          7          8          9         10
      3              7          8          9         10         11
      4              8          9         10         11         12
      5              9         10         11         12         13
      6             10         11         12         13         14
      7             11         12         13         14         15
      8             12         13         14         15         16
      9             13         14         15         16         17
     10             14         15         16         17         18


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     7
 ***********
     Band            1          2          3          4          5
   Samp
      1              6          7          8          9         10
      2              7          8          9         10         11
      3              8          9         10         11         12
      4              9         10         11         12         13
      5             10         11         12         13         14
      6             11         12         13         14         15
      7             12         13         14         15         16
      8             13         14         15         16         17
      9             14         15         16         17         18
     10             15         16         17         18         19


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     8
 ***********
     Band            1          2          3          4          5
   Samp
      1              7          8          9         10         11
      2              8          9         10         11         12
      3              9         10         11         12         13
      4             10         11         12         13         14
      5             11         12         13         14         15
      6             12         13         14         15         16
      7             13         14         15         16         17
      8             14         15         16         17         18
      9             15         16         17         18         19
     10             16         17         18         19         20


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =     9
 ***********
     Band            1          2          3          4          5
   Samp
      1              8          9         10         11         12
      2              9         10         11         12         13
      3             10         11         12         13         14
      4             11         12         13         14         15
      5             12         13         14         15         16
      6             13         14         15         16         17
      7             14         15         16         17         18
      8             15         16         17         18         19
      9             16         17         18         19         20
     10             17         18         19         20         21


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:13 2012
 ***********
 Line =    10
 ***********
     Band            1          2          3          4          5
   Samp
      1              9         10         11         12         13
      2             10         11         12         13         14
      3             11         12         13         14         15
      4             12         13         14         15         16
      5             13         14         15         16         17
      6             14         15         16         17         18
      7             15         16         17         18         19
      8             16         17         18         19         20
      9             17         18         19         20         21
     10             18         19         20         21         22
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1) of band     1
Max. value:           22   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            5   at  (     3,     3) of band     2
Max. value:           11   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR REAL*4 DATA
 
 
2D files compatibility check
gen a 10 10 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:14 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.800000E+01   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 1.000000E+00   at  (     1,     2)
Max. value: 1.700000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 4.000000E+00   at  (     3,     3)
Max. value: 8.000000E+00   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:16 2012
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:16 2012
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:16 2012
 ***********
 Band =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:16 2012
 ***********
 Band =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:16 2012
 ***********
 Band =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     6
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     7
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     8
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =     9
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:17 2012
 ***********
 Line =    10
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     1
 ***********
     Band             1           2           3           4           5
   Samp
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     2
 ***********
     Band             1           2           3           4           5
   Samp
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     3
 ***********
     Band             1           2           3           4           5
   Samp
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     4
 ***********
     Band             1           2           3           4           5
   Samp
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     5
 ***********
     Band             1           2           3           4           5
   Samp
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     6
 ***********
     Band             1           2           3           4           5
   Samp
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     7
 ***********
     Band             1           2           3           4           5
   Samp
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     8
 ***********
     Band             1           2           3           4           5
   Samp
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =     9
 ***********
     Band             1           2           3           4           5
   Samp
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:19 2012
 ***********
 Line =    10
 ***********
     Band             1           2           3           4           5
   Samp
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR REAL*8 DATA
 
 
2D files compatibility check
gen a 10 10 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:20 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
maxmin a MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.800000E+01   at  (    10,    10)

let $echo="no"
Stretch this image to byte format
Image b should cover the range 0 to 255.
cform a b 'BYTE IRANGE=(0.000000000000e+00, 1.800000000000e+01) ORANGE=(0, 255)
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *    14.167+     0.000
INPUT FORMAT = DOUB
OUTPUT FORMAT = BYTE
CONVERSION COMPLETE
list b 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:20 2012
 Task:CFORM     User:lwk       Date_Time:Wed Oct 17 19:51:22 2012
     Samp     1       3       5       7       9
   Line
      1       0  14  28  43  57  71  85  99 113 128
      2      14  28  43  57  71  85  99 113 128 142
      3      28  43  57  71  85  99 113 128 142 156
      4      43  57  71  85  99 113 128 142 156 170
      5      57  71  85  99 113 128 142 156 170 184
      6      71  85  99 113 128 142 156 170 184 198
      7      85  99 113 128 142 156 170 184 198 213
      8      99 113 128 142 156 170 184 198 213 227
      9     113 128 142 156 170 184 198 213 227 241
     10     128 142 156 170 184 198 213 227 241 255
maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 1.700000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=0.0


maxvar=17.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 4.000000E+00   at  (     3,     3)
Max. value: 8.000000E+00   at  (     5,     5)

let $echo="no"
3D BSQ files
gen a 10 10 nb=5 org=bsq 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:23 2012
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:23 2012
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:23 2012
 ***********
 Band =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:23 2012
 ***********
 Band =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:23 2012
 ***********
 Band =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIL files
gen a 10 10 nb=5 org=bil 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     1
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     2
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     3
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     4
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     5
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     6
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     7
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     8
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =     9
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:24 2012
 ***********
 Line =    10
 ***********
     Samp             1           2           3           4           5           6           7           8           9          10
   Band
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) sb=2 nb=3
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
3D BIP files
gen a 10 10 nb=5 org=bip 'REAL8
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a 'NOEJECT
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     1
 ***********
     Band             1           2           3           4           5
   Samp
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     2
 ***********
     Band             1           2           3           4           5
   Samp
      1       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      2       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      3       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      4       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      5       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      7       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      8       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      9       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
     10       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     3
 ***********
     Band             1           2           3           4           5
   Samp
      1       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      2       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      3       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      4       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      5       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      6       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      7       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      8       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      9       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
     10       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     4
 ***********
     Band             1           2           3           4           5
   Samp
      1       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      2       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      3       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      4       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      6       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      7       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      8       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      9       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
     10       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     5
 ***********
     Band             1           2           3           4           5
   Samp
      1       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      2       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      3       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      4       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      5       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      6       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      7       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      8       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      9       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
     10       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     6
 ***********
     Band             1           2           3           4           5
   Samp
      1       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     7
 ***********
     Band             1           2           3           4           5
   Samp
      1       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      2       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      3       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      4       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      5       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      6       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      8       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      9       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     10       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     8
 ***********
     Band             1           2           3           4           5
   Samp
      1       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      2       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      4       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      5       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      6       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      7       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      8       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      9       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     10       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =     9
 ***********
     Band             1           2           3           4           5
   Samp
      1       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      2       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      3       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      4       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      5       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      7       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      8       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      9       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
     10       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01


 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:25 2012
 ***********
 Line =    10
 ***********
     Band             1           2           3           4           5
   Samp
      1       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      3       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      4       1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      5       1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      6       1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      7       1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      8       1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      9       1.700E+01   1.800E+01   1.900E+01   2.000E+01   2.100E+01
     10       1.800E+01   1.900E+01   2.000E+01   2.100E+01   2.200E+01
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1) of band     1
Max. value: 2.200000E+01   at  (    10,    10) of band     5

maxmin a (3,3,3,3) (2,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 5.000000E+00   at  (     3,     3) of band     2
Max. value: 1.100000E+01   at  (     5,     5) of band     4

let $echo="no"
 
 TEST FOR COMPLEX DATA
 
 
2D files compatibility check
gen aa 10 10 IVAL=-2 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
cform aa a OFORM=COMPLEX
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = HALF
OUTPUT FORMAT = COMP
CONVERSION COMPLETE
list a 'NOEJECT
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:27 2012
 Task:CFORM     User:lwk       Date_Time:Wed Oct 17 19:51:27 2012
     Samp                         1                       2                       3                       4                       5
   Line
      1      -2.000E+00   0.000E+00  -1.000E+00   0.000E+00   0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00
      2      -1.000E+00   0.000E+00   0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00
      3       0.000E+00   0.000E+00   1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00
      4       1.000E+00   0.000E+00   2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00
      5       2.000E+00   0.000E+00   3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00
      6       3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00
      7       4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00
      8       5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00
      9       6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00
     10       7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Wed Oct 17 19:51:27 2012
 Task:CFORM     User:lwk       Date_Time:Wed Oct 17 19:51:27 2012
     Samp                         6                       7                       8                       9                      10
   Line
      1       3.000E+00   0.000E+00   4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00
      2       4.000E+00   0.000E+00   5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00
      3       5.000E+00   0.000E+00   6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00
      4       6.000E+00   0.000E+00   7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00
      5       7.000E+00   0.000E+00   8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00
      6       8.000E+00   0.000E+00   9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00
      7       9.000E+00   0.000E+00   1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00
      8       1.000E+01   0.000E+00   1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00
      9       1.100E+01   0.000E+00   1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00   1.500E+01   0.000E+00
     10       1.200E+01   0.000E+00   1.300E+01   0.000E+00   1.400E+01   0.000E+00   1.500E+01   0.000E+00   1.600E+01   0.000E+00
maxmin a
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     3)
Max. value: 1.600000E+01   at  (    10,    10)

maxmin a EXCLUDE=0 LINC=2 MINIVAL=minvar MAXIVAL=maxvar
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 1.000000E+00   at  (     1,     2)
Max. value: 1.500000E+01   at  (     9,    10)

let $echo="no"
Print minimum and maximum variables

minvar=1.0


maxvar=15.0

maxmin a (3,3,3,3)
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 2.000000E+00   at  (     3,     3)
Max. value: 6.000000E+00   at  (     5,     5)

let $echo="no"
Try some miscellaneous EXCLUDE tests.
Should get min=1, max=9
gen a 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
maxmin a EXCLUDE=0
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            1   at  (     2,     1)
Max. value:            9   at  (    10,     1)

let $echo="no"
Should get min=0, max=8
maxmin a EXCLUDE=9
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (     1,     1)
Max. value:            8   at  (     9,     1)

let $echo="no"
Should get min=1, max=18
gen a 10 10 linc=-1 ival=9
Beginning VICAR task gen
GEN Version 6
GEN task completed
maxmin a EXCLUDE=0
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            1   at  (     9,     1)
Max. value:           18   at  (     1,    10)

let $echo="no"
Should get min=0, max=17
maxmin a EXCLUDE=18
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value:            0   at  (    10,     1)
Max. value:           17   at  (     1,     9)

let $echo="no"
exit
slogoff
$ Return
$!#############################################################################
