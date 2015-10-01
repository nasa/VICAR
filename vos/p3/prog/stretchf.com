$!****************************************************************************
$!
$! Build proc for MIPL module stretchf
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:31:44
$!
$! Execute by entering:		$ @stretchf
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
$ write sys$output "*** module stretchf ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to stretchf.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("stretchf.imake") .nes. ""
$   then
$      vimake stretchf
$      purge stretchf.bld
$   else
$      if F$SEARCH("stretchf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretchf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretchf.bld "STD"
$   else
$      @stretchf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretchf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretchf.com -mixed -
	-s stretchf.f -
	-p stretchf.pdf -
	-i stretchf.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stretchf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C   ADAPTED FROM STRETCH by   Ron Alley           3/2/94
C   REVISION HISTORY
C
C    3/94  rea  Initial release
C    3/01  rea  Fix calls to knuth & xknuth to make compatible with the
C		new calling sequence
C
      INCLUDE 'VICMAIN_FOR'
 
      SUBROUTINE MAIN44
      EXTERNAL STRMAIN
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,DNMIN,DNMAX
      INTEGER*4 OUNIT,STAT,SL,SS,DNMIN,DNMAX,LUTSIZ
      CHARACTER*8 FORMAT
C
      CALL XVMESSAGE('STRETCHF Version 3-30-01',' ')
C                                                       OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C                                                    GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT .NE. 'FULL') CALL MYABORT (
     +				' STRETCHF accepts FULLWORD data only')
C                                               GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI)
     +    CALL MYABORT(' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      IF(SS+NSO-1 .GT. NSI)
     +    CALL MYABORT(
     +		' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
C                                                        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     &            'U_NL',NLO,'U_NS',NSO,' ')
C                                                 'DNMIN' - MINIMUM DN VALUE
      CALL XVPARM('DNMIN',DNMIN,ICOUNT,IDEF,1)
C                                                  'DNMAX' - MAXIMUM DN VALUE
      CALL XVPARM('DNMAX',DNMAX,ICOUNT,IDEF,1)
      IF(DNMIN.GT.DNMAX) 
     +    CALL MYABORT(' *** ERROR - DNMIN EXCEEDS DNMAX ***')
C                                                 DYNAMIC ALLOCATION OF BUFFERS
      NBI = 4*NSI
      NLEV=DNMAX-DNMIN+1
      LUTSIZ=4*NLEV
      CALL STACKA(4,STRMAIN,2,NBI,LUTSIZ)
C                                                               CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,0)
      CALL XVCLOSE(OUNIT,STAT,0)
      RETURN
      END
C***********************************************************************
      SUBROUTINE STRMAIN(BUF,NBI,LUT,LUTSIZ)
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,DNMIN,DNMAX
      REAL*4 RPARM(1000),TABBUF(200)
      INTEGER*4 OUNIT,DNMIN,DNMAX,STAT,LUTSIZ
      INTEGER*4 IPARM(1000)
      INTEGER*4 DNVAL,BCKGND,CONINC,SL,SS,HVAL,ALRBUF(100)
      LOGICAL XVPTST
      INTEGER*4 BUF(NBI),LUT(DNMIN:DNMAX)
      CHARACTER*1024 FNCBUF  
      EQUIVALENCE (RPARM,IPARM)
      CHARACTER*80 PRT,PRT2
C                                                                initialize
      ICHK=0
      NCHAR2=0
      NLEV=DNMAX-DNMIN+1
C              *** PROCESS STRETCH SPECIFICATION PARAMETERS ***
C                                                  'COMP' - COMPLEMENT IMAGE
      IF(XVPTST('COMP')) THEN
         IMODE=1
         ICHK=ICHK+1
      END IF
C                                                  'LINEAR' - LINEAR STRETCH
      CALL XVPARM('LINEAR',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .EQ. 2) THEN
         IMODE=2
         ICHK=ICHK+1
         NMIN = IPARM(1)
         NMAX = IPARM(2)
      END IF
C                                                  'CONTOUR'
      CALL XVPARM('CONTOUR',CONINC,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=4
         ICHK=ICHK+1
         IF(CONINC .EQ. 0) CALL MYABORT(
     +			' *** INVALID CONTOUR VALUE ***')
      ENDIF
C                                            'ALARM' - ALARM SPECIFIED DN VALUES
      CALL XVPARM('ALARM',ALRBUF,NALARM,IDEF,100)
      IF(NALARM .GE. 1) THEN
         IMODE=5
         ICHK=ICHK+1
      END IF
C                                                  'TABLE' - TABLE STRETCH
      CALL XVPARM('TABLE',TABBUF,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=6
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT(
     +      ' INVALID COUNT FOR PARAMETER "TABLE"')
      ENDIF
C                                      'ITABLE' - INDIVIDUAL DN TABLE STRETCH
      CALL XVPARM('ITABLE',IPARM,ICOUNT,IDEF,200)
      IF(ICOUNT .GE. 2) THEN
         IMODE=7
         ICHK=ICHK+1
         NPAIRS = ICOUNT/2
         IF(2*NPAIRS.NE.ICOUNT) CALL MYABORT( 
     +      ' INVALID COUNT FOR PARAMETER "ITABLE"')
         DO I=1,ICOUNT
            TABBUF(I)=IPARM(I)
         ENDDO
      ENDIF
C                                                  'PSTRETCH' - PERIODIC STRETCH
      IF(XVPTST('PSTRETCH')) THEN
         IMODE=8
         ICHK=ICHK+1
      END IF
C                                      'FUNCTION' - USER SPECIFIED FUNCTION 
      CALL XVP('FUNCTION',FNCBUF,ICOUNT)
      CALL XVSPTR(FNCBUF,1,I,NCFUNC)
      IF (NCFUNC.GT.1) THEN
          IMODE=9
          ICHK=ICHK+1
      END IF
C                                                  'GAMMA' - GAMMA STRETCH
      CALL XVPARM('GAMMA',GAMMA,ICOUNT,IDEF,1)
      IF(ICOUNT .EQ. 1) THEN
         IMODE=10
         ICHK=ICHK+1
      ENDIF
C                                CHECK THAT ONLY ONE STRETCH WAS SPECIFIED
      IF(ICHK.GT.1) THEN
         CALL XVMESSAGE(' *** MULTIPLE STRETCHES SPECIFIED ***',' ')
         CALL MYABORT('  ONLY ONE STRETCH MAY BE SPECIFIED')
      END IF
C        *** END OF GENERAL PARAMETER PROCESSING ***
C**********************************************************************
C        *** GENERATE LOOKUP TABLE FOR SPECIFIED STRETCH ***
      IF(IMODE.LT.1 .OR. IMODE.GT.10) CALL MYABORT(
     +   ' *** ILLEGAL STRETCH MODE ***')
      GO TO (110,120,110,140,150,160,170,180,190,200) IMODE
C                                                  --- COMPLEMENT MODE ---
110   CALL XVMESSAGE(' *** COMPLEMENT MODE ***',' ')
      DO I=DNMIN,DNMAX
         LUT(I) = DNMAX - (I-DNMIN)
      ENDDO
      WRITE (PRT,115) DNMIN,DNMAX,DNMAX,DNMIN
  115 FORMAT (' Complement Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR = 58
      GO TO 800
C                                               --- LINEAR STRETCH MODE ---
120   CALL XVMESSAGE(' *** LINEAR CONTRAST STRETCH MODE ***',' ')
      IF(NMIN.EQ.NMAX) NMAX = NMAX+1
C                                                 COMPUTE STRETCH TABLE
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO I = DNMIN,DNMAX
         LUT(I) = MAX(DNMIN,MIN(DNMAX,NINT(A*I + B)))
      ENDDO
      WRITE (PRT,125) NMIN,DNMIN,NMAX,DNMAX
  125 FORMAT (' Linear Stretch:',I9,' to',I9,' and',I9,' to',I9)
      NCHAR = 62
      GO TO 800
C                                                   --- CONTOUR MODE ---
140   CALL XVMESSAGE(' *** CONTOUR MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      CONINC=IABS(CONINC)
      DO I=DNMIN,DNMAX,CONINC
         LUT(I) = DNVAL
      ENDDO
      WRITE (PRT,145) CONINC,DNVAL
  145 FORMAT (' Contour Stretch: Interval =',I5,'  DNvalue =',I7)
      NCHAR=51
      GO TO 800
C                                                    --- ALARM MODE ---
150   CALL XVMESSAGE(' *** ALARM MODE ***',' ')
      DNVAL=DNMAX
      CALL XVPARM('DNVALUE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) DNVAL=IPARM(1)
      DO I=DNMIN,DNMAX
         LUT(I) = I
      ENDDO
      DO I=1,NALARM
         IDN=ALRBUF(I)
         IF (IDN.GE.DNMIN .AND. IDN.LE.DNMAX) LUT(IDN)=DNVAL
      ENDDO
      WRITE (PRT,155) DNVAL
  155 FORMAT (' Alarm Stretch: DNvalue =',I7)
      NCHAR=32
      GO TO 800
C                                                 --- TABLE STRETCH MODE ---
160   CALL XVMESSAGE(' *** TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      NINTRV=NPAIRS-1
      DO J=1,NINTRV
         INDN1  = NINT( TABBUF(2*(J-1)+1) )
         OUTDN1 = TABBUF(2*(J-1)+2)
         INDN2  = NINT( TABBUF(2*J+1) )
         OUTDN2 = TABBUF(2*J+2)
         IF(INDN1.LT.DNMIN.OR.INDN2.LE.INDN1.OR.INDN2.GT.DNMAX)
     +      CALL MYABORT(' *** TABLE STRETCH PARAMETER ERROR')
         A=(OUTDN2-OUTDN1)/(INDN2-INDN1)
         B=OUTDN1-A*INDN1
         DO I = INDN1,INDN2
            LUT(I) = NINT(A*I + B)
         ENDDO
      ENDDO
      PRT = ' Table Stretch'
      NCHAR=14
      GO TO 800
C                                                  --- ITABLE MODE ---
170   CALL XVMESSAGE(' *** INDIVIDUAL DN TABLE STRETCH MODE ***',' ')
      CALL XVPARM('BACKGND',BCKGND,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DO I=DNMIN,DNMAX
            LUT(I) = BCKGND
         ENDDO
      ELSE
         DO I=DNMIN,DNMAX
            LUT(I) = I
         ENDDO
      END IF
      DO I=1,NPAIRS
         INDN  = TABBUF(2*(I-1)+1)
         IF(INDN.LT.DNMIN .OR. INDN.GT.DNMAX) CALL MYABORT(
     +      ' *** ERROR IN ITABLE PARAMETER')
         OUTDN = TABBUF(2*(I-1)+2)
         IF(OUTDN.LT.DNMIN) OUTDN=DNMIN
         IF(OUTDN.GT.DNMAX) OUTDN=DNMAX
         LUT(INDN)=OUTDN
      ENDDO
      PRT = ' Individual Table Stretch'
      NCHAR=25
      GO TO 800
C                                                     --- PSTRETCH MODE ---
180   CALL XVMESSAGE(' *** PERIODIC STRETCH MODE ***',' ')
C                                                    get related parameters 
      CALL XVPARM('FREQ',FREQ,ICOUNT,IDEF,1)
      CALL XVPARM('PHI',PHI,ICOUNT,IDEF,1)
      CALL XVPARM('DC',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         DC = RPARM(1)
      ELSE
         DC = (DNMAX+DNMIN)/2.0
      END IF
      CALL XVPARM('AMPL',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) THEN
         AMPL = RPARM(1)/2.0
      ELSE
         AMPL = (DNMAX-DNMIN)/2.0
      END IF
      W=2.0*3.14159*FREQ/(DNMAX-DNMIN)
      DO I=DNMIN,DNMAX
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(AMPL*SIN(W*I+PHI) + DC)))
      ENDDO
      WRITE (PRT,185) FREQ,DC,2.0*AMPL,PHI
  185 FORMAT (' Periodic Stretch: FREQ=',F6.2,'  DC=',F8.1,
     +        '  AMPL=',F8.1,'  PHI=',F6.2)
      NCHAR=70
      GO TO 800
C                                                 --- FUNCTION MODE ---
190   CALL XVMESSAGE(' *** USER SPECIFIED FUNCTION MODE ***',' ')
      J = MIN(NCFUNC,50)
      NCHAR=30+J
      PRT = ' Function Stretch: FUNCTION = ' // FNCBUF(1:J)
C
      CALL ZIA(RPARM,1000)
      CALL KNUTH(FNCBUF,RPARM,IER)
      IF(IER.NE.0) THEN
         CALL XVMESSAGE(' *** ERROR IN PARSING FUNCTION WITH KNUTH',' ')
         CALL MYABORT(FNCBUF)
      END IF
      DO I=DNMIN,DNMAX
         RPARM(1)=I
         CALL XKNUTH(RPARM,DN)
         LUT(I) = MIN(DNMAX, MAX(DNMIN, NINT(DN)))
      ENDDO
      GO TO 800
C                                                      --- GAMMA OPTION ---
200   CONTINUE
      WRITE(PRT,205) GAMMA
205   FORMAT(' *** Gamma Stretch, Gamma =',F6.3)
      DNRANGE = DNMAX - DNMIN
      DO I=DNMIN,DNMAX
          LUT(I) = NINT(DNRANGE*((I-DNMIN)/DNRANGE)**(1.0/GAMMA)+DNMIN)
      END DO
C                                               *** LOOKUP TABLE COMPLETED ***
C*****************************************************************************
  800 CONTINUE
      CALL XVMESSAGE(PRT,' ')
      CALL XVPARM('POST',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
C                                          *** PERFORM POST-STRETCH ***
C                                   CALCULATE LINEAR TRANSFORMATION SO THAT
C                                   DNMIN GOES TO LVAL AND DNMAX GOES TO HVAL.
C                                   MODIFY LOOKUP TABLE TO INCLUDE THIS STRETCH.
        CALL XVMESSAGE(' ',' ')
        CALL XVMESSAGE(' *** POST-STRETCH OPTION ***',' ')
        LVAL=IPARM(1)
        HVAL=IPARM(2)
        IF(LVAL.LT.DNMIN .OR. HVAL.LT.DNMIN) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER LESS THAN DNMIN',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMIN',' ')
           IF(LVAL.LT.DNMIN) LVAL=DNMIN
           IF(HVAL.LT.DNMIN) HVAL=DNMIN
        END IF
        IF(LVAL.GT.DNMAX .OR. HVAL.GT.DNMAX) THEN
           CALL XVMESSAGE(' POST-STRETCH PARAMETER EXCEEDS DNMAX',' ')
           CALL XVMESSAGE(' PARAMETER RESET TO DNMAX',' ')
           IF(LVAL.GT.DNMAX) LVAL=DNMAX
           IF(HVAL.GT.DNMAX) HVAL=DNMAX
        END IF
        IF(LVAL.EQ.HVAL) CALL MYABORT(
     +     ' *** ERROR - POST VALUES SPECIFIED ARE EQUAL')
C                                           COMPUTE COMPOSITE LOOKUP TABLE
        A = FLOAT(HVAL-LVAL)/FLOAT(DNMAX-DNMIN)
        B = -A*DNMIN+LVAL
        DO I = DNMIN,DNMAX
           LUT(I) = NINT( A*LUT(I) + B )
        ENDDO
C                                            SET UP SECOND ADDED LABEL
        WRITE (PRT2,810) DNMIN,LVAL,DNMAX,HVAL
  810   FORMAT (' Post-stretch:',I7,' to',I7,' and',I7,' to',I7)
        NCHAR2=52
        CALL XVMESSAGE(PRT2,' ')
      END IF
C                                        *** LOOKUP TABLE GENERATED ***
C ***********************************************************************
C                                              *** PERFORM STRETCH ***
C                                                   UPDATE LABEL
      CALL XLADD(OUNIT,'HISTORY','PARMS',PRT,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR,0)
      IF(NCHAR2.GT.0) CALL XLADD(OUNIT,'HISTORY','PARMS2',PRT2,STAT,
     &           'FORMAT','STRING','ULEN',NCHAR2,0)
C                                              APPLY STRETCH TABLE TO THE DATA
      DO L=SL,SL+NLO-1
         CALL XVREAD(IUNIT,BUF,STAT,'LINE',L,'SAMP',SS,'NSAMPS',NSO,0)
         DO I=1,NSO
            IF (BUF(I) .GE. DNMAX) THEN
               BUF(I) = LUT(DNMAX)
            ELSE IF (BUF(I) .LE. DNMIN) THEN
               BUF(I) = LUT(DNMIN)
            ELSE
               BUF(I) = LUT(BUF(I))
	    END IF
         END DO
         CALL XVWRIT(OUNIT,BUF,STAT,0)
      END DO
      RETURN
      END
C*************************************************************************
        subroutine MYABORT(msg)
        character*(*) msg
        call xvmessage(msg, ' ')
        call abend
        return
        end

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create stretchf.pdf
PROCESS     HELP=*
PARM INP      TYPE=(STRING,40)  COUNT=1
PARM OUT      TYPE=(STRING,40)  COUNT=1           
PARM SIZE     TYPE=INTEGER COUNT=4           DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1           DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1           DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1           DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1           DEFAULT=0
PARM DNMIN    TYPE=INTEGER
PARM DNMAX    TYPE=INTEGER
PARM LINEAR   TYPE=INTEGER COUNT=(0,2)              DEFAULT=--
PARM CONTOUR  TYPE=INTEGER COUNT=(0,1)              DEFAULT=--
PARM GAMMA    TYPE=REAL    COUNT=(0,1)              DEFAULT=--
PARM FUNCTION TYPE=(STRING,128)  COUNT=(0:1)        DEFAULT=--
PARM ALARM    TYPE=INTEGER COUNT=(0:50)            DEFAULT=--
PARM TABLE    TYPE=REAL    COUNT=(0,2:200)          DEFAULT=--
PARM ITABLE   TYPE=INTEGER COUNT=(0,2:200)          DEFAULT=--
PARM POST     TYPE=INTEGER COUNT=(0,2)              DEFAULT=--
PARM OTHER    TYPE=KEYWORD COUNT=0:1    DEFAULT=--  VALID=(COMP,PSTRETCH)
PARM DNVALUE  TYPE=INTEGER COUNT=0:1                DEFAULT=--
PARM BACKGND  TYPE=INTEGER COUNT=0:1                DEFAULT=--
PARM FREQ     TYPE=REAL    COUNT=0:1                DEFAULT=1
PARM AMPL     TYPE=REAL    COUNT=0:1                DEFAULT=--
PARM PHI      TYPE=REAL    COUNT=0:1                DEFAULT=0
PARM DC       TYPE=REAL    COUNT=0:1                DEFAULT=--
END-PROC
.TITLE
 VICAR Program STRETCHF
.HELP
    STRETCHF is a VICAR application program which changes the point by point 
intensity of an image by generating a transfer function on the domain of
intensity values.  It is similar to the program STRETCH, but is designed to
accept FULLWORD (integer*4) input. Nine types of stretches are available. 
These are:

    COMP,LINEAR,CONTOUR,ALARM,GAMMA,TABLE,ITABLE,PSTRETCH,FUNCTION 

     The input image must be fullword. The output image will also be fullword.
The user must specify the DN range of the data by using the DNMIN and DNMAX 
parameters.


     An optional linear post stretch may be performed following the initial 
stretch. This post stretch will take the output image DN values ranging from 
DNMIN to DNMAX to a user specified DN range. This post stretch does not require
a separate pass through the data. Instead, the initial stretch and the post
stretch are incorporated in a single look-up table which is then used to process
the input data.
.PAGE
 RESTRICTIONS:

          1. Only one of the nine types of stretches
             available may be specified in a single execution.
          2. Input image must be in either fullword format.
          3. If a post-stretch is requested the values
             specified must be in the range DNMIN to DNMAX.

EXECUTION:

   The following is the execution statement for STRETCH:
          STRETCH  INP  OUT  PARAMS
where INP, OUT, and PARAMS are parameters discussed in their
respective parameter section in TUTOR mode.
.LEVEL1
.VARIABLE INP
 Input dataset
.VARIABLE OUT
 Output data set
.VARIABLE SIZE
 Size field = (SL,SS,NL,NS)
.VARIABLE SL
 Starting line
.VARIABLE SS
 Starting sample
.VARIABLE NL
 Number of lines
.VARIABLE NS
 Number of samples
.VARIABLE LINEAR
 Linear stretch
 supply MIN, MAX
.VARIABLE CONTOUR
 Contour stretch - needs contour
 interval [See DNVALUE]
.VARIABLE FUNCTION       
 User specified function stretch
 (Example: FUNC="2*DN+1")
.VARIABLE ALARM
 Alarm stretch - list DN's to
 alarm [See DNVALUE]
.VARIABLE GAMMA
 Gamma stretch
 Supply value of gamma
.VARIABLE TABLE
 Table stretch
 (N Pairs of values - 
   In, Out,
   In, Out,
   In, Out,...)
 [See BACKGND]
.VARIABLE ITABLE
 Individual table stretch
 (N Pairs of values -
   In, Out,
   In, Out,
   In, Out,...)
 [See BACKGND]
.VARIABLE POST              
 Post-stretch option
 supply MIN, MAX
.VARIABLE OTHER
 Other stretch options
 Valid: COMP, PSTRETCH
.VARIABLE DNVALUE
 DN value used for CONTOUR and
 ALARM stretches
.VARIABLE BACKGND
 Background DN value used for
 TABLE and ITABLE stretches
.VARIABLE FREQ
 Frequency for PSTRETCH stretch
.VARIABLE AMPL
 Amplitude for PSTRETCH stretch
.VARIABLE PHI
 Phase for PSTRETCH stretch
.VARIABLE DC
 Mean for PSTRETCH stretch
.VARIABLE DNMIN
 Minimum DN value 
.VARIABLE DNMAX   
 Maximum DN value  
.LEVEL2
.VARIABLE INP
A standard Vicar input data set
.VARIABLE OUT
A standard Vicar output file.
.VARIABLE SIZE
  FOUR INTEGERS - A standard Vicar size field specifying starting line,
                  starting sample, number of lines, and number of samples.
.VARIABLE SL
 INTEGER - Starting line
.VARIABLE SS
 INTEGER - Starting sample
.VARIABLE NL
 INTEGER - Number of lines
.VARIABLE NS
 INTEGER - Number of samples

.VARIABLE DNMIN
 INTEGER -      Specifies the minimum DN value of the input data. 
		The domain of the stretch function is DNMIN to DNMAX.
.VARIABLE DNMAX
 INTEGER -	Specifies the maximum DN value of the input data.
		The domain of the stretch function is DNMIN to DNMAX.
.VARIABLE LINEAR
 TWO INTEGERS - LINEAR=(min,max) performs the following transform:
		           a = (dnmax-dnmin)/(max-min)
		           DN(out) = a*(DN(in)-min) + dnmin   
                if DN(out) would be less than DNMIN:    DN(out) = DNMIN
                if DN(out) would be greater than DNMAX: DN(out) = DNMAX
.VARIABLE CONTOUR
 INTEGER -	Input intensities which are a multiple of n are
		set to the value specified by the DNVALUE parameter.
.VARIABLE ALARM
 N INTEGERS -	The input DN values specified are set the value
                specified by the DNVALUE parameter.
.VARIABLE GAMMA
 This parameter causes a "gamma" stretch to be performed. The gamma
 stretch is defined by the following formula:
                            /                        \
          DN    = DNRANGE * | (DN  -DN   ) / DNRANGE | ** (1.0/gamma)
            out             |    in   min            |
                            \                        /
    where the user specifies the parameter, gamma.
 
 In all cases, the output DN is rounded to the nearest integer prior to
 output.
.VARIABLE TABLE
 N PAIRS REALS - TABLE=(in1,out1,in2,out2,..,inN,outN) will set the
		 input DN values in1,in2,... to the DN values 
                 out1,out2,... and will linearly interpolate between
		 these points to compute the intervening DN values.
		 All DN values outside the range in1 to inN will 
		 remain unchanged unless the parameter BACKGND is 
		 also specified.  In that case, all DN values 
		 outside the range in1 to inN will be set to
                 the BACKGND value. The maximum number of pairs 
		 which may be specified is 100.  The values 
		 in1,in2,...,inN must be specified in increasing 
		 order.  
.VARIABLE ITABLE
 N PAIRS INTEGERS - ITABLE=(in1,out1,in2,out2,...,inN,outN) will
		set the input DN values in1,in2,... to the DN values 
                out1,out2,... but will not do any interpolation.
		All DN values not explicitly specified, including
 		those outside the range in1 to inN will remain
 		unchanged unless the BACKGND parameter is also
		specified.  In that case, all DN values not
                explicitly specified, including those outside the
                range in1 to inN, will be set to the BACKGND value.
                The maximum number of pairs which may be specified 
		is 100. 
.VARIABLE FUNCTION
 STRING - 	Allows the user to explicitly specify the transfer
		function as a FORTRAN type expression with the
		keyword DN as the independent variable.  This
                is similar to the VICAR program F2. For example,
		FUNCTION="2*DN+3" will multiply each input DN
		value by 2 and add 3.  This will then be the output
                DN value, provided it is in the range DNMIN to DNMAX.
		If it is less than DNMIN it is set to DNMIN or if it
                is greater than DNMAX it is set to DNMAX.
.VARIABLE OTHER
 KEYWORD   There are two stretch options that are specified by a keyword.
           These two (COMP and PSTRETCH) options are lumped together here.

   COMP     - Causes intensity levels to be complemented. The actual transform 
              is:          DN(out) = DNMAX+DNMIN-DN(in)

   PSTRETCH - Specifies that a periodic stretch is to be performed.
              The transfer function is:

              DN(out)=AMPL/2*SIN[2*PI*FREQ*DN(in)/(DNMAX-DNMIN)+PHI]+DC

                  where PI = 3.14159
                  and AMPL,FREQ,PHI,DC are parameters which may be specified 
                  seperately by the user and which have the following defaults:
                                  AMPL= DNMAX-DNMIN
                                  FREQ= 1.0
                                  PHI = 0.0
                                  DC  = (DNMAX-DNMIN)/2
.VARIABLE POST
 TWO INTEGERS - Specifies that a linear post stretch will be performed
                following the initial stretch. The initial stretch
	        will performed over the range DNMIN to DNMAX.
                Specifying a post stretch of POST=(LVAL,HVAL) will
                then take DN value DNMIN to LVAL and DN value DNMAX
                to HVAL. Actually only one stretch is performed;  
                the composite of the initial and post stretches.
                (Default is that no post-stretch is performed.)
.VARIABLE DNVALUE
 INTEGER -      Specifies the DN value to use for the CONTOUR and
		ALARM stretches. The appropriate DN's will be set
		to this DN value.  (Default is DNVALUE=DNMAX)
.VARIABLE BACKGND
 INTEGER -      Specifies a DN value to use as background for the
		TABLE and ITABLE stretches.  All DN values outside
		the range specified by TABLE, or all those not
		explicitly specified by ITABLE will be set to this 
		value.  (Default is to leave these DN's unchanged.)
.VARIABLE FREQ
 REAL -         Specifies the frequency to be used in the case of
		the PSTRETCH (periodic) stretch.  (Default is
		FREQ=1.0)
.VARIABLE AMPL
 REAL -         Specifies the amplitude to be used in the case of
		the PSTRETCH (periodic) stretch.  (Default is
		AMPL=DNMAX-DNMIN)
.VARIABLE PHI
 REAL -         Specifies the phase to be used in the case of 
		the PSTRETCH (periodic) stretch.  (Default is
		PHI=0)
.VARIABLE DC
 REAL -         Specifies the mean to be used in the case of 
		the PSTRETCH (periodic) stretch.  (Default is
		DC=(DNMAX-DNMIN)/2 )
.END
$ Return
$!#############################################################################
$Imake_File:
$ create stretchf.imake
#define  PROGRAM   stretchf

#define MODULE_LIST stretchf.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P3SUB
#define LIB_P2SUB
$ Return
$!#############################################################################
