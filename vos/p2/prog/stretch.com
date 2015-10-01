$!****************************************************************************
$!
$! Build proc for MIPL module stretch
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:26:49
$!
$! Execute by entering:		$ @stretch
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
$ write sys$output "*** module stretch ***"
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
$ write sys$output "Invalid argument given to stretch.com file -- ", primary
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
$   if F$SEARCH("stretch.imake") .nes. ""
$   then
$      vimake stretch
$      purge stretch.bld
$   else
$      if F$SEARCH("stretch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretch.bld "STD"
$   else
$      @stretch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretch.com -mixed -
	-s stretch.f manual_stretch.f hist_params.f get_histogram.f -
	   exclude_histogram.f histogram_stretch.f bimodal.f hpeak.f -
	   astretch.f post_stretch.f print_histogram.f stati.f -
	-p stretch.pdf -
	-t tststretch.pdf tststretch.log -
	-i stretch.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      PROGRAM STRETCH
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT NONE
      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX		!Input DN range
      INTEGER*4 DNMIN,DNMAX		!Output DN range

C     ....Note: HIST is made an extra 512 words longer to prevent overflow
C     ....when reading it in from an IBIS file.
      INTEGER*4 HIST(-32768:33280)	!Input histogram
      INTEGER*4 BHIST(0:255)		!Copy of input histogram (byte data)
      INTEGER*2 BUF(65536)		!Input line buffer
      INTEGER*2 LUT(-32768:32767)	!Stretch look-up table

      INTEGER*4 STAT,IMODE,NSTRETCH
      INTEGER*4 HFLAG,IHIST,OHIST,ICDF,OCDF,NPTS
      CHARACTER*8 FORMAT

      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NCHAR,NCHAR2
      CHARACTER*80 PRT,PRT2

      CALL XVMESSAGE('STRETCH version 11 Jan 2013',' ')

C     ....Open input image and check format and size field
      CALL OPENINP(format,sl,ss,sb,nlo,nso,nbo,nli,nsi,nbi,iunit,inmin,
     .             inmax)

C     ....Open output image and check output DN range
      CALL OPENOUT(FORMAT,NLO,NSO,NBO,ounit,dnmin,dnmax)

      IMODE=0			!Stretch mode
      NSTRETCH = 0              !Count of number of stretches specified
C     ....Compute look-up table for non-histogram stretches
      CALL MANUAL_STRETCH(FORMAT,lut,imode,nstretch,nchar,prt)
C     ....Get most histogram-related user parameters
      CALL HIST_PARAMS(FORMAT,imode,hflag,ihist,ohist,
     &          icdf,ocdf,power,nstretch)
      IF (HFLAG.EQ.0) GOTO 50
C     ....Compute or read the input image histogram
      CALL GET_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
C     ....Print input histogram and CDF, if specified
      CALL PRINT_HISTOGRAM(HIST,IHIST,ICDF,INMAX,TMEAN,TSIGMA,NPTS)
C     ....Exclude DN ranges from histogram, if specified
      CALL MVE(4,256,HIST(0),BHIST,1,1)		!Save it
      CALL EXCLUDE_HISTOGRAM(FORMAT,hist,npts)
C     ....Compute look-up table for histogram stretches
      CALL HISTOGRAM_STRETCH(FORMAT,HIST,NPTS,imode,lut,nchar,prt)

   50 CALL POST_STRETCH(lut,nchar2,prt2)

C     ....Stretch the image and write to output
      CALL STRETCH_IMAGE(FORMAT,PRT,PRT2,NCHAR,NCHAR2,LUT,buf)

C     ....Print output histogram and CDF
      IF (OHIST+OCDF.GT.0)
     &	  CALL  PRINT_OHISTOGRAM(BHIST,LUT,OHIST,OCDF,NPTS)
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open input image and check format and size field
C
      SUBROUTINE OPENINP(format,sl,ss,sb,nlo,nso,nbo,nli,nsi,nbi,
     +		iunit,inmin,inmax)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN
      INTEGER*4 SL,SS,NLO,NSO,NLI,NSI,IUNIT,INMIN,INMAX,SB,NBO,NBI

      INTEGER*4 STAT

      CALL XVUNIT(iunit,'INP',1,stat, ' ')
      CALL XVOPEN(IUNIT,stat,'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,STAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')


      CALL XVSIZE(sl,ss,nlo,nso,nli,nsi)
      CALL XVBANDS(sb,nbo,nbi)

      IF ( sb .GT. nbi ) CALL MABEND(
     +  'SB is greater than the total number of bands')
      
      IF (SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE('***Number of lines truncated',' ')
         NLO = NLI - SL + 1
      ENDIF
      IF (SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE('***Number of samples truncated', ' ')
         NSO = NSI - SS + 1
      ENDIF
      IF (SB+NBO-1 .GT. NBI) THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ')
         NBO = NBI - SB + 1
      ENDIF

      CALL XVGET(IUNIT,stat,'FORMAT',format,' ')
      IF (FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('***Invalid input image data format',' ')
         CALL XVMESSAGE('***Image must be byte or halfword',' ')
         CALL MABEND('***STRETCH task cancelled')
      ENDIF

C     ....Set input DN range
      IF (FORMAT.EQ.'BYTE') THEN
         INMIN = 0
         INMAX = 255
      ELSE
         INMIN = -32768
         INMAX = 32767
      ENDIF
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open output image and check output DN range
C
      SUBROUTINE OPENOUT(FORMAT,NLO,NSO,NBO,ounit,dnmin,dnmax)
      IMPLICIT NONE
      INTEGER*4 OUNIT,NLO,NSO,DNMIN,DNMAX,NBO
      CHARACTER*8 FORMAT

      INTEGER*4 STAT,ICOUNT,IDEF,IDEF2,NI

      CALL XVUNIT(ounit,'OUT',1,stat,' ')
      CALL XVOPEN(OUNIT,stat,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     &            'U_NL',NLO,'U_NS',NSO, 'U_NB' ,NBO, ' ')

C     ....Get output DN range DNMIN,DNMAX
      CALL XVPARM('DNMIN',dnmin,icount,idef,1)
      CALL XVPARM('DNMAX',dnmax,icount,idef2,1)
      IF (FORMAT.EQ.'HALF') THEN      
         IF (IDEF.EQ.1) DNMIN=-32768
         IF (IDEF2.EQ.1) DNMAX=32767
      ELSE		!Here for byte data
         IF (DNMIN.LT.0) THEN
            CALL XVMESSAGE('*** DNMIN RESET TO BYTE MIN OF 0', ' ')
            DNMIN=0
         ENDIF
         IF (DNMAX.GT.255) THEN
            CALL XVMESSAGE('*** DNMAX RESET TO BYTE MAX OF 255', ' ')
            DNMAX=255
         ENDIF
      ENDIF

C     ....Check restrictions imposed by HISTGEN input
      CALL XVPCNT('INP',NI)	!Number of input files.
      IF (DNMIN.NE.0 .AND. NI.EQ.2) DNMIN=0
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Stretch the image using LUT and write to output
C
      SUBROUTINE STRETCH_IMAGE(FORMAT,PRT,PRT2,NCHAR,NCHAR2,LUT,buf)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 NCHAR,NCHAR2	!Number of chars in labels
      CHARACTER*80 PRT,PRT2	!Labels for stretch and post-stretch
      INTEGER*2 LUT(-32768:32767),BUF(*)

      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,L,EL,IDN,STAT
      LOGICAL*1 LUTB(256)		!Byte look-up table
      INTEGER*4 BAND
      INTEGER*4 BANDOUT,LINEOUT

C     ....If byte data, compress LUT to byte table
      IF (FORMAT.EQ.'BYTE') CALL MVE(-3,256,LUT(0),LUTB,1,1)

      EL = NLO+SL-1
      BANDOUT = 0
      DO BAND=SB,SB+NBO-1
       BANDOUT = BANDOUT + 1
       LINEOUT = 0
       DO 10 L=SL,EL
        LINEOUT = LINEOUT + 1  
        CALL XVREAD(IUNIT,buf,stat,'LINE',L,'SAMP',SS,'NSAMPS',NSO,
     +             'BAND',BAND,' ')
        IF (FORMAT.EQ.'BYTE') THEN
         CALL TBL(buf,LUTB,NSO)
        ELSE
         DO I=1,NSO
            IDN = BUF(I)
            BUF(I) = LUT(IDN)
         ENDDO
       ENDIF
   10  CALL XVWRIT(OUNIT,BUF,stat,'LINE',LINEOUT,'BAND',BANDOUT,' ')
      END DO

C     ....Add description of stretch and post-stretch to label
      CALL XLADD(OUNIT,'HISTORY','PARMS',PRT,STAT,
     +  	 'FORMAT','STRING','ULEN',NCHAR,' ')
      IF (NCHAR2.GT.0) CALL XLADD(OUNIT,'HISTORY','PARMS2',
     +		PRT2,STAT,'FORMAT','STRING','ULEN',NCHAR2,' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create manual_stretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute look-up table for non-histogram stretches
C
      SUBROUTINE MANUAL_STRETCH(FORMAT,lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 FORMAT	!'BYTE' or 'HALF'
      INTEGER*2 LUT(-32768:32767)!Computed stretch table
      INTEGER*4 IMODE		!Stretch mode
      INTEGER*4 NSTRETCH	!Number of stretch options specified
      INTEGER*4 NCHAR		!Number of characters in PRT buffer
      CHARACTER*80 PRT		!String to be inserted in image label

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      CALL SLINEAR(lut,imode,nstretch,nchar,prt)
      CALL SCOMP(lut,imode,nstretch,nchar,prt)
      CALL SCLIP(FORMAT,lut,imode,nstretch,nchar,prt)
      CALL SCONTOUR(lut,imode,nstretch,nchar,prt)
      CALL SALARM(lut,imode,nstretch,nchar,prt)
      CALL SITABLE(lut,imode,nstretch,nchar,prt)
      CALL STABLE(lut,imode,nstretch,nchar,prt)
      CALL SPERIODIC(lut,imode,nstretch,nchar,prt)
      CALL SLOG(lut,imode,nstretch,nchar,prt)
      CALL SFUNC(lut,imode,nstretch,nchar,prt)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCOMP(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ITRUNC
      LOGICAL XVPTST

      IF (.NOT.XVPTST('COMP')) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** COMPLEMENT MODE ***',' ')
      IMODE=1

      DO IDN=INMIN,INMAX
         LUT(IDN) = ITRUNC(DNMIN+DNMAX-IDN)
      ENDDO

      WRITE (PRT,115) DNMIN,DNMAX,DNMAX,DNMIN
  115 FORMAT ('Complement Stretch:',I7,' to',I7,' and',I7,' to',I7)
      NCHAR=58
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SLINEAR(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN
      REAL*4 RPARM(2),RMIN,RMAX,A,B,RTRUNC

      CALL XVPARM('LINEAR',RPARM,ICOUNT,IDEF,2)
      IF (ICOUNT.LT.2) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** LINEAR CONTRAST STRETCH MODE ***',' ')
      IMODE=2
      RMIN = RPARM(1)
      RMAX = RPARM(2)
      IF (RMIN.EQ.RMAX) RMAX=RMAX+1

      A = FLOAT(DNMAX-DNMIN)/(RMAX-RMIN)	!Scale
      B = -A*RMIN+DNMIN				!Offset
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

      WRITE (PRT,125) NINT(RMIN),DNMIN,NINT(RMAX),DNMAX
  125 FORMAT ('Linear Stretch:',I7,' to',I7,' and',I7,' to',I7)
      IF (NINT(RMIN) .NE. RMIN)  CALL REALCON(RMIN,PRT(16:),7)
      IF (NINT(RMAX) .NE. RMAX)  CALL REALCON(RMAX,PRT(37:),7)
      NCHAR=54
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCLIP(FORMAT,lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT
      include  'fortport'       !Defines INT2BYTE

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,NBITS,IDN,ODN,ITRUNC
      INTEGER*2 HVALUE
      BYTE LVALUE
  133 FORMAT ('Bit Clip Stretch:',I3,
     +          ' Most  Significant Bits Clipped')
  136 FORMAT ('Bit Clip Stretch:',I3,
     +          ' Least Significant Bits Clipped')

      CALL XVPARM('CLIP',nbits,icount,idef,1)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** BIT CLIPPING MODE ***',' ')
      IMODE = 3

      DO IDN=INMIN,INMAX
         IF (FORMAT.EQ.'BYTE') THEN
            LVALUE = INT2BYTE(IDN)
            CALL SHFV(1,1,NBITS,lvalue,1)
            ODN = BYTE2INT(LVALUE)
         ELSE
            HVALUE = IDN
            CALL SHFV(2,1,NBITS,hvalue,1)
            ODN = HVALUE
         END IF
         LUT(IDN) = ITRUNC(ODN)
      ENDDO

      IF (NBITS.GT.0) THEN
         WRITE (PRT,133) NBITS
      ELSE
         WRITE (PRT,136) -NBITS
      ENDIF
      NCHAR=52
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SCONTOUR(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,INC,DNVAL,IDN,ITRUNC

      CALL XVPARM('CONTOUR',inc,icount,idef,1)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** CONTOUR MODE ***',' ')
      IMODE = 4
      CALL INIT_LUT(LUT)	!Initialize the LUT

C     ....Get DNVALUE of contours
      CALL XVPARM('DNVALUE',dnval,icount,idef,1)
      IF (ICOUNT.EQ.0) DNVAL=DNMAX
      DNVAL = ITRUNC(DNVAL)

C     ....Put contours into LUT
      DO IDN=DNMIN,DNMAX,INC
         LUT(IDN) = DNVAL
      ENDDO

      WRITE (PRT,145) INC,DNVAL
  145 FORMAT ('Contour Stretch: Interval =',I5,'  DNvalue =',I7)
      NCHAR=51
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SALARM(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

c     COMMON/CW/ALRBUF
      INTEGER*4 ALRBUF(256)

      INTEGER*4 I,ICOUNT,IDEF,DNVAL,IDN,ITRUNC,NALARM
  155 FORMAT ('Alarm Stretch: DNvalue =',I7)

      CALL XVPARM('ALARM',alrbuf,nalarm,idef,100)
      IF (NALARM.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** ALARM MODE ***',' ')
      IMODE = 5
      CALL INIT_LUT(LUT)	!Initialize the LUT

      CALL XVPARM('DNVALUE',dnval,icount,idef,1)	!Alarm DN value
      IF (ICOUNT.EQ.0) DNVAL=DNMAX
      DNVAL = ITRUNC(DNVAL)

      DO I=1,NALARM		!Set the alarms
         IDN = ALRBUF(I)
         LUT(IDN) = DNVAL
      END DO
      WRITE (PRT,155) DNVAL
      NCHAR=32
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SITABLE(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

c     COMMON/CW/IPARM
      INTEGER*4 IPARM(512)

      INTEGER*4 I,ICOUNT,IDEF,IDN,NPAIRS,ITRUNC

      CALL XVPARM('ITABLE',iparm,icount,idef,512)
      IF (ICOUNT.LT.2) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** INDIVIDUAL DN TABLE STRETCH MODE ***',' ')
      IMODE = 7
      NPAIRS = ICOUNT/2
      IF (2*NPAIRS.NE.ICOUNT) THEN
         CALL XVMESSAGE('***Invalid number of table entries',' ')
         CALL ABEND
      END IF

      CALL INIT_LUT(LUT)	!Initialize the lookup table
      ICOUNT = 2*NPAIRS		!Set the individual table elements
      DO I=1,ICOUNT,2
         IDN = IPARM(I)
         LUT(IDN) = ITRUNC(IPARM(I+1))
      END DO
      PRT = 'Individual Table Stretch'
      NCHAR=25
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE STABLE(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

cc    COMMON/CW/TABBUF
      REAL*4 TABBUF(512)

      INTEGER*4 I,N,ICOUNT,IDEF,IDN,NPAIRS
      INTEGER*4 INDN1,OUTDN1,INDN2,OUTDN2
      REAL*8 A,B,RTRUNC8

      CALL XVPARM('TABLE',tabbuf,icount,idef,512)
      IF (ICOUNT.LT.4) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** TABLE STRETCH MODE ***',' ')
      IMODE = 6
      NPAIRS = ICOUNT/2
      IF (2*NPAIRS.NE.ICOUNT) THEN
         CALL XVMESSAGE('***Invalid # of TABLE entries',' ')
         CALL ABEND
      END IF
      N = 2*(NPAIRS-1)		!2*(number of intervals)
      CALL INIT_LUT(lut)	!Initialize the look-up table

      DO I=1,N,2
         INDN1  = NINT(TABBUF(I))
         OUTDN1 = TABBUF(I+1)
         INDN2  = NINT(TABBUF(I+2))
         OUTDN2 = TABBUF(I+3)
         IF (INDN2.LE.INDN1)
     &		 CALL MABEND('***Table stretch parameter error')
         A=(OUTDN2-OUTDN1)/(INDN2-INDN1)	!Scale
         B=OUTDN1-A*INDN1			!Offset
         DO IDN=INDN1,INDN2
            LUT(IDN) = RTRUNC8(A*IDN+B)
         ENDDO
      ENDDO

      PRT = 'Table Stretch'
      NCHAR=14
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SPERIODIC(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN
      REAL*4 FREQ,AMPL,PHI,DC,W,RTRUNC
      LOGICAL XVPTST
  185 FORMAT ('Periodic Stretch: FREQ=',F6.2,'  DC=',F8.1,
     +        '  AMPL=',F8.1,'  PHI=',F6.2)

      IF (.NOT.XVPTST('PSTRETCH')) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** PERIODIC STRETCH MODE ***',' ')
      IMODE = 8
      CALL XVPARM('FREQ',freq,icount,idef,1)
      CALL XVPARM('DC',dc,icount,idef,1)
      IF (ICOUNT.EQ.0) DC=(DNMAX+DNMIN)/2.0
      CALL XVPARM('AMPL',ampl,icount,idef,1)
      IF (ICOUNT.EQ.0) AMPL=DNMAX-DNMIN
      CALL XVPARM('PHI',phi,icount,idef,1)
      W = 2.0*3.14159*FREQ/(DNMAX-DNMIN)
      AMPL = AMPL/2.0
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(AMPL*SIN(W*IDN+PHI)+DC)
      END DO
      WRITE (PRT,185) FREQ,DC,2.0*AMPL,PHI
      NCHAR=70
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SFUNC(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32727)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

c     COMMON/CW/RPARM,FNCBUF
      REAL*4 RPARM(300)
      CHARACTER*1024 FNCBUF

      INTEGER*4 I,J,ICOUNT,IDEF,IER,NCFUNC,IDN
      REAL*4 DN,RTRUNC

      CALL XVPARM('FUNCTION',fncbuf,icount,idef,1)
      CALL XVSPTR(FNCBUF,1,i,ncfunc)
      IF (ICOUNT.EQ.0 .OR. NCFUNC.LE.1) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** USER SPECIFIED FUNCTION MODE ***',' ')
      IMODE = 9
      J = MIN(NCFUNC,50)
      NCHAR = 30 + J
      PRT = 'Function Stretch: FUNCTION = ' // FNCBUF(1:J)
      CALL XVMESSAGE(PRT,' ')
      CALL KNUTH(FNCBUF,RPARM,IER)
      IF (IER.NE.0) THEN
         CALL XVMESSAGE('***Error in parsing function with KNUTH',' ')
         CALL XVMESSAGE(FNCBUF(1:NCFUNC),' ')
         CALL ABEND
      END IF
      DO IDN=INMIN,INMAX
         RPARM(1) = IDN
         CALL XKNUTH(RPARM,DN)
         LUT(IDN) = RTRUNC(DN)
      END DO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SLOG(lut,imode,nstretch,nchar,prt)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IMODE,NSTRETCH,NCHAR
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IDN,ODN
      REAL*4 LOW,HIGH,CURVE,RPARM(2),R,RTRUNC
      REAL*8 AA,BB,CC
  205 FORMAT ('LOGARITHMIC Stretch:',F7.1,' to',I7,' and',F7.1,
     .        ' to',I7,' with CURVE=',G10.2)

      CALL XVPARM('LOG',rparm,icount,idef,2)
      IF (ICOUNT.EQ.0) RETURN
      CALL CHK_NUM_STRETCH(nstretch)
      CALL XVMESSAGE('*** LOGARITHMIC STRETCH MODE ***',' ')
      IMODE = 10
      IF (ICOUNT .EQ. 2) THEN
         LOW  = RPARM(1)
         HIGH = RPARM(2)
      ELSE
         LOW  = DNMIN
         HIGH = DNMAX
      END IF
      IF ( HIGH .EQ. LOW)    CALL MABEND(
     .     'Sorry,the HIGH and LOW parameters may not be equal.')

      CALL XVPARM('CURVE',curve,icount,idef,0)
      IF (LOW + CURVE .LE. 0.)
     .     CALL MABEND('Sorry,LOW + CURVE must be greater than zero.')
      IF (HIGH + CURVE .LE. 0.)
     .     CALL MABEND('Sorry,HIGH + CURVE must be greater than zero.')

      CC = CURVE
      AA = (DNMAX-DNMIN)/DLOG((HIGH+CC)/(LOW+CC))
      BB = DNMIN - AA*DLOG(LOW+CC)

      DO IDN=INMIN,INMAX
         IF (IDN+CC .LE. 0.) THEN
            IF (LOW .LT. HIGH) THEN
               ODN = DNMIN
            ELSE
               ODN = DNMAX
            END IF
         ELSE
            R = AA*DLOG(IDN+CC)+BB
            LUT(IDN) = RTRUNC(R)
         END IF
      ENDDO
      WRITE (PRT,205) LOW,DNMIN,HIGH,DNMAX,CURVE
      NCHAR=80
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Truncate output DN to fit in DNMIN to DNMAX range.
C
      FUNCTION ITRUNC(ODN)
      INTEGER ODN

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      ITRUNC = ODN
      IF (ODN.LT.DNMIN) ITRUNC=DNMIN
      IF (ODN.GT.DNMAX) ITRUNC=DNMAX
      RETURN
      END      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Round-off DN to an integer and fit in DNMIN to DNMAX range.
C
      REAL FUNCTION RTRUNC(DN)
      REAL DN

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX
      INTEGER IDN

      IF (DN.GE.0.0) THEN
        IDN = DN + 0.5
      ELSE
        IDN = DN - 0.5
      ENDIF
      IF (IDN.GT.DNMAX) IDN=DNMAX
      IF (IDN.LT.DNMIN) IDN=DNMIN
      RTRUNC = IDN
      RETURN
      END      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Round-off DN to an integer and fit in DNMIN to DNMAX range.
C
      REAL*8 FUNCTION RTRUNC8(DN)
      REAL*8 DN

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX
      INTEGER IDN

      IF (DN.GE.0.0) THEN
        IDN = DN + 0.5
      ELSE
        IDN = DN - 0.5
      ENDIF
      IF (IDN.GT.DNMAX) IDN=DNMAX
      IF (IDN.LT.DNMIN) IDN=DNMIN
      RTRUNC8 = IDN
      RETURN
      END      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Initiale the look-up table.
c
      SUBROUTINE INIT_LUT(LUT)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER IDN,ITRUNC,BCKGND,ICOUNT,IDEF

      CALL XVPARM('BACKGND',bckgnd,icount,idef,1)
      IF (ICOUNT.EQ.1) THEN
         BCKGND = ITRUNC(BCKGND)
         DO IDN=INMIN,INMAX	!Initialize LUT to background if specified
            LUT(IDN) = BCKGND
         ENDDO
      ELSE			!Else, initialize LUT to identify transform
         DO IDN=INMIN,INMAX
            LUT(IDN) = ITRUNC(IDN)
         ENDDO
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check that only one stretch was specified
C
      SUBROUTINE CHK_NUM_STRETCH(NSTRETCH)
      INTEGER*4 NSTRETCH

      NSTRETCH = NSTRETCH + 1
      IF (NSTRETCH.LE.1) RETURN
      CALL XVMESSAGE('***More than one stretch specified',' ')
      CALL XVMESSAGE('***STRETCH task cancelled',' ')
      CALL ABEND
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hist_params.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return histogram stretch and print parameters (but not all)
C
      SUBROUTINE HIST_PARAMS(FORMAT,imode,hflag,ihist,ohist,
     &		icdf,ocdf,power,nstretch)
      IMPLICIT NONE
      INTEGER*4 IMODE,HFLAG,IHIST,OHIST,ICDF,OCDF,NSTRETCH
      REAL*4 POWER
      CHARACTER*8 FORMAT
      LOGICAL XVPTST

      INTEGER*4 IPARM,ICOUNT,IDEF

      HFLAG=0		!=1 if histogram is needed
      IHIST=0		!=1 to print input histogram
      OHIST=0		!=1 to print output histogram
      ICDF=0		!=1 to print input CDF
      OCDF=0		!=1 to print output CDF

      IF (XVPTST('IHIST')) THEN		!Print input histogram?
         IF (FORMAT.EQ.'BYTE') THEN
            IHIST = 1
            HFLAG = 1
         ELSE
            CALL XVMESSAGE('Histogram available only for byte data',
     .                     ' ')
         END IF
      END IF

      IF (XVPTST('OHIST')) THEN		!Print output histogram?
         IF (FORMAT.EQ.'BYTE') THEN
            OHIST=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('Histogram available only for byte data',
     .                     ' ')
         END IF
      END IF

      IF (XVPTST('ICDF')) THEN		!Print input CDF?
         IF (FORMAT.EQ.'BYTE') THEN
            ICDF=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('CDF available only for byte data',' ')
         END IF
      END IF

      IF (XVPTST('OCDF')) THEN
         IF(FORMAT.EQ.'BYTE') THEN
            OCDF=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('CDF AVAILABLE ONLY FOR BYTE DATA',' ')
         END IF
      END IF

      IF (XVPTST('SMOOTH')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=11
         HFLAG=1
      END IF

      IF (XVPTST('GAUSS')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=12
         HFLAG=1
      END IF

      IF (XVPTST('ELLIPSE')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=13
         HFLAG=1
      END IF

      CALL XVPARM('POWER',IPARM,ICOUNT,IDEF,1)
      IF (ICOUNT .EQ. 1) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=14
         HFLAG=1
         POWER=IPARM
      END IF

      CALL XVPARM('BIMODAL',IPARM,ICOUNT,IDEF,1)
      IF (ICOUNT.GT.0) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=15
         HFLAG=1
      END IF

      IF (XVPTST('PEAK')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=16
         HFLAG=1
      END IF

      IF (XVPTST('MEAN')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=17
         HFLAG=1
      END IF

      IF (XVPTST('ASTRETCH') .OR. IMODE.EQ.0) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=18
         HFLAG=1
      END IF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_histogram.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute or read histogram of input image
C
      SUBROUTINE GET_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 BUF(*)		!Image line buffer
      REAL*4 TMEAN,TSIGMA	!Mean and sigma
      INTEGER*4 NPTS		!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 NI

      CALL ZIA(hist,65536)	!Zero out histogram
      CALL XVPCNT('INP',NI)     !Number of input files.
      IF (NI.EQ.2) THEN		!Histogram supplied as second input
         CALL READ_HISTOGRAM(hist,tmean,tsigma,npts)
      ELSE			!Else, calculate histogram from input image
         CALL COMPUTE_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      END IF

      IF (NPTS.LT.1) THEN
         CALL XVMESSAGE('***Histogram is empty',' ')
         CALL ABEND
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read histogram from 2nd input file (must be IBIS format).
C
      SUBROUTINE READ_HISTOGRAM(hist,tmean,tsigma,npts)
      IMPLICIT NONE
      INTEGER*4 HIST(-32768:32767)
      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NPTS			!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,IDN,NLEV
      INTEGER*4 IUNIT2,IBIS,STAT,STATUS,NL2,NS2

      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL IBIS_FILE_OPEN(IUNIT2,IBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_GET(IBIS,'NR',NS2,1,1)
      CALL IBIS_FILE_GET(IBIS,'NC',NL2,1,1)        
      CALL XLGET(IUNIT2,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',
     &        nlev,status,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLGET(IUNIT2,'PROPERTY','MEAN_VALUE',tmean,status,
     &           'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLGET(IUNIT2,'PROPERTY','STANDARD_DEVIATION_VALUE',tsigma,
     &           status,'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL ZIA(HIST(INMIN),INMAX-INMIN+1)
      DO I=1,NL2
         CALL IBIS_COLUMN_READ(IBIS,HIST((I-1)*NS2-3),I,1,NS2,STATUS)
         IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      ENDDO
      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE(IUNIT2,STAT,' ')
      HIST(-3) = 0
      HIST(-2) = 0
      HIST(-1) = 0
      NPTS=0
      DO IDN=0,NLEV-1
         NPTS = NPTS + HIST(IDN)
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute histogram of input image
C
      SUBROUTINE COMPUTE_HISTOGRAM(FORMAT,hist,buf,tmean,tsigma,npts)
      IMPLICIT NONE
      LOGICAL*1 BUF(*)
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      REAL*4 TMEAN,TSIGMA
      INTEGER*4 NPTS		!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/C1/IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI
      INTEGER*4 IUNIT,OUNIT,SL,SS,NLO,NSO,NLI,NSI,SB,NBO,NBI

      INTEGER*4 LINC,ICOUNT,IDEF,STAT
      INTEGER*4 LINE,DCODE
      INTEGER*4 SLA,SSA,NLA,NSA,ELA
      INTEGER*4 IPARM(4)
      INTEGER*4 BAND
      CHARACTER*80 PRT
   60 FORMAT('(SL,SS,NL,NS) = (',I5,',',I5,',',I5,',',I5,')')

      CALL XVPARM('LINC',linc,icount,idef,1)	!Line subsampling
      CALL XVPARM('AREA',iparm,icount,idef,4)	!Image area
      IF (ICOUNT.EQ.4) THEN
         SLA=IPARM(1)
         SSA=IPARM(2)
         NLA=IPARM(3)
         NSA=IPARM(4)
         IF (SLA+NLA-1.GT.NLI .OR. SSA+NSA-1.GT.NSI) THEN
            CALL XVMESSAGE('Specified area exceeds size of input',' ')
            CALL XVMESSAGE('Area reduced',' ')
            IF (SLA+NLA-1.GT.NLI) NLA=NLI-SLA+1
            IF (SSA+NSA-1.GT.NSI) NSA=NSI-SSA+1
         END IF
         CALL XVMESSAGE('Histogram computed from sub-area',' ') 
         WRITE (PRT,60) SLA,SSA,NLA,NSA
         CALL XVMESSAGE(PRT,' ')
      ELSE
         SLA=SL
         SSA=SS
         NLA=NLO
         NSA=NSO
      END IF
      ELA=SLA+NLA-1
      CALL ZIA(hist,65536)	!Zero out the histogram
      DCODE = 1
      IF (FORMAT.EQ.'HALF') DCODE=2

      DO BAND=SB,SB+NBO-1
        DO LINE=SLA,ELA,LINC
         CALL XVREAD(IUNIT,buf,stat,'LINE',LINE,
     &			'SAMP',SSA,'NSAMPS',NSA,'BAND',BAND,' ')
         CALL HSUB(DCODE,NSA,buf,hist(inmin),INMIN,INMAX)
        ENDDO
      ENDDO
      CALL STATI(HIST,tmean,tsigma,npts)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create exclude_histogram.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Exclude DN ranges from histogram, as specified.
C
      SUBROUTINE EXCLUDE_HISTOGRAM(FORMAT,hist,npts)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 NPTS			!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,IDN,NPAIR,IXL,IXH,ICOUNT,IDEF
      REAL*4 TMEAN,TSIGMA,CUT

      REAL*4 RPARM(1000)
      INTEGER*4 IPARM(1000)
      EQUIVALENCE (RPARM,IPARM)

      CHARACTER*80 PRT
      LOGICAL*4 XVPTST
   20 FORMAT('DN values',I7,' thru',I7,' excluded')
   25 FORMAT('DN values',I7,' and',I7,' excluded')
   30 FORMAT(I7,' Pixels Outside Range',I7,' to',I7)
   40 FORMAT('Histogram after exclusion: Mean=',F11.4,' Sigma=',F11.4)
   50 FORMAT('Histogram after cutting: Mean=',F11.4,' Sigma=',F11.4)

C     ....Default is to exclude 0 and the maximum dn.
      IF (.NOT. XVPTST('INCLUDE'))  THEN
         WRITE (PRT,25) DNMIN,DNMAX
         CALL XVMESSAGE(PRT,' ')
         HIST(DNMIN) = 0
         HIST(DNMAX) = 0
      ENDIF        

      CALL XVPARM('REXCLUDE',IPARM,ICOUNT,IDEF,200)
      IF (ICOUNT.GT.0) THEN
         NPAIR=ICOUNT/2
         IF (2*NPAIR.NE.ICOUNT) GOTO 900
         CALL XVMESSAGE(' ',' ')
         DO I=1,ICOUNT,2
            IXL=IPARM(I)
            IXH=IPARM(I+1)
            IF (IXL.LT.INMIN) IXL=INMIN
            IF (IXH.GT.INMAX) IXH=INMAX
            IF (IXL.GT.IXH) GOTO 902
            WRITE (PRT,20) IXL,IXH
            CALL XVMESSAGE(PRT,' ')
            CALL ZIA(hist(ixl),IXH-IXL+1)	!zero out exluded entries
         END DO
      END IF

      CALL XVPARM('IEXCLUDE',IPARM,ICOUNT,IDEF,100)
      IF (ICOUNT.GT.0) THEN
         CALL XVMESSAGE(' ',' ')
         DO I=1,ICOUNT
            IDN=IPARM(I)
            IF (IDN.LT.INMIN) IDN=INMIN
            IF (IDN.GT.INMAX) IDN=INMAX
            HIST(IDN)=0				!zero out excluded entries
         END DO
      END IF

C     ....Calculate new mean and standard deviation
      CALL STATI(HIST,tmean,tsigma,npts)
      IF (NPTS.LE.0) GOTO 904
      CALL XVMESSAGE(' ',' ')
      WRITE (PRT,40) TMEAN,TSIGMA
      CALL XVMESSAGE(PRT,' ')

C     ....Exclude DNs with less than a specified percent of max DN level
      CALL XVPARM('CUT',RPARM,ICOUNT,IDEF,1)
      IF (ICOUNT.GT.0) THEN
         CUT=RPARM(1)
         IF (CUT.LT.0.0 .OR. CUT.GT.100.0) THEN
            CALL XVMESSAGE('INVALID CUT VALUE - RESET TO 5.0',' ')
            CUT=5.0
         END IF
         CALL FCLIP(HIST,INMIN,INMAX,CUT)
         CALL STATI(HIST,tmean,tsigma,npts)
         IF (NPTS.LE.0) GOTO 908
         CALL XVMESSAGE(' ',' ')
         WRITE (PRT,40) TMEAN,TSIGMA
         CALL XVMESSAGE(PRT,' ')
      END IF

C        Print number OF pixels outside expected range.
      RETURN

  900 CALL XVMESSAGE('***Invalid count for parameter REXCLUDE',' ')
      CALL ABEND
  902 CALL XVMESSAGE('***Error in rexclude range ***',' ')
      CALL ABEND
  904 CALL XVMESSAGE('***No pixels remain after exclusion',' ')
      CALL ABEND
  908 CALL XVMESSAGE('***No pixels remain after cutting',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Exclude all histogram entries with a frequency less than FCLIPC percent
C from the maximum frequency.
C
      SUBROUTINE FCLIP(HIST,INMIN,INMAX,FCLIPC)
      INTEGER*4 INMIN,INMAX
      INTEGER*4 HIST(-32768:32767)
      REAL      RTOL

C     ....Find max frequency
      MAX=0
      DO I=INMIN,INMAX
         IF (HIST(I).GT.MAX) MAX=HIST(I)
      END DO

C     ....Zero out any frequencies less than FCLIPC percent of max.
      RTOL = FLOAT(MAX)*(FCLIPC/100.0)
      DO I=INMIN,INMAX
         IF (HIST(I).LT.RTOL) HIST(I)=0
      END DO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create histogram_stretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute look-up table for histogram stretches
C
      SUBROUTINE HISTOGRAM_STRETCH(FORMAT,HIST,NPTS,
     &		imode,lut,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 FORMAT        !'BYTE' or 'HALF'
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 NPTS		!Number of pixels in histogram
      INTEGER*4 IMODE		!Stretch mode
      INTEGER*2 LUT(-32768:32767)!Computed stretch table
      INTEGER*4 NCHAR           !Number of characters in PRT buffer
      CHARACTER*80 PRT          !String to be inserted in image label

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

      INTEGER*4 NMIN,NMAX,ICOUNT,IDEF
      EXTERNAL NGAUSS,NRAMP,NELLIP,NPOWER
C
  225 FORMAT ('Gaussian Stretch: GSIGMA =',F7.2,'  GMEAN =',F9.2)
  245 FORMAT ('Power Law Stretch: POWER =',F8.2)
  285 FORMAT ('AUTO-STRETCH:',I7,' to',I7,' and',I7,' to',I7)

      IF (IMODE.EQ.11) THEN
         CALL XVMESSAGE('*** SMOOTH OPTION ***',' ')
         CALL HMOD(HIST,LUT,NRAMP,NPTS)
         PRT = 'Ramp CDF Stretch'
         NCHAR = 17
         CALL XVMESSAGE(PRT,' ')
cccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.12) THEN
         CALL XVMESSAGE('*** GAUSS OPTION ***',' ')
         CALL XVPARM('GSIGMA',gsigma,icount,idef,1)
         CALL XVPARM('GMEAN',GMEAN,icount,idef,1)
         IF (ICOUNT.EQ.0) GMEAN=(DNMIN+DNMAX)/2.0
         CALL HMOD(HIST,LUT,NGAUSS,NPTS)
         WRITE (PRT,225) GSIGMA,GMEAN
         CALL XVMESSAGE(PRT,' ')
         NCHAR = 52
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF
 
      IF (IMODE.EQ.13) THEN
         CALL XVMESSAGE('*** ELLIPSE OPTION ***',' ')
         CALL HMOD(HIST,LUT,NELLIP,NPTS)
         PRT = 'Elliptical Stretch'
         NCHAR = 19
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.14) THEN
         CALL XVMESSAGE('*** POWER LAW OPTION ***',' ')
         CALL HMOD(HIST,LUT,NPOWER,NPTS)
         WRITE (PRT,245) POWER
         NCHAR = 35
         CALL XVMESSAGE(PRT,' ')
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.15) THEN
         CALL BIMODAL(FORMAT,HIST,lut,prt,nchar,imode)
         IF (IMODE.EQ.15) RETURN
      ENDIF

      IF (IMODE.EQ.16 .OR. IMODE.EQ.17) THEN
         CALL HPEAK(IMODE,HIST,NPTS,lut,prt,nchar)
         RETURN
      ENDIF

      IF (IMODE.EQ.18) THEN
         CALL XVMESSAGE('*** AUTO-STRETCH OPTION ***',' ')
         CALL ASTRETCH(HIST,NPTS,lut,nmin,nmax)
         WRITE (PRT,285) NMIN,DNMIN,NMAX,DNMAX
         NCHAR=52
         CALL XVMESSAGE(PRT,' ')
         RETURN
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate a stretch table (LUT) to produce a histogram with the specified
C distribution.  The function pointer LCDF is used to call a routine which
C computes CDF(IDN).  I.e., LCDF points to one of the following functions:
C NRAMP, NGAUSS, NELLPI, or NPOWER.
C
      SUBROUTINE HMOD(HIST,lut,LCDF,NPTS)
      EXTERNAL  LCDF			!Function pointer
      INTEGER*4 HIST(-32768:32767)	!Input histogram
      INTEGER*2 LUT(-32768:32767)	!Output look-up table
      INTEGER*4 NPTS			!Number of pixels in HIST

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ODN,CDF,HCDF,I

      HCDF = 0			!CDF computed by integrating over histogram
      IDN = INMIN

C     ....Compute LUT so that CDF of histogram matches CDF computed by function
      DO 30 ODN=DNMIN,DNMAX
      CDF = LCDF(ODN,NPTS,DNMIN,DNMAX)		!Call function to compute CDF
   20 IF (HCDF+HIST(IDN).GT.CDF) GOTO 30	!Compare the two CDFs
      HCDF = HCDF + HIST(IDN)			!Update histogram computed CDF
      LUT(IDN) = ODN
      IDN = IDN + 1
      IF (IDN.GT.INMAX) RETURN
      GOTO 20
   30 CONTINUE

      DO I=IDN,INMAX
         LUT(I) = DNMAX
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for uniform-distribution histogram
C
      FUNCTION NRAMP(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 DNMIN,DNMAX
      SAVE		!Save SLOPE for subsequent calls
      REAL*4 SLOPE

C     ....On first call, compute scale for normalizing CDF
      IF (ODN.LE.DNMIN) SLOPE=FLOAT(NPTS)/FLOAT(DNMAX-DNMIN)
      NRAMP = FLOAT(ODN-DNMIN)*SLOPE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for gaussian-distribution histogram
C
      FUNCTION NGAUSS(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 DNMIN,DNMAX
      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

      SAVE	!Save variables for subsequent calls
      REAL*4 ANORM,SUM
      REAL*4 GMEAN2,ST,A,B

C     ....On first call, compute normalization factor ANORM
      IF (ODN.GT.DNMIN) GOTO 100
      GMEAN2 = 2.0*GMEAN
      SIGMA = (DNMAX-DNMIN+1)/(2*GSIGMA)
      A = 1./(SQRT(2.*3.14159)*SIGMA)
      SIGSQ = SIGMA*SIGMA
      B = -1./(2.*SIGSQ)
      SS = FLOAT(DNMIN)**2 - 2.*DNMIN*GMEAN + GMEAN**2
      ST = SS
C     ....Compute normalization constant ANORM
      SUM = 0.
      DO I=DNMIN,DNMAX
         SUM = SUM + A*EXP(B*SS)
         SS = SS - GMEAN2 +2*I + 1.
      ENDDO
      ANORM = A*FLOAT(NPTS)/SUM

      SUM = 0.
C     ....Accumulate CDF of normalized gaussian
  100 SUM = SUM + ANORM*EXP(B*ST)
      ST = ST - GMEAN2 + 2*ODN + 1.
      NGAUSS = NINT(SUM)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for elliptic-distribution histogram
C
      FUNCTION NELLIP(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 NPTS	!Total number of pixels in histogram
      INTEGER*4 DNMIN,DNMAX	!DN range of histogram

      SAVE	!Save variables for subsequent calls

C     ....On first call, compute ellipse parameters
      IF (ODN.GT.DNMIN) GOTO 100
      B = FLOAT(DNMAX-DNMIN+1)/2.
      BSQ = B*B
      NOFFST= NINT(B)
      SUM=0.

C     ....Compute discrete ellipse area
      DO I=DNMIN,DNMAX
         X = I - NOFFST
         BSQXSQ = BSQ - X*X
         IF (BSQXSQ.LT.0.) BSQXSQ=0.
         SUM = SUM + SQRT(BSQXSQ)
      END DO

C     ....Calculate normalization factor
      ANORM=FLOAT(NPTS)/SUM
      SUM=0.

C     ....Accumulate CDF of normalized ellipse
  100 X = ODN - NOFFST
      BSQXSQ = BSQ - X*X
      IF (BSQXSQ.LT.0.) BSQXSQ=0.
      Y = ANORM*SQRT(BSQXSQ)
      SUM = SUM + Y
      NELLIP = NINT(SUM)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for power-distribution histogram
C The power function is of the form:
C      y = 1 -ABS(x)**POWER  on interval (-1,1)
C but is scaled so that the function covers the interval (dnmin,dnmax) and
C the max occurs and mid-range.
C
      FUNCTION NPOWER(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 NPTS	!Total number of pixels in histogram
      INTEGER*4 DNMIN,DNMAX	!DN range of histogram

      SAVE	!Save variables for subsequent calls

      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

C     ....On first call, compute function parameters
      IF (ODN.GT.DNMIN) GOTO 100
      HLDN = DNMAX - DNMIN
      SUM = 0.
      DO I=DNMIN,DNMAX
         X = I - DNMIN
         C = ABS(2.*(X/HLDN)-1.)
         C = C**POWER
         SUM = SUM + (1.-C)
      END DO

C     ....Calculate normalization factor
      ANORM = FLOAT(NPTS)/SUM
      SUM = 0.

C     ....Accumulate cdf of normalized power law function
  100 X = ODN - DNMIN
      C = ABS(2.*(X/HLDN)-1.)
      Y = ANORM*(1.-C**POWER)
      SUM = SUM + Y
      NPOWER = NINT(SUM)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create bimodal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a piecewise linear stretch table for a bimodal histogram.
C
      SUBROUTINE BIMODAL(FORMAT,HIST,lut,prt,nchar,imode)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 NCHAR,IMODE
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IND

      CALL TWOPK(FORMAT,HIST(INMIN),lut,ind)
      IF (IND.EQ.0) THEN
         PRT = 'BIMODAL STRETCH'
         NCHAR = 16
         RETURN
      ELSE
         CALL XVMESSAGE('Bimodal option unsuccessful',' ')
         CALL XVMESSAGE('Reset to default astretch option',' ')
         IMODE=18	!Revert to ASTRETCH mode
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute stretch table for bimodal histogram
C
      SUBROUTINE TWOPK(FORMAT,HIST,lut,ind)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(65536)
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IND		!0=OK

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/CW/HISTW
      INTEGER*4 HISTW(65536,2)

      INTEGER*4 ICOUNT,IDEF,IPARM(6),LOC(3)
      INTEGER*4 I,J,K,NLEV,NLEV2,NSW,NSW2,NTOL
      INTEGER*4 SUM,LSUM,HSUM,NPEAK,LOPK,HIPK,IMIN
      INTEGER*4 IDUP,IFREQ,MINFREQ
      INTEGER*4 LPT1,LPT2,HPT1,HPT2
      REAL*4 L1PERC,L2PERC,H1PERC,H2PERC,FF,RPARM(6)

      INTEGER*4 T(0:5)
      INTEGER*4 ITER/5/		!Max number of iterations
      REAL*4 FTOL/10.0/		!Min freq percent tol for a peak
      CHARACTER*80 MSG8

      IND=-1
      NLEV = INMAX - INMIN + 1      
      SUM = 0
C     ....Copy histogram
      DO I=1,NLEV
         HISTW(I,1) = HIST(I)
         SUM = SUM + HIST(I)
      ENDDO
      NTOL = 0.01*FLOAT(SUM)/FLOAT(NLEV)

      NSW = 0.03*NLEV
      NSW = (NSW/2)*2 + 1   !Force filter NSW to be an odd number
      NSW2 = NSW/2
      WRITE (MSG8,1000) NSW
 1000 FORMAT('Filter window NSW=',I4)
      CALL XVMESSAGE(MSG8,' ')

      CALL XVPARM('LPERCENT',RPARM,ICOUNT,IDEF,1)
      L1PERC = RPARM(1)
      H2PERC = RPARM(1)
      CALL XVPARM('HPERCENT',RPARM,ICOUNT,IDEF,1)
      L2PERC = RPARM(1)
      H1PERC = RPARM(1)

      J = 1
C     ....Filter histogram, take derivative, and find candidate peaks
      DO 20 I=1,ITER
      CALL UNIFLT(4,NLEV,HISTW(1,J),histw(1,3-j),NSW)
      CALL HMAX(NLEV,NSW,NTOL,HISTW(1,3-J),histw(1,j),loc,npeak)
      IF (NPEAK.EQ.2) GOTO 30
      J = 3 - J
   20 CONTINUE
      IND=-1			!Unable to find two peaks
      RETURN

C     ....Here if two peaks located
   30 J = 3 - J
      LOPK = MIN0(LOC(1),LOC(2))	!Left peak
      HIPK = MAX0(LOC(1),LOC(2))	!Right peak

C     ....Find the minimum between the peaks.
      NLEV2 = HIPK - LOPK + 1
      DO 80 I=1,ITER
      CALL UNIFLT(4,NLEV2,HISTW(LOPK,J),histw(lopk,3-j),NSW)
      J = 3 - J
      IDUP = 0
      MINFREQ=1000000
      DO K=LOPK,HIPK
         IFREQ = HISTW(K,J)
         IF (IFREQ.EQ.0) GOTO 80
         IF (IFREQ.EQ.MINFREQ) IDUP=1
         IF (IFREQ.LT.MINFREQ) THEN
            IDUP = 0
            MINFREQ = IFREQ
            IMIN = K
         ENDIF
      ENDDO
      IF (IDUP.EQ.0) GOTO 90
   80 CONTINUE
      IND=-2				!Cannot find unique minumum
      RETURN

C     ....Calc area around low and high peaks and chk that each is .gt. ftol
   90 LSUM = 0
      DO I=1,IMIN
         LSUM = LSUM + HIST(I)
      END DO

      HSUM = 0
      DO I=IMIN+1,NLEV
         HSUM = HSUM + HIST(I)
      END DO

      SUM = LSUM + HSUM
      IND=-3
      FF = 100.*FLOAT(LSUM)/FLOAT(SUM)
      IF (FF.GT.100.-FTOL .OR. FF.LT.FTOL) RETURN

C     ....Compute stretch limits around low peak
      CALL ASTRCH(HIST,lpt1,lpt2,L1PERC,H1PERC,IMIN)
      IND=-4
      IF (LPT1.LT.0) LPT1=0
      IF (LPT2.GT.IMIN-1) LPT2=IMIN-1
      IF (LPT1.GE.LPT2) RETURN

C     ....Compute stretch limits around high peak
      CALL ASTRCH(HIST(IMIN+1),hpt1,hpt2,L2PERC,H2PERC,NLEV-IMIN)
      HPT1 = HPT1 + IMIN
      HPT2 = HPT2 + IMIN
      IND=-5
      IF (HPT1.LT.IMIN) HPT1=IMIN
      IF (HPT2.GT.NLEV-1) HPT2=NLEV-1
      IF (HPT1.GE.HPT2) RETURN

C     ....Compute piece-wise linear stretch
      CALL XVMESSAGE('*** BIMODAL OPTIION ***',' ')
      LPT1 = LPT1 + INMIN
      LPT2 = LPT2 + INMIN
      HPT1 = HPT1 + INMIN
      HPT2 = HPT2 + INMIN
      T(0) = DNMIN
      T(1) = DNMIN
      T(2) = (DNMIN+DNMAX)/2
      T(3) = T(2) + 1
      T(4) = DNMAX
      T(5) = DNMAX
      CALL XVPARM('BIMODAL',iparm,icount,idef,1)
      DO I=1,ICOUNT
         T(I-1) = IPARM(I)
      ENDDO
      CALL COMPUTE_LST(INMIN,LPT1,T(0),T(1),lut)
      CALL COMPUTE_LST(LPT1,LPT2,T(1),T(2),lut)
      CALL COMPUTE_LST(LPT2,HPT1,T(2),T(3),lut)
      CALL COMPUTE_LST(HPT1,HPT2,T(3),T(4),lut)
      CALL COMPUTE_LST(HPT2,INMAX,T(4),T(5),lut)
cccc      IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Find all local maxima in histogram and return their locations.
C
      SUBROUTINE HMAX(NLEV,NSW,NTOL,HIST,histo,loc,npeak)
      IMPLICIT NONE
      INTEGER*4 NLEV		!# of DN levels in histogram
      INTEGER*4 NSW		!Width of pre-filter width
      INTEGER*4 NTOL		!Determines whether a peak is significant
      INTEGER*4 HIST(NLEV)	!Input histogram
      INTEGER*4 HISTO(NLEV)	!Ouput filtered histogram
      INTEGER*4 LOC(10)		!DN location of maxima
      INTEGER*4 NPEAK		!# of local maxima in histogram

      INTEGER I,FIRST,LAST

C     ....Compute derivative of histogram
      HISTO(1) = 0
      DO I=2,NLEV
         HISTO(I)=HIST(I)-HIST(I-1)
      END DO

      FIRST = 1    + MAX0(NSW/2,1)
      LAST  = NLEV - MAX0(NSW/2,1)
      NPEAK = 0
C     ....Find all local maxima larger than NTOL
      DO 30 I=FIRST,LAST
      IF (.NOT.(HISTO(I-1).GT.0.AND.HISTO(I+1).LT.0)) GOTO 30
      IF (HISTO(I-1)-HISTO(I+1).LE.NTOL) GOTO 30
      IF (NPEAK.GT.0) THEN
         IF (LOC(NPEAK).EQ.I-1) GOTO 30	!Reject if max is adjacent to last max
      ENDIF
      NPEAK = NPEAK + 1		!New relative max found
      IF (NPEAK.GT.2) RETURN
      LOC(NPEAK) = I
   30 CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute linear stretch table so that IMIN maps to OMIN and IMAX maps to OMAX
C
      SUBROUTINE COMPUTE_LST(IMIN,IMAX,OMIN,OMAX,lut)
      IMPLICIT NONE
      INTEGER*4 IMIN,IMAX,OMIN,OMAX
      INTEGER*2 LUT(-32768:32767)

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ODN
      REAL*4 A,B

      A = FLOAT(OMAX-OMIN)/FLOAT(IMAX-IMIN)
      B = -A*IMIN + OMIN

      DO IDN=IMIN,IMAX
         ODN = NINT(A*IDN + B)
         IF (ODN.LT.DNMIN) ODN=DNMIN
         IF (ODN.GT.DNMAX) ODN=DNMAX
         LUT(IDN) = ODN
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hpeak.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PEAK and MEAN stretch options
C
      SUBROUTINE HPEAK(IMODE,HIST,NPTS,lut,prt,nchar)
      IMPLICIT NONE
      INTEGER*4 IMODE,NPTS,NCHAR
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 LUT(-32768:32767)
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IMEAN,IDN
      INTEGER*4 I1,I2,NPKFQ,NPKDN,NRANGE,DNMIDDLE
      INTEGER*4 NMIN,NMAX,NLEV
      REAL*4 A,B,XFACTR,AMEAN,RTRUNC
      CHARACTER*14 PRT3
  275 FORMAT (A14,I7,' to',I7,' and',I7,' to',I7)

      NLEV = INMAX - INMIN + 1
      IF (IMODE.EQ.16) THEN
         CALL XVMESSAGE('*** PEAK OPTION ***',' ')
         NPKFQ=0
         NPKDN=0
         I1 = INMIN + 1
         I2 = INMAX - 1
         DO IDN=I1,I2			!Find IDN with max frequency
            IF (NPKFQ.LT.HIST(IDN)) THEN
               NPKFQ = HIST(IDN)
               NPKDN = IDN
            END IF
         END DO
         PRT3 = 'Peak Stretch:'
      ENDIF

      IF (IMODE.EQ.17) THEN
         CALL XVMESSAGE('*** MEAN OPTION ***',' ')
         AMEAN=0.0
         DO IDN=INMIN,INMAX		!Compute mean DN
            AMEAN = AMEAN + IDN*HIST(IDN)
         ENDDO
         AMEAN = AMEAN/FLOAT(NPTS)
         IMEAN = NINT(AMEAN)
         NPKFQ = HIST(IMEAN)
         NPKDN = IMEAN
         PRT3 = 'Mean Stretch:'
      ENDIF

      CALL XVPARM('RANGE',nrange,icount,idef,1)
      IF (ICOUNT.EQ.1) THEN
         NMIN = NPKDN - NRANGE
         NMAX = NPKDN + NRANGE
      ELSE
         CALL ASTRETCH_LIMITS(HIST,NPTS,nmin,nmax)
      ENDIF

      DNMIDDLE = 0.5*(DNMIN+DNMAX)
      CALL XVPARM('FACTOR',xfactr,icount,idef,1)

C........Compute lower part of stretch table
      A = FLOAT(DNMIDDLE-DNMIN)/FLOAT(NPKDN-NMIN)
      IF (ICOUNT.EQ.1) A=XFACTR
      B = -A*NMIN + DNMIN
      DO IDN=INMIN,NPKDN
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

C........Compute upper part of stretch table
      A = FLOAT(DNMAX-DNMIDDLE)/FLOAT(NMAX-NPKDN)
      IF (ICOUNT.EQ.1) A=XFACTR
      B = -A*NPKDN + DNMIDDLE
      DO IDN=NPKDN,INMAX
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

      WRITE (PRT,275) PRT3,NMIN,DNMIN,NMAX,DNMAX
      NCHAR=52
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create astretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute LUT for automatic linear stretch.
C
      SUBROUTINE ASTRETCH(HIST,NPTS,lut,nmin,nmax)
      IMPLICIT NONE
      INTEGER*4 NPTS		!number of pixels in histogram
      INTEGER*4 NMIN,NMAX	!linear stretch limits (returned)
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 LUT(-32768:32767)

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      REAL*4 A,B,RTRUNC
      INTEGER*4 IDN

      CALL ASTRETCH_LIMITS(HIST,NPTS,nmin,nmax)
C     ....Compute stretch table
      A = FLOAT(DNMAX-DNMIN)/FLOAT(NMAX-NMIN)
      B = -A*NMIN+DNMIN
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute linear stretch limits from histogram.
C
      SUBROUTINE ASTRETCH_LIMITS(HIST,NPTS,nmin,nmax)
      IMPLICIT NONE
      INTEGER*4 NPTS		!number of pixels in histogram
      INTEGER*4 NMIN,NMAX	!linear stretch limits (returned)
      INTEGER*4 HIST(-32768:32767)

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      REAL*4 RPARM(2),LPERC,HPERC
      INTEGER*4 ICOUNT,IDEF,NLEV
      CHARACTER*80 PRT
  101 FORMAT('Percent saturation at low end=',F6.2,' at high end=',F6.2)

      CALL XVPARM('PERCENT',RPARM,ICOUNT,IDEF,1)
      LPERC = RPARM(1)/2.
      HPERC = LPERC
      CALL XVPARM('LPERCENT',RPARM,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) LPERC=RPARM(1)
      CALL XVPARM('HPERCENT',RPARM,ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) HPERC=RPARM(1)
      WRITE (PRT,101) LPERC,HPERC
      CALL XVMESSAGE(PRT,' ')

      NLEV = INMAX - INMIN + 1
      CALL ASTRCH(HIST(INMIN),nmin,nmax,LPERC,HPERC,NLEV)
      NMIN = NMIN + INMIN
      NMAX = NMAX + INMIN
      IF (NMIN.EQ.NMAX) NMAX= NMAX+1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create post_stretch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Add post-stretch to look-up table
C
      SUBROUTINE POST_STRETCH(lut,nchar2,prt2)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 NCHAR2
      CHARACTER*80 PRT2

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      REAL*4 A,B,RTRUNC
      INTEGER*4 IDN,LVAL,HVAL,IPARM(2),ICOUNT,IDEF
  810 FORMAT ('Post-stretch:',I7,' to',I7,' and',I7,' to',I7)

      CALL XVPARM('POST',iparm,icount,idef,2)
      IF (ICOUNT.LT.2) RETURN
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('*** POST-STRETCH OPTION ***',' ')
      LVAL = IPARM(1)
      HVAL = IPARM(2)
      IF (LVAL.LT.DNMIN .OR. HVAL.LT.DNMIN) THEN
         CALL XVMESSAGE('Post-stretch parameter less than dnmin',' ')
         CALL XVMESSAGE('Parameter reset to dnmin',' ')
         IF (LVAL.LT.DNMIN) LVAL=DNMIN
         IF (HVAL.LT.DNMIN) HVAL=DNMIN
      ENDIF
      IF (LVAL.GT.DNMAX .OR. HVAL.GT.DNMAX) THEN
         CALL XVMESSAGE('Post-stretch parameter exceeds dnmax',' ')
         CALL XVMESSAGE('Parameter reset to dnmax',' ')
         IF (LVAL.GT.DNMAX) LVAL=DNMAX
         IF (HVAL.GT.DNMAX) HVAL=DNMAX
      ENDIF
      IF (LVAL.EQ.HVAL) THEN
         CALL XVMESSAGE('***Err - post values specified are equal',' ')
         CALL ABEND
      ENDIF

C     ....Compute composite lookup table
      A = FLOAT(HVAL-LVAL)/FLOAT(DNMAX-DNMIN)
      B = -A*DNMIN + LVAL
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(A*LUT(IDN) + B)
      ENDDO
      WRITE (PRT2,810) DNMIN,LVAL,DNMAX,HVAL
      NCHAR2 = 52
      CALL XVMESSAGE(PRT2,' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_histogram.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print input histogram and CDF, as specified
C
      SUBROUTINE PRINT_HISTOGRAM(HIST,IHIST,ICDF,INMAX,
     +	TMEAN,TSIGMA,NPTS)
      IMPLICIT NONE
      INTEGER*4 HIST(-32768:32767),IHIST,ICDF,INMAX,NPTS
      REAL TMEAN,TSIGMA

      INTEGER*4 ICOUNT,IDEF,NSPIKE
      CHARACTER*80 MSG
   40 FORMAT('Histogram of input image: Mean  =',F11.4,
     &          ' Sigma =',F11.4)

      IF (IHIST.EQ.1) THEN		!Print input histogram
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('INPUT HISTOGRAM',' ')
         CALL XVPARM('SPIKES',nspike,icount,idef,1)
         CALL PHIST(HIST(0),NPTS,0,INMAX,NSPIKE)
         WRITE(MSG,40) TMEAN,TSIGMA
         CALL XVMESSAGE(MSG,' ')
      END IF

      IF (ICDF.EQ.1) THEN		!Print input CDF
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('CUMULATIVE DISTRIBUTION FUNCTION',' ')  
         CALL CDF(HIST(0),NPTS)
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print output histogram and CDF (byte only), as specified.
C
      SUBROUTINE PRINT_OHISTOGRAM(HIST,LUT,OHIST,OCDF,NPTS)
      IMPLICIT NONE
      INTEGER*4 HIST(0:255)
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 OHIST,OCDF,NPTS
      REAL*4 TMEAN,TSIGMA

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 HIST2(0:255)
      INTEGER*4 ICOUNT,IDEF,NSPIKE,IDN,ODN
      CHARACTER*80 MSG
   40 FORMAT('Histogram of output image: Mean=',F11.4,' Sigma=',F11.4)


C     ....Compute output histogram if required (byte case only)
      CALL ZIA(HIST2,256)
      DO IDN=INMIN,INMAX
         ODN = LUT(IDN)
         HIST2(ODN) = HIST2(ODN) + HIST(IDN)
      ENDDO

      IF (OHIST.EQ.1) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('OUTPUT HISTOGRAM',' ')
         CALL XVPARM('SPIKES',nspike,icount,idef,1)
         CALL PHIST(HIST2,NPTS,0,DNMAX,NSPIKE)
         CALL STATIB(HIST2,tmean,tsigma,npts)
         WRITE(MSG,40) TMEAN,TSIGMA
         CALL XVMESSAGE(MSG,' ')
      ENDIF
      IF (OCDF.EQ.1) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('OUTPUT CUMULATIVE DISTRIBUTION FUNCTION',' ')  
         CALL CDF(HIST2,NPTS)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print a variable width histogram with spikes option
C  FREQ   - INTEGER ARRAY CONTAINING HISTOGRAM
C  NUM    - INTEGER SUM OF FREQUENCIES OF ALL DN LEVELS
C  NSPIKE - INTEGER NUMBER OF SPIKES TO SKIP IN NORMALIZATION
C  LOWDN  - INTEGER VALUE OF DN LEVEL TO ASSIGN TO HIST(1)
C  HIGHDN - INTEGER VALUE OF THE UPPER DN LEVEL OF THE HISTOGRAM
C
      SUBROUTINE PHIST(FREQ,NUM,LOWDN,HIGHDN,NSPIKE)
      INTEGER FREQ(*),HIGHDN,MAXT,SPIKE
      CHARACTER*132 LISTO
      CHARACTER*23 NUMS
      CHARACTER*103 ASTER/'**************************************************************************************************** *'/
      CHARACTER*103  PLUS/'+         +         +         +         +         +         +         +         +         +        +  '/
      DATA MAXT/2147483647/    ! 2147483647 = z'7FFFFFFF'
  500 FORMAT (I4,I10,F8.3)

      NLEV=HIGHDN-LOWDN+1
      IF(NLEV.LT.1 .OR. NUM.LT.1) THEN
         CALL XVMESSAGE('ERROR IN PHIST ARGUMENT LIST',' ')
         CALL XVMESSAGE('NO HISTOGRAM PRINTED',' ')
         RETURN
      END IF
      SPIKE=NSPIKE+1
      IF(SPIKE.GE.NLEV.OR.SPIKE.LT.1) SPIKE=1
      CALL XVMESSAGE(
     &'GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100'
     & ,' ')
      CALL XVMESSAGE(' ',' ')
      MAXS=MAXT
      DO J=1,SPIKE
         MAX=0
         DO I=1,NLEV
            IF(FREQ(I).GT.MAX.AND.FREQ(I).LT.MAXS) MAX=FREQ(I)
         ENDDO
         MAXS=MAX
      ENDDO
      IF(MAX.LT.1) MAX=1
      DO I=1,NLEV
         IFREQ=FREQ(I)
         IF(I.GT.1 .AND. IFREQ.EQ.0) THEN
            IF(FREQ(I-1).NE.0) CALL XVMESSAGE(' ',' ')
         ELSE
            PERCEN=(100.0*IFREQ)/NUM
            MTEMP=((100*IFREQ)/MAX)+1
            IF(MTEMP.GT.101) MTEMP=103
            WRITE (NUMS,500) LOWDN+I-1, IFREQ, PERCEN
            LISTO = NUMS//'    '//ASTER(1:MTEMP)//PLUS(MTEMP+1:105)
            CALL XVMESSAGE(LISTO,' ')
         ENDIF
      END DO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute and print the CDF from the histogram (byte format only).
C
      SUBROUTINE CDF(HIST,NPTS)
      INTEGER*4 HIST(0:255),NPTS

      INTEGER*4 CDFUN(0:255),I,IMAX

      CDFUN(0) = HIST(0)
      IMAX = 0
      DO I=1,255
         CDFUN(I) = CDFUN(I-1) + HIST(I)
         IF (HIST(I).GT.0) IMAX=I
      END DO
      CALL PHIST(CDFUN,NPTS,0,IMAX,0)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create stati.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate mean and sigma from input histogram
C
      SUBROUTINE STATI(HIST,mean,sigma,npts)
      IMPLICIT NONE
      INTEGER*4 HIST(-32768:32767)
      REAL*4 MEAN,SIGMA		!Mean and sigma (returned)
      INTEGER*4 NPTS		!Number of pixels in histogram (returned)      

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,NPIX
      REAL*8 SUM,SUMSQ,DMEAN,DN

      SUMSQ=0.0D0
      SUM=0.0D0
      NPTS=0

      DO IDN=INMIN,INMAX
         NPIX = HIST(IDN)
	 DN = IDN
         SUM = SUM + NPIX*DN
         SUMSQ = SUMSQ + NPIX*DN*DN
         NPTS = NPTS + NPIX
      END DO

      IF (NPTS.GT.0) THEN
         DMEAN = SUM/NPTS
         MEAN = DMEAN
         SIGMA=DSQRT(DABS(SUMSQ/NPTS-DMEAN*DMEAN))
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate mean and sigma for byte histogram
C
      SUBROUTINE STATIB(HIST,mean,sigma,npts)
      IMPLICIT NONE
      INTEGER*4 HIST(0:255)
      REAL*4 MEAN,SIGMA		!Mean and sigma (returned)
      INTEGER*4 NPTS		!Number of pixels in histogram (returned)      

      INTEGER*4 IDN,NPIX
      REAL*8 SUM,SUMSQ,DMEAN,DN

      SUMSQ=0.0D0
      SUM=0.0D0
      NPTS=0

      DO IDN=0,255
         NPIX = HIST(IDN)
	 DN = IDN
         SUM = SUM + NPIX*DN
         SUMSQ = SUMSQ + NPIX*DN*DN
         NPTS = NPTS + NPIX
      END DO

      IF (NPTS.GT.0) THEN
         DMEAN = SUM/NPTS
         MEAN = DMEAN
         SIGMA=DSQRT(DABS(SUMSQ/NPTS-DMEAN*DMEAN))
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print the stretch table
C
      SUBROUTINE PTABLE(LUT,DNMIN,DNMAX)
      INTEGER*4 DNMIN,DNMAX
      INTEGER*2 LUT(-32768:32767)
      CHARACTER*132 PRT, PRTOUT
  180 FORMAT('IN  ',15I8)
  190 FORMAT('OUT ',15I8)

      CALL XVMESSAGE(' ', ' ')
      CALL XVMESSAGE(' T R A N S F O R M A T I O N', ' ')

      DO I=DNMIN,DNMAX,15
         J = MIN0(I+14,DNMAX)
         WRITE (PRT, 180) (K,K=I,J)
         WRITE (PRTOUT, 190) (LUT(K),K=I,J)
         CALL XVMESSAGE(PRT,' ')
         CALL XVMESSAGE(PRTOUT,' ')
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create stretch.pdf
PROCESS     HELP=*
PARM INP      TYPE=STRING  COUNT=1:2         
PARM OUT      TYPE=STRING  COUNT=1           
PARM SIZE     TYPE=INTEGER COUNT=4           DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1 VALID=(1:99999)	DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1 VALID=(1:99999)	DEFAULT=1
PARM SB       TYPE=INTEGER COUNT=1 VALID=(1:99999)	DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1 VALID=(0:99999)	DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1 VALID=(0:99999)	DEFAULT=0
PARM NB       TYPE=INTEGER COUNT=1 VALID=(0:99999)	DEFAULT=0
PARM LINEAR   TYPE=REAL    COUNT=(0,2)			DEFAULT=--
PARM DNMIN    TYPE=INTEGER COUNT=1 VALID=(-32768:0)	DEFAULT=0
PARM DNMAX    TYPE=INTEGER COUNT=0:1 VALID=(0:32767)	DEFAULT=255
PARM ASTRETCH TYPE=KEYWORD COUNT=0:1 VALID=ASTRETCH	DEFAULT=--
PARM PERCENT  TYPE=REAL    COUNT=0:1 VALID=(0.:100.)	DEFAULT=2.
PARM LPERCENT TYPE=REAL    COUNT=0:1 VALID=(0.:100.)	DEFAULT=1.
PARM HPERCENT TYPE=REAL    COUNT=0:1 VALID=(0.:100.)	DEFAULT=1.
PARM COMP     TYPE=KEYWORD COUNT=0:1 VALID=COMP     DEFAULT=-- 
PARM CLIP     TYPE=INTEGER COUNT=(0,1)              DEFAULT=--
PARM LOGARITH TYPE=KEYWORD COUNT=0:1 VALID=LOG      DEFAULT=--
PARM LOG      TYPE=REAL    COUNT=(0,2) VALID=(-10E15:10E15)       DEFAULT=--
PARM CURVE    TYPE=REAL    COUNT=0:1 VALID=(0.0001:10E15)         DEFAULT=1.0
PARM FUNCTION TYPE=STRING  COUNT=0:1                DEFAULT=--
PARM PSTRETCH TYPE=KEYWORD COUNT=0:1 VALID=PSTRETCH DEFAULT=--
PARM FREQ     TYPE=REAL    COUNT=0:1                DEFAULT=1
PARM AMPL     TYPE=REAL    COUNT=0:1                DEFAULT=--
PARM PHI      TYPE=REAL    COUNT=0:1                DEFAULT=0
PARM DC       TYPE=REAL    COUNT=0:1                DEFAULT=--
PARM CONTOUR  TYPE=INTEGER COUNT=(0,1) VALID=(1:65538)            DEFAULT=--
PARM ALARM    TYPE=INTEGER COUNT=(0,1:256) VALID=(-32768:32767)   DEFAULT=--
PARM DNVALUE  TYPE=INTEGER COUNT=0:1 VALID=(-32768:32767)         DEFAULT=--
PARM TABLE    TYPE=REAL    COUNT=(0,2:512) VALID=(-32768.:32767.) DEFAULT=--
PARM ITABLE   TYPE=INTEGER COUNT=(0,2:512) VALID=(-32768:32767)   DEFAULT=--
PARM BACKGND  TYPE=INTEGER COUNT=0:1 VALID=(-32768:32767)         DEFAULT=--
PARM PEAK     TYPE=KEYWORD COUNT=0:1 VALID=PEAK     DEFAULT=--
PARM MEAN     TYPE=KEYWORD COUNT=0:1 VALID=MEAN     DEFAULT=--
PARM RANGE    TYPE=INTEGER COUNT=0:1		    DEFAULT=--
PARM FACTOR   TYPE=REAL    COUNT=0:1		    DEFAULT=--
PARM SMOOTH   TYPE=KEYWORD COUNT=0:1 VALID=SMOOTH   DEFAULT=--
PARM GAUSS    TYPE=KEYWORD COUNT=0:1 VALID=GAUSS    DEFAULT=--
PARM GSIGMA   TYPE=REAL    COUNT=0:1 VALID=0.0005:1000.  DEFAULT=3.0
PARM GMEAN    TYPE=REAL    COUNT=0:1 VALID=-32767.:32766.         DEFAULT=--
PARM ELLIPSE  TYPE=KEYWORD COUNT=0:1 VALID=ELLIPSE  DEFAULT=--
PARM POWER    TYPE=INTEGER COUNT=0:1 VALID=0:99999  DEFAULT=--
PARM BIMODAL  TYPE=INTEGER COUNT=0:6 VALID=-32768:32767           DEFAULT=--
PARM REXCLUDE TYPE=INTEGER COUNT=(0,2:200) VALID=-32768:32767     DEFAULT=--
PARM IEXCLUDE TYPE=INTEGER COUNT=(0,1:100) VALID=-32768:32767     DEFAULT=--
PARM INCLUDE  TYPE=KEYWORD COUNT=0:1 VALID=INCLUDE  DEFAULT=--
PARM CUT      TYPE=REAL    COUNT=0:1 VALID=0:100    DEFAULT=--
PARM POST     TYPE=INTEGER COUNT=(0,2) VALID=-32768:32767   DEFAULT=--
PARM IHIST    TYPE=KEYWORD COUNT=0:1 VALID=IHIST    DEFAULT=--
PARM OHIST    TYPE=KEYWORD COUNT=0:1 VALID=OHIST    DEFAULT=--
PARM SPIKES   TYPE=INTEGER COUNT=0:1 VALID=0:65536  DEFAULT=3
PARM ICDF     TYPE=KEYWORD COUNT=0:1 VALID=ICDF     DEFAULT=--
PARM OCDF     TYPE=KEYWORD COUNT=0:1 VALID=OCDF     DEFAULT=--
PARM AREA     TYPE=INTEGER COUNT=0:4 VALID=1:99999  DEFAULT=--
PARM LINC     TYPE=INTEGER COUNT=0:1 VALID=1:100    DEFAULT=1
END-PROC
.TITLE
Contrast enhancement via DN transformation
.HELP
PURPOSE:

STRETCH is a VICAR application program for changing the contrast of an image.
More generally, the program changes the intensity of each pixel by performing
a transformation on the DN values:

            output_DN = T(input_DN)

Approximately 20 different transformations T are available.  Some of these
are analytic functions (e.g. linear, lorarithmic) and some are defined so
as to modify the image histogram is some specified manner.  See also programs
F2, FIT, HSTRETCH, STRETVAR.

.page
EXECUTION:

    STRETCH  INP=IPIC  OUT=OPIC  user-parameters....
 or STRETCH  INP=(IPIC,HIST)  OUT=OPIC  user-parameters...
where
    IPIC is the input image (byte or halword)
    HIST is an optional histogram file as generated by HISTGEN
    OPIC is the output image (same data format as input)

The type of transformation desired is specified via parameters.  If no
parameters are specified, a linear transformation which saturates 1 percent of
the data at both low and high ends of the histogram is performed (see
ASTRETCH parameter).

.page
OPERATION:

STRETCH performs a transformation T which maps every input DN value (IDN) to
an output DN value (ODN):

		            ODN = T(IDN)

The transformation is defined for all possible input DN values (0 to 255 DN
for byte data and -32768 to 32767 DN for halfword data).  Whenever an input DN
value leads to some mathematical difficulty, it is replaced by a more acceptable
input.  For example:

	STRETCH IN OUT FUNC="255/IN1"		0 DN is replaced by 1 DN
        STRETCH IN OUT FUNC="255*LOG(IN1)"      All DN<1 replaced by 1 DN

The DN range of the output image may be specified via the DNMIN and DNMAX
parameters.  If the transformation maps a DN value outside this range, then
ODN is set equal to these limits.  Note that many of the stretch options adjust
the DN scale so as to map the input DNs over the entire output range.  The
defaults for DNMIN and DNMAX are 0 and 255 for byte images and -32768 and 32767
for halfword images.

.page
TYPES OF STRETCH FUNCTIONS:

Approximately 20 different transformations are available.  Some of these
are analytic functions (e.g. linear, lorarithmic) and some are defined so
as to modify the image histogram is some specified manner.

The non-histogram stretches are specified by the following keywords:

    LINEAR,COMP,TABLE			Linear and piece-wise linear stretches
    LOGARITHMIC,PSTRETCH,FUNCTION	Analytic functions
    CONTOUR,ALARM,ITABLE		Image contouring
    CLIP				Bit clipping

The histogram stretches are:

    ASTRETCH,PEAK,MEAN			Linear stretches
    SMOOTH,GAUSS,ELLIPSE,POWER		Histogram matching stretches

Histogram stretches require that the histogram of the input image be computed.
Alternatively, the histogram may be included as an optional input file.

Each of the above stretches may be followed by a post stretch.	POST=(n1,n2)
causes a linear transformation such that DNMIN is mapped to n1 and DNMAX is
mapped to n2.

.page
NON-HISTOGRAM STRETCHES:

These stretches are based on some analytic function (e.g. linear, logarithmic).
The following is a summary of all available options.  See the help for each
individual keyword for a detailed description.

(1) Linear and piece-wise linear stretches:

        STRETCH  IN  OUT  LINEAR=(lowdn,highdn)
        STRETCH  IN  OUT  TABLE=(i1,o1,i2,o2,i3,o3,..,iN,oN)
        STRETCH  IN  OUT  'COMP

    LINEAR is a linear function which maps lowdn to DNMIN and highdn to DNMAX.
    TABLE defines a piece-wise linear function which maps i1 to o1, etc.
    COMP is a linear function which maps DNMIN to DNMAX and DNMAX to DNMIN
    (complementing the image).

(2) Non-linear stretches:

        STRETCH  IN  OUT  LOG=(lowdn,highdn)  CURVE=c
        STRETCH  IN  OUT  'PSTRETCH  AMPL=a  FREQ=f  PHI=p  DC=do
        STRETCH  IN  OUT  FUNCTION="fortran-expression"

    LOG is a natural log function for increasing the contrast of low DN areas.
    PSTRETCH is a sinusoidal function with specifiable period and amplitude.
    FUNCTION permits the function to be defined via a FORTRAN-like expression.

(3) Contouring and DN mapping:

        STRETCH  IN  OUT  CONTOUR=n  DNVALUE=m
        STRETCH  IN  OUT  ALARM=(dn1,dn2,dn3,...)  DNVALUE=m
        STRETCH  IN  OUT  ITABLE=(i1,o1,i2,o2,i3,o3,..,iN,oN)

    CONTOUR sets every nth DN value to m.
    ALARM sets DN values dn1, dn2, dn3,... to m.
    ITABLE sets DN values i1 to o1, i2 to o2,...

    All other DN values are left unchanged.  Alternatively, if BACKGND=b is
    specified, then all other DN values are set to b.

(4) Bit clipping:

        STRETCH  IN  OUT  CLIP=n

    If n is positive, the bits are shifted left.  If n is negative, the
    bits are shifted right.  This causes the n most-significant-bits or
    least-significant-bits to be truncated.  Shifting the bits left causes
    the output DN range to be reused multiple times, causing image contouring.

.page
HISTOGRAM STRETCHES:

Histogram stretches all require that the histogram of the input image be
first computed, or included as a secondary input file.  During histogram
computation, all input pixels less than DNMIN or greater than DNMAX are set
equal to these values.

The area of the image from which the histogram is computed can be specified via
the AREA parameter.  LINC=n parameter causes every nth line to be used to
compute the histogram.

Unless 'INCLUDE is specified, all histogram stretches ignore the data at the
limits (DNMIN and DNMAX) when computing the transfer function.  The REXCLUDE,
IEXCLUDE, and CUT parameters can be used to exclude additional data from
the histogram as well.

The following is a brief description of each histogram stretch.  See the
help on each individual parameter for a more detailed description.

(1) Linear stretches:

        STRETCH  IN  OUT  'ASTRETCH  LPERCENT=r1  HPERCENT=r2
        STRETCH  IN  OUT  'PEAK
        STRETCH  IN  OUT  'MEAN

    ASTRETCH is similar to LINEAR, except that the stretch limits n1 and n2
    are determined by saturating r1 and r2 percent of the data from the
    low and high ends of the histogram.

    PEAK and MEAN are useful variants of ASTRETCH.  The stretch limits n1
    and n2 are determined as before.  A piece-wise linear function is used
    to map n1 to DNMIN, n2 to DNMAX, and the median or mean pixel value to
    the mid-range of the output image, i.e. (DNMIN+DNMAX)/2.

(2) Histogram matching stretches:

        STRETCH  A  B  'SMOOTH
        STRETCH  A  B  'GAUSS  GSIGMA=s  GMEAN=m
        STRETCH  IN  OUT  'ELLIPSE
        STRETCH  IN  OUT  POWER=r

    For these stretches, the transfer function is computed so that the
    histogram of the output image approximates a certain specified shape.
    The shape is only approximated since all pixels with the same input DN
    must map to the same output DN.

    KEYWORD	SHAPE OF OUTPUT HISTOGRAM		
    --------	-------------------------
    SMOOTH 	uniform distribution (all DN values have equal frequencies)
    GAUSS 	normal (or Gaussian) distribution centered about midrange
    ELLIPSE	top half of an ellipse centered about midrange
    POWER	Of the form y=1-|x|**r, but scaled and centered at midrange


.page
HISTOGRAM PRINT OPTIONS:

If the input image is in byte data format, several histogram print options
apply.

    IHIST prints the histogram of the input image
    OHIST prints the histogram of the output image
    ICDF  prints the Cumulative Distribution Function of the input image
    OCDF  prints the Cumulative Distribution Function of the output image

The CDF is computed from the histogram of the input image as follows:

                               i
                     CDF(i)=  SUM  HIST(j)      for i=0,1,2,3,...,n-1
                               j=0

where CDF(i) is the total number of pixels with DN values less than or equal
to i, and the histogram HIST has n grey levels.

.page
TIMING:

The transformation is first computed in the form of a look-up table, i.e.
a table which contains the output DN value for every possible input DN value.
For byte data, the table contains 256 values (0 to 255 DN).  For halfword data,
the table contains 2**16 values (-32768 to 32767 DN).  If a post stretch is
specified, this stretch is combined in the look-up table.

After the look-up table has been computed, it is used to create the output
image.  Each pixel in the input image is replaced by its corresponding output
value in the table.  Because of the use of the look-up table, program execution
time is not significant and is independent of the complexity of the
transformation.

.page
EXAMPLES:

Type in the following commands and observe the results.

gen a 10 10	!Create a 10x10 byte image
list a		!Print the image

After each of the following executions of stretch, print the results as follows:

list b

!.............Complementing the image (3 equivalent ways)........
stretch a b 'comp
stretch a b linear=(255,0)
stretch a b table=(0,255,255,0)

!.............Linear histogram stretch (2 equivalent ways)........
histgen a h				!First compute histogram h
stretch (a,h) b perc=5. 'include	!and input it to stretch
stretch a b perc=5. 'include		!Or make stretch compute histogram

!.............Piecewise linear histogram stretches........
f2 a c func="0.5*in1**2"		!Could have used stretch to do this
stretch c b 'mean 'include		!Put the mean in the middle
stretch c,b 'peak 'include		!Put the median in the middle
stretch c b 'peak 'include range=41	!This doesn't look too useful
stretch c b 'peak 'include factor=2	!This doesn't look too useful either

!.............Bimodal stretch (do not try this at home)........
stretch a b func="in1+18"
concat (a,b) c ns=20 'nost	!Create image with bimodal histogram
hist c				!See?  Bimodal
size c d zoom=10 'noin		!Make it larger or algorithm won't work
stretch d e bimodal=0 'ohist	!Auto-stretches around both peaks

!.............Bit clipping (2 equivalent ways)...............
stretch a b clip=6
stretch a b func="mod(64*DN1,256)"

!.............Contouring (3 equivalent ways)...................
stretch a b contour=4 dnvalue=100
stretch a b alarm=(0,4,8,12,16) dnvalue=100
stretch a b itable=(0,100,4,100,8,100,12,100,16,100)

!.............Logarithmic function (2 equivalent ways).......
stretch a b log=(0,18)
stretch a b func="86.6039343332527*ln(IN1+1)"

!.............Sinusoidal function (3 equivalent ways)..............
stretch a b 'pstretch freq=16 phi=128 ampl=100 dc=49.5
stretch a b func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
f2 a b func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"

!................Histogram-matching stretches.....
stretch a b 'smooth 'ohist
stretch a b 'ellipse 'ohist
stretch a b power=1 'ohist
stretch a b 'gauss gsigma=5.0 gmean=137.5 'ohist

.page
PROGRAM HISTORY:

The current version of STRETCH began as two older programs:

(1) STRETCH, written by Tom Rindfleish (April 4, 1969) contained the
    non-histogram stretches.
(2) ASTRTCH2, written by Arnie Schwartz (August 3, 1971) contained the
    histogram stretches.

The two programs were merged and the FUNCTION keyword added by John Reimer.

Written by:  J. H. Reimer   10/31/85
Cognizant programmer: Lucas Kamp
Revision history:
 5 87  SP   Used NINT function for rounding for compatibility with the previous
	    version of stretch for negavite DNs.
 6 87  SP   Added parms parameter in PDF to be compatible with ASTRTCH2.
 1 88  SP   Added 'INCLUDE and changed EXCLUDE defaults to agree with ASTRTCH2.
10 88  SP   Extended linear stretch to handle inversion.  This means that
            linear=(255,0) is the same as 'COMP.
 5 92  SP  oMade portable for UNIX.
	   oUsed Niles Ritter changes to replace calls to OUTCON.
	   oAdapted for new version of KNUTH.
           oAdded call xvpcnt('INP',ni) to fix FR 46486.
	   oReplaced variable ITOL with RTOL in Subroutine FCLIP because
            the code had a bizarre .05 in it and really was not per spec.
           oFollowed Ron Alley's lead in deleting the RATIO parameter
            because it had no effect.  (It cancels itself out in the
            calculation.)
           oCorrected the DC default for PSTRETCH to be the average of DNMAX
	    and DNMIN.
           oFixed error for byte data where DNMAX<255.  Routines INIT and ALTER
	    were assuming that LUT has 256 entries. I make sure LUT is at least
            256 halfwords, and I fill it all in.
	   oFixed default DC for PSTRETCH.  Clarified Help that AMPL=
            2*(mathematical amplitude) for PSTRETCH.
	   oAdded GMEAN parameter for FR 48458.  Note that the default
            GMEAN used to be 127 = (255-0)/2 for BYTE data.  The default
            is now 127.5 = (255+0)/2.0, which makes STRETCH consistent
            with VIDS JSTRETCH.
 9-92  JFM  Data number in STATI set to a REAL*2 value to avoid integer
	    overflow when dealing with negative DN values. (FR 76821)	
10-92   SP oChanged name of subroutine MAIN to WORK to avoid name collisions.
           oAdded EXTERNAL statement to prevent compiler warnings.
 2-93   SP oAdded Logarithmic stretch ala VIDS.
           oMove initializers into data statements to comply with Porting Guide.
	   oMade LINEAR parameters REAL instead of INTEGER ala VIDS.
 3-95  CRS (CRI) ... MSTP S/W CONVERSION (VICAR PORTING)
            Revised to use Histogram files in IBIS format    
10-02  GMY  1) Massive code reorganization.
            2) Rewrote help file, documenting each stretch option.
            3) Rewrote test script, testing all stretch options.
            4) Fixed numerous bugs detected as a result of new test script.
08-03  NTT  Enabled 3D image capability to stretch and get_histogram.
08-07  LWK  1. Fixed problem in STABLE where a very steep slope resulted in loss
            of precision in computing LUT, by doing the calculation in double
            precision.
            2. Fixed error in RTRUNC rounding negative numbers.
            3. Increased size of buffer passed to KNUTH from 100 to 300 words
            (was causing crash in Linux).
            4. Removed COMMON/CW/ lines in MANUAL_STRETCH.F which were not being
            used as COMMONs and were causing compiler warnings due to unequal
            lengths.
01-2013 LWK Fixed CHARACTER continuation lines in print_histogram.f for new compiler
            flag on Solaris.

.page
PROGRAMMER'S NOTES:

The following is the subroutine tree for STRETCH:

STRETCH
WORK
  MANUAL_STRETCH
    SCOMP		IMODE=1
    SLINEAR		IMODE=2
    SCLIP		IMODE=3
    SCONTOUR		IMODE=4
    SALARM		IMODE=5
    STABLE		IMODE=6
    SITABLE		IMODE=7
    SPERIODIC		IMODE=8
    SFUNC		IMODE=9
    SLOG		IMODE=10
  HIST_PARAMS
  GET_HISTOGRAM
    READ_HISTOGRAM
    COMPUTE_HISTOGRAM
      HISGET
  PRINT_HISTOGRAM
  EXCLUDE_HISTOGRAM
    FCLIP	Exclude HIST entries less than given % from max
  HISTOGRAM_STRETCH
    HMOD	Compute LUT which modifies histogram to one of the following:
      NRAMP	'SMOOTH		IMODE=11
      NGAUSS	'GAUSS		IMODE=12
      NELLIP	'ELLIPSE	IMODE=13
      NPOWER	'POWER		IMODE=14
    BIMODAL			IMODE=15
      TWOPK
        HFILT	Apply low-pass filter to HIST
        HDIFF	Compute derivative (HIST(I)-HIST(I-1) of histogram
    SPEAK	'PEAK and 'MEAN IMODE=16 or 17
    ASTRETCH	'ASTRETCH	IMODE=18
  POST_STRETCH	Modify LUT for POST stretch
  STRETCH_IMAGE	Use the LUT to stretch the image and write to output file
  PRINT_OHISTOGRAM  Print output histogram

Utility routines:
CHK_NUM_STRETCH	Increment nstretch and ABEND if greater than 1
STATI		Compute mean and standard deviation from histogram
PTABLE		Print the LUT
PHIST		Print HIST
CDF		Compute the CDF from HIST and print it

.LEVEL1
.VARIABLE INP
INP=image  or
INP=(image,histogram)
.VARIABLE OUT
OUT=output_image
.VARIABLE SIZE
SIZE=(sl,ss,nl,ns)
(4 Integers)
.VARIABLE SL
Starting line
(1 integer)
.VARIABLE SS
Starting sample
(1 integer)
.VARIABLE SB
Starting band
(1 integer)
.VARIABLE NL
Number of lines
(1 integer)
.VARIABLE NS
Number of samples
(1 integer)
.VARIABLE NB
Number of bands
(1 integer)
.VARIABLE DNMIN
Minimum output DN
(1 Integer)
.VARIABLE DNMAX   
Maximum output DN  
(1 Integer)     
.VARIABLE LINEAR
Linear function
LINEAR=(r1,r2)
(2 reals)
.VARIABLE ASTRETCH          
Auto-stretch (linear)
(Keyword)                  
.VARIABLE PERCENT
Total percentage of image
to be saturated.
.VARIABLE LPERCENT
Percentage of image to be
saturated at the low end.
.VARIABLE HPERCENT
Percentage of image to be
saturated at the high end.
.VARIABLE COMP     
Complement the image
(Keyword)          
.vari logarith
Logarithmic stretch
.vari log
LOG=(low,high)
Logarithmic stretch
.vari curve 
Curvature of logarithmic
stretch (real)
.VARIABLE FUNCTION       
(String)
Fortran-like exptression
Ex: FUNC="2*DN+1")
.VARIABLE CLIP
Bit clipping
(Integer)
.VARIABLE PSTRETCH
Periodic stretch
(Keyword)
.VARIABLE FREQ
Frequency used in PSTRETCH
(Real)
.VARIABLE AMPL
Amplitude used in PSTRETCH
(Real)
.VARIABLE PHI
Phase used in PSTRETCH
(Real)
.VARIABLE DC
Mean used in PSTRETCH
(Real)
.VARIABLE CONTOUR
Contour interval
(Integer)
.VARIABLE ALARM
Flag Dn values
ALARM=(dn1,dn2,...)
.VARIABLE DNVALUE
DN value for CONTOUR
and ALARM stretches
DN=integer
.VARIABLE TABLE
Piece-wise linear stretch
TABLE=(i1,o1,i2,o2,...)
(up to 100 pairs of reals)
.VARIABLE ITABLE
Change specific DN values
ITABLE=(i1,o1,i2,o2,...)
(up to 256 pairs of integers)
.VARIABLE BACKGND
Background DN value
(see CONTOUR,ALARM,ITABLE,
 TABLE)
.VARIABLE PEAK                
Auto-stretch around
median DN value
(Keyword)                    
.VARIABLE BIMODAL
Auto-stretch around
both peaks of a
bimodal histogram.
1-6 integers
.VARIABLE MEAN                
Auto-stretch around
mean DN value
(Keyword)                    
.VARIABLE RANGE
Range used in PEAK or MEAN stretch
(integer)
.VARIABLE FACTOR
Factor used in PEAK or MEAN stretch
(real)
.VARIABLE SMOOTH               
Output histogram has
uniform distribution
(Keyword)                     
.VARIABLE GAUSS                
Output histogram has
Gaussian distribution
(Keyword)                     
.VARIABLE GSIGMA
Standard deviation for
GAUSS stretch
(real)
.vari GMEAN
Mean for GAUSS stretch
(real)
.VARIABLE ELLIPSE              
Output histogram has
elliptic distribution
(Keyword)                    
.VARIABLE POWER             
Output histogram has
distribution of the
form y=1-|x|**r
(real)
.VARIABLE REXCLUDE
Range of DNs to exclude
from stretch
(N Pairs of Integers)
.VARIABLE IEXCLUDE
Individual DNs to exclude
from stretch.
(N Integers)
.VARIABLE INCLUDE
Include 0 and the maximum DN in
the histogram=.
(Keyword)                    
.VARIABLE CUT
CUT=r excludes DN's with
frequencies less than r%
of maximum frequency
from histogram.
(Real)
.VARIABLE POST              
POST=(lowdn,highdn)
Follow initial stretch
with linear stretch
maping DNMIN to lowdn
and DNMAX to highdn
(2 integers)     
.VARIABLE IHIST
Print input histogram
(Keyword)
.VARIABLE OHIST
Print output histogram
(Keyword)
.VARIABLE SPIKES
Number of spikes in
printed histogram
(1 Integer)
.VARIABLE ICDF
Print input CDF      
(Keyword)
.VARIABLE OCDF
Print output CDF     
(Keyword)
.VARIABLE AREA
Image area used to
calculate histogram
AREA=(sl,ss,nl,ns)
(4 integers)
.VARIABLE LINC
Line subsampling used in
computing histogram
(Integer)

.LEVEL2
.VARIABLE INP
	INP=A
     or INP=(A,H)
where
      A is the input image (byte or halfword).
      H is an optional histogram previously generated by HISTGEN.
.VARIABLE OUT
	OUT=B
where B is the output image.  B will have the same data format as the input
image.
.VARIABLE SIZE
	SIZE=(sl,ss,nl,ns)
Four integers, representing the standard Vicar size field.  This specifies an
image area by starting line, starting sample, number of lines, and number of
samples.
.VARIABLE SL
 INTEGER - Starting line
.VARIABLE SS
 INTEGER - Starting sample
.VARIABLE SB
 INTEGER - Starting band
.VARIABLE NL
 INTEGER - Number of lines
.VARIABLE NS
 INTEGER - Number of samples
.VARIABLE NB
 INTEGER - Number of bands
.VARIABLE DNMIN
		DNMIN=n1  DNMAX=n2	(n1 and n2 are integers)

Specifies the minimum and maximum DN values in the output image.  Many of the
stretch options adjust the DN scale so as to map the input DNs over the entire
output range.  All input pixels which map to an output value outside these
limits are set equal to these limits.

For byte images, the defaults are DNMIN=0 and DNMAX=255.
For halfword images, the defaults are DNMIN=-32768 and DNMAX=32767.  However,
if a HISTGEN created histogram is supplied as the second input, DNMIN=0.
.VARIABLE DNMAX
See parameter DNMIN for description.
.VARIABLE LINEAR
	STRETCH  IN  OUT  LINEAR=(r1,r2)	(real numbers r1, r2)

A linear transformation T is applied, mapping each input DN (IDN) onto an
output DN (ODN) such that r1 maps to DNMIN and r2 maps to DNMAX.  The actual
formula is:

	               DNMAX - DNMIN
		ODN = ---------------(IDN - r1) + DNMIN
			  r2 - r1

The transformation is truncated for all DN values outside the input range
defined by r1 and r2, since these would map outside the output range.

Note that r1 may be greater than r2.  For example, if the input image is 
in byte format (DNMIN=0,DNMAX=255), the operation
	STRETCH A B LINEAR=(255,0)
is equivalent to complementing the image
	STRETCH A B 'COMP
.VARIABLE CLIP
	STRETCH  IN  OUT  CLIP=n		(integer n)

If n>0, the binary representation of the DN value is shifted n bits to the left
and the n most significant bits are truncated.  If n<0, the bits are shifted to
the right and the n least significant bits are truncated.  The following
examples assume that the input image is byte data.

Ex:      STRETCH  A  B  CLIP=4
 
     The bits are shifted left 4 bits:  abcdefgh ----> efgh0000
     This has the same effect as

         STRETCH  A  B  FUNC="mod(16*DN,256)"

Ex:  STRETCH  A  B  CLIP=-4

     The bits are shifted right 4 bits:  abcdefgh ----> 0000abcd
     This has nearly the same effect as

         STRETCH  A  B FUNC="DN/16"

     However, in the latter, rounding occurs so that, for example, 8/16=1.
.VARIABLE ASTRETCH
		STRETCH  IN  OUT  'ASTRETCH
       or	STRETCH  IN  OUT  PERCENT=r
       or	STRETCH  IN  OUT  LPERCENT=r1  HPERCENT=r2

This is the default stretch.  A linear transformation is applied so that lowdn
maps to DNMIN and highdn maps to DNMAX.  The stretch limits (lowdn and highdn)
are computed so as to saturate r1 percent and r2 percent of the data from the
low and high ends of the histogram, respectively.

If PERCENT=r is specified, then r1 = r2 = 0.5*r.

See also parameters LPERCENT, HPERCENT, PERCENT, PEAK, MEAN, DNMIN, DNMAX,
REXCLUDE, IEXCLUDE, INCLUDE, and CUT.
.VARIABLE PERCENT
	PERCENT=r	(real number)

Specifies the total percentage of the input image to be saturated in the
auto-stretch option (0<=r<=100).  The above statement is equivalent to:

	LPERCENT=r1  HPERCENT=r1

where r1=r/2.  Used in the following stretches: ASTRETCH, PEAK, MEAN.
.VARIABLE LPERCENT
	LPERCENT=r1

Specifies the percentage of the input image to be saturated from the low end
of the histogram in the auto-stretch option (0<=r1<=100).  If defaulted, then
LPERCENT = PERCENT/2.  Used in the following stretches: ASTRETCH, PEAK, MEAN.
.VARIABLE HPERCENT
	HPERCENT=r2

Specifies the percentage of the input image to be saturated from the high end
of the histogram in the auto-stretch option (0<=r2<=100).  If defaulted, then
HPERCENT = PERCENT/2.  Used in the following stretches: ASTRETCH, PEAK, MEAN.
.VARIABLE COMP
	STRETCH  IN  OUT  'COMP

This is a linear transformation which maps DNMIN to DNMAX and DNMAX to DNMIN,
causing the image to be complemented (reversed).  The statement
	STRETCH A B 'COMP
is equivalent to
	STRETCH A B LINEAR=(255,0)
for byte images, and
	STRETCH A B LINEAR=(32767,-32768)
for halfword images.
.VARI LOGARITH
	STRETCH  IN  OUT  'LOG

This is equivalent to

	STRETCH  IN  OUT  LOG=(0,255)	for byte data
or      STRETCH  IN  OUT  LOG=(0,32767) for halfword data
.VARI LOG
	STRETCH  IN  OUT  LOG=(lowdn,highdn)  CURVE=c

The transformation is a natural logarithmic function of the form:

	ODN = a*ln(IDN+c) + b

where IDN and ODN are the input and output DN values of a pixel, c controls the
curvature of the function, and a and b are scale and offset terms calculated so
that lowdn maps to DNMIN and highdn maps to DNMAX.  The log stretch increases
the contrast in dark areas of the image at the expense of decreased contrast in
brighter areas.

The CURVE parameter c causes a horizontal offset to the log curve.  This
controls the portion of the curve that the input DNs are mapped onto.  Recall
that the slope of ln x approaches -infinity as x approaches 0.  Therefore, for
the curve to be defined over the entire input domain, we must have lowdn+c > 0.
The default is c=1.

See also parameters DNMIN, DNMAX.
.VARI CURVE 
	CURVE=c			(real number)

where 0 < c < 10**15.  Default is c=1.  See parameter LOG for description.
.VARIABLE FUNCTION
	STRETCH  IN  OUT  FUNCTION="fortran-like expression"

Allows the user to specify the transfer function as a FORTRAN-like expression
with the keyword DN as the independent variable.  This is similar to the VICAR
program F2.  For example,

	STRETCH  A  B  FUNCTION="2*DN+3"

multiplies each input DN value by 2 and adds 3.  All resulting values outside
the range specified by DNMIN and DNMAX are truncated.
.VARIABLE PSTRETCH
	STRETCH  IN  OUT  'PSTRETCH  AMPL=a  FREQ=f  PHI=p  DC=do

The transfer function is a periodic stretch of the form:

              a          2*PI*f*IDN
       ODN = --- * SIN( ------------- + p) + do
              2          DNMAX-DNMIN

The defaults are:	AMPL= DNMAX-DNMIN
			FREQ= 1.0
			PHI = 0.0
			DC  = (DNMAX+DNMIN)/2

These defaults map the input DN to one period of a sin wave, offset and scaled
so that the output has a range from DNMIN to DNMAX (see AMPL and DC terms).

See also parameters DNMIN, DNMAX.
.VARIABLE AMPL

	AMPL=a		(see keyword PSTRETCH)

Specifies the amplitude to be used in the periodic stretch.  Note that AMPL is
the peak-to-peak amplitude (i.e. 2 times what is the normal meaning of
amplitude).
.VARIABLE FREQ

	FREQ=r		(see keyword PSTRETCH)

Specifies the frequency to be used in the periodic stretch.
.VARIABLE PHI

	PHI=r		(see keyword PSTRETCH)

Specifies the phase to be used in the periodic stretch.
.VARIABLE DC

	DC=r		(see keyword PSTRETCH)

Specifies the mean output DN in the periodic stretch.
.VARIABLE CONTOUR

	STRETCH  IN  OUT  CONTOUR=n  DNVALUE=m

Input intensities which are a multiple of n are	set to the value m.  All
other DN values remain unchanged unless the BACKGND parameter is specified.

Ex: For byte images, the following will produce identical results, i.e.
    all pixels with DN values of 64, 128, and 192 are set to 0 DN.

        STRETCH  A  B  CONTOUR=64  DNVALUE=0
        STRETCH  A  B  ALARM=(64,128,192) DNVALUE=0

.VARIABLE ALARM
        STRETCH  IN  OUT  ALARM=(dn1,dn2,dn3,...)  DNVALUE=m

The input pixels with the DN values specified are set to the value m.  All
other DN values remain unchanged unless the BACKGND parameter is specified.

Ex: For byte images, the following will produce identical results, i.e.
    all pixels with DN values of 64, 128, and 192 are set to 0 DN.

        STRETCH  A  B  ALARM=(64,128,192) DNVALUE=0
        STRETCH  A  B  CONTOUR=64  DNVALUE=0
.VARIABLE DNVALUE
		DNVALUE=n

Specifies the replacement DN value for the CONTOUR and ALARM stretches. The
appropriate DN's will be set to this DN value.  (Default is DNVALUE=DNMAX)

See also parameters CONTOUR, ALARM, DNMAX.
.VARIABLE TABLE
        STRETCH  IN  OUT  TABLE=(i1,o1,i2,o2,i3,o3,..,iN,oN)

N pairs of DN values (real).  The TABLE defines a piece-wise linear transform
where the input DN values i1, i2, i3, ..., iN are mapped to the output DN
values o1, o2, o3, ..., oN, respectively, and the intervening DN values are
computed via linear interpolation.  Note that i1 < i2 < i3 < ...< iN whe N<=100.

All DN values outside the range in1 to inN normally remain unchanged.
However, if the parameter BACKGND is specified, these out-of-range DNs are
set to the BACKGND value.
.VARIABLE ITABLE
        STRETCH  IN  OUT  ITABLE=(i1,o1,i2,o2,i3,o3,..,iN,oN)

N pairs of integers.  Input DN values i1, i2, i3,...,iN are set equal to
output DN values o1, o2, o3,...,oN, respectively.  Note N<=100.
All other DN values remain unchanged unless the BACKGND parameter is specified.
.VARIABLE BACKGND
	BACKGND=m

Used with parameters CONTOUR, ALARM, ITABLE, and TABLE.  Specifies the output DN
value for all DNs not specified in the above parameters.  If BACKGND is not
specified, the corresponding pixels are copied from input to output unchanged.
.VARIABLE PEAK
		STRETCH  IN  OUT  'PEAK

Specifies a symmetrical linear stretch about the median DN of the image.  The
median is the DN with the maximum frequency in the image, and is determined
by locating the histogram peak.

A linear stretch is applied so that the median DN is mapped to mid-range in
the output (0.5*(DNMIN+DNMAX)).  The scale (slope) of the stretch is
determined in one of three ways:

If RANGE=n is specified, then lowdn=median-n/2 and hidn=median+n/2 are mapped
to DNMIN and DNMAX, respectively.

If FACTOR=s is specified, then the scale is s.

If neither RANGE or FACTOR are specified, then the scale is computed so as
to saturate PERCENT of the data (see PERCENT parameter).

See also parameters ASTRETCH, MEAN, RANGE, PEAK, DNMIN, DNMAX.
.VARIABLE MEAN
		STRETCH  IN  OUT  'MEAN

Specifies a symmetrical linear stretch about the mean DN of the image.  Since
the mean DN is determined from the histogram, all parameters effecting
histogram computations apply (e.g. AREA, INCLUDE, REXCLUDE).

A linear stretch is applied so that the mean DN is mapped to mid-range in
the output (0.5*(DNMIN+DNMAX)).  The scale (slope) of the stretch is
determined in one of three ways:

If RANGE=n is specified, then lowdn=mean-n/2 and hidn=mean+n/2 are mapped
to DNMIN and DNMAX, respectively.

If FACTOR=s is specified, then the scale is s.

If neither RANGE or FACTOR are specified, then the scale is computed so as
to saturate PERCENT of the data (see PERCENT parameter).

See also parameters ASTRETCH, RANGE, FACTOR, PEAK, DNMIN,DNMAX.

.VARIABLE RANGE
		STRETCH  IN  OUT  'PEAK  RANGE=n
		STRETCH  IN  OUT  'MEAN  RANGE=n

Causes the PEAK/MEAN stretch to saturate all but the specified DN interval
about the peak or mean DN.  I.e., this results in a linear transformation
where N-n maps to DNMIN and N+n maps to DNMAX, where N is the peak/mean DN.

Default is a linear transformation which saturates PERCENT of the data
symmetrically about the peak/mean DN.
.VARIABLE FACTOR
		STRETCH  IN  OUT  'PEAK  FACTOR=r
		STRETCH  IN  OUT  'MEAN  FACTOR=r

Specifies the slope term of the linear transformation defined by the peak/mean
DN.

Default is a linear transformation which saturates PERCENT of the data
symmetrically about the peak/mean DN.
.VARIABLE SMOOTH

	STRETCH  IN  OUT  'SMOOTH

This transformation causes the histogram of the output image to approximate a
uniform distribution (i.e. the output sample frequency is the same at every
DN level).  The transfer function is computed from the input histogram.

See also parameters DNMIN, DNMAX, REXCLUDE, IEXCLUDE, INCLUDE, CUT.

.VARIABLE GAUSS

	STRETCH  IN  OUT  'GAUSS  GSIGMA=s  GMEAN=m

This transformation causes the histogram of the output image to approximate a
Gaussian (or normal) distribution with a standard deviation of s DN and the
mean at m DN:

                    A                (i-m)**2
	h(i) = ------------ * e**( - -------- )
               s*sqrt(2*pi)            2s**2

where h(i) is the output sample frequency at DN i, and A is a scale term which
causes the total number of samples in the output histogram (area under the
curve) to be the same as the input histogram.

If GSIGMA is defaulted, s=3.  If GMEAN is defaulted, m=(DNMAX+DNMIN)/2.

For values of GSIGMA less than 1.0, the distribution will tend toward 
uniformity.

See also parameters DNMIN, DNMAX, REXCLUDE, IEXCLUDE, INCLUDE, CUT.

.VARIABLE GSIGMA

	GSIGMA=s		(real number)

Specifies the standard deviation in the Gaussian stretch.  See parameter
GAUSS for details.

.VARI GMEAN

	GMEAN=m			(real number)

Specifies the standard deviation in the Gaussian stretch.  See parameter
GAUSS for details.

.VARIABLE ELLIPSE

	STRETCH  IN  OUT  'ELLIPSE

This transformation causes the histogram of the output image to approximate the
the shape of the top half of an ellipse centered at the mid-DN level:

	h(x) = N*sqrt(a**2 - (x-a)**2)

where h(x) is the output sample frequency at x DN, a is both the center of the
ellipse and the size of the semi-major axis (h is 0 at x=a and x=2a and reaches
a maximum at x=a), and N is a scale term which causes the total number of
samples in the output histogram (area under the curve) to be the same as the
input histogram.  More exactly,

	a = (MAXDN-MINDN+1)/2

See parameters MAXDN, MINDN, REXCLUDE, IEXCLUDE, INCLUDE, CUT.

.VARIABLE POWER

		STRETCH  IN  OUT  POWER=r

This transformation causes the histogram of the output image to approximate the
the shape of the function:

		y = 1 - |x|**r

Let DNMIN=i1, DNMAX=i2.  The actual function is:

                                     i-i1
		h(i) = s*(1 - abs(2*------ - 1)**r)
                                    i2-i1

where h(i) is the sample frequency at output DN value i, and s is a scale term
which causes the total number of samples in the output histogram (area under
the curve) to be the same as the input histogram.  This causes a translation
and scaling of the x-axis (output DN) such that h(i1)=h(i2)=0 and the maximum
frequency occurs mid-way between this range.

See also parameters DNMIN, DNMAX, REXCLUDE, IEXCLUDE, INCLUDE, and CUT.

.VARIABLE BIMODAL
		BIMODAL=(o1,o2,o3,o4,o5,o6)

BIMODAL is intended to be applied to images with a bimodal histogram.  An
automatic linear stretch (see ASTRETCH option) is applied to the section of
the histogram surrounding each peak.

First, the two peaks are located.  If more than two peaks are located, the
histogram is progressively filtered to smooth the data.  If exactly two peaks
cannot be located, the program reverts to the ASTRETCH mode.

The minimum between the two peaks is then located.  This divides the histogram
into lower and upper pieces.  The auto-stretch algorithm is applied to both
pieces, resulting in stretch limits l1, h1 and l2, h2.  We have

		DNMIN < l1 < h1 <l2 <h2 < DNMAX

A piecewise linear transformation is applied such that DNMIN maps to o1,
l1 maps to o2, h1 maps to o3, l2 maps to o4, h2 maps to o5, and DNMAX maps to
o6.

The default (specified by BIMODAL=0) maps DNMIN to DNMIN, l1 to DNMIN, 
h1 to MIDDN, l2 to MIDDN+1, h2 to DNMAX, and DNMAX to DNMAX, where
MIDDN=(DNMIN+DNMAX)/2.
.VARIABLE REXCLUDE

		REXCLUDE=(m1,n1,m2,n2,m3,n3,...)

Up to 100 pairs of integers specifying DN ranges to exclude from the histogram.
All DN values within any of the specified ranges will have their frequencies
set to zero prior to using the histogram to compute the transfer function.

Overlapping DN ranges may be specified. You must have  DNMIN < mi < ni < DNMAX.
See also parameters INCLUDE, IEXCLUDE.

.VARIABLE IEXCLUDE

		IEXCLUDE=(n1,n2,n3,...)

Up to 100 integers specifying individual DN values to exclude from the
histogram.  All DN values specified will have their frequencies set to zero
prior to using the histogram to compute the transfer function.

See also parameters INCLUDE, REXCLUDE, CUT.

.VARIABLE INCLUDE

	KEYWORD:  'INCLUDE

'INCLUDE suppresses the default exclusion of 0 DN and max DN (255 for byte
data and 32767 for halfword data) from the histogram.

The reason that 0 DN and max DN are normally excluded is because these values
usually represent saturated samples (dark sky or overexposed image areas).
The various histogram stretch options perform best when these saturated samples
are excluded during computation of the transfer function.

In general, 'INCLUDE should be used for halfword images where 0 and negative
DNs represent valid image data.

See also parameters REXCLUDE, IEXCLUDE, CUT.

.VARIABLE CUT

		CUT=r		(0 <= r <= 100.)

Specifies a percentage of the maximum frequency count in the input image
histogram.  All DN values with a frequency count less than this pecentage of
the maximum frequency count will be excluded from the histogram (their
frequencies set to zero) prior to computation of the transfer function.
Default is r=0.

See also parametes REXCLUDE, IEXCLUDE, INCLUDE

.VARIABLE POST

		POST=(n1,n2)

A linear post stretch is performed following the initial stretch. The initial
stretch will map the input DN values to the range DNMIN to DNMAX.  The post
stretch will be a linear transformation such that DNMIN maps to n1 and DNMAX
maps to n2, as in the following byte image example:

		STRETCH  A  C  'GAUSS  POST=(255,0)

Since DNMIN and DNMAX are defaulted, the gaussian stretch will map the input
DNs to the full byte range (0 to 255).  The post stretch will then perform a
linear stretch such that 0 DN maps to 255 and 255 maps to 0.  The above is
equivalent to:

		STRETCH  A  B  'GAUSS
		STRETCH  B  C  'COMP

.VARIABLE IHIST

	KEYWORD:  'IHIST	(valid for byte data only)

Prints the histogram of the input image. If SPIKES=n is specified, the display
is normalized so that the n+1st highest frequency is at full scale.
.VARIABLE OHIST

	KEYWORD:  'OHIST	(valid for byte data only)

Prints the histogram of the output image. If SPIKES=n is specified, the display
is normalized so that the n+1st highest frequency is at full scale.
.VARIABLE SPIKES

		SPIKES=n	(see keywords 'IHIST and 'OHIST)

This adjusts the frequency scale of the histogram so that the DN value with
the n+1st highest frequency will appear at full scale (the n higher frequencies
are saturated).  This scaling does not affect the frequency values printed with
the histogram, nor does it affect the operation of the program in generating
the output image.  (Default is SPIKES=3) 
.VARIABLE ICDF

	KEYWORD:  'CDF		(valid for byte data only)

Prints the Cumulative Distribution Function (CDF) of the input image, where
CDF(i) is the total number of pixels with DN values less than or equal to i.
The CDF is computed from the histogram of the input image as follows:
	                       i
	             CDF(i)=  SUM  HIST(j)	for i=0,1,2,3,...,n-1
			       j=0

where the histogram HIST has n grey levels.
.VARIABLE OCDF

	KEYWORD:  'OCDF		(valid for byte data only)

Prints the Cumulative Distribution Function (CDF) of the output image, where
CDF(i) is the total number of pixels with DN values less than or equal to i.
The CDF is computed from the histogram of the output image.
.VARIABLE AREA

	AREA=(sl,ss,nl,ns)

Four integers: (starting line,starting sample,number of lines,number of samples)
Defines the area of the input image used to calculate the histogram of the
input image.  For the histogram stretches, this histogram is used to compute
the transfer function.  However, the stretch is performed on the portion of the
image specified by the SIZE field.

If AREA is not specified, this area is the same as the SIZE field.  See also
parameter LINC.

.VARIABLE LINC

	LINC=n

Causes the histogram to be calculated from every nth line of the input image.
Default is LINC=1.  See also AREA parameter.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststretch.pdf
PROCEDURE
REFGBL $ECHO
refgbl $autousage
BODY
LET _ONFAIL="CONTINUE"
LET $ECHO="YES"
let $autousage = "no"

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!            ****Test for all functions on byte data ****                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 10 10			!Create a 10x10 byte image
list a

!.............Complementing the image........
stretch a b 'comp		!Test COMP options by comparing
stretch a c linear=(255,0)	!output with linear and
stretch a d table=(0,255,255,0) !table stretches.
difpic (b,c)			!Difference should be zero
difpic (b,d)			!Difference should be zero

!.............Linear stretches..........
stretch a b linear=(0,18)
list b
stretch a c perc=0 'include
difpic (b,c)			!Difference should be zero
histgen a h			!Compute histogram h
stretch (a,h) c perc=0 'include
difpic (b,c)			!Difference should be zero

!.............Piecewise linear histogram stretches........
stretch a c func="0.5*in1**2"	!Make the input image more interesting
list c
stretch c b 'mean 'include	!Put the mean in the middle
list b
stretch c b 'peak 'include	!Put the median in the middle
list b
stretch c b 'peak range=41
list b
stretch c b 'peak factor=2
list b

!.............Bimodal stretch (do not try this at home)........
stretch a b func="in1+18"	!Create image with bimodal histogram
concat (a,b) c ns=20 'nost
size c d zoom=10 'noin
stretch d e bimodal=0
hist e

!.............Bit clipping...............
stretch a b clip=6
list b
stretch a c func="mod(64*DN1,256)"
difpic (b,c)			!Difference should be zero

!.............Contouring...................
stretch a b contour=4 dnvalue=100
list b
stretch a c alarm=(0,4,8,12,16) dnvalue=100
difpic (b,c)			!Difference should be zero
stretch a c itable=(0,100,4,100,8,100,12,100,16,100)
difpic (b,c)			!Difference should be zero

!.............Logarithmic function...........
stretch a b log=(0,18)
stretch a c func="86.6039343332527*ln(IN1+1)"
difpic (b,c)			!Difference should be zero

!.............Sinusoidal function.............
stretch a b 'pstretch freq=16 phi=128 ampl=100 dc=49.5
list b
stretch a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
difpic (b,c)			!Difference should be zero
f2 a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
difpic (b,c)			!Difference should be zero

!................Histogram-matching stretches.....
stretch a b 'smooth
list b
stretch a b 'ellipse
list b
stretch a b power=1
list b
stretch a b 'gauss gsigma=5.0 'include
stretch b c func="dn+10"
stretch a b 'gauss gsigma=5.0 gmean=137.5 'include
difpic (b,c)		!Differences should be 0

!................Test histogram printing options..........
stretch a b 'ihist 'include
stretch a b 'ohist 'include
stretch a b 'icdf
stretch a b 'ocdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!            ****Test for all functions on halfword data ****              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gen a 10 10 'half ival=-9	!Generate a 10x10 halfword image
list a

!.............Complementing the image........
stretch a b 'comp
list b
stretch a c linear=(32767,-32767)
difpic (b,c)				!Difference should be zero
stretch a c table=(-32767,32767,32767,-32767)
difpic (b,c)				!Difference should be zero
stretch a c func="-in1"
difpic (b,c)				!Difference should be zero

!.............Linear stretches..........
stretch a b linear=(-9,9)
list b
stretch a c perc=0 'include
difpic (b,c)			!Difference should be zero

!.............Piecewise linear histogram stretches........
gen c 10 10 'half
stretch c d func="0.5*in1**2"	!Make the input image more interesting
list d
stretch d b 'mean		!Put the mean in the middle
list b
stretch d b 'peak		!Put the median in the middle
list b

!.............Bimodal stretch (do not try this at home)........
stretch c d func="in1+18"	!Create image with bimodal histogram
concat (c,d) b ns=20 'nost
size b c zoom=10 'noin
stretch c d bimodal=0
hist d

!.............Contouring...............
stretch a b clip=6
list b
stretch a b contour=4 dnvalue=100
list b
stretch a c alarm=(-8,-4,0,4,8) dnvalue=100
difpic (b,c)                    !Difference should be zero
stretch a c itable=(-8,100,-4,100,0,100,4,100,8,100)
difpic (b,c)                    !Difference should be zero

!................Various functions...........
stretch a b log=(-9,9) curve=10
list b
stretch a b 'pstretch freq=4096 ampl=100
list b

!................Histogram-matching stretches.....
stretch a b 'smooth
list b
stretch a b 'gauss gsigma=2.
list b
stretch a b 'ellipse
list b
stretch a b power=1
list b

!................Test include/exclude/cut........................
stretch a b rexclude=(-9,-1) 'include perc=0.
list b
stretch a c iexclude=(-9,-8,-7,-6,-5,-4,-3,-2,-1) 'include perc=0.
difpic (b,c)		!Differences should be 0
stretch a b cut=10 'include
list b

!................Test area and linc parameters....................
stretch a b area=(2,2,8,8) perc=0.
list b
stretch a b linc=2 perc=0.
list b

!................Test SIZE parameter.....
stretch a b perc=0 size=(2,2,8,8) 'include
list b

!......SS+NS-1 Outside picture....samples trucated
stretch a b perc=0 ss=3 ns=11

!
!
!
!  Testing 3D images
!
!
!

gen a 10 10 10		!Create a 10x10X10 byte image
list a

!.............Complementing the image........
stretch a b 'comp		!Test COMP options by comparing
stretch a c linear=(255,0)	!output with linear and
stretch a d table=(0,255,255,0) !table stretches.
difpic (b,c)			!Difference should be zero
difpic (b,d)			!Difference should be zero

!.............Linear stretches..........
stretch a b linear=(0,18)
list b NB=2
stretch a c perc=0 'include
difpic (b,c)			!Difference should be zero


!.............Piecewise linear histogram stretches........
stretch a c func="0.5*in1**2"	!Make the input image more interesting
list c NB=2
stretch c b 'mean 'include	!Put the mean in the middle
list b NB=2
stretch c b 'peak 'include	!Put the median in the middle
list b NB=2
stretch c b 'peak range=41
list b NB=2
stretch c b 'peak factor=2
list b NB=2

!.............Bimodal stretch (do not try this at home)........
stretch a b func="in1+18"	!Create image with bimodal histogram



!.............Bit clipping...............
stretch a b clip=6
list b NB=2
stretch a c func="mod(64*DN1,256)"
difpic (b,c)			!Difference should be zero

!.............Contouring...................
stretch a b contour=4 dnvalue=100
list b NB=2
stretch a c alarm=(0,4,8,12,16) dnvalue=100
difpic (b,c)			!Difference should be zero
stretch a c itable=(0,100,4,100,8,100,12,100,16,100)
difpic (b,c)			!Difference should be zero

!.............Logarithmic function...........
stretch a b log=(0,18)
stretch a c func="86.6039343332527*ln(IN1+1)"
difpic (b,c)			!Difference should be zero

!.............Sinusoidal function.............
stretch a b 'pstretch freq=16 phi=128 ampl=100 dc=49.5
list b NB=2
stretch a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
difpic (b,c)			!Difference should be zero
f2 a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
difpic (b,c)			!Difference should be zero

!................Histogram-matching stretches.....
stretch a b 'smooth
list b NB=2
stretch a b 'ellipse
list b NB=2
stretch a b power=1
list b NB=2
stretch a b 'gauss gsigma=5.0 'include
stretch b c func="dn+10"
stretch a b 'gauss gsigma=5.0 gmean=137.5 'include
difpic (b,c)		!Differences should be 0

!................Test histogram printing options..........
stretch a b 'ihist 'include
stretch a b 'ohist 'include
stretch a b 'icdf
stretch a b 'ocdf

! test the case shown in AR 114525
gen xxb 11 7 ival=100 linc=0 sinc=0 'half
qsar xxb xxa area=(2 2 1 5 -32668 4 2 1 5 -100 6 2 1 5 2330 +
 8 2 1 5 3995 10 2 1 5 32667)
list xxa size=(1 1 11 7)
stretch xxa xxc table=(-32768 -32768 -1 -32768 0 0 1 1000 2429 1000 +
 2430 2430 2431 1000 4094 1000 4095 4095 4096 32767 32767 32767)
list xxc size=(1 1 11 7)

end-proc
$!-----------------------------------------------------------------------------
$ create tststretch.log
tststretch
let $autousage = "no"
gen a 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
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
stretch a b 'comp
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** COMPLEMENT MODE ***
Complement Stretch:      0 to    255 and    255 to      0
stretch a c linear=(255,0)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:    255 to      0 and      0 to    255
stretch a d table=(0,255,255,0)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b linear=(0,18)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:      0 to      0 and     18 to    255
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:23 2013
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
stretch a c perc=0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:      0 to      0 and     18 to    255
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
histgen a h
Beginning VICAR task histgen
 HISTGEN version July 26, 1999
 HISTGEN task completed
stretch (a,h) c perc=0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:      0 to      0 and     18 to    255
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a c func="0.5*in1**2"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = 0.5*in1**2
list c
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:26 2013
     Samp     1       3       5       7       9
   Line
      1       0   1   2   5   8  13  18  25  32  41
      2       1   2   5   8  13  18  25  32  41  50
      3       2   5   8  13  18  25  32  41  50  61
      4       5   8  13  18  25  32  41  50  61  72
      5       8  13  18  25  32  41  50  61  72  85
      6      13  18  25  32  41  50  61  72  85  98
      7      18  25  32  41  50  61  72  85  98 113
      8      25  32  41  50  61  72  85  98 113 128
      9      32  41  50  61  72  85  98 113 128 145
     10      41  50  61  72  85  98 113 128 145 162
stretch c b 'mean 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    49.0000 Sigma=    37.8272
*** MEAN OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Mean Stretch:       0 to      0 and    146 to    255
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:27 2013
     Samp     1       3       5       7       9
   Line
      1       0   3   5  13  21  34  47  65  83 106
      2       3   5  13  21  34  47  65  83 106 128
      3       5  13  21  34  47  65  83 106 128 143
      4      13  21  34  47  65  83 106 128 143 157
      5      21  34  47  65  83 106 128 143 157 175
      6      34  47  65  83 106 128 143 157 175 192
      7      47  65  83 106 128 143 157 175 192 211
      8      65  83 106 128 143 157 175 192 211 231
      9      83 106 128 143 157 175 192 211 231 254
     10     106 128 143 157 175 192 211 231 254 255
stretch c b 'peak 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    49.0000 Sigma=    37.8272
*** PEAK OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Peak Stretch:       0 to      0 and    146 to    255
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:28 2013
     Samp     1       3       5       7       9
   Line
      1       0   3   6  15  25  40  56  77  99 127
      2       3   6  15  25  40  56  77  99 127 138
      3       6  15  25  40  56  77  99 127 138 151
      4      15  25  40  56  77  99 127 138 151 165
      5      25  40  56  77  99 127 138 151 165 181
      6      40  56  77  99 127 138 151 165 181 196
      7      56  77  99 127 138 151 165 181 196 215
      8      77  99 127 138 151 165 181 196 215 233
      9      99 127 138 151 165 181 196 215 233 254
     10     127 138 151 165 181 196 215 233 254 255
stretch c b 'peak range=41
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    49.4949 Sigma=    37.6942
*** PEAK OPTION ***
Peak Stretch:       0 to      0 and     82 to    255
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:28 2013
     Samp     1       3       5       7       9
   Line
      1       0   3   6  15  25  40  56  77  99 127
      2       3   6  15  25  40  56  77  99 127 155
      3       6  15  25  40  56  77  99 127 155 189
      4      15  25  40  56  77  99 127 155 189 224
      5      25  40  56  77  99 127 155 189 224 255
      6      40  56  77  99 127 155 189 224 255 255
      7      56  77  99 127 155 189 224 255 255 255
      8      77  99 127 155 189 224 255 255 255 255
      9      99 127 155 189 224 255 255 255 255 255
     10     127 155 189 224 255 255 255 255 255 255
stretch c b 'peak factor=2
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    49.4949 Sigma=    37.6942
*** PEAK OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Peak Stretch:       1 to      0 and    146 to    255
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:29 2013
     Samp     1       3       5       7       9
   Line
      1       0   0   2   8  14  24  34  48  62 127
      2       0   2   8  14  24  34  48  62 127 145
      3       2   8  14  24  34  48  62 127 145 167
      4       8  14  24  34  48  62 127 145 167 189
      5      14  24  34  48  62 127 145 167 189 215
      6      24  34  48  62 127 145 167 189 215 241
      7      34  48  62 127 145 167 189 215 241 255
      8      48  62 127 145 167 189 215 241 255 255
      9      62 127 145 167 189 215 241 255 255 255
     10     127 145 167 189 215 241 255 255 255 255
stretch a b func="in1+18"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = in1+18
concat (a,b) c ns=20 'nost
Beginning VICAR task concat
size c d zoom=10 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   20)
     OUTPUT SIZE=    100 X    200
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
stretch d e bimodal=0
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    18.0905 Sigma=     9.8160
Filter window NSW=   7
*** BIMODAL OPTIION ***
hist e
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***

Bin Width =        1.0
          0      300   ***************
          7*     300   ***************
         15*     400   ********************
         22*     500   *************************
         30*     600   ******************************
         37*     700   ***********************************
         45*     800   ****************************************
         52*     900   *********************************************
         60*    1000   **************************************************  1
         67*     900   *********************************************
         75*     800   ****************************************
         82*     700   ***********************************
         90*     600   ******************************
         97*     500   *************************
        105*     400   ********************
        112*     300   ***************
        120*     200   **********
        127*     200   **********
        128      200   **********
        135*     300   ***************
        143*     400   ********************
        150*     500   *************************
        158*     600   ******************************
        165*     700   ***********************************
        173*     800   ****************************************
        180*     900   *********************************************
        188*    1000   **************************************************  2
        195*     900   *********************************************
        203*     800   ****************************************
        210*     700   ***********************************
        218*     600   ******************************
        225*     500   *************************
        233*     400   ********************
        240*     300   ***************
        248*     200   **********
        255*     100   *****

AVERAGE GRAY LEVEL=123.8250
STANDARD DEVIATION=70.80921
NUMBER ELEMENTS=     20000
MIN. DN=         0
MAX. DN=       255

stretch a b clip=6
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** BIT CLIPPING MODE ***
Bit Clip Stretch:  6 Most  Significant Bits Clipped
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:33 2013
     Samp     1       3       5       7       9
   Line
      1       0  64 128 192   0  64 128 192   0  64
      2      64 128 192   0  64 128 192   0  64 128
      3     128 192   0  64 128 192   0  64 128 192
      4     192   0  64 128 192   0  64 128 192   0
      5       0  64 128 192   0  64 128 192   0  64
      6      64 128 192   0  64 128 192   0  64 128
      7     128 192   0  64 128 192   0  64 128 192
      8     192   0  64 128 192   0  64 128 192   0
      9       0  64 128 192   0  64 128 192   0  64
     10      64 128 192   0  64 128 192   0  64 128
stretch a c func="mod(64*DN1,256)"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = mod(64*DN1,256)
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b contour=4 dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** CONTOUR MODE ***
Contour Stretch: Interval =    4  DNvalue =    100
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:34 2013
     Samp     1       3       5       7       9
   Line
      1     100   1   2   3 100   5   6   7 100   9
      2       1   2   3 100   5   6   7 100   9  10
      3       2   3 100   5   6   7 100   9  10  11
      4       3 100   5   6   7 100   9  10  11 100
      5     100   5   6   7 100   9  10  11 100  13
      6       5   6   7 100   9  10  11 100  13  14
      7       6   7 100   9  10  11 100  13  14  15
      8       7 100   9  10  11 100  13  14  15 100
      9     100   9  10  11 100  13  14  15 100  17
     10       9  10  11 100  13  14  15 100  17  18
stretch a c alarm=(0,4,8,12,16) dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** ALARM MODE ***
Alarm Stretch: DNvalue =    100
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a c itable=(0,100,4,100,8,100,12,100,16,100)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** INDIVIDUAL DN TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b log=(0,18)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LOGARITHMIC STRETCH MODE ***
LOGARITHMIC Stretch:    0.0 to      0 and   18.0 to    255 with CURVE=   1.0
stretch a c func="86.6039343332527*ln(IN1+1)"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = 86.6039343332527*ln(IN1+1)
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b 'pstretch freq=16 phi=128 ampl=100 dc=49.5
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** PERIODIC STRETCH MODE ***
Periodic Stretch: FREQ= 16.00  DC=    49.5  AMPL=   100.0  PHI=128.00
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:38 2013
     Samp     1       3       5       7       9
   Line
      1      86  69  50  31  15   4   0   3  14  30
      2      69  50  31  15   4   0   3  14  30  49
      3      50  31  15   4   0   3  14  30  49  68
      4      31  15   4   0   3  14  30  49  68  85
      5      15   4   0   3  14  30  49  68  85  96
      6       4   0   3  14  30  49  68  85  96  99
      7       0   3  14  30  49  68  85  96  99  96
      8       3  14  30  49  68  85  96  99  96  85
      9      14  30  49  68  85  96  99  96  85  68
     10      30  49  68  85  96  99  96  85  68  49
stretch a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = (100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
f2 a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using byte table lookup
FUNCTION EVALUATED 256 TIMES
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b 'smooth
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** SMOOTH OPTION ***
Ramp CDF Stretch
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:41 2013
     Samp     1       3       5       7       9
   Line
      1       0   6  13  24  37  52  70  91 114 140
      2       6  13  24  37  52  70  91 114 140 163
      3      13  24  37  52  70  91 114 140 163 183
      4      24  37  52  70  91 114 140 163 183 201
      5      37  52  70  91 114 140 163 183 201 217
      6      52  70  91 114 140 163 183 201 217 230
      7      70  91 114 140 163 183 201 217 230 240
      8      91 114 140 163 183 201 217 230 240 248
      9     114 140 163 183 201 217 230 240 248 253
     10     140 163 183 201 217 230 240 248 253 255
stretch a b 'ellipse
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** ELLIPSE OPTION ***
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:41 2013
     Samp     1       3       5       7       9
   Line
      1       0  11  23  36  50  64  80  97 116 136
      2      11  23  36  50  64  80  97 116 136 155
      3      23  36  50  64  80  97 116 136 155 171
      4      36  50  64  80  97 116 136 155 171 187
      5      50  64  80  97 116 136 155 171 187 201
      6      64  80  97 116 136 155 171 187 201 214
      7      80  97 116 136 155 171 187 201 214 226
      8      97 116 136 155 171 187 201 214 226 236
      9     116 136 155 171 187 201 214 226 236 245
     10     136 155 171 187 201 214 226 236 245 251
stretch a b power=1
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** POWER LAW OPTION ***
Power Law Stretch: POWER =    1.00
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:20 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:42 2013
     Samp     1       3       5       7       9
   Line
      1       0  22  38  53  67  80  93 106 120 133
      2      22  38  53  67  80  93 106 120 133 146
      3      38  53  67  80  93 106 120 133 146 158
      4      53  67  80  93 106 120 133 146 158 171
      5      67  80  93 106 120 133 146 158 171 184
      6      80  93 106 120 133 146 158 171 184 196
      7      93 106 120 133 146 158 171 184 196 209
      8     106 120 133 146 158 171 184 196 209 221
      9     120 133 146 158 171 184 196 209 221 233
     10     133 146 158 171 184 196 209 221 233 242
stretch a b 'gauss gsigma=5.0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** GAUSS OPTION ***
Gaussian Stretch: GSIGMA =   5.00  GMEAN =   127.50
stretch b c func="dn+10"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = dn+10
stretch a b 'gauss gsigma=5.0 gmean=137.5 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** GAUSS OPTION ***
Gaussian Stretch: GSIGMA =   5.00  GMEAN =   137.50
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b 'ihist 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

INPUT HISTOGRAM
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         1   1.000     ***************     +         +         +         +         +         +         +         +        +   
   1         2   2.000     ***************************** +         +         +         +         +         +         +        +   
   2         3   3.000     *******************************************       +         +         +         +         +        +   
   3         4   4.000     **********************************************************  +         +         +         +        +   
   4         5   5.000     ************************************************************************        +         +        +   
   5         6   6.000     **************************************************************************************    +        +   
   6         7   7.000     ****************************************************************************************************   
   7         8   8.000     **************************************************************************************************** * 
   8         9   9.000     **************************************************************************************************** * 
   9        10  10.000     **************************************************************************************************** * 
  10         9   9.000     **************************************************************************************************** * 
  11         8   8.000     **************************************************************************************************** * 
  12         7   7.000     ****************************************************************************************************   
  13         6   6.000     **************************************************************************************    +        +   
  14         5   5.000     ************************************************************************        +         +        +   
  15         4   4.000     **********************************************************  +         +         +         +        +   
  16         3   3.000     *******************************************       +         +         +         +         +        +   
  17         2   2.000     ***************************** +         +         +         +         +         +         +        +   
  18         1   1.000     ***************     +         +         +         +         +         +         +         +        +   

Histogram of input image: Mean  =     9.0000 Sigma =     4.0620

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      0 to      0 and     18 to    255
stretch a b 'ohist 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     9.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      0 to      0 and     18 to    255

OUTPUT HISTOGRAM
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         1   1.000     ***************     +         +         +         +         +         +         +         +        +   

  14         2   2.000     ***************************** +         +         +         +         +         +         +        +   

  28         3   3.000     *******************************************       +         +         +         +         +        +   

  43         4   4.000     **********************************************************  +         +         +         +        +   

  57         5   5.000     ************************************************************************        +         +        +   

  71         6   6.000     **************************************************************************************    +        +   

  85         7   7.000     ****************************************************************************************************   

  99         8   8.000     **************************************************************************************************** * 

 113         9   9.000     **************************************************************************************************** * 

 128        10  10.000     **************************************************************************************************** * 

 142         9   9.000     **************************************************************************************************** * 

 156         8   8.000     **************************************************************************************************** * 

 170         7   7.000     ****************************************************************************************************   

 184         6   6.000     **************************************************************************************    +        +   

 198         5   5.000     ************************************************************************        +         +        +   

 213         4   4.000     **********************************************************  +         +         +         +        +   

 227         3   3.000     *******************************************       +         +         +         +         +        +   

 241         2   2.000     ***************************** +         +         +         +         +         +         +        +   

 255         1   1.000     ***************     +         +         +         +         +         +         +         +        +   
Histogram of output image: Mean=   127.5900 Sigma=    57.5608
stretch a b 'icdf
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

CUMULATIVE DISTRIBUTION FUNCTION
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         1   1.000     **        +         +         +         +         +         +         +         +         +        +   
   1         3   3.000     ****      +         +         +         +         +         +         +         +         +        +   
   2         6   6.000     *******   +         +         +         +         +         +         +         +         +        +   
   3        10  10.000     ***********         +         +         +         +         +         +         +         +        +   
   4        15  15.000     ****************    +         +         +         +         +         +         +         +        +   
   5        21  21.000     **********************        +         +         +         +         +         +         +        +   
   6        28  28.000     ***************************** +         +         +         +         +         +         +        +   
   7        36  36.000     *************************************   +         +         +         +         +         +        +   
   8        45  45.000     **********************************************    +         +         +         +         +        +   
   9        55  55.000     ********************************************************    +         +         +         +        +   
  10        64  64.000     *****************************************************************     +         +         +        +   
  11        72  72.000     *************************************************************************       +         +        +   
  12        79  79.000     ********************************************************************************+         +        +   
  13        85  85.000     **************************************************************************************    +        +   
  14        90  90.000     *******************************************************************************************        +   
  15        94  94.000     ***********************************************************************************************    +   
  16        97  97.000     ************************************************************************************************** +   
  17        99  99.000     ****************************************************************************************************   
  18       100 100.000     ****************************************************************************************************   
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      1 to      0 and     18 to    255
stretch a b 'ocdf
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=     9.0909 Sigma=     3.9800
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      1 to      0 and     18 to    255

OUTPUT CUMULATIVE DISTRIBUTION FUNCTION
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   1         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   2         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   3         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   4         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   5         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   6         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   7         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   8         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
   9         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  10         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  11         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  12         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  13         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  14         3   3.030     ****      +         +         +         +         +         +         +         +         +        +   
  15         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  16         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  17         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  18         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  19         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  20         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  21         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  22         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  23         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  24         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  25         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  26         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  27         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  28         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  29         6   6.061     *******   +         +         +         +         +         +         +         +         +        +   
  30        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  31        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  32        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  33        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  34        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  35        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  36        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  37        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  38        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  39        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  40        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  41        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  42        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  43        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  44        10  10.101     ***********         +         +         +         +         +         +         +         +        +   
  45        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  46        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  47        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  48        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  49        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  50        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  51        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  52        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  53        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  54        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  55        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  56        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  57        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  58        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  59        15  15.152     ****************    +         +         +         +         +         +         +         +        +   
  60        21  21.212     **********************        +         +         +         +         +         +         +        +   
  61        21  21.212     **********************        +         +         +         +         +         +         +        +   
  62        21  21.212     **********************        +         +         +         +         +         +         +        +   
  63        21  21.212     **********************        +         +         +         +         +         +         +        +   
  64        21  21.212     **********************        +         +         +         +         +         +         +        +   
  65        21  21.212     **********************        +         +         +         +         +         +         +        +   
  66        21  21.212     **********************        +         +         +         +         +         +         +        +   
  67        21  21.212     **********************        +         +         +         +         +         +         +        +   
  68        21  21.212     **********************        +         +         +         +         +         +         +        +   
  69        21  21.212     **********************        +         +         +         +         +         +         +        +   
  70        21  21.212     **********************        +         +         +         +         +         +         +        +   
  71        21  21.212     **********************        +         +         +         +         +         +         +        +   
  72        21  21.212     **********************        +         +         +         +         +         +         +        +   
  73        21  21.212     **********************        +         +         +         +         +         +         +        +   
  74        21  21.212     **********************        +         +         +         +         +         +         +        +   
  75        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  76        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  77        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  78        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  79        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  80        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  81        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  82        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  83        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  84        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  85        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  86        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  87        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  88        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  89        28  28.283     ***************************** +         +         +         +         +         +         +        +   
  90        36  36.364     *************************************   +         +         +         +         +         +        +   
  91        36  36.364     *************************************   +         +         +         +         +         +        +   
  92        36  36.364     *************************************   +         +         +         +         +         +        +   
  93        36  36.364     *************************************   +         +         +         +         +         +        +   
  94        36  36.364     *************************************   +         +         +         +         +         +        +   
  95        36  36.364     *************************************   +         +         +         +         +         +        +   
  96        36  36.364     *************************************   +         +         +         +         +         +        +   
  97        36  36.364     *************************************   +         +         +         +         +         +        +   
  98        36  36.364     *************************************   +         +         +         +         +         +        +   
  99        36  36.364     *************************************   +         +         +         +         +         +        +   
 100        36  36.364     *************************************   +         +         +         +         +         +        +   
 101        36  36.364     *************************************   +         +         +         +         +         +        +   
 102        36  36.364     *************************************   +         +         +         +         +         +        +   
 103        36  36.364     *************************************   +         +         +         +         +         +        +   
 104        36  36.364     *************************************   +         +         +         +         +         +        +   
 105        45  45.455     **********************************************    +         +         +         +         +        +   
 106        45  45.455     **********************************************    +         +         +         +         +        +   
 107        45  45.455     **********************************************    +         +         +         +         +        +   
 108        45  45.455     **********************************************    +         +         +         +         +        +   
 109        45  45.455     **********************************************    +         +         +         +         +        +   
 110        45  45.455     **********************************************    +         +         +         +         +        +   
 111        45  45.455     **********************************************    +         +         +         +         +        +   
 112        45  45.455     **********************************************    +         +         +         +         +        +   
 113        45  45.455     **********************************************    +         +         +         +         +        +   
 114        45  45.455     **********************************************    +         +         +         +         +        +   
 115        45  45.455     **********************************************    +         +         +         +         +        +   
 116        45  45.455     **********************************************    +         +         +         +         +        +   
 117        45  45.455     **********************************************    +         +         +         +         +        +   
 118        45  45.455     **********************************************    +         +         +         +         +        +   
 119        45  45.455     **********************************************    +         +         +         +         +        +   
 120        55  55.556     ********************************************************    +         +         +         +        +   
 121        55  55.556     ********************************************************    +         +         +         +        +   
 122        55  55.556     ********************************************************    +         +         +         +        +   
 123        55  55.556     ********************************************************    +         +         +         +        +   
 124        55  55.556     ********************************************************    +         +         +         +        +   
 125        55  55.556     ********************************************************    +         +         +         +        +   
 126        55  55.556     ********************************************************    +         +         +         +        +   
 127        55  55.556     ********************************************************    +         +         +         +        +   
 128        55  55.556     ********************************************************    +         +         +         +        +   
 129        55  55.556     ********************************************************    +         +         +         +        +   
 130        55  55.556     ********************************************************    +         +         +         +        +   
 131        55  55.556     ********************************************************    +         +         +         +        +   
 132        55  55.556     ********************************************************    +         +         +         +        +   
 133        55  55.556     ********************************************************    +         +         +         +        +   
 134        55  55.556     ********************************************************    +         +         +         +        +   
 135        64  64.646     *****************************************************************     +         +         +        +   
 136        64  64.646     *****************************************************************     +         +         +        +   
 137        64  64.646     *****************************************************************     +         +         +        +   
 138        64  64.646     *****************************************************************     +         +         +        +   
 139        64  64.646     *****************************************************************     +         +         +        +   
 140        64  64.646     *****************************************************************     +         +         +        +   
 141        64  64.646     *****************************************************************     +         +         +        +   
 142        64  64.646     *****************************************************************     +         +         +        +   
 143        64  64.646     *****************************************************************     +         +         +        +   
 144        64  64.646     *****************************************************************     +         +         +        +   
 145        64  64.646     *****************************************************************     +         +         +        +   
 146        64  64.646     *****************************************************************     +         +         +        +   
 147        64  64.646     *****************************************************************     +         +         +        +   
 148        64  64.646     *****************************************************************     +         +         +        +   
 149        64  64.646     *****************************************************************     +         +         +        +   
 150        72  72.727     *************************************************************************       +         +        +   
 151        72  72.727     *************************************************************************       +         +        +   
 152        72  72.727     *************************************************************************       +         +        +   
 153        72  72.727     *************************************************************************       +         +        +   
 154        72  72.727     *************************************************************************       +         +        +   
 155        72  72.727     *************************************************************************       +         +        +   
 156        72  72.727     *************************************************************************       +         +        +   
 157        72  72.727     *************************************************************************       +         +        +   
 158        72  72.727     *************************************************************************       +         +        +   
 159        72  72.727     *************************************************************************       +         +        +   
 160        72  72.727     *************************************************************************       +         +        +   
 161        72  72.727     *************************************************************************       +         +        +   
 162        72  72.727     *************************************************************************       +         +        +   
 163        72  72.727     *************************************************************************       +         +        +   
 164        72  72.727     *************************************************************************       +         +        +   
 165        79  79.798     ********************************************************************************+         +        +   
 166        79  79.798     ********************************************************************************+         +        +   
 167        79  79.798     ********************************************************************************+         +        +   
 168        79  79.798     ********************************************************************************+         +        +   
 169        79  79.798     ********************************************************************************+         +        +   
 170        79  79.798     ********************************************************************************+         +        +   
 171        79  79.798     ********************************************************************************+         +        +   
 172        79  79.798     ********************************************************************************+         +        +   
 173        79  79.798     ********************************************************************************+         +        +   
 174        79  79.798     ********************************************************************************+         +        +   
 175        79  79.798     ********************************************************************************+         +        +   
 176        79  79.798     ********************************************************************************+         +        +   
 177        79  79.798     ********************************************************************************+         +        +   
 178        79  79.798     ********************************************************************************+         +        +   
 179        79  79.798     ********************************************************************************+         +        +   
 180        85  85.859     **************************************************************************************    +        +   
 181        85  85.859     **************************************************************************************    +        +   
 182        85  85.859     **************************************************************************************    +        +   
 183        85  85.859     **************************************************************************************    +        +   
 184        85  85.859     **************************************************************************************    +        +   
 185        85  85.859     **************************************************************************************    +        +   
 186        85  85.859     **************************************************************************************    +        +   
 187        85  85.859     **************************************************************************************    +        +   
 188        85  85.859     **************************************************************************************    +        +   
 189        85  85.859     **************************************************************************************    +        +   
 190        85  85.859     **************************************************************************************    +        +   
 191        85  85.859     **************************************************************************************    +        +   
 192        85  85.859     **************************************************************************************    +        +   
 193        85  85.859     **************************************************************************************    +        +   
 194        85  85.859     **************************************************************************************    +        +   
 195        90  90.909     *******************************************************************************************        +   
 196        90  90.909     *******************************************************************************************        +   
 197        90  90.909     *******************************************************************************************        +   
 198        90  90.909     *******************************************************************************************        +   
 199        90  90.909     *******************************************************************************************        +   
 200        90  90.909     *******************************************************************************************        +   
 201        90  90.909     *******************************************************************************************        +   
 202        90  90.909     *******************************************************************************************        +   
 203        90  90.909     *******************************************************************************************        +   
 204        90  90.909     *******************************************************************************************        +   
 205        90  90.909     *******************************************************************************************        +   
 206        90  90.909     *******************************************************************************************        +   
 207        90  90.909     *******************************************************************************************        +   
 208        90  90.909     *******************************************************************************************        +   
 209        90  90.909     *******************************************************************************************        +   
 210        94  94.949     ***********************************************************************************************    +   
 211        94  94.949     ***********************************************************************************************    +   
 212        94  94.949     ***********************************************************************************************    +   
 213        94  94.949     ***********************************************************************************************    +   
 214        94  94.949     ***********************************************************************************************    +   
 215        94  94.949     ***********************************************************************************************    +   
 216        94  94.949     ***********************************************************************************************    +   
 217        94  94.949     ***********************************************************************************************    +   
 218        94  94.949     ***********************************************************************************************    +   
 219        94  94.949     ***********************************************************************************************    +   
 220        94  94.949     ***********************************************************************************************    +   
 221        94  94.949     ***********************************************************************************************    +   
 222        94  94.949     ***********************************************************************************************    +   
 223        94  94.949     ***********************************************************************************************    +   
 224        94  94.949     ***********************************************************************************************    +   
 225        97  97.980     ************************************************************************************************** +   
 226        97  97.980     ************************************************************************************************** +   
 227        97  97.980     ************************************************************************************************** +   
 228        97  97.980     ************************************************************************************************** +   
 229        97  97.980     ************************************************************************************************** +   
 230        97  97.980     ************************************************************************************************** +   
 231        97  97.980     ************************************************************************************************** +   
 232        97  97.980     ************************************************************************************************** +   
 233        97  97.980     ************************************************************************************************** +   
 234        97  97.980     ************************************************************************************************** +   
 235        97  97.980     ************************************************************************************************** +   
 236        97  97.980     ************************************************************************************************** +   
 237        97  97.980     ************************************************************************************************** +   
 238        97  97.980     ************************************************************************************************** +   
 239        97  97.980     ************************************************************************************************** +   
 240        99 100.000     ****************************************************************************************************   
 241        99 100.000     ****************************************************************************************************   
 242        99 100.000     ****************************************************************************************************   
 243        99 100.000     ****************************************************************************************************   
 244        99 100.000     ****************************************************************************************************   
 245        99 100.000     ****************************************************************************************************   
 246        99 100.000     ****************************************************************************************************   
 247        99 100.000     ****************************************************************************************************   
 248        99 100.000     ****************************************************************************************************   
 249        99 100.000     ****************************************************************************************************   
 250        99 100.000     ****************************************************************************************************   
 251        99 100.000     ****************************************************************************************************   
 252        99 100.000     ****************************************************************************************************   
 253        99 100.000     ****************************************************************************************************   
 254        99 100.000     ****************************************************************************************************   
 255       100 101.010     ****************************************************************************************************   
gen a 10 10 'half ival=-9
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        -9    -8    -7    -6    -5    -4    -3    -2    -1     0
      2        -8    -7    -6    -5    -4    -3    -2    -1     0     1
      3        -7    -6    -5    -4    -3    -2    -1     0     1     2
      4        -6    -5    -4    -3    -2    -1     0     1     2     3
      5        -5    -4    -3    -2    -1     0     1     2     3     4
      6        -4    -3    -2    -1     0     1     2     3     4     5
      7        -3    -2    -1     0     1     2     3     4     5     6
      8        -2    -1     0     1     2     3     4     5     6     7
      9        -1     0     1     2     3     4     5     6     7     8
     10         0     1     2     3     4     5     6     7     8     9
stretch a b 'comp
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** COMPLEMENT MODE ***
Complement Stretch: -32768 to  32767 and  32767 to -32768
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         8     7     6     5     4     3     2     1     0    -1
      2         7     6     5     4     3     2     1     0    -1    -2
      3         6     5     4     3     2     1     0    -1    -2    -3
      4         5     4     3     2     1     0    -1    -2    -3    -4
      5         4     3     2     1     0    -1    -2    -3    -4    -5
      6         3     2     1     0    -1    -2    -3    -4    -5    -6
      7         2     1     0    -1    -2    -3    -4    -5    -6    -7
      8         1     0    -1    -2    -3    -4    -5    -6    -7    -8
      9         0    -1    -2    -3    -4    -5    -6    -7    -8    -9
     10        -1    -2    -3    -4    -5    -6    -7    -8    -9   -10
stretch a c linear=(32767,-32767)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:  32767 to -32768 and -32767 to  32767
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =  45
stretch a c table=(-32767,32767,32767,-32767)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES = 100
stretch a c func="-in1"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = -in1
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES = 100
stretch a b linear=(-9,9)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:     -9 to -32768 and      9 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:51 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-29127-25486-21846-18205-14564-10923 -7282 -3641    -1
      2    -29127-25486-21846-18205-14564-10923 -7282 -3641    -1  3640
      3    -25486-21846-18205-14564-10923 -7282 -3641    -1  3640  7281
      4    -21846-18205-14564-10923 -7282 -3641    -1  3640  7281 10922
      5    -18205-14564-10923 -7282 -3641    -1  3640  7281 10922 14563
      6    -14564-10923 -7282 -3641    -1  3640  7281 10922 14563 18204
      7    -10923 -7282 -3641    -1  3640  7281 10922 14563 18204 21845
      8     -7282 -3641    -1  3640  7281 10922 14563 18204 21845 25485
      9     -3641    -1  3640  7281 10922 14563 18204 21845 25485 29126
     10        -1  3640  7281 10922 14563 18204 21845 25485 29126 32767
stretch a c perc=0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:     -9 to -32768 and      9 to  32767
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
gen c 10 10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
stretch c d func="0.5*in1**2"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = 0.5*in1**2
list d
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:52 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:52 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     5     8    13    18    25    32    41
      2         1     2     5     8    13    18    25    32    41    50
      3         2     5     8    13    18    25    32    41    50    61
      4         5     8    13    18    25    32    41    50    61    72
      5         8    13    18    25    32    41    50    61    72    85
      6        13    18    25    32    41    50    61    72    85    98
      7        18    25    32    41    50    61    72    85    98   113
      8        25    32    41    50    61    72    85    98   113   128
      9        32    41    50    61    72    85    98   113   128   145
     10        41    50    61    72    85    98   113   128   145   162
stretch d b 'mean
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=    49.0000 Sigma=    37.8272
*** MEAN OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Mean Stretch:       0 to -32768 and    146 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:52 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:53 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-32099-31431-29424-27418-24074-20731-16050-11368 -5350
      2    -32099-31431-29424-27418-24074-20731-16050-11368 -5350   338
      3    -31431-29424-27418-24074-20731-16050-11368 -5350   338  4054
      4    -29424-27418-24074-20731-16050-11368 -5350   338  4054  7769
      5    -27418-24074-20731-16050-11368 -5350   338  4054  7769 12161
      6    -24074-20731-16050-11368 -5350   338  4054  7769 12161 16552
      7    -20731-16050-11368 -5350   338  4054  7769 12161 16552 21619
      8    -16050-11368 -5350   338  4054  7769 12161 16552 21619 26687
      9    -11368 -5350   338  4054  7769 12161 16552 21619 26687 32429
     10     -5350   338  4054  7769 12161 16552 21619 26687 32429 32767
stretch d b 'peak
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=    49.0000 Sigma=    37.8272
*** PEAK OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Peak Stretch:       0 to -32768 and    146 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:52 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:54 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-31969-31170-28772-26374-22378-18382-12788 -7193     0
      2    -31969-31170-28772-26374-22378-18382-12788 -7193     0  2809
      3    -31170-28772-26374-22378-18382-12788 -7193     0  2809  6241
      4    -28772-26374-22378-18382-12788 -7193     0  2809  6241  9674
      5    -26374-22378-18382-12788 -7193     0  2809  6241  9674 13731
      6    -22378-18382-12788 -7193     0  2809  6241  9674 13731 17788
      7    -18382-12788 -7193     0  2809  6241  9674 13731 17788 22469
      8    -12788 -7193     0  2809  6241  9674 13731 17788 22469 27150
      9     -7193     0  2809  6241  9674 13731 17788 22469 27150 32455
     10         0  2809  6241  9674 13731 17788 22469 27150 32455 32767
stretch c d func="in1+18"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = in1+18
concat (c,d) b ns=20 'nost
Beginning VICAR task concat
size b c zoom=10 'noin
Beginning VICAR task size
 SIZE version 18-Jul-2012
      INPUT AREA=(    1,    1,   10,   20)
     OUTPUT SIZE=    100 X    200
 PICTURE SIZE SCALED BY     10*NL,     10*NS
 SIZE task completed
stretch c d bimodal=0
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    18.0905 Sigma=     9.8160
Filter window NSW=   7
*** BIMODAL OPTIION ***
hist d
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***

Bin Width =        1.0
          0      300   ***************
          7*     300   ***************
         15*     400   ********************
         22*     500   *************************
         30*     600   ******************************
         37*     700   ***********************************
         45*     800   ****************************************
         52*     900   *********************************************
         60*    1000   **************************************************  1
         67*     900   *********************************************
         75*     800   ****************************************
         82*     700   ***********************************
         90*     600   ******************************
         97*     500   *************************
        105*     400   ********************
        112*     300   ***************
        120*     200   **********
        127*     200   **********
        128      200   **********
        135*     300   ***************
        143*     400   ********************
        150*     500   *************************
        158*     600   ******************************
        165*     700   ***********************************
        173*     800   ****************************************
        180*     900   *********************************************
        188*    1000   **************************************************  2
        195*     900   *********************************************
        203*     800   ****************************************
        210*     700   ***********************************
        218*     600   ******************************
        225*     500   *************************
        233*     400   ********************
        240*     300   ***************
        248*     200   **********
        255*     100   *****

AVERAGE GRAY LEVEL=123.8250
STANDARD DEVIATION=70.80921
NUMBER ELEMENTS=     20000
MIN. DN=         0
MAX. DN=       255

stretch a b clip=6
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** BIT CLIPPING MODE ***
Bit Clip Stretch:  6 Most  Significant Bits Clipped
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:57 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1      -576  -512  -448  -384  -320  -256  -192  -128   -64     0
      2      -512  -448  -384  -320  -256  -192  -128   -64     0    64
      3      -448  -384  -320  -256  -192  -128   -64     0    64   128
      4      -384  -320  -256  -192  -128   -64     0    64   128   192
      5      -320  -256  -192  -128   -64     0    64   128   192   256
      6      -256  -192  -128   -64     0    64   128   192   256   320
      7      -192  -128   -64     0    64   128   192   256   320   384
      8      -128   -64     0    64   128   192   256   320   384   448
      9       -64     0    64   128   192   256   320   384   448   512
     10         0    64   128   192   256   320   384   448   512   576
stretch a b contour=4 dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** CONTOUR MODE ***
Contour Stretch: Interval =    4  DNvalue =    100
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:02:58 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        -9   100    -7    -6    -5   100    -3    -2    -1   100
      2       100    -7    -6    -5   100    -3    -2    -1   100     1
      3        -7    -6    -5   100    -3    -2    -1   100     1     2
      4        -6    -5   100    -3    -2    -1   100     1     2     3
      5        -5   100    -3    -2    -1   100     1     2     3   100
      6       100    -3    -2    -1   100     1     2     3   100     5
      7        -3    -2    -1   100     1     2     3   100     5     6
      8        -2    -1   100     1     2     3   100     5     6     7
      9        -1   100     1     2     3   100     5     6     7   100
     10       100     1     2     3   100     5     6     7   100     9
stretch a c alarm=(-8,-4,0,4,8) dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** ALARM MODE ***
Alarm Stretch: DNvalue =    100
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a c itable=(-8,100,-4,100,0,100,4,100,8,100)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** INDIVIDUAL DN TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b log=(-9,9) curve=10
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LOGARITHMIC STRETCH MODE ***
LOGARITHMIC Stretch:   -9.0 to -32768 and    9.0 to  32767 with CURVE=   10.
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:01 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-17340 -8316 -1913  3054  7112 10543 13515 16136 18481
      2    -17340 -8316 -1913  3054  7112 10543 13515 16136 18481 20602
      3     -8316 -1913  3054  7112 10543 13515 16136 18481 20602 22539
      4     -1913  3054  7112 10543 13515 16136 18481 20602 22539 24321
      5      3054  7112 10543 13515 16136 18481 20602 22539 24321 25970
      6      7112 10543 13515 16136 18481 20602 22539 24321 25970 27506
      7     10543 13515 16136 18481 20602 22539 24321 25970 27506 28942
      8     13515 16136 18481 20602 22539 24321 25970 27506 28942 30291
      9     16136 18481 20602 22539 24321 25970 27506 28942 30291 31564
     10     18481 20602 22539 24321 25970 27506 28942 30291 31564 32767
stretch a b 'pstretch freq=4096 ampl=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** PERIODIC STRETCH MODE ***
Periodic Stretch: FREQ=******  DC=    -0.5  AMPL=   100.0  PHI=  0.00
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:01 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        19     0   -20   -36   -47   -51   -47   -36   -20    -1
      2         0   -20   -36   -47   -51   -47   -36   -20    -1    19
      3       -20   -36   -47   -51   -47   -36   -20    -1    19    35
      4       -36   -47   -51   -47   -36   -20    -1    19    35    46
      5       -47   -51   -47   -36   -20    -1    19    35    46    50
      6       -51   -47   -36   -20    -1    19    35    46    50    46
      7       -47   -36   -20    -1    19    35    46    50    46    35
      8       -36   -20    -1    19    35    46    50    46    35    19
      9       -20    -1    19    35    46    50    46    35    19    -1
     10        -1    19    35    46    50    46    35    19    -1   -20
stretch a b 'smooth
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** SMOOTH OPTION ***
Ramp CDF Stretch
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:02 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32112-30801-28835-26214-22937-19005-14418 -9175 -3277  3277
      2    -30801-28835-26214-22937-19005-14418 -9175 -3277  3277  9175
      3    -28835-26214-22937-19005-14418 -9175 -3277  3277  9175 14418
      4    -26214-22937-19005-14418 -9175 -3277  3277  9175 14418 19005
      5    -22937-19005-14418 -9175 -3277  3277  9175 14418 19005 22937
      6    -19005-14418 -9175 -3277  3277  9175 14418 19005 22937 26214
      7    -14418 -9175 -3277  3277  9175 14418 19005 22937 26214 28835
      8     -9175 -3277  3277  9175 14418 19005 22937 26214 28835 30801
      9     -3277  3277  9175 14418 19005 22937 26214 28835 30801 32112
     10      3277  9175 14418 19005 22937 26214 28835 30801 32112 32767
stretch a b 'gauss gsigma=2.
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** GAUSS OPTION ***
Gaussian Stretch: GSIGMA =   2.00  GMEAN =    -0.50
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:03 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -31433-27503-23556-19800-16215-12739 -9295 -5803 -2162  1768
      2    -27503-23556-19800-16215-12739 -9295 -5803 -2162  1768  5388
      3    -23556-19800-16215-12739 -9295 -5803 -2162  1768  5388  8839
      4    -19800-16215-12739 -9295 -5803 -2162  1768  5388  8839 12215
      5    -16215-12739 -9295 -5803 -2162  1768  5388  8839 12215 15588
      6    -12739 -9295 -5803 -2162  1768  5388  8839 12215 15588 19010
      7     -9295 -5803 -2162  1768  5388  8839 12215 15588 19010 22504
      8     -5803 -2162  1768  5388  8839 12215 15588 19010 22504 26015
      9     -2162  1768  5388  8839 12215 15588 19010 22504 26015 29257
     10      1768  5388  8839 12215 15588 19010 22504 26015 29257 31432
stretch a b 'ellipse
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** ELLIPSE OPTION ***
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:03 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1       850  2500  4252  6158  8219 10431 12792 15302 17964 20785
      2      2500  4252  6158  8219 10431 12792 15302 17964 20785 23238
      3      4252  6158  8219 10431 12792 15302 17964 20785 23238 25370
      4      6158  8219 10431 12792 15302 17964 20785 23238 25370 27208
      5      8219 10431 12792 15302 17964 20785 23238 25370 27208 28769
      6     10431 12792 15302 17964 20785 23238 25370 27208 28769 30063
      7     12792 15302 17964 20785 23238 25370 27208 28769 30063 31094
      8     15302 17964 20785 23238 25370 27208 28769 30063 31094 31867
      9     17964 20785 23238 25370 27208 28769 30063 31094 31867 32382
     10     20785 23238 25370 27208 28769 30063 31094 31867 32382 32639
stretch a b power=1
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** POWER LAW OPTION ***
Power Law Stretch: POWER =    1.00
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:04 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -29491-25441-21900-18485-15122-11786 -8467 -5157 -1855  1509
      2    -25441-21900-18485-15122-11786 -8467 -5157 -1855  1509  4771
      3    -21900-18485-15122-11786 -8467 -5157 -1855  1509  4771  8028
      4    -18485-15122-11786 -8467 -5157 -1855  1509  4771  8028 11280
      5    -15122-11786 -8467 -5157 -1855  1509  4771  8028 11280 14523
      6    -11786 -8467 -5157 -1855  1509  4771  8028 11280 14523 17752
      7     -8467 -5157 -1855  1509  4771  8028 11280 14523 17752 20954
      8     -5157 -1855  1509  4771  8028 11280 14523 17752 20954 24099
      9     -1855  1509  4771  8028 11280 14523 17752 20954 24099 27094
     10      1509  4771  8028 11280 14523 17752 20954 24099 27094 29495
stretch a b rexclude=(-9,-1) 'include perc=0.
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

DN values     -9 thru     -1 excluded

Histogram after exclusion: Mean=     3.0000 Sigma=     2.4495
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:      0 to -32768 and      9 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:05 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
      2    -32768-32768-32768-32768-32768-32768-32768-32768-32768-25486
      3    -32768-32768-32768-32768-32768-32768-32768-32768-25486-18205
      4    -32768-32768-32768-32768-32768-32768-32768-25486-18205-10923
      5    -32768-32768-32768-32768-32768-32768-25486-18205-10923 -3641
      6    -32768-32768-32768-32768-32768-25486-18205-10923 -3641  3640
      7    -32768-32768-32768-32768-25486-18205-10923 -3641  3640 10922
      8    -32768-32768-32768-25486-18205-10923 -3641  3640 10922 18204
      9    -32768-32768-25486-18205-10923 -3641  3640 10922 18204 25485
     10    -32768-25486-18205-10923 -3641  3640 10922 18204 25485 32767
stretch a c iexclude=(-9,-8,-7,-6,-5,-4,-3,-2,-1) 'include perc=0.
Beginning VICAR task stretch
STRETCH version 11 Jan 2013


Histogram after exclusion: Mean=     3.0000 Sigma=     2.4495
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:      0 to -32768 and      9 to  32767
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b cut=10 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620

Histogram after exclusion: Mean=     0.0000 Sigma=     4.0620
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:     -9 to -32768 and      9 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:06 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-29127-25486-21846-18205-14564-10923 -7282 -3641    -1
      2    -29127-25486-21846-18205-14564-10923 -7282 -3641    -1  3640
      3    -25486-21846-18205-14564-10923 -7282 -3641    -1  3640  7281
      4    -21846-18205-14564-10923 -7282 -3641    -1  3640  7281 10922
      5    -18205-14564-10923 -7282 -3641    -1  3640  7281 10922 14563
      6    -14564-10923 -7282 -3641    -1  3640  7281 10922 14563 18204
      7    -10923 -7282 -3641    -1  3640  7281 10922 14563 18204 21845
      8     -7282 -3641    -1  3640  7281 10922 14563 18204 21845 25485
      9     -3641    -1  3640  7281 10922 14563 18204 21845 25485 29126
     10        -1  3640  7281 10922 14563 18204 21845 25485 29126 32767
stretch a b area=(2,2,8,8) perc=0.
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
Histogram computed from sub-area
(SL,SS,NL,NS) = (    2,    2,    8,    8)
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     0.0000 Sigma=     3.2404
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:     -7 to -32768 and      7 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:07 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-32768-32768-28087-23406-18725-14044 -9363 -4682    -1
      2    -32768-32768-28087-23406-18725-14044 -9363 -4682    -1  4681
      3    -32768-28087-23406-18725-14044 -9363 -4682    -1  4681  9362
      4    -28087-23406-18725-14044 -9363 -4682    -1  4681  9362 14043
      5    -23406-18725-14044 -9363 -4682    -1  4681  9362 14043 18724
      6    -18725-14044 -9363 -4682    -1  4681  9362 14043 18724 23405
      7    -14044 -9363 -4682    -1  4681  9362 14043 18724 23405 28086
      8     -9363 -4682    -1  4681  9362 14043 18724 23405 28086 32767
      9     -4682    -1  4681  9362 14043 18724 23405 28086 32767 32767
     10        -1  4681  9362 14043 18724 23405 28086 32767 32767 32767
stretch a b linc=2 perc=0.
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=    -0.5000 Sigma=     4.0311
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:     -9 to -32768 and      8 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:08 2013
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-28913-25058-21203-17348-13493 -9638 -5783 -1928  1927
      2    -28913-25058-21203-17348-13493 -9638 -5783 -1928  1927  5782
      3    -25058-21203-17348-13493 -9638 -5783 -1928  1927  5782  9637
      4    -21203-17348-13493 -9638 -5783 -1928  1927  5782  9637 13492
      5    -17348-13493 -9638 -5783 -1928  1927  5782  9637 13492 17347
      6    -13493 -9638 -5783 -1928  1927  5782  9637 13492 17347 21202
      7     -9638 -5783 -1928  1927  5782  9637 13492 17347 21202 25057
      8     -5783 -1928  1927  5782  9637 13492 17347 21202 25057 28912
      9     -1928  1927  5782  9637 13492 17347 21202 25057 28912 32767
     10      1927  5782  9637 13492 17347 21202 25057 28912 32767 32767
stretch a b perc=0 size=(2,2,8,8) 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=     0.0000 Sigma=     3.2404
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:     -7 to -32768 and      7 to  32767
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:02:47 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:08 2013
     Samp       1     2     3     4     5     6     7     8
   Line
      1    -32768-28087-23406-18725-14044 -9363 -4682    -1
      2    -28087-23406-18725-14044 -9363 -4682    -1  4681
      3    -23406-18725-14044 -9363 -4682    -1  4681  9362
      4    -18725-14044 -9363 -4682    -1  4681  9362 14043
      5    -14044 -9363 -4682    -1  4681  9362 14043 18724
      6     -9363 -4682    -1  4681  9362 14043 18724 23405
      7     -4682    -1  4681  9362 14043 18724 23405 28086
      8        -1  4681  9362 14043 18724 23405 28086 32767
stretch a b perc=0 ss=3 ns=11
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
***Number of samples truncated
DN values -32768 and  32767 excluded

Histogram after exclusion: Mean=     1.0000 Sigma=     3.6742
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:     -7 to -32768 and      9 to  32767
gen a 10 10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 ***********
 Band =     6
 ***********
     Samp     1       3       5       7       9
   Line
      1       5   6   7   8   9  10  11  12  13  14
      2       6   7   8   9  10  11  12  13  14  15
      3       7   8   9  10  11  12  13  14  15  16
      4       8   9  10  11  12  13  14  15  16  17
      5       9  10  11  12  13  14  15  16  17  18
      6      10  11  12  13  14  15  16  17  18  19
      7      11  12  13  14  15  16  17  18  19  20
      8      12  13  14  15  16  17  18  19  20  21
      9      13  14  15  16  17  18  19  20  21  22
     10      14  15  16  17  18  19  20  21  22  23


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 ***********
 Band =     7
 ***********
     Samp     1       3       5       7       9
   Line
      1       6   7   8   9  10  11  12  13  14  15
      2       7   8   9  10  11  12  13  14  15  16
      3       8   9  10  11  12  13  14  15  16  17
      4       9  10  11  12  13  14  15  16  17  18
      5      10  11  12  13  14  15  16  17  18  19
      6      11  12  13  14  15  16  17  18  19  20
      7      12  13  14  15  16  17  18  19  20  21
      8      13  14  15  16  17  18  19  20  21  22
      9      14  15  16  17  18  19  20  21  22  23
     10      15  16  17  18  19  20  21  22  23  24


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 ***********
 Band =     8
 ***********
     Samp     1       3       5       7       9
   Line
      1       7   8   9  10  11  12  13  14  15  16
      2       8   9  10  11  12  13  14  15  16  17
      3       9  10  11  12  13  14  15  16  17  18
      4      10  11  12  13  14  15  16  17  18  19
      5      11  12  13  14  15  16  17  18  19  20
      6      12  13  14  15  16  17  18  19  20  21
      7      13  14  15  16  17  18  19  20  21  22
      8      14  15  16  17  18  19  20  21  22  23
      9      15  16  17  18  19  20  21  22  23  24
     10      16  17  18  19  20  21  22  23  24  25


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 ***********
 Band =     9
 ***********
     Samp     1       3       5       7       9
   Line
      1       8   9  10  11  12  13  14  15  16  17
      2       9  10  11  12  13  14  15  16  17  18
      3      10  11  12  13  14  15  16  17  18  19
      4      11  12  13  14  15  16  17  18  19  20
      5      12  13  14  15  16  17  18  19  20  21
      6      13  14  15  16  17  18  19  20  21  22
      7      14  15  16  17  18  19  20  21  22  23
      8      15  16  17  18  19  20  21  22  23  24
      9      16  17  18  19  20  21  22  23  24  25
     10      17  18  19  20  21  22  23  24  25  26


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 ***********
 Band =    10
 ***********
     Samp     1       3       5       7       9
   Line
      1       9  10  11  12  13  14  15  16  17  18
      2      10  11  12  13  14  15  16  17  18  19
      3      11  12  13  14  15  16  17  18  19  20
      4      12  13  14  15  16  17  18  19  20  21
      5      13  14  15  16  17  18  19  20  21  22
      6      14  15  16  17  18  19  20  21  22  23
      7      15  16  17  18  19  20  21  22  23  24
      8      16  17  18  19  20  21  22  23  24  25
      9      17  18  19  20  21  22  23  24  25  26
     10      18  19  20  21  22  23  24  25  26  27
stretch a b 'comp
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** COMPLEMENT MODE ***
Complement Stretch:      0 to    255 and    255 to      0
stretch a c linear=(255,0)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:    255 to      0 and      0 to    255
stretch a d table=(0,255,255,0)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
difpic (b,d)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b linear=(0,18)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LINEAR CONTRAST STRETCH MODE ***
Linear Stretch:      0 to      0 and     18 to    255
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:12 2013
 ***********
 Band =     1
 ***********
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


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:12 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1      14  28  43  57  71  85  99 113 128 142
      2      28  43  57  71  85  99 113 128 142 156
      3      43  57  71  85  99 113 128 142 156 170
      4      57  71  85  99 113 128 142 156 170 184
      5      71  85  99 113 128 142 156 170 184 198
      6      85  99 113 128 142 156 170 184 198 213
      7      99 113 128 142 156 170 184 198 213 227
      8     113 128 142 156 170 184 198 213 227 241
      9     128 142 156 170 184 198 213 227 241 255
     10     142 156 170 184 198 213 227 241 255 255
stretch a c perc=0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    13.5000 Sigma=     4.9749
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  0.00 at high end=  0.00
AUTO-STRETCH:      0 to      0 and     27 to    255
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES = 998
stretch a c func="0.5*in1**2"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = 0.5*in1**2
list c NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:14 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   2   5   8  13  18  25  32  41
      2       1   2   5   8  13  18  25  32  41  50
      3       2   5   8  13  18  25  32  41  50  61
      4       5   8  13  18  25  32  41  50  61  72
      5       8  13  18  25  32  41  50  61  72  85
      6      13  18  25  32  41  50  61  72  85  98
      7      18  25  32  41  50  61  72  85  98 113
      8      25  32  41  50  61  72  85  98 113 128
      9      32  41  50  61  72  85  98 113 128 145
     10      41  50  61  72  85  98 113 128 145 162


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:14 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   5   8  13  18  25  32  41  50
      2       2   5   8  13  18  25  32  41  50  61
      3       5   8  13  18  25  32  41  50  61  72
      4       8  13  18  25  32  41  50  61  72  85
      5      13  18  25  32  41  50  61  72  85  98
      6      18  25  32  41  50  61  72  85  98 113
      7      25  32  41  50  61  72  85  98 113 128
      8      32  41  50  61  72  85  98 113 128 145
      9      41  50  61  72  85  98 113 128 145 162
     10      50  61  72  85  98 113 128 145 162 181
stretch c b 'mean 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=   102.5630 Sigma=    65.7930
*** MEAN OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Mean Stretch:       2 to      0 and    255 to    255
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:15 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   0   4   8  14  20  29  38  49
      2       0   0   4   8  14  20  29  38  49  60
      3       0   4   8  14  20  29  38  49  60  74
      4       4   8  14  20  29  38  49  60  74  88
      5       8  14  20  29  38  49  60  74  88 104
      6      14  20  29  38  49  60  74  88 104 121
      7      20  29  38  49  60  74  88 104 121 135
      8      29  38  49  60  74  88 104 121 135 148
      9      38  49  60  74  88 104 121 135 148 162
     10      49  60  74  88 104 121 135 148 162 177


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:15 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   4   8  14  20  29  38  49  60
      2       0   4   8  14  20  29  38  49  60  74
      3       4   8  14  20  29  38  49  60  74  88
      4       8  14  20  29  38  49  60  74  88 104
      5      14  20  29  38  49  60  74  88 104 121
      6      20  29  38  49  60  74  88 104 121 135
      7      29  38  49  60  74  88 104 121 135 148
      8      38  49  60  74  88 104 121 135 148 162
      9      49  60  74  88 104 121 135 148 162 177
     10      60  74  88 104 121 135 148 162 177 193
stretch c b 'peak 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=   102.5630 Sigma=    65.7930
*** PEAK OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Peak Stretch:       2 to      0 and    255 to    255
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:16 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   0   5   9  17  24  35  46  60
      2       0   0   5   9  17  24  35  46  60  73
      3       0   5   9  17  24  35  46  60  73  90
      4       5   9  17  24  35  46  60  73  90 107
      5       9  17  24  35  46  60  73  90 107 127
      6      17  24  35  46  60  73  90 107 127 137
      7      24  35  46  60  73  90 107 127 137 148
      8      35  46  60  73  90 107 127 137 148 159
      9      46  60  73  90 107 127 137 148 159 172
     10      60  73  90 107 127 137 148 159 172 185


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:16 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   5   9  17  24  35  46  60  73
      2       0   5   9  17  24  35  46  60  73  90
      3       5   9  17  24  35  46  60  73  90 107
      4       9  17  24  35  46  60  73  90 107 127
      5      17  24  35  46  60  73  90 107 127 137
      6      24  35  46  60  73  90 107 127 137 148
      7      35  46  60  73  90 107 127 137 148 159
      8      46  60  73  90 107 127 137 148 159 172
      9      60  73  90 107 127 137 148 159 172 185
     10      73  90 107 127 137 148 159 172 185 199
stretch c b 'peak range=41
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    97.1349 Sigma=    60.0527
*** PEAK OPTION ***
Peak Stretch:      44 to      0 and    126 to    255
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:16 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line

      2       0   0   0   0   0   0   0   0   0  19
      3       0   0   0   0   0   0   0   0  19  53
      4       0   0   0   0   0   0   0  19  53  87
      5       0   0   0   0   0   0  19  53  87 127
      6       0   0   0   0   0  19  53  87 127 168
      7       0   0   0   0  19  53  87 127 168 214
      8       0   0   0  19  53  87 127 168 214 255
      9       0   0  19  53  87 127 168 214 255 255
     10       0  19  53  87 127 168 214 255 255 255


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:16 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0  19
      2       0   0   0   0   0   0   0   0  19  53
      3       0   0   0   0   0   0   0  19  53  87
      4       0   0   0   0   0   0  19  53  87 127
      5       0   0   0   0   0  19  53  87 127 168
      6       0   0   0   0  19  53  87 127 168 214
      7       0   0   0  19  53  87 127 168 214 255
      8       0   0  19  53  87 127 168 214 255 255
      9       0  19  53  87 127 168 214 255 255 255
     10      19  53  87 127 168 214 255 255 255 255
stretch c b 'peak factor=2
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    97.1349 Sigma=    60.0527
*** PEAK OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
Peak Stretch:       4 to      0 and    242 to    255
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:17 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   0   2   8  18  28  42  56  74
      2       0   0   2   8  18  28  42  56  74  92
      3       0   2   8  18  28  42  56  74  92 114
      4       2   8  18  28  42  56  74  92 114 136
      5       8  18  28  42  56  74  92 114 136 127
      6      18  28  42  56  74  92 114 136 127 153
      7      28  42  56  74  92 114 136 127 153 183
      8      42  56  74  92 114 136 127 153 183 213
      9      56  74  92 114 136 127 153 183 213 247
     10      74  92 114 136 127 153 183 213 247 255


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:17 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   0   2   8  18  28  42  56  74  92
      2       0   2   8  18  28  42  56  74  92 114
      3       2   8  18  28  42  56  74  92 114 136
      4       8  18  28  42  56  74  92 114 136 127
      5      18  28  42  56  74  92 114 136 127 153
      6      28  42  56  74  92 114 136 127 153 183
      7      42  56  74  92 114 136 127 153 183 213
      8      56  74  92 114 136 127 153 183 213 247
      9      74  92 114 136 127 153 183 213 247 255
     10      92 114 136 127 153 183 213 247 255 255
stretch a b func="in1+18"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = in1+18
stretch a b clip=6
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** BIT CLIPPING MODE ***
Bit Clip Stretch:  6 Most  Significant Bits Clipped
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:18 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0  64 128 192   0  64 128 192   0  64
      2      64 128 192   0  64 128 192   0  64 128
      3     128 192   0  64 128 192   0  64 128 192
      4     192   0  64 128 192   0  64 128 192   0
      5       0  64 128 192   0  64 128 192   0  64
      6      64 128 192   0  64 128 192   0  64 128
      7     128 192   0  64 128 192   0  64 128 192
      8     192   0  64 128 192   0  64 128 192   0
      9       0  64 128 192   0  64 128 192   0  64
     10      64 128 192   0  64 128 192   0  64 128


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:18 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1      64 128 192   0  64 128 192   0  64 128
      2     128 192   0  64 128 192   0  64 128 192
      3     192   0  64 128 192   0  64 128 192   0
      4       0  64 128 192   0  64 128 192   0  64
      5      64 128 192   0  64 128 192   0  64 128
      6     128 192   0  64 128 192   0  64 128 192
      7     192   0  64 128 192   0  64 128 192   0
      8       0  64 128 192   0  64 128 192   0  64
      9      64 128 192   0  64 128 192   0  64 128
     10     128 192   0  64 128 192   0  64 128 192
stretch a c func="mod(64*DN1,256)"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = mod(64*DN1,256)
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b contour=4 dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** CONTOUR MODE ***
Contour Stretch: Interval =    4  DNvalue =    100
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:20 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1     100   1   2   3 100   5   6   7 100   9
      2       1   2   3 100   5   6   7 100   9  10
      3       2   3 100   5   6   7 100   9  10  11
      4       3 100   5   6   7 100   9  10  11 100
      5     100   5   6   7 100   9  10  11 100  13
      6       5   6   7 100   9  10  11 100  13  14
      7       6   7 100   9  10  11 100  13  14  15
      8       7 100   9  10  11 100  13  14  15 100
      9     100   9  10  11 100  13  14  15 100  17
     10       9  10  11 100  13  14  15 100  17  18


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:20 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   2   3 100   5   6   7 100   9  10
      2       2   3 100   5   6   7 100   9  10  11
      3       3 100   5   6   7 100   9  10  11 100
      4     100   5   6   7 100   9  10  11 100  13
      5       5   6   7 100   9  10  11 100  13  14
      6       6   7 100   9  10  11 100  13  14  15
      7       7 100   9  10  11 100  13  14  15 100
      8     100   9  10  11 100  13  14  15 100  17
      9       9  10  11 100  13  14  15 100  17  18
     10      10  11 100  13  14  15 100  17  18  19
stretch a c alarm=(0,4,8,12,16) dnvalue=100
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** ALARM MODE ***
Alarm Stretch: DNvalue =    100
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =  46
stretch a c itable=(0,100,4,100,8,100,12,100,16,100)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** INDIVIDUAL DN TABLE STRETCH MODE ***
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =  46
stretch a b log=(0,18)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** LOGARITHMIC STRETCH MODE ***
LOGARITHMIC Stretch:    0.0 to      0 and   18.0 to    255 with CURVE=   1.0
stretch a c func="86.6039343332527*ln(IN1+1)"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = 86.6039343332527*ln(IN1+1)
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b 'pstretch freq=16 phi=128 ampl=100 dc=49.5
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** PERIODIC STRETCH MODE ***
Periodic Stretch: FREQ= 16.00  DC=    49.5  AMPL=   100.0  PHI=128.00
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:24 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1      86  69  50  31  15   4   0   3  14  30
      2      69  50  31  15   4   0   3  14  30  49
      3      50  31  15   4   0   3  14  30  49  68
      4      31  15   4   0   3  14  30  49  68  85
      5      15   4   0   3  14  30  49  68  85  96
      6       4   0   3  14  30  49  68  85  96  99
      7       0   3  14  30  49  68  85  96  99  96
      8       3  14  30  49  68  85  96  99  96  85
      9      14  30  49  68  85  96  99  96  85  68
     10      30  49  68  85  96  99  96  85  68  49


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:24 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1      69  50  31  15   4   0   3  14  30  49
      2      50  31  15   4   0   3  14  30  49  68
      3      31  15   4   0   3  14  30  49  68  85
      4      15   4   0   3  14  30  49  68  85  96
      5       4   0   3  14  30  49  68  85  96  99
      6       0   3  14  30  49  68  85  96  99  96
      7       3  14  30  49  68  85  96  99  96  85
      8      14  30  49  68  85  96  99  96  85  68
      9      30  49  68  85  96  99  96  85  68  49
     10      49  68  85  96  99  96  85  68  49  30
stretch a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = (100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   3
f2 a c func="(100/2)*sin((2*3.1415*16*dn1)/255+128)+49.5"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using byte table lookup
FUNCTION EVALUATED 256 TIMES
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   3
stretch a b 'smooth
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    13.5135 Sigma=     4.9590
*** SMOOTH OPTION ***
Ramp CDF Stretch
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:26 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   1   3   5   9  15  22  31  42  56
      2       1   3   5   9  15  22  31  42  56  72
      3       3   5   9  15  22  31  42  56  72  90
      4       5   9  15  22  31  42  56  72  90 109
      5       9  15  22  31  42  56  72  90 109 128
      6      15  22  31  42  56  72  90 109 128 147
      7      22  31  42  56  72  90 109 128 147 166
      8      31  42  56  72  90 109 128 147 166 183
      9      42  56  72  90 109 128 147 166 183 199
     10      56  72  90 109 128 147 166 183 199 213


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:26 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       1   3   5   9  15  22  31  42  56  72
      2       3   5   9  15  22  31  42  56  72  90
      3       5   9  15  22  31  42  56  72  90 109
      4       9  15  22  31  42  56  72  90 109 128
      5      15  22  31  42  56  72  90 109 128 147
      6      22  31  42  56  72  90 109 128 147 166
      7      31  42  56  72  90 109 128 147 166 183
      8      42  56  72  90 109 128 147 166 183 199
      9      56  72  90 109 128 147 166 183 199 213
     10      72  90 109 128 147 166 183 199 213 225
stretch a b 'ellipse
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    13.5135 Sigma=     4.9590
*** ELLIPSE OPTION ***
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:27 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   3   8  13  19  26  35  45  56  69
      2       3   8  13  19  26  35  45  56  69  83
      3       8  13  19  26  35  45  56  69  83  98
      4      13  19  26  35  45  56  69  83  98 113
      5      19  26  35  45  56  69  83  98 113 128
      6      26  35  45  56  69  83  98 113 128 143
      7      35  45  56  69  83  98 113 128 143 158
      8      45  56  69  83  98 113 128 143 158 172
      9      56  69  83  98 113 128 143 158 172 186
     10      69  83  98 113 128 143 158 172 186 199


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:27 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       3   8  13  19  26  35  45  56  69  83
      2       8  13  19  26  35  45  56  69  83  98
      3      13  19  26  35  45  56  69  83  98 113
      4      19  26  35  45  56  69  83  98 113 128
      5      26  35  45  56  69  83  98 113 128 143
      6      35  45  56  69  83  98 113 128 143 158
      7      45  56  69  83  98 113 128 143 158 172
      8      56  69  83  98 113 128 143 158 172 186
      9      69  83  98 113 128 143 158 172 186 199
     10      83  98 113 128 143 158 172 186 199 210
stretch a b power=1
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    13.5135 Sigma=     4.9590
*** POWER LAW OPTION ***
Power Law Stretch: POWER =    1.00
list b NB=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:28 2013
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0   9  17  25  33  42  52  62  73  84
      2       9  17  25  33  42  52  62  73  84  96
      3      17  25  33  42  52  62  73  84  96 107
      4      25  33  42  52  62  73  84  96 107 117
      5      33  42  52  62  73  84  96 107 117 127
      6      42  52  62  73  84  96 107 117 127 137
      7      52  62  73  84  96 107 117 127 137 148
      8      62  73  84  96 107 117 127 137 148 159
      9      73  84  96 107 117 127 137 148 159 170
     10      84  96 107 117 127 137 148 159 170 182


 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:09 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:28 2013
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1       9  17  25  33  42  52  62  73  84  96
      2      17  25  33  42  52  62  73  84  96 107
      3      25  33  42  52  62  73  84  96 107 117
      4      33  42  52  62  73  84  96 107 117 127
      5      42  52  62  73  84  96 107 117 127 137
      6      52  62  73  84  96 107 117 127 137 148
      7      62  73  84  96 107 117 127 137 148 159
      8      73  84  96 107 117 127 137 148 159 170
      9      84  96 107 117 127 137 148 159 170 182
     10      96 107 117 127 137 148 159 170 182 192
stretch a b 'gauss gsigma=5.0 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    13.5000 Sigma=     4.9749
*** GAUSS OPTION ***
Gaussian Stretch: GSIGMA =   5.00  GMEAN =   127.50
stretch b c func="dn+10"
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** USER SPECIFIED FUNCTION MODE ***
Function Stretch: FUNCTION = dn+10
stretch a b 'gauss gsigma=5.0 gmean=137.5 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    13.5000 Sigma=     4.9749
*** GAUSS OPTION ***
Gaussian Stretch: GSIGMA =   5.00  GMEAN =   137.50
difpic (b,c)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
stretch a b 'ihist 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

INPUT HISTOGRAM
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         1   0.100     **        +         +         +         +         +         +         +         +         +        +   
   1         3   0.300     *****     +         +         +         +         +         +         +         +         +        +   
   2         6   0.600     **********+         +         +         +         +         +         +         +         +        +   
   3        10   1.000     ****************    +         +         +         +         +         +         +         +        +   
   4        15   1.500     ************************      +         +         +         +         +         +         +        +   
   5        21   2.100     **********************************      +         +         +         +         +         +        +   
   6        28   2.800     *********************************************     +         +         +         +         +        +   
   7        36   3.600     **********************************************************  +         +         +         +        +   
   8        45   4.500     ************************************************************************        +         +        +   
   9        55   5.500     ****************************************************************************************  +        +   
  10        63   6.300     ****************************************************************************************************   
  11        69   6.900     **************************************************************************************************** * 
  12        73   7.300     **************************************************************************************************** * 
  13        75   7.500     **************************************************************************************************** * 
  14        75   7.500     **************************************************************************************************** * 
  15        73   7.300     **************************************************************************************************** * 
  16        69   6.900     **************************************************************************************************** * 
  17        63   6.300     ****************************************************************************************************   
  18        55   5.500     ****************************************************************************************  +        +   
  19        45   4.500     ************************************************************************        +         +        +   
  20        36   3.600     **********************************************************  +         +         +         +        +   
  21        28   2.800     *********************************************     +         +         +         +         +        +   
  22        21   2.100     **********************************      +         +         +         +         +         +        +   
  23        15   1.500     ************************      +         +         +         +         +         +         +        +   
  24        10   1.000     ****************    +         +         +         +         +         +         +         +        +   
  25         6   0.600     **********+         +         +         +         +         +         +         +         +        +   
  26         3   0.300     *****     +         +         +         +         +         +         +         +         +        +   
  27         1   0.100     **        +         +         +         +         +         +         +         +         +        +   

Histogram of input image: Mean  =    13.5000 Sigma =     4.9749

Histogram after exclusion: Mean=    13.5000 Sigma=     4.9749
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      2 to      0 and     25 to    255
stretch a b 'ohist 'include
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

Histogram after exclusion: Mean=    13.5000 Sigma=     4.9749
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      2 to      0 and     25 to    255

OUTPUT HISTOGRAM
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0        10   1.000     ****************    +         +         +         +         +         +         +         +        +   

  11        10   1.000     ****************    +         +         +         +         +         +         +         +        +   

  22        15   1.500     ************************      +         +         +         +         +         +         +        +   

  33        21   2.100     **********************************      +         +         +         +         +         +        +   

  44        28   2.800     *********************************************     +         +         +         +         +        +   

  55        36   3.600     **********************************************************  +         +         +         +        +   

  67        45   4.500     ************************************************************************        +         +        +   

  78        55   5.500     ****************************************************************************************  +        +   

  89        63   6.300     ****************************************************************************************************   

 100        69   6.900     **************************************************************************************************** * 

 111        73   7.300     **************************************************************************************************** * 

 122        75   7.500     **************************************************************************************************** * 

 133        75   7.500     **************************************************************************************************** * 

 144        73   7.300     **************************************************************************************************** * 

 155        69   6.900     **************************************************************************************************** * 

 166        63   6.300     ****************************************************************************************************   

 177        55   5.500     ****************************************************************************************  +        +   

 188        45   4.500     ************************************************************************        +         +        +   

 200        36   3.600     **********************************************************  +         +         +         +        +   

 211        28   2.800     *********************************************     +         +         +         +         +        +   

 222        21   2.100     **********************************      +         +         +         +         +         +        +   

 233        15   1.500     ************************      +         +         +         +         +         +         +        +   

 244        10   1.000     ****************    +         +         +         +         +         +         +         +        +   

 255        10   1.000     ****************    +         +         +         +         +         +         +         +        +   
Histogram of output image: Mean=   127.5000 Sigma=    54.8526
stretch a b 'icdf
Beginning VICAR task stretch
STRETCH version 11 Jan 2013

CUMULATIVE DISTRIBUTION FUNCTION
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0         1   0.100     *         +         +         +         +         +         +         +         +         +        +   
   1         4   0.400     *         +         +         +         +         +         +         +         +         +        +   
   2        10   1.000     **        +         +         +         +         +         +         +         +         +        +   
   3        20   2.000     ***       +         +         +         +         +         +         +         +         +        +   
   4        35   3.500     ****      +         +         +         +         +         +         +         +         +        +   
   5        56   5.600     ******    +         +         +         +         +         +         +         +         +        +   
   6        84   8.400     ********* +         +         +         +         +         +         +         +         +        +   
   7       120  12.000     *************       +         +         +         +         +         +         +         +        +   
   8       165  16.500     *****************   +         +         +         +         +         +         +         +        +   
   9       220  22.000     ***********************       +         +         +         +         +         +         +        +   
  10       283  28.300     ***************************** +         +         +         +         +         +         +        +   
  11       352  35.200     ************************************    +         +         +         +         +         +        +   
  12       425  42.500     *******************************************       +         +         +         +         +        +   
  13       500  50.000     ***************************************************         +         +         +         +        +   
  14       575  57.500     **********************************************************  +         +         +         +        +   
  15       648  64.800     *****************************************************************     +         +         +        +   
  16       717  71.700     ************************************************************************        +         +        +   
  17       780  78.000     ******************************************************************************* +         +        +   
  18       835  83.500     ************************************************************************************      +        +   
  19       880  88.000     ***************************************************************************************** +        +   
  20       916  91.600     ********************************************************************************************       +   
  21       944  94.400     ***********************************************************************************************    +   
  22       965  96.500     *************************************************************************************************  +   
  23       980  98.000     ***************************************************************************************************+   
  24       990  99.000     ****************************************************************************************************   
  25       996  99.600     ****************************************************************************************************   
  26       999  99.900     ****************************************************************************************************   
  27      1000 100.000     ****************************************************************************************************   
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    13.5135 Sigma=     4.9590
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      2 to      0 and     25 to    255
stretch a b 'ocdf
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
DN values      0 and    255 excluded

Histogram after exclusion: Mean=    13.5135 Sigma=     4.9590
*** AUTO-STRETCH OPTION ***
Percent saturation at low end=  1.00 at high end=  1.00
AUTO-STRETCH:      2 to      0 and     25 to    255

OUTPUT CUMULATIVE DISTRIBUTION FUNCTION
GRAY      FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90    100

   0        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   1        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   2        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   3        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   4        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   5        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   6        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   7        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   8        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
   9        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
  10        10   1.001     **        +         +         +         +         +         +         +         +         +        +   
  11        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  12        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  13        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  14        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  15        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  16        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  17        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  18        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  19        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  20        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  21        20   2.002     ***       +         +         +         +         +         +         +         +         +        +   
  22        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  23        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  24        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  25        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  26        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  27        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  28        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  29        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  30        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  31        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  32        35   3.504     ****      +         +         +         +         +         +         +         +         +        +   
  33        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  34        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  35        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  36        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  37        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  38        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  39        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  40        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  41        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  42        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  43        56   5.606     ******    +         +         +         +         +         +         +         +         +        +   
  44        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  45        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  46        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  47        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  48        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  49        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  50        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  51        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  52        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  53        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  54        84   8.408     ********* +         +         +         +         +         +         +         +         +        +   
  55       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  56       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  57       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  58       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  59       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  60       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  61       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  62       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  63       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  64       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  65       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  66       120  12.012     *************       +         +         +         +         +         +         +         +        +   
  67       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  68       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  69       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  70       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  71       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  72       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  73       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  74       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  75       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  76       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  77       165  16.517     *****************   +         +         +         +         +         +         +         +        +   
  78       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  79       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  80       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  81       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  82       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  83       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  84       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  85       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  86       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  87       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  88       220  22.022     ***********************       +         +         +         +         +         +         +        +   
  89       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  90       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  91       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  92       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  93       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  94       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  95       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  96       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  97       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  98       283  28.328     ***************************** +         +         +         +         +         +         +        +   
  99       283  28.328     ***************************** +         +         +         +         +         +         +        +   
 100       352  35.235     ************************************    +         +         +         +         +         +        +   
 101       352  35.235     ************************************    +         +         +         +         +         +        +   
 102       352  35.235     ************************************    +         +         +         +         +         +        +   
 103       352  35.235     ************************************    +         +         +         +         +         +        +   
 104       352  35.235     ************************************    +         +         +         +         +         +        +   
 105       352  35.235     ************************************    +         +         +         +         +         +        +   
 106       352  35.235     ************************************    +         +         +         +         +         +        +   
 107       352  35.235     ************************************    +         +         +         +         +         +        +   
 108       352  35.235     ************************************    +         +         +         +         +         +        +   
 109       352  35.235     ************************************    +         +         +         +         +         +        +   
 110       352  35.235     ************************************    +         +         +         +         +         +        +   
 111       425  42.543     *******************************************       +         +         +         +         +        +   
 112       425  42.543     *******************************************       +         +         +         +         +        +   
 113       425  42.543     *******************************************       +         +         +         +         +        +   
 114       425  42.543     *******************************************       +         +         +         +         +        +   
 115       425  42.543     *******************************************       +         +         +         +         +        +   
 116       425  42.543     *******************************************       +         +         +         +         +        +   
 117       425  42.543     *******************************************       +         +         +         +         +        +   
 118       425  42.543     *******************************************       +         +         +         +         +        +   
 119       425  42.543     *******************************************       +         +         +         +         +        +   
 120       425  42.543     *******************************************       +         +         +         +         +        +   
 121       425  42.543     *******************************************       +         +         +         +         +        +   
 122       500  50.050     ***************************************************         +         +         +         +        +   
 123       500  50.050     ***************************************************         +         +         +         +        +   
 124       500  50.050     ***************************************************         +         +         +         +        +   
 125       500  50.050     ***************************************************         +         +         +         +        +   
 126       500  50.050     ***************************************************         +         +         +         +        +   
 127       500  50.050     ***************************************************         +         +         +         +        +   
 128       500  50.050     ***************************************************         +         +         +         +        +   
 129       500  50.050     ***************************************************         +         +         +         +        +   
 130       500  50.050     ***************************************************         +         +         +         +        +   
 131       500  50.050     ***************************************************         +         +         +         +        +   
 132       500  50.050     ***************************************************         +         +         +         +        +   
 133       575  57.558     **********************************************************  +         +         +         +        +   
 134       575  57.558     **********************************************************  +         +         +         +        +   
 135       575  57.558     **********************************************************  +         +         +         +        +   
 136       575  57.558     **********************************************************  +         +         +         +        +   
 137       575  57.558     **********************************************************  +         +         +         +        +   
 138       575  57.558     **********************************************************  +         +         +         +        +   
 139       575  57.558     **********************************************************  +         +         +         +        +   
 140       575  57.558     **********************************************************  +         +         +         +        +   
 141       575  57.558     **********************************************************  +         +         +         +        +   
 142       575  57.558     **********************************************************  +         +         +         +        +   
 143       575  57.558     **********************************************************  +         +         +         +        +   
 144       648  64.865     *****************************************************************     +         +         +        +   
 145       648  64.865     *****************************************************************     +         +         +        +   
 146       648  64.865     *****************************************************************     +         +         +        +   
 147       648  64.865     *****************************************************************     +         +         +        +   
 148       648  64.865     *****************************************************************     +         +         +        +   
 149       648  64.865     *****************************************************************     +         +         +        +   
 150       648  64.865     *****************************************************************     +         +         +        +   
 151       648  64.865     *****************************************************************     +         +         +        +   
 152       648  64.865     *****************************************************************     +         +         +        +   
 153       648  64.865     *****************************************************************     +         +         +        +   
 154       648  64.865     *****************************************************************     +         +         +        +   
 155       717  71.772     ************************************************************************        +         +        +   
 156       717  71.772     ************************************************************************        +         +        +   
 157       717  71.772     ************************************************************************        +         +        +   
 158       717  71.772     ************************************************************************        +         +        +   
 159       717  71.772     ************************************************************************        +         +        +   
 160       717  71.772     ************************************************************************        +         +        +   
 161       717  71.772     ************************************************************************        +         +        +   
 162       717  71.772     ************************************************************************        +         +        +   
 163       717  71.772     ************************************************************************        +         +        +   
 164       717  71.772     ************************************************************************        +         +        +   
 165       717  71.772     ************************************************************************        +         +        +   
 166       780  78.078     ******************************************************************************* +         +        +   
 167       780  78.078     ******************************************************************************* +         +        +   
 168       780  78.078     ******************************************************************************* +         +        +   
 169       780  78.078     ******************************************************************************* +         +        +   
 170       780  78.078     ******************************************************************************* +         +        +   
 171       780  78.078     ******************************************************************************* +         +        +   
 172       780  78.078     ******************************************************************************* +         +        +   
 173       780  78.078     ******************************************************************************* +         +        +   
 174       780  78.078     ******************************************************************************* +         +        +   
 175       780  78.078     ******************************************************************************* +         +        +   
 176       780  78.078     ******************************************************************************* +         +        +   
 177       835  83.584     ************************************************************************************      +        +   
 178       835  83.584     ************************************************************************************      +        +   
 179       835  83.584     ************************************************************************************      +        +   
 180       835  83.584     ************************************************************************************      +        +   
 181       835  83.584     ************************************************************************************      +        +   
 182       835  83.584     ************************************************************************************      +        +   
 183       835  83.584     ************************************************************************************      +        +   
 184       835  83.584     ************************************************************************************      +        +   
 185       835  83.584     ************************************************************************************      +        +   
 186       835  83.584     ************************************************************************************      +        +   
 187       835  83.584     ************************************************************************************      +        +   
 188       880  88.088     ***************************************************************************************** +        +   
 189       880  88.088     ***************************************************************************************** +        +   
 190       880  88.088     ***************************************************************************************** +        +   
 191       880  88.088     ***************************************************************************************** +        +   
 192       880  88.088     ***************************************************************************************** +        +   
 193       880  88.088     ***************************************************************************************** +        +   
 194       880  88.088     ***************************************************************************************** +        +   
 195       880  88.088     ***************************************************************************************** +        +   
 196       880  88.088     ***************************************************************************************** +        +   
 197       880  88.088     ***************************************************************************************** +        +   
 198       880  88.088     ***************************************************************************************** +        +   
 199       880  88.088     ***************************************************************************************** +        +   
 200       916  91.692     ********************************************************************************************       +   
 201       916  91.692     ********************************************************************************************       +   
 202       916  91.692     ********************************************************************************************       +   
 203       916  91.692     ********************************************************************************************       +   
 204       916  91.692     ********************************************************************************************       +   
 205       916  91.692     ********************************************************************************************       +   
 206       916  91.692     ********************************************************************************************       +   
 207       916  91.692     ********************************************************************************************       +   
 208       916  91.692     ********************************************************************************************       +   
 209       916  91.692     ********************************************************************************************       +   
 210       916  91.692     ********************************************************************************************       +   
 211       944  94.494     ***********************************************************************************************    +   
 212       944  94.494     ***********************************************************************************************    +   
 213       944  94.494     ***********************************************************************************************    +   
 214       944  94.494     ***********************************************************************************************    +   
 215       944  94.494     ***********************************************************************************************    +   
 216       944  94.494     ***********************************************************************************************    +   
 217       944  94.494     ***********************************************************************************************    +   
 218       944  94.494     ***********************************************************************************************    +   
 219       944  94.494     ***********************************************************************************************    +   
 220       944  94.494     ***********************************************************************************************    +   
 221       944  94.494     ***********************************************************************************************    +   
 222       965  96.597     *************************************************************************************************  +   
 223       965  96.597     *************************************************************************************************  +   
 224       965  96.597     *************************************************************************************************  +   
 225       965  96.597     *************************************************************************************************  +   
 226       965  96.597     *************************************************************************************************  +   
 227       965  96.597     *************************************************************************************************  +   
 228       965  96.597     *************************************************************************************************  +   
 229       965  96.597     *************************************************************************************************  +   
 230       965  96.597     *************************************************************************************************  +   
 231       965  96.597     *************************************************************************************************  +   
 232       965  96.597     *************************************************************************************************  +   
 233       980  98.098     ***************************************************************************************************+   
 234       980  98.098     ***************************************************************************************************+   
 235       980  98.098     ***************************************************************************************************+   
 236       980  98.098     ***************************************************************************************************+   
 237       980  98.098     ***************************************************************************************************+   
 238       980  98.098     ***************************************************************************************************+   
 239       980  98.098     ***************************************************************************************************+   
 240       980  98.098     ***************************************************************************************************+   
 241       980  98.098     ***************************************************************************************************+   
 242       980  98.098     ***************************************************************************************************+   
 243       980  98.098     ***************************************************************************************************+   
 244       990  99.099     ****************************************************************************************************   
 245       990  99.099     ****************************************************************************************************   
 246       990  99.099     ****************************************************************************************************   
 247       990  99.099     ****************************************************************************************************   
 248       990  99.099     ****************************************************************************************************   
 249       990  99.099     ****************************************************************************************************   
 250       990  99.099     ****************************************************************************************************   
 251       990  99.099     ****************************************************************************************************   
 252       990  99.099     ****************************************************************************************************   
 253       990  99.099     ****************************************************************************************************   
 254       990  99.099     ****************************************************************************************************   
 255      1000 100.100     ****************************************************************************************************   
gen xxb 11 7 ival=100 linc=0 sinc=0 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
qsar xxb xxa area=(2 2 1 5 -32668 4 2 1 5 -100 6 2 1 5 2330  +
 8 2 1 5 3995 10 2 1 5 32667)
Beginning VICAR task qsar
QSAR version 08-SEP-03
list xxa size=(1 1 11 7)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:32 2013
 Task:QSAR      User:lwk       Date_Time:Fri Jan 11 09:03:33 2013
     Samp       1     2     3     4     5     6     7
   Line
      1       100   100   100   100   100   100   100
      2       100-32568-32568-32568-32568-32568   100
      3       100   100   100   100   100   100   100
      4       100     0     0     0     0     0   100
      5       100   100   100   100   100   100   100
      6       100  2430  2430  2430  2430  2430   100
      7       100   100   100   100   100   100   100
      8       100  4095  4095  4095  4095  4095   100
      9       100   100   100   100   100   100   100
     10       100 32767 32767 32767 32767 32767   100
     11       100   100   100   100   100   100   100
stretch xxa xxc table=(-32768 -32768 -1 -32768 0 0 1 1000 2429 1000  +
 2430 2430 2431 1000 4094 1000 4095 4095 4096 32767 32767 32767)
Beginning VICAR task stretch
STRETCH version 11 Jan 2013
*** TABLE STRETCH MODE ***
list xxc size=(1 1 11 7)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Fri Jan 11 09:03:32 2013
 Task:STRETCH   User:lwk       Date_Time:Fri Jan 11 09:03:34 2013
     Samp       1     2     3     4     5     6     7
   Line
      1      1000  1000  1000  1000  1000  1000  1000
      2      1000-32768-32768-32768-32768-32768  1000
      3      1000  1000  1000  1000  1000  1000  1000
      4      1000     0     0     0     0     0  1000
      5      1000  1000  1000  1000  1000  1000  1000
      6      1000  2430  2430  2430  2430  2430  1000
      7      1000  1000  1000  1000  1000  1000  1000
      8      1000  4095  4095  4095  4095  4095  1000
      9      1000  1000  1000  1000  1000  1000  1000
     10      1000 32767 32767 32767 32767 32767  1000
     11      1000  1000  1000  1000  1000  1000  1000
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC

TSTSTRETCH.LOG ON LINUX IS IDENTICAL, EXCEPT FOR SMALL DIFFERENCES
BY 1 IN LARGE INTEGERS, WHICH ARE DUE TO ROUNDOFF ERROR AND ARE NOT
SIGNIFICANT. 

$ Return
$!#############################################################################
$Imake_File:
$ create stretch.imake
#define PROGRAM stretch

#define MODULE_LIST stretch.f manual_stretch.f \
                    hist_params.f get_histogram.f exclude_histogram.f \
                    histogram_stretch.f bimodal.f \
                    hpeak.f astretch.f post_stretch.f print_histogram.f stati.f

#define MAIN_LANG_FORTRAN
#define R2LIB
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
