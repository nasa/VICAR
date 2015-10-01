$!****************************************************************************
$!
$! Build proc for MIPL module fit
$! VPACK Version 1.9, Monday, December 07, 2009, 16:15:55
$!
$! Execute by entering:		$ @fit
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
$ write sys$output "*** module fit ***"
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
$ write sys$output "Invalid argument given to fit.com file -- ", primary
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
$   if F$SEARCH("fit.imake") .nes. ""
$   then
$      vimake fit
$      purge fit.bld
$   else
$      if F$SEARCH("fit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fit.bld "STD"
$   else
$      @fit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fit.com -mixed -
	-s fit.f -
	-i fit.imake -
	-p fit.pdf -
	-t tstfit.pdf new_fit_3dcase_1.log orig_fit_3dcase_1.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C VICAR PROGRAM FIT -- Automatic stretch program
C Performs a histogram (ends-in) stretch.  See also program ASTRCH
C
C          FIT  INP  OUT  user-parameters
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      CHARACTER*5 FMT
      CHARACTER*3 ORGIN
      LOGICAL XVPTST
      INTEGER*4 HIST(65536)
      INTEGER*4 NLT,NST
      CHARACTER*40 MSG
      CHARACTER*30 MSG1,MSG2,MSG3,MSG4
      DATA  MSG1/'INVALID VALUE OF SL SET TO SL='/,
     *      MSG2/'INVALID VALUE OF NL SET TO NL='/,
     *      MSG3/'INVALID VALUE OF SS SET TO SS='/,
     *      MSG4/'INVALID VALUE OF NS SET TO NS='/
 111  FORMAT(A30,I3)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('FIT version 5 August, 2003',' ')
 
      CALL XVUNIT(IUN,'INP',1,IND,' ')
      CALL XVSIGNAL(IUN,IND,.TRUE.)
      CALL XVOPEN(IUN,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(IUN,IND,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      CALL XVBANDS(isb,nb,nbi)

      IF ( isb .GT. nbi ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( isb + nb - 1 .GT. nbi) THEN 
         CALL XVMESSAGE('***Number of bands truncated', ' ') 
         nb = nbi + 1 - isb
      ENDIF


      CALL XVGET(IUN,IND,'NL',NLT,'NS',NST,' ')
      IF (IND .NE. 1) THEN
        CALL XVMESSAGE('CANNOT GET INPUT INAGE SIZE.',' ')
        CALL XVMESSAGE('CANNOT VERIFY USER INPUT SIZE.',' ')
        CALL XVMESSAGE('*******ABANDING*********',' ')
        CALL ABEND
      ENDIF 
      IF (SL.LT.1) THEN
        SL=1
        WRITE(MSG,111) MSG1,SL
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (NL.GT.NLT) THEN
        NL=NLT
        WRITE(MSG,111) MSG2,NL
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (SS.LT.1) THEN
        SS=1
        WRITE(MSG,111) MSG3,SS
        CALL XVMESSAGE(MSG,0)
      ENDIF
      IF (NS.GT.NST) THEN
        NS=NST
        WRITE(MSG,111) MSG4,NL
        CALL XVMESSAGE(MSG,0)
      ENDIF

C     ....Check data format of input image
      CALL XVGET(IUN,ind,'FORMAT',fmt,' ')
C     ....Format must be halfword, otherwise ABEND with message.
      IF (FMT.NE.'HALF'.AND.FMT.NE.'WORD') GOTO 992	

C     ....Generate the histogram
      CALL HISTGEN2(hist,nfreq)
      IF (NFREQ.EQ.0) GOTO 990

C     ....Print the histogram (optional)
      CALL PRINTHIS(HIST,NFREQ)

      CALL XVPCNT('OUT',NO)
      IF (NO.EQ.0) GOTO 100

C     ....Open output image
      IF (XVPTST('BYTE')) THEN
         OCODE=1
         FMT = 'BYTE'
      ELSE
         OCODE = 2
         FMT = 'HALF'
      ENDIF
      CALL XVUNIT(OUN,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUN,IND,.TRUE.)
      CALL XVOPEN(OUN,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &	'OP','WRITE','U_FORMAT',FMT,'U_NB', nb, 'O_FORMAT',FMT,' ')

C     ....Determine stretch limits
      CALL SLIMITS(OUN,OCODE,HIST,NFREQ,min,max,lval,hval)

C     ....Generate the output image
      CALL FITPIC(OCODE,MIN,MAX,LVAL,HVAL)

  100 CALL XVMESSAGE('FIT task completed',' ')
      RETURN

C     ....Error message handling
  990 CALL XVMESSAGE('***Histogram contains zero elements',' ')
      GOTO 999
  992 CALL XVMESSAGE('***Invalid input data format',' ')
      CALL XVMESSAGE('***Input image must be in halfword format',' ')
  999 CALL XVMESSAGE('***FIT task cancelled',' ')
      CALL ABEND
      END

C Routine to generate a histogram of 16-bit data
C Outputs:  HIST = 65536 grey-level histogram
C           NFREQ = total number of pixels in histogram
C Variable: BUF = temporary work area to hold input image lines
C
      SUBROUTINE HISTGEN2(hist,nfreq)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 HIST(-32768:32767)

      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      COMMON/AREA/LSL,LSS,LNL,LNS
      INTEGER*2 BUF(10000)

      CALL ZIA(hist,65536)		   !Zero out the histogram

      CALL XVPARM('AREA',lsl,idef,icnt,4)	!Requested image area
      IF (LSL.EQ.0) THEN 
	CALL MVE(4,4,SL,lsl,1,1)   	   !Default is the entire picture
      ENDIF

      CALL XVPARM('SPEED',linc,idef,icnt,1)
      LEL = LSL + LNL - 1
      eb = isb + nb - 1
      
C     ....Accumulate the histogram
      DO ib=isb,eb
         DO L=LSL,LEL,LINC
            CALL XVREAD(IUN,buf,ind,'LINE',L,'SAMP',LSS,'NSAMPS',LNS, 
     +                  'BAND',ib, ' ')
            DO I=1,LNS
               IDN = BUF(I)
               HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDDO
      ENDDO

      NFREQ = (LNL/LINC)*LNS*nb
      RETURN
      END

C Routine to print raw and excluded histograms.  Also processes EXCLUDE
C and INCLUDE parameters.  Excluded DNs are zeroed out in HIST.
C
      SUBROUTINE PRINTHIS(HIST,NFREQ)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 HIST(-32768:32767),EXCL(2,101)
      LOGICAL XVPTST
      CHARACTER*1  FORMFEED
      CHARACTER*33 MSG
      DATA MSG/'     RAW HISTOGRAM STATISTICS...'/

C     Form feed string for passing to XVMESSAGE
      FORMFEED = CHAR(12)

      CALL XVP('HINC',INC,ICNT)
      CALL XVP('NLINES',NLIN,ICNT)
      CALL XVP('SPIKES',NSPIKE,ICNT)

      IF (XVPTST('LOG')) LOG=1
      PHIS = 0
      IF (XVPTST('PHIST')) THEN
	CALL XVMESSAGE(FORMFEED,' ')
	PHIS = 1
      ELSE
	CALL XVMESSAGE(' ',' ')
      ENDIF
      CALL XVMESSAGE(MSG,' ')

      CALL PHIST2(HIST,NFREQ,NLIN,INC,NSPIKE,LOG,PHIS)

      CALL XVPARM('EXCLUDE',EXCL,NEXCL,IDEF,202)
      IF (IDEF.EQ.1) THEN
         IF (XVPTST('INCLUDE')) RETURN
      ELSE
         IF (MOD(NEXCL,2).NE.0) THEN
             CALL XVMESSAGE('***EXCLUDE must have pairs of numbers **',
     .		' ')
	     CALL ABEND
         ENDIF
         NEXCL = NEXCL/2		! # OF PAIRS

         DO K=1,NEXCL
            J1 = EXCL(1,K)      
            J2 = EXCL(2,K)     
            J1 = MAX0(J1,-32768) 
            J2 = MIN0(J2,32767)
            DO J=J1,J2
               NFREQ = NFREQ - HIST(J)
               HIST(J) = 0
            ENDDO            
         ENDDO
      ENDIF

C     ....Check for exclusion of lower saturation DN
      IF (.NOT.XVPTST('INCLUDE')) THEN
         DO J=-1,-32768,-1
            IF (HIST(J).GT.0) THEN
               NFREQ = NFREQ - HIST(-32768)
               HIST(-32768) = 0
               GOTO 50
            ENDIF
         ENDDO
         NFREQ = NFREQ - HIST(0)
         HIST(0) = 0
      ENDIF

   50 MSG(1:8) = 'EXCLUDED'
      IF (NFREQ.EQ.0) THEN
         CALL XVMESSAGE('***All pixels have been excluded',' ')
	 RETURN
      ENDIF
      PEHIS = 0
      IF (XVPTST('EHIST')) THEN
	CALL XVMESSAGE(FORMFEED,' ')
	PEHIS = 1
      ELSE
	CALL XVMESSAGE(' ',' ')
      ENDIF
      CALL XVMESSAGE(MSG,' ')
      CALL PHIST2(HIST,NFREQ,NLIN,INC,NSPIKE,LOG,PEHIS)
      RETURN
      END

C Print out a histogram containing 65536 grey-levels.
C
      SUBROUTINE PHIST2(HIST,NFREQ,NLIN,INC0,NSPIKE,LOG,IPRNT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 MAXT/2.147E09/,HIST(-32768:32767)

      CHARACTER*100 MSG1
      CHARACTER*550 MSG2
      CHARACTER*150 LISTO
      CHARACTER*150 LIST1

C     ....Initialize character arrays with blanks.
      DO I=1,150
	LISTO(I:I) = ' '
	LIST1(I:I) = ' '
      ENDDO
      DO I=1,532
	MSG2(I:I) = ' '
      ENDDO

C     ....Write header line for table of histogram
      MSG2(1:4) = 'GRAY'
      MSG2(12:24) = 'FREQ  PERCENT'
      DO I=1,10
	J = (I-1)*10
        K = 27 + J
	L = K + 1
	WRITE (MSG2(K:L), '(I2)') J
      ENDDO
      J = 100
      WRITE (MSG2(126:128), '(I3)') J	

C     ....Compute and print out mean and standard deviation
      S = 0.D0
      S2 = 0.D0

      DO I=-32768,32767
         IFREQ = HIST(I) 
         IF (IFREQ.GT.0) THEN
            R = I
            RMOM = IFREQ*R
            S = S + RMOM
            S2 = S2 + RMOM*R
         ENDIF
      ENDDO

      AVG = S/NFREQ
      SIGMA = S2/NFREQ - AVG*AVG
      SIGMA = DSQRT(SIGMA)
      WRITE(MSG1,101) AVG,SIGMA,NFREQ
  101 FORMAT('AVERAGE GRAY LEVEL=',F10.3,' STANDARD DEVIATION=',F10.3,
     +  ' NUMBER OF ELEMENTS=',I8)
      CALL XVMESSAGE(MSG1,' ')
C
C     ....Now print out the histogram
      IF (NLIN*IPRNT.EQ.0) RETURN
      INC = INC0        !Determine histogram increment
      IF (INC.EQ.0) CALL PINC(HIST,NLIN,inc)
      IEND = 32767 - INC + 1
      MAXS = MAXT

      DO J=1,NSPIKE	!Determine maximum frequency (ignoring spikes)
         MAX = 0
         DO I=-32768,IEND,INC
            CALL SUMV(4,INC,HIST(I),ifreq,1)
            IF (IFREQ.GT.MAX.AND.IFREQ.LT.MAXS) MAX=IFREQ
         ENDDO
         IF (MAX.LE.0) GOTO 20
         MAXS = MAX
      ENDDO

   20 Z0 = MAX0(MAX,1)
      IF (LOG.EQ.1) Z0=DLOG(Z0)
      R = 100.D0/NFREQ
      IF (LOG.EQ.1) THEN
	CALL XVMESSAGE('Horizontal scale is logarithmic',' ')
      ENDIF
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2,' ')

      NLOUT = 0		!Count of number of histogram lines printed
C
C     ....Print out the histogram
      DO 100 I=-32768,IEND,INC
      CALL SUMV(4,INC,HIST(I),IFREQ,1)
      IF (IFREQ.EQ.0.) THEN
         IF (IFLAG.EQ.0) CALL XVMESSAGE(' ',' ') !Print space to indicate data gap
         IFLAG = 1
      ELSE
         IF (NLOUT.GE.NLIN) THEN
            N = (32767-I)/INC		!See how close we are to end
            IF (N.GT.0.5*NLIN) GOTO 990 !If not close, truncate listing
            NLOUT = 0			!If close, finish it.
         ENDIF
         IFLAG = 0
         PERCEN = IFREQ*R
         NCHAR = 129
         Z = MAX0(IFREQ,0)
         IF (LOG.EQ.1.AND.IFREQ.GT.0) Z=DLOG(Z)
         IVAL = 100.D0*Z/Z0 + 1
         IF (IVAL.GT.101) THEN		!Truncate values greater than 100
            IVAL = 101
            NCHAR = 131
         ENDIF

	 WRITE (LISTO(1:6),'(I6)') I
	 WRITE (LISTO(9:15),'(I7)') IFREQ
	 WRITE (LISTO(17:24),'(F8.3)') PERCEN
         WRITE (LISTO(28:127), '(A100)') LIST1
	 DO J=38,138,10
		LISTO(J:J) = '+'
	 ENDDO
	 DO J=1,IVAL
		LISTO(27+J:27+J) = '*'
	 ENDDO
         CALL XVMESSAGE(LISTO,' ')
         NLOUT = NLOUT + 1
      ENDIF
  100 CONTINUE
      RETURN

  990 CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('***Maximum number of lines (NLIN) exceeded',' ')
      CALL XVMESSAGE('***Histogram listing truncated',' ')
      RETURN
      END
C Find a suitable DN increment for printing out a histogram.
C
      SUBROUTINE PINC(HIST,NLIN,inc)
      INTEGER*4 HIST(-32768:32767)
      N1 = 0
      N2 = 0
      N4 = 0
      N8 = 0
      N16 = 0
      N32 = 0
      N64 = 0
      N128 = 0
      N256 = 0
      I = -32769
C
      DO I512=1,65536,256
	M128 = N128
	DO I256=1,2
          M64 = N64
          DO I128=1,2  
            M32 = N32
            DO I64=1,2   
              M16 = N16
              DO I32=1,2  
                M8 = N8
                DO I16=1,2
                  M4 = N4
                  DO I8=1,2 
                    M2 = N2
                    DO I4=1,2  
                      M1 = N1
                      DO I2=1,2 
                        I = I + 1
                        IF(HIST(I).GT.0) N1=N1+1    
                      ENDDO                          
                      IF(M1.LT.N1) N2=N2+1
                    ENDDO                            
                    IF(M2.LT.N2) N4=N4+1
                  ENDDO                              
                  IF(M4.LT.N4) N8=N8+1
                ENDDO                                
                IF(M8.LT.N8) N16=N16+1
              ENDDO                                   
              IF(M16.LT.N16) N32=N32+1
            ENDDO     
            IF(M32.LT.N32) N64=N64+1
          ENDDO         
          IF(M64.LT.N64) N128=N128+1
        ENDDO 
        IF(M128.LT.N128) N256=N256+1
      ENDDO

      INC = 1
      IF(N1.LE.NLIN.AND.N1.GT.N2) RETURN
      INC = 2
      IF(N2.LE.NLIN.AND.N2.GT.N4) RETURN
      INC = 4
      IF(N4.LE.NLIN.AND.N4.GT.N8) RETURN
      INC = 8
      IF(N8.LE.NLIN.AND.N8.GT.N16) RETURN
      INC = 16
      IF(N16.LE.NLIN.AND.N16.GT.N32) RETURN
      INC = 32
      IF(N32.LE.NLIN.AND.N32.GT.N64) RETURN
      INC = 64
      IF(N64.LE.NLIN.AND.N64.GT.N128) RETURN
      INC = 128
      IF(N128.LE.NLIN.AND.N128.GT.N256)RETURN
      INC=256
      IF(N256.GT.NLIN) INC=512
      RETURN
      END
C Determine stretch limits
C
      SUBROUTINE SLIMITS(OUN,OCODE,HIST,NFREQ,min,max,lval,hval)
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 RPER,LPER,HPER
      CHARACTER*39 MSG1
      CHARACTER*39 MSG2
      CHARACTER*43 MSG5
      DATA MSG1/'MINIMUM DN OF          SCALED TO XXXXX'/
      DATA MSG2/'MAXIMUM DN OF          SCALED TO XXXXX'/
      DATA MSG5/'STRETCH        TO XXXXX AND        TO      '/
      CALL XVPARM('PERCENT',RPER,ICNT,IDEF,1)
      LPER = RPER/2.
      HPER = RPER/2.
      IF (IDEF.EQ.1) THEN
	CALL XVPARM('LPERCENT',LPER,ICNT,IDEF,1)
	CALL XVPARM('HPERCENT',HPER,ICNT,IDEF,1)
      ENDIF

      CALL XVPARM('LVALUE',LVAL,ICNT,IDEF,1)
      CALL XVPARM('HVALUE',HVAL,ICNT,IDEF,1)
      IF (OCODE.EQ.1.AND.IDEF.EQ.1) HVAL=255

C     ....Compute percentage limits (MIN,MAX)
      CALL ASTRC2(HIST,NFREQ,LPER,HPER,min,max)

C     ....Print stretch limits
      WRITE (MSG1(15:20),'(I6)') MIN
      WRITE (MSG1(34:38),'(I5)') LVAL
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG1,' ')
      WRITE (MSG2(15:20),'(I6)') MAX
      WRITE (MSG2(34:38),'(I5)') HVAL
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2,' ')

C     ....Put stretch limits in label
      WRITE (MSG5(8:13),'(I6)') MIN
      WRITE (MSG5(18:22),'(I5)') LVAL
      WRITE (MSG5(28:33),'(I6)') MAX
      WRITE (MSG5(38:42),'(I5)') HVAL
      CALL XLADD(OUN,'HISTORY','LIMITS',MSG5,STAT,'ULEN',43,
     &		'FORMAT','STRING',' ')
      RETURN
      END
C Determine stretch limits from the histogram
C
      SUBROUTINE ASTRC2(HIST,NFREQ,LPER,HPER,min,max)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 LPER,HPER

      MIN = -32768
      MAX = 32767

      IF (NFREQ.EQ.0) RETURN

      ISUM = HIST(MIN)
      KOUNT = NFREQ*.01*LPER + 1.5
      DO WHILE (ISUM.LT.KOUNT)
         MIN = MIN + 1
         ISUM = ISUM + HIST(MIN)
      ENDDO

      ISUM = HIST(MAX)
      KOUNT = NFREQ*.01*HPER + 1.5
      DO WHILE(ISUM.LT.KOUNT)
         MAX = MAX - 1
         ISUM = ISUM + HIST(MAX)
      ENDDO

      IF (MAX.LT.-32767) MAX=-32767
      IF (MIN.GE.MAX) MIN=MAX-1
      RETURN
      END
C Generate the stretched output image
C Output image code: 	OCODE ( =1 is byte, =2 is halfword )
C 			BUF(NS)  = input and/or output image-line buffer
C             		OBUF(NS) = output image-line buffer
C
      SUBROUTINE FITPIC(OCODE,MIN,MAX,LVAL,HVAL)
      IMPLICIT INTEGER(A-Z)

      COMMON/CP/isb,nb,SL,SS,NL,NS,IUN,OUN
      INTEGER*2 ITBL(-32768:32767),BUF(10000)
      BYTE TBL(-32768:32767),OBUF(20000)

      IF (OCODE.EQ.1) THEN
C     ....Generate a halfword-to-byte lookup table
	      CALL MVE(-5,MIN+32769,LVAL,TBL,0,1)
	      N = MAX - MIN + 1
	      CALL INTRPA(1,N,TBL(MIN),LVAL,HVAL)
	      CALL MVE(-5,32768-MAX,HVAL,TBL(MAX),0,1)

	      EL = SL + NL - 1
              eb = isb + nb - 1
C          ....Generate byte output image
           BANDOUT=0
           DO ib=isb,eb
              BANDOUT = BANDOUT + 1
              LINEOUT = 0
              DO L=SL,EL
                 LINEOUT = LINEOUT + 1
                 CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',SS,
     .                       'NSAMPS',NS, 'BAND', ib, ' ')
                 DO I=1,NS
                    OBUF(I) = TBL(BUF(I))
                 ENDDO
                 CALL XVWRIT(OUN,OBUF,STAT,'LINE',LINEOUT,
     +            'BAND',BANDOUT,' ')
              ENDDO
           ENDDO
      ELSE
C     ....Generate a halfword lookup table
	      CALL MVE(-6,MIN+32769,LVAL,ITBL,0,1)
	      N = MAX - MIN + 1
	      CALL INTRPA(2,N,ITBL(MIN),LVAL,HVAL)
	      CALL MVE(-6,32768-MAX,HVAL,ITBL(MAX),0,1)

	      EL = SL + NL - 1
           eb = isb + nb - 1
C          ....Generate halfword output image
           BANDOUT = 0
  	      DO ib=isb,eb
              BANDOUT = BANDOUT + 1
              LINEOUT = 0
              DO L=SL,EL
                 LINEOUT = LINEOUT + 1
                 CALL XVREAD(IUN,BUF,STAT,'LINE',L,'SAMP',SS,
     .                'NSAMPS',NS, 'BAND', ib, ' ')
                 DO I=1,NS
                    BUF(I) = ITBL(BUF(I))
                 ENDDO
                 CALL XVWRIT(OUN,BUF,STAT,'LINE',LINEOUT,
     +                       'BAND',BANDOUT,' ')
              ENDDO
           ENDDO
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fit.imake
#define PROGRAM   fit

#define MODULE_LIST fit.f

#define MAIN_LANG_FORTRAN

#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create fit.pdf
process help=*
PARM INP      TYPE=STRING
PARM OUT      TYPE=STRING  COUNT=(0:1)			DEFAULT=--
PARM SIZE     TYPE=INTEGER COUNT=4			DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER                              DEFAULT=1
PARM SS       TYPE=INTEGER                              DEFAULT=1
PARM SB       TYPE=INTEGER                              DEFAULT=1
PARM NL       TYPE=INTEGER                              DEFAULT=0
PARM NS       TYPE=INTEGER                              DEFAULT=0
PARM NB       TYPE=INTEGER                              DEFAULT=0
PARM OFORM    TYPE=KEYWORD VALID=(BYTE,HALF)            DEFAULT=HALF
PARM PERCENT  TYPE=REAL    VALID=(0.:100.)		DEFAULT=0.
PARM LPERCENT TYPE=REAL    VALID=(0.:100.)              DEFAULT=0.
PARM HPERCENT TYPE=REAL    VALID=(0.:100.)		DEFAULT=0.
PARM HVALUE   TYPE=INTEGER VALID=(0:32768) COUNT=1      DEFAULT=511
PARM LVALUE   TYPE=INTEGER VALID=(0:32768) COUNT=1      DEFAULT=0
PARM SPEED    TYPE=INTEGER VALID=(1:1000)  COUNT=1      DEFAULT=1      
PARM AREA     TYPE=INTEGER COUNT=4			DEFAULT=(0,0,0,0)
PARM EXCLUDE  TYPE=INTEGER COUNT=(0:200)		DEFAULT=--
PARM INCLUDE  TYPE=KEYWORD VALID=INCLUDE   COUNT=(0:1)	DEFAULT=--
PARM PHIST    TYPE=KEYWORD VALID=PHIST     COUNT=(0:1)	DEFAULT=--
PARM EHIST    TYPE=KEYWORD VALID=EHIST     COUNT=(0:1)	DEFAULT=--
PARM SPIKES   TYPE=INTEGER VALID=(0:32768) COUNT=(0:1)   DEFAULT=1
PARM LOG      TYPE=KEYWORD VALID=LOG       COUNT=(0:1)	DEFAULT=--
PARM NLINES   TYPE=INTEGER VALID=(0:65536) COUNT=1	DEFAULT=256
PARM HINC     TYPE=INTEGER VALID=(0:65536) COUNT=1	DEFAULT=0
END-PROC
.TITLE
	Program FIT
.HELP
PURPOSE:

FIT is a VICAR applications program which performs automatic linear
stretches on halfword pictures anywhere in the halfword range (-32768
to +32767).  The program is primarily used to compress the DN range of
an image so that it "fits" within a 0-511 DN range (a restriction of some
older programs) or a 0-255 DN range, the latter being output in byte
data format.  The program employs a histogram based automatic stretch 
algorithm similar to program STRETCH.  FIT may also be used to print the
histogram.

EXECUTION:

	FIT  INP=A  OUT=B  user-parameters...

where the input image A must be in halfword (16-bit integer) data format.
The output image B may be specified to be in byte or halfword data
format (see OFORM keyword).  The maximum sample length is 10,000.

.page
DESCRIPTION OF THE AUTO-STRETCH ALGORITHM:

FIT computes a grey level frequency table (histogram) of the input 
image.  The histogram has 65536 bins, hence spans the entire halfword
range from -32768 to +32767.

If EXCLUDE=(N1,N2) is specified, the histogram is modified by setting:

	H(I) = 0 for N1 <= I <= N2

In addition, the histogram is modified by setting the low-saturation DN
to zero.  If the input image contains negative DN values, the low-saturation
DN is assumed to be -32768.  If not, then the low-saturation DN is 0.  This
feature may be suppressed via the INCLUDE keyword.
.page
                                                      I2
                                                     ---
Define the area function A:	      A(I1,I2) =     \     H(I)
                                                     /
        ^                                            ---
        |                                           I=I1	
        |
        |
        |                        _--_    __
     H  |                      _-    -__-  -_
        |                    _-              -_
        |                  _-                  -_
        |                _-                      -_
        |              _-|                        |#-_
        |           _-###|                        |###-__
        |        __-#####|                        |######-_____
        ---|-------------|------------------------|-----------------|---->
        -32768         IMIN                     IMAX              32767  I
.page
The program determines linear stretch constants IMIN and IMAX such that:

 a) IMIN is the largest I which satisfies the equation
       A(-32768,I) <= PL * A(-32768,32767)

 b) IMAX is the smallest I which satisfies the equation
       A(I,32767) <= PH * A(-32768,32767)

where PL and PH may be specified by the LPER and HPER keywords.  The values
A(-32768,IMIN) and A(IMAX,32767) represent the shaded areas under the curve in 
the above graph.  Note that A(-32768,32767) is the total histogram area, and 
represents the number of samples in the image which have not been excluded.
.page
The linear stretch which is applied to the image is defined as follows:
	
	   HVAL - LVAL
      Y =  -----------*(X - IMIN) + LVAL
	   IMAX - IMIN

where X and Y are the input and output DN values respectively for each
pixel.  If Y is less than LVAL, then Y is set equal to LVAL.  If Y is greater
than HVAL, then Y is set equal to HVAL (See LVAL and HVAL parameters).

.page
DESCRIPTION OF PRINTED HISTOGRAMS AND STATISTICS:

After computing the histogram, the mean and standard deviation of the 
input image are printed out.

If the keyword PHIST is specified, the histogram is printed out.  Since
it is normally undesirable to print all the grey levels, the histogram
is condensed so that a maximum of NLIN grey levels are printed. 
This is accomplished by determining a suitable increment M.  Alternatively, 
the increment M may be specified via the HINC parameter.  The condensed
histogram H' may be interpreted as follows:

   H'(I) = H(I) + H(I+1) + ... + H(I+M-1)   for I=1, M+1, 2*M+1, 3*M+1,...
   H'(I) = 0  for all other I.

After zeroing out the excluded elements from the histogram, the mean and
standard deviation computed from the excluded histogram are printed out.  
If EHIST is specified, the excluded histogram is printed out.  Same 
comments as above apply.
.page
EXAMPLES:

	FIT  INP=A  OUT=B  'BYTE  PERC=1.  EXCL=(12001,32767)

The halfword image contained in data set A is scaled to 8 bits, saturating
0.5 percent of the data at both ends of the histogram.  All sample values
less than 1 or greater than 12000 are ignored in the auto-stretch computation.

	FIT  INP=A  'PHIST 

The halfword histogram is printed.  No output image is produced.

	FIT  INP=A  OUT=B  'BYTE  PERC=1.0
The image is converted to byte data.

.PAGE
PROGRAM HISTORY
WRITTEN BY:			GARY YAGI	2 SEPT 1982
CONVERTED TO VAX BY:		J. A. MOSHER	JUNE 1985
CURRENT COGNIZANT PROGRAMMER:	G. Yagi
REVISIONS:
 05 Aug 03  NTT   Enabled for 3-D images
 08 Mar 03  GMY   Fixed minor print problem on Linux (AR 108262).
 12 Jun 98  RRP   Changed all calls to xvparm to have 5 parameters rather
                  then six parameter to make time work on hp platform.
 27 Apr 98  RRP   Updated pdf to only allow valid values for parameters.
                  AR-9585
 23 Feb 96  FFM   Initialize phis, & pehis to work on SGI. (FR 88195).
  5 May 94  GMY   Updated help file (FR 48460)
 17 Mar 89  GMY   Fix case where excluded histogram is empty.
                  Change default exclude so that -32768 is excluded if
                  negative DNs exist (IOM MSD:384-89-071).
  8 Nov 88  GMY   Fix 1 DN stretch (ASTRC2).
  7 Nov 88  GMY   Add check of input image format.
 15 JUL 88  GMY   Massive code reorganization and cleanup.
		  Rewrote table-stretch algoritm.
		  Fixed HINC and SPEED parameters.
		  STACKA deleted.
		  Modified help file and rewrote test file.
    OCT 87  FFM   Modified the source to handle the case which input
                  is a halfword, output is a byte, lval=255, hval=0.
    FEB 85  LWK   Full VICAR2 conversion & bug fixes. STACKA added.
    NOV 84  LWK   Partially converted to VICAR2 (i/o only)  
 13 FEB 84  LWK   Renamed FIT
    JUN 83  JAM   Converted to VAX
  2 SEP 82  GMY   Initial release: program FIT3

.LEVEL1
.VARIABLE INP
Input halfword image
.VARIABLE OUT
Output image (stretched)
.VARIABLE SIZE
VICAR size field (SL,SS,NL,NS)
.VARIABLE SL
INTEGER - Starting Line of 
input image.
.VARIABLE SS
INTEGER - Starting Sample of 
input image.
.VARIABLE SB
INTEGER - Starting Band of 
input image.
.VARIABLE NL
INTEGER - Number of lines in 
the input image.
.VARIABLE NS
INTEGER - Number of Samples 
in input image.
.VARIABLE NB
INTEGER - Number of Bands 
in input image.
.VARIABLE OFORM
Output data format.
Valid: BYTE,HALF
.VARIABLE SPEED
INTEGER - line increment 
used for histogram.
.VARIABLE PERCENT
REAL - percentage of 
histogram to be saturated.
.VARIABLE LPERCENT
REAL - percentage of lower end 
of histogram to be saturated.
.VARIABLE HPERCENT
REAL - percentage of higher end
of histogram to be saturated.
.VARIABLE HVALUE
INTEGER - input histogram upper
saturation DN mapped to HVAL
.VARIABLE LVALUE
INTEGER - input histogram lower
saturation DN mapped to LVAL
.VARIABLE AREA
(SL,SS,NL,NS) - field of image
for histogram computation
.VARIABLE EXCLUDE
2-200 INTEGERS - exclude the DNs 
within the specified range
.VARIABLE INCLUDE
Suppress exclusion of low-saturation
DNs from auto-stretch computation
.VARIABLE PHIST
Print histogram of input image
.VARIABLE EHIST
Print excluded histogram
.VARIABLE SPIKE
Specifies the number of spikes
in the histogram (for printing)
.VARIABLE LOG
Frequency axis of printed
histogram is logrithmicaly scaled
.VARIABLE NLIN
INTEGER - number of grey
levels/lines in printed histogram
.VARIABLE HINC
INTEGER - grey level increment
factor for printed histogram
.LEVEL2
.VARIABLE INP
Input image in halfword data format.  Maximum sample length is 10,000.
.VARIABLE OUT
Output image. The stretched version of INP. Can be halfword or byte as
specified by the OFORM parameter. Default is halfword.
.VARIABLE SIZE
4 INTEGERS - A VICAR size field. (Starting_Line, Starting_sample, 
Number_of_Lines, Number_of_samples) Default = it will be read from
the image label.
.VARIABLE SL
INTEGER - Starting Line of input image.
.VARIABLE SS
INTEGER - Starting Sample of input image.
.VARIABLE SB
INTEGER - Starting Band of input image.
.VARIABLE NL
INTEGER - Number of lines in the input image.
.VARIABLE NS
INTEGER - Number of Samples in input image.
.VARIABLE NB
INTEGER - Number of Bands in input image.
.VARIABLE OFORM
Keyword with valid values BYTE and HALF.
BYTE specifies that the output image is to be in byte format, with 
DN values from 0 to 255. 
 
HALF specifies that the output is a halfword data format with DN values 
ranging from 0 to 511.
.VARIABLE SPEED
SPEED is an integer specifying a line increment.  When computing 
the halfword histogram, only every SPEEDth line will be read. 
Default is SPEED = 1.
.VARIABLE PERCENT
PERCENT is a real number specifying the percent of the histogram 
to be saturated (half of PERCENT at each end). 
Default is PERCENT = 0.0.
.VARIABLE LPERCENT
LPERCENT is a real number specifying the percentage of the lower 
end of the histogram to be saturated. 
Default is LPERCENT = 0.0.
.VARIABLE HPERCENT
HPERCENT is a real number specifying the percentage of the high
end of the histogram to be saturated. 
Default is HPERCENT = 0.0.
.VARIABLE HVALUE
HVALUE is an integer specifying the output DN value to which that 
input DN corresponding to the upper saturation point on the  
histogram is to be mapped.
Default is 511, unless BYTE has been specified in which case the 
default value is 255.
.VARIABLE LVALUE
LVALUE is an integer specifying the output DN value to which that
input DN corresponding to the lower saturation point on the histogram
is to be mapped. 
Default is LVALUE = 0.
.VARIABLE AREA
AREA is composed of four integers which correspond to a size field within 
which the histogram is to be computed.  AREA refers to pixels and is 
relative to line = 1, sample = 1, not to the origin of the size field 
(the default being the entire picture).
.VARIABLE EXCLUDE
EXCLUDE is composed of pairs of integers specifying the exclusion of the 
DN range between them from auto_stretch computation.  EXCLUDE may have up
to 100 pairs of values, to specify multiple DN ranges.

The default is to exclude only 0 DN.
.VARIABLE INCLUDE
Before scanning the histogram for the stretch limits, the histogram is normally
modified by setting the low-saturation DN to zero.  If the input image contains
negative DN values, the low-saturation DN is assumed to be -32768.  If not,
then the low-saturation DN is 0.  This feature may be suppressed via the
INCLUDE keyword.

Note that if the user specifies a range using EXCLUDE that excludes the low-
saturation DN, the INCLUDE parameter is ignored.
.VARIABLE PHIST
PHIST is a keyword specifying the histogram of the input image is to be 
printed. Default is no histogram will be printed.
.VARIABLE EHIST
EHIST is a keyword specifying the excluded histogram is to be printed out.
.VARIABLE SPIKE
SPIKE is an integer specifying the number of spikes in the histogram. 
This parameter is used to control the scale of the frequency axis of the
printed histogram by setting the SPIKE + 1st highest frequency to the 
maximum value (100), and normalizing all other frequencies to this scale.
Default is SPIKE = 1.
.VARIABLE LOG
If the keyword 'LOG is specified, the frequency axis of the histogram is
printed using a logrithmic scale.
.VARIABLE NLIN 
NLIN is an integer specifying the number of grey levels to display in the
printed histogram.  This controls the number of lines printed per histogram.
Default NLIN = 256.
.VARIABLE HINC
HINC is an integer specifying the grey level increment factor for the 
printed histogram.  Note that if the number of lines printed exceeds NLIN,
the printout will be terminated.
The default is to have the program automatically compute the histogram
increment based upon the value of NLIN.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfit.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "This is a test for FIT"
write ""
write "Test halfword output,SIZE,PERC,LVAL,HVAL,EXCLUDE,EHIST"
write ""
gen a 20 20 'half ival=-100 sinc=20 linc=50
fit a b (2,2,18,18) perc=1. lval=10 hval=300 exclude=(-32768,0) 'ehist
list b
write ""
write "Test byte output, LPERC, HPERC, PHIST"
write ""
fit a b  lperc=2. hperc=4. 'include  'phist  'byte
list b
write ""
write "Test SPEED,AREA"
write ""
fit a b  speed=2 area=(4,4,16,16) 'byte
list b
write ""
write "Test no output file,HINC,LOG"
write ""
fit a hinc=20 nlines=80 'log 'phist
write "Test FR 88195"
write "The histogram should not be displayed because phist is not specified"
gen a.dat 200 200 'half
fit a.dat b.dat perc=.5 'byte

write "Test with nl and ns bigger then file size."

gen a.dat 200 200 'half
fit inp=a.dat out=a.out sl=1 nl=800 ss=1 ns=800 +
lvalue=0 hvalue=255  area=(1,10,20,40)

fit inp=a.dat out=a.out size=(1,1,800,800) +
lvalue=0 hvalue=255  area=(1,10,20,40)

write "Test 3D (multibanded) processing capability (simple)"
gen a NS=10 NL=10 NB=10 'half
fit a
fit a NB=4


write "Test 3D (multibanded) processing capability (linc, sinc)"
gen a NS=10 NL=10 NB=10 'half ival=-100 sinc=20 linc=50
fit a

end-proc

$!-----------------------------------------------------------------------------
$ create new_fit_3dcase_1.log
tstfit
write "This is a test for FIT"
This is a test for FIT
write ""

write "Test halfword output,SIZE,PERC,LVAL,HVAL,EXCLUDE,EHIST"
Test halfword output,SIZE,PERC,LVAL,HVAL,EXCLUDE,EHIST
write ""

gen a 20 20 'half ival=-100 sinc=20 linc=50
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a b (2,2,18,18) perc=1. lval=10 hval=300 exclude=(-32768,0) 'ehist
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   279.389 NUMBER OF ELEMENTS=     324

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   568.634 STANDARD DEVIATION=   276.412 NUMBER OF ELEMENTS=     322

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

     8        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    16        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    24        1    0.311   **************************    +         +         +         +         +         +         +         +         +

    40        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    48        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    56        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    64        2    0.621   ***************************************************         +         +         +         +         +         +

    80        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    88        2    0.621   ***************************************************         +         +         +         +         +         +
    96        1    0.311   **************************    +         +         +         +         +         +         +         +         +
   104        2    0.621   ***************************************************         +         +         +         +         +         +

   120        2    0.621   ***************************************************         +         +         +         +         +         +
   128        2    0.621   ***************************************************         +         +         +         +         +         +
   136        2    0.621   ***************************************************         +         +         +         +         +         +
   144        2    0.621   ***************************************************         +         +         +         +         +         +

   160        2    0.621   ***************************************************         +         +         +         +         +         +
   168        3    0.932   ****************************************************************************    +         +         +         +
   176        2    0.621   ***************************************************         +         +         +         +         +         +
   184        3    0.932   ****************************************************************************    +         +         +         +

   200        2    0.621   ***************************************************         +         +         +         +         +         +
   208        3    0.932   ****************************************************************************    +         +         +         +
   216        3    0.932   ****************************************************************************    +         +         +         +
   224        3    0.932   ****************************************************************************    +         +         +         +

   240        3    0.932   ****************************************************************************    +         +         +         +
   248        3    0.932   ****************************************************************************    +         +         +         +
   256        3    0.932   ****************************************************************************    +         +         +         +
   264        4    1.242   *****************************************************************************************************         +

   280        3    0.932   ****************************************************************************    +         +         +         +
   288        4    1.242   *****************************************************************************************************         +
   296        3    0.932   ****************************************************************************    +         +         +         +
   304        4    1.242   *****************************************************************************************************         +

   320        4    1.242   *****************************************************************************************************         +
   328        3    0.932   ****************************************************************************    +         +         +         +
   336        4    1.242   *****************************************************************************************************         +
   344        3    0.932   ****************************************************************************    +         +         +         +

   360        4    1.242   *****************************************************************************************************         +
   368        4    1.242   *****************************************************************************************************         +
   376        3    0.932   ****************************************************************************    +         +         +         +
   384        4    1.242   *****************************************************************************************************         +

   400        3    0.932   ****************************************************************************    +         +         +         +
   408        4    1.242   *****************************************************************************************************         +
   416        4    1.242   *****************************************************************************************************         +
   424        3    0.932   ****************************************************************************    +         +         +         +

   440        4    1.242   *****************************************************************************************************         +
   448        3    0.932   ****************************************************************************    +         +         +         +
   456        4    1.242   *****************************************************************************************************         +
   464        4    1.242   *****************************************************************************************************         +

   480        3    0.932   ****************************************************************************    +         +         +         +
   488        4    1.242   *****************************************************************************************************         +
   496        3    0.932   ****************************************************************************    +         +         +         +
   504        4    1.242   *****************************************************************************************************         +

   520        4    1.242   *****************************************************************************************************         +
   528        3    0.932   ****************************************************************************    +         +         +         +
   536        4    1.242   *****************************************************************************************************         +
   544        3    0.932   ****************************************************************************    +         +         +         +

   560        4    1.242   *****************************************************************************************************         +
   568        4    1.242   *****************************************************************************************************         +
   576        3    0.932   ****************************************************************************    +         +         +         +
   584        4    1.242   *****************************************************************************************************         +

   600        3    0.932   ****************************************************************************    +         +         +         +
   608        4    1.242   *****************************************************************************************************         +
   616        4    1.242   *****************************************************************************************************         +
   624        3    0.932   ****************************************************************************    +         +         +         +

   640        4    1.242   *****************************************************************************************************         +
   648        3    0.932   ****************************************************************************    +         +         +         +
   656        4    1.242   *****************************************************************************************************         +
   664        4    1.242   *****************************************************************************************************         +

   680        3    0.932   ****************************************************************************    +         +         +         +
   688        4    1.242   *****************************************************************************************************         +
   696        3    0.932   ****************************************************************************    +         +         +         +
   704        4    1.242   *****************************************************************************************************         +

   720        4    1.242   *****************************************************************************************************         +
   728        3    0.932   ****************************************************************************    +         +         +         +
   736        4    1.242   *****************************************************************************************************         +
   744        3    0.932   ****************************************************************************    +         +         +         +

   760        4    1.242   *****************************************************************************************************         +
   768        4    1.242   *****************************************************************************************************         +
   776        3    0.932   ****************************************************************************    +         +         +         +
   784        4    1.242   *****************************************************************************************************         +

   800        3    0.932   ****************************************************************************    +         +         +         +
   808        4    1.242   *****************************************************************************************************         +
   816        4    1.242   *****************************************************************************************************         +
   824        3    0.932   ****************************************************************************    +         +         +         +

   840        4    1.242   *****************************************************************************************************         +
   848        3    0.932   ****************************************************************************    +         +         +         +
   856        4    1.242   *****************************************************************************************************         +
   864        3    0.932   ****************************************************************************    +         +         +         +

   880        3    0.932   ****************************************************************************    +         +         +         +
   888        3    0.932   ****************************************************************************    +         +         +         +
   896        3    0.932   ****************************************************************************    +         +         +         +
   904        3    0.932   ****************************************************************************    +         +         +         +

   920        3    0.932   ****************************************************************************    +         +         +         +
   928        2    0.621   ***************************************************         +         +         +         +         +         +
   936        3    0.932   ****************************************************************************    +         +         +         +
   944        2    0.621   ***************************************************         +         +         +         +         +         +

   960        3    0.932   ****************************************************************************    +         +         +         +
   968        2    0.621   ***************************************************         +         +         +         +         +         +
   976        2    0.621   ***************************************************         +         +         +         +         +         +
   984        2    0.621   ***************************************************         +         +         +         +         +         +

  1000        2    0.621   ***************************************************         +         +         +         +         +         +
  1008        2    0.621   ***************************************************         +         +         +         +         +         +
  1016        2    0.621   ***************************************************         +         +         +         +         +         +
  1024        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1040        2    0.621   ***************************************************         +         +         +         +         +         +
  1048        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1056        2    0.621   ***************************************************         +         +         +         +         +         +
  1064        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1080        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1088        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1096        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1104        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1120        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1136        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1160        1    0.311   **************************    +         +         +         +         +         +         +         +         +


MINIMUM DN OF     30   SCALED TO    10

MAXIMUM DN OF   1120   SCALED TO   300
FIT task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
 Task:FIT       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        10    10    10    10    15    21    26    31    37    42    47    53    58    63    69
      2        10    13    18    23    29    34    39    45    50    55    61    66    71    77    82
      3        21    26    31    37    42    47    53    58    63    69    74    79    84    90    95
      4        34    39    45    50    55    61    66    71    77    82    87    92    98   103   108
      5        47    53    58    63    69    74    79    84    90    95   100   106   111   116   122
      6        61    66    71    77    82    87    92    98   103   108   114   119   124   130   135
      7        74    79    84    90    95   100   106   111   116   122   127   132   138   143   148
      8        87    92    98   103   108   114   119   124   130   135   140   146   151   156   162
      9       100   106   111   116   122   127   132   138   143   148   154   159   164   170   175
     10       114   119   124   130   135   140   146   151   156   162   167   172   178   183   188
     11       127   132   138   143   148   154   159   164   170   175   180   186   191   196   202
     12       140   146   151   156   162   167   172   178   183   188   194   199   204   210   215
     13       154   159   164   170   175   180   186   191   196   202   207   212   218   223   228
     14       167   172   178   183   188   194   199   204   210   215   220   226   231   236   241
     15       180   186   191   196   202   207   212   218   223   228   233   239   244   249   255
     16       194   199   204   210   215   220   226   231   236   241   247   252   257   263   268
     17       207   212   218   223   228   233   239   244   249   255   260   265   271   276   281
     18       220   226   231   236   241   247   252   257   263   268   273   279   284   289   295

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
 Task:FIT       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
     Samp      16    17    18
   Line
      1        74    79    84
      2        87    92    98
      3       100   106   111
      4       114   119   124
      5       127   132   138
      6       140   146   151
      7       154   159   164
      8       167   172   178
      9       180   186   191
     10       194   199   204
     11       207   212   218
     12       220   226   231
     13       233   239   244
     14       247   252   257
     15       260   265   271
     16       273   279   284
     17       287   292   297
     18       300   300   300
write ""

write "Test byte output, LPERC, HPERC, PHIST"
Test byte output, LPERC, HPERC, PHIST
write ""

fit a b  lperc=2. hperc=4. 'include  'phist  'byte
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

  -104        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -80        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -64        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -56        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -40        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -32        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -24        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -16        1    0.250   **************************    +         +         +         +         +         +         +         +         +

     0        2    0.500   ***************************************************         +         +         +         +         +         +
     8        1    0.250   **************************    +         +         +         +         +         +         +         +         +
    16        2    0.500   ***************************************************         +         +         +         +         +         +
    24        1    0.250   **************************    +         +         +         +         +         +         +         +         +

    40        2    0.500   ***************************************************         +         +         +         +         +         +
    48        2    0.500   ***************************************************         +         +         +         +         +         +
    56        2    0.500   ***************************************************         +         +         +         +         +         +
    64        2    0.500   ***************************************************         +         +         +         +         +         +

    80        2    0.500   ***************************************************         +         +         +         +         +         +
    88        2    0.500   ***************************************************         +         +         +         +         +         +
    96        3    0.750   ****************************************************************************    +         +         +         +
   104        2    0.500   ***************************************************         +         +         +         +         +         +

   120        3    0.750   ****************************************************************************    +         +         +         +
   128        2    0.500   ***************************************************         +         +         +         +         +         +
   136        3    0.750   ****************************************************************************    +         +         +         +
   144        3    0.750   ****************************************************************************    +         +         +         +

   160        3    0.750   ****************************************************************************    +         +         +         +
   168        3    0.750   ****************************************************************************    +         +         +         +
   176        3    0.750   ****************************************************************************    +         +         +         +
   184        3    0.750   ****************************************************************************    +         +         +         +

   200        4    1.000   *****************************************************************************************************         +
   208        3    0.750   ****************************************************************************    +         +         +         +
   216        4    1.000   *****************************************************************************************************         +
   224        3    0.750   ****************************************************************************    +         +         +         +

   240        4    1.000   *****************************************************************************************************         +
   248        4    1.000   *****************************************************************************************************         +
   256        4    1.000   *****************************************************************************************************         +
   264        4    1.000   *****************************************************************************************************         +

   280        4    1.000   *****************************************************************************************************         +
   288        4    1.000   *****************************************************************************************************         +
   296        4    1.000   *****************************************************************************************************         +
   304        4    1.000   *****************************************************************************************************         +

   320        4    1.000   *****************************************************************************************************         +
   328        4    1.000   *****************************************************************************************************         +
   336        4    1.000   *****************************************************************************************************         +
   344        4    1.000   *****************************************************************************************************         +

   360        4    1.000   *****************************************************************************************************         +
   368        4    1.000   *****************************************************************************************************         +
   376        4    1.000   *****************************************************************************************************         +
   384        4    1.000   *****************************************************************************************************         +

   400        4    1.000   *****************************************************************************************************         +
   408        4    1.000   *****************************************************************************************************         +
   416        4    1.000   *****************************************************************************************************         +
   424        4    1.000   *****************************************************************************************************         +

   440        4    1.000   *****************************************************************************************************         +
   448        4    1.000   *****************************************************************************************************         +
   456        4    1.000   *****************************************************************************************************         +
   464        4    1.000   *****************************************************************************************************         +

   480        4    1.000   *****************************************************************************************************         +
   488        4    1.000   *****************************************************************************************************         +
   496        4    1.000   *****************************************************************************************************         +
   504        4    1.000   *****************************************************************************************************         +

   520        4    1.000   *****************************************************************************************************         +
   528        4    1.000   *****************************************************************************************************         +
   536        4    1.000   *****************************************************************************************************         +
   544        4    1.000   *****************************************************************************************************         +

   560        4    1.000   *****************************************************************************************************         +
   568        4    1.000   *****************************************************************************************************         +
   576        4    1.000   *****************************************************************************************************         +
   584        4    1.000   *****************************************************************************************************         +

   600        4    1.000   *****************************************************************************************************         +
   608        4    1.000   *****************************************************************************************************         +
   616        4    1.000   *****************************************************************************************************         +
   624        4    1.000   *****************************************************************************************************         +

   640        4    1.000   *****************************************************************************************************         +
   648        4    1.000   *****************************************************************************************************         +
   656        4    1.000   *****************************************************************************************************         +
   664        4    1.000   *****************************************************************************************************         +

   680        4    1.000   *****************************************************************************************************         +
   688        4    1.000   *****************************************************************************************************         +
   696        4    1.000   *****************************************************************************************************         +
   704        4    1.000   *****************************************************************************************************         +

   720        4    1.000   *****************************************************************************************************         +
   728        4    1.000   *****************************************************************************************************         +
   736        4    1.000   *****************************************************************************************************         +
   744        4    1.000   *****************************************************************************************************         +

   760        4    1.000   *****************************************************************************************************         +
   768        4    1.000   *****************************************************************************************************         +
   776        4    1.000   *****************************************************************************************************         +
   784        4    1.000   *****************************************************************************************************         +

   800        4    1.000   *****************************************************************************************************         +
   808        4    1.000   *****************************************************************************************************         +
   816        4    1.000   *****************************************************************************************************         +
   824        4    1.000   *****************************************************************************************************         +

   840        4    1.000   *****************************************************************************************************         +
   848        4    1.000   *****************************************************************************************************         +
   856        4    1.000   *****************************************************************************************************         +
   864        4    1.000   *****************************************************************************************************         +

   880        4    1.000   *****************************************************************************************************         +
   888        4    1.000   *****************************************************************************************************         +
   896        3    0.750   ****************************************************************************    +         +         +         +
   904        4    1.000   *****************************************************************************************************         +

   920        3    0.750   ****************************************************************************    +         +         +         +
   928        4    1.000   *****************************************************************************************************         +
   936        3    0.750   ****************************************************************************    +         +         +         +
   944        3    0.750   ****************************************************************************    +         +         +         +

   960        3    0.750   ****************************************************************************    +         +         +         +
   968        3    0.750   ****************************************************************************    +         +         +         +
   976        3    0.750   ****************************************************************************    +         +         +         +
   984        3    0.750   ****************************************************************************    +         +         +         +

  1000        2    0.500   ***************************************************         +         +         +         +         +         +
  1008        3    0.750   ****************************************************************************    +         +         +         +
  1016        2    0.500   ***************************************************         +         +         +         +         +         +
  1024        3    0.750   ****************************************************************************    +         +         +         +

  1040        2    0.500   ***************************************************         +         +         +         +         +         +
  1048        2    0.500   ***************************************************         +         +         +         +         +         +
  1056        2    0.500   ***************************************************         +         +         +         +         +         +
  1064        2    0.500   ***************************************************         +         +         +         +         +         +

  1080        2    0.500   ***************************************************         +         +         +         +         +         +
  1088        2    0.500   ***************************************************         +         +         +         +         +         +
  1096        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1104        2    0.500   ***************************************************         +         +         +         +         +         +

  1120        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1128        2    0.500   ***************************************************         +         +         +         +         +         +
  1136        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1144        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1160        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1168        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1176        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1184        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1208        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1224        1    0.250   **************************    +         +         +         +         +         +         +         +         +


MINIMUM DN OF      0   SCALED TO     0

MAXIMUM DN OF   1080   SCALED TO   255
FIT task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
 Task:FIT       User:ntt       Date_Time:Wed Aug  6 15:27:45 2003
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   5   9  14  19  24  28  33  38  42  47  52  57  61  66
      2       0   0   0   2   7  12  17  21  26  31  35  40  45  50  54  59  64  68  73  78
      3       0   5   9  14  19  24  28  33  38  42  47  52  57  61  66  71  76  80  85  90
      4      12  17  21  26  31  35  40  45  50  54  59  64  68  73  78  83  87  92  97 102
      5      24  28  33  38  42  47  52  57  61  66  71  76  80  85  90  94  99 104 109 113
      6      35  40  45  50  54  59  64  68  73  78  83  87  92  97 102 106 111 116 120 125
      7      47  52  57  61  66  71  76  80  85  90  94  99 104 109 113 118 123 128 132 137
      8      59  64  68  73  78  83  87  92  97 102 106 111 116 120 125 130 135 139 144 149
      9      71  76  80  85  90  94  99 104 109 113 118 123 128 132 137 142 146 151 156 161
     10      83  87  92  97 102 106 111 116 120 125 130 135 139 144 149 153 158 163 168 172
     11      94  99 104 109 113 118 123 128 132 137 142 146 151 156 161 165 170 175 179 184
     12     106 111 116 120 125 130 135 139 144 149 153 158 163 168 172 177 182 187 191 196
     13     118 123 128 132 137 142 146 151 156 161 165 170 175 179 184 189 194 198 203 208
     14     130 135 139 144 149 153 158 163 168 172 177 182 187 191 196 201 205 210 215 220
     15     142 146 151 156 161 165 170 175 179 184 189 194 198 203 208 213 217 222 227 231
     16     153 158 163 168 172 177 182 187 191 196 201 205 210 215 220 224 229 234 238 243
     17     165 170 175 179 184 189 194 198 203 208 213 217 222 227 231 236 241 246 250 255
     18     177 182 187 191 196 201 205 210 215 220 224 229 234 238 243 248 253 255 255 255
     19     189 194 198 203 208 213 217 222 227 231 236 241 246 250 255 255 255 255 255 255
     20     201 205 210 215 220 224 229 234 238 243 248 253 255 255 255 255 255 255 255 255
write ""

write "Test SPEED,AREA"
Test SPEED,AREA
write ""

fit a b  speed=2 area=(4,4,16,16) 'byte
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   610.000 STANDARD DEVIATION=   246.982 NUMBER OF ELEMENTS=     128

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   610.000 STANDARD DEVIATION=   246.982 NUMBER OF ELEMENTS=     128

MINIMUM DN OF    110   SCALED TO     0

MAXIMUM DN OF   1110   SCALED TO   255
FIT task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Aug  6 15:27:44 2003
 Task:FIT       User:ntt       Date_Time:Wed Aug  6 15:27:45 2003
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   3   8  13  18  23  28  33  38  43
      2       0   0   0   0   0   0   0   0   0   5  10  15  20  25  31  36  41  46  51  56
      3       0   0   0   0   0   0   3   8  13  18  23  28  33  38  43  48  54  59  64  69
      4       0   0   0   0   5  10  15  20  25  31  36  41  46  51  56  61  66  71  76  82
      5       0   3   8  13  18  23  28  33  38  43  48  54  59  64  69  74  79  84  89  94
      6      10  15  20  25  31  36  41  46  51  56  61  66  71  76  82  87  92  97 102 107
      7      23  28  33  38  43  48  54  59  64  69  74  79  84  89  94  99 105 110 115 120
      8      36  41  46  51  56  61  66  71  76  82  87  92  97 102 107 112 117 122 127 133
      9      48  54  59  64  69  74  79  84  89  94  99 105 110 115 120 125 130 135 140 145
     10      61  66  71  76  82  87  92  97 102 107 112 117 122 127 133 138 143 148 153 158
     11      74  79  84  89  94  99 105 110 115 120 125 130 135 140 145 150 156 161 166 171
     12      87  92  97 102 107 112 117 122 127 133 138 143 148 153 158 163 168 173 179 184
     13      99 105 110 115 120 125 130 135 140 145 150 156 161 166 171 176 181 186 191 196
     14     112 117 122 127 133 138 143 148 153 158 163 168 173 179 184 189 194 199 204 209
     15     125 130 135 140 145 150 156 161 166 171 176 181 186 191 196 201 207 212 217 222
     16     138 143 148 153 158 163 168 173 179 184 189 194 199 204 209 214 219 224 230 235
     17     150 156 161 166 171 176 181 186 191 196 201 207 212 217 222 227 232 237 242 247
     18     163 168 173 179 184 189 194 199 204 209 214 219 224 230 235 240 245 250 255 255
     19     176 181 186 191 196 201 207 212 217 222 227 232 237 242 247 252 255 255 255 255
     20     189 194 199 204 209 214 219 224 230 235 240 245 250 255 255 255 255 255 255 255
write ""

write "Test no output file,HINC,LOG"
Test no output file,HINC,LOG
write ""

fit a hinc=20 nlines=80 'log 'phist
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400
Horizontal scale is logarithmic

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

  -108        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
   -88        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
   -68        2    0.500   **********************************      +         +         +         +         +         +         +         +
   -48        2    0.500   **********************************      +         +         +         +         +         +         +         +
   -28        2    0.500   **********************************      +         +         +         +         +         +         +         +
    -8        3    0.750   *****************************************************       +         +         +         +         +         +
    12        3    0.750   *****************************************************       +         +         +         +         +         +
    32        4    1.000   *******************************************************************   +         +         +         +         +
    52        4    1.000   *******************************************************************   +         +         +         +         +
    72        4    1.000   *******************************************************************   +         +         +         +         +
    92        5    1.250   ******************************************************************************  +         +         +         +
   112        5    1.250   ******************************************************************************  +         +         +         +
   132        6    1.500   ***************************************************************************************   +         +         +
   152        6    1.500   ***************************************************************************************   +         +         +
   172        6    1.500   ***************************************************************************************   +         +         +
   192        7    1.750   **********************************************************************************************      +         +
   212        7    1.750   **********************************************************************************************      +         +
   232        8    2.000   *****************************************************************************************************         +
   252        8    2.000   *****************************************************************************************************         +
   272        8    2.000   *****************************************************************************************************         +
   292        8    2.000   *****************************************************************************************************         +
   312        8    2.000   *****************************************************************************************************         +
   332        8    2.000   *****************************************************************************************************         +
   352        8    2.000   *****************************************************************************************************         +
   372        8    2.000   *****************************************************************************************************         +
   392        8    2.000   *****************************************************************************************************         +
   412        8    2.000   *****************************************************************************************************         +
   432        8    2.000   *****************************************************************************************************         +
   452        8    2.000   *****************************************************************************************************         +
   472        8    2.000   *****************************************************************************************************         +
   492        8    2.000   *****************************************************************************************************         +
   512        8    2.000   *****************************************************************************************************         +
   532        8    2.000   *****************************************************************************************************         +
   552        8    2.000   *****************************************************************************************************         +
   572        8    2.000   *****************************************************************************************************         +
   592        8    2.000   *****************************************************************************************************         +
   612        8    2.000   *****************************************************************************************************         +
   632        8    2.000   *****************************************************************************************************         +
   652        8    2.000   *****************************************************************************************************         +
   672        8    2.000   *****************************************************************************************************         +
   692        8    2.000   *****************************************************************************************************         +
   712        8    2.000   *****************************************************************************************************         +
   732        8    2.000   *****************************************************************************************************         +
   752        8    2.000   *****************************************************************************************************         +
   772        8    2.000   *****************************************************************************************************         +
   792        8    2.000   *****************************************************************************************************         +
   812        8    2.000   *****************************************************************************************************         +
   832        8    2.000   *****************************************************************************************************         +
   852        8    2.000   *****************************************************************************************************         +
   872        8    2.000   *****************************************************************************************************         +
   892        7    1.750   **********************************************************************************************      +         +
   912        7    1.750   **********************************************************************************************      +         +
   932        6    1.500   ***************************************************************************************   +         +         +
   952        6    1.500   ***************************************************************************************   +         +         +
   972        6    1.500   ***************************************************************************************   +         +         +
   992        5    1.250   ******************************************************************************  +         +         +         +
  1012        5    1.250   ******************************************************************************  +         +         +         +
  1032        4    1.000   *******************************************************************   +         +         +         +         +
  1052        4    1.000   *******************************************************************   +         +         +         +         +
  1072        4    1.000   *******************************************************************   +         +         +         +         +
  1092        3    0.750   *****************************************************       +         +         +         +         +         +
  1112        3    0.750   *****************************************************       +         +         +         +         +         +
  1132        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1152        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1172        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1192        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
  1212        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +


EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400
FIT task completed
write "Test FR 88195"
Test FR 88195
write "The histogram should not be displayed because phist is not specified"
The histogram should not be displayed because phist is not specified
gen a.dat 200 200 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a.dat b.dat perc=.5 'byte
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   199.000 STANDARD DEVIATION=    81.649 NUMBER OF ELEMENTS=   40000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   199.005 STANDARD DEVIATION=    81.644 NUMBER OF ELEMENTS=   39999

MINIMUM DN OF     13   SCALED TO     0

MAXIMUM DN OF    385   SCALED TO   255
FIT task completed
write "Test with nl and ns bigger then file size."
Test with nl and ns bigger then file size.
gen a.dat 200 200 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit inp=a.dat out=a.out sl=1 nl=800 ss=1 ns=800  +
lvalue=0 hvalue=255  area=(1,10,20,40)
Beginning VICAR task fit

FIT version 5 August, 2003
INVALID VALUE OF NL SET TO NL=200
INVALID VALUE OF NS SET TO NS=200

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

MINIMUM DN OF      9   SCALED TO     0

MAXIMUM DN OF     67   SCALED TO   255
FIT task completed
fit inp=a.dat out=a.out size=(1,1,800,800)  +
lvalue=0 hvalue=255  area=(1,10,20,40)
Beginning VICAR task fit

FIT version 5 August, 2003
INVALID VALUE OF NL SET TO NL=200
INVALID VALUE OF NS SET TO NS=200

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

MINIMUM DN OF      9   SCALED TO     0

MAXIMUM DN OF     67   SCALED TO   255
FIT task completed
write "Test 3D (multibanded) processing capability (simple)"
Test 3D (multibanded) processing capability (simple)
gen a NS=10 NL=10 NB=10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    13.500 STANDARD DEVIATION=     4.975 NUMBER OF ELEMENTS=    1000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    13.514 STANDARD DEVIATION=     4.959 NUMBER OF ELEMENTS=     999
FIT task completed
fit a NB=4
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    10.500 STANDARD DEVIATION=     4.213 NUMBER OF ELEMENTS=     400

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    10.526 STANDARD DEVIATION=     4.185 NUMBER OF ELEMENTS=     399
FIT task completed
write "Test 3D (multibanded) processing capability (linc, sinc)"
Test 3D (multibanded) processing capability (linc, sinc)
gen a NS=10 NL=10 NB=10 'half ival=-100 sinc=20 linc=50
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a
Beginning VICAR task fit

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   219.500 STANDARD DEVIATION=   154.704 NUMBER OF ELEMENTS=    1000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   219.500 STANDARD DEVIATION=   154.704 NUMBER OF ELEMENTS=    1000
FIT task completed
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create orig_fit_3dcase_1.log
tstfit
write "This is a test for FIT"
This is a test for FIT
write ""

write "Test halfword output,SIZE,PERC,LVAL,HVAL,EXCLUDE,EHIST"
Test halfword output,SIZE,PERC,LVAL,HVAL,EXCLUDE,EHIST
write ""

gen a 20 20 'half ival=-100 sinc=20 linc=50
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a b (2,2,18,18) perc=1. lval=10 hval=300 exclude=(-32768,0) 'ehist
Beginning VICAR task fit

FIT version Mar 9, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   279.389 NUMBER OF ELEMENTS=     324

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   568.634 STANDARD DEVIATION=   276.412 NUMBER OF ELEMENTS=     322

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

     8        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    16        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    24        1    0.311   **************************    +         +         +         +         +         +         +         +         +

    40        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    48        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    56        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    64        2    0.621   ***************************************************         +         +         +         +         +         +

    80        1    0.311   **************************    +         +         +         +         +         +         +         +         +
    88        2    0.621   ***************************************************         +         +         +         +         +         +
    96        1    0.311   **************************    +         +         +         +         +         +         +         +         +
   104        2    0.621   ***************************************************         +         +         +         +         +         +

   120        2    0.621   ***************************************************         +         +         +         +         +         +
   128        2    0.621   ***************************************************         +         +         +         +         +         +
   136        2    0.621   ***************************************************         +         +         +         +         +         +
   144        2    0.621   ***************************************************         +         +         +         +         +         +

   160        2    0.621   ***************************************************         +         +         +         +         +         +
   168        3    0.932   ****************************************************************************    +         +         +         +
   176        2    0.621   ***************************************************         +         +         +         +         +         +
   184        3    0.932   ****************************************************************************    +         +         +         +

   200        2    0.621   ***************************************************         +         +         +         +         +         +
   208        3    0.932   ****************************************************************************    +         +         +         +
   216        3    0.932   ****************************************************************************    +         +         +         +
   224        3    0.932   ****************************************************************************    +         +         +         +

   240        3    0.932   ****************************************************************************    +         +         +         +
   248        3    0.932   ****************************************************************************    +         +         +         +
   256        3    0.932   ****************************************************************************    +         +         +         +
   264        4    1.242   *****************************************************************************************************         +

   280        3    0.932   ****************************************************************************    +         +         +         +
   288        4    1.242   *****************************************************************************************************         +
   296        3    0.932   ****************************************************************************    +         +         +         +
   304        4    1.242   *****************************************************************************************************         +

   320        4    1.242   *****************************************************************************************************         +
   328        3    0.932   ****************************************************************************    +         +         +         +
   336        4    1.242   *****************************************************************************************************         +
   344        3    0.932   ****************************************************************************    +         +         +         +

   360        4    1.242   *****************************************************************************************************         +
   368        4    1.242   *****************************************************************************************************         +
   376        3    0.932   ****************************************************************************    +         +         +         +
   384        4    1.242   *****************************************************************************************************         +

   400        3    0.932   ****************************************************************************    +         +         +         +
   408        4    1.242   *****************************************************************************************************         +
   416        4    1.242   *****************************************************************************************************         +
   424        3    0.932   ****************************************************************************    +         +         +         +

   440        4    1.242   *****************************************************************************************************         +
   448        3    0.932   ****************************************************************************    +         +         +         +
   456        4    1.242   *****************************************************************************************************         +
   464        4    1.242   *****************************************************************************************************         +

   480        3    0.932   ****************************************************************************    +         +         +         +
   488        4    1.242   *****************************************************************************************************         +
   496        3    0.932   ****************************************************************************    +         +         +         +
   504        4    1.242   *****************************************************************************************************         +

   520        4    1.242   *****************************************************************************************************         +
   528        3    0.932   ****************************************************************************    +         +         +         +
   536        4    1.242   *****************************************************************************************************         +
   544        3    0.932   ****************************************************************************    +         +         +         +

   560        4    1.242   *****************************************************************************************************         +
   568        4    1.242   *****************************************************************************************************         +
   576        3    0.932   ****************************************************************************    +         +         +         +
   584        4    1.242   *****************************************************************************************************         +

   600        3    0.932   ****************************************************************************    +         +         +         +
   608        4    1.242   *****************************************************************************************************         +
   616        4    1.242   *****************************************************************************************************         +
   624        3    0.932   ****************************************************************************    +         +         +         +

   640        4    1.242   *****************************************************************************************************         +
   648        3    0.932   ****************************************************************************    +         +         +         +
   656        4    1.242   *****************************************************************************************************         +
   664        4    1.242   *****************************************************************************************************         +

   680        3    0.932   ****************************************************************************    +         +         +         +
   688        4    1.242   *****************************************************************************************************         +
   696        3    0.932   ****************************************************************************    +         +         +         +
   704        4    1.242   *****************************************************************************************************         +

   720        4    1.242   *****************************************************************************************************         +
   728        3    0.932   ****************************************************************************    +         +         +         +
   736        4    1.242   *****************************************************************************************************         +
   744        3    0.932   ****************************************************************************    +         +         +         +

   760        4    1.242   *****************************************************************************************************         +
   768        4    1.242   *****************************************************************************************************         +
   776        3    0.932   ****************************************************************************    +         +         +         +
   784        4    1.242   *****************************************************************************************************         +

   800        3    0.932   ****************************************************************************    +         +         +         +
   808        4    1.242   *****************************************************************************************************         +
   816        4    1.242   *****************************************************************************************************         +
   824        3    0.932   ****************************************************************************    +         +         +         +

   840        4    1.242   *****************************************************************************************************         +
   848        3    0.932   ****************************************************************************    +         +         +         +
   856        4    1.242   *****************************************************************************************************         +
   864        3    0.932   ****************************************************************************    +         +         +         +

   880        3    0.932   ****************************************************************************    +         +         +         +
   888        3    0.932   ****************************************************************************    +         +         +         +
   896        3    0.932   ****************************************************************************    +         +         +         +
   904        3    0.932   ****************************************************************************    +         +         +         +

   920        3    0.932   ****************************************************************************    +         +         +         +
   928        2    0.621   ***************************************************         +         +         +         +         +         +
   936        3    0.932   ****************************************************************************    +         +         +         +
   944        2    0.621   ***************************************************         +         +         +         +         +         +

   960        3    0.932   ****************************************************************************    +         +         +         +
   968        2    0.621   ***************************************************         +         +         +         +         +         +
   976        2    0.621   ***************************************************         +         +         +         +         +         +
   984        2    0.621   ***************************************************         +         +         +         +         +         +

  1000        2    0.621   ***************************************************         +         +         +         +         +         +
  1008        2    0.621   ***************************************************         +         +         +         +         +         +
  1016        2    0.621   ***************************************************         +         +         +         +         +         +
  1024        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1040        2    0.621   ***************************************************         +         +         +         +         +         +
  1048        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1056        2    0.621   ***************************************************         +         +         +         +         +         +
  1064        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1080        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1088        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1096        1    0.311   **************************    +         +         +         +         +         +         +         +         +
  1104        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1120        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1136        1    0.311   **************************    +         +         +         +         +         +         +         +         +

  1160        1    0.311   **************************    +         +         +         +         +         +         +         +         +


MINIMUM DN OF     30   SCALED TO    10

MAXIMUM DN OF   1120   SCALED TO   300
FIT task completed
list b
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
 Task:FIT       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        10    10    10    10    15    21    26    31    37    42    47    53    58    63    69
      2        10    13    18    23    29    34    39    45    50    55    61    66    71    77    82
      3        21    26    31    37    42    47    53    58    63    69    74    79    84    90    95
      4        34    39    45    50    55    61    66    71    77    82    87    92    98   103   108
      5        47    53    58    63    69    74    79    84    90    95   100   106   111   116   122
      6        61    66    71    77    82    87    92    98   103   108   114   119   124   130   135
      7        74    79    84    90    95   100   106   111   116   122   127   132   138   143   148
      8        87    92    98   103   108   114   119   124   130   135   140   146   151   156   162
      9       100   106   111   116   122   127   132   138   143   148   154   159   164   170   175
     10       114   119   124   130   135   140   146   151   156   162   167   172   178   183   188
     11       127   132   138   143   148   154   159   164   170   175   180   186   191   196   202
     12       140   146   151   156   162   167   172   178   183   188   194   199   204   210   215
     13       154   159   164   170   175   180   186   191   196   202   207   212   218   223   228
     14       167   172   178   183   188   194   199   204   210   215   220   226   231   236   241
     15       180   186   191   196   202   207   212   218   223   228   233   239   244   249   255
     16       194   199   204   210   215   220   226   231   236   241   247   252   257   263   268
     17       207   212   218   223   228   233   239   244   249   255   260   265   271   276   281
     18       220   226   231   236   241   247   252   257   263   268   273   279   284   289   295

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
 Task:FIT       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
     Samp      16    17    18
   Line
      1        74    79    84
      2        87    92    98
      3       100   106   111
      4       114   119   124
      5       127   132   138
      6       140   146   151
      7       154   159   164
      8       167   172   178
      9       180   186   191
     10       194   199   204
     11       207   212   218
     12       220   226   231
     13       233   239   244
     14       247   252   257
     15       260   265   271
     16       273   279   284
     17       287   292   297
     18       300   300   300
write ""

write "Test byte output, LPERC, HPERC, PHIST"
Test byte output, LPERC, HPERC, PHIST
write ""

fit a b  lperc=2. hperc=4. 'include  'phist  'byte
Beginning VICAR task fit

FIT version Mar 9, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

  -104        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -80        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -64        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -56        1    0.250   **************************    +         +         +         +         +         +         +         +         +

   -40        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -32        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -24        1    0.250   **************************    +         +         +         +         +         +         +         +         +
   -16        1    0.250   **************************    +         +         +         +         +         +         +         +         +

     0        2    0.500   ***************************************************         +         +         +         +         +         +
     8        1    0.250   **************************    +         +         +         +         +         +         +         +         +
    16        2    0.500   ***************************************************         +         +         +         +         +         +
    24        1    0.250   **************************    +         +         +         +         +         +         +         +         +

    40        2    0.500   ***************************************************         +         +         +         +         +         +
    48        2    0.500   ***************************************************         +         +         +         +         +         +
    56        2    0.500   ***************************************************         +         +         +         +         +         +
    64        2    0.500   ***************************************************         +         +         +         +         +         +

    80        2    0.500   ***************************************************         +         +         +         +         +         +
    88        2    0.500   ***************************************************         +         +         +         +         +         +
    96        3    0.750   ****************************************************************************    +         +         +         +
   104        2    0.500   ***************************************************         +         +         +         +         +         +

   120        3    0.750   ****************************************************************************    +         +         +         +
   128        2    0.500   ***************************************************         +         +         +         +         +         +
   136        3    0.750   ****************************************************************************    +         +         +         +
   144        3    0.750   ****************************************************************************    +         +         +         +

   160        3    0.750   ****************************************************************************    +         +         +         +
   168        3    0.750   ****************************************************************************    +         +         +         +
   176        3    0.750   ****************************************************************************    +         +         +         +
   184        3    0.750   ****************************************************************************    +         +         +         +

   200        4    1.000   *****************************************************************************************************         +
   208        3    0.750   ****************************************************************************    +         +         +         +
   216        4    1.000   *****************************************************************************************************         +
   224        3    0.750   ****************************************************************************    +         +         +         +

   240        4    1.000   *****************************************************************************************************         +
   248        4    1.000   *****************************************************************************************************         +
   256        4    1.000   *****************************************************************************************************         +
   264        4    1.000   *****************************************************************************************************         +

   280        4    1.000   *****************************************************************************************************         +
   288        4    1.000   *****************************************************************************************************         +
   296        4    1.000   *****************************************************************************************************         +
   304        4    1.000   *****************************************************************************************************         +

   320        4    1.000   *****************************************************************************************************         +
   328        4    1.000   *****************************************************************************************************         +
   336        4    1.000   *****************************************************************************************************         +
   344        4    1.000   *****************************************************************************************************         +

   360        4    1.000   *****************************************************************************************************         +
   368        4    1.000   *****************************************************************************************************         +
   376        4    1.000   *****************************************************************************************************         +
   384        4    1.000   *****************************************************************************************************         +

   400        4    1.000   *****************************************************************************************************         +
   408        4    1.000   *****************************************************************************************************         +
   416        4    1.000   *****************************************************************************************************         +
   424        4    1.000   *****************************************************************************************************         +

   440        4    1.000   *****************************************************************************************************         +
   448        4    1.000   *****************************************************************************************************         +
   456        4    1.000   *****************************************************************************************************         +
   464        4    1.000   *****************************************************************************************************         +

   480        4    1.000   *****************************************************************************************************         +
   488        4    1.000   *****************************************************************************************************         +
   496        4    1.000   *****************************************************************************************************         +
   504        4    1.000   *****************************************************************************************************         +

   520        4    1.000   *****************************************************************************************************         +
   528        4    1.000   *****************************************************************************************************         +
   536        4    1.000   *****************************************************************************************************         +
   544        4    1.000   *****************************************************************************************************         +

   560        4    1.000   *****************************************************************************************************         +
   568        4    1.000   *****************************************************************************************************         +
   576        4    1.000   *****************************************************************************************************         +
   584        4    1.000   *****************************************************************************************************         +

   600        4    1.000   *****************************************************************************************************         +
   608        4    1.000   *****************************************************************************************************         +
   616        4    1.000   *****************************************************************************************************         +
   624        4    1.000   *****************************************************************************************************         +

   640        4    1.000   *****************************************************************************************************         +
   648        4    1.000   *****************************************************************************************************         +
   656        4    1.000   *****************************************************************************************************         +
   664        4    1.000   *****************************************************************************************************         +

   680        4    1.000   *****************************************************************************************************         +
   688        4    1.000   *****************************************************************************************************         +
   696        4    1.000   *****************************************************************************************************         +
   704        4    1.000   *****************************************************************************************************         +

   720        4    1.000   *****************************************************************************************************         +
   728        4    1.000   *****************************************************************************************************         +
   736        4    1.000   *****************************************************************************************************         +
   744        4    1.000   *****************************************************************************************************         +

   760        4    1.000   *****************************************************************************************************         +
   768        4    1.000   *****************************************************************************************************         +
   776        4    1.000   *****************************************************************************************************         +
   784        4    1.000   *****************************************************************************************************         +

   800        4    1.000   *****************************************************************************************************         +
   808        4    1.000   *****************************************************************************************************         +
   816        4    1.000   *****************************************************************************************************         +
   824        4    1.000   *****************************************************************************************************         +

   840        4    1.000   *****************************************************************************************************         +
   848        4    1.000   *****************************************************************************************************         +
   856        4    1.000   *****************************************************************************************************         +
   864        4    1.000   *****************************************************************************************************         +

   880        4    1.000   *****************************************************************************************************         +
   888        4    1.000   *****************************************************************************************************         +
   896        3    0.750   ****************************************************************************    +         +         +         +
   904        4    1.000   *****************************************************************************************************         +

   920        3    0.750   ****************************************************************************    +         +         +         +
   928        4    1.000   *****************************************************************************************************         +
   936        3    0.750   ****************************************************************************    +         +         +         +
   944        3    0.750   ****************************************************************************    +         +         +         +

   960        3    0.750   ****************************************************************************    +         +         +         +
   968        3    0.750   ****************************************************************************    +         +         +         +
   976        3    0.750   ****************************************************************************    +         +         +         +
   984        3    0.750   ****************************************************************************    +         +         +         +

  1000        2    0.500   ***************************************************         +         +         +         +         +         +
  1008        3    0.750   ****************************************************************************    +         +         +         +
  1016        2    0.500   ***************************************************         +         +         +         +         +         +
  1024        3    0.750   ****************************************************************************    +         +         +         +

  1040        2    0.500   ***************************************************         +         +         +         +         +         +
  1048        2    0.500   ***************************************************         +         +         +         +         +         +
  1056        2    0.500   ***************************************************         +         +         +         +         +         +
  1064        2    0.500   ***************************************************         +         +         +         +         +         +

  1080        2    0.500   ***************************************************         +         +         +         +         +         +
  1088        2    0.500   ***************************************************         +         +         +         +         +         +
  1096        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1104        2    0.500   ***************************************************         +         +         +         +         +         +

  1120        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1128        2    0.500   ***************************************************         +         +         +         +         +         +
  1136        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1144        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1160        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1168        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1176        1    0.250   **************************    +         +         +         +         +         +         +         +         +
  1184        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1208        1    0.250   **************************    +         +         +         +         +         +         +         +         +

  1224        1    0.250   **************************    +         +         +         +         +         +         +         +         +


MINIMUM DN OF      0   SCALED TO     0

MAXIMUM DN OF   1080   SCALED TO   255
FIT task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
 Task:FIT       User:ntt       Date_Time:Tue Aug  5 17:50:02 2003
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   5   9  14  19  24  28  33  38  42  47  52  57  61  66
      2       0   0   0   2   7  12  17  21  26  31  35  40  45  50  54  59  64  68  73  78
      3       0   5   9  14  19  24  28  33  38  42  47  52  57  61  66  71  76  80  85  90
      4      12  17  21  26  31  35  40  45  50  54  59  64  68  73  78  83  87  92  97 102
      5      24  28  33  38  42  47  52  57  61  66  71  76  80  85  90  94  99 104 109 113
      6      35  40  45  50  54  59  64  68  73  78  83  87  92  97 102 106 111 116 120 125
      7      47  52  57  61  66  71  76  80  85  90  94  99 104 109 113 118 123 128 132 137
      8      59  64  68  73  78  83  87  92  97 102 106 111 116 120 125 130 135 139 144 149
      9      71  76  80  85  90  94  99 104 109 113 118 123 128 132 137 142 146 151 156 161
     10      83  87  92  97 102 106 111 116 120 125 130 135 139 144 149 153 158 163 168 172
     11      94  99 104 109 113 118 123 128 132 137 142 146 151 156 161 165 170 175 179 184
     12     106 111 116 120 125 130 135 139 144 149 153 158 163 168 172 177 182 187 191 196
     13     118 123 128 132 137 142 146 151 156 161 165 170 175 179 184 189 194 198 203 208
     14     130 135 139 144 149 153 158 163 168 172 177 182 187 191 196 201 205 210 215 220
     15     142 146 151 156 161 165 170 175 179 184 189 194 198 203 208 213 217 222 227 231
     16     153 158 163 168 172 177 182 187 191 196 201 205 210 215 220 224 229 234 238 243
     17     165 170 175 179 184 189 194 198 203 208 213 217 222 227 231 236 241 246 250 255
     18     177 182 187 191 196 201 205 210 215 220 224 229 234 238 243 248 253 255 255 255
     19     189 194 198 203 208 213 217 222 227 231 236 241 246 250 255 255 255 255 255 255
     20     201 205 210 215 220 224 229 234 238 243 248 253 255 255 255 255 255 255 255 255
write ""

write "Test SPEED,AREA"
Test SPEED,AREA
write ""

fit a b  speed=2 area=(4,4,16,16) 'byte
Beginning VICAR task fit

FIT version Mar 9, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   610.000 STANDARD DEVIATION=   246.982 NUMBER OF ELEMENTS=     128

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   610.000 STANDARD DEVIATION=   246.982 NUMBER OF ELEMENTS=     128

MINIMUM DN OF    110   SCALED TO     0

MAXIMUM DN OF   1110   SCALED TO   255
FIT task completed
list b
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Aug  5 17:50:01 2003
 Task:FIT       User:ntt       Date_Time:Tue Aug  5 17:50:02 2003
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   3   8  13  18  23  28  33  38  43
      2       0   0   0   0   0   0   0   0   0   5  10  15  20  25  31  36  41  46  51  56
      3       0   0   0   0   0   0   3   8  13  18  23  28  33  38  43  48  54  59  64  69
      4       0   0   0   0   5  10  15  20  25  31  36  41  46  51  56  61  66  71  76  82
      5       0   3   8  13  18  23  28  33  38  43  48  54  59  64  69  74  79  84  89  94
      6      10  15  20  25  31  36  41  46  51  56  61  66  71  76  82  87  92  97 102 107
      7      23  28  33  38  43  48  54  59  64  69  74  79  84  89  94  99 105 110 115 120
      8      36  41  46  51  56  61  66  71  76  82  87  92  97 102 107 112 117 122 127 133
      9      48  54  59  64  69  74  79  84  89  94  99 105 110 115 120 125 130 135 140 145
     10      61  66  71  76  82  87  92  97 102 107 112 117 122 127 133 138 143 148 153 158
     11      74  79  84  89  94  99 105 110 115 120 125 130 135 140 145 150 156 161 166 171
     12      87  92  97 102 107 112 117 122 127 133 138 143 148 153 158 163 168 173 179 184
     13      99 105 110 115 120 125 130 135 140 145 150 156 161 166 171 176 181 186 191 196
     14     112 117 122 127 133 138 143 148 153 158 163 168 173 179 184 189 194 199 204 209
     15     125 130 135 140 145 150 156 161 166 171 176 181 186 191 196 201 207 212 217 222
     16     138 143 148 153 158 163 168 173 179 184 189 194 199 204 209 214 219 224 230 235
     17     150 156 161 166 171 176 181 186 191 196 201 207 212 217 222 227 232 237 242 247
     18     163 168 173 179 184 189 194 199 204 209 214 219 224 230 235 240 245 250 255 255
     19     176 181 186 191 196 201 207 212 217 222 227 232 237 242 247 252 255 255 255 255
     20     189 194 199 204 209 214 219 224 230 235 240 245 250 255 255 255 255 255 255 255
write ""

write "Test no output file,HINC,LOG"
Test no output file,HINC,LOG
write ""

fit a hinc=20 nlines=80 'log 'phist
Beginning VICAR task fit

FIT version Mar 9, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400
Horizontal scale is logarithmic

GRAY       FREQ  PERCENT   0        10        20        30        40        50        60        70        80        90       100

  -108        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
   -88        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
   -68        2    0.500   **********************************      +         +         +         +         +         +         +         +
   -48        2    0.500   **********************************      +         +         +         +         +         +         +         +
   -28        2    0.500   **********************************      +         +         +         +         +         +         +         +
    -8        3    0.750   *****************************************************       +         +         +         +         +         +
    12        3    0.750   *****************************************************       +         +         +         +         +         +
    32        4    1.000   *******************************************************************   +         +         +         +         +
    52        4    1.000   *******************************************************************   +         +         +         +         +
    72        4    1.000   *******************************************************************   +         +         +         +         +
    92        5    1.250   ******************************************************************************  +         +         +         +
   112        5    1.250   ******************************************************************************  +         +         +         +
   132        6    1.500   ***************************************************************************************   +         +         +
   152        6    1.500   ***************************************************************************************   +         +         +
   172        6    1.500   ***************************************************************************************   +         +         +
   192        7    1.750   **********************************************************************************************      +         +
   212        7    1.750   **********************************************************************************************      +         +
   232        8    2.000   *****************************************************************************************************         +
   252        8    2.000   *****************************************************************************************************         +
   272        8    2.000   *****************************************************************************************************         +
   292        8    2.000   *****************************************************************************************************         +
   312        8    2.000   *****************************************************************************************************         +
   332        8    2.000   *****************************************************************************************************         +
   352        8    2.000   *****************************************************************************************************         +
   372        8    2.000   *****************************************************************************************************         +
   392        8    2.000   *****************************************************************************************************         +
   412        8    2.000   *****************************************************************************************************         +
   432        8    2.000   *****************************************************************************************************         +
   452        8    2.000   *****************************************************************************************************         +
   472        8    2.000   *****************************************************************************************************         +
   492        8    2.000   *****************************************************************************************************         +
   512        8    2.000   *****************************************************************************************************         +
   532        8    2.000   *****************************************************************************************************         +
   552        8    2.000   *****************************************************************************************************         +
   572        8    2.000   *****************************************************************************************************         +
   592        8    2.000   *****************************************************************************************************         +
   612        8    2.000   *****************************************************************************************************         +
   632        8    2.000   *****************************************************************************************************         +
   652        8    2.000   *****************************************************************************************************         +
   672        8    2.000   *****************************************************************************************************         +
   692        8    2.000   *****************************************************************************************************         +
   712        8    2.000   *****************************************************************************************************         +
   732        8    2.000   *****************************************************************************************************         +
   752        8    2.000   *****************************************************************************************************         +
   772        8    2.000   *****************************************************************************************************         +
   792        8    2.000   *****************************************************************************************************         +
   812        8    2.000   *****************************************************************************************************         +
   832        8    2.000   *****************************************************************************************************         +
   852        8    2.000   *****************************************************************************************************         +
   872        8    2.000   *****************************************************************************************************         +
   892        7    1.750   **********************************************************************************************      +         +
   912        7    1.750   **********************************************************************************************      +         +
   932        6    1.500   ***************************************************************************************   +         +         +
   952        6    1.500   ***************************************************************************************   +         +         +
   972        6    1.500   ***************************************************************************************   +         +         +
   992        5    1.250   ******************************************************************************  +         +         +         +
  1012        5    1.250   ******************************************************************************  +         +         +         +
  1032        4    1.000   *******************************************************************   +         +         +         +         +
  1052        4    1.000   *******************************************************************   +         +         +         +         +
  1072        4    1.000   *******************************************************************   +         +         +         +         +
  1092        3    0.750   *****************************************************       +         +         +         +         +         +
  1112        3    0.750   *****************************************************       +         +         +         +         +         +
  1132        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1152        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1172        2    0.500   **********************************      +         +         +         +         +         +         +         +
  1192        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +
  1212        1    0.250   *         +         +         +         +         +         +         +         +         +         +         +


EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   565.000 STANDARD DEVIATION=   310.524 NUMBER OF ELEMENTS=     400
FIT task completed
write "Test FR 88195"
Test FR 88195
write "The histogram should not be displayed because phist is not specified"
The histogram should not be displayed because phist is not specified
gen a.dat 200 200 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a.dat b.dat perc=.5 'byte
Beginning VICAR task fit

FIT version Mar 9, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   199.000 STANDARD DEVIATION=    81.649 NUMBER OF ELEMENTS=   40000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   199.005 STANDARD DEVIATION=    81.644 NUMBER OF ELEMENTS=   39999

MINIMUM DN OF     13   SCALED TO     0

MAXIMUM DN OF    385   SCALED TO   255
FIT task completed
write "Test with nl and ns bigger then file size."
Test with nl and ns bigger then file size.
gen a.dat 200 200 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit inp=a.dat out=a.out sl=1 nl=800 ss=1 ns=800  +
lvalue=0 hvalue=255  area=(1,10,20,40)
Beginning VICAR task fit

FIT version Mar 9, 2003
INVALID VALUE OF NL SET TO NL=200
INVALID VALUE OF NS SET TO NS=200

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

MINIMUM DN OF      9   SCALED TO     0

MAXIMUM DN OF     67   SCALED TO   255
FIT task completed
fit inp=a.dat out=a.out size=(1,1,800,800)  +
lvalue=0 hvalue=255  area=(1,10,20,40)
Beginning VICAR task fit

FIT version Mar 9, 2003
INVALID VALUE OF NL SET TO NL=200
INVALID VALUE OF NS SET TO NS=200

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=    38.000 STANDARD DEVIATION=    12.903 NUMBER OF ELEMENTS=     800

MINIMUM DN OF      9   SCALED TO     0

MAXIMUM DN OF     67   SCALED TO   255
FIT task completed
write "Test 3D (multibanded) processing capability (simple)"
Test 3D (multibanded) processing capability (simple)
gen a NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a
Beginning VICAR task fit

FIT version Mar 9, 2003
***Invalid input data format
***Input image must be in halfword format
***FIT task cancelled
 ** ABEND called **
continue
write "Test 3D (multibanded) processing capability (linc, sinc)"
Test 3D (multibanded) processing capability (linc, sinc)
gen a NS=10 NL=10 NB=10 'half ival=-100 sinc=20 linc=50
Beginning VICAR task gen
GEN Version 6
GEN task completed
fit a
Beginning VICAR task fit

FIT version Mar 9, 2003
[VIC2-GENERR] Exception in XVREAD, processing file: a
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
end-proc
disable-log
$ Return
$!#############################################################################
