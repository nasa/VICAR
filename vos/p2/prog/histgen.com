$!****************************************************************************
$!
$! Build proc for MIPL module histgen
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:35
$!
$! Execute by entering:		$ @histgen
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
$ write sys$output "*** module histgen ***"
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
$ write sys$output "Invalid argument given to histgen.com file -- ", primary
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
$   if F$SEARCH("histgen.imake") .nes. ""
$   then
$      vimake histgen
$      purge histgen.bld
$   else
$      if F$SEARCH("histgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histgen.bld "STD"
$   else
$      @histgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histgen.com -mixed -
	-s histgen.f -
	-i histgen.imake -
	-p histgen.pdf -
	-t tsthistgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create histgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR program HISTGEN
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      COMMON/C1/HIST(-32768:32767),IBUF(32768)
      INTEGER*4 BUF(32771),HIST
      INTEGER*4 OUNIT,ILOW,IHIGH
      INTEGER*2 IBUF
      REAL*8 DMEAN,DSIGMA
      REAL*4 RMEAN,RSIGMA
      CHARACTER*8 FORMAT
      CHARACTER*5 IBISFRMT(8)
      INTEGER  IBIS,NLO,NSO,STATUS
      CHARACTER*10 ORGANIZATION
      CHARACTER*32 INSTANCE_NAME
      CHARACTER*20 SUBTYPE_NAME
      CHARACTER*40 MEMBER_NAME
      CHARACTER*10 SCLTYPE
C
      EQUIVALENCE(BUF,HIST(-3))
C
      CALL XVMESSAGE(' HISTGEN version July 26, 1999',' ')
C     ... Initialize ibis constants
      do i=1,8
         IBISFRMT(i)='FULL'
      enddo
      ORGANIZATION='COLUMN'
      MEMBER_NAME='HISTOGRAM'
      INSTANCE_NAME='HISTOGRAM FOR MASKV OR VGRMASK'
      SCLTYPE='0 - MAXD'
      SUBTYPE_NAME='STATISTICS'
C     ... Open input file and ge parameters
      CALL OPENINP(iunit,format,ilow,ihigh,maxd,*999)
C     ....Determine size of output histogram file (NLOxNSO)
      NWORD = MAXD+4	!Total # of words for output histogram
      NSO = NWORD
      NLO = 1
C     ....Open output file
      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
C     ..... Open ibis file
      call ibis_file_open(OUNIT,IBIS,'write',NLO,NSO,IBISFRMT,
     +                     ORGANIZATION,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUNIT,STATUS,1)
      CALL IBIS_FILE_SET(IBIS,'type',SUBTYPE_NAME,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C     .... Zero Array
      CALL ZIA(HIST(ILOW),IHIGH-ILOW+1)
C     .... Compute Histogram 
      CALL COMPHIST(IUNIT,FORMAT,hist,ibuf,npts,*999)
C     .... Comput mean & sigma
      CALL COMPSTAT(NPTS,ILOW,IHIGH,MAXD,hist,dmean,dsigma)
C     ....Write histogram
      BUF(1) = MAXD + 1		!Number of DN levels in output histogram
      BUF(2)= 1000.*DMEAN + .5
      BUF(3)= 1000.*DSIGMA + .5

      CALL IBIS_COLUMN_WRITE(IBIS,BUF,1,1,NSO,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
C         ... Install column ID into the HISTOGRAM member ...
      call icl_new_statistics(IBIS,1,1,0,0,MEMBER_NAME,
     &                        INSTANCE_NAME,STATUS)
      IF (STATUS.LT.0) CALL IBIS_SIGNAL(IBIS,STATUS,1)

C     ... Install the STATISTICS  property values
      CALL XLADD(OUNIT,'PROPERTY','SCALE_TYPE',SCLTYPE,IND,
     &       'FORMAT','STRING','PROPERTY','STATISTICS',' ')
      CALL XLADD(OUNIT,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',
     &     BUF(1),IND,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      RMEAN = DMEAN
      CALL XLADD(OUNIT,'PROPERTY','MEAN_VALUE',RMEAN,IND,
     &       'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      RSIGMA = DSIGMA
      CALL XLADD(OUNIT,'PROPERTY','STANDARD_DEVIATION_VALUE',
     &     RSIGMA,IND,'FORMAT','REAL','PROPERTY','STATISTICS',' ')

      CALL IBIS_FILE_CLOSE(IBIS,0,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL XVCLOSE (OUNIT,STATUS,' ')
      CALL XVMESSAGE(' HISTGEN task completed', ' ')
      RETURN

  999 CALL XVMESSAGE(' ***HISTGEN task cancelled', ' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open the input image, get data format, and determine input and output
C DN limits (ilow to ihigh and 0 to MAXD).
C
      SUBROUTINE OPENINP(iunit,format,ilow,ihigh,maxd,*)
C      COMMON/C2/FORMAT,ILOW,IHIGH
      CHARACTER*8 FORMAT	!Input data format (BYTE or HALF)
      INTEGER*4  ILOW,IHIGH

      CALL XVUNIT(iunit,'INP',1,IND,' ')
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA', ' ')
C     ....Get FORMAT and MAXD
      CALL XVGET(IUNIT,IND,'FORMAT',FORMAT, ' ')
      IF (FORMAT.EQ.'BYTE') THEN
         MAXD = 255
         ILOW = 0
         IHIGH = 255
      ELSE IF (FORMAT.EQ.'HALF') THEN
         MAXD = 32767
         ILOW = -32768
         IHIGH = 32767
      ELSE
         CALL XVMESSAGE(' ***Invalid input data format', ' ')
         RETURN1
      ENDIF

      CALL XVPARM('MAXD',IVAL,ICOUNT,IDEF,1)
      IF (ICOUNT.EQ.1) MAXD=IVAL
      RETURN
      END
C Compute the histogram
C
      SUBROUTINE COMPHIST(IUNIT,FORMAT,hist,ibuf,npts,*)
C      COMMON/C2/FORMAT,ILOW,IHIGH
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 IBUF(32768)
      INTEGER*4 SL,SS,EL

      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      IF (SL+NL-1 .GT. NLI) GOTO 902
      IF (SS+NS-1 .GT. NSI) GOTO 903
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,ibuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     1 NS,' ')
         IF (FORMAT.EQ.'BYTE') THEN
            CALL HSUB(1,NS,IBUF,HIST(0),0,255)
         ELSE
            DO J=1,NS
               IDN = IBUF(J)
               HIST(IDN) = HIST(IDN) + 1
            ENDDO
         ENDIF
      ENDDO

      NPTS = NL*NS
      RETURN

  902 CALL XVMESSAGE(' ***Number of lines requested exceeds input size'
     & ,' ')
      GOTO 999
  903 CALL XVMESSAGE(
     & ' ***Number of samples requested exceeds input size',' ')
  999 RETURN1
      END
C Compute mean and standard deviation.  Modify histogram to set all values
C below 0 DN to 0 DN and all values above MAXD to MAXD.
C
      SUBROUTINE COMPSTAT(NPTS,ILOW,IHIGH,MAXD,hist,dmean,dsigma)
      INTEGER*4 HIST(-32768:32767)
      REAL*8 DMEAN,DSIGMA
      CHARACTER*80 MSG
  800 FORMAT(' Number of DN values below 0=',I7)
  801 FORMAT(' Number of DN values above',I5,' =',I7)

      DMEAN = 0.0D0
      DSIGMA = 0.0D0
      NLOW = 0
      NHIGH = 0

      DO J=ILOW,IHIGH
         NPIXELS = HIST(J)
	 DN = J
         DMEAN = DMEAN + NPIXELS*DN
         DSIGMA = DSIGMA + NPIXELS*DN**2
         IF (J.LT.0) THEN
            NLOW = NLOW + NPIXELS
         ELSE
            IF (J.GT.MAXD) NHIGH=NHIGH+NPIXELS
         ENDIF 
      ENDDO

      DMEAN = DMEAN/NPTS
      DSIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
C     ....Modify histogram
      HIST(0) = HIST(0) + NLOW
      HIST(MAXD) = HIST(MAXD) + NHIGH
C     ....Print number of values outside DN range
      IF (NLOW+NHIGH.GT.0) THEN
         WRITE(MSG,800) NLOW
         CALL XVMESSAGE( MSG, ' ')
         WRITE(MSG,801) MAXD,NHIGH
         CALL XVMESSAGE( MSG, ' ')
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histgen.imake
#define  PROGRAM   histgen

#define MODULE_LIST histgen.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create histgen.pdf
process help=*
parm INP type=STRING
parm OUT type=STRING
PARM SIZE TYPE=INTEGER COUNT=4      DEFAULT=(1,1,0,0)
PARM SL   TYPE=INTEGER DEFAULT=1
PARM SS   TYPE=INTEGER DEFAULT=1
PARM NL   TYPE=INTEGER DEFAULT=0
PARM NS   TYPE=INTEGER DEFAULT=0
PARM MAXD TYPE=INTEGER COUNT=(0:1) VALID=(0:32767)	DEFAULT=--
!# annot function="Image Display"
!# annot keywords=(compute,"mean value","standard deviation",IBIS,+
!#   STRETCH, halfword)
END-PROC
.TITLE
 Creates histogram file for byte or halfword image
.HELP
PURPOSE:

HISTGEN computes the histogram, mean value, and standard deviation of the
input image.  These values are output to an IBIS histogram file which is
suitable for input to programs STRETCH and MASKV.

EXECUTION:
         HISTGEN  INP  OUT  user-parameters...
where
  INP is the input image (byte or halfword)
  OUT is the output histogram file in 32-bit integer data format.

.page
OPERATION:

The histogram is first computed by reading the input image.  For byte data, the
histogram contains 256 DN levels ranging from 0 to 255.  For halfword data,
the histogram contains 64K DN levels ranging from -32768 to +32767.

The mean and standard deviation of the image is then computed from the resulting
histogram.

Prior to output, the histogram is modified so that the output histogram has a
DN range from 0 DN to MAXD DN (see MAXD parameter):
   All DN values less than zero are set to zero and
   All DN values greater than MAXD are set to MAXD.
Note, however, that the mean and standard deviation will reflect the full DN
range.

.page
OUTPUT HISTOGRAM FILE FORMAT:

The histogram is output as an IBIS tabular file containing one column and
MAXD+4 rows.  The data is stored in INTEGER*4 format as follows:

    Column 1 = Number of DN levels in the histogram (=MAXD+1)
    Column 2 = 1000*meanDN
    Column 3 = 1000*standard-deviation
    Column 4 = Number of pixels at 0 DN or lower
    Column 5 = Number of pixels at 1 DN
    Column 6 = Number of pixels at 2 DN
      .
      .
      .
    Column MAXD+4 = Number of pixels at MAXD DN or higher

.page
PROGRAM HISTORY:

Written by: John H. Reimer		June 10, 1981
Cognizant programmer: Gary Yagi, September 1989
Revisions:
 00-1-24  GMY  Add ILOW and IHIGH to call to HSUB (args no longer optional)
 99-7-25  GMY  Fix bug: Histogram was offset by 1 DN so that HIS(1) was HIS(0)
               Store mean and sigma as float in property label
               Update help file to document histogram file format (IBIS).
 95-8-22  AMS (CRI) As per FR87131 fixed to work with half format and maxd
               not specified. Now fits in single column.
 95-3-6   CRS (CRI) revised to create IBIS histogram file
 90-1-22  GMY  Float DN to fix integer overflow.
 89-9-12  GMY  Massive revisions to source code, help, and test files
		Fixed bug in MAXD parameter
 89-2-21  HBM  Calculated SIGMA in subroutine MAIN as floating
                    point to avoid a possible integer overflow
 89-2-6   CCM  Histogram data collection loop somewhat optimized
 85-8-20  JHR  CONVERT TO VICAR2
 84-3-28  CCA  PUT IN GLABEL AND SOME VICAR2
 83-4-1   LWK  adjust MAXD if output dataset too small

.LEVEL1
.VARI INP
 input image
.VARI OUT
 output histogram file
.VARI SIZE
 image size field
.VARI SL
 starting line
.VARI SS
 starting sample
.VARI NL
 number of lines
.VARI NS
 number of samples
.VARI MAXD
 max. input DN
.LEVEL2
.VARI INP
 Input image in byte or 16-bit integer (halfword) data format.
.VARI OUT
The histogram is output as an IBIS tabular file containing one column and
MAXD+4 rows.  The data is stored in INTEGER*4 format as follows:

    Column 1 = Number of DN levels in the histogram (=MAXD+1)
    Column 2 = 1000*meanDN
    Column 3 = 1000*standard-deviation
    Column 4 = Number of pixels at 0 DN or lower
    Column 5 = Number of pixels at 1 DN
    Column 6 = Number of pixels at 2 DN
      .
      .
      .
    Column MAXD+4 = Number of pixels at MAXD DN or higher
.VARI SIZE
 Standard VICAR size parameter, specifying the area of the input image to be
 used to generate the histogram.
.VARI MAXD
 Specifies the maximum DN in the output histogram.  I.e., the histogram will
 have a DN range from 0 to MAXD DN.   Default is 255 for byte, 32767 for
 halfword.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsthistgen.pdf
procedure

refgbl $echo
refgbl $autousage
refgbl $syschar

PARM SCR STRING DEFAULT="V2$SCRATCH"

body

let _onfail="continue"
let $echo="yes"
let $autousage="none"

WRITE "THIS IS A TEST OF MODULE HISTGEN"
WRITE "TEST ON BYTE IMAGE (RAMP) FIRST,"
WRITE "THEN TEST ON HALFWORD IMAGE (RAMP)"

  gen a 128 128
  histgen a his MAXD=100
  ibis-list his 'groups space=0
  label-list his 'property

  gen a 128 128 IVAL=-127 'HALF
  histgen a his MAXD=100
  ibis-list his 'groups space=0
  label-list his 'property

end-proc
$ Return
$!#############################################################################
