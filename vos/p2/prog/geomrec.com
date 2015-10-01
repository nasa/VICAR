$!****************************************************************************
$!
$! Build proc for MIPL module geomrec
$! VPACK Version 1.7, Monday, August 08, 1994, 10:34:32
$!
$! Execute by entering:		$ @geomrec
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
$ write sys$output "*** module geomrec ***"
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
$ write sys$output "Invalid argument given to geomrec.com file -- ", primary
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
$   if F$SEARCH("geomrec.imake") .nes. ""
$   then
$      vimake geomrec
$      purge geomrec.bld
$   else
$      if F$SEARCH("geomrec.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geomrec
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geomrec.bld "STD"
$   else
$      @geomrec.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geomrec.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geomrec.com -
	-s geomrec.f -
	-i geomrec.imake -
	-p geomrec.pdf -
	-t tstgeomrec.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geomrec.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
C
C----------------------------------------------------------------------
C
C        1984   CONVERT FROM IBM VICAR1 TO VICAR1*
C  4 FEB 1985   CONVERT TO VICAR2
C 31 OCT 1994   CONVERT FOR PORTING
C
C----------------------------------------------------------------------
C
C TRANSFORMS RANGE MEASUREMENT TO DISTANCE ON SURFACE
C (TRANSFORMS FROM SLANT RANGE TO GROUND RANGE)
C
C OUTPUT SCALE IS EQUAL TO AZIMUTH SCALE
C PARAMETER 1 - SENSOR HEIGHT (METERS) SPECIFIED AS AN INTEGER
C PARAMETER 2 - RANGE SAMPLE SPACING (MICRONS) SPECIFIED AS AN INTEGER
C PARAMETER 3 - AZIMUTH SAMP SPACING (MICRONS) SPECIFIED AS AN INTEGER
C PARAMETER 4 - RANGE SCALE (METERS/MICRON ON FILM) FLOATING POINT
C PARAMETER 5 - AZIMUTH SCALE (METERS/MICRON ON FILM) FLOATING POINT
C PARAMETER 6 - ANGLE FROM VERTICAL OF 1ST RETURN (DEGREES) FLOAT. PT.
C
      COMMON / C1 / INBUF, OBUF
      COMMON / C2 / INS1, INS2, F, G  
      INTEGER*2 INBUF(20000), OBUF(20000)
      INTEGER   INS1(20000), INS2(20000)
      INTEGER   INUNIT, OUTUNIT, STATUS, ICNT
      REAL*4    F(20000), G(20000)
      CHARACTER*61  MES02
      CHARACTER*50  MES03, MES13
C
C
C   PROCESS THE PARAMETERS
C
      CALL IFMESSAGE('GEOMREC version 31-OCT-94')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'I_FORMAT','BYTE','U_FORMAT','HALF',
     +   'IO_ACT','SA','OPEN_ACT','SA',' ')
C
      CALL XVSIZE(ISL,ISS,NL,NS,INLI,INSI)
      CALL XVP('HEIGHT',IHGT,ICNT)
      CALL XVP('RSSPACE',IRSS,ICNT)
      CALL XVP('ASSPACE',IASS,ICNT)
      CALL XVP('RSCALE',RSCAL,ICNT)
      CALL XVP('ASCALE',ASCAL,ICNT)
      CALL XVP('THETA',ANGLE,ICNT)
C
C      OPEN THE OUTPUT FILE
C
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','O_FORMAT','BYTE',
     +        'U_FORMAT','HALF','IO_ACT','SA','OPEN_ACT','SA',' ')
C
      MES02 = '   INPUT   HEIGHT(PIXEL AND FEET)  = '
      MES03 = '           RANGE SAMP SPACING(MIC) =  '
      MES13 = '  OUTPUT NUMBER OF SAMPLES         =  '
      THETA = ANGLE*(3.1415/180.)
      RSS = FLOAT(IRSS)
      ASS = FLOAT(IASS)
      CONE = 1./(RSS*RSCAL)
      CTWO = 1./(ASS*ASCAL)
      RRES = RSS*RSCAL
      ARES = ASS*ASCAL
      HGT = FLOAT(IHGT)
      HGTFT = HGT * 3.28084
      ONETWO = CONE/CTWO
      HGTM = FLOAT(IHGT)*CONE
      HMS = HGTM*HGTM
      OFFSET = HGT*TAN(THETA)
      SNI = FLOAT(NS-1)
      Z = RRES*SNI
      ZZ = (Z+HGT/COS(THETA))**2 - HGT**2
      SNO = CTWO*(SQRT(ZZ)-OFFSET) + 1.0
      NSO = IFIX(SNO)
C
      WRITE(MES02(42:48),'(F7.1)') HGTM
      WRITE(MES02(52:61),'(F10.1)') HGTFT
      WRITE(MES03(42:48),'(F7.1)') RSS
      WRITE(MES13(41:47),'(I7)') NSO
      CALL XVMESSAGE(MES02,' ')
      CALL XVMESSAGE(MES03,' ')
      CALL XVMESSAGE(MES13,' ')
C
C     INITIALIZE ALL ARRAYS TO ZERO
C
      CALL ZIA(INS1,NSO)
      CALL ZIA(INS2,NSO)
      CALL ZIA(F,NSO)
      CALL ZIA(G,NSO)
      DO 30  KOSAMP= 1, NSO
         ZZ = (ARES*(KOSAMP-1)+OFFSET)**2 + HGT**2
         XINS = CONE*(SQRT(ZZ)-HGT/COS(THETA))
         INS = IFIX(XINS)
         FINS = XINS - FLOAT(INS)
         INS = INS + 2
         IF(INS .GT. NS) GO TO 30
         INS1(KOSAMP) = INS - 1
         INS2(KOSAMP) = INS
         F(KOSAMP) = 1.0 - FINS
         G(KOSAMP) = FINS
   30 CONTINUE
C
C  AI LOOP
C
      DO 100  IOLINE = 1,NL
         CALL XVREAD(INUNIT,INBUF,STATUS,'NSAMPS',NS,'SAMP',ISS,
     +               'LINE',IOLINE,' ')
         DO  90  IOSAMP = 1,NSO
            IN1 = INS1(IOSAMP)
            IN2 = IN1 + 1
            FF = F(IOSAMP)
            GG = 1.0 - FF
            FFF = (INBUF(IN1))
            GGG = (INBUF(IN2))
            XOUT = (FF*FFF) + (GG*GGG)
            IOUT = IFIX( 0.5 + XOUT)
            OBUF(IOSAMP) = IOUT
   90    CONTINUE
C
C  WRITE OUTPUT LINE
C
         CALL XVWRIT(OUTUNIT,OBUF,STATUS,'NSAMPS',NS,' ')
  100 CONTINUE
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geomrec.imake
#define  PROGRAM   geomrec

#define MODULE_LIST geomrec.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create geomrec.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SS TYPE=INTEGER COUNT=1 DEFAULT=1
PARM NL TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NS TYPE=INTEGER COUNT=1 DEFAULT=0
PARM HEIGHT TYPE=INTEGER DEFAULT=0 
PARM RSSPACE TYPE=INTEGER DEFAULT=50 
PARM ASSPACE TYPE=INTEGER DEFAULT=50 
PARM RSCALE TYPE=REAL DEFAULT=.18 
PARM ASCALE TYPE=REAL DEFAULT=.25 
PARM THETA TYPE=REAL DEFAULT=0.
END-PROC
.TITLE
VICAR2 Program GEOMREC
.HELP
TRANSFORMS RANGE MEASUREMENT TO THE DISTANCE ON THE SURFACE.
(TRANSFORMS FROM SLANT RANGE TO GROUND RANGE)

COGNIZANT PROGRAMMER:  JAN HEYADA
.LEVEL1
.VARIABLE INP
input dataset
.VARIABLE OUT
output dataset
.VARIABLE SIZE
standard VICAR size field
.VARIABLE SL
starting line
.VARIABLE SS
starting sample
.VARIABLE NL
number of lines
.VARIABLE NS
number of samples 
.VARIABLE HEIGHT
sensor height (meters)
specified as an integer
.VARIABLE RSSPACE
range sample spacing 
(microns)
specified as an integer
.VARIABLE ASSPACE
azimuth sample spacing 
(microns) 
specified as an integer
.VARIABLE RSCALE
range scale 
(meters/micron on film) 
specified as a floating point
.VARIALBE ASCALE
azimuth scale 
(meters/micron on file) 
specified as a floating point
.VARIALBE THETA
angle from vertical of 
first return (degrees) 
specified as a floating point
.LEVEL2
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgeomrec.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "none"

!
!  THIS IS A TEST OF PROGRAM GEOMREC
!
gen a 10 10  linc=20
list a
!
!  default case
!
geomrec a d
list d
!
geomrec a b SIZE=(3,2,7,8)
list b
!
geomrec a b RSSPACE=30
list b
!
geomrec a b HEIGHT=40
list b
!
geomrec a b ASSPACE=40
list b
!
geomrec a b RSCALE=.2
list b
!
geomrec a b ASCALE=.25
! Should get 0 differences
difpic (b d)
!
geomrec a b ASCALE=.5
list b
!
geomrec a b THETA=20
list b
end-proc
$ Return
$!#############################################################################
