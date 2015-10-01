$!****************************************************************************
$!
$! Build proc for MIPL module printpix
$! VPACK Version 1.8, Thursday, April 06, 1995, 14:37:55
$!
$! Execute by entering:		$ @printpix
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
$ write sys$output "*** module printpix ***"
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
$ write sys$output "Invalid argument given to printpix.com file -- ", primary
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
$   if F$SEARCH("printpix.imake") .nes. ""
$   then
$      vimake printpix
$      purge printpix.bld
$   else
$      if F$SEARCH("printpix.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake printpix
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @printpix.bld "STD"
$   else
$      @printpix.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create printpix.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack printpix.com -
	-s printpix.f -
	-i printpix.imake -
	-p printpix.pdf -
	-t tstprintpix.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create printpix.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C      8 MAY 95 ...CRI...  MSTP S/W CONVERSION (VICAR PORTING)
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C     18 OCT 79   ...JJL...    INITIAL RELEASE
C     PROGRAM PRINTPIX                                       BILL BENTON
C
C     PRINTPIX WILL TAKE ANY SIZE INPUT PICTURE AND WILL PRINT A
C     PICTURE ON THE LINE PRINTER OF WIDTHS VARYING FROM ONE PAGE TO A
C     MAXIMUM NUMBER OF PAGES CORRESPONDING TO ONE CHARACTER PER PIXEL
C     AT 130 CHARACTERS PER PAGE. THE OUTPUT PICTURE IS CORRECTED FOR
C     ASPECT RATIO.
C     FORMAT= E,PRINTPIX,IN,*,,PARAMS
C     SYMBOLS=     IN      ...USER SPECIFIED INPUT DATA SET
C     PARAMS=      NP,N1   ...WIDTH OF OUTPUT IN PAGES. (DEFAULT=1)
C                  INC,N2...SETS THE SCANNING INCREMENT AND OVERRIDES
C                             THE PARAMETER NP.
C                  COMP    ...PRODUCES A COMPLIMENTARY PICTURE.
C                  STRETCH,N3,N4...PERFORMS A LINEAR STRETCH BETWEEN
C                             THE LOWER VALUE N3 AND THE UPPER VALUE N4.
C                  HEAD    ...PRODUCES SAMPLE HEADINGS AT TOP OF PAGE.
C                  SMALL   ...PAPER IS THE SMALL TYPE WITH AN ASPECT
C                             RATIO OF 4 TO 5.
C                  LARGE   ...PAPER IS THE LARGE TYPE WITH AN ASPECT
C                             RATIO OF 3 TO 5
C
      COMMON /C3/ DNW
      REAL SINC,R
      INTEGER SSO
      INTEGER SS,SL,NSO,NLO,NLI,NSI,NSW,NLW,NP,SMALL,INC,STAT
      INTEGER DW(13),KINC,INC2,JINC,NN,LINES,LSTR,HSTR
      INTEGER HEAD,PFLAG,COMP,IPARM(50),TDN(3500),DNW(130)
      INTEGER*2 DNBUF(3500),STRTAB(256)
      LOGICAL XVPTST
      CHARACTER*34 MSG1
      CHARACTER*74 MSG2
      CHARACTER*45 MSG3
      CHARACTER*130 MSG4,MSG6,UNCNFMT
      CHARACTER*65  MSG5
      CHARACTER*23  MSG7
      CHARACTER*6   FORMAT

C        INITIALIZE ALL DEFAULT VALUES.
      MSG1='STRETCH=     ,      (COMPLEMENTED)'
      MSG2(1:53)='WARNING---SAMPLE COUNT TOO LARGE---SAMPLE COUNT TRUNC'
      MSG2(54:74)='ATED TO 3500 SAMPLES.'
      MSG4(1:53)='I***-****I****-****I****-****I****-****I****-****I***'
      MSG4(54:104)='*-****I****-****I****-****I****-****I****-****I****'
      MSG4(105:130)='-****I****-****I****-****I'
      MSG6=' '
      MSG7='READ ERROR ON LINE'
      DSRN=2
      ZERO=0
      NP=1
      LSTR=0
      HSTR=255
      HEAD=0
      PFLAG=0
      SMALL=1
      COMP=0

      CALL IFMESSAGE('PRINTPIX version 8-MAY-95')
C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')

C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT(1:4).NE.'BYTE') THEN
         CALL MABEND('PRINTPIX ACCEPTS BYTE DATA ONLY')
      END IF

C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
         CALL MABEND('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL MABEND('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
      END IF

C                DETERMINE USER PARAMETERS
C        'COMP'
      IF(XVPTST('COMP')) COMP=255
C        'STRE'
      CALL XVPARM('STRETCH',IPARM,ICOUNT,IDEF,2)
      LSTR=IPARM(1)
      HSTR=IPARM(2)
C        'NP'
      CALL XVPARM('NP',NP,ICOUNT,IDEF,1)
C        'INC'
      CALL XVPARM('INC',INC,ICOUNT,IDEF,1)
      IF(INC.NE.1) PFLAG=1
C        'HEAD'
      IF(XVPTST('HEAD')) HEAD=1
C        'SMALL'
      IF(XVPTST('SMALL')) SMALL=1
C        'LARGE'
      IF(XVPTST('LARGE')) SMALL=0

      IF(COMP .EQ. 255) LSTR=255-LSTR
      IF(COMP .EQ. 255) HSTR=255-HSTR
      SL=SL-1
      SS=SS-1
      SSO=SS

C        CHECK FOR MAXIMUM PICTURE SIZE.  TRUNCATE IF TOO LARGE.
      IF(NSO .GT. 3500) THEN
         CALL XVMESSAGE(MSG2,' ')
         NSO=3500
      END IF

C        BUILD STRETCH TABLE FOR LINEAR STRETCH.
      R=255./(HSTR-LSTR)
      DO I=1,256
         STRTAB(I)=NINT(((I-1)-LSTR)*R) 
         IF(STRTAB(I) .LT. 0) STRTAB(I)=0
         IF(STRTAB(I) .GT. 255) STRTAB(I)=255
      END DO

C        BEGIN PROCESSING INPUT FILE AND CREATE NEW WORK FILE.
      IF(PFLAG .EQ. 1) GO TO 2
      SINC=NSO/130./NP
      INC=SINC
      IF(INC*1. .EQ. SINC) GO TO 2
      INC=INC+1

2     NSW=NSO/INC
      NLW=NLO/INC
      NP=NSW/130+(MOD(NSW,130)+129)/130
      LINES=(NLW/5)*(3+SMALL)+MOD(NLW,5)-MOD(NLW,5)/3
      J=SL+1
      I=SS+1
      K=1
      IF(MOD(NSW,130) .EQ. 0) K=0
      IF(PFLAG .EQ.0) NP=NSW/130+K
      WRITE (MSG3,9900) J,I,NLO,NSO
9900  FORMAT ('INPUT SL=  ',I4,' SS=  ',I4,' NL=  ',I4,' NS=  ',I4)
      CALL XVMESSAGE(MSG3,' ')
      WRITE (MSG5,9910) INC,LINES,NSW,NP
9910  FORMAT ('OUTPUT SCANNING INCREMENT=  ',I4,' NL=  ',I4,' NS=  ',I4,
     &        ' PAGES= ',I4)
      CALL XVMESSAGE(MSG5,' ')
      IF(SMALL .EQ. 0) CALL XVMESSAGE
     &   ('***THIS PRINTOUT IS FOR THE LARGE PAPER ASPECT RATIO***',' ')
      IF(SMALL .EQ. 1) CALL XVMESSAGE
     &   ('***THIS PRINTOUT IS FOR THE SMALL PAPER ASPECT RATIO***',' ')
      INC2=INC*INC
      KINC=0
7     CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      SS=SS+KINC
      KINC=130*INC
      IF(SS+KINC .GT. NSO+SSO) KINC=NSO+SSO-SS
      K=KINC/INC
      KINC=K*INC
      IF(HEAD) 14,14,10
10    M=K/10
      DO 9 N=1,M
         MM=4
         DW(N)=SS+(N-1)*10*INC
         IF(N.EQ.1) DW(N)=DW(N)+1
         NN=(N-1)*10+1
         IF(N.EQ.1) NN=NN+4
         IF(DW(N)-1) 99,17,99
17       MM=1
         NN=2
99       WRITE (UNCNFMT,'(''(I'',I4.4,'')'')') MM
         WRITE (MSG6(NN-(MM):NN-1),UNCNFMT) DW(N)
9        CONTINUE
      CALL XVMESSAGE(MSG6(1:K),' ')
      CALL XVMESSAGE(MSG4(1:K),' ')
14    CONTINUE
      DO 3 I=1,NLW
         IF(SMALL) 6,6,8
6        IF(MOD(I,5).EQ. 0) GO TO 3
8        IF(MOD(I,5).EQ. 3) GO TO 3
         JINC=(I-1)*INC+SL
         CALL ZIA(TDN,KINC)
         DO J=1,INC
            CALL XVREAD(IUNIT,DNBUF,STAT,'LINE',JINC+J,
     &                 'SAMP',SS+1,'NSAMPS',KINC,' ')
            CALL ADDV(6,KINC,DNBUF,TDN,1,1)
         END DO
         CALL ZIA(DNW,130)
         CALL ZZ(DNW,TDN,K,INC)
         CALL DIVV(4,K,INC2,DNW,0,1)
         DO J=1,K
            DNW(J)=STRTAB(DNW(J))
         END DO
         CALL DNCONV(K)
3     CONTINUE
      IF(HEAD) 12,12,13
13    CALL XVMESSAGE(MSG4(1:K),' ')
      CALL XVMESSAGE(MSG6(1:K),' ')
12    CONTINUE
      IF(SS+KINC+INC-NSO) 7,15,15
15    IF(LSTR.EQ.0.AND.HSTR.EQ.255.AND.COMP.EQ.0) GO TO 16
      WRITE (MSG1(10:13),'(I4)') LSTR
      WRITE (MSG1(16:19),'(I4)') HSTR
      N=20
      IF(COMP .EQ. 255) N=34
      CALL XVMESSAGE(MSG1(1:N),' ')
16    CONTINUE
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')

C        CLOSE DATA SET
      CALL XVCLOSE(IUNIT,STAT,' ')

      RETURN
      END

      SUBROUTINE DNCONV(NCHAR)

C     SUBROUTINE DNCONV CONVERTS A LINE OF 130 DN-LEVELS INTO A LINE
C     OF CHARACTERS WITH AN APPARENT DARKNESS CORRESPONDING TO EACH
C     DN-LEVEL.  DCONV THEN PRINTS AND OVERPRINTS THESE CHARACTERS.

      COMMON /C3/ DNW
      CHARACTER*16 PR
      CHARACTER*131 ALINE
      INTEGER DNW(130),TMP

      PR='#@$&WNV%*=?!-:. '
      ALINE=' '
      DO I=1,NCHAR
         TMP=DNW(I)/16+1
         ALINE(I:I)=PR(TMP:TMP)
      END DO
      CALL XVMESSAGE(ALINE(1:NCHAR),' ')

      RETURN
      END

	subroutine zz(out,in,ns,inc)
c	subroutine zz take an incremental box over an input buffer,
c	and forms a summation (with no overlap) which is output in out
c	thenumber of elements in out is ns and the number of
c	input elements is ns*inc
	integer*4 IN(3500),out(3500)

c	   put low pass filter code here
	j=0
	k=0
	NSIN=NS*INC
	do  i=1,NSIN,inc
	isum=in(i)
	       do  j=1,inc
	       l=i+j
	       if(l.LE.NSIN)isum=isum+IN(l)-IN(i)
	       enddo
	k=k+1
	out(k)=isum/inc
	enddo

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create printpix.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM printpix

   To Create the build file give the command:

		$ vimake printpix			(VMS)
   or
		% vimake printpix			(Unix)


************************************************************************/


#define PROGRAM	printpix
#define R2LIB

#define MODULE_LIST printpix.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create printpix.pdf
process help=*
parm INP type=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM NP TYPE=INTEGER DEFAULT=1
PARM INC TYPE=INTEGER DEFAULT=1
parm NC type=integer default=1
parm STRETCH type=integer count=2 default=(0,255)
parm COMP type=KEYWORD COUNT=0:1 VALID=COMP default=--
parm LABEL type=integer default=0
parm HEAD type=KEYWORD COUNT=0:1 VALID=HEAD default=--
parm ASPECT type=KEYWORD COUNT=0:1 VALID=(SMALL,LARGE) default=SMALL

!# annot function="Image Display"
!# annot keywords=(print,picture)

end-proc
.TITLE
  Prints a grey level display of an image
.HELP
PURPOSE:
  "printpix" IS A PROGRAM TO PRINT FORMATTED PICTURES
  ON THE LINE PRINTER.  "printpix" READS THE INPUT PICTURE
  AND BUILDS THE OUTPUT BY TAKING THE AVERAGE OF THE INPUT
  SAMPLES WITHIN THE INC X INC SIZE BOX AND ASSIGNING THAT
  VALUE TO THE OUTPUT PICTURE CHARACTER.  SIMULTANEOUSLY,
  A LINEAR STRETCH AND/OR THE COMPLEMENT OPERATION IS
  PERFORMED ON EACH CHARACTER.
  IN ORDER TO CORRECT FOR THE PRINTER ASPECT RATIO,
  "printpix" DELETES EVERY 3RD AND 5TH OR JUST THE 5TH LINE
  ( FOR 6 OR 8 LINES RESPECTIVELY ) AND PRINTS A DOUBLE
  PRINTED CHARACTER ON THE LINE PRINTER CORRESPONDING TO
  THE PROPER DN VALUE FOR EACH SAMPLE IN THE LINE.
EXECUTION:
  The following is the execution statement for "printpix"
         printpix  INP  PARAMS
where INP and PARAMS are parameters discussed in their
respective parameter section in TUTOR mode.
WRITTEN BY: W. D. Benton	September 10, 1974
COGNIZANT PROGRAMMER: Joel A. Mosher	March 14, 1983
REVISION: New
          May 8, 1995        A. Scop  (CRI) Made portable for UNIX 

.LEVEL1
.VARIABLE INP
 THE VICAR INPUT DATA SET
.VARIABLE SIZE
 THE STANDARD VICAR SIZE FIELD
.VARIABLE NP
 INTEGER, MAXIMUM WIDTH OF
 PICTURE IN PAGES
.VARIABLE NC
 INTEGER, INCREMENT FOR
 COMPRESSING PICTURE
.VARIABLE INC
 INTEGER, SETS THE SCANNING
 INCREMENT
.VARIABLE STRETCH
 TWO INTEGERS, LINEAR STRETCH
.VARIABLE COMP
 KEYWORD - INPUT PICTURE
 COMPLEMENT
 VALID: COMP
.VARIABLE LABEL
 INTEGER, NUMBER OF LABELS
.VARIABLE HEAD
 KEWYORD - SAMPLE NUMBERS ARE
 PRINTED
 VALID: HEAD
.VARIABLE ASPECT
 KEYWORD: PRINT ASPECT RATIO SIZE
 VALID: LARGE,SMALL
.LEVEL2
.VARI INP
 STANDARD VICAR INPUT DATASET PARAMETER
 (ONE DATASET)
.VARI SIZE
 STANDARD VICAR IMAGE SIZE PARAMETER.
.VARIABLE NP
 NP CAUSES THE INPUT PICTURE TO BE AUTOMATICALLY
 COMPRESSED BY AVERAGING OVER AN INCREMENT AND
 ASSIGNING THAT VALUE TO THE OUTPUT SAMPLE.
 DEFAULT IS NP = 1.
.VARIABLE NC
 INC OVERRIDES NP CAUSING THE INPUT TO BE AUTOMATICALLY
 COMPRESSED BY AVERAGING OVER INC AND ASSIGNING
 THAT VALUE TO THE OUTPUT SAMPLE.
 DEFAULT IS NP = 1.
.VARIABLE INC
 SETS THE SCANNING INCREMENT AND OVERRIDES THE PARAMETER NP.
.VARIABLE STRETCH
 THE OUTPUT PICTURE IS LINEARLY STRETCHED BETWEEN
 THE VALUES N AND N1.
 DEFAULT IS STRETCH = ( 0,255 )
.VARIABLE COMP
 OUTPUT IS THE COMPLEMENT OF THE INPUT PICTURE.
 DEFAULT IS THE POSITIVE PICTURE.
.VARIABLE LABEL
 THE FIRST N NUMBER OF LABELS ARE PRINTED AT THE
 BOTTOM OF THE FIRST PAGE WIDTH OF OUTPUT.  IF N IS
 GREATER THAN THE NUMBER OF LABELS, ALL LABELS ARE
 PRINTED.
 DEFAULT IS NO LABELS ARE PRINTED.
.VARIABLE HEAD
 ALL SAMPLE NUMBERS CORRESPONDING TO THE INPUT
 SAMPLES ARE PRINTED ABOVE AND BELOW THE PICTURE.
 DEFAULT IS NO SAMPLE HEADINGS ARE PRINTED.
.VARI ASPECT
 "LARGE" = LARGE FORMAT PRINTER PAPER, 6 LINES/INCH
 "SMALL" = SMALL FORMAT PRINTER PAPER, 8 LINES/INCH
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstprintpix.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULE printpix
!GENERATE TEST IMAGE 100X100
gen OUT=PRI.DAT NL=100 NS=100
!DEFAULT COMPRESSION
printpix INP=PRI.DAT
!NO COMPRESSION, LABELS
printpix INP=PRI.DAT SIZE=(1,1,10,10) NC=1 LABEL=3
! HEADINGS AND STRETCH WITH DEFAULT COMPRESSION
printpix INP=PRI.DAT 'HEAD STRETCH=(50,200)
!COMPLEMENT, COMPRESS TO ONE PAGE
printpix INP=PRI.DAT NP=1 'COMP
end-proc
$ Return
$!#############################################################################
