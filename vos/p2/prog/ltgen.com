$!****************************************************************************
$!
$! Build proc for MIPL module ltgen
$! VPACK Version 1.9, Friday, March 27, 2015, 12:11:50
$!
$! Execute by entering:		$ @ltgen
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
$ write sys$output "*** module ltgen ***"
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
$ write sys$output "Invalid argument given to ltgen.com file -- ", primary
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
$   if F$SEARCH("ltgen.imake") .nes. ""
$   then
$      vimake ltgen
$      purge ltgen.bld
$   else
$      if F$SEARCH("ltgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ltgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ltgen.bld "STD"
$   else
$      @ltgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ltgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ltgen.com -mixed -
	-s ltgen.f -
	-i ltgen.imake -
	-p ltgen.pdf -
	-t tstltgen.pdf tstltgen.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ltgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR PROGRAM LTGEN
C Radiometric calibration routine to initialize the Light Transfer File.
C The output Light Transfer File will contain a header record, followed
C by one record for each exposure level in the light transfer (or
C reciprocity) sequence.  The LTF file format is documented in the
C MOMGEN source.
C
C        LTGEN INP LTF.DAT NI=5 'GRID EXPO=(e0,e1,e2,e3...) LIST=
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         IMPLICIT NONE

         REAL*4 EXPO(100)

         INTEGER*4 AREA(4,400)
         INTEGER*4 SL, SS, NL, NS, NLI2, NSI2, SLI, SSI, NLI, NSI
         INTEGER*4 SLBUF(4), NAREA, NEXP, NX, PAR(6400) 
         INTEGER*4 ICNT, IDEFL, NI, IUNI, STAT, CNT, I, K, M, N
         INTEGER*4 NIX, NLA, NSA, IND, L, OUNI
         INTEGER*4 IGRES, NGRID, LOFF, SOFF, NLG, MAX0, NSG  

         LOGICAL*4 XVPTST,DBUG

         character*80 lfname
         CHARACTER*120 MSG

        ni=0
	nexp=0
	do i=1,400
	   expo=0.0
        enddo 
         CALL IFMESSAGE('LTGEN Version 23-Nov-2012  (64-bit) - rjb')
 
c-------If a LIST of filenames is input, get the NI, NEXP and EXPO
         call xvparm('LIST',lfname,icnt,idefl,80)
         if (icnt .eq. 1) call dolist(lfname,expo,nexp,ni)
 
c-------Open the INP file to get size field and label
c-------Label will automatically be transfered to ouput file
         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLI2,NSI2) !Get size field of input image
 
         DBUG = XVPTST('DBUG')
         NAREA = 0
         CALL ZIA(AREA,1600)
         IF (DBUG) CALL XVMESSAGE ('Area size fields...',' ')
 
         CALL XVP('AREA',PAR,CNT)          !Get specified areas

C        retrive AREA values from user input
         IF (CNT .NE. 0) THEN
            NAREA = CNT/4
            I = 1
 
            DO K=1,NAREA
               SL = PAR(I)
               SS = PAR(I+1)
               NL = PAR(I+2)
               NS = PAR(I+3)
               CALL MVE(4,4,PAR(I),AREA(1,K),1,1)
               WRITE(MSG,501)AREA(1,K),AREA(2,K),AREA(3,K),AREA(4,K)
  501          FORMAT('  ',I6,I6,I6,I6)
               IF (DBUG) CALL XVMESSAGE(MSG,' ')
               IF ((SL .LT. 1) .OR. (SS .LT. 1) .OR.
     &             (NL .LT. 1) .OR. (NS .LT. 1)) GOTO 990
               IF ((SL+NL-1 .GT. NLI) .OR. (SS+NS-1 .GT. NSI)) GOTO 990
               I = I + 4
            ENDDO
         ENDIF
 
         CALL XVPARM('GRES',PAR,CNT,IGRES,6400)  !grid resolution
         IF ((PAR(1) .LE. 0) .OR. (PAR(2) .LE. 0) .OR. 
     &       (PAR(3) .LE. 0)) GOTO 960 
         NGRID = PAR(1)
         NL = PAR(2)
         NS = PAR(3)
         SLBUF(3) = PAR(2)
         SLBUF(4) = PAR(3)
 
         NLG = NLI/NGRID
         NSG = NSI/NGRID
         LOFF = MAX0((NLG-NL)/2,1)   !Offsets
         SOFF = MAX0((NSG-NS)/2,1)
 
         IF (XVPTST('GRID') .OR. (IGRES .EQ. 0)) THEN    !Generate grid
            DO N=1,NGRID
               SLBUF(1) = (N-1)*NLG+LOFF
 
               DO M=1,NGRID
                  NAREA = NAREA + 1
                  SLBUF(2) = (M-1)*NSG+SOFF
                  CALL MVE(4,4,SLBUF(1),AREA(1,NAREA),1,1)
                  WRITE(MSG,502)AREA(1,NAREA),AREA(2,NAREA),
     &                            AREA(3,NAREA),AREA(4,NAREA)
 502              FORMAT('  ',I6,I6,I6,I6)
                  IF (DBUG) CALL XVMESSAGE (MSG,' ')
               ENDDO
            ENDDO
         ENDIF
 
         IF (NAREA .EQ. 0) GOTO 980
         IF (NAREA .GT. 400) GOTO 981
         WRITE (MSG,503)NAREA
 503     FORMAT ('NUMBER OF AREAS     = ',I6)
         CALL XVMESSAGE (MSG,' ')

c-----If EXP is entered, then it will override that derived from LIST
         CALL XVP('EXPO',EXPO,NX)          !Get exposure numbers
 
c-----If no LIST entered (or exps not found in labels)
c-----& no EXPO parameter, then abend
         IF ((NEXP .eq. 0) .and. (NX .EQ. 0)) GOTO 970

         if (NX .ne. 0) NEXP = NX           !EXP entered, override

         WRITE (MSG,504)NEXP
 504     FORMAT ('NUMBER OF EXPOSURES = ',I6)
         CALL XVMESSAGE (MSG,' ')
 
         CALL PRNT(7,NEXP,EXPO,'EXPOSURES = .')
 
c-----If NI is entered, then it will override that derived from LIST
         CALL XVP('NI',NIX,CNT)            !get max num of input/exp
         IF ((NI .eq. 0) .and. (CNT .EQ. 0)) GOTO 971    !no LIST & no NI
         if (CNT .ne. 0) NI = NIX          !NI entered, override
 
         CALL PRNT(4,1,NI,'MAX FRAMES/LEVEL = .')

         NLA = NEXP                    !one record for each exposure
         NSA = 3*NI*NAREA+1            !3 stats for each image*area
                                       !reserve 1 place holder for NI
         IF (NSA .GT. 32768) GOTO 982        !VMS record size restriction
 
         CALL XVMESSAGE 
     &      ('WRITING HALFWORD LIGHT TRANSFER FILE WITH',' ')
         CALL PRNT(4,1,NLA,' NL (NREC) = .')
         CALL PRNT(4,1,NSA,' NS        = .')

C        write to output file 
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         CALL XVOPEN(OUNI,STAT,'U_NL',NLA,'U_NS',NSA,'OPEN_ACT',
     &               'SA','IO_ACT','SA','OP','WRITE','U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')

C        write NUM_AREAS and AREAS onto VICAR label
         CALL XLADD(OUNI,'HISTORY','NUM_AREAS',NAREA,IND,
     &              'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','AREAS',AREA,IND,
     &              'NELEMENT',4*NAREA,'FORMAT','INT',' ')

C        write NUM_EXPOS and EXPOSURES onto VICAR label
         CALL XLADD(OUNI,'HISTORY','NUM_EXPOS',NEXP,IND,
     &              'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','EXPOSURES',EXPO(1),IND,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')

C        generate place holders for MOMGEN
         CALL ZIA(PAR,NSA)

C        write one record for each exposure...
         DO L=1,NEXP                                     !Each line is a place  
            CALL XVWRIT(OUNI,PAR,STAT,'NSAMPS',NSA,' ')  !holders for momgen
         ENDDO

         CALL XVMESSAGE('LTGEN task completed',' ')
         RETURN
 
C        error conditions 
  960    CALL XVMESSAGE ('??E - Grid resolution must be positive',' ')
         GOTO 999 
  970    CALL XVMESSAGE ('??E - No exposures from file list and ',' ')
         CALL XVMESSAGE ('   no exposures specified',' ')
         CALL XVMESSAGE ('***Use LIST or EXPO parameters',' ')
         GOTO 999
  971    CALL XVMESSAGE ('??E - No file list and no NI specified',' ')
         CALL XVMESSAGE ('***Use LIST or NI parameters',' ')
         GOTO 999
  980    CALL XVMESSAGE ('??E - No areas specified',' ')
         CALL XVMESSAGE ('***Use AREA, GRID, or GRES parameter',' ')
         GOTO 999
  981    CALL XVMESSAGE ('??E - Maximum of 400 areas exceeded',' ')
         GOTO 999
  982    CALL XVMESSAGE ('??E - Output file record size exceeds 32768',
     1  ' ')
         CALL XVMESSAGE 
     &      ('*** Reduce number of areas (see HELP LTGEN)',' ')
         GOTO 999
  990    CALL XVMESSAGE ('??E - Illegal area',' ')
  999    CALL XVMESSAGE ('***LTGEN task cancelled',' ')
         CALL ABEND
      END

C************************************************************************
      subroutine dolist(lfname,e,nexp,imax)
C     This subroutine only executes when the LIST parameter is given.  
C     It reads the SRCH-format file, which it is sorted by increasing
C     exposure time.  For each file in the list, it retrives the 
C     EXPOSRUE_DURATION label from the CASSINI-ISS property label.
C     The result of this subroutine is an array of exposures values. 
c      
c-------assumes only that the list of frames is in order of
c-------increasing exposure and in SRCH-format
c-------lfname is the file name of the list file
c-------exposure is the list of unique exposures (get rid of exp - f77 conflict)
c-------nexp is the number of them
c-------ni is the maximum number of frames found per any exposure
         implicit none
        integer*4 nexp,imax,i,ist,iu,j,ninstance,noexp,num
         real*4 exposure(500),e(100)
         character*80 fn
         character*9 tasks(30)
         character*(*) lfname
         integer*4 n(100),INSTANCE(30)
 
         open(unit=99,file=lfname,status='OLD',err=999)
 
         read(99,fmt=1) fn                      !skip first line
 
c-------initialize flag for errors finding exposures in labels
         noexp=0
	do i=1,100
	  n(i)=0		!fix for 64-bit
	enddo
c-------Open all input files and extract EXP from labels, if present
         do 10 i=1,501
            read(99,fmt=1,end=11) fn
            if (i .eq. 501) go to 992
            NUM = i
            call xvunit(iu,'NONE',1,ist,'u_name',fn,' ')
            call xvopen(iu,ist,'OPEN_ACT','SA','IO_ACT','SA',' ')
c-------try Cassini style
            call xlget(iu,'PROPERTY','EXPOSURE_DURATION',exposure(i),ist,
     &               'FORMAT','REAL','PROPERTY','CASSINI-ISS',' ')
            if (ist .eq. 1) go to 9
c-------try Galileo style
            NINSTANCE = 30
            CALL XLHINFO(iu,TASKS,INSTANCE,NINSTANCE,IST,' ')
            call xlget(iu,'HISTORY','EXP',exposure(i),ist,
     &                 'FORMAT','REAL','HIST',TASKS(1),' ')
            if (ist .eq. 1) goto 9
            noexp=1                        !Exposure not found in label
    9       continue
 
            call xvclose(iu,ist,' ')
   10    continue
 
   11    call prnt(4,1,num,'NUMBER OF FILES  =.')
 
c-------find number of frames at each exposure time and store the
c-------unique exposure times for return to main program
         i=1
         j=1
         e(j)=exposure(i)
         n(j)=1
 
         do 15 i=2,num
            if (exposure(i) .eq. e(j)) then
               n(j) = n(j) + 1     ! increment number of identical exposures
               goto 15
            else
               j=j+1
               if (j .gt. 100) go to 993
               e(j)=exposure(i)         ! record unique exposure times
               n(j)=1
            end if
   15    continue
 
         nexp=j      ! store size of exposure array to return to main
 
c-------If error finding EXP in labels, set NEXP to zero as a flag
c-------for the main program
         if (noexp .eq. 1) NEXP=0
 
         call prnt(4,1,nexp,'NUMBER OF LEVELS =.')
 
         imax=0
         do i=1,num
            if (n(i) .gt. imax) imax=n(i)
         enddo
 
         call prnt(4,1,imax,'MAX FRAMES/LEVEL =.')
 
         return
 
    1    format(a)

  992    call xvmessage('??E - more than 500 filenames in list',' ')
         call abend
  993    call xvmessage ('??E - more than 100 exposure levels',' ')
         call abend
  999    call xvmessage ('??E - Error openning input list file',' ')
         call xvmessage (lfname,' ')
         call abend
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ltgen.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ltgen

   To Create the build file give the command:

		$ vimake ltgen			(VMS)
   or
		% vimake ltgen			(Unix)


************************************************************************/


#define PROGRAM	ltgen
#define R2LIB

#define MODULE_LIST ltgen.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ltgen.pdf
PROCESS HELP = *
PARM INP  TYPE=STRING
PARM OUT  TYPE=STRING
PARM DBUG TYPE=KEYWORD  COUNT=(0:1)   VALID=DBUG        DEFAULT=--
PARM AREA TYPE=INTEGER  COUNT=(0:600) VALID=(1:1024)    DEFAULT=--
PARM GRES TYPE=INTEGER  COUNT=(0:3)                     DEFAULT=(10,20,20)
PARM GRID TYPE=KEYWORD  COUNT=(0:1)   VALID=GRID        DEFAULT=--
PARM EXPO TYPE=REAL     COUNT=(0:100)                   DEFAULT=--
PARM NI   TYPE=INTEGER  COUNT=(0:1)   VALID=(1:80)      DEFAULT=--
PARM LIST TYPE=STRING   COUNT=(0:1)                     DEFAULT=--
END-PROC
.TITLE
VICAR Application Program LTGEN
.HELP
PURPOSE:
 
LTGEN is the first of a sequence of VICAR applications programs which
measure the radiometric properties of a camera system.  The programs
are designed to support radiometric calibration of flight camera systems.
 
 
LTGEN will initialize the Light Transfer File (LTF) by specifying areas
within the image where measurements are to be made, and initializing one
record for each exposure level of the light transfer or reciprocity
sequence.  The next program in the sequence, MOMGEN, then extracts the
moments for each area (sum of DNs, sum of squares of DNs, and cross terms)
and stores these moments in each exposure record.  Finally, data analysis
routines access the LTF and extract camera properties such as:
 
        1) System gain constant and read noise floor (see CCDNOISE).
        2) Light transfer curve slope and offset (see CCDSLOPE).
        3) Shutter offset and camera sensitivity (see CCDRECIP).
 
.PAGE
EXECUTION:
 
                  LTGEN INP=PIC.DAT OUT=LTF.DAT PARAMS
 
.PAGE
OPERATION:
 
The input image (INP) may be any image from the light transfer or
reciprocity test sequence.  The VICAR label of this image is copied
to LTF to help identify the file.
 
The output Light Transfer File will contain a header record, followed
by one record for each exposure level in the light transfer (or
reciprocity) sequence.
 
The AREA parameter may be used to specify the size fields of areas in
the image from which data is to be extracted.
 
The GRID keyword may be specified if a uniform grid of areas is desired.
The default grid consists of 100 areas spaced in a 10x10 grid pattern,
with each area 20x20 pixels in size.   The size of the input image
(800x800 for Galileo) is used to space these 100 areas uniformly
throughout the field of view.
 
The grid spacing and area size may be modified by using the GRES
parameter:
 
        Example:  GRES=(16,10,10) will create 256 areas in a 16x16 grid
                pattern, with each area 10x10 pixels in size.
 
At least one of the parameters AREA, GRID, or GRES must be used to
specify the areas.  These parameters may be used in combination,
so long as the total number of areas does not exceed 400.  However,
see note below.  Also, note that the AREA parameter should not be
used for reciprocity sequences since CCDRECIP requires a rectangular grid
pattern to determine line dependency for shutter offsets.
 
The EXPO parameter is used to specify the exposure levels of the light
transfer (or reciprocity) sequence.  The exposures should be specified
in ascending order, beginning with 0.0 for the dark current frame and ending
with the highest exposure in the sequence.  If, for Galileo, extended
exposure dark-current frames are also included in the sequence, then
they are entered first (as -1.0) followed by the normal dark-current
and the remaining exposures.
 
        Example:  EXPO=(-1.0,0.0,4.167,6.25,8.333,12.5)
 
The NI parameter specifies the maximum number of frames at any one
exposure level in the sequence.  This is necessary in order to ensure
that the exposure record is made large enough to hold the moments
for each input frame.
 
The LIST parameter contains the filename of the list of files for
which MOMGEN will be storing statistics.  The file must be a SRCH-format
file (see program SRCH) in which the files are sorted by increasing
exposure time.  LTGEN will read the labels of the files in
the list, extract the exposure times and derive NI automatically.
Therefore, if LIST is specified, EXPO and NI need not be used.

LTGEN will store the area size fields and exposures in the LTF file 
label.  The LTF will generate one place holder record for each exposure, 
which will be filled with moment information entered by MOMGEN.
 
Note that there is a VAX/VMS restriction on the maximum record size
of the LTF file (32,768 bytes).  This places a limit on the number of
specifiable areas which may be computed as follows:
 
   Let:  NAREA = the number of areas specified by the AREA and/or
                 GRID or GRES keywords.
         NI    = the maximum number of frames at a fixed exposure in the
                 light transfer sequence.
 
   Then: NS = 3*NAREA*NI+1 must not exceed 32768.
 
         NEXPO = the number of exposures (including dark current).
         NREC  = NEXPO
 
The following table gives the maximum number of areas specifiable for
each NI:
 
                     NI   GRES  NAREA  RECLEN(bytes)
 
                     1     20   400     4808
                     2     20   400     9608
                     3     20   400    14408
                     4     20   400    19208
                     5     20   400    24008
                     6     20   400    28808
                     7     19   361    30332
                     8     18   324    31112
                     9     17   289    31220
                    10     16   256    30728
 
.PAGE
RESTRICTIONS:
 
   1. Maximum of 100 destinct exposure levels.
   2. Maximum of 500 frames in the LIST file.
 
ORIGINAL PROGRAMMER: Gary Yagi, circa 1983
CURRENT COGNIZANT PROGRAMMER: Gary Yagi
REVISION HISTORY:

   12 Nov 2012...R. Bambery..... Fixes for 64-bit unix, gcc 4.6.2
                                changed conflicting exp to exposure, zero arrays
   13 MAR 1997...T.Huang........Ported from VAX to UNIX to support both 
                              CASSINI and Galileo. 
   22 NOV 1993...C.C.Avis.......Added LIST parameter, extraction of EXPs
                              from labels, and error checking
   26 APR 1988...G.M.Yagi.......Added more documentation.
    4 JUL 1986...G.M.Yagi.......Code and documentation clean-up
   19 FEB 1985...M.E.MORRILL....INCLUDED EXTENDED EXPOSURE MODE
   14 JAN 1985...M.E.MORRILL....EXPANSION TO 20x20 GRID WITH VARIABLE
                                RESOLUTION AND SAMPLE AREA
   27 SEP 1984...M.E.MORRILL....CONVERSION TO VAX-VICAR*2
   12 MAR 1984...M.E.MORRILL....CONVERSION TO VAX-VICAR 1*
   15 OCT 1982...G.M.YAGI.......INITIAL RELEASE
 
.LEVEL1
.VARIABLE INP
 An image from the light
 transfer sequence.
.VARIABLE OUT
 The LTF output file.
.VARIABLE AREA
 INTEGER
 Specifies areas by
 SL1,SS1,NL1,NS1
 SL2,SS2,NL2,NS2
  "   "   "   "
 SLx,SSx,NLx,NSx.
.VARIABLE GRID
 KEYWORD
 Invokes a 10x10 grid of
 20x20 areas uniformly
 distributed over the image.
.VARIABLE GRES
 INTEGER
 GRES=(N,NL,NS).
 Generates NxN grid
 of NLxNS pixel areas
 spaced uniformly over
 the image.
.VARIABLE EXPO
 REAL
 Exposure times (msec) for the
 light transfer sequence.
.VARIABLE NI
 INTEGER
 Specifies the max number of
 frames at any one exposure of
 the light transfer sequence.
.VARIABLE LIST
 STRING
 Specifies the name of a file
 containing the names of all
 files to be processed by
 MOMGEN.
.VARIABLE DBUG
 KEYWORD
 Invokes special debug print.
.LEVEL2
.VARIABLE INP
 An image from the light transfer sequence. The image label
 is used to identify the output Light Transfer File.
.VARIABLE OUT
 The output Light Transfer File (LTF).  For details use HELP
 MOMGEN and TUTOR MOMGEN.
.VARAIABLE AREA
 INTEGER-OPTIONAL
 AREA=(SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...,SLn,SSn,NLn,NSn)
 specifying the size fields of up to 125 areas in the image.
.VARIABLE GRID
 KEYWORD-OPTIONAL
 Generates a 10x10 grid of areas uniformly distributed over
 the image to be used to produce the statistics in MOMGEN.
 Each area will be 20x20 in size. The resolution of this grid
 can be increased by using the GRES keyword.
.VARIABLE GRES
 INTEGER-OPTIONAL
 GRES=(N,NL,NS)
 Generates an NxN grid of areas uniformly distributed over the
 input image.  The areas are NLxNS pixels in size.  Data is extracted
 from these areas by MOMGEN and stored in the LTF.  Maximum N is 20.
 Default GRES=(10,20,20).
.VARIABLE EXPO
 REAL - (Required if LIST is not used)
 A string of floating point numbers specifying the commanded exposure times
 (in msec) of the light transfer sequence in ascending order.
 The first exposure is usually the dark current (0.0 msec):
        EXPO=(0.0,4.167,6.25,8.33,12.5,16.67)
 Galileo specific:
 If extended exposure frames are present in the sequence, then
 the sequence should begin with the extended-exposure dark-current
 (specified as -1.0 msec), followed by the normal DC, followed by the
 rest of the exposure levels:
        EXPO=(-1.0,0.0,4.167,6.25,8.33,12.5,16.67)
 However, sequences which consist entirely of extended-exposure mode
 frames should be treated as though they were normal exposure frames
 I.e., the extended-exposure dark-current should be specified as
 0.0 exposure, and later, when running CCDNOISE or CCDSLOPE, the EXTEXPO
 parameter should not be used.
.VARIABLE NI
 INTEGER - (Required if LIST is not used)
 Specifies the maximum number of frames at any one exposure level of
 the light transfer sequence.
.VARIABLE LIST
 STRING - Optional
 Specifies the name of a SRCH-format ASCII file containing a list of
 the filenames of all frames to be processed by MOMGEN.  The list must
 be in order of increasing exposure time.  LTGEN will automatically
 extract the exposure times from the VICAR labels and determine the
 proper value of NI.  This probably should not be used for the special
 case of extended-exposures for Galileo.
.VARIABLE DBUG
 KEYWORD
 Invokes special debug print.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstltgen.pdf
procedure
local   afidsroot   type=string count=1
local   aftestdata  type=string count=1

local testmin1 real count=1 init=2.0 
local testmax1 real count=1 init=4.144700E+04

local testmin2 real count=1 init=2.0
local testmax2 real count=1 init=4.027336E+06

local testmin3 real count=1 init=2.0
local testmax3 real count=1 init=4.014217280E+08

local casmin1 real count=1 init=3.000000000000e+00
local casmax1 real count=1 init=4.204830000000e+05

local casmin2 real count=1 init=3.000000000000e+00
local casmax2 real count=1 init=7.009883000000e+06

local casmin3 real count=1 init=3.000000000000e+00
local casmax3 real count=1 init=2.159928400000e+07

local casmin4 real count=1 init=3.000000000000e+00
local casmax4 real count=1 init=7.477808000000e+07

local minval  real count=1 
local maxval  real count=1 

! Aug 22, 2013 - RJB
! TEST SCRIPT FOR LTGEN   
! tests BYTE and HALF images
!
! Vicar Programs:
!   gen label-list f2 label-replace copy createfile
!   maxmin addtofile reset list hist flot momgen typetext  
!
! External programs
!       <none>
!
! Parameters:
!       <none>           
!
! Requires external test data: 
!   cartlab or mipl dependent pointers
!            
!   Cartlab defines env var $AFIDS_ROOT, mipl doesn't
!   The test data in cartlab is on /raid1/test_data 
!   but in other facilities it might be somewhere else. 
!   
!   To facilitate this test you can define an
!   environment variable $AFIDS_TESTDATA to point to
!   that data. The cartlab system does not. In the git archive
!   on pistol there is softlink to the test data in vdev that
!   allows this test to pass 
!
!!
! Note: Previous tests of Cassini data only tested
! the 0-filled initial generation of the ltgen file
! This new test fills it in with real data             
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
refgbl $echo
refgbl $autousage
refgbl $syschar

body
!let _onfail="stop"
let _onfail="stop"
let $echo="no"

!check to see if mipl or cartlab for certain programs
!cartlab defines env var $AFIDS_ROOT, mipl doesm't
translog INP=AFIDS_ROOT TRANS=afidsroot
translog INP=AFIDS_TESTDATA TRANS=aftestdata
if (afidsroot = "")
!MIPL        
    ush ln -s /project/test_work/testdata/cassini/iss cas 
else         
!CARTLAB
    if (aftestdata = "")    
       ush ln -s ../test_data/vicar_test_images/testdata/cassini/iss cas
    else
       ush ln -s $AFIDS_TESTDATA/vicar_test_images/testdata/cassini/iss cas
    end-if
end-if       
           
let _onfail="goto rm"
 
write "*******************"
write "   TEST 1         *"
write "*******************"
let $echo="yes"
 
! TEST 1 - GENERAL TEST:  BYTE 0-dn Image
gen a.img 800 800
ltgen a.img ltf.out 'DBUG EXPO=(0.,50.,66.67,100.,133.33,200.,266.67) +
            AREA=(15,15,10,10) GRES=(3,12,13) NI=3

let $echo="no" 
WRITE "Verify from file: zeroed place holders"
let $echo="yes"
list ltf.out     

maxmin ltf.out maxival=maxval minival=minval
let $echo="no"
write "Data set  max = &maxval should be 0.000"
write "Data set  min = &minval should be 0.000"
write "  "
WRITE "Verify the NUM_AREA, AREAS, NUM_EXP, and EXPOSURES labels"
let $echo="yes"
label-list ltf.out

! TEST 2 - noisy image HALF image
!
let $echo="no"
write "*******************"
write "   TEST 2         *"
write "*******************"
let $echo="yes"
gausnois d1.img mean=10.5 sigma=1.023  nl=500 ns=500 form=half seed=82651
hist d1.img 'nohist
flot d1.img d2.img 'coun


gausnois a1.img mean=100.5 sigma=3.1623 nl=500 ns=500 form=half seed=987654
hist a1.img 'nohist
flot a1.img a2.img 'coun
gausnois b1.img mean=1000.5 sigma=10 nl=500 ns=500 form=half seed=876543
hist b1.img 'nohist
flot b1.img b2.img 'coun
ltgen a1.img ltf1.out 'grid ni=2 exp=(0,10,20)  !CREATE LTF
momgen (d1.img,d2.img) ltf1.out  exp=0                    !LOAD LTF
momgen (a1.img,a2.img) ltf1.out  exp=10                   !LOAD LTF
momgen (b1.img,b2.img) ltf1.out  exp=20                   !LOAD LTF

! get min and max for exp=0
maxmin ltf1.out sl=1 nl=1 maxival=maxval minival=minval 
let $echo="no"
write "Data set line 1 max = &maxval should be &testmax1"
write "Data set line 1 min = &minval should be &testmin1"
    if (&maxval = testmax1  and &minval = testmin1) write "          ******* Line 1 passes *******"
let $echo="yes"
maxmin ltf1.out sl=2 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 2 max = &maxval should be &testmax2"
write "Data set line 2 min = &minval should be &testmin2"
    if (&maxval = testmax2  and &minval = testmin2) write "          ******* Line 2 passes *******"
let $echo="yes"

maxmin ltf1.out sl=3 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 3 max = &maxval should be &testmax3"
write "Data set line 3 min = &minval should be &testmin3"
    if (&maxval = testmax3  and &minval = testmin3) write "          ******* Line 3 passes *******"

write " "
write " "
write "**********************"
write "   TEST 3  - Cassini *"
write "**********************"
let $echo="yes"

! TEST 3 - Cassini summation mode image
!---------------------------
! Make a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! Each level will have 3 frames associated with it.
 
!Using a frame with Cassini labels, set dns and exposures
!Set dns to 10 and replicate -  set exposure to 0
f2 cas/sum2.1 l1.a func="in1+10"
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 110 and replicate - set exposure to 10
f2 cas/sum2.1 l2.a func="in1+110"
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 210 and replicate - set exposure to 20
f2 cas/sum2.1 l3.a func="in1+210"
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 410 and replicate - set exposure to 40
f2 cas/sum2.1 l4.a func="in1+410"
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created (in SRCH-format)
createfile l.list
addtofile l.list "NEXT FILE=0001"
addtofile l.list "l1.a"
addtofile l.list "l1.b"
addtofile l.list "l1.c"
addtofile l.list "l2.a"
addtofile l.list "l2.b"
addtofile l.list "l2.c"
addtofile l.list "l3.a"
addtofile l.list "l3.b"
addtofile l.list "l3.c"
addtofile l.list "l4.a"
addtofile l.list "l4.b"
addtofile l.list "l4.c"
reset l.list
typetext l.list
 
!Initialize Light Transfer File
ltgen l1.a out=casltf.out list=l.list 'GRID
momgen (l1.a,l1.b,l1.c) casltf.out  exp=0                    !LOAD LTF
momgen (l2.a,l2.b,l2.c) casltf.out  exp=10                   !LOAD LTF
momgen (l3.a,l3.b,l3.c) casltf.out  exp=20                   !LOAD LTF
momgen (l4.a,l4.b,l4.c) casltf.out  exp=40                   !LOAD LTF
let $echo="no" 
WRITE "Verify from file:  zeroed place holders"
let $echo="yes"

! get min and max for exp=0
maxmin casltf.out sl=1 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 1 max = &maxval should be &casmax1"
write "Data set line 1 min = &minval should be &casmin1"
    if (&maxval = casmax1  and &minval = casmin1) write "          ******* Line 1 passes *******"
let $echo="yes"
maxmin casltf.out sl=2 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 2 max = &maxval should be &casmax2"
write "Data set line 2 min = &minval should be &casmin2"
    if (&maxval = casmax2  and &minval = casmin2) write "          ******* Line 2 passes *******"
let $echo="yes"
maxmin casltf.out sl=3 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 3 max = &maxval should be &casmax3"
write "Data set line 3 min = &minval should be &casmin3"
    if (&maxval = casmax3  and &minval = casmin3) write "          ******* Line 3 passes *******"
let $echo="yes"
maxmin casltf.out sl=4 nl=1 maxival=maxval minival=minval
let $echo="no"
write "Data set line 4 max = &maxval should be &casmax4"
write "Data set line 4 min = &minval should be &casmin4"
    if (&maxval = casmax4  and &minval = casmin4) write "          ******* Line 4 passes *******"
let $echo="yes"

list casltf.out sl=1 ss=1 nl=4 ns=10 

let $echo="no"
WRITE "Verify NUM_AREA, AREAS, NUM_EXP, and EXPOSURES labels." 
let $echo="yes"

label-list casltf.out


rm>
let $echo="no"
   ush rm cas
end-proc

$!-----------------------------------------------------------------------------
$ create tstltgen.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

*******************
   TEST 1         *
*******************
gen a.img 800 800
Beginning VICAR task gen
GEN Version 6
GEN task completed
ltgen a.img ltf.out 'DBUG EXPO=(0.,50.,66.67,100.,133.33,200.,266.67)  +
            AREA=(15,15,10,10) GRES=(3,12,13) NI=3
Beginning VICAR task ltgen
LTGEN Version 23-Nov-2012  (64-bit) - rjb
Area size fields...
      15    15    10    10
     127   126    12    13
     127   392    12    13
     127   658    12    13
     393   126    12    13
     393   392    12    13
     393   658    12    13
     659   126    12    13
     659   392    12    13
     659   658    12    13
NUMBER OF AREAS     =     10
NUMBER OF EXPOSURES =      7
EXPOSURES = 
            0.000E+00  5.000E+01  6.667E+01  1.000E+02  1.333E+02  2.000E+02  2.667E+02
MAX FRAMES/LEVEL =           3
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           7
 NS        =          91
LTGEN task completed
let $echo="no"
Verify from file: zeroed place holders
list ltf.out
Beginning VICAR task list
 ** The specified window is all zero.
maxmin ltf.out maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 0.000000E+00   at  (     1,     1)
Max. value: 0.000000E+00   at  (     1,     1)

let $echo="no"
Data set  max = 0.000000000000e+00 should be 0.000
Data set  min = 0.000000000000e+00 should be 0.000
  
Verify the NUM_AREA, AREAS, NUM_EXP, and EXPOSURES labels
label-list ltf.out
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File ltf.out ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                7 lines per band
                91 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: wlb -- Fri Mar 27 12:01:51 2015 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LTGEN -- User: wlb -- Fri Mar 27 12:01:51 2015 ----
NUM_AREAS=10
AREAS=(15, 15, 10, 10, 127, 126, 12, 13, 127, 392, 12, 13, 127, 658, 12, 13, 
393, 126, 12, 13, 393, 392, 12, 13, 393, 658, 12, 13, 659, 126, 12, 13, 659, 
392, 12, 13, 659, 658, 12, 13)
NUM_EXPOS=7
EXPOSURES=(0.0, 50.0, 66.67, 100.0, 133.33, 200.0, 266.67)
 
************************************************************
let $echo="no"
*******************
   TEST 2         *
*******************
gausnois d1.img mean=10.5 sigma=1.023  nl=500 ns=500 form=half seed=82651
Beginning VICAR task gausnois
hist d1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=9.999316       STANDARD DEVIATION=1.063144       NUMBER ELEMENTS=    250000
MIN. DN=         6
MAX. DN=        14

flot d1.img d2.img 'coun
Beginning VICAR task flot
gausnois a1.img mean=100.5 sigma=3.1623 nl=500 ns=500 form=half seed=987654
Beginning VICAR task gausnois
hist a1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=99.99788       STANDARD DEVIATION=3.176533       NUMBER ELEMENTS=    250000
MIN. DN=        87
MAX. DN=       112

flot a1.img a2.img 'coun
Beginning VICAR task flot
gausnois b1.img mean=1000.5 sigma=10 nl=500 ns=500 form=half seed=876543
Beginning VICAR task gausnois
hist b1.img 'nohist
Beginning VICAR task hist
*** HIST version 17 Dec 2012 ***


AVERAGE GRAY LEVEL=1000.000       STANDARD DEVIATION=10.00253       NUMBER ELEMENTS=    250000
MIN. DN=       959
MAX. DN=      1039

flot b1.img b2.img 'coun
Beginning VICAR task flot
ltgen a1.img ltf1.out 'grid ni=2 exp=(0,10,20)
Beginning VICAR task ltgen
LTGEN Version 23-Nov-2012  (64-bit) - rjb
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      3
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01
MAX FRAMES/LEVEL =           2
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           3
 NS        =         601
LTGEN task completed
momgen (d1.img,d2.img) ltf1.out  exp=0
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             2
NUMBER OF AREAS=        100
momgen (a1.img,a2.img) ltf1.out  exp=10
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             2
NUMBER OF AREAS=        100
momgen (b1.img,b2.img) ltf1.out  exp=20
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             2
NUMBER OF AREAS=        100
maxmin ltf1.out sl=1 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 2.000000E+00   at  (     1,     1)
Max. value: 4.144700E+04   at  (     1,   125)

let $echo="no"
Data set line 1 max = 4.144700000000e+04 should be 4.144700000000e+04
Data set line 1 min = 2.000000000000e+00 should be 2.000000000000e+00
          ******* Line 1 passes *******
maxmin ltf1.out sl=2 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 2.000000E+00   at  (     2,     1)
Max. value: 4.027336E+06   at  (     2,   359)

let $echo="no"
Data set line 2 max = 4.027336000000e+06 should be 4.027336000000e+06
Data set line 2 min = 2.000000000000e+00 should be 2.000000000000e+00
          ******* Line 2 passes *******
maxmin ltf1.out sl=3 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 2.000000E+00   at  (     3,     1)
Max. value: 4.014217E+08   at  (     3,   527)

let $echo="no"
Data set line 3 max = 4.014217280000e+08 should be 4.014217280000e+08
Data set line 3 min = 2.000000000000e+00 should be 2.000000000000e+00
          ******* Line 3 passes *******
 
 
**********************
   TEST 3  - Cassini *
**********************
f2 cas/sum2.1 l1.a func="in1+10"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l1.a l1.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l1.a l1.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l2.a func="in1+110"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l2.a l2.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l2.a l2.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l3.a func="in1+210"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l3.a l3.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l3.a l3.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
f2 cas/sum2.1 l4.a func="in1+410"
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 26 TIMES
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
Beginning VICAR task label
LABEL version 15-Nov-2010
Keyword EXPOSURE_DURATION replaced
copy l4.a l4.b
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
copy l4.a l4.c
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
createfile l.list
refgbl  $syschar
write "CREATEFILE version 1-3-97"
CREATEFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  USH cp /dev/null  l.list
end-if
END-PROC
addtofile l.list "NEXT FILE=0001"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="NEXT FILE=0001"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l1.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l1.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l2.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l2.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l3.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l3.c"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.a"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.a"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.b"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.b"
Beginning VICAR task addtofil
end-if
END-PROC
addtofile l.list "l4.c"
refgbl  $syschar
write "ADDTOFILE version 1-3-97"
ADDTOFILE version 1-3-97
if ($syschar(1) = "VAX_VMS")
else
  addtofil INPUT=@INPUT STRING1="l4.c"
Beginning VICAR task addtofil
end-if
END-PROC
reset l.list
Beginning VICAR task reset
typetext l.list
Beginning VICAR task typetext
NEXT FILE =     1
l1.a
l1.b
l1.c
l2.a
l2.b
l2.c
l3.a
l3.b
l3.c
l4.a
l4.b
l4.c
ltgen l1.a out=casltf.out list=l.list 'GRID
Beginning VICAR task ltgen
LTGEN Version 23-Nov-2012  (64-bit) - rjb
NUMBER OF FILES  =         12
NUMBER OF LEVELS =          4
MAX FRAMES/LEVEL =          3
NUMBER OF AREAS     =    100
NUMBER OF EXPOSURES =      4
EXPOSURES = 
            0.000E+00  1.000E+01  2.000E+01  4.000E+01
MAX FRAMES/LEVEL =           3
WRITING HALFWORD LIGHT TRANSFER FILE WITH
 NL (NREC) =           4
 NS        =         901
LTGEN task completed
momgen (l1.a,l1.b,l1.c) casltf.out  exp=0
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    0.000E+00
INPUT FRAMES=             3
NUMBER OF AREAS=        100
momgen (l2.a,l2.b,l2.c) casltf.out  exp=10
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    10.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
momgen (l3.a,l3.b,l3.c) casltf.out  exp=20
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    20.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
momgen (l4.a,l4.b,l4.c) casltf.out  exp=40
Beginning VICAR task momgen
MOMGEN Version 19-MAR-1997

EXPOSURE TIME=    40.000000
INPUT FRAMES=             3
NUMBER OF AREAS=        100
let $echo="no"
Verify from file:  zeroed place holders
maxmin casltf.out sl=1 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 3.000000E+00   at  (     1,     1)
Max. value: 4.204830E+05   at  (     1,   545)

let $echo="no"
Data set line 1 max = 4.204830000000e+05 should be 4.204830000000e+05
Data set line 1 min = 3.000000000000e+00 should be 3.000000000000e+00
          ******* Line 1 passes *******
maxmin casltf.out sl=2 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 3.000000E+00   at  (     2,     1)
Max. value: 7.009883E+06   at  (     2,   545)

let $echo="no"
Data set line 2 max = 7.009883000000e+06 should be 7.009883000000e+06
Data set line 2 min = 3.000000000000e+00 should be 3.000000000000e+00
          ******* Line 2 passes *******
maxmin casltf.out sl=3 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 3.000000E+00   at  (     3,     1)
Max. value: 2.159928E+07   at  (     3,   545)

let $echo="no"
Data set line 3 max = 2.159928400000e+07 should be 2.159928400000e+07
Data set line 3 min = 3.000000000000e+00 should be 3.000000000000e+00
          ******* Line 3 passes *******
maxmin casltf.out sl=4 nl=1 maxival=maxval minival=minval
Beginning VICAR task maxmin
*** maxmin - 06-Jul-2012

Min. value: 3.000000E+00   at  (     4,     1)
Max. value: 7.477808E+07   at  (     4,   545)

let $echo="no"
Data set line 4 max = 7.477808000000e+07 should be 7.477808000000e+07
Data set line 4 min = 3.000000000000e+00 should be 3.000000000000e+00
          ******* Line 4 passes *******
list casltf.out sl=1 ss=1 nl=4 ns=10
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:F2        User:wlb       Date_Time:Fri Mar 27 12:01:52 2015
 Task:LTGEN     User:wlb       Date_Time:Fri Mar 27 12:01:52 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       3.000E+00   1.280E+04   1.280E+04   1.280E+04   4.107E+05   4.107E+05   4.107E+05   4.107E+05   4.107E+05   4.107E+05
      2       3.000E+00   5.280E+04   5.280E+04   5.280E+04   6.971E+06   6.971E+06   6.971E+06   6.971E+06   6.971E+06   6.971E+06
      3       3.000E+00   9.280E+04   9.280E+04   9.280E+04   2.153E+07   2.153E+07   2.153E+07   2.153E+07   2.153E+07   2.153E+07
      4       3.000E+00   1.728E+05   1.728E+05   1.728E+05   7.465E+07   7.465E+07   7.465E+07   7.465E+07   7.465E+07   7.465E+07
let $echo="no"
Verify NUM_AREA, AREAS, NUM_EXP, and EXPOSURES labels.
label-list casltf.out
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File casltf.out ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                4 lines per band
                901 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: CASSINI-ISS ----
MISSION_NAME='CASSINI'
MISSION_PHASE_TYPE='BENCH'
INSTRUMENT_ID='ISSNA'
IMAGE_NUMBER=1
IMAGE_TIME='1997-128T12:34:56.800'
SOFTWARE_VERSION_ID='V4'
INSTRUMENT_MODE_ID='SUM2'
FILTER1_NAME='RED'
FILTER2_NAME='CLR'
GAIN_MODE_ID='100K'
ENCODING_TYPE='NOTCOMP'
CONVERSION_TYPE='8LSB'
DETECTOR_TEMPERATURE=-90.0
OPTICS_TEMPERATURE=-10.0
FILTER_TEMPERATURE=-999.0
LIGHT_FLOOD_STATE_FLAG='OFF'
ANTIBLOOMING_STATE_FLAG='OFF'
CALIB_LAMP_STATE_FLAG='OFF'
COMPRESSION_PARAMETER_VALUE=-1249
OFFSET=3
DARK_CURRENT=2
COMPRESSION_RATIO=-999.0
TARGET_NAME='FLATFIELD'
OBSERVATION_ID='LIGHT_XFR'
ILLUMINANT='TUNGSTEN'
LUMINANCE=5.1
RADIANCE=5.1
EXPOSURE_DURATION=0
---- Task: F2 -- User: wlb -- Fri Mar 27 12:01:52 2015 ----
FUNCTION='in1+10'
---- Task: LTGEN -- User: wlb -- Fri Mar 27 12:01:52 2015 ----
NUM_AREAS=100
AREAS=(15, 15, 20, 20, 15, 66, 20, 20, 15, 117, 20, 20, 15, 168, 20, 20, 15, 
219, 20, 20, 15, 270, 20, 20, 15, 321, 20, 20, 15, 372, 20, 20, 15, 423, 20, 
20, 15, 474, 20, 20, 66, 15, 20, 20, 66, 66, 20, 20, 66, 117, 20, 20, 66, 
168, 20, 20, 66, 219, 20, 20, 66, 270, 20, 20, 66, 321, 20, 20, 66, 372, 20, 
20, 66, 423, 20, 20, 66, 474, 20, 20, 117, 15, 20, 20, 117, 66, 20, 20, 117, 
117, 20, 20, 117, 168, 20, 20, 117, 219, 20, 20, 117, 270, 20, 20, 117, 321, 
20, 20, 117, 372, 20, 20, 117, 423, 20, 20, 117, 474, 20, 20, 168, 15, 20, 
20, 168, 66, 20, 20, 168, 117, 20, 20, 168, 168, 20, 20, 168, 219, 20, 20, 
168, 270, 20, 20, 168, 321, 20, 20, 168, 372, 20, 20, 168, 423, 20, 20, 168, 
474, 20, 20, 219, 15, 20, 20, 219, 66, 20, 20, 219, 117, 20, 20, 219, 168, 
20, 20, 219, 219, 20, 20, 219, 270, 20, 20, 219, 321, 20, 20, 219, 372, 20, 
20, 219, 423, 20, 20, 219, 474, 20, 20, 270, 15, 20, 20, 270, 66, 20, 20, 
270, 117, 20, 20, 270, 168, 20, 20, 270, 219, 20, 20, 270, 270, 20, 20, 270, 
321, 20, 20, 270, 372, 20, 20, 270, 423, 20, 20, 270, 474, 20, 20, 321, 15, 
20, 20, 321, 66, 20, 20, 321, 117, 20, 20, 321, 168, 20, 20, 321, 219, 20, 
20, 321, 270, 20, 20, 321, 321, 20, 20, 321, 372, 20, 20, 321, 423, 20, 20, 
321, 474, 20, 20, 372, 15, 20, 20, 372, 66, 20, 20, 372, 117, 20, 20, 372, 
168, 20, 20, 372, 219, 20, 20, 372, 270, 20, 20, 372, 321, 20, 20, 372, 372, 
20, 20, 372, 423, 20, 20, 372, 474, 20, 20, 423, 15, 20, 20, 423, 66, 20, 
20, 423, 117, 20, 20, 423, 168, 20, 20, 423, 219, 20, 20, 423, 270, 20, 20, 
423, 321, 20, 20, 423, 372, 20, 20, 423, 423, 20, 20, 423, 474, 20, 20, 474, 
15, 20, 20, 474, 66, 20, 20, 474, 117, 20, 20, 474, 168, 20, 20, 474, 219, 
20, 20, 474, 270, 20, 20, 474, 321, 20, 20, 474, 372, 20, 20, 474, 423, 20, 
20, 474, 474, 20, 20)
NUM_EXPOS=4
EXPOSURES=(0.0, 10.0, 20.0, 40.0)
 
************************************************************
let $echo="no"
$ Return
$!#############################################################################
