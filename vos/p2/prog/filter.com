$!****************************************************************************
$!
$! Build proc for MIPL module filter
$! VPACK Version 1.9, Monday, February 09, 2015, 19:53:24
$!
$! Execute by entering:		$ @filter
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
$ write sys$output "*** module filter ***"
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
$ write sys$output "Invalid argument given to filter.com file -- ", primary
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
$   if F$SEARCH("filter.imake") .nes. ""
$   then
$      vimake filter
$      purge filter.bld
$   else
$      if F$SEARCH("filter.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake filter
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @filter.bld "STD"
$   else
$      @filter.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create filter.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack filter.com -mixed -
	-s filter.f -
	-i filter.imake filter.afmake -
	-p filter.pdf -
	-t tstfilter.pdf tstfilter.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create filter.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C VICAR PROGRAM filter:    ARRAY PROCESSOR EMULATION VERSION OF FILTERAP
C  SPATIAL FILTER PROGRAM
C DERIVED FROM FIL.FTN  AP FILTER (PDP-11 TASK)
C THIS VERSION READS AND WRITES HALFWORD DATA 
C WEIGHTS CAN BE VERTICALLY SYMMETRIC OR ASYMMETRIC
C
C  09-09-09    ...RJB...  Allow images up to 8196 samples
C  03-08-14    ...NTT...  Enabled 3D image capability.  Reads are
C                         no longer sequential.  BIP images prohibited.
C  96-10-11    ...BAM...  Removed all checks for the physical size of the
C                         memory of the array processor which we dont
C                         have any more. I left in the checks for 4096
C                         samples, however, which really could be changed
C                         to a much bigger number at any time when required.
C  95-1-28     ...SP....  Modified for name change: from VADD to VADDEM.
C  95-1-2      ...AS....  (CRI)  MSTP S/W CONVERSION (VICAR PORTING)
C  85-8-18     ...LWK...  BUG FIXES
C  85-4-18     ...LWK...  CONVERTED TO VICAR2
C  84-10-11    ...LWK...  AP EMULATION VERSION
C  01 JAN  84  ...CCA...    MAKE HALFWORD RANGE -32768 TO 32767
C  01 NOV  83  ...CCA...    CONVERT TO VAX
C  13 JULY 81  ...HJF...   FIX BUG - DIVISOR > 32768 NOW WORKS
C  7/81        ...HJF... REMOVE APWD AND APWR (USE NEW DAPEX)
C  30 JUNE 81  ... ? ...    MAKE DIVIDE I*4, DEL SPACE,ADD ASYM
C  6/81        ...HJF... CORRECT DIVIDE ERROR (MAKE IT I*4)
C  27 MAY 81   ...HJF...   INITIAL RELEASE - ARRAY PROCESSOR

	IMPLICIT INTEGER(A-Z)
	COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	COMMON LSA,LMIN,LMAX,LSM
	COMMON NLWH,NSWH,NSS,NLWNSS
	COMMON /FPC/ SA,DNMIN,DNMAX,SM
	COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND

	real*4 SM,SA,DNMIN,DNMAX,FLOAT,RNSS,RSPACE		!RSPAC before 2-10-2010
	integer*4 BAND
	integer*4 BANDOUT,LINEOUT

	integer*2 W(8192),A(8192)
        integer*4 w4(8192)
        equivalence (w,w4)
        integer*4 a4(8192)
        equivalence (a,a4)

C	REAL T0,SECNDS,T1
C
        CALL IFMESSAGE('** Filter 13-Aug-2010 (64-bit) - RJB')
	CALL APINIT		!removed parms (1,0,ISTAT) !holdover from AP-120B Array processor on VAX
c	IF(ISTAT.LT.0) GOTO 910			! apinit routine doesn't do anything except return
	CALL GETPAR(W,SL,SS,SB,IFMT,OFMT,IUN,OUN)
	SKIP = SS 
	IF(NSO.GT.8192) GOTO 950
	NLWH=NLW/2
	NLWHP1=NLWH+1
	NSWH=NSW/2
	NSWHP1=NSWH+1
	NLWNSW=NLWHP1*NSW            ! TOP HALF OF SYMMETRIC WEIGHT MATRIX
	IF(ASYM.EQ.1) NLWNSW=NLW*NSW ! CHECK FOR ASYMMETRIC WTS
	NSS=NSO+NSW-1
	NLWNSS=NLW*NSS               ! AP MEMORY REQUIREMENT
	RNSS=NSS                     ! CALC AP SPACE REQUIRED
	RSPACE=RNSS*(NLW+1)+2*NSO+NLWNSW


c	IF(RSPACE.GT.65530) GOTO 940    bam 10/96

	SM=FLOAT(ISC(2))/FLOAT(I4DIV)        ! MULTIPLICATIVE SCALE FACTOR
	SA=ISC(1)+.5                         ! ADDITIVE SCALE FACTOR
	DNMIN=IDMIN
	DNMAX=IDMAX
C
C	SF.VFC USES THE FOLLOWING AP MEMORY LAYOUT
C	DO NOT CHANGE IT, UNLESS SF.VFC IS ALSO CHANGED *****
	LOC1=0
	LOC=0
	LWTS=NLWNSS
	LOUT=LWTS+NLWNSW                    ! LOC OF OUTPUT LINE
	LTEMP=LOUT+NSO                      ! LOC FOR TEMP STORAGE LINE
	LSUM=LTEMP+NSO
	LSA=LSUM+NSS
	LMIN=LSA+1
	LMAX=LMIN+1
	LSM=LMAX+1
C
c	T0=SECNDS(0.)
	CALL APPUT(SA,LSA,4,2)                 ! SEND R*4 CONSTANTS TO AP
	CALL APPUT(W,LWTS,2*NLWNSW,1)          ! TRANSFER THE WEIGHTS
	CALL APWD
	CALL VFLT(LWTS,1,LWTS,1,NLWNSW)        ! FLOAT THEM
	CALL VSMUL(LWTS,1,LSM,LWTS,1,NLWNSW)   ! SCALE THEM
	LCW=LWTS+NLWH*NSW          ! STORE LOC OF CENTER WT FOR SYM FILTER
	IF(ASYM.EQ.1) LCW=0        ! CHECK FOR ASYMMETRIC FILTER
C
C NLW LINES ARE STORED IN A CIRCULAR BUFFER
C LOC1 POINTS TO THE TOP LINE
C LCL POINTS TO THE CENTER LINE
C THE EDGES ARE HANDLED BY UNFOLDING
C FOR EXAMPLE, IF NLW EQ 5, THE INITIAL CIRCULAR BUFFER WILL CONTAIN
C LINE 3 AT LOC 3*NSS
C LINE 2 AT LOC 4*NSS
C LINE 1 AT LOC 0
C LINE 2 AT LOC NSS
C LINE 3 AT LOC 2*NSS
C
	BANDOUT=0
	DO 200 BAND=SB,SB+NBO-1
	BANDOUT = BANDOUT + 1
	LINEOUT = 0
	LCL=0                       ! LOC OF CENTER LINE
	LOC=0
	LINE = SL
	DO 360 L=1,NLWHP1      ! READ IN FIRST NLWHP1 LINES, STARTING AT 0
	CALL GNL(A)	   	! GET NEXT LINE INTO AP AT LOC
	LINE = LINE + 1
c	LINEOUT = LINEOUT + 1 		! NO MORE SEQUENTIAL READS!
360	LOC=LOC+NSS
C
	LOC1=0                      ! ASSUME NLWH EQ 0
	IF(NLWH.EQ.0) GOTO 375      ! IF NO UNFOLDING REQUIRED, GOTO 375
	LOC=0                       ! SET TO UNFOLD TOP OF PICTURE
	LOC1=NLWNSS
	DO 370 I=1,NLWH             ! UNFOLD NLWH LINES
	LOC=LOC+NSS                 ! STEP LOC OF LINE TO BE UNFOLDED
	LOC1=LOC1-NSS               ! STEP LOC OF UNFOLDED LINE
	CALL VMOV(LOC,1,LOC1,1,NSS) ! UNFOLD A LINE
370	CALL APWR
C
C NLW LINES ARE NOW IN AP MEMORY, LOC1 POINTS TO TOP LINE
C
375	OL=0                        ! INITIALIZE OUTPUT LINE COUNTER
400	OL=OL+1                     ! INCREMENT OUTPUT LINE COUNTER
	LINEOUT = LINEOUT + 1 
	CALL APFIL(A,LINEOUT,BANDOUT)               ! FILTER AND OUTPUT LINE OL
	IF(OL.EQ.NLO) GOTO 500      ! IF ALL DONE, GOTO 500
	LOC=LOC1                    ! LOC FOR NEXT INPUT LINE
	LOC1=LOC1+NSS               ! UPDATE LOC OF TOP LINE
	IF(LOC1.EQ.NLWNSS) LOC1=0
	LCL=LCL+NSS                 ! UPDATE LOC OF CENTER LINE
	IF(LCL.EQ.NLWNSS) LCL=0
	IF(OL.GE.NLO-NLWH) GOTO 450 ! IF NO MORE INPUT LINES, GOTO 450
	CALL GNL(A)		    ! GET NEXT LINE
	 LINE = LINE + 1 
	IF(OL.EQ.NLO-NLWHP1) LOCN=LOC       ! SAVE LOC OF LINE NLO
	GOTO 400
C
450	CONTINUE                    ! UNFOLD LINES AT BOTTOM OF PICTURE
	IF(LOCN.EQ.0) LOCN=NLWNSS
	LOCN=LOCN-NSS               ! STEP BACK ONE LINE
	CALL VMOV(LOCN,1,LOC,1,NSS) ! UNFOLD THE LINE
	GOTO 400
C
500	CONTINUE
200	CONTINUE				  !for each band...
c	CALL PRNT(7,1,SECNDS(T0),' SEC.')
	RETURN
C
C910	CALL MABEND('??E - APINIT ERR')
950	CALL MABEND('??E - NSO GT 8192')
	END

C***************************************************************
	SUBROUTINE APFIL(A,LINEOUT,BANDOUT)
	IMPLICIT INTEGER(A-Z)
	COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	COMMON LSA,LMIN,LMAX,LSM
	COMMON NLWH,NSWH,NSS,NLWNSS
	COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND
c	COMMON /OUT/ LINEOUT, BANDOUT
	INTEGER*2 A(8192)
	INTEGER*4 BANDOUT,LINEOUT

C
C	SEE SF.VFC FOR DETAILS
	CALL SF(LOC1,LWTS,LOUT,NSO,NSW,NLW,NSS,NLWNSS,LCW,LCL)
	CALL APWR
	CALL APGET(A,LTEMP,NSO,1)
	CALL APWD
	CALL XVWRIT( OUN, A, I,'LINE',LINEOUT,'NSAMPS', NSO,
     +	        'BAND',BANDOUT,' ')
C
	RETURN
        end
C
C=====================================================================
	 subroutine GNL(A)
	 IMPLICIT INTEGER(A-Z)
	 COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	 COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	 COMMON LSA,LMIN,LMAX,LSM
	 COMMON NLWH,NSWH,NSS,NLWNSS
	 COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND
	 INTEGER*2 A(8192)
	 INTEGER*4 BAND
C

      CALL XVREAD( IUN, A, I ,'LINE', LINE, 'SAMP', SKIP,'NSAMPS', NSO,
     +	'BAND', BAND, ' ')
	
	 CALL APPUT(A,LTEMP,NSO,1)          ! TRANSFER NEXT LINE TO AP
	 CALL APWD

	 CALL FLN(LTEMP,LOC+NSWH,LOC+NSS-NSWH,NSO,NSWH) ! UNFOLD IT

	 RETURN
	 END

C**********************************************************************
         SUBROUTINE GETPAR(WTAB,SL,SS,SB,IFMT,OFMT,IUN,OUN)
         IMPLICIT INTEGER(A-Z)
         COMMON NLO,NSO,NBO,NLW,NSW,DIVIDE,ISC(2),IDMIN,IDMAX,IASYM
         INTEGER*4 NONS,DIVSW,SCALE(2),WBUF(50000)
         INTEGER*4 ASYM, MINSW, MAXSW, RANGE(2)
         INTEGER*2 WTAB(8192),W(120)
	LOGICAL*4 XVPTST
	    CHARACTER*8 FMT
	    CHARACTER*3 ORGIN
         CHARACTER*90 ERROR3
         DATA NONS/0/,DIVSW/0/,SCALE/0,1/,ASYM/0/
         DATA MINSW/0/, MAXSW/0/, RANGE/0,255/
         DATA W/-23,-19,5,31,43,42,37,34,37,42,43,31,5,-19,-23,-16,17,  
     *                       42,36,7,-20,-36,-40,-36,-20,7,36,42,17,-16,
     *            15,44,22,-28,-59,-52,-28,-17,-28,-52,-59,-28,22,44,15,
     *            41,25,-40,-69,-12,78,137,153,137,78,-12,-69,-40,25,41,
     *           41,-21,-76,-8,135,185,103,40,103,185,135,-8,-76,-21,41,
     *     22,-61,-58,102,193,-67,-566,-825,-566,-67,193,102,-58,-61,22,
     *    3,-80,-19,173,94,-607,-1617,-2104,-1617,-607,94,173,-19,-80,3,
     *      -3,-84,0,191,16,-897,-2141,30001,-2141,-897,16,191,0,-84,-3/
C
         ERROR3(1:45)='FILTER-REQUESTED SIZE OF PICTURE EXCEEDS SIZE'
         ERROR3(46:90)=' SPECIFIED IN SYSTEM LABEL.  POSSIBLE ERROR. '
	CALL XVUNIT( IUN, 'INP', 1, I,' ')
	CALL XVOPEN( IUN, I, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .	 'U_FORMAT', 'HALF',' ')
	CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
	CALL XVBANDS(SB, NB, NBI)
 
	IF ( SB .GT. NBI ) CALL MABEND(
     +  '??W - SB is greater than the total number of bands')
                 
      IF ( SB + NB - 1 .GT. NBI) THEN 
	 CALL XVMESSAGE('***Number of bands truncated', ' ')
	 NB = NBI + 1 - SB
      ENDIF
	

c     Check organization of image, prohibit BIP
      CALL XVGET(IUN,I,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  '??E - BIP files unsupported, use program TRAN to convert to BSQ')	 

	IF (NL.GT.NLI .OR. NS.GT.NSI) THEN
	  CALL MABEND(ERROR3)
	ENDIF
	IF (NS.GT.8192) GOTO 970

	CALL XVGET(  IUN, I, 'FORMAT', FMT,' ')
	IF (FMT.EQ.'BYTE') THEN
	  IFMT = 1
	ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	  IFMT = 2
	ELSE
	  CALL MABEND('??E - ONLY BYTE/HALF FORMATS SUPPORTED **')
	ENDIF

C  OUTPUT FORMAT:

	OFMT = IFMT			! BY DEFAULT, SAME AS INPUT
	IF (XVPTST('BYTE')) THEN
	  OFMT = 1
	  FMT = 'BYTE'
	ELSEIF (XVPTST('HALF')) THEN
	  OFMT = 2
	  FMT = 'HALF'
	ENDIF

C  WEIGHTS

	CALL XVPARM( 'NLW', NLW, I, NLDEF,1)
	IF (I.EQ.0) NLW = 15		!FOR CASE WHEN CALLED INSIDE PROC
	CALL XVPARM( 'NSW', NSW, I, NSDEF,1)
	IF (I.EQ.0) NSW = 15
	CALL XVPARM( 'WEIGHTS', WBUF, NW, WDEF,500)
	IF (NW.EQ.0) THEN
	  IF (NLW.EQ.15 .AND. NSW.EQ.15) THEN
	    CALL MVE( 6, 120, W, WBUF,1,1)	!DEFAULT WEIGHTS
	    NONS = 1				!NONSYMMETRIC
	    ASYM = 0
	  ELSE
	    CALL MABEND('??E - DEFAULT WEIGHTS REQUIRE NLW=NSW=15')
	  ENDIF
	ELSE
	  IF (XVPTST('NONSYMME')) NONS = 1
	  IF (XVPTST('ASYMMETR')) ASYM = 1
	ENDIF

C   IF NONSYMMETRIC OPTION WAS SPECIFIED ,THEN TOP HALF OF WEIGHT
C    TABLE IS INPUT
	ILA=(NLW+1)/2
	IF (NONS.EQ.0 .AND. ASYM.EQ.0) THEN
	  IF(ILA*NSW.GT.8192) GOTO 960
	  ILB=(NSW+1)/2
	  DO IJ=1,ILA
	  DO IK=1,ILB
	    IKM=IK+(IJ-1)*ILB
	    WTAB((IJ-1)*NSW+IK) = WBUF(IKM)
	    WTAB(IJ*NSW+1-IK) = WBUF(IKM)
	  ENDDO
	  ENDDO
	ELSE
	  ILA=ILA*NSW
          IF (ASYM.EQ.1) ILA=NLW*NSW
	  IF (ILA.GT.8192) GOTO 960
	  DO IJ=1,ILA
	    WTAB(IJ) = WBUF(IJ)
	  ENDDO
	ENDIF

	CALL XVPARM( 'RANGE', RANGE, CNT, J,2)
	IF (CNT.GT.0) THEN
	  MINSW = 1
	  MAXSW = 1
	ELSE
	  CALL XVPARM( 'DNMIN', RANGE, CNT, J,1)
	  IF (CNT.GT.0) MINSW = 1
	  CALL XVPARM( 'DNMAX', RANGE(2), CNT, J,1)
	  IF (CNT.GT.0) MAXSW = 1
	ENDIF

	CALL XVPARM( 'SCALE', SCALE, I, J,2)

	CALL XVPARM( 'DIVIDE', DIVIDE, CNT, J,1)
	IF (CNT.GT.0) DIVSW = 1
C
	IJF = NSW*(NLW-1)/2
	IJF1 = IJF+1
	IJF2 = IJF+NSW			! NUMBER OF WEIGHTS TO SEND
	IF(ASYM.EQ.1) IJF2=NLW*NSW
C CHECK THAT DATA WILL FIT IN AP MEMORY
c	IF ((NLW+1)*(NS+NSW-1)+2*NS+IJF2.GT.65530) GO TO 980

	IF (DIVSW.LE.0) THEN
	  DIVIDE=0
	  IF (ASYM.EQ.1) THEN
	    DO I2=1,IJF2
	      DIVIDE = DIVIDE+WTAB(I2)
	    ENDDO
	  ELSE
	    IF (IJF.NE.0) THEN
	      DO I2=1,IJF
	        DIVIDE = DIVIDE+WTAB(I2)*2
	      ENDDO
	    ENDIF
	    DO I2=IJF1,IJF2
	      DIVIDE = DIVIDE+WTAB(I2)
	    ENDDO
	  ENDIF
	ENDIF
	IF (DIVIDE.EQ.0) DIVIDE=1

      NLO = NL
      NSO = NS
      NBO = NB
C
C  OPEN OUTPUT
	CALL XVUNIT( OUN, 'OUT', 1, I,' ')
	CALL XVOPEN( OUN, I, 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     .   'O_FORMAT', FMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .   'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')  ! added 8.22.03

	NEO2 = NSO*2		! # OF BYTES
	ISC(1) = SCALE(1)
	ISC(2) = SCALE(2)
	IDMIN = RANGE(1)
	IDMAX = RANGE(2)
	IF (MINSW.EQ.0) THEN
	  IF (OFMT.EQ.1) IDMAX = 0
	  IF (OFMT.EQ.2) IDMAX = -32768
	ENDIF
	IF (MAXSW.EQ.0) THEN
	  IF (OFMT.EQ.1) IDMAX = 255
	  IF (OFMT.EQ.2) IDMAX = 32767
	ENDIF
	IASYM=ASYM

	RETURN

960      CALL MABEND('??E - NLW*NSW TOO LARGE')
970      CALL MABEND('??E - NS GT 8192')
        END

C*****************************************************************
C THE FOLLOWING ROUTINES REPLACE VFC ROUTINES IN THE AP VERSION:


	SUBROUTINE SF(ILOC1,ILWTS,ILOUT,INSO,INSW,INLW,INSS,INLWNSS,
     .	  ILCW,ILCL)

C THIS REPLACES VFC ROUTINE. NOTE ALL ARGUMENTS ARE COPIED TO LOCALS.

	IMPLICIT INTEGER(A-Z)

	LOC1 = ILOC1
	LWTS = ILWTS
	LOUT = ILOUT
	NSO = INSO
	NSW = INSW
	NLW = INLW
	NSS = INSS
	NLWNSS = INLWNSS
	LCW = ILCW
	LCL = ILCL

	LTEMP = LOUT+NSO
	LSUM = LTEMP+NSO
	LSA = LSUM+NSS
	LMIN = LSA+1
	LMAX = LMIN+1

	IF(LCW.EQ.0) GOTO 50	! CHECK FOR ASYMMETRIC WTS
	CALL CONV(LCL,1,LCW,1,LOUT,1,NSO,NSW) ! SYMMETRIC WTS
	LT=LCL
	LB=LCL
	DO 40 I=1,NLW/2
	IF (LT.EQ.0) LT = NLWNSS
	LT=LT-NSS ! LOC OF TOP LINE
	LB=LB+NSS ! LOC OF BOTTOM LINE
	IF (LB.EQ.NLWNSS) LB = 0
	CALL VADDEM(LT,1,LB,1,LSUM,1,NSS)
	LCW=LCW-NSW
	CALL CONV(LSUM,1,LCW,1,LTEMP,1,NSO,NSW)
	CALL VADDEM(LTEMP,1,LOUT,1,LOUT,1,NSO)
40	CONTINUE
	GOTO 60

50	CALL CONV(LOC1,1,LWTS,1,LOUT,1,NSO,NSW)
	LD = LOC1
	LW = LWTS
	DO 80 L=1,NLW-1
	LD = LD+NSS
	IF (LD.EQ.NLWNSS) LD = 0
	LW = LW+NSW
	CALL CONV(LD,1,LW,1,LTEMP,1,NSO,NSW)
	CALL VADDEM(LTEMP,1,LOUT,1,LOUT,1,NSO)
80	CONTINUE

60	CALL VSADD(LOUT,1,LSA,LOUT,1,NSO)
	CALL VCLIP(LOUT,1,LMIN,LMAX,LOUT,1,NSO)
	CALL VFIX(LOUT,1,LTEMP,1,NSO)               ! FIX

	RETURN
	END
	SUBROUTINE FLN(ILTEMP,ILS,ILE,INSO,INSWH)

C REPLACE FLN.VFC

	IMPLICIT INTEGER (A-Z)

	LTEMP = ILTEMP
	LS = ILS
	LE = ILE
	NSO = INSO
	NSWH = INSWH

	CALL VFLT(LTEMP,1,LS,1,NSO) ! FLOAT
	IF(NSWH.EQ.0) RETURN
	CALL VMOV(LS+1,1,LS-1,-1,NSWH)
	CALL VMOV(LE-2,-1,LE,1,NSWH)

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create filter.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM filter

   To Create the build file give the command:

		$ vimake filter			(VMS)
   or
		% vimake filter			(Unix)


************************************************************************/


#define PROGRAM	filter
#define R2LIB

#define MODULE_LIST filter.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define FTNINC_LIST fortport
/************************* End of Imake file ***************************/

$!-----------------------------------------------------------------------------
$ create filter.afmake
#==============================================================
# List of things to build

PROG_LIST = filter

# Include makefile to get rules
include $(AFIDS_ROOT)/AfidsMakefile.in

#CPPFLAGS += -I$(AFIDSTOP)/include -I.
FFLAGS += -Wunused -Wuninitialized -Wsurprising

#==============================================================
# Extra files stretch depends on.



$ Return
$!#############################################################################
$PDF_File:
$ create filter.pdf
process help=*
PARM	INP	TYPE=STRING	
PARM	OUT	TYPE=STRING
PARM	SIZE	TYPE=INTEGER	COUNT=4        DEFAULT=(1,1,0,0)
PARM	SL	TYPE=INTEGER			DEFAULT=1
PARM	SS	TYPE=INTEGER			DEFAULT=1
PARM	SB	TYPE=INTEGER			DEFAULT=1
PARM	NL	TYPE=INTEGER			DEFAULT=0
PARM	NS	TYPE=INTEGER			DEFAULT=0
PARM	NB	TYPE=INTEGER			DEFAULT=0
PARM	NLW	TYPE=INTEGER			DEFAULT=15
PARM	NSW 	TYPE=INTEGER			DEFAULT=15
PARM	RANGE	TYPE=INTEGER	COUNT=(0:2)	DEFAULT=--
PARM	SCALE	TYPE=INTEGER	COUNT=2		DEFAULT=(0,1)
PARM	DIVIDE	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM 	WEIGHTS	TYPE=INTEGER	COUNT=0:500	DEFAULT=--
PARM	SYMM KEYWORD VALID=(NONSYMME,ASYMMETR) COUNT=(0:1) DEFAULT=--
PARM	DNMAX	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	DNMIN	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM    OFORM   KEYWORD  COUNT=0:1   VALID=(BYTE,HALF)  DEFAULT=--
PARM    PARMS   TYPE=(STRING,100) COUNT=0:1   DEFAULT=--
END-PROC
.TITLE
VICAR1 PROGRAM "FILTER"
.HELP
PURPOSE:

"FILTER" is a VICAR applications program which performs two-dimensional
convolution filtering.  For each output sample, the program computes
a weighted average of a rectangular set of input pixels followed by
a linear transformation.  "FILTER" may be used to perform high-pass or 
low-pass filtering.

The following system procedure will invoke "FILTER":

  FILTER2 - first invoked program "FIL2", which generates a proper set
        of weights, and then "FILTER".

It is recommended that the user normally calls the above procedure
rather than "FILTER" directly.

.page
EXECUTION:

   The following is the execution statement format for "FILTER":

             FILTER INP=PIX OUT=OPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 

OPERATION:

Before "FILTER" can be used a proper set of weights must be generated for
the camera system which took the picture.  These weights are usually 
generated by the weight generator program "FIL2" (invoked by procedure
"FILTER2").
.page
"FILTER" accepts as input a rectangular set of weights and calculates each
output point FP(l,s) as follows:
               nlw nsw 
    TP(l,s) = SUM SUM  P[l-(nlw+1)/2 + i , s-(nsw+1)/2 + j] * W(i,j)
               i=1 j=1
and
    FP(l,s) = A + B * TP(l,s) /  D
where
	P  	is the input file
	W	is the weight matrix
	nlw 	is the number of lines in the weight matrix
	nsw  	is the number of samples in the weight matrix
	A,B	are the two scaling inputs
	D	is the parameter for scaling
.PAGE
RESTRICTIONS:

1) Maximum number of output samples per line is 4096.
2) For asymmetric FILTERs, NW = NLW*NSW must not exceed 4096.
3) For symmetric or vertically symmetric FILTERs, NW = NSW * (NLW+1)/2
	must not exceed 4096.
.PAGE
EXAMPLES:

1) FILTER IN OUT
   The file will be filtered by the default set of weights.

2) FILTER IN OUT NLW=3 NSW=5 WEIGHTS=(0,-1,-2,-1,-2,10)
   The file will be filtered with the weight table shown below:
         0   -1   -2   -1   0
	-1   -2   10   -2   -1
	 0   -1   -2   -1   0
   The final linear transformation is
		FP(L,S) = 0 + 1*TP(L,S) / 6
   where 6 is the default for the DIVIDE parameter, (i.e., the sum of the
   entries in the weight table).

3) FILTER IN OUT NLW=3 NSW=5 SCALE=(-30,1) DIVIDE=3 +
	WEIGHTS=(0,-1,-2,-1,-2,10)
   This is the same as the example above except the final transformation is 
   given by:
		FP(L,S) = -30 + 1*TP(L,S) / 3

4) FILTER IN OUT NLW=3 NSW=5 'NONS 'HALF DNMIN=-15 +
	WEIGHTS=(-2,5,4,3,-1,-5,20,3,1,1)
   These parameters generate the horizontally nonsymmetric weights shown
   below:
   	 -2   5   4   3   -1
	 -5  20   3   1    1
	 -2   5   4   3   -1
   and the halfword output will be computed as follows:  If the output is less
   than or equal to -15 dn, it is set to -15 dn.  If the output is greater than
   or equal to 32767 dn, then it is set to 32767 dn.  If the output is between
   -15 and 32767 dn, then no linear transformation is applied.

5) FILTER IN OUT NLW=3 NSW=3 'ASYM WEIGHTS=(-1,0,0,0,0,0,0,0,1)
   These parmeters will produce a diagonal gradient picture using this weight
   matrix:  
  	-1   0   0
	 0   0   0
	 0   0   1
.PAGE
TIMING:
  TBD

AP VERSION WRITTEN BY:  H. J. FREIDEN,   27 MAY 1981

EMULATION VERSION IMPLEMENTED BY:  L.W.KAMP,  11 OCT. 1984

COGNIZANT PROGRAMMER:  L. W. KAMP

REVISIONS:  
        27 MAY 81   ...HJF...   INITIAL RELEASE - ARRAY PROCESSOR
        6/81        ...HJF... CORRECT DIVIDE ERROR (MAKE IT I*4)
        30 JUNE 81  ... ? ...    MAKE DIVIDE I*4, DEL SPACE,ADD ASYM
        7/81        ...HJF... REMOVE APWD AND APWR (USE NEW DAPEX)
        13 JULY 81  ...HJF...   FIX BUG - DIVISOR > 32768 NOW WORKS
        01 NOV  83  ...CCA...    CONVERT TO VAX
        01 JAN  84  ...CCA...    MAKE HALFWORD RANGE -32768 TO 32767
        84-10-11    ...LWK...  AP EMULATION VERSION
        85-4-18     ...LWK...  CONVERTED TO VICAR2
        85-8-18     ...LWK...  BUG FIXES

        95-02-01    A. Scop (CRI)   Made portable for UNIX
        95-1-28     ...SP....  Modified for name change: from VADD to VADDEM.
        95-10-22    F. Moss    Rename FILTEREM to FILTER throughout
                               the entire com file, make programs
                               FILTERAP, APFLAG, FILTEREM, & proc
                               FILTER obsolete at a later date.
        96-10-11    ...BAM...  Removed all checks for the physical size of the
                                memory of the array processor which we dont
                                have any more. I left in the checks for 4096
                                samples, however, which really could be changed
                                to a much bigger number at any time when required.
                                size.          
        03-08-14    ...NTT...  Enabled 3D image capability.  Reads are
                                no longer sequential.  BIP images prohibited.
                                Internal processing in HALF format
        09-09-09    ...RJB...  Allow images up to 8196 samples
        2010-02-10  R. Bambery    Fixes for 64-bit Linux, MacOSX (Intel/PowerPC)
        2010-02-14  R. Bambery  Reincorporate changes of smyth for XVPTST from
                                09-21-09
        2010-08-13  R. Bambery  Changed apinit call with parameters to apinit
                                without parameters to avoid errors on MacOSX:
                                (gfortran 4.4.4 vsl g77 3.4.6 on linux)
                                filter filter.X filter.Y (41,41,100,100)
                                Beginning VICAR task filter
                                ** FILTER 14-FEB-2010 (64-bit) - RJB
                                ??E - APINIT ERR
                                 ** ABEND called **


.LEVEL1
.VARI INP
Input image file
.VARI OUT
filtered image file
.VARI SIZE
Vicar size field
.VARI SL
size field starting line
.VARI SS
Size field starting sample
.VARI SB
Size field starting band
.VARI NL
Size field number of lines
.VARI NS
Size field number of samples
.VARI NB
Size field number of bands
.VARI NLW
Number of lines of weights
.VARI NSW
Number of samples of weights
.VARI SYMM
Horizontally nonsymmetric or 
asymmetric weights?
.VARI WEIGHTS
Defines the weight matrix
.VARI DIVIDE
Scaling paramater upon output
.VARI SCALE
Linear output scaling:
(offset,scale)
.VARI DNMAX
Defines maximum output dn
.VARI DNMIN
Defines minimum output dn
.VARI RANGE
DN interval to which output
will be clipped.
.vari OFORM
Output data format.
(Default: input format)
.VARI PARMS
 Parameter data set name 
.LEVEL2
.VARI INP
A VICAR labeled image file
.VARI OUT
A file to write the filtered product into
.VARI SIZE
The standard size field defining the area of the input picture that is to
be filtered.
.VARI SL
Starting line of the area to be filtered.
.VARI SS
Starting sample of the area to be filtered
.VARI SB
Starting band to be filtered
.VARI NL
Number of lines in the area to be filtered.
.VARI NS
Number of samples in the area to be filtered.
.VARI NB
Number of bands to be filtered.
.VARI NLW
This specifies the number of lines of weights. Must be odd. Default = 15
.VARI NSW
This specifies the number of samples in each line of the weight matrix.
Must be odd. Always refers to samples not bytes. Default = 15
.VARI SYMM
This keyword parameter has 2 valid values:  NONSYMME and ASYMMETR.

If it is null (default), then the weight matrix is assumed to be
  symmetric, i.e., only one quadrant containing (NLW+1)/2*(NSW+1)/2 weights
  is input.

NONSYMME indicates horizontally nonsymmetric weights, i.e., the weights to
  be input will consist of ( NSW * (NLW+1)/2 ) integers representing the
  upper half of the vertically symetric FILTER weight matrix.  

ASYMMETR indicates that NLW*NSW weights for the asymmetric FILTER will be 
  input.
.VARI WEIGHTS
This keyword is followed by ( (NLW+1) * (NSW+1) / 4 ) integers, representing 
the upper left quadrant of the four-way symmteric matrix of weights.  

If NONSYMMETRIC or ASYMMETRIC was specified, then the upper half or all
of the weight matrix must be specified, respectively.

Each value must be less than 32767 in absolute value.  

Default is the SURVEYOR high-pass FILTER (15*15 weights, symmetric).
.VARI DIVIDE
This is used in the final transformation equation.  Each output point O(l,s)
is given by:

        O(l,s) = A + B*T(l,s) / DIVIDE

where A and B are defined by SCALE and T is the output of the convolution.
The default is that the sum of the weights is used.  If this sum is zero, 
then 1 is used.
.VARI SCALE
This keyword specifies the application of a linear transformation to each
output point T(l,s):

         O(l,s) = A + B*T(l,s) / DIVIDE

where SCALE=(A,B).  Default is SCALE=(0,1).
See also DIVIDE.
.VARI DNMAX
All pixels with DN greater than DNMAX upon output are set to DNMAX. 

DNMAX is synonymous with RANGE(1).

The default depends on the output data format: 255 for byte data, 
and 32767 for halfword.
.VARI DNMIN
All pixels with DN less than DNMIN upon output are set to DNMIN. 

DNMIN is synonymous with RANGE(2).

The defaults depends on the output data format: 0 for byte, and 
-32768 for halfword.

DNMIN must be positive for byte data.  
.VARI RANGE
The pair of values (a,b) specifies the range of DN to which the output will
be clipped.

RANGE is synonymous with (DNMIN, DNMAX).  If both RANGE and DNMIN or DNMAX
are specified, then RANGE will take precedence.

The default depends on the output data format: it is (0,255) for Byte data,
and (32768,32767) for Halfword.
.vari OFORM
This specifies the data format of the output.  Valid: BYTE, HALF.

Default is that the output has the same format as the input.
.VARI PARMS
 PARMS can be used to specify the name of an optional parameter data
 set. Any combination of the allowable parameters may be given. If
 any of the parameters are given interactively, the interactive value
 takes precedence.
$ Return
$!#############################################################################
$Test_File:
$ create tstfilter.pdf
procedure
parm    mode    type=keyword count=(0:1) valid=(batch,nobatch,inter) default=batch

local oldsfi type=integer count=1 init=0
local oldskey type=string count=1 init=""

! Jun 22, 2012 - RJB
! TEST SCRIPT FOR FILTER
! tests BYTE, HALF images
!
! Vicar Programs:
!       gen list difpic cform xvd
! 
! External Programs:
!   <none>
!
! Parameters:
!   mode - method for processing: 
!       1) batch provides no xvd display
!       2) interactive or nobatch is used for xvd 
!           display requiring user interaction.
!           
!
! Requires NO external test data: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
refgbl $echo
refgbl $autousage
refgbl $SFI
refgbl $SKEY

body
let $echo="no"
let $autousage="none"
let _onfail= "goto erry"        !if command abends

write "*******************************************************"
write "THIS IS A TEST OF MODULE filter"
write "WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS"
write "AFTER EACH RUN."
write "*******************************************************"
write " "
!
write "Generate a picture"
let $echo="yes"
gen A nl=64 ns=64 ival=0 linc=1 sinc=1 modulo=6
list A size=(1,1,10,10)
f2 A filter.A func=("in1*51")

list filter.A size=(1,1,10,10)
let $echo="no"
!write "List  a small 10X10 area of the input"
!
write "TEST 1 - Do a default filter  (Surveyor Footpad Filter Kernel)"
let $echo="yes"
filter filter.A filter.Y1 
list filter.Y1 size=(1,1,10,10)
difpic (filter.A,filter.Y1) filter.dif1
list filter.dif1 size=(1,1,10,10)
if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.A
    xvd filter.Y1
    xvd filter.dif1
end-if

!
let $echo="no"
write "TEST 2 - Do default filter using size field"
let $echo="yes"
!filter filter.A filter.Y2 (41,41,100,100)
filter filter.A filter.Y2 (7,7,39,39)
list filter.Y2 size=(1,1,10,10)
!difpic (filter.X,filter.Y2) filter.dif2
!list filter.dif1 size=(1,1,10,10)

if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.A
    xvd filter.Y2
end-if

!
let $echo="no"
write "TEST 3 - Now filter with halfword input and output"
let $echo="yes"
!insert filter.A filter.Y2 (41,41,100,100)
cform filter.A filter.Z oform=half
filter filter.Z filter.W 
list filter.W size=(1,1,10,15) 
difpic (filter.Z,filter.W) filter.dif2
list filter.dif2 size=(1,1,10,10)
if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.Z
    xvd filter.W
    xvd filter.dif2
end-if

!
let $echo="no"
write "TEST 4 - Now filter with nonsymmetric weights"
let $echo="yes"
filter filter.A filter.V nlw=5 nsw=5 'nons +
   weights=(-60,10,10,10,-50,   +
	    -10,60,40,50,-20,   +
	     -5,80,100,70,-10)
list filter.V size=(1,1,10,10)
difpic (filter.A,filter.V) filter.dif3
list filter.dif3 size=(1,1,10,10)
if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.A
    xvd filter.V
    xvd filter.dif3
end-if

!
let $echo="no"
write "TEST 5 - Now do same with a scale factor to multiply values by 2"
let $echo="yes"
filter filter.A filter.U nlw=5 nsw=5 'nons scale=(0,2) +
   weights=(-60,10,10,10,-50,   +
	    -10,60,40,50,-20,   +
	     -5,80,100,70,-10)
list filter.U size=(1,1,10,10)
difpic (filter.A,filter.U) filter.dif4
list filter.dif4 size=(1,1,10,10) 'zer

if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.A
    xvd filter.U
    xvd filter.dif4
end-if


let $echo="no"
write "TEST 6 - Now filter with asymmetric weights"
let $echo="yes"
filter filter.A filter.R nlw=3 nsw=3 'asym +
	weights=(-20,50,20      +
	           5,100,0      +
	         -10,20,-10)
list filter.R size=(1,1,10,10)
difpic (filter.A,filter.R) filter.dif5
list filter.dif5 size=(1,1,10,10)

if (mode = "nobatch" or mode = "inter")
    let $echo="no"
    xvd filter.A
    xvd filter.R
    xvd filter.dif4
end-if

!
! Test a 3D image
!
gen filter.3X nl=100 ns=100 nb=3 ival=10 sinc=15 linc=20 binc=30
list filter.3X size=(41,41,10,10)
let $echo="no"
write "TEST 7 - Test a 3D image"
let $echo="yes"

filter filter.3X filter.3Y 
list filter.3Y size=(1,1,10,10) sb=1 nb=3
let $echo="no"
goto endit
!!!!!!
! ERROR ROUTINES
!!!!!!
erry>
let $echo="no"
let _onfail = "continue"               ! continue even if command abends
let oldsfi = $SFI                       ! we want a fail log
let oldskey = $SKEY

write "### abend SFI = &$SFI  SKEY = &$SKEY"

! if there are no other errors (ie, sfi > -1) then if the
! following commands fail, ie, if no files are found, then
! they return sfi = 1
! if sfi = -1 and oldsfi = -1 then pdf is a fail
!!!!!!
! WRAPUP
!!!!!!
endit>
write "### endit> | SFI = &$SFI   SKEY = &$SKEY | oldSFI = &oldsfi oldSKEY = &oldskey"

!!let $echo = "yes"
if ($SFI = 1 and oldsfi < 0) let $SFI = &oldsfi
if ($SFI < 0 and oldsfi = 0) let $SFI = 0
if ($SKEY = "1000") return $SFI=1000
return

end-proc
$!-----------------------------------------------------------------------------
$ create tstfilter.log
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

*******************************************************
THIS IS A TEST OF MODULE filter
WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS
AFTER EACH RUN.
*******************************************************
 
Generate a picture
gen A nl=64 ns=64 ival=0 linc=1 sinc=1 modulo=6
Beginning VICAR task gen
GEN Version 6
GEN task completed
list A size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   0   1   2   3
      2       1   2   3   4   5   0   1   2   3   4
      3       2   3   4   5   0   1   2   3   4   5
      4       3   4   5   0   1   2   3   4   5   0
      5       4   5   0   1   2   3   4   5   0   1
      6       5   0   1   2   3   4   5   0   1   2
      7       0   1   2   3   4   5   0   1   2   3
      8       1   2   3   4   5   0   1   2   3   4
      9       2   3   4   5   0   1   2   3   4   5
     10       3   4   5   0   1   2   3   4   5   0
f2 A filter.A func=("in1*51")
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using byte table lookup
FUNCTION EVALUATED 256 TIMES
list filter.A size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:F2        User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0  51 102 153 204 255   0  51 102 153
      2      51 102 153 204 255   0  51 102 153 204
      3     102 153 204 255   0  51 102 153 204 255
      4     153 204 255   0  51 102 153 204 255   0
      5     204 255   0  51 102 153 204 255   0  51
      6     255   0  51 102 153 204 255   0  51 102
      7       0  51 102 153 204 255   0  51 102 153
      8      51 102 153 204 255   0  51 102 153 204
      9     102 153 204 255   0  51 102 153 204 255
     10     153 204 255   0  51 102 153 204 255   0
let $echo="no"
TEST 1 - Do a default filter  (Surveyor Footpad Filter Kernel)
filter filter.A filter.Y1
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.Y1 size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   0   0  82 255 255   0   0   0  89
      2       0  87 148 255 255   0   0  78 141 255
      3       0 147 255 255   0   0  75 177 255 255
      4      82 255 255   0   0  66 165 255 255   0
      5     255 255   0   0  79 175 255 255   0   0
      6     255   0   0  64 174 255 255   0   0  75
      7       0   0  73 163 255 255   0   0  76 175
      8       0  78 177 255 255   0   0  78 177 255
      9       0 141 255 255   0   0  75 177 255 255
     10      87 255 255   0   0  75 175 255 255   0
difpic (filter.A,filter.Y1) filter.dif1
Beginning VICAR task difpic
DIFPIC version 06Oct11
 AVE VAL OF POS DIFFS=  39.1419
 NUMBER OF POS DIFF=1367
 AVE VAL OF NEG DIFFS= -39.1201
 NUMBER OF NEG DIFFS=1365
 TOTAL NUMBER OF DIFFERENT PIXELS=2732
 AVE VAL OF DIFFS= 0.263672E-01
 % DIFF PIXELS=  66.6992
list filter.dif1 size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:DIFPIC    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0  51 102  71   0   0   0  51 102  64
      2      51  15   5   0   0   0  51  24  12   0
      3     102   6   0   0   0  51  27   0   0   0
      4      71   0   0   0  51  36   0   0   0   0
      5       0   0   0  51  23   0   0   0   0  51
      6       0   0  51  38   0   0   0   0  51  27
      7       0  51  29   0   0   0   0  51  26   0
      8      51  24   0   0   0   0  51  24   0   0
      9     102  12   0   0   0  51  27   0   0   0
     10      66   0   0   0  51  27   0   0   0   0
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 2 - Do default filter using size field
filter filter.A filter.Y2 (7,7,39,39)
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.Y2 size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   0   0  82 255 255   0   0   0  89
      2       0  87 148 255 255   0   0  78 141 255
      3       0 147 255 255   0   0  75 177 255 255
      4      82 255 255   0   0  66 165 255 255   0
      5     255 255   0   0  79 175 255 255   0   0
      6     255   0   0  64 174 255 255   0   0  75
      7       0   0  73 163 255 255   0   0  76 175
      8       0  78 177 255 255   0   0  78 177 255
      9       0 141 255 255   0   0  75 177 255 255
     10      87 255 255   0   0  75 175 255 255   0
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 3 - Now filter with halfword input and output
cform filter.A filter.Z oform=half
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
filter filter.Z filter.W
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.W size=(1,1,10,15)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     0     0    82   295   568     0     0     0    89   303   569     0     0     0
      2         0    87   148   266   476     0     0    78   141   267   481     0     0    78   141
      3         0   147   307   491     0     0    75   177   305   483     0     0    75   177   305
      4        82   265   490     0     0    66   165   303   491     0     0    67   167   303   491
      5       297   477     0     0    79   175   300   481     0     0    78   174   300   481     0
      6       571     0     0    64   174   308   485     0     0    75   177   306   483     0     0
      7         0     0    73   163   299   485     0     0    76   175   303   483     0     0    76
      8         0    78   177   303   481     0     0    78   177   303   480     0     0    78   177
      9         0   141   307   494     0     0    75   177   303   480     0     0    78   177   303
     10        87   267   484     0     0    75   175   303   480     0     0    78   177   303   480
difpic (filter.Z,filter.W) filter.dif2
Beginning VICAR task difpic
DIFPIC version 06Oct11
 AVE VAL OF POS DIFFS=  39.1419
 NUMBER OF POS DIFF=1367
 AVE VAL OF NEG DIFFS= -116.115
 NUMBER OF NEG DIFFS=2047
 TOTAL NUMBER OF DIFFERENT PIXELS=3414
 AVE VAL OF DIFFS= -44.9658
 % DIFF PIXELS=  83.3496
list filter.dif2 size=(1,1,10,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:DIFPIC    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0    51   102    71   -91  -313     0    51   102    64
      2        51    15     5   -62  -221     0    51    24    12   -63
      3       102     6  -103  -236     0    51    27   -24  -101  -228
      4        71   -61  -235     0    51    36   -12   -99  -236     0
      5       -93  -222     0    51    23   -22   -96  -226     0    51
      6      -316     0    51    38   -21  -104  -230     0    51    27
      7         0    51    29   -10   -95  -230     0    51    26   -22
      8        51    24   -24   -99  -226     0    51    24   -24   -99
      9       102    12  -103  -239     0    51    27   -24   -99  -225
     10        66   -63  -229     0    51    27   -22   -99  -225     0
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 4 - Now filter with nonsymmetric weights
filter filter.A filter.V nlw=5 nsw=5 'nons  +
   weights=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.V size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   0 180 250 194  80  34  27 180 250
      2       0  71 179 230 174  70   4 108 179 230
      3     180 158 207 161 115  49  86 147 207 161
      4     240 219 161 115  49  86 147 207 161 115
      5     189 182 115  49  86 147 207 161 115  49
      6      85  83  49  86 147 207 161 115  49  86
      7      39  17  86 147 207 161 115  49  86 147
      8      32 107 147 207 161 115  49  86 147 207
      9     180 158 207 161 115  49  86 147 207 161
     10     240 219 161 115  49  86 147 207 161 115
difpic (filter.A,filter.V) filter.dif3
Beginning VICAR task difpic
DIFPIC version 06Oct11
 AVE VAL OF POS DIFFS=  31.8498
 NUMBER OF POS DIFF=2649
 AVE VAL OF NEG DIFFS= -58.5336
 NUMBER OF NEG DIFFS=1445
 TOTAL NUMBER OF DIFFERENT PIXELS=4094
 AVE VAL OF DIFFS=-0.515137E-01
 % DIFF PIXELS=  99.9512
list filter.dif3 size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:DIFPIC    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0  51   0   0  10 175   0  24   0   0
      2      51  31   0   0  81   0  47   0   0   0
      3       0   0   0  94   0   2  16   6   0  94
      4       0   0  94   0   2  16   6   0  94   0
      5      15  73   0   2  16   6   0  94   0   2
      6     170   0   2  16   6   0  94   0   2  16
      7       0  34  16   6   0  94   0   2  16   6
      8      19   0   6   0  94   0   2  16   6   0
      9       0   0   0  94   0   2  16   6   0  94
     10       0   0  94   0   2  16   6   0  94   0
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 5 - Now do same with a scale factor to multiply values by 2
filter filter.A filter.U nlw=5 nsw=5 'nons scale=(0,2)  +
   weights=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.U size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   0 255 255 255 160  68  53 255 255
      2       0 142 255 255 255 139   8 217 255 255
      3     255 255 255 255 230  99 172 255 255 255
      4     255 255 255 230  99 172 255 255 255 230
      5     255 255 230  99 172 255 255 255 230  99
      6     170 165  99 172 255 255 255 230  99 172
      7      78  34 172 255 255 255 230  99 172 255
      8      63 214 255 255 255 230  99 172 255 255
      9     255 255 255 255 230  99 172 255 255 255
     10     255 255 255 230  99 172 255 255 255 230
difpic (filter.A,filter.U) filter.dif4
Beginning VICAR task difpic
DIFPIC version 06Oct11
 AVE VAL OF POS DIFFS=  55.5652
 NUMBER OF POS DIFF=  92
 AVE VAL OF NEG DIFFS= -99.9327
 NUMBER OF NEG DIFFS=3341
 TOTAL NUMBER OF DIFFERENT PIXELS=3433
 AVE VAL OF DIFFS= -80.2644
 % DIFF PIXELS=  83.8135
list filter.dif4 size=(1,1,10,10) 'zer
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:DIFPIC    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0  51   0   0   0  95   0   0   0   0
      2      51   0   0   0   0   0  43   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0
      6      85   0   0   0   0   0   0   0   0   0
      7       0  17   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
if (mode = "nobatch" or mode = "inter")
end-if
let $echo="no"
TEST 6 - Now filter with asymmetric weights
filter filter.A filter.R nlw=3 nsw=3 'asym  +
	weights=(-20,50,20       +
	           5,100,0       +
	         -10,20,-10)
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.R size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1      12  79 130 181 212 125  38  79 130 181
      2      30  97 148 219 230  64   7  97 148 219
      3      81 148 219 230  64   7  97 148 219 230
      4     132 219 230  64   7  97 148 219 230  64
      5     222 230  64   7  97 148 219 230  64   7
      6     224  64   7  97 148 219 230  64   7  97
      7      77   7  97 148 219 230  64   7  97 148
      8      30  97 148 219 230  64   7  97 148 219
      9      81 148 219 230  64   7  97 148 219 230
     10     132 219 230  64   7  97 148 219 230  64
difpic (filter.A,filter.R) filter.dif5
Beginning VICAR task difpic
DIFPIC version 06Oct11
 AVE VAL OF POS DIFFS=  20.0923
 NUMBER OF POS DIFF=2720
 AVE VAL OF NEG DIFFS= -39.8408
 NUMBER OF NEG DIFFS=1376
 TOTAL NUMBER OF DIFFERENT PIXELS=4096
 AVE VAL OF DIFFS=-0.415039E-01
 % DIFF PIXELS=  100.000
list filter.dif5 size=(1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:DIFPIC    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0 130   0   0   0   0
      2      21   5   5   0  25   0  44   5   5   0
      3      21   5   0  25   0  44   5   5   0  25
      4      21   0  25   0  44   5   5   0  25   0
      5       0  25   0  44   5   5   0  25   0  44
      6      31   0  44   5   5   0  25   0  44   5
      7       0  44   5   5   0  25   0  44   5   5
      8      21   5   5   0  25   0  44   5   5   0
      9      21   5   0  25   0  44   5   5   0  25
     10      21   0  25   0  44   5   5   0  25   0
if (mode = "nobatch" or mode = "inter")
end-if
gen filter.3X nl=100 ns=100 nb=3 ival=10 sinc=15 linc=20 binc=30
Beginning VICAR task gen
GEN Version 6
GEN task completed
list filter.3X size=(41,41,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     1
 ***********
     Samp    41      43      45      47      49
   Line
     41     130 145 160 175 190 205 220 235 250   9
     42     150 165 180 195 210 225 240 255  14  29
     43     170 185 200 215 230 245   4  19  34  49
     44     190 205 220 235 250   9  24  39  54  69
     45     210 225 240 255  14  29  44  59  74  89
     46     230 245   4  19  34  49  64  79  94 109
     47     250   9  24  39  54  69  84  99 114 129
     48      14  29  44  59  74  89 104 119 134 149
     49      34  49  64  79  94 109 124 139 154 169
     50      54  69  84  99 114 129 144 159 174 189


 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     2
 ***********
     Samp    41      43      45      47      49
   Line
     41     160 175 190 205 220 235 250   9  24  39
     42     180 195 210 225 240 255  14  29  44  59
     43     200 215 230 245   4  19  34  49  64  79
     44     220 235 250   9  24  39  54  69  84  99
     45     240 255  14  29  44  59  74  89 104 119
     46       4  19  34  49  64  79  94 109 124 139
     47      24  39  54  69  84  99 114 129 144 159
     48      44  59  74  89 104 119 134 149 164 179
     49      64  79  94 109 124 139 154 169 184 199
     50      84  99 114 129 144 159 174 189 204 219


 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     3
 ***********
     Samp    41      43      45      47      49
   Line
     41     190 205 220 235 250   9  24  39  54  69
     42     210 225 240 255  14  29  44  59  74  89
     43     230 245   4  19  34  49  64  79  94 109
     44     250   9  24  39  54  69  84  99 114 129
     45      14  29  44  59  74  89 104 119 134 149
     46      34  49  64  79  94 109 124 139 154 169
     47      54  69  84  99 114 129 144 159 174 189
     48      74  89 104 119 134 149 164 179 194 209
     49      94 109 124 139 154 169 184 199 214 229
     50     114 129 144 159 174 189 204 219 234 249
let $echo="no"
TEST 7 - Test a 3D image
filter filter.3X filter.3Y
Beginning VICAR task filter
** Filter 13-Aug-2010 (64-bit) - RJB
list filter.3Y size=(1,1,10,10) sb=1 nb=3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1       0  11  30  40  49  58  69  88 115 135
      2      23  56  71  80  91 104 118 134 153 165
      3      44  76  88  97 113 133 150 160 164 164
      4      53  86 101 114 134 153 164 168 174 189
      5      61  97 120 138 154 162 163 175 212 255
      6      82 118 141 155 164 166 173 212 255 255
      7     115 145 155 160 171 191 232 255 255   0
      8     140 164 162 167 204 255 255 255   0   0
      9     140 166 177 210 255 255   0   0   0  51
     10     141 183 233 255 255   0   0  30  70  85


 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1      12  42  53  58  68  88 115 135 139 133
      2      51  83  96 105 117 135 154 165 169 177
      3      62  97 118 135 150 160 165 164 175 218
      4      72 109 136 153 164 169 175 189 228 255
      5      96 132 151 158 162 176 213 255 255 255
      6     128 157 163 159 169 212 255 255   0   0
      7     141 170 178 188 226 255 255   0   0  32
      8     129 171 217 255 255 255   0   0  51  89
      9     136 201 255 255   0   0   0  51  87  94
     10     222 255 255   0   0  30  70  85  90  94


 Task:GEN       User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 Task:FILTER    User:wlb       Date_Time:Mon Feb  9 15:05:33 2015
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1      28  60  75  89 114 135 140 133 132 157
      2      68 103 121 136 153 165 169 177 204 255
      3      91 127 148 159 164 166 176 218 255 255
      4     119 149 160 164 172 189 229 255 255   0
      5     143 166 165 171 207 255 255 255   0   0
      6     142 168 179 213 255 255   0   0   0  49
      7     142 184 234 255 255   0   0  32  70  84
      8     204 255 255 255   0   0  50  89  93  90
      9     255 255   0   0   0  54  87  94  92 101
     10     255   0   0  28  74  89  91  94 105 129
let $echo="no"
### endit> | SFI = 1   SKEY =  | oldSFI = 0 oldSKEY = 
$ Return
$!#############################################################################
