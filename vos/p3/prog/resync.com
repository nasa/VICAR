$!****************************************************************************
$!
$! Build proc for MIPL module resync
$! VPACK Version 1.8, Wednesday, June 11, 2003, 17:45:43
$!
$! Execute by entering:		$ @resync
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
$ write sys$output "*** module resync ***"
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
$ write sys$output "Invalid argument given to resync.com file -- ", primary
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
$   if F$SEARCH("resync.imake") .nes. ""
$   then
$      vimake resync
$      purge resync.bld
$   else
$      if F$SEARCH("resync.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake resync
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @resync.bld "STD"
$   else
$      @resync.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create resync.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack resync.com -
	-s resync.f -
	-i resync.imake -
	-p resync.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create resync.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C	11 June 2003    ...rea...  Expand arrays to handle 20,000 lines
C
      SUBROUTINE MAIN44
      REAL BUF(20000,2)
      INTEGER JOFF(20000)
      INTEGER ISET(500)/500*0/
      CHARACTER*80 PRT
      CHARACTER*8 FORMAT
      CHARACTER*3 ORG
C						   open the first input dataset
      CALL XVUNIT(INP,'INP',1,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		      'U_FORMAT','REAL',' ')
      CALL XVGET(INP,ISTAT,'FORMAT',FORMAT,'ORG',ORG,'NB',NB,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							    get the parameters
      CALL XVPARM('SEARCH',IRANGE,ICNT,IDEF,0)
      CALL XVPARM('IGNORE',IGNORE,ICNT,IDEF,0)
      CALL XVPARM('MODULO',MODULO,ICNT,IDEF,0)
      CALL XVPCNT('INP',NFILES)
C							read in first line
      CALL XVREAD(INP,BUF(1,1),ISTAT,'LINE',ISL,'SAMP',ISS,'NSAMPS',NS,
     +            'BAND',1,' ')
C							loop through each line
      LAST = 1
      NEXT = 2
      IEL = ISL + NL - 1
      NSTRY = NS - 2*IGNORE - IRANGE
      JOFF(ISL) = 0
      JMAX = 0
      JMIN = 0
      DO ILINE=ISL+1,IEL
          CALL XVREAD(INP,BUF(1,NEXT),ISTAT,'LINE',ILINE,'SAMP',ISS,
     +                'NSAMPS',NS,'BAND',1,' ')
C						   loop through possible offsets
          IOFF = 0
          ERR = 1.0E30
          DO IOFFSET=-IRANGE,IRANGE
              IF (IOFFSET .GE. 0) THEN
                  LOCL = IGNORE
                  LOCN = LOCL + IOFFSET
              ELSE
                  LOCN = IGNORE
                  LOCL = LOCN - IOFFSET
              END IF
C						    compute correlation residual
              TEST = 0.0
              DO N=1,NSTRY
                  TEST = TEST + ABS(BUF(LOCN+N,NEXT)-BUF(LOCL+N,LAST))
              END DO
C
              IF (TEST .LT. ERR) THEN
                  IOFF = IOFFSET
                  ERR = TEST
              END IF
          END DO
	  JOFF(ILINE) = IOFF
C					if a shift is found, update ISET array
          IF (IOFF .NE. 0) THEN
	      N = MOD(ILINE,MODULO) + 1
	      ISET(N) = ISET(N) +1
          END IF
          IX = LAST
          LAST = NEXT
          NEXT = IX
      END DO
      CALL XVCLOSE(INP,ISTAT,' ')
C						if MODULO specified...
      IF (MODULO .NE. 1) THEN
C							      find seed location
	  NUM = -1
	  DO I=1,MODULO
	      IF (ISET(I) .GT. NUM) THEN
		  LOC = I
		  NUM = ISET(I)
	      END IF
	  END DO
C						 reject shifts unrelated to seed
	  DO ILINE=ISL+1,IEL
	      IF (JOFF(ILINE) .NE. 0) THEN
		  N = MOD(ILINE,MODULO) + 1
		  IF (N .NE. LOC) JOFF(ILINE)=0
	      END IF
	  END DO
      END IF
C						       compute composite offsets
      JOFF(ISL) = 0
      DO ILINE=ISL+1,IEL
	  IF (JOFF(ILINE) .NE. 0) THEN
              WRITE (PRT,200) JOFF(ILINE),ILINE
  200         FORMAT(' Image shifted by',I5,' at Line',I6)
              CALL XVMESSAGE(PRT,' ')
	  END IF
	  JOFF(ILINE) = JOFF(ILINE-1) + JOFF(ILINE)
      END DO
C					      find the min & max overall shifts
      CALL MINMAX(4,NL,JOFF(ISL),MINOFF,MAXOFF,IMIN,IMAX)
      NSO = NS - MINOFF + MAXOFF
      WRITE (PRT,400) NSO
  400 FORMAT(' *** Output Image contains',I5,' samples')
      CALL XVMESSAGE(PRT,' ')
C
      IF (FORMAT .EQ. 'BYTE') THEN
	  ICODE = 1
      ELSE IF (FORMAT .EQ. 'HALF') THEN
	  ICODE = 2
      ELSE
	  ICODE = 4
      END IF
C							resync all the data
      DO IFILE=1,NFILES
C					     open the input and output datasets
          CALL XVUNIT(INP,'INP',IFILE,ISTAT,' ')
          CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
          CALL XVUNIT(IOUT,'OUT',IFILE,ISTAT,' ')
          CALL XVOPEN(IOUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +                'U_NL',NL,'U_NS',NSO,'OP','WRITE',' ')
C
	  IF (FORMAT .EQ. 'BYTE') THEN
	      CALL FIXB(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE IF (FORMAT .EQ. 'HALF') THEN
	      CALL FIXH(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE IF (FORMAT .EQ. 'FULL') THEN
	      CALL FIXF(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  ELSE
	      CALL FIXR(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +			BUF)
	  END IF
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXB(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      BYTE BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  CALL ITLA(0,BUF,NSO)
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      CALL ITLA(0,BUF,NSO)
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXH(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      INTEGER*2 BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXF(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      INTEGER BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FIXR(INP,IOUT,ISL,IEL,ISS,NS,NSO,NB,MAXOFF,JOFF,ORG,
     +		      BUF)
C
      INTEGER JOFF(*)
      REAL BUF(*)
      CHARACTER*3 ORG
C						        read input, write output
      IF (ORG .EQ. 'BSQ') THEN
	  DO IBAND = 1,NB
	      DO ILINE=ISL,IEL
		  NLEFT = MAXOFF - JOFF(ILINE)
		  DO I=1,NLEFT
		      BUF(I) = 0.0
		  END DO
		  DO I=NLEFT+NS+1,NSO
		      BUF(I) = 0.0
		  END DO
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      ELSE
	  DO ILINE=ISL,IEL
 	      NLEFT = MAXOFF - JOFF(ILINE)
	      DO I=1,NLEFT
	          BUF(I) = 0
	      END DO
	      DO I=NLEFT+NS+1,NSO
		  BUF(I) = 0
	      END DO
	      DO IBAND = 1,NB
		  CALL XVREAD(INP,BUF(NLEFT+1),ISTAT,'LINE',ILINE,
     +                        'BAND',IBAND,'SAMP',ISS,'NSAMPS',NS,' ')
	          CALL XVWRIT(IOUT,BUF,ISTAT,'NSAMPS',NSO,' ')
	      END DO
	  END DO
      END IF
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create resync.imake
#define  PROGRAM   resync

#define MODULE_LIST resync.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create resync.pdf
process help=*
PARM INP    TYPE=(STRING,60) COUNT=(1:15)
PARM OUT    TYPE=(STRING,60) COUNT=(1:15)
PARM SIZE   TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL     TYPE=INTEGER DEFAULT=1
PARM SS     TYPE=INTEGER DEFAULT=1
PARM NL     TYPE=INTEGER DEFAULT=0
PARM NS     TYPE=INTEGER DEFAULT=0
PARM SEARCH TYPE=INTEGER DEFAULT=25
PARM IGNORE TYPE=INTEGER DEFAULT=0
PARM MODULO TYPE=INTEGER DEFAULT=1
END-PROC
.TITLE
RESYNC
.HELP
PURPOSE:
RESYNC finds and removes horizontal line-to-line misalignment within images.
This is done by finding the displacement that has the highest line-to-line
correlation, and shifting the output image to force this displacement to be
zero. Only the first channel is used to compute displacements; the shifts
computed from the first input are applied to each subsequent channel.  The
channels may be either in separate datasets, or in a single multichannel file.
Note that this algorithm produces output images somewhat larger than their 
corresponding inputs.
 
OPERATION:
RESYNC uses the first input channel only when computing the displacements to
be applied when forming the output images.  These displacements are computed
in the following manner: 
     (1) Each line (other than the first line) is compared to the previous 
         line by computing the sum of all the line-to-line pixel 
         differences.  Since image edges sometimes contain engineering or 
         fill data, which would be undesirable to include in the computation,
         an IGNORE parameter has been provided.  The 'IGNORE' number of pixels
         on both sides of the image are excluded from the computation.
     (2) The previous computation is repeated for all possible shifts, left
         and right, up to the search limit specified by the parameter 'SEARCH'.
     (3) The shift corresponding to the smallest computed value (the most
         highly correlated shift) is considered the correct shift, and is saved
         in a table.
     (4) In some cases, it is known that the resync'ing should be performed
         only after every nth line.  To accomodate this, there is a MODULO
         parameter.  The user specifies a value (say, 16 for raw TM data),
         and the program computes which of the n (16 in our example) possible 
         resync'ing set locations is most commonly shifted. The program then 
         rejects shifts that occur which are not part of the most common set
         of n (16) apart shifts.  The default is to accept all shifts.
Once the table of displacements has been completed, each of the inputs is 
corrected, in sequence.  All fill pixels are filled with the value of zero.

Note that the VICAR labels of all outputs will originate from the VICAR label
of the first input (This is the VICAR convention for label handling.), and
labels denoting channel numbers or channel unique processing will be incorrect
for all channels other than the first input.  Note also that the output images
will be wider than the input images by an unpredictable amount.  RESYNC 
reports the width of the outputs.

 
WRITTEN BY:  Ron Alley,  September20, 1994
COGNIZANT PROGRAMMER:  Ron Alley
REVISION:  1, 3 October, 1994
 
.LEVEL1
.VARIABLE INP
Input image file name(s)
.VARIABLE OUT
Output image file name(s)
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE SEARCH
Maximum possible offset
.VARIABLE IGNORE
Ignore this # of edge pixels
.VARIABLE MODULO
Resync only at intervals of
this length.
.LEVEL2
.VARIABLE INP
INP contains the name(s) of the input VICAR labelled dataset(s)
.VARIABLE OUT
OUT contains the name(s) of the dataset(s) which will contain the output 
resynch'ed VICAR image.
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE SEARCH
This parameter specifies the range of offsets to be searched for maximum
correlation. The program will test all offsets (both left and right) that are
no greater than the value of SEARCH.
.VARIABLE IGNORE
Often the edges of an image contain engineering or fill data that should not
be used to compute line to line correlation.  This parameter directs the program
to exclude this number of pixels from each side, when computing correlation
statistics.  IGNORE'd pixels are, however, resynched and included in the output
image.
.VARIABLE MODULO
Often, it is known that resync'ing need be performed only at specific intervals
within the image. The MODULO parameter allow the user to specify that interval.
The RESYNC program tests all lines for resync'ing, finds the most appropriate
starting point for the resync'ing operation, then adjusts the shift only at
those lines that are at a multiple of the specified interval.
     The default value (1) permits adjustment of the shift at any line.
.END
$ Return
$!#############################################################################
