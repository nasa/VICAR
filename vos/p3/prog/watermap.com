$!****************************************************************************
$!
$! Build proc for MIPL module watermap
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:48
$!
$! Execute by entering:		$ @watermap
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
$ write sys$output "*** module watermap ***"
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
$   if F$SEARCH("watermap.imake") .nes. ""
$   then
$      vimake watermap
$      purge watermap.bld
$   else
$      if F$SEARCH("watermap.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake watermap
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @watermap.bld "STD"
$   else
$      @watermap.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create watermap.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack watermap.com -
	-s watermap.f -
	-p watermap.pdf -
	-i watermap.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create watermap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL WATER(500),CIBR(500),BUFOUT(10000),VAL(32767)
	INTEGER IBUF(10000)
	CHARACTER*80 PRT
	CHARACTER*40 FILNAM(2)
C						open input, get size and org
	CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','FULL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C			       					open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		    'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL','O_FORMAT','REAL',' ')
C							get remaining parameters
	CALL XVPARM('SCALE',SCALE,ICNT,IDEF,0)
	CALL XVPARM('INP',FILNAM,ICNT,IDEF,0)
C						    read in CIBR and H2O columns
	CALL READDS(FILNAM(2),WATER,CIBR,NVALS)
C						       compute linear regression
	N = 0
	SUMX = 0.0
	SUMY = 0.0
	SUMX2 = 0.0
	SUMXY = 0.0
	DO I=1,NVALS
	    IF (WATER(I).GT.0.0 .AND. CIBR(I).LT.1.0 .AND.
     +		CIBR(I).GT.0.0) THEN
		N = N+1
		X = ALOG(WATER(I))
		Y = ALOG(-1.0*ALOG(CIBR(I)))
		SUMX = SUMX + X
		SUMY = SUMY + Y
		SUMX2 = SUMX2 + X*X
		SUMXY = SUMXY + X*Y
		SUMY2 = SUMY2 + Y*Y
	    END IF
	END DO
	SLOPE = (SUMX*SUMY - N*SUMXY) / (SUMX*SUMX - N*SUMX2)
	OFFSET = (SUMY - SLOPE*SUMX) / N
	R2 = (SLOPE*SLOPE*(SUMX2 - (SUMX*SUMX/N))) / 
     +		(SUMY2 - (SUMY*SUMY/N))
	ALPHA = EXP(OFFSET)
	BETA = SLOPE
C							      update VICAR label
	CALL XLADD(IOUTUNIT,'HISTORY','ALPHA',ALPHA,ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUTUNIT,'HISTORY','BETA',BETA,ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUTUNIT,'HISTORY','FIT',R2,ISTAT,
     +		   'FORMAT','REAL',' ')
	WRITE (PRT,100) ALPHA,BETA,R2
  100	FORMAT(' alpha = ',F7.5,'   beta = ',F7.5,'   fit = ',F6.4)
	CALL XVMESSAGE(PRT,' ')
	IF (ALPHA .LE. 0.0) THEN
	    CALL XVMESSAGE(' Invalid alpha value',' ')
	    CALL ABEND
	ENDIF
C								       build LUT
	X = -1.0/ALPHA
	Y = 1.0/BETA
	DO I=1,32767
	    Z = SCALE*I
	    IF (Z.LT.1.0) THEN
		VAL(I) = (X*LOG(Z))**Y
	    ELSE
		VAL(I) = -1.0
	    END IF
	END DO
C							      build output image
	IEL = ISL + NL - 1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,IBUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',I,' ')
	    DO J=1,NS
		IF (IBUF(J) .GT. 0) THEN
		    BUFOUT(J) = VAL(IBUF(J))
		ELSE
		    BUFOUT(J) = 99999.0
		END IF
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUFOUT,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C**************************************************************************
      SUBROUTINE READDS(FILENAME,COL1,COL2,NRECS)
C
C      This subroutine reads all records from the file FILENAME, and puts
C      the first numeric value that it finds in the COL1 array and the
C      second numeric value in the COL2 array.  If something other than a 
C      numeric or delimiter is encountered prior to finding two numeric fields, 
C      the entire line is discarded.
C
      CHARACTER*40 FILENAME
      REAL COL1(*),COL2(*)
C
      OPEN (51,FILE=FILENAME,STATUS='OLD')
      NRECS = 1
  100 CONTINUE
          READ (51,*,END=900,ERR=100) COL1(NRECS),COL2(NRECS)
	  NRECS = NRECS+1
          GO TO 100
  900 CONTINUE
      NRECS = NRECS-1
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create watermap.pdf
process help=*
  PARM INP  	(STRING,40)		COUNT=2
  PARM OUT  	(STRING,40)
  PARM SIZE 	INTEGER DEFAULT=--	COUNT=0:4 
  PARM SL 	INTEGER DEFAULT=1
  PARM SS 	INTEGER DEFAULT=1
  PARM NL 	INTEGER DEFAULT=0
  PARM NS 	INTEGER DEFAULT=0
  PARM SCALE	REAL
end-proc
.HELP
      WATERMAP is a program to compute the atmospheric moisture content image,
starting with the CIBR image and a 2 column table of moisture contents and
CIBR values.  WATERMAP finds the values of a and b that fits the table values 
to a curve of the form
                                          b
                       CIBR = exp(-a*WATER )

Then, using the inverse of this equation, it computes the water image from
the CIBR map.
       Operationally, this is done by means of a lookup table of the possible
input values.  The data type of the input image may be byte, halfword, or 
fullword, but the values must fall within the range of 1 to 32767.  The 
parameter SCALE is used to rescale the data into the units used in the fitted 
table.
       The rescaled CIBR values should all fall within the range 0 to 1.  If
a value less than 0 is encountered, it is marked in the output with a value of 
99999.0.  Input values that are greater than 1 are set to -1 in the output.
.LEVEL1
.VARIABLE INP
Input CIBR image, 
Water-CIBR table
.VARIABLE OUT
Output watermap image
(REAL)
.VARIABLE SIZE
Window into input
.VARIABLE SL
Starting line
= size(1)
.VARIABLE SS
Starting sample
= size(2)
.VARIABLE NL
Number of lines
= size(3)
.VARIABLE NS
Number of samples
= size(4)
.VARIABLE SCALE
Input rescaling factor
.LEVEL2
.VARIABLE INP
     Two inputs are required.  The first input is a VICAR labelled image of the 
CIBR.  It may be byte, halfword, or integer, but must not contain negative 
values or values greater than 32767.
     The second input is a table (ASCII file, not a VICAR labelled file) of two
columns.  The first column should contain the values of water content, while the
second column should have the corresponding CIBR values.  Entries that have CIBR
values outside the range 0 to 1, or water values less than or equal to 0, are
discarded.
.VARIABLE OUT
The output dataset is an image of water content.  It has a data type of real,
regardless of the input data type.  The image will have the same units as the
water column in the input table.
.VARIABLE SIZE
The size parameter determines the boundaries in the input
file from which the computation is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be procesed
	NS is the number of samples (pixels) on each line
.VARIABLE SCALE
This value is multiplied to all pixels prior to applying the CIBR
transformation.  It must be specified to get the units of the CIBR image
to match the units used to determine the fit parameters.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create watermap.imake
#define  PROGRAM   watermap

#define MODULE_LIST watermap.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
