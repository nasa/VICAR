$!****************************************************************************
$!
$! Build proc for MIPL module ds4
$! VPACK Version 1.9, Monday, March 16, 1998, 17:05:37
$!
$! Execute by entering:		$ @ds4
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
$ write sys$output "*** module ds4 ***"
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
$ write sys$output "Invalid argument given to ds4.com file -- ", primary
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
$   if F$SEARCH("ds4.imake") .nes. ""
$   then
$      vimake ds4
$      purge ds4.bld
$   else
$      if F$SEARCH("ds4.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ds4
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ds4.bld "STD"
$   else
$      @ds4.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ds4.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ds4.com -
	-s ds4.f -
	-p ds4.pdf -
	-i ds4.imake -
	-t tstds4.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ds4.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	INTEGER*2 IBUF(4000,6),TABLE(256,6)
C
	INTEGER SL,SS,NL,NS
	INTEGER PTR,PTR1,PTR2,PTR3,PTR4
	INTEGER PBUF(256,6),CDFBUF(256)
	INTEGER RSEN,IWTS(41)
	LOGICAL QAVERAGE,XVPTST
C
	COMMON IBUF,TABLE,SL,SS,NL,NS,PTR1,PTR2,PTR3,PTR4,PBUF,CDFBUF,
     &         LINE1,LINE2,LINE3,LINE4,RSEN

	CALL XVMESSAGE('DS4 version March 15, 1998',' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN,' ')

	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'OP','WRITE','U_NL',NL,'U_NS',NS,'U_FORMAT','HALF',' ')

	CALL ZIA(TABLE(1,1),768)
	CALL ZIA(PBUF(1,1),1536)
	CALL ZIA(CDFBUF(1),256)

	CALL XVPARM('RSEN',RSEN,ICNT,IDEF,' ')
	CALL XVPARM('GROUP',NSETSG,ICNT,IDEF,' ')
	QAVERAGE = XVPTST('AVERAGE')
	CALL XVPARM('FILTER',IWTS,NWTS,IDEF,' ')
	NSETS = (NL-12)/6
	NGT = NSETS-NSETSG+1

	LINE1 = SL
	LINE2 = LINE1+MOD((RSEN+3),6)
	LINE3 = LINE2
	LINE4 = LINE2

C LINE2 is the starting line of the first sensor
C set such that RSEN becomes sensor #3 within the set
C
C      ....Process the first group of sensor sets
	DO I=1,NSETSG
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE3,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE3 = LINE3+1
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)+1	!Compute histogram PBUF
		END DO
	    END DO
	END DO

C       ....Generate the CDF for RSEN
	IF (QAVERAGE) THEN
	    CDFBUF(1) = (PBUF(1,1)+PBUF(1,2)+PBUF(1,3)+PBUF(1,4)+
     &			 PBUF(1,5)+PBUF(1,6))/6
	    DO I=2,256
		CDFBUF(I) = CDFBUF(I-1)+(PBUF(I,1)+PBUF(I,2)+PBUF(I,3)+
     &			    PBUF(I,4)+PBUF(I,5)+PBUF(I,6))/6
	    END DO
	    CALL PRCDF(CDFBUF(1),NSETSG,NS,IWTS,NWTS)
	ELSE
	    CDFBUF(1)=PBUF(1,3)
	    DO I=2,256
		CDFBUF(I) = PBUF(I,3)+CDFBUF(I-1)
	    END DO
	END IF
C       ....Match cdf's and generate lookup tables
	CALL MATCH
C       ....Transform all lines up through the middle of the first group
	NLT = MOD((RSEN+3),6)+((NSETSG/2)+1)*6
	DO I=1,NLT
	    CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			'SAMP',SS,'NSAMPS',NS,' ')
	    PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
	    CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
	    CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
	    LINE1 = LINE1+1
	END DO
C						  process the rest of the groups
	NGR = NSETS-NSETSG
C
	DO I=1,NGR
C						 add in the data for the end set
C						 in the current group
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE3,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE3 = LINE3+1
	    END DO
	    DO J=1,6
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)+1
		END DO
	    END DO
C						 subtract out the data for the
C						 first set in the previous group
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,J),ISTAT,'LINE',LINE4,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		LINE4 = LINE4+1
	    END DO
	    DO J=1,6
		DO K=1,NS
		    N = IBUF(K,J)+1
		    PBUF(N,J) = PBUF(N,J)-1
		END DO
	    END DO
C						      recompute the cdf for RSEN
	    IF (QAVERAGE) THEN
		CDFBUF(1) = (PBUF(1,1)+PBUF(1,2)+PBUF(1,3)+PBUF(1,4)+
     &			     PBUF(1,5)+PBUF(1,6))/6
		DO J=2,256
		    CDFBUF(J) = CDFBUF(J-1)+(PBUF(J,1)+PBUF(J,2)+
     &			      PBUF(J,3)+PBUF(J,4)+PBUF(J,5)+PBUF(J,6))/6
		END DO
	        CALL PRCDF(CDFBUF(1),NSETSG,NS,IWTS,NWTS)
	    ELSE
		CDFBUF(1)=PBUF(1,3)
		DO J=2,256
		    CDFBUF(J) = PBUF(J,3)+CDFBUF(J-1)
		END DO
	    END IF
C					  match CDF's and generate lookup tables
	    CALL MATCH
C							transform the middle set
	    DO J=1,6
		CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			    'SAMP',SS,'NSAMPS',NS,' ')
		PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
		CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
		CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
		LINE1 = LINE1+1
	    END DO
	END DO
C						   transform the remaining lines
	NLR = SL+NL-LINE1
C
	DO I=1,NLR
	    CALL XVREAD(INUNIT,IBUF(1,1),ISTAT,'LINE',LINE1,
     +			'SAMP',SS,'NSAMPS',NS,' ')
	    PTR = MOD(((LINE1-SL+1)-RSEN+8),6)+1
	    CALL LOOKUP(IBUF(1,1),TABLE(1,PTR),NS)
	    CALL XVWRIT(IOUTUNIT,IBUF(1,1),ISTAT,'NSAMPS',NS,' ')
	    LINE1 = LINE1+1
	END DO
C
	RETURN
	END
C**********************************************************************
C
      SUBROUTINE MATCH
      INTEGER*2 IBUF(4000,6),TABLE(256,6)
      INTEGER SL,SS,NL,NS
      INTEGER PTR1,PTR2,PTR3,PTR4
      INTEGER PBUF(256,6),CDFBUF(256)
      INTEGER RSEN
      LOGICAL EXIT,INCL
      COMMON IBUF,TABLE,SL,SS,NL,NS,PTR1,PTR2,PTR3,PTR4,PBUF,CDFBUF,
     &         LINE1,LINE2,LINE3,LINE4,RSEN
C
C     SCAN CDFBUF TO FIND THE FIRST NONZERO ENTRY
C
              M=1
    5         IF(CDFBUF(M).NE.0) GO TO 10
              M=M+1
              GO TO 5
   10         CONTINUE
              MREF=M
C
              DO 200 I=1,6
C
C     scan pbuf to find the first nonzero entry
C     for the sensor being processed
              L=1
   15         IF(PBUF(L,I).NE.0) GO TO 20
              L=L+1
              GO TO 15
   20         CONTINUE
C
              L=L-1
              IF(L.EQ.0) GO TO 40
              DO 30 J=1,L
                  TABLE(J,I)=0
   30         CONTINUE
   40         CONTINUE
              L=L+1
C
C     MATCH THE CDF'S
C
              M=MREF
              ISUM=PBUF(L,I)
	      EXIT = .FALSE.
	      DO WHILE (.NOT.EXIT)
                  IF(ISUM.GE.CDFBUF(M)) THEN
		      INCL = .TRUE.
                      IF(ISUM.NE.CDFBUF(M)) THEN
	      		  M = M + 1
                          DO WHILE (ISUM.GT.CDFBUF(M))
                              M=M+1
     	  	          END DO
		          IF(ISUM.NE.CDFBUF(M)) THEN
		  	      IDH=ISUM-CDFBUF(M-1)
		              IDL=CDFBUF(M)-ISUM
		              IDD=IDH-IDL
		              IF(IDD.GE.0) TABLE(L,I)=M-1
		              IF(IDD.LT.0) TABLE(L,I)=M-2
	  	          ELSE
		              TABLE(L,I)=M-1
		  	  END IF
		      ELSE
                          TABLE(L,I)=M-1
		      END IF
                  ELSE IF (M.EQ.MREF) THEN
		      INCL = .TRUE.
		      TABLE(L,I)=M-1
                  ELSE
		      INCL = .FALSE.
	              M=M-1
                  END IF
		  IF (INCL) THEN
		      L=L+1
		      IF (L.GT.256) THEN
			  TABLE(1,I) = 0
			  EXIT = .TRUE.
		      ELSE
			  ISUM = ISUM + PBUF(L,I)
		      END IF
		  END IF
              END DO
C
  200         CONTINUE
C
              RETURN
              END
C**********************************************************************
C
      SUBROUTINE PRCDF(CDFBUF,NSETSG,NS,IWTS,NWTS)
      INTEGER CDFBUF(256),WBUF(300),IWTS(41)
      INTEGER NSETSG,NS,NWTS
      INTEGER PTR5,PTR6,SUMWTS
C
C     INSERT CDFBUF INTO WBUF STARTING AT WBUF(21)
C
              CALL ZIA(WBUF(1),300)
              CALL MVE(1,1024,CDFBUF(1),WBUF(21),1,1)
C
C     MIRROR ABOUT EACH END
C
              DO 10 I=1,20
                  J1=20+I
                  J2=21-I
                  K1=277-I
                  K2=276+I
                  WBUF(J2)=WBUF(J1)
                  WBUF(K2)=WBUF(K1)
   10         CONTINUE
C
              ISUM=0
              PTR5=21
              PTR6=PTR5-NWTS/2
              SUMWTS=0
C
C     CALCULATE SUMWTS
C
              DO 15 I=1,NWTS
                  SUMWTS=SUMWTS+IWTS(I)
   15         CONTINUE
C
C     SCALE CDFBUF PROPERLY
C
              RMAX=CDFBUF(256)
              RAMAX=NS*NSETSG
              FACTOR=RAMAX/RMAX
C
              DO 20 I=1,300
                  WBUF(I)=WBUF(I)*FACTOR+0.5
   20         CONTINUE
C
C     APPLY FILTER
C
              DO 40 I=21,276
                  DO 30 J=1,NWTS
                      ISUM=ISUM+IWTS(J)*WBUF(PTR6)
                      PTR6=PTR6+1
   30             CONTINUE
                  CDFBUF(I-20)=ISUM/SUMWTS
                  PTR5=PTR5+1
                  PTR6=PTR5-NWTS/2
                  ISUM=0
   40         CONTINUE
              IF(CDFBUF(256).NE.NS*NSETSG) CDFBUF(256)=NS*NSETSG
C
              RETURN
              END
C**********************************************************************
	      SUBROUTINE LOOKUP(IBUF,TABLE,NS)
	      INTEGER*2 TABLE(0:*),IBUF(1:*)
	      DO I=1,NS
	      	  IBUF(I) = TABLE(IBUF(I))
	      END DO
	      RETURN
	      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ds4.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM RSEN TYPE=INTEGER DEFAULT=3
PARM AVERAGE TYPE=KEYWORD VALID=AVERAGE COUNT=0:1 DEFAULT=--
PARM GROUP TYPE=INTEGER DEFAULT=3
PARM FILTER TYPE=INTEGER COUNT=1:19 DEFAULT=(0,1,0)
END-PROC
.TITLE
DS4
.HELP
PURPOSE:
DS4 attempts to remove 6 line striping from LANDSAT imagery by
performing localized histogram matching of the 6 sensors.

EXECUTION:

Examples
	DS4 IN OUT

	will remove striping using sensor 3 as the reference sensor
	(which is the default), and assuming 3 sets per group (default).
	The first line of the first set, in this case, is line number 1,
	chosen so that the reference sensor becomes the third line within
	the set.

	DS4 IN OUT RSEN=5

	will remove striping as above, with the exception that the
	reference sensor will be sensor number 5, making line number 3
	the first line of the first set.

	DS4 IN OUT 'AVERAGE

	will remove striping as in the first example above, but instead
	of using one particular sensor as a reference sensor, will
	use the average of all of the sensors as a reference.  Line 1
	is again the first line in the first set.

	DS4 IN OUT 'AVERAGE RSEN=6

	Here the AVERAGE option has been combined with the RSEN option
	to cause averaging and, at the same time, make sensor number 6
	the third line in the image.  The net effect is to ignore the
	first three lines of the first set.  Again, as above, the
	number of lines per group defaults to 3.

	DS4 IN OUT FILTER=(1,2,3,2,1) RSEN=1

	The reference sensor is sensor number 1, and there are three sets
	in a group.  Line 5, then, is the first line of the first set.
	The reference CDF is filtered with the weights 1,2,3,2,1 before
	matching is performed.

	DS4 IN OUT RSEN=2 GROUP=7 'AVERAGE  FILTER=(1,2,1)

	Averaging and filtering are performed as above, and line number 6
	is the first line of the first set.  Note, here, that there are
	7 sets per group.

SPECIAL NOTES:
1) LANDSAT frames have been received with 12 line striping superimposed
   upon the 6 line striping.  DS4 will not correct the 12 line striping.
   The 12 line striping should be removed by other means prior to the
   execution of DS4.
2) DS4 is unable to remove 6 line striping from water bodies.


OPERATION:
The input picture is considered to be subdivided into contiguous sets of
6 lines each.  The starting line of a given set is determined by the
reference sensor number such that the reference sensor becomes the third
line within the set.  For example, if the reference sensor were chosen to
be 5, then the starting line of the first set would be 3 and that of the
second set would be 9, and so on.  Note that the line numbers above
refer to the image given by the VICAR size field.  That is, if a subarea
is specified, then line 5 above refers to the fifth line of the subarea.

To initiate the matching process, the CDF's for each sensor are compiled
using the first group of sets (i.e., the first GROUP sets).  The reference
CDF is that of the specified sensor RSEN.  If the AVERAGE parameter is
specified, the reference CDF is taken to be the average of all 6 sensor
CDF's.  If the parameter FILTER is specified, then the reference CDF
is low-pass filtered using the integer weights F1, . . ., FN.  The CDF
of each sensor is matched to that of the reference CDF and corresponding
transfer tables are generated.  The first ((N2-1)/2) + 1 sets plus any
lines skipped over at the beginning (see the first paragraph) are then
transformed using the generated transfer table.

Subsequent sets are transformed in the following manner:
1) The CDF's of the sensors of the first set in the group are subtracted
   out of the group sensor CDF's.
2) The CDF's of the sensors in the first set following the group are added
   in to the group sensor CDF's.
3) The transfer tables are generated as before.
4) The middle set of the updated group is transformed.

This process continues until the last set of the image has been included
in a group.  At this point, the middle set of the group, all sets following
it, and any lines after the last set are transformed.

The parameters AVERAGE and RSEN are not mutually exclusive.  While the
AVERAGE parameter will override the RSEN parameter in terms of how the
reference CDF is generated (i.e., from one sensor or from an average of all
six sensors), the RSEN parameter can be used to define which lines
compose the first set of lines and hence all subsequent sets of lines.
This was illustrated in the examples.

.page
PROGRAM HISTORY

WRITTEN BY:  Daryl Madura, 5 January 1979
COGNIZANT PROGRAMMER:  A. S. Mazer
REVISIONS
SEP 02 83  Alan Mazer  MODIFIED FOR VAX CONVERSION
JUN XX 87  Ron Alley   CONVERTED TO VICAR2 BY RON ALLEY
MAR 15 98  GMY  Ported to Unix

.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE OUT
Output image file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE RSEN
Reference sensor number
.VARIABLE AVERAGE
Use average of all sensors
as reference (AVERAGE)
.VARIABLE GROUP
Sets per group
.VARIABLE FILTER
Species filtering weights
.LEVEL2
.VARIABLE RSEN
RSEN specifies the sensor which is to be used as a reference sensor.
Allowable values are 1, 2, 3, 4, 5, and 6.  Default is 3.
.VARIABLE AVERAGE
'AVERAGE specifies that the average of the six sensors is to be
used as the reference.  Default is that averaging is not performed. 
.VARIABLE GROUP
GROUP specifies the number of 6-line sets to be grouped together in
generating the local transfer tables.  Default is 3.  GROUP should be odd.
.VARIABLE FILTER
FILTER is a list of weights to be used in performing a low-pass filter
of the reference CDF.  The weights should be integer, and in number, less 
than 19 and odd.  The default is not to perform filtering.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create ds4.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM ds4

   To Create the build file give the command:
		$ vimake ds4			(VMS)
   or
		% vimake ds4			(Unix)
************************************************************************/
#define PROGRAM	ds4

#define MODULE_LIST ds4.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define R2LIB
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstds4.pdf
procedure		! DS4 TEST SCRIPT
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
GEN O NL=30 NS=10 
QSAR O G3010 AREA=(2,1,1,10,5,8,1,1,10,4,14,1,1,10,5,20,1,1,10,6,23,1,1,10,-5)
DS4 G3010 O RSEN=1
LIST O
!
DS4 G3010 O 'AVERAGE
LIST O
!
DS4 G3010 O GROUP=1
LIST O
!
DS4 G3010 O FILTER=(1,2,1)
LIST O
!
DS4 G3010 O FILTER=(1,2,3)
LIST O
!
DS4 G3010 O 'AVERAGE RSEN=6
LIST O
end-proc

$ Return
$!#############################################################################
