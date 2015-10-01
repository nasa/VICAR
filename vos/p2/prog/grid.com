$!****************************************************************************
$!
$! Build proc for MIPL module grid
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:21:40
$!
$! Execute by entering:		$ @grid
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
$ write sys$output "*** module grid ***"
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
$ write sys$output "Invalid argument given to grid.com file -- ", primary
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
$   if F$SEARCH("grid.imake") .nes. ""
$   then
$      vimake grid
$      purge grid.bld
$   else
$      if F$SEARCH("grid.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake grid
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @grid.bld "STD"
$   else
$      @grid.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create grid.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack grid.com -mixed -
	-s grid.f -
	-i grid.imake -
	-p grid.pdf -
	-t tstgrid.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create grid.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
C
c     10 Jan 2013 -lwk- fixed continued error msgs for new compiler flag on Solaris
C     26 FEB 01     BUG FIX                     BY REA
C     13 JUL 94     FIX: INITIALIZE TOP         BY CRS (CRI)
C     25 AUG 86     FIX HELP FILE (FR 16800)    BY FFM
C     16 AUG 85     MODIFY HELP AND BUFFER SIZE
C     08 MAR 85     CONVERSION TO VICAR2   	BY MXM
C     13 JUN 72     REVISED			BY A.A. SCHWARTZ
C     27 MAR 69     INITIAL RELEASE	 	BY E.F. DOBIES
C

      COMMON  /C1/  INUNIT,OUTUNIT,SL,SS,NL,NS,
     &		    IMGNL,IMGNS,REP,MUL,ADD,LIM,MODE,
     &		    BLI,LFS,SFS,LFL,SFL,
     &              TOP,RMUL
      INTEGER*4     INUNIT,OUTUNIT,STATUS
      INTEGER*4     SL,SS,NL,NS,IMGNL,IMGNS,PIX_SIZ
      INTEGER*4     REP,MUL,ADD,LIM,MODE,TOP
      INTEGER*4     BLI,LFS,SFS,LFL,SFL
      REAL*4        RMUL
C
C     DISPLAY PROGRAM NAME
C
      CALL IFMESSAGE('GRID version 10-Jan-2013')
 
C
C     OPEN INPUT FILE
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT','SA',' ')
      CALL XVGET(INUNIT,STATUS,'PIX_SIZE',PIX_SIZ,' ')
      IF (PIX_SIZ.NE.1) THEN
	 CALL XVMESSAGE('Image must be in byte format.',' ')
	 CALL ABEND
      ENDIF

      CALL PARAMS

C
C     OPEN OUTPUT FILE
C
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OPEN_ACT','SA','OP','WRITE',
     $		  'U_FORMAT','HALF','U_NL',NL,'U_NS',NS,' ')
      CALL GRID
C
C     CLOSE FILES
C
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END



      SUBROUTINE  PARAMS
      IMPLICIT NONE
      COMMON  /C1/  INUNIT,OUTUNIT,SL,SS,NL,NS,
     &		    IMGNL,IMGNS,REP,MUL,ADD,LIM,MODE,
     &		    BLI,LFS,SFS,LFL,SFL,
     &              TOP,RMUL
      INTEGER*4     INUNIT,OUTUNIT
      INTEGER*4     SL,SS,NL,NS,IMGNL,IMGNS
      INTEGER*4     ADD,REP,MUL,LIM,MODE
      INTEGER*4     BLI,LFS,SFS,LFL,SFL
      INTEGER*4     INT(4),GEO(5)
      INTEGER*4     ICOUNT,IDEF,MULDEF
      INTEGER*4     NLMAX,NSMAX,TOP
      REAL*4        RMUL
C
C     DEFINE SIZE PARAMETERS
C
      CALL XVSIZE(SL,SS,NL,NS,IMGNL,IMGNS)
      IF (NS.GT.20000) THEN
   	 CALL XVMESSAGE('MAXIMUM NUMBER OF SAMPLES IS 20000.',' ')
	 CALL ABEND
      ENDIF
C
C     CHECK RANGES FOR LINES AND SAMPLES
C
      IF (SL.LE.IMGNL)  GO TO 10
         CALL XVMESSAGE('SL greater than image NL',' ') 
         CALL ABEND
   10 IF (SS.LE.IMGNS)  GO TO 20
         CALL XVMESSAGE('SS greater than image NS',' ')
         CALL ABEND
   20 NLMAX=SL+NL-1
      IF (NLMAX.LE.IMGNL) GO TO 30
         CALL XVMESSAGE('NL too large',' ')
         CALL ABEND
   30 NSMAX=SS+NS-1
      IF (NSMAX.LE.IMGNS) GO TO 40
         CALL XVMESSAGE('NS too large',' ')
         CALL ABEND
C
C     DEFINE INTENSITY PARAMETERS
C
   40 CALL XVPARM('INT',INT,ICOUNT,IDEF,4)
      REP = INT(1)
      MUL = INT(2)
      ADD = INT(3)
      LIM = INT(4)
      CALL XVPARM('REP',INT(1),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) REP=INT(1)
      CALL XVPARM('MUL',INT(2),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) MUL=INT(2)
      CALL XVPARM('ADD',INT(3),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) ADD=INT(3)
      CALL XVPARM('LIM',INT(4),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) LIM=INT(4)
C
C     PARAMETER CONSISTENCY CHECK
C
      MULDEF=0
      IF (REP.NE.0) MULDEF=MULDEF+1
      IF (MUL.NE.0) MULDEF=MULDEF+1
      IF (ADD.NE.0) MULDEF=MULDEF+1
      IF (MULDEF.LE.1) GO TO 50
         CALL XVMESSAGE('REPLACE, MULTIPLY, and ADD may not be used together.',' ')
         CALL ABEND
   50 IF (MULDEF.EQ.0) THEN
 	 MODE=1
	 REP=255
      ENDIF
      IF (REP.NE.0) THEN
         MODE=1
         IF ((REP.LT.1).OR.(REP.GT.255)) THEN
	    CALL XVMESSAGE('REPLACE value must be a legal DN value.',' ')
	    CALL ABEND
	 ENDIF
      ELSE IF (MUL.NE.0) THEN
         MODE=2
      ELSE IF (ADD.NE.0) THEN
	 MODE=3
      ENDIF
      IF (MODE.EQ.2) GO TO 60
         IF (LIM.EQ.0) GO TO 60
            CALL XVMESSAGE('LIMIT may be used only with MULTIPLY option. ',' ')
  	    CALL ABEND
   60 IF ((LIM.EQ.0).AND.(MODE.EQ.2)) LIM=64
C
C     DEFINE GEOMETRIC PARAMETERS
C
      CALL XVPARM('GEO',GEO,ICOUNT,IDEF,5)
      BLI = GEO(1)
      LFS = GEO(2)
      SFS = GEO(3)
      LFL = GEO(4)
      SFL = GEO(5)
      CALL XVPARM('BLI',GEO(1),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) BLI = GEO(1)
      CALL XVPARM('LFS',GEO(2),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) LFS = GEO(2)
      CALL XVPARM('SFS',GEO(3),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) SFS = GEO(3)
      CALL XVPARM('LFL',GEO(4),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) LFL = GEO(4)
      CALL XVPARM('SFL',GEO(5),ICOUNT,IDEF,1)
      IF (IDEF.EQ.0) SFL = GEO(5)
      IF (BLI.EQ.0)  BLI=100
      IF (BLI.LT.NL) GO TO 70
         CALL XVMESSAGE('BLI must be less than NL.',' ')
         CALL ABEND
   70 IF (LFS.EQ.0)  LFS=BLI
      IF (SFS.EQ.0)  SFS=LFS
      RETURN
      END



      SUBROUTINE  GRID
      IMPLICIT NONE
      COMMON  /C1/  INUNIT,OUTUNIT,SL,SS,NL,NS,
     &		    IMGNL,IMGNS,REP,MUL,ADD,LIM,MODE,
     &		    BLI,LFS,SFS,LFL,SFL,
     &              TOP,RMUL
      INTEGER*2     INBUF(20000),OUTBUF(20000)
      INTEGER*4     INUNIT,OUTUNIT,STATUS
      INTEGER*4     SL,SS,NL,NS,IMGNL,IMGNS
      INTEGER*4     REP,MUL,ADD,LIM,MODE
      INTEGER*4     BLI,LFS,SFS,LFL,SFL
      INTEGER*4     I,J,K,L,M,N1,N2,N3,N4,N5
      INTEGER*4     BMOD,LMOD,SMOD,TOP
      REAL*4        RMUL

      N1=BLI-LFL
      N2=BLI-SFL
      N3=BLI-1
      N4=2*LFL
      N5=2*SFL
      RMUL=FLOAT(MUL)/100.0
      TOP=255-LIM
      DO I=1,NL
          CALL XVREAD (INUNIT,INBUF,STATUS,'LINE',I+SL-1,'SAMP',SS,
     &		       'NSAMPS',NS,' ')
	  CALL MVE(2,NS,INBUF,OUTBUF,1,1)
          BMOD=MOD(I,BLI)
          LMOD=MOD(I,LFS)
          SMOD=MOD(I,SFS)
C
C     UPDATE INTENSITY VALUE EVERY BLI LINES
C
          IF (BMOD.EQ.0) THEN
	      DO J=1,NS
		  CALL UPDATE(INBUF(J),OUTBUF(J))
	      END DO
C
C     UPDATE INTENSITY VALUE EVERY LFS LINES
C
          ELSE IF (LMOD.EQ.0) THEN
	      K=1
	      L=LFL
	      DO M=K,L
		  CALL UPDATE(INBUF(M),OUTBUF(M))
	      END DO
	      DO J=BLI,NS,BLI
		  K=J-LFL
		  L=MIN(K+N4,NS)
		  DO M=K,L
		      CALL UPDATE(INBUF(M),OUTBUF(M))
		  END DO
	      END DO
C
C     UPDATE INTENSITY VALUE EVERY SFS LINES
C
          ELSE IF (SMOD.EQ.0) THEN
	      K=1
	      L=SFL
	      DO M=K,L
		  CALL UPDATE(INBUF(M),OUTBUF(M))
	      END DO
	      DO J=BLI,NS,BLI
		  K=J-SFL
		  L=MIN(K+N5,NS)
		  DO M=K,L
		      CALL UPDATE(INBUF(M),OUTBUF(M))
		  END DO
	      END DO
C
C     UPDATE INTENSITY VALUE EVERY BLI SAMPLES
C
	  ELSE
	      DO J=BLI,NS,BLI
		  CALL UPDATE(INBUF(J),OUTBUF(J))
	      END DO
	  END IF
C
C     UPDATE INTENSITY VALUE EVERY SFS SAMPLES
C
	  IF (((BMOD.GE.N2).AND.(BMOD.LE.N3)) .OR.
     &	      ((BMOD.LE.SFL).AND.(BMOD.GE.1))) THEN
	      DO J=SFS,NS,SFS
		  CALL UPDATE(INBUF(J),OUTBUF(J))
	      END DO
	      IF (MOD(LFS,SFS).NE.0) THEN
		  DO J=LFS,NS,LFS
		      CALL UPDATE(INBUF(J),OUTBUF(J))
		  END DO
	      END IF
C
C     UPDATE INTENSITY VALUE EVERY LFS SAMPLES
C
          ELSE IF (((BMOD.GE.N1).AND.(BMOD.LT.N2)) .OR.
     &		   ((BMOD.LE.LFL).AND.(BMOD.GT.SFL))) THEN
	      DO J=LFS,NS,LFS
		  CALL UPDATE(INBUF(J),OUTBUF(J))
	      END DO
	  END IF
	  CALL XVWRIT(OUTUNIT,OUTBUF,STATUS,' ')
      END DO
      RETURN
      END


      SUBROUTINE  UPDATE(INPIX,OUTPIX)
      IMPLICIT NONE
      COMMON  /C1/  INUNIT,OUTUNIT,SL,SS,NL,NS,
     &		    IMGNL,IMGNS,REP,MUL,ADD,LIM,MODE,
     &		    BLI,LFS,SFS,LFL,SFL,
     &              TOP,RMUL
      INTEGER*2     INPIX,OUTPIX
      INTEGER*4     INUNIT,OUTUNIT
      INTEGER*4     SL,SS,NL,NS,IMGNL,IMGNS
      INTEGER*4     REP,MUL,ADD,LIM,MODE
      INTEGER*4     BLI,LFS,SFS,LFL,SFL
      INTEGER*4     TOP,IDATA
      REAL*4        RMUL

C
C     UPDATE DN VALUE ACCORDING TO THE INTENSITY PARAMETER VALUE
C
      GO TO (100,110,120), MODE
C
  100 CONTINUE
      OUTPIX=REP
      RETURN
C
  110 CONTINUE
      IDATA = NINT(INPIX*RMUL)
      OUTPIX = MIN(TOP,MAX(LIM,MOD(IDATA,255)))
      RETURN 
C
  120 CONTINUE
      OUTPIX = MOD(INPIX+ADD,255)
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create grid.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM grid

   To Create the build file give the command:

		$ vimake grid			(VMS)
   or
		% vimake grid			(Unix)


************************************************************************/


#define PROGRAM	grid
#define R2LIB

#define MODULE_LIST grid.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG /* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create grid.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM INT TYPE=INTEGER COUNT=4 DEFAULT=(0,0,0,0)
PARM REP TYPE=INTEGER DEFAULT=0
PARM MUL TYPE=INTEGER DEFAULT=0
PARM ADD TYPE=INTEGER DEFAULT=0
PARM LIM TYPE=INTEGER DEFAULT=0
PARM GEO TYPE=INTEGER COUNT=5 DEFAULT=(100,25,5,2,1)
PARM BLI TYPE=INTEGER DEFAULT=100
PARM LFS TYPE=INTEGER DEFAULT=25
PARM SFS TYPE=INTEGER DEFAULT=5
PARM LFL TYPE=INTEGER DEFAULT=2
PARM SFL TYPE=INTEGER DEFAULT=1
end-proc
.TITLE
GRID
.HELP
PURPOSE:
   GRID superimposes a grid network on a byte data set.   The grid consists
of evenly spaced horizontal and vertical lines.  Each grid line will have 
long and short tick marks (fiducial marks) somewhat like the long and short
marks on a ruler between the lines for each inch.  The details of the grid are
as follows:

   1.  The intensity of the pixels in the network is given by the 
       intensity parameters. 
   2.  Every BLI lines and samples, a one-pixel-wide line is superimposed
       over the data set.
   3.  A (long) mark of length (2 * LFL + 1) pixels is placed across each 
       grid line every LFS pixels.
   4.  A (short) mark of length (2 * SFL + 1) pixels is placed across each 
       grid line every SFS pixels.

       Remark: Intensity parameters,BLI,LFL,SFL,LFS,SFS are defined in 
               the parameter section in TUTOR mode.
.PAGE
EXECUTION:
   The followings are the possible execution statements for 'GRID'.
 
    1. GRID A B 

       where A is an input data set, B is an output data set.
       This will cause a grid network of intensity 255, aligned with
       the reference marks. The default for GEO is (100,25,5,2,1), 
       meaning:
       a)  The lines of the grid are spaced every 100 pixels.
       b)  The long tick marks are spaced every 25 pixels and are 5
           pixels in length.
       c)  The short tick marks are spaced every 5 pixels and are 3
           pixels in length.

    2. GRID A B SIZE=(1,1,100,100) INT=(0,0,100,0)
   
       This will cause the size of the output data set to be 100 lines
       by 100 samples. And all pixels in the network have resultant
       values which are modulo(255,(original value)+100).


    3. GRID A B INT=(0,200,0,100) GEO=(50,30,5,2,1)
       
       This will cause intensity value in the network specified by
       GEO parameters to be multipled by 2 and then taken modulo 255.
       Since LIMIT parameter is specified, if the resultant value is 
       less than the limit or greater than (255-limit), then it is set
       equal to limit or (255-limit) respectively.
 

    4. GRID A B REP=100 BLI=50

       This will cause a grid network of intensity 255 with the basic
       line interval to be every 50 samples.
   
.PAGE
.LEVEL1
.VARI INP
An input data set 
.VARI OUT
An output data set
.VARI SIZE
Image size
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
NUmber of samples
.VARI INT
Intensity parameters 
  (REP,MUL,ADD,LIM)
.VARI REP
Value to replace the current DN value with
.VARI MUL
Value to multiply the current DN value with 
(if MUL=100 then newDN = oldDN * 1.0)
.VARI ADD
Value to add to the current DN value
.VARI LIM
Value to set the upper and lower limit on DN value
(may be used only with MUL option)
.VARI GEO
Geometric parameters
  (BLI,LFS,SFS,LFL,SFL)
.VARI BLI
Basic line interval
.VARI LFS
Long fiducial spacing
.VARI SFS
Short fiducial spacing
.VARI LFL
Long fiducial length
.VARI SFL
Short fiducial length
.LEVEL2
.VARI INP
Standard GRID input dataset (one dataset)
Limitation: an input data set must be less than 20000 samples per line.
.VARI OUT
Standard GRID output dataset (one dataset)
Limitation: an output data set must be less than 20000 samples per line.
.VARI SIZE
A SIZE into the input file which is to be written to the output
file.  SIZE is the standard size field format,
	
	SIZE = (SL,SS,NL,NS) where

        SL is the starting line to read
        SS is the starting sample to read
   	NL is the number of lines to be written
  	NS is the number of samples in a line to be written

The default SIZE values are (1,1,0,0).
.PAGE
.VARI INT
INT [intensity parameters]

    The intensity parameters may be specified by two ways.  Each
 keyword may be followed by an integer, or all values may be specified
 as INT=(x,x,x,x), where (x,x,x,x) refer to the values for REP, MUL,
 ADD, and LIM, respectively.
    The first parameter is the replace (REP) value, the second is the multiply
(MUL) value,  the third is the add (ADD) value and the last one is the limit
(LIM) value (all defined  later).  Replace, multiply and add may not be used
together.  And  limit may be used only with multiply. 
    The keywords are as follows:
 
	1. REP --     causes 'GRID' to replace the DN value of all pixels
		      in the network with a constant DN.  If no other
		      intensity parameters are specified, all pixels in
		      the network are replaced by 255.

	2. MUL --     causes 'GRID' to multiply each pixel in the overlay
 		      by a constant which is 1/100 of the integer value.
		      The value of the pixel is then set equal to the 
		      product, modulo 256.  If the LIMIT parameter has
		      been specified, and the value is less than the of
		      LIMIT or greater than the value of (255-LIMIT),
		      it is set equal to LIMIT or (255-LIMIT) respectively.
		      If LIMIT value is not specified when MULTIPLY value
		      has been set, LIMIT is set to 64.

	3. ADD --     causes 'GRID' to add a constant value to the DN
		      of each pixel in the overlay network.  The resulting
		      value, modulo 256, is used in the output.

	4. LIM --     see explanation of MULTIPLY.
		      
.VARI REP
(See under INT.)
.VARI MUL
(See under INT.)
.VARI ADD
(See under INT.)
.VARI LIM
(See under INT.)
.PAGE
.VARI GEO
 GEO [geometric parameters]
    
    'GRID' operates under the control of geometric parameters, which
 control grid spacing and line length.  GEO parameters may be assigned
 by two ways.  Each keyword may be followed by an integer, or all value
 may be specified as GEO=(x,x,x,x,x), where (x,x,x,x,x) refer to the values
 for BLI, LFS, SFS, LFL, and SFL, respectively.    The keywords are as follows:

	1. BLI -- the basic line interval (spacing) for the grid.  The
		  default is 100.

 	2. LFS -- the long fiducial spacing (i.e.,the spacing
		  between the long fiducial marks perpendicular to the 
		  grid lines).  The default is 25.

	3. SFS -- the short fiducial spacing (i.e., the spacing
		  between the short fiducial marks perpendicular to the
		  grid lines).  The default is 5.   

  	4. LFL -- the length of the long marks.  Actual total
		  length will be (2 * LFL + 1), across the grid lines.
		  The default is 2.

	5. SFL -- the length of the short marks.  Actual
		  total length will be (2 * SFL + 1), across the grid lines.
		  The default is 1.
.VARI BLI
(See under GEO.)
.VARI LFS
(See under GEO.)
.VARI SFS
(See under GEO.)
.VARI LFL
(See under GEO.)
.VARI SFL
(See under GEO.)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgrid.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
 gen a 300 300
 grid a b int=(100,0,0,0)
 label-list b
 list b SIZE=(90,90,20,20)
end-proc
$ Return
$!#############################################################################
