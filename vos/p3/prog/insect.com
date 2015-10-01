$!****************************************************************************
$!
$! Build proc for MIPL module insect
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:37
$!
$! Execute by entering:		$ @insect
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
$ write sys$output "*** module insect ***"
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
$   if F$SEARCH("insect.imake") .nes. ""
$   then
$      vimake insect
$      purge insect.bld
$   else
$      if F$SEARCH("insect.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake insect
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @insect.bld "STD"
$   else
$      @insect.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create insect.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack insect.com -
	-s insect.f -
	-p insect.pdf -
	-i insect.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create insect.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    01 MAY 1991 ...REA... Conversion to UNIX/VICAR
C    29 JUL 1985 ...JHR... allow half,full,real data types
C    02 NOV 1984 ...BXG... converted to VICAR2
C    29 FEB 1984 ...SJR... converted for use on the VAX 11/780 
C    27 JUN 1975 ...DAH... CHANGES FOR CONVERSION TO 360/OS
C     INSERT SECTOR FROM SECONDARY PICTURE INTO PRIMARY PICTURE
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
      INTEGER SL,SS,BYTPIX
      INTEGER IPARM(6),OUNIT,SL2,SS2,STAT
      LOGICAL*1 BUF(20000)
      CHARACTER*8 FORM1,FORM2
C
      CALL XVPARM('INSECT',IPARM,ICOUNT,IDEF,0)
C
C        OPEN INPUT DATA SETS
      CALL XVUNIT(IUNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT1,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT2,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C        CHECK THAT BOTH FORMATS ARE THE SAME
      CALL XVGET(IUNIT1,STAT,'NL',NLI1,'NS',NSI1,'FORMAT',FORM1,' ')
      CALL XVGET(IUNIT2,STAT,'NL',NLI2,'NS',NSI2,'FORMAT',FORM2,' ')
      IF(FORM1.NE.FORM2) THEN
	 CALL XVMESSAGE(' INPUTS MUST BE IN THE SAME FORMAT',' ')
	 CALL ABEND
      ENDIF
      BYTPIX=0
      IF(FORM1.EQ.'BYTE') BYTPIX=1
      IF(FORM1.EQ.'HALF') BYTPIX=2
      IF(FORM1.EQ.'FULL') BYTPIX=4
      IF(FORM1.EQ.'REAL') BYTPIX=4
      IF(BYTPIX.EQ.0) THEN
	 CALL XVMESSAGE(' INVALID DATA FORMAT',' ')
	 CALL ABEND
      ENDIF
C
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF(NS.GT.20000) THEN
	 CALL XVMESSAGE(' IMAGE SIZE EXCEEDS BUFFER SIZE',' ')
	 CALL ABEND
      ENDIF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		  'OPEN_ACT','SA','IO_ACT','SA',' ')
C
      SL2=IPARM(1)
      SS2=IPARM(2)
      NL2=IPARM(3)
      NS2=IPARM(4)
      LN2=IPARM(5)
      SN2=IPARM(6)
      NL1=NLI-SL+1
      NS1=NSI-SS+1
      IL1=SL
      IL2=SL2
      BN2=BYTPIX*(SN2-1)+1
      NB=BYTPIX*NS
C
      DO L=1,NL
         CALL ITLA(0,BUF,NB)
         IF(L.LE.NL1) THEN
            CALL XVREAD(IUNIT1,BUF,STAT,'LINE',IL1,'SAMP',SS,
     &                 'NSAMPS',NS1,' ')
            IL1=IL1+1
         END IF
         IF(L.GE.LN2.AND.L.LT.LN2+NL2) THEN
            CALL XVREAD(IUNIT2,BUF(BN2),STAT,'LINE',IL2,
     &                 'SAMP',SS2,'NSAMPS',NS2,' ')
            IL2=IL2+1
         END IF
         CALL XVWRIT(OUNIT,BUF,STAT,'NSAMPS',NS,' ')
      ENDDO

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT1,STAT,' ')
      CALL XVCLOSE(IUNIT2,STAT,' ') 
      CALL XVCLOSE(OUNIT,STAT,' ')

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create insect.pdf
process help=*
PARM INP 	TYPE=(STRING,60)	COUNT=2
PARM OUT	TYPE=(STRING,60)	COUNT=1
PARM SIZE       TYPE=INTEGER	COUNT=0:4	DEFAULT=--
PARM SL		TYPE=INTEGER			DEFAULT=1
PARM SS		TYPE=INTEGER			DEFAULT=1
PARM NL		TYPE=INTEGER			DEFAULT=0
PARM NS		TYPE=INTEGER			DEFAULT=0
PARM INSECT	TYPE=INTEGER	COUNT=6	
END-PROC

 
.TITLE
	Vicar Program INSECT
.HELP
PURPOSE:

INSECT is a VICAR applications program which combines two pictures
of unequal size into one composite picture.  INSECT may be used to
mosaic two pictures or to replace a portion of one picture with a 
portion of another. 

INSECT creates an output picture with its size given by the NL and 
NS.   The section of the primary input given by the SL and SS 
parameters is placed in the top left-hand section of the output
picture.  The desired portion of the second input is placed in the 
location given by the parameter INSECT.
If an output pixel corresponds to pixels from both inputs, it is 
assigned the DN of the pixel from the secondary input.  All pixels 
of the output not corresponding to any pixel in either input will 
be assigned the DN of 0.  The maximum length of an output line 
is 20000 bytes.

.PAGE
EXECUTION:


INSECT INP=(A.DAT,B.DAT) OUT=C.DAT SL=1 SS=1 NL=40 NS=500 +
 insect=(50,150,150,150,250,350)

If A.DAT is a 300 line by 400 sample picture and B.dat is a 200 line
by 400 sample picture, then, the above statement will mosaic the two 
pictures replacing the last 150 lines and 50 samples of A.DAT by the
last 150 lines and middle 150 samples of B.DAT. (see following example)


.PAGE


		400                           150     300
	____________________  		____________________
       |                    |          |                    |
       |                    |          |      _______       | 50
       |                    |    200   |     |       |      |
       |    A.DAT           |          |     | B.DAT |      |
 300   |                    |          |     |       |      |
       |                    |          |     |       |      |
       |                    |           --------------------
       |                ----|250		400
       |                |   |
        --------------------
		     350
		For output picture C.DAT see the next page.
.PAGE


				   500
			 _________________________
		     	|                  |	  |
			|		   |	  |
			|   A.DAT	   |  0   |
		    400	|		   |	  |
			|		   |	  |
			|		  --------|
			|________________|        |
			|		 | B.DAT  |
			|	 0 	 |	  |
			 -------------------------
			           C.DAT
.page
WRITTEN BY:  H. J. Frieden,  30 Aug. 1968

CONVERTED TO VAX BY:  S. J. Rueff,  1 March 1984

CONVERTED TO VICAR2 BY:  B. Gokhman,  19 Nov. 1984

CURRENT COGNIZANT PROGRAMMER:  F. F. Moss
.LEVEL1
.VARIABLE INP
Two input Image data sets.
.VARIABLE OUT
The output image data set.
.VARIABLE SIZE
The Vicar Size Field.
.VARIABLE SL
Starting Line of Primary input
.VARIABLE SS
Starting Sample of Primary input
.VARIABLE NL
Number of Lines output 
.VARIABLE NS
Number of Samples output 
.VARIABLE INSECT
1. Starting Line of second input
2. Starting Sample of second 
   input
3. Number of Lines of second
   input to be copied
4. Number of Samples of second
   input to be copied
5. Output line for Line 1 of 
   second input
6. Output sample for Line 1 of 
   second output
.LEVEL2
.VARIABLE INP
The 2 input image datasets to be inserted into the output file.
The first file specified will be the Primary Input Picture.
The second file specified is the Secondary Input Picture. The Primary 
input picture is located beginning at the upper left of the output
picture, while the Secondary file will be placed according to the 
parameters specified.
.VARIABLE OUT
The output image data set.
.VARIABLE SIZE
The Vicar size field. The first two integers refere to the Starting 
Line number of the first line and the Starting Sample number of the 
first sample  to be transfered to the output data set from the Primary 
input data set.  The second two integers specify the dimensions of the 
output data set in Number of Lines  and Number of Samples.
.VARIABLE SL
The Starting Line number of the Primary input image location which will
be mapped to location (1,1) in the output picture.
.VARIABLE SS
The Starting Sample number of the Primary input image location which
will be mapped to location (1,1) in the output picture.
.VARIABLE NL
The number of lines in the output picture. NL determines the vertical 
size of the output picture.
.VARIABLE NS
The number of samples in the output picture.  NS determines the 
horizontal size of the output picture.
.VARIABLE INSECT
A six value parameter:
value 1: The Starting Line in the Section of the Secondary input picture
         to be inserted into the output picture.
value 2: The Starting Sample in the Section of the Secondary input picture
         to be inserted into the output picture.
value 3: The Number of Lines in the Section of the Secondary input picture
	 to be inserted into the output picture.
value 4: The Number of Samples in the Section of the Secondary input picture
	 to be inserted into the output picture.
value 5: The Starting Line in the Output Picture which is to correspond to the
 	 upper left of the secondary section to be inserted.
value 6: The Starting Sample of the Output Picture which is to correspond to 
	 the upper left of the secondary section to be inserted.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create insect.imake
#define  PROGRAM   insect

#define MODULE_LIST insect.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
