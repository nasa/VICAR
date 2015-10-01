$!****************************************************************************
$!
$! Build proc for MIPL module insect
$! VPACK Version 1.9, Wednesday, March 18, 2015, 16:43:11
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
$ write sys$output "*** module insect ***"
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
$ write sys$output "Invalid argument given to insect.com file -- ", primary
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
$ vpack insect.com -mixed -
	-s insect.f -
	-i insect.imake -
	-p insect.pdf -
	-t tstinsect.pdf tstinsect.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create insect.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    29 JUL 1985 ...JHR... allow half,full,real data types
C    02 NOV 1984 ...BXG... converted to VICAR2
C    29 FEB 1984 ...SJR... converted for use on the VAX 11/780 
C    27 JUN 1975 ...DAH... CHANGES FOR CONVERSION TO 360/OS
C    22 MAR 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C     INSERT SECTOR FROM SECONDARY PICTURE INTO PRIMARY PICTURE
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
	implicit none
      INTEGER*4 SL,SS,NL,NS,MAXCNT
      INTEGER*4 IPARM(6),IUNIT1,IUNIT2,OUNIT,SL2,SS2,BN2,STAT
      INTEGER*4 J,L,LN2,ICOUNT,IDEF,IL1,IL2,NL1,NL2,NLI,NSI
      INTEGER*4 NLI1,NLI2,NSI1,NSI2,NS1,NS2,SN2,icode
      REAL*4 BUF(60000)
      CHARACTER*8 FORM1,FORM2
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
      DATA MAXCNT/6/ 
C
      CALL IFMESSAGE('INSECT version 7 Jan 2013 (64-bit) - rjb')
      CALL XVEACTION('SA',' ')	
      CALL XVPARM('INSECT',IPARM,ICOUNT,IDEF,MAXCNT)
C
C        OPEN INPUT DATA SETS
      CALL XVUNIT(IUNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT1,STAT,' ')
      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT2,STAT,' ')
C
C        CHECK THAT BOTH FORMATS ARE THE SAME
      CALL XVGET(IUNIT1,STAT,'NL',NLI1,'NS',NSI1,'FORMAT',FORM1,' ')
      CALL XVGET(IUNIT2,STAT,'NL',NLI2,'NS',NSI2,'FORMAT',FORM2,' ')
      IF (FORM1.NE.FORM2) THEN
         CALL XVMESSAGE('??E - Inputs must be in the same format',' ')
         CALL ABEND
      END IF
        icode = 0
        if (form1.eq.'BYTE') icode=1
        if (form1.eq.'HALF'.or.form1.eq.'WORD') icode=2
        if (form1.eq.'FULL') icode=3
        if (form1.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend  
        endif
        call xvclose(iunit1,stat,' ')
	call xvclose(iunit2,stat,' ')
c
         call xvopen( iunit1, stat, 'OP', 'READ', 
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ' )

         call xvopen( iunit2, stat, 'OP', 'READ',
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ' )

c      BYTPIX=0
c      IF(FORM1.EQ.'BYTE') BYTPIX=1
c      IF(FORM1.EQ.'HALF') BYTPIX=2
c      IF(FORM1.EQ.'FULL') BYTPIX=4
c      IF(FORM1.EQ.'REAL') BYTPIX=4
c      IF(BYTPIX.EQ.0) THEN
c         CALL XVMESSAGE('??E - INVALID DATA FORMAT',' ')
c         CALL ABEND
c      END IF
C
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
C
      IF (NS.GT.60000) THEN
         CALL XVMESSAGE('??E - Number of Samples Exceeds 60,000 ',' ')
         CALL ABEND
      END IF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     & 'O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
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
      BN2=(SN2-1)+1
c      NB=BYTPIX*NS
C
      DO L=1,NL
	 do j=1,nsi1
	    buf(j)=0.0
	 enddo
c         CALL ITLA(0,BUF,NB)
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
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create insect.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM insect

   To Create the build file give the command:

		$ vimake insect			(VMS)
   or
		% vimake insect			(Unix)


************************************************************************/


#define PROGRAM	insect
#define R2LIB

#define MODULE_LIST insect.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create insect.pdf
process help=*
PARM INP 	TYPE=STRING	    COUNT=2
PARM OUT	TYPE=STRING     COUNT=1
PARM SIZE   TYPE=INTEGER	COUNT=4		DEFAULT=(1,1,0,0)
PARM SL		TYPE=INTEGER			DEFAULT=1
PARM SS		TYPE=INTEGER			DEFAULT=1
PARM NL		TYPE=INTEGER			DEFAULT=0
PARM NS		TYPE=INTEGER			DEFAULT=0
PARM INSECT	TYPE=INTEGER	COUNT=6	
END-PROC

 
.TITLE
	Vicar Program "insect"
.HELP
PURPOSE:

"insect" is a VICAR applications program which combines two pictures
of unequal size into one composite picture.  "insect" may be used to
mosaic two pictures or to replace a portion of one picture with a 
portion of another. 

"insect" creates an output picture with its size given by the NL and 
NS.   The section of the primary input given by the SL and SS 
parameters is placed in the top left-hand section of the output
picture.  The desired portion of the second input is placed in the 
location given by the parameter INSECT.

If an output pixel corresponds to pixels from both inputs, it is 
assigned the DN of the pixel from the secondary input.  All pixels 
of the output not corresponding to any pixel in either input will 
be assigned the DN of 0.  

The maximum length of an output line is 60000 bytes. 

Supports BYTE, HALF, FULL and REAL format data.
.PAGE
EXECUTION:

NOTE: There are no optional parameters for this program.  All values
must be input.


insect INP=(A.DAT,B.DAT) OUT=C.DAT SL=1 SS=1 NL=40 NS=500 +
 insect=(50,150,150,150,250,350)

If A.DAT is a 300 line by 400 sample picture and B.dat is a 200 line
by 400 sample picture, then, the above statement will mosaic the two 
pictures replacing the last 150 lines and 50 samples of A.DAT by the
last 150 lines and middle 150 samples of B.DAT. (see following example)


.PAGE


		400                           150     300
   	    ____________________            ____________________
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
                |                  |      |
                |                  |      |
                |   A.DAT          |  0   |
            400 |                  |      |
                |                  |      |
                |		          --------|
                |________________|        |
                |                | B.DAT  |
                |              0 |        |
                 -------------------------
			           C.DAT
.page
WRITTEN BY:  H. J. Frieden,  30 Aug. 1968

    1984-03-01 S. J. Rueff - Converted to VAX
    1984-11-19 B. Gokhman - Converted to VICAR2
    1994-05-02 Alan Scop (CRI) - Made portable for UNIX
    2013-01-07 RJB - Internals updated - NS increased to 60,000

CURRENT COGNIZANT PROGRAMMER:  R. J. Bambery
.LEVEL1
.VARIABLE INP
Two input Image data sets.
.VARIABLE OUT
The output image data set.
.VARIABLE SIZE
The Vicar Size Field.
.VARIABLE SL
INTEGER - Starting Line of the Primary input
.VARIABLE SS
INTEGER - Starting Sample of the Primary input
.VARIABLE NL
INTEGER _ Number of Lines in output data set
.VARIABLE NS
INTEGER - Number of Samples in output data set 
.VARIABLE INSECT
INTEGER - Six values defining insertion of segment.
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
$Test_File:
$ create tstinsect.pdf
procedure
! Jan 7, 2013 - RJB
! TEST SCRIPT FOR PSF
! tests images of BYTE, HALF, FULL and REAL formats
!
! Vicar Programs:
!      gen, list
! 
! External programs
!       <none>
!
! parameters:
!       <none>
!
! Requires No external test data: 
! 
!
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="stop"
!
write "Test BYTE images"
let $echo="yes"
gen A NL= 10 NS=10 linc=0 sinc=0 ival=1
gen B NL=5 NS=5 linc=0 sinc=0 ival=128
let $echo="no"
write "PUT B INTO A STARTING AT (5,5)"
let $echo="yes"
insect (A,B) C insect=(1,1,5,5,5,5)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C 'zero
let $echo="no"
write "PUT B INTO A STARTING AT (15,15)"
let $echo="yes"
insect (A,B) C sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C 'zero
let $echo="no"
write " "
write "TEST HALF IMAGES"
write " "
gen A1 NL=10 NS=10 linc=0 sinc=0 ival=10 'HALF
gen B1 NL=5 NS=5 linc=0 sinc=0 ival=5000 'HALF
let $echo="no"
write "PUT B1 INTO A1 STARTING AT (5,5)"
let $echo="yes"
insect (A1,B1) C1 insect=(1,1,5,5,5,5)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C1 'zero
let $echo="no"
write "PUT B1 INTO A1 STARTING AT (15,15)"
let $echo="yes"
insect (A1,B1) C1 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C1 'zero
let $echo="no"
write " "
write "TEST FULL IMAGES"
write " "
gen A2 NL=10 NS=10 linc=0 sinc=0 ival=1000 'FULL
gen B2 NL=5 NS=5 linc=0 sinc=0 ival=15000 'FULL
let $echo="no"
write "PUT B2 INTO A2 STARTING AT (5,5)"
let $echo="yes"
insect (A2,B2) C2 insect=(1,1,5,5,5,5)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C2 'zero
let $echo="no"
write "PUT B2 INTO A2 STARTING AT (15,15)"
let $echo="yes"
insect (A2,B2) C2 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C2 'zero
let $echo="no"
write " "
write "TEST REAL IMAGES"
write " "
gen A3 NL=10 NS=10 linc=0 sinc=0 ival=1012.6 'REAL
gen B3 NL=5 NS=5 linc=0 sinc=0 ival=25123.36 'REAL
let $echo="no"
write "PUT B3 INTO A3 STARTING AT (5,5)"
let $echo="yes"
insect (A3,B3) C3 insect=(1,1,5,5,5,5)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C3 'zero
let $echo="no"
write "PUT B2 INTO A2 STARTING AT (15,15)"
let $echo="yes"
insect (A3,B3) C3 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
let $echo="no"
write "LIST RESULTING IMAGE"
let $echo="yes"
list C3 'zero
let $echo="no"






end-proc
$!-----------------------------------------------------------------------------
$ create tstinsect.log
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

Test BYTE images
gen A NL= 10 NS=10 linc=0 sinc=0 ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen B NL=5 NS=5 linc=0 sinc=0 ival=128
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
PUT B INTO A STARTING AT (5,5)
insect (A,B) C insect=(1,1,5,5,5,5)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp     1       3       5       7       9
   Line
      1       1   1   1   1   1   1   1   1   1   1
      2       1   1   1   1   1   1   1   1   1   1
      3       1   1   1   1   1   1   1   1   1   1
      4       1   1   1   1   1   1   1   1   1   1
      5       1   1   1   1 128 128 128 128 128   1
      6       1   1   1   1 128 128 128 128 128   1
      7       1   1   1   1 128 128 128 128 128   1
      8       1   1   1   1 128 128 128 128 128   1
      9       1   1   1   1 128 128 128 128 128   1
     10       1   1   1   1   1   1   1   1   1   1
let $echo="no"
PUT B INTO A STARTING AT (15,15)
insect (A,B) C sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      2       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      3       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      4       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      5       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      6       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      7       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      8       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
      9       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
     10       1   1   1   1   1   1   1   1   1   1   0   0   0   0   0   0   0   0   0   0
     11       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     12       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0 128 128 128 128 128   0
let $echo="no"
 
TEST HALF IMAGES
 
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
PUT B1 INTO A1 STARTING AT (5,5)
insect (A1,B1) C1 insect=(1,1,5,5,5,5)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C1 'zero
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1        10    10    10    10    10    10    10    10    10    10
      2        10    10    10    10    10    10    10    10    10    10
      3        10    10    10    10    10    10    10    10    10    10
      4        10    10    10    10    10    10    10    10    10    10
      5        10    10    10    10  5000  5000  5000  5000  5000    10
      6        10    10    10    10  5000  5000  5000  5000  5000    10
      7        10    10    10    10  5000  5000  5000  5000  5000    10
      8        10    10    10    10  5000  5000  5000  5000  5000    10
      9        10    10    10    10  5000  5000  5000  5000  5000    10
     10        10    10    10    10    10    10    10    10    10    10
let $echo="no"
PUT B1 INTO A1 STARTING AT (15,15)
insect (A1,B1) C1 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C1 'zero
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      2        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      3        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      4        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      5        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      6        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      7        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      8        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
      9        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
     10        10    10    10    10    10    10    10    10    10    10     0     0     0     0     0
     11         0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
     12         0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
     13         0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
     14         0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
     15         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000
     16         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000
     17         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000
     18         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000
     19         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000
     20         0     0     0     0     0     0     0     0     0     0     0     0     0     0  5000

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp      16    17    18    19    20
   Line
      1         0     0     0     0     0
      2         0     0     0     0     0
      3         0     0     0     0     0
      4         0     0     0     0     0
      5         0     0     0     0     0
      6         0     0     0     0     0
      7         0     0     0     0     0
      8         0     0     0     0     0
      9         0     0     0     0     0
     10         0     0     0     0     0
     11         0     0     0     0     0
     12         0     0     0     0     0
     13         0     0     0     0     0
     14         0     0     0     0     0
     15      5000  5000  5000  5000     0
     16      5000  5000  5000  5000     0
     17      5000  5000  5000  5000     0
     18      5000  5000  5000  5000     0
     19      5000  5000  5000  5000     0
     20      5000  5000  5000  5000     0
let $echo="no"
 
TEST FULL IMAGES
 
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
PUT B2 INTO A2 STARTING AT (5,5)
insect (A2,B2) C2 insect=(1,1,5,5,5,5)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C2 'zero
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      2           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      3           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      4           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      5           1000       1000       1000       1000      15000      15000      15000      15000      15000       1000
      6           1000       1000       1000       1000      15000      15000      15000      15000      15000       1000
      7           1000       1000       1000       1000      15000      15000      15000      15000      15000       1000
      8           1000       1000       1000       1000      15000      15000      15000      15000      15000       1000
      9           1000       1000       1000       1000      15000      15000      15000      15000      15000       1000
     10           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
let $echo="no"
PUT B2 INTO A2 STARTING AT (15,15)
insect (A2,B2) C2 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C2 'zero
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      2           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      3           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      4           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      5           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      6           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      7           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      8           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
      9           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
     10           1000       1000       1000       1000       1000       1000       1000       1000       1000       1000
     11              0          0          0          0          0          0          0          0          0          0
     12              0          0          0          0          0          0          0          0          0          0
     13              0          0          0          0          0          0          0          0          0          0
     14              0          0          0          0          0          0          0          0          0          0
     15              0          0          0          0          0          0          0          0          0          0
     16              0          0          0          0          0          0          0          0          0          0
     17              0          0          0          0          0          0          0          0          0          0
     18              0          0          0          0          0          0          0          0          0          0
     19              0          0          0          0          0          0          0          0          0          0
     20              0          0          0          0          0          0          0          0          0          0

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp           11         12         13         14         15         16         17         18         19         20
   Line
      1              0          0          0          0          0          0          0          0          0          0
      2              0          0          0          0          0          0          0          0          0          0
      3              0          0          0          0          0          0          0          0          0          0
      4              0          0          0          0          0          0          0          0          0          0
      5              0          0          0          0          0          0          0          0          0          0
      6              0          0          0          0          0          0          0          0          0          0
      7              0          0          0          0          0          0          0          0          0          0
      8              0          0          0          0          0          0          0          0          0          0
      9              0          0          0          0          0          0          0          0          0          0
     10              0          0          0          0          0          0          0          0          0          0
     11              0          0          0          0          0          0          0          0          0          0
     12              0          0          0          0          0          0          0          0          0          0
     13              0          0          0          0          0          0          0          0          0          0
     14              0          0          0          0          0          0          0          0          0          0
     15              0          0          0          0      15000      15000      15000      15000      15000          0
     16              0          0          0          0      15000      15000      15000      15000      15000          0
     17              0          0          0          0      15000      15000      15000      15000      15000          0
     18              0          0          0          0      15000      15000      15000      15000      15000          0
     19              0          0          0          0      15000      15000      15000      15000      15000          0
     20              0          0          0          0      15000      15000      15000      15000      15000          0
let $echo="no"
 
TEST REAL IMAGES
 
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
PUT B3 INTO A3 STARTING AT (5,5)
insect (A3,B3) C3 insect=(1,1,5,5,5,5)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C3 'zero
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      2       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      3       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      4       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      5       1.013E+03   1.013E+03   1.013E+03   1.013E+03   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   1.013E+03
      6       1.013E+03   1.013E+03   1.013E+03   1.013E+03   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   1.013E+03
      7       1.013E+03   1.013E+03   1.013E+03   1.013E+03   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   1.013E+03
      8       1.013E+03   1.013E+03   1.013E+03   1.013E+03   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   1.013E+03
      9       1.013E+03   1.013E+03   1.013E+03   1.013E+03   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   1.013E+03
     10       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
let $echo="no"
PUT B2 INTO A2 STARTING AT (15,15)
insect (A3,B3) C3 sl=1  ss=1 nl=20 ns=20  insect=(1,1,5,5,15,15)
Beginning VICAR task insect
INSECT version 7 Jan 2013 (64-bit) - rjb
let $echo="no"
LIST RESULTING IMAGE
list C3 'zero
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      2       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      3       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      4       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      5       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      6       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      7       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      8       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
      9       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
     10       1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03   1.013E+03
     11       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     12       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     13       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     14       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     15       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     16       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     17       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     18       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     19       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     20       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
 Task:INSECT    User:wlb       Date_Time:Wed Mar 18 16:35:47 2015
     Samp            11          12          13          14          15          16          17          18          19          20
   Line
      1       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      2       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      3       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      4       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      5       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      6       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      7       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      8       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
      9       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     10       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     11       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     12       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     13       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     14       0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00
     15       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
     16       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
     17       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
     18       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
     19       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
     20       0.000E+00   0.000E+00   0.000E+00   0.000E+00   2.512E+04   2.512E+04   2.512E+04   2.512E+04   2.512E+04   0.000E+00
let $echo="no"
$ Return
$!#############################################################################
