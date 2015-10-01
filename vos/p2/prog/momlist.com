$!****************************************************************************
$!
$! Build proc for MIPL module momlist
$! VPACK Version 1.8, Monday, July 07, 1997, 12:10:38
$!
$! Execute by entering:		$ @momlist
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
$ write sys$output "*** module momlist ***"
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
$ write sys$output "Invalid argument given to momlist.com file -- ", primary
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
$   if F$SEARCH("momlist.imake") .nes. ""
$   then
$      vimake momlist
$      purge momlist.bld
$   else
$      if F$SEARCH("momlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momlist.bld "STD"
$   else
$      @momlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momlist.com -
	-s momlist.f -
	-i momlist.imake -
	-p momlist.pdf -
	-t tstmomlist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create momlist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM MOMLIST
C
      SUBROUTINE MAIN44

         REAL*8 DN,SIG,VIG,DNSUM,DN2SUM
         REAL*4 BEXPO, EXPOS(30)
         REAL*4 DNS(6400)

         INTEGER NAREA,AREA(4,400)
         INTEGER NI,SL,SS,NL,NS,NL2,NS2,STAT
         INTEGER OUTAREA,AREA_ARRAY(100),AREA_ARRAY_CNT,NEXP

         CHARACTER*132  MSG1
         CHARACTER*255  TABLE_DS
         CHARACTER*1    TAB
         
         LOGICAL ITBL

 
         CALL IFMESSAGE ('MOMLIST version July 7, 1997')

         MSG1 = ' '

C-----Open the input Light Transfer File
         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
C     &             'U_FORMAT','HALF',' ')

C        Get area size fields...
         CALL XLGET (IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',
     &               4*NAREA,'FORMAT','INT','HIST','LTGEN',' ')

         CALL XVPARM ('TABLE',TABLE_DS,ICNT,IDEF,1)

         ITBL = ICNT .GT. 0
         TAB = CHAR(9)

         IF (ITBL) THEN 
            OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=998)
            WRITE(12,10)
     &         'AREA',TAB,'EXPOS',TAB,'MEAN.DN',TAB,'SIGMA'
         END IF

         CALL XVPARM('AREA',AREA_ARRAY,AREA_ARRAY_CNT,IDEF,100) 

         CALL XVSIZE(SL,SS,NL,NS,NL2,NS2)

	 WRITE(MSG1,12) 
     &      'AREA','   ','EXPOS','   ','MEAN.DN','   ','SIGMA'

         CALL XVMESSAGE (MSG1,' ')

         CALL XLGET (IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &               'NELEMENT',NEXP,'FORMAT','REAL',
     &               'HIST','LTGEN',' ')

         DO 50 L=1,NL 
            CALL XVREAD (IUNI,DNS,STAT,'LINE',L,'NSAMPS',NS,' ')
            NI = NINT (DNS(1))
            IF (NI .EQ. 0) THEN
               BEXPO = EXPOS(L)
               GOTO 970
            ENDIF
 
            DO 50 K=1,AREA_ARRAY_CNT
               IF (AREA_ARRAY(K) .GT. NAREA) THEN
                  CALL XVMESSAGE ('*** AREA INPUT OUT OF RANGE',' ')
                  WRITE(MSG1,13)
     &               '*** Max input area index =',NAREA
   13             FORMAT (A26,I4)
                  CALL XVMESSAGE (MSG1,' ')
                  CALL ABEND()
               ENDIF 
 	       IB = 3 * NI * (AREA_ARRAY(K)-1) 
               SL = AREA(1,AREA_ARRAY(K))
               SS = AREA(2,AREA_ARRAY(K))
               NL = AREA(3,AREA_ARRAY(K))
               NS = AREA(4,AREA_ARRAY(K))
	       OUTAREA=AREA_ARRAY(K)
               N = NI*NL*NS            !Total number of pixels in area*inputs
               DNSUM = 0.0D0
	       DN2SUM = 0.0D0

               DO I=1,NI
C              Sum of DNs across inputs
                  DNSUM = DNSUM + DBLE(DNS(IB+I+1))
C              Sum of DN*DN across inputs
                  DN2SUM = DN2SUM + DBLE(DNS(IB+I+1+NI))
	       ENDDO

               DN = DNSUM/DBLE(N)              !Average DN for area
               VIG = DN2SUM/DBLE(N) - DN*DN
               IF (VIG .LT. 0.0) THEN
                  CALL XVMESSAGE ('*** INVALID INPUT DATA FILE.',' ')
                  CALL XVMESSAGE
     &               ('*** Please make sure input has been',' ')
                  CALL XVMESSAGE
     &               ('*** correctly generated by MOMGEN.',' ')
                  CALL ABEND()
               ENDIF
               SIG = SQRT(VIG)   ! Calculate Standard Devation

	       WRITE(MSG1,11)
     &            OUTAREA,'   ',EXPOS(L),'   ',DN,'   ',SIG

               CALL XVMESSAGE (MSG1,' ')

               IF (ITBL) 
     &            WRITE(12,9) OUTAREA,TAB,EXPOS(L),TAB,DN,TAB,SIG

   50   CONTINUE

         CALL XVCLOSE(IUNI,IST,' ')
         IF(ITBL) CLOSE(12)

         RETURN
 
    9    FORMAT(1x,I5,A1,F8.2,A1,F8.2,A1,F8.3)
   10    FORMAT(1x,A5,A1,A8,A1,A8,A1,A8)
   11    FORMAT(1x,I5,A3,F8.2,A3,F8.2,A3,F8.3)
   12    FORMAT(1x,A5,A3,A8,A3,A8,A3,A8)
  998    CALL XVMESSAGE ('ERROR OPENING TABLE FILE',' ')
         CALL PRNT(4,1,JST,'IOSTAT =.')
         GO TO 999
  970    CALL PRNT(7,1,BEXPO,'***No data for exposure=.')
         CALL XVMESSAGE
     &     ('***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  999    CALL XVMESSAGE ('***MOMLIST task cancelled',' ')
         CALL ABEND
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momlist.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM momgen

   To Create the build file give the command:

		$ vimake momlist		(VMS)
   or
		% vimake momlist		(Unix)


************************************************************************/


#define PROGRAM	momlist
#define R2LIB

#define MODULE_LIST momlist.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create momlist.pdf
PROCESS HELP=*
PARM INP     STRING
PARM TABLE   STRING COUNT=0:1 default=--
PARM AREA    INTEGER COUNT=(1:100)
END-PROC
.TITLE
VICAR PROGRAM MOMLIST
.HELP
PURPOSE:

MOMLIST extracts the light transfer data from Light Transfer and 
Reciprocity files made by MOMGEN.

The input is a Light Transfer File (LTF) containing statistical data for
specified areas in the image for each exposure of a light transfer
sequence.  The LTF must have been previously initialized via LTGEN and
loaded with data via MOMGEN.

If an output file is specified for the TABLE parameter, an ASCII file of
tab-delimitted text is output.  This file will contain a tabular list of the
area, exposure time, mean DN and sigma for the area specified.

   MOMLIST LTF.DAT TABLE=A5.TBL AREA=5

HISTORY:
  7-JUL-97...Thomas Huang...Ported from VAX/VMS to UNIX and ALPHA/VMS.
  1-SEP-96...young kwon...INITIAL RELEASE
 17-SEP-96...c avis.......fix loop

.LEVEL1
.VARIABLE INP
 The Light Transfer or 
 Reciprocity File.
.VARIABLE TABLE
 The output table
 file
.VARIABLE AREA
 Specifies the area
 of which to print
 the stats
.LEVEL2
.VARIABLE INP
 STRING COUNT=1
 The Light Transfer or Reciprocity File created by LTGEN and MOMGEN 
 containing area statistics.
.VARIABLE TABLE
 The output table file
.VARIABLE AREA
 Specifies the area of which to print the stats
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstmomlist.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar

body
local dir string
let $autousage="none"
let _onfail="continue"

let $echo="no"
write ">>> GENERAL TEST <<<"
write " "
let $echo="yes"

gen e1.img 200 200 sinc=0
gen e2.img 200 200 sinc=0 ival=2
gen e3.img 200 200 sinc=0 ival=4
ltgen e1.img ltf.out gres=(2,10,10) expo=(-1.0,0.,50.,66.67,100.) ni=3

!verify exposure and area in VICAR label.
label-list ltf.out

!This should return all zeros, because they are place holders for momgen
list ltf.out
 
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=-1.
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=0.
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=50.
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=66.67
momgen (e1.img,e2.img,e3.img) ltf.out 'DBUG expo=100.
label-list ltf.out
list ltf.out (1,1,4,2) 
momlist inp=ltf.out table=ltf1.tbl area=(1,2,3,4)

typetext ltf1.tbl

let $echo="no"
write " "
write ">>> CASSINI TEST <<<"
write " "
let $echo="yes"

if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
end-if
 
!---------------------------
! Make a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! Each level has 3 frames associated with it.
 
!Set dns to 10 and replicate - set exposure to 0
f2 &"dir"sum2.1 l1.a func=10
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 110 and replicate - set exposure to 10
f2 &"dir"sum2.1 l2.a func=110
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 210 and replicate - set exposure to 20
f2 &"dir"sum2.1 l3.a func=210
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 410 and replicate - set exposure to 40
f2 &"dir"sum2.1 l4.a func=410
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created in SRCH format
createfile m.list
addtofile m.list "NEXT FILE=0001"
addtofile m.list "l1.a"
addtofile m.list "l1.b"
addtofile m.list "l1.c"
addtofile m.list "l2.a"
addtofile m.list "l2.b"
addtofile m.list "l2.c"
addtofile m.list "l3.a"
addtofile m.list "l3.b"
addtofile m.list "l3.c"
addtofile m.list "l4.a"
addtofile m.list "l4.b"
addtofile m.list "l4.c"
reset m.list
typetext m.list
 
!Initialize Light Transfer File
ltgen l1.a out=testltf.out list=m.list 'GRID

!Verify area and exposure on VICAR label
label-list testltf.out 
 
!Create list of the files for one exposure level in SRCH format
createfile m1.list
addtofile m1.list "NEXT FILE=0001"
addtofile m1.list "l1.a"
addtofile m1.list "l1.b"
addtofile m1.list "l1.c"
 
!Fill Light Transfer File with stats
momgen list=m1.list out=testltf.out 'DBUG expo=0.0
momgen list=m1.list out=testltf.out 'DBUG expo=10.0
momgen list=m1.list out=testltf.out 'DBUG expo=20.0
momgen list=m1.list out=testltf.out 'DBUG expo=40.0

label-list testltf.out

momlist inp=testltf.out table=ltf2.tbl area=(10,20,30,40,50,60,70,80,90,100)  
typetext ltf2.tbl

momlist inp=testltf.out table=ltf3.tbl area=(3,13,23,33,43,53,63,73,83,93)
typetext ltf3.tbl

momlist inp=testltf.out table=ltf4.tbl area=(7,17,27,37,47,57,67,77,87,97)
typetext ltf4.tbl

momlist inp=testltf.out table=ltf5.tbl area=(9,19,29,39,49,59,69,79,89,99)
typetext ltf5.tbl

if ($syschar(1)="UNIX")
   ush rm e1.*
   ush rm e2.*
   ush rm e3.*
   ush rm l1.*
   ush rm l2.*
   ush rm l3.*
   ush rm l4.*
   ush rm m.list
   ush rm m1.list
   ush rm *.out
else
   dcl del e1.*;*
   dcl del e2.*;*
   dcl del e3.*;*
   dcl del l1.*;*
   dcl del l2.*;*
   dcl del l3.*;*
   dcl del l4.*;*
   dcl del m.list;*
   dcl del m1.list;*
   dcl del *.out;*
end-if
end-proc

$ Return
$!#############################################################################
