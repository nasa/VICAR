$!****************************************************************************
$!
$! Build proc for MIPL module momgen2
$! VPACK Version 1.8, Thursday, March 20, 1997, 14:31:32
$!
$! Execute by entering:		$ @momgen2
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module momgen2 ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to momgen2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("momgen2.imake") .nes. ""
$   then
$      vimake momgen2
$      purge momgen2.bld
$   else
$      if F$SEARCH("momgen2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momgen2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momgen2.bld "STD"
$   else
$      @momgen2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momgen2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momgen2.com -
	-i momgen2.imake -
	-p momgen2.pdf -
	-t tstmomgen2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momgen2.imake
#define PROCEDURE momgen2
#define R2LIB
$ Return
$!#############################################################################
$PDF_File:
$ create momgen2.pdf
PROCEDURE help=*

! PROCEDURE TO PREPARE DATA FOR MOMGEN
! Will create Light Transfer Files from sets of
! images which have fixed radiances and increasing
! exposures or Reciprocity Files from sets of
! images where the radiances and exposures both vary

PARM LIST       TYPE=STRING
PARM LTFRCP     TYPE=STRING
PARM LTFILE     TYPE=STRING   COUNT=(0:1) DEFAULT=--

refgbl $BECHO
refgbl $syschar

BODY
LOCAL (F,G,LUMS,EXS)      STRING
LOCAL (NLVL,I1,I2,I3,I,J) INTEGER
LOCAL (EXP,LM,X)          REAL
LOCAL EX                  REAL      COUNT=1:100
LOCAL LMS                 STRING    COUNT=1:100

if ($syschar(1)="UNIX")
   defcmd-replace typeit "ush cat"
else
   defcmd-replace typeit "dcl type"
end-if
 
reset &LIST
 
!Read labels for exposure and radiance of first file in list
nxt &LIST F I1 I2 I3

getlab &F lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY +
       itm_type=REAL itm_task="CASSINI-ISS"

let EX(1)=EXP
 
getlab &F lab_item="RADIANCE" itm_name=LM 'PROPERTY +
       itm_type=REAL itm_task="CASSINI-ISS"

let LUMS = "&LM"

let LMS(1)=LUMS
 
!Create and initialize the file to contain the filenames for the
!first exposure level.
createfile list1.dat
addtofile list1.dat "NEXT FILE=0001"
addtofile list1.dat "&F"

!Loop thru the files in the input list (which is in order of increasing
!exposure) and group into exposure groups (levels).  For each group, make
!a list of the filenames for submission to MOMGEN

let J=1                    !NUMBER OF EXPOSURE LEVELS
loop
   nxt &LIST F I1 I2 I3
   if (F="END_OF_FILE") break
   getlab &F lab_item="EXPOSURE_DURATION" itm_name=EXP 'PROPERTY +
          itm_type=REAL itm_task="CASSINI-ISS"
   getlab &F lab_item="RADIANCE" itm_name=LM 'PROPERTY +
          itm_type=REAL itm_task="CASSINI-ISS"
   let LUMS = "&LM"   !convert real to string 
   if (EXP=EX(J))
       let G="list"//"&J"//".dat"
       addtofile &G "&F"
   else
       let J=J+1
       let EX(J)=EXP
       let LMS(J) = LUMS
       let G="list"//"&J"//".dat"
       createfile &G
       addtofile &G "NEXT FILE=0001"
       addtofile &G "&F"
   end-if
end-loop
 
let NLVL=J             !NUMBER OF UNIQUE EXPOSURE TIMES
 
let $BECHO="NO"
write " NUMBER OF EXPOSURE LEVELS = &NLVL"
write " "
 
!print out all exposure levels found
write " "
write " EXPOSURE LEVEL ... EXPOSURE TIME ... RADIANCES"
let I=1
loop
   let X=EX(I)
   let EXS = "&X"
   let LUMS=LMS(I)
   write "       &I              &EXS             &LUMS"
   if (I=NLVL) break
   let I=I+1
end-loop
 
write " "
let $BECHO="YES"
 
!run MOMGEN for all exposure levels
let I=1
loop
   let X=EX(I)
   let EXS = "&X"   !convert real value to string 
   let G="list"//"&I"//".dat"
 
   let $BECHO="NO"
   write "FRAME LIST FOR LEVEL &I    EXPOSURE TIME &EXS"
   typeit &G
   write " "
   let $BECHO="YES"
 
   momgen LIST="list"&I".dat" out=&LTFRCP

 
   if (I=NLVL) break
   let I=I+1
end-loop
 
if ($count(LTFILE) = 1)
! Create and fill text file of radiance values for CCCDRECIP
! CCDRECIP expects radiance of dc to be 0.0
   createfile &LTFILE
   let I=1
   loop 
      let LUMS=LMS(I)
      if (EX(I)=0.0) let LUMS="0.0"
      addtofile &LTFILE "&LUMS"
      if (I=NLVL) break
      let I=I+1
   end-loop
end-if
 
end-proc
 
.TITLE
Vicar Procedure MOMGEN2
.HELP
PURPOSE:
 
Calls MOMGEN to fill Light Transfer Files or Reciprocity Files from sets of
images of known radiances and exposures.  These output files are
required for the programs CCDRECIP, CCDSLOPE and CCDNOISE.
 
EXECUTION:
 
        MOMGEN2 LIST=file LTFRCP=outfile LTFILE=ltfile
 
.PAGE
OPERATION:
 
MOMGEN2 processes frames of a Light-transfer or Reciprocity sequence
into a Light-transfer file or Reciprocity file.  All frames are input
in a SRCH-format list.  MOMGEN2 splits the list into multiple executions of
MOMGEN.
 
The input list of filenames is assumed to be in order of increasing
exposure time.  MOMGEN2 splits the frames into exposure groups by
reading the VICAR labels and calls MOMGEN to process each group.  The
Light-transfer file or Reciprocity file filled by multiple executions
of MOMGEN.
 
In addition, the radiances of the frames are read from the labels and,
if the LTFILE parameter is given, a text file listing the radiances
of each exposure group is written.  This file may be input to the
program CCDRECIP.
 
.PAGE
 
RESTRICTIONS:
 1.  The list of filenames must be in order of increasing exposure
     time.
 
 2.  The output Light-transfer or Reciprocity file must have been
     previously created and formatted by LTGEN.
 
.PAGE
ORIGINAL PROGRAMMER: Charlie Avis
COGNIZANT PROGRAMMER: Charlie Avis
REVISION HISTORY:

   20 Mar  97 ...T.Huang.... Ported from VAX to UNIX. 
   29 July 94 ...C.C.Avis... Initial release
 
.LEVEL1
.VARIABLE LIST
 A SRCH-format list
 of filenames to
 process.
 
.VARIABLE LTFRCP
 A Light-transfer file
 or Reciprocity file
 
.VARIABLE LTFILE
 A text file of
 radiance values
 
.LEVEL2
.VARIABLE LIST
 STRING
 Specifies the name of a SRCH-format file containing a list
 of filenames to process.  The files must be in order of increasing
 exposure time.  Multiple frames of the same exposure time are
 grouped for executions of MOMGEN.
 
.VARIABLE LTFRCP
 STRING
 Specifies the output Light-transfer file or Reciprocity file which
 is filled by the executions of MOMGEN.  It must have been previously
 created by LTGEN.  This file is necessary for input to programs
 CCDRECIP, CCDNOISE, and  CCDSLOPE.
 
.VARIABLE LTFILE
 STRING OPTIONAL
 Specifies an output file to contain a list of the radiance values
 for the unique exposure times.  Note:  If the exposure time = 0.0,
 the actual radiance value will be ignored and a value of 0.0 entered.
 This file may be input to CCDRECIP.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstmomgen2.pdf
procedure

refgbl $echo
refgbl $becho
refgbl $syschar
refgbl $autousage

body
local dir string
let $echo="yes"
let $becho="yes"
let $autousage="none"
let _onfail="continue"
 
!CASSINI TEST:
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   defcmd-replace typeit "ush cat"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
   defcmd-replace typeit "dcl type"
end-if

!---------------------------
! Make a test light transfer file which has exposure levels of
! 0,10,20,40 and each input frame was 10,110,210,410 dn respectively.
! Each level has three frames associated with it.
 
!Set dns to 10 and replicate - set exposure to 0
f2 inp=&"dir"sum2.1 out=l1.a func=10
label-rep l1.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=0"
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 110 and replicate - set exposure to 10
f2 inp=&"dir"sum2.1 out=l2.a func=110
label-rep l2.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=10."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 210 and replicate - set exposure to 20
f2 inp=&"dir"sum2.1 out=l3.a func=210
label-rep l3.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=20."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 410 and replicate - set exposure to 40
f2 inp=&"dir"sum2.1 out=l4.a func=410
label-rep l4.a 'PROP property="CASSINI-ISS" item="EXPOSURE_DURATION=40."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created
createfile m2.list
addtofile m2.list "NEXT FILE=0001"
addtofile m2.list "l1.a"
addtofile m2.list "l1.b"
addtofile m2.list "l1.c"
addtofile m2.list "l2.a"
addtofile m2.list "l2.b"
addtofile m2.list "l2.c"
addtofile m2.list "l3.a"
addtofile m2.list "l3.b"
addtofile m2.list "l3.c"
addtofile m2.list "l4.a"
addtofile m2.list "l4.b"
addtofile m2.list "l4.c"
reset m2.list 
typeit m2.list
 
!Initialize Light Transfer File
ltgen l1.a out=testltf.out list=m2.list 'GRID
!Make a copy
copy testltf.out testltf2.out
 
!Fill Light Transfer File with stats
momgen2 list=m2.list ltfrcp=testltf.out
 
!See if CCDSLOPE is happy with it.
ccdslope testltf.out mofset=0.0 light=10. rej=0 'SUBDC
 
!Try it with the LTFILE option
!(but all the radiances are the same except the first one)
momgen2 list=m2.list ltfrcp=testltf2.out ltfile=lum.tmp
typeit lum.tmp

if ($syschar(1) = "UNIX")
   ush rm l1.*
   ush rm l2.*
   ush rm l3.*
   ush rm l4.*
   ush rm lum.tmp
   ush rm m2.list
else
   dcl del l1.*;*
   dcl del l2.*;*
   dcl del l3.*;*
   dcl del l4.*;*
   dcl del lum.tmp;*
   dcl del m2.list;*
end-if
END-PROC
$ Return
$!#############################################################################
