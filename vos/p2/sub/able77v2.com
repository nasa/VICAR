$!****************************************************************************
$!
$! Build proc for MIPL module able77v2
$! VPACK Version 1.9, Monday, December 07, 2009, 16:06:33
$!
$! Execute by entering:		$ @able77v2
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module able77v2 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to able77v2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("able77v2.imake") .nes. ""
$   then
$      vimake able77v2
$      purge able77v2.bld
$   else
$      if F$SEARCH("able77v2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake able77v2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @able77v2.bld "STD"
$   else
$      @able77v2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create able77v2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack able77v2.com -mixed -
	-s able77v2.f zable77v2.c -
	-i able77v2.imake -
	-t table77v2.f tzable77v2.c table77v2.imake table77v2.pdf -
	   tstable77v2.pdf -
	-o able77v2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create able77v2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***************************************************************************
C      SUBROUTINE ABLE77V2
C
C  PARAMETERS
C  ----------
C       IND     0=OK
C              -1=ONE OR MORE ITEMS COULD NOT BE FOUND,AND ARE SET TO -999
C                   (NUMBERS) OR "?" (CHARACTERS) IN ARR
C
C       DSRN       UNIT NUMBER
C
C       ARR       I*4 ARRAY OF RETURNED DATA (SEE ABLE77 DOCUMENT UPSTAIRS).
C              USER MUST INSERT REQUIRED LENGTH OF ARRAY TO BE FILLED,INTO
C              ARR(1) BEFORE CALLING.
C              THE FOLLOWING ITEMS HAVE BEEN IMPLEMENTED SO FAR:
C              1      =3 for a valid Voyager label, =0 otherwise
C              2      GIVES AN INTEGER FDS COUNT : THE REAL VALUE MULTIPLIED
C                     BY 100
C              3      EXPOSURE
C              4      FILTER POSITION
C              5      SCAN RATE
C              6      CAMERA SERIAL NUMBER,IE
C                     FOR VOYAGER 2,  WIDE ANGLE,CAMERA S/N IS 4
C                     FOR VOYAGER 2,NARROW ANGLE,CAMERA S/N IS 5
C                     FOR VOYAGER 1,  WIDE ANGLE,CAMERA S/N IS 6
C                     FOR VOYAGER 1,NARROW ANGLE,CAMERA S/N IS 7
C             7       CAMERA 1=NA 2=WA
C             8       GAIN 1=HI 0=LO
C            10       EVENT YEAR
C            11       EVENT DAY
C	     12       EVENT HOUR   
C	     13       EVENT MINUTE  
C	     14       EVENT SECOND   
C	     19       S/C ID
C	     20       PICNO
C	     29       INPUT TAPE
C	     31       OUTPUT TAPE
C	     33       INPUT FILE - 1
C	     34       OUTPUT FILE   
C	     35       ERT YEAR   
C	     36       ERT DAY
C	     37       ERT HOUR
C	     38       ERT MINUTE
C	     39       ERT SECOND
C
C  OPERATION
C  ---------
C
C     THE DATA SET MUST BE OPENED PRIOR TO CALLING ABLE77V2.
C     NOT-FOUND NUMERIC DATA IS RETURNED AS -999
C     NOT-FOUND CHARACTER DATA IS RETURNED AS "!"
C     PROGRAM DOES ASSUME STANDARD VOYAGER-TYPE VICAR LABELS,BUT IT WILL
C     SUCCESSFULLY IGNORE ANY OTHER LABELS PRIOR TO THE ONE STARTING 
C     "VGR-....".
C
C       NICK MARRIAGE       30 SEPTEMBER 1981
C     
C***************************************************************************
       SUBROUTINE ABLE77V2 (IND,DSRN,ARR)
c
       INTEGER*4 IND,DSRN,ARR(*),SIZE,VOYAGER,ANGLE,CAM
       INTEGER*4 VGRIND,IFDS1,IFDS2
       REAL*4 EXPOSURE,TMP
       CHARACTER*7200 ALABEL
c
       SIZE=ARR(1)              ! CALLER PASSES LENGTH OF ARRAY
       IF (SIZE.LT.1) THEN
          CALL XVMESSAGE( ' ABLE77V2 -SIZE IS LESS THAN 1',' ')
          CALL ABEND
       END IF
       IF (SIZE.GT.50)SIZE=50

       IND=0                     ! UNLESS SOMETHING GOES WRONG
c
C--SET ARR(1) TO DEFAULT VGR FLIGHT LABEL
       ARR(1)=3                        

C--NOW GET THE FIRST LABEL BLOCK INTO ALABEL...
        BUFSIZE=7200
       CALL XLGETLABEL(DSRN,ALABEL,BUFSIZE,ISTAT)
       CALL CHKSTAT(ISTAT,' ABLE77V2  ERR, ISTAT=',1,ISTAT,1)
        ISTAT=INDEX(ALABEL(1:),'VGR-')
        IF(ISTAT.EQ.0)ARR(1)=0
C--NOW START FINDING THINGS AND INSERTING INTO ARRAY...
c
C--FIRST FIND WHICH VOYAGER IT WAS
        IF(ALABEL(ISTAT+4:ISTAT+4).eq.'1')THEN
            VOYAGER=1
            VGRIND=0
        ELSE IF(ALABEL(ISTAT+4:ISTAT+4).eq.'2')THEN
            VOYAGER=2
            VGRIND=0
        ELSE
            VGRIND=1
        ENDIF
C
C--FIND THE FDS COUNT
       IF (2 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' FDS ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+5:ISTAT+5).NE.'*') THEN
           READ(ALABEL(ISTAT+5:),'(BN,I5)') IFDS1
           READ(ALABEL(ISTAT+11:),'(BN,I2)') IFDS2
           ARR(2) = IFDS1*100 + IFDS2
       ELSE
           CALL XVMESSAGE( ' ABLE77V2 -FDS COUNT NOT FOUND',' ')
           ARR(2) = -999
           IND = -1
       END IF
C
C--FIND EXPO TIME
       IF (3 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' EXP ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+5:ISTAT+5).NE.'*')  THEN
          READ(ALABEL(ISTAT+5:),'(BN,F7.0)')  EXPOSURE
          CALL MVe(4,1,EXPOSURE,ARR(3),1,1)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -EXPO TIME NOT FOUND',' ') ! FLAG ERROR
         ARR(3) = -999
         IND = -1
       ENDIF
C
C--FILTER POSITION
       IF (4 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' FILT ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+6:ISTAT+6).NE.'*') THEN 
          READ(ALABEL(ISTAT+6:),'(BN,I1)')  ARR(4)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -FILTER POSITION NOT FOUND',' ') ! FLAG MISTAKE MADE
         ARR(4) = -999
         IND=-1
       ENDIF
C
C--SCAN RATE
C--FIX CODE TO HANDLE 10:1 SCAN RATE ... FFM
       IF (5 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),'  SCAN RATE')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+12:ISTAT+12) .NE. '*') THEN
          READ(ALABEL(ISTAT+12:),'(BN,I2)') ARR(5)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -SCAN RATE NOT FOUND',' ')
         ARR(5)=-999
         IND=-1
       ENDIF
C
C--FIND CAMERA SERIAL NUMBER (ARR(6)
C       FOR VOYAGER 2,  WIDE ANGLE,CAMERA S/N IS 4
C       FOR VOYAGER 2,NARROW ANGLE,CAMERA S/N IS 5
C       FOR VOYAGER 1,  WIDE ANGLE,CAMERA S/N IS 6
C       FOR VOYAGER 1,NARROW ANGLE,CAMERA S/N IS 7
C  AND
C--CAMERA NA=1 WA=2 (CAM)
C
       IF (6 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),'NA CAMERA')
       IF(ISTAT.GT.0)THEN
         CAM = 1
         ANGLE=1
       ELSE IF(INDEX(ALABEL(1:),'WA CAMERA').GT.0)THEN
         CAM  = 2
         ANGLE = 0
       ELSE                            ! FLAG IT AS 'WRONG FORMAT LABEL'
         CALL XVMESSAGE( ' ABLE77V2 -CAMERA NOT FOUND',' ')
         CAM  = -999
         IND = -1
       ENDIF
c
       IF (VOYAGER .EQ. 1) THEN
          ARR(6) = ANGLE + 6
       ELSE IF (VOYAGER .EQ. 2) THEN
          ARR(6) = ANGLE + 4
       ELSE                     ! FLAG IT AS "BAD LABEL" AGAIN
          CALL XVMESSAGE( ' ABLE77V2 -CAM SERIAL # NOT FOUND',' ')
          ARR(6) = -999
          IND = -1
       END IF
c
       IF (7 .GT. SIZE) RETURN	   !CAMERA
       ARR(7) = CAM
C
C--FIND GAIN
       IF (8 .GT. SIZE) RETURN
       IF(INDEX(ALABEL(1:),' LO GAIN').GT.0)THEN
         ARR(8) = 0
       ELSE IF(INDEX(ALABEL(1:),' HI GAIN ').GT.0)THEN
         ARR(8) = 1
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -GAIN NOT FOUND',' ')
         ARR(8) = -999
         IND = -1
       END IF
C
C--GET TEMP (WHICH IS ALWAYS WRONG)
      IF (9 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' VIDICON TEMP ')
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+15:ISTAT+15) .NE. '*') THEN
          READ(ALABEL(ISTAT+14:),'(BN,F4.0)') TMP
          CALL MVE(4,1,TMP,ARR(9),1,1)
      ELSE
        CALL XVMESSAGE( ' ABLE77V2 -TEMP NOT FOUND',' ')
        ARR(9) = -999
        IND = -1
      END IF
C
C--WHICH YEAR WAS IT?
       IF (10 .GT. SIZE) RETURN
       ISTAT=INDEX(ALABEL(1:),' SCET ')
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+6:ISTAT+6) .NE. '*') THEN
          READ(ALABEL(ISTAT+6:),'(BN,I2)') ARR(10)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -YEAR NOT FOUND',' ')
         ARR(10)=-999                  ! NOT FOUND
         IND=-1
       ENDIF
C
C--WHICH DAY WAS IT?
       IF (11 .GT. SIZE) RETURN
       IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+9:ISTAT+9) .NE. '*') THEN
          READ(ALABEL(ISTAT+9:),'(BN,I3)') ARR(11)
       ELSE
         CALL XVMESSAGE( ' ABLE77V2 -DAY NOT FOUND',' ')
         ARR(11)=-999       ! NOT FOUND
         IND=-1
       ENDIF
C
C--WHICH HOUR IS IT?
      IF (12 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+13:ISTAT+13) .NE. '*') THEN
          READ(ALABEL(ISTAT+13:),'(BN,I2)') ARR(12)
      ELSE
        CALL XVMESSAGE( ' ABLE77V2 -HOUR NOT FOUND',' ')
        ARR(12)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--WHICH MINUTE IS IT?
      IF (13 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+16:ISTAT+16) .NE. '*') THEN
          READ(ALABEL(ISTAT+16:),'(BN,I2)') ARR(13)
      ELSE   
        CALL XVMESSAGE(' ABLE77V2 -MINUTE NOT FOUND',' ')
        ARR(13)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--WHICH SECOND IS IT?
      IF (14 .GT. SIZE) RETURN
      IF (ISTAT.NE.0 .AND. ALABEL(ISTAT+19:ISTAT+19) .NE. '*') THEN
          READ(ALABEL(ISTAT+19:),'(BN,I2)') ARR(14)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -SECOND NOT FOUND',' ')
        ARR(14)=-999       ! NOT FOUND
        IND=-1
      ENDIF
C
C--FILL IN WHICH VOYAGER IT WAS
       IF (19 .GT. SIZE) RETURN
       IF (VGRIND .NE. 0) THEN           ! A SILLY NUMBER IF CORRUPT LABEL
         CALL XVMESSAGE(' ABLE77V2 -VOYAGER FLIGHT # NOT FOUND',' ')
         ARR(19)=-999
         IND=-1
       ELSE
          ARR(19)=VOYAGER
       ENDIF
C
C--GET THE PICNO
      IF (20 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' PICNO ')
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+7:ISTAT+7).NE.'x'
     +    .and. ALABEL(ISTAT+7:ISTAT+7).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+7:),ARR(20),10)
      else
            call mvcl('!        ',arr(20),10)  !PICNO # NOT FOUND
            IND = -1
      END IF
C
C--GET EDR TAPE AND FILE
      IF (29 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),'IN/')
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+3:ISTAT+3).NE.'x'
     +    .and. ALABEL(ISTAT+3:ISTAT+3).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+3:),ARR(29),6)
      else
            call mvcl('!    ',arr(29),6)          !EDR TAPE NOT FOUND
            IND = -1
      END IF
         
      IF (33 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+10:ISTAT+10).NE.'x'
     +    .and. ALABEL(ISTAT+10:ISTAT+10).NE.'*'))THEN
          READ(ALABEL(ISTAT+10:),'(BN,I2)') ARR(33)
      ELSE
            ARR(33) = -999        !EDR FILE NOT FOUND
            IND = -1
      END IF
C
C--GET OUTPUT TAPE AND FILE
      IF (31 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),' OUT/')
      if(istat.gt.0 .and. (alabel(istat+5:istat+5).ne.'x'
     +    .and. ALABEL(ISTAT+5:ISTAT+5).NE.'*'))THEN
            CALL MVCL(ALABEL(ISTAT+5:),ARR(31),6)
      ELSE
            call mvcl('!    ',arr(31),6)          !output TAPE NOT FOUND
            IND = -1
      ENDIF
      IF (34 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. (ALABEL(ISTAT+12:ISTAT+12).NE.'x'
     +    .and. ALABEL(ISTAT+12:ISTAT+12).NE.'*'))THEN
           READ(ALABEL(ISTAT+12:),'(BN,I2)') ARR(34)
      ELSE
              ARR(34) = -999           !output file not found
              IND = -1         
      ENDIF
c
C--WHICH ERT YEAR WAS IT?
      IF (35 .GT. SIZE) RETURN
      ISTAT=INDEX(ALABEL(1:),'ERT ')
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+4:ISTAT+4).NE.'*')THEN
         READ(ALABEL(ISTAT+4:),'(BN,I2)') ARR(35)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT YEAR NOT FOUND',' ')
        ARR(35) = -999
        IND = -1
      END IF
c
C--WHICH ERT DAY WAS IT?
      IF (36 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+7:ISTAT+7).NE.'*')THEN
         READ(ALABEL(ISTAT+7:),'(BN,I3)') ARR(36)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT DAY NOT FOUND',' ')
        ARR(36) = -999
        IND = -1
      END IF
c
C--WHICH ERT HOUR IS IT?
      IF (37 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+12:ISTAT+12).NE.'*')THEN
         READ(ALABEL(ISTAT+11:),'(BN,I2)') ARR(37)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT HOUR NOT FOUND',' ')
        ARR(37) = -999
        IND = -1
      END IF
C
C--WHICH ERT MINUTE IS IT
      IF (38 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+14:ISTAT+14).NE.'*')THEN
         READ(ALABEL(ISTAT+14:),'(BN,I2)') ARR(38)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT MINUTE NOT FOUND',' ')
        ARR(38) = -999
        IND = -1
      END IF
C
C--WHICH ERT SECOND IS IT?
      IF (39 .GT. SIZE) RETURN
      IF(ISTAT.GT.0 .AND. ALABEL(ISTAT+17:ISTAT+17).NE.'*')THEN
         READ(ALABEL(ISTAT+17:),'(BN,I2)') ARR(39)
      ELSE
        CALL XVMESSAGE(' ABLE77V2 -ERT SECOND NOT FOUND',' ')
        ARR(39) = -999
        IND = -1
      END IF
1     RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zable77v2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void FTN_NAME2(able77v2,ABLE77V2)();
/************************************************************************/
/*  C-Callable Version ZABLE77V2  (See Fortran Source ABLE77V2)             */
/************************************************************************/

void  zable77v2(ind,unit,arra)  
int   *ind;          /*  returned status (output) */
int   unit;          /* VICAR  unit # (input)     */
void   *arra;          /* array containing the extracted data (output)  */
{
FTN_NAME2(able77v2, ABLE77V2) (ind, &unit, arra) ;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create able77v2.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY able77v2

   To Create the build file give the command:

	$ vimake able77v2                     (VMS)
   or
	% vimake able77v2                     (Unix)


*************************************************************************/

#define SUBROUTINE able77v2

#define MODULE_LIST  able77v2.f  zable77v2.c

#define P2_SUBLIB

#define USES_FORTRAN
#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create table77v2.f
c*********************************************************
c
c     Test program for subroutine ABLE77V2
c
c*********************************************************
c
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c
      INTEGER IND, ARR(50), UNIT, NUM(3)
      DATA NUM /39,6,22/
      CHARACTER*132 MSG
c      
      CALL XVUNIT(UNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(UNIT,ISTAT,' ')
      IF(ISTAT.NE.1)THEN
         CALL XVMESSAGE(' CANT OPEN INPUT',' ')
         CALL ABEND
      ENDIF
c
      CALL XVMESSAGE('*****************FORTRAN CALLABLE*********',' ')
c      
      DO I = 1, 3
         WRITE (MSG,9900) NUM(I)
9900  FORMAT (' ABLE77 TEST RUN--SIZE=',I4,'.')
         CALL XVMESSAGE(MSG(2:28),' ')
         ARR(1) = NUM(I)
         CALL ABLE77V2(IND,UNIT,ARR)
         CALL PRNT(4,1,ARR(1),' label type=.')
         CALL PRNT(4,1,ARR(2),' FDS COUNT=.')
         CALL PRNT(7,1,ARR(3),' EXPOSURE=.')
         CALL PRNT(4,1,ARR(4),' FILTER POSITION=.')
         CALL PRNT(4,1,ARR(5),' SCAN RATE=.')
         CALL PRNT(4,1,ARR(6),' CAMERA SERIAL NUMBER=.')
c
         IF (NUM(I).EQ.6) THEN
            CALL XVMESSAGE('6 VALUES RETURNED, ................',' ')
            GO TO 100
         ENDIF
         CALL PRNT(4,1,ARR(7),' CAMERA =.')
         CALL PRNT(4,1,ARR(8),' GAIN = .')
         CALL PRNT(4,1,ARR(10),' EVENT YEAR =.')
         CALL PRNT(4,1,ARR(11),' EVENT DAY = .')
         CALL PRNT(4,1,ARR(12),' EVENT HOUR =.')
         CALL PRNT(4,1,ARR(13),' EVENT MINUTE =.')
         CALL PRNT(4,1,ARR(14),' EVENT SECOND =.')
         CALL PRNT(4,1,ARR(19),' S/C ID =.')
         CALL PRNT(99,10,ARR(20),'PICNO =.')
c         
         IF (NUM(I).EQ.22) THEN
           CALL XVMESSAGE(' 22 VALUES RETURNED, ................',' ')
           GO TO 100
         ENDIF
         CALL PRNT(99,6,ARR(29),'INPUT TAPE =.')
         CALL PRNT(99,6,ARR(31),'OUTPUT TAPE =.')
         CALL PRNT(4,1,ARR(33),'INPUT FILE =.')
         CALL PRNT(4,1,ARR(34),'OUTPUT FILE =.')
         CALL PRNT(4,1,ARR(35),'ERT YEAR =.')
         CALL PRNT(4,1,ARR(36),'ERT DAY = .')
         CALL PRNT(4,1,ARR(37),'ERT HOUR =.')
         CALL PRNT(4,1,ARR(38),'ERT MINUTE =.')
         CALL PRNT(4,1,ARR(39),'ERT SECOND =.')
100      CALL PRNT(4,1,IND,'IND =.')
      END DO
C
      CALL XVMESSAGE('*****************C  CALLABLE***************',' ')
c      
      DO I = 1, 3
         WRITE (MSG,9990) NUM(I)
9990  FORMAT (' ABLE77 TEST RUN--SIZE=',I4,'.')
         CALL XVMESSAGE(MSG(2:28),' ')
         ARR(1) = NUM(I)
         CALL TZABLE77V2(IND,UNIT,ARR)
         CALL PRNT(4,1,ARR(1),' label type=.')
         CALL PRNT(4,1,ARR(2),' FDS COUNT=.')
         CALL PRNT(7,1,ARR(3),' EXPOSURE=.')
         CALL PRNT(4,1,ARR(4),' FILTER POSITION=.')
         CALL PRNT(4,1,ARR(5),' SCAN RATE=.')
         CALL PRNT(4,1,ARR(6),' CAMERA SERIAL NUMBER=.')
c
         IF (NUM(I).EQ.6) THEN
            CALL XVMESSAGE('6 VALUES RETURNED, ................',' ')
            GO TO 200
         ENDIF
         CALL PRNT(4,1,ARR(7),' CAMERA =.')
         CALL PRNT(4,1,ARR(8),' GAIN = .')
         CALL PRNT(4,1,ARR(10),' EVENT YEAR =.')
         CALL PRNT(4,1,ARR(11),' EVENT DAY = .')
         CALL PRNT(4,1,ARR(12),' EVENT HOUR =.')
         CALL PRNT(4,1,ARR(13),' EVENT MINUTE =.')
         CALL PRNT(4,1,ARR(14),' EVENT SECOND =.')
         CALL PRNT(4,1,ARR(19),' S/C ID =.')
         CALL PRNT(99,10,ARR(20),'PICNO =.')
c         
         IF (NUM(I).EQ.22) THEN
           CALL XVMESSAGE(' 22 VALUES RETURNED, ................',' ')
           GO TO 200
         ENDIF
         CALL PRNT(99,6,ARR(29),'INPUT TAPE =.')
         CALL PRNT(99,6,ARR(31),'OUTPUT TAPE =.')
         CALL PRNT(4,1,ARR(33),'INPUT FILE =.')
         CALL PRNT(4,1,ARR(34),'OUTPUT FILE =.')
         CALL PRNT(4,1,ARR(35),'ERT YEAR =.')
         CALL PRNT(4,1,ARR(36),'ERT DAY = .')
         CALL PRNT(4,1,ARR(37),'ERT HOUR =.')
         CALL PRNT(4,1,ARR(38),'ERT MINUTE =.')
         CALL PRNT(4,1,ARR(39),'ERT SECOND =.')
200      CALL PRNT(4,1,IND,'IND =.')
      END DO
C
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzable77v2.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TABLE77V2.F */
/************************************************************************/

void FTN_NAME(tzable77v2)(ind,unit,arra) 
  int   *ind;     /* returned status (output) */
  int   *unit;    /* VICAR unit # (input) */
  void  *arra;   /* Array containing extracted data (output) */

{
       zable77v2(ind,*unit,arra);
}

$!-----------------------------------------------------------------------------
$ create table77v2.imake
/* Imake file for Fortran-Test of VICAR subroutine  ABLE77V2  */

#define PROGRAM table77v2

#define MODULE_LIST table77v2.f  tzable77v2.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_P2SUB    
#define LIB_RTL
#define LIB_TAE       
/*#define LIB_LOCAL*/   /*  disable when delivering   */
$!-----------------------------------------------------------------------------
$ create table77v2.pdf
!*****************************************************************************
! TABLE77V2.PDF - pdf for test program TABLE77V2.F for the subroutine ABLE77V2
!*****************************************************************************
PROCESS
PARM INP  TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstable77v2.pdf
!****************************************************************************
! TSTABLE77V2_VMS.PDF, unit test procedure for subroutine ABLE77V2.F.
!*****************************************************************************
procedure help=*
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let  $echo="yes"
WRITE "Testing subroutine ABLE77V2"
if ($syschar(1) = "VAX_VMS")
   table77v2 inp=WMS_TEST_WORK:[TESTDATA.MIPL.VGR]f1636832.geo
   table77v2 inp=WMS_TEST_WORK:[TESTDATA.MIPL.VGR]able77v2.dat
   !table77v2 inp=dev:[tyr030.able77]mimas1339s1na.geoma
else 
   table77v2 inp=/project/it/testdata/mipl/vgr/f1636832.geo
   table77v2 inp=/project/it/testdata/mipl/vgr/able77v2.dat
end-if
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create able77v2.hlp
1 ABLE77V2

   ABLE77V2 provides programmers with a convenient means of extracting data
  from VICAR labels conforming to Voyager label format conventions.

  FORTRAN Calling Sequence:  INTEGER*4 IND,UNIT,ARR(50)

                             CALL ABLE77V2(IND,UNIT,ARR)

  C Calling Sequence:   int ind, unit, arr[50];
            
                        zable77v2(&ind,unit,arr)

  Arguments:   
		IND	(output)      integer
		UNIT	(input)	      integer
		ARR	(output)      integer array

2 Operation

   This subroutine provides programmers with a convenient means of
  extracting data from VICAR labels conforming to Voyager label format
  conventions.

   To utilize ABLE77V2 the calling program must first open the data set in
  question. Once a data set has been properly opened, its labels may be
  scanned by calling ABLE77V2. The subroutine reads each label and extracts
  data items, and fills ARR with the data as shown in TABLE 1. Upon return
  the indicator IND is set, and the data set is left "open".

   If, while performing label processing, ABLE77V2 is unable to successfully
  extract a data item, it fills the corresponding array element with a "not
  found" code of -999 for numeric items, or "!" for alphanumeric items.

				TABLE 1

	     1      =3 for a valid Voyager label, =0 otherwise
             2      GIVES AN INTEGER FDS COUNT : THE REAL VALUE MULTIPLIED
                     BY 100
             3      EXPOSURE
             4      FILTER POSITION
             5      SCAN RATE
             6      CAMERA SERIAL NUMBER,IE
                     FOR VOYAGER 2,  WIDE ANGLE,CAMERA S/N IS 4
                     FOR VOYAGER 2,NARROW ANGLE,CAMERA S/N IS 5
                     FOR VOYAGER 1,  WIDE ANGLE,CAMERA S/N IS 6
                     FOR VOYAGER 1,NARROW ANGLE,CAMERA S/N IS 7
             7       CAMERA 1=NA 2=WA
             8       GAIN 1=HI 0=LO
            10       EVENT YEAR
            11       EVENT DAY
	    12       EVENT HOUR   
	    13       EVENT MINUTE  
	    14       EVENT SECOND   
	    19       S/C ID
	    20       PICNO
	    29       INPUT TAPE
	    31       OUTPUT TAPE
	    33       INPUT FILE - 1
	    34       OUTPUT FILE   
	    35       ERT YEAR   
	    36       ERT DAY
	    37       ERT HOUR
	    38       ERT MINUTE
	    39       ERT SECOND

2 Arguments

  	IND:    is an output integer, upon return, indicates how processing was
		terminated:
		a)IND<0 indicates that at least one label item is in error.
		b)IND=0 indicates successful completion of processing.

	UNIT:   is an integer specifying the VICAR file unit number
		to be processed.

	ARR:  is an array of at least 1 fullword to receive the extracted
		data. Upon entry, the first word of the array must contain
		an integer value specifying the length of the array in words.
		Upon, return, the array is filled as shown in Table 1.

2 restrictions

  ABLE77V2 checks the first eight bytes of the first non-system label for
  the Voyager identifier. If it is found, the subroutine proceeds with the
  label scan on the assumption that all labels required by that format actually
  appear. Since no checks are made, unpredictable results can be expected if
  this constraint is not met.

2 History

 The function of this subroutine is a subset of that of the JPL routine
 ABLE77, vis.the extraction of certain information from standard VICAR
 labels.  Gary Yagi of JPL once wrote:
  "ABLE77 was written by a graduate student (since departed) who was
  intrigued by obscure languages.  Nobody here understands the program well
  enough to modify it.  I recommend that you re-write the routine from
  scratch."
 This is what I have done.  (Nick Marriage)

 Original Programmer: Nick Marriage			30  SEPT 1981
 Current Cognizant Programmer: Gary Yagi
 Source Language: Fortran
 Revisions:
  22 Mar 96  GMY  Changed FDS calculation from floating point to integer
                  to avoid round-off error (FR 89109)
  28-Jun-94 -TLT- Ported to alpha-vms and sun-solaris:
                  - revised unit test
                  - istat.gt.o changed to istat.gt.0
  29-Mar-94 -TLT- Fixed tstable77v2.pdf for FR83070.
                  Also: Corrected prnt mode for i/o tape and file in
                  table77v2.f.  Defaulted not found character data to !.
  17-Aug-93 -TLT- replace intdec and expoval with read
   3-Aug-93 -TLT- Ported to UNIX.  Dropped TBUF parameter.
   8-Aug-88 -PxZ- Increased the size of exposure value to 7 due to longer
                  "extended" exposure values in the label.
  25 AUG 88 -GMY- Correct documentation of return indicator, ARR(1).
  15-SEP-86 .FFM. MODIFY CODE TO HANDLE 10:1 SCAN RATE
  28-FEB-85 -JAM. CREATE ABLE77V2 (A VICAR2 VERSION OF ABLE77)
  16-FEB-85 -JAM. PUT IN CHECK OF INDICATOR FOR XLGETLABEL
   1-DEC-84 -JAM. USE XLGETLABEL 
   8 MAY 84 -HBD- ACCEPT ARR OF LENGTH 'SIZE' AND RETURN
	          ONLY INFORMATION REQUESTED AND NOTHING MORE
   5 APR 84 -HBD- REPLACE READ CALL WITH GLABEL CALL
                  ALSO RESTRUCTURED CODE
  27 APR 83 -JAM- ADD EXPOSURE TIME,SCAN RATE,GAIN 
                  CAMERA,HOUR,MINUTE,SECOND 
  29 APR 83 -JAM- RESTRUCTURE PROGRAM TO READ IN FIRST LABEL RECORD
                  AND GET MORE VALUESl
  Circa 1981....Original UCL VICAR source

$ Return
$!#############################################################################
