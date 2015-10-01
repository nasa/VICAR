$!****************************************************************************
$!
$! Build proc for MIPL module itiepoint
$! VPACK Version 1.9, Monday, December 07, 2009, 16:24:58
$!
$! Execute by entering:		$ @itiepoint
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
$ write sys$output "*** module itiepoint ***"
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
$ write sys$output "Invalid argument given to itiepoint.com file -- ", primary
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
$   if F$SEARCH("itiepoint.imake") .nes. ""
$   then
$      vimake itiepoint
$      purge itiepoint.bld
$   else
$      if F$SEARCH("itiepoint.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake itiepoint
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @itiepoint.bld "STD"
$   else
$      @itiepoint.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create itiepoint.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack itiepoint.com -mixed -
	-s itiepoint.f zitiepoint.c -
	-i itiepoint.imake -
	-t titiepoint.f tzitiepoint.c titiepoint.imake titiepoint.pdf -
	   tstitiepoint.pdf -
	-o itiepoint.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create itiepoint.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE IWRITE_TIEPOINTS(UNIT,NAH,NAV,NPTS,TIEPOINTS,COLCOUNT)
C#######################################################################
C  NAME OF ROUTINE
C      IWRITE_TIEPOINTS (Ibis WRITE TIEPOINTS)
C  PURPOSE
C      IWRITE_TIEPOINTS writes the tiepoint data to an IBIS2 file.
C      This includes opening and closing the file.
C
C    In the most common case,
C      the TIEPOINTS array defines a geometric transformation which is applied
C      to the input image to produce the output image.  The transformation is 
C      defined via a grid of tiepoints.  The convention (see HELP for programs
C      GEOMA and TIEPARM) is that there are NAV+1 rows of tiepoints in the grid,
C      with NAH+1 tiepoints in each row.  ("row" here means a horizontally 
C      arranged set of points in an image.)  NPTS is 0 in this case.
C      
C      The IBIS tiepoint file will have one record (IBIS row) per tiepoint.
C      For the case of COLCOUNT = 4, IBIS columns 1 and 2 will contain the line
C      sample coordinates of the tiepoint in the output image.  IBIS columns
C      3 and 4 will contain the line and sample coordinates of the tiepoint in
C      the input image.
C      
C    In the other case the tiepoints are not constrained to form a grid.
C      In this case NAH and NAV are 0 and the number of points to be written
C      is specified in NPTS.
C  WRITTEN BY   
C      STEVE POHORSKY   JET PROPULSION LABORATORY        JULY 1995
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE
C      Standard subroutine call and return if no errors.  ABEND called if
C      an error occurs.  The calling routine should call XVUNIT before
C      this routine but should not open the file.
C
C  INPUT PARAMETERS     
C      (see the accompanying HELP file.)
C  REVISION HISTORY
C    JUL 95  SP  INITIAL RELEASE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INTEGER UNIT, NAH, NAV, NPTS, COLCOUNT
      REAL    TIEPOINTS(*)

      INTEGER IBIS, IRECORD, NUMPTS, STATUS, IROW, TINDX
      INTEGER INPUTCOLS(3), OUTPUTCOLS(3), TIECOLS(5)
      CHARACTER*20 SUBTYPE_NAME

      DATA INPUTCOLS/  3,4,0 /  ! see above under PURPOSE
      DATA OUTPUTCOLS/ 1,2,0 /
      DATA TIECOLS/    1,2,3,4,0 /

C=====START OF EXECUTABLE CODE=================================================

      IF (COLCOUNT .NE. 4)
     .   CALL MABEND('ERROR: Option not available in IWRITE_TIEPOINTS')

      IF (NAH*NAV .NE. 0)  THEN
          NUMPTS = (NAH+1) * (NAV+1)    ! GRIDDED SET OF TIEPOINTS.
      ELSE IF (NPTS .EQ. 0) THEN
          CALL MABEND('ERROR: NPTS or NAH*NAV = 0 in IWRITE_TIEPOINTS')
      ELSE
          NUMPTS = NPTS
      END IF

C.. Open IBIS tiepoint file for WRITE organized by (IBIS) ROW with
C.. COLCOUNT columns and NUMPTS (IBIS) rows.

      CALL IBIS_FILE_OPEN(UNIT,IBIS,'WRITE',COLCOUNT, NUMPTS,
     +                          ' ','ROW',STATUS)
       IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      SUBTYPE_NAME='TIEPOINT'
      CALL ibis_file_set(IBIS,'type',SUBTYPE_NAME,STATUS)
       IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL ICL_NEW_POS_IMAGE(IBIS, OUTPUTCOLS(1),OUTPUTCOLS(2),0,
     .                       ' ', STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL ICL_NEW_POS_IMAGE(IBIS, INPUTCOLS(1),INPUTCOLS(2),0,
     .                       ' ', STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      CALL ICL_NEW_POINT(IBIS,INPUTCOLS,2,0,0,'PIXEL','INPUT',STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL ICL_NEW_POINT(IBIS,OUTPUTCOLS,2,0,0,'PIXEL','OUTPUT',STATUS)
       IF (STATUS .LT. 0)   CALL IBIS_SIGNAL(IBIS,STATUS,1)

      IF (NAH*NAV .NE. 0)  THEN
        CALL XLADD(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',NAH,
     &           STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
         IF (STATUS .NE. 1)   
     &     CALL MABEND('ERROR from XLADD in IWRITE_TIEPOINTS')
       
        CALL XLADD(UNIT,'PROPERTY','NUMBER_OF_AREAS_VERTICAL',  NAV,
     &           STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
         IF (STATUS .NE. 1)   
     &     CALL MABEND('ERROR from XLADD in IWRITE_TIEPOINTS')
      END IF

      CALL IBIS_RECORD_OPEN(IBIS,IRECORD,' ',
     +                        TIECOLS,COLCOUNT,'NONE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      TINDX = 1

      DO IROW = 1, NUMPTS
	  CALL IBIS_RECORD_WRITE(IRECORD,TIEPOINTS(TINDX),IROW,STATUS)
          IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TINDX = TINDX + COLCOUNT
      ENDDO

      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      RETURN
      END

      SUBROUTINE IREAD_TIEPOINTS(UNIT,NAH,NAV,MAXPTS,TIEPOINTS,COLCOUNT)
C#######################################################################
C  NAME OF ROUTINE
C      IREAD_TIEPOINTS (Ibis READ TIEPOINTS)
C  PURPOSE
C      IREAD_TIEPOINTS reads the tiepoint data from an IBIS2 file.
C      This includes opening and closing the file.
C
C    In the most common case,
C      the IBIS2 file defines a geometric transformation which is applied
C      to the input image to produce the output image.  The transformation is 
C      defined via a grid of tiepoints.  The convention (see HELP for programs
C      GEOMA and TIEPARM) is that there are NAV+1 rows of tiepoints in the grid,
C      with NAH+1 tiepoints in each row.  ("row" here means a horizontally 
C      arranged set of points in an image.)  This case is distinguished by
C      the presence of NUMBER_OF_AREAS_HORIZONTAL and NUMBER_OF_AREAS_VERTICAL
C      (the NAH and NAV values) in the VICAR property label.
C      
C      The IBIS tiepoint file will have one record (IBIS row) per tiepoint.
C      For the case of COLCOUNT = 4, IBIS columns 1 and 2 will contain the line
C      sample coordinates of the tiepoint in the output image.  IBIS columns
C      3 and 4 will contain the line and sample coordinates of the tiepoint in
C      the input image.
C      
C    In the other case the tiepoints are not constrained to form a grid.
C      In this case NAH and NAV are set to 0 and the number of points to be 
C      read is the minimum of MAXPTS and the number of IBIS rows in the file.
C  WRITTEN BY   
C      STEVE POHORSKY   JET PROPULSION LABORATORY        JULY 1995
C  ENVIRONMENT
C      VMS or UNIX  with TAE/VICAR EXECUTIVE       FORTRAN-77
C     
C  CALLING SEQUENCE
C      Standard subroutine call and return if no errors.  ABEND called if
C      an error occurs.  The calling routine should call XVUNIT before
C      this routine but should not open the file.
C
C  INPUT PARAMETERS     
C      (see the accompanying HELP file.)
C  REVISION HISTORY
C    JUL 95  SP  INITIAL RELEASE
C    AUG 95  SP  Changed call to IBIS_RECORD_OPEN to use UFORMAT 'REAL'
C                instead of 'NONE' to handle files form other machine types.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT NONE

      INTEGER UNIT, NAH, NAV, MAXPTS, COLCOUNT
      REAL    TIEPOINTS(*)

      INTEGER IBIS, IRECORD, NUMPTS, STATUS, IROW, TINDX,CLEN,NCOL
      INTEGER TIECOLS(5)

      DATA TIECOLS/    1,2,3,4,0 /  ! see above under PURPOSE

C=====START OF EXECUTABLE CODE=================================================

      IF (COLCOUNT .NE. 4)
     .   CALL MABEND('ERROR: Option not available in IREAD_TIEPOINTS')

C.. Open IBIS tiepoint file for READ.

      CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
      CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)

      NAH = 0       ! 0 IF NOT IN LABEL.
      NAV = 0
      CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',
     &        NAH,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')
      CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_VERTICAL',
     &        NAV,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')

      IF (NAH*NAV .NE. 0 )   THEN
          NUMPTS = (NAH+1) * (NAV+1)    ! GRIDDED SET OF TIEPOINTS.
          NUMPTS = MIN( NUMPTS,MAXPTS)
      ELSE IF (MAXPTS .LE. 0) THEN
          CALL MABEND('ERROR:MAXPTS and NAH*NAV = 0 in IREAD_TIEPOINTS')
      ELSE
          NUMPTS = MIN(CLEN,MAXPTS)    ! READ WHOLE FILE UNLESS CLEN>MAXPTS.
          NAV    = CLEN                ! RETURN NUMBER OF POINTS IN FILE
          NAH    = 0                   ! INDICATES NON-GRIDDED SET.
      END IF

      IF (NCOL .LT. COLCOUNT)
     .  CALL MABEND('ERROR: COLCOUNT > file''s NCOL in IREAD_TIEPOINTS')
      IF (CLEN .LT. NUMPTS)
     .  CALL MABEND('ERROR: NUMPTS > file''s CLEN in IREAD_TIEPOINTS')

      CALL IBIS_RECORD_OPEN(IBIS,IRECORD,' ',
     +                        TIECOLS,COLCOUNT,'REAL',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      TINDX = 1

      DO IROW = 1, NUMPTS
	  CALL IBIS_RECORD_READ(IRECORD,TIEPOINTS(TINDX),IROW,STATUS)
          IF (STATUS .NE. 1)   CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  TINDX = TINDX + COLCOUNT
      ENDDO

      CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zitiepoint.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ziwrite_tiepoints - IBIS write tiepoints           */
/************************************************************************/

void ziwrite_tiepoints( unit,nah,nav,npts, tiepoints, colcount)
int unit, nah, nav, npts, colcount;
float *tiepoints;
{
FTN_NAME2_(iwrite_tiepoints, IWRITE_TIEPOINTS) ( &unit,&nah,&nav,&npts,
						tiepoints, &colcount);
}

/************************************************************************/
/* C-Callable Version: ziread_tiepoints - IBIS read tiepoints           */
/************************************************************************/

void ziread_tiepoints( unit,nah,nav,maxpts, tiepoints, colcount)
int unit, *nah, *nav, maxpts, colcount;
float *tiepoints;
{
FTN_NAME2_(iread_tiepoints, IREAD_TIEPOINTS) ( &unit,nah,nav,&maxpts,
						tiepoints, &colcount);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create itiepoint.imake
/* Imake file for VICAR subroutine ITIEPOINT */

#define SUBROUTINE itiepoint

#define MODULE_LIST itiepoint.f zitiepoint.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create titiepoint.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER NAH,NAH2, NAV,NAV2, NPTS,NPTS2, I,J,K, COLCOUNT, 
     .        STATUS, UNIT,UNIT2
      REAL LINE,SAMP, LINC, SINC, TIE1(100), TIE2(100)
      CHARACTER*132 PBUF
C==================================================================
      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR

C  TEST1 FOR ITIEPOINT : GRIDDED SET OF TIEPOINTS
      NAH = 2
      NAV = 3
      COLCOUNT = 4
      LINC = 10.0
      SINC = 5.0
      K = 1

      DO I=1,NAV+1
         IF (I .EQ. 1) THEN
             LINE = 40.0
         ELSE
             LINE = LINE + LINC
         END IF

         DO J=1,NAH+1
            IF (J .EQ. 1) THEN
             SAMP = 10.0
            ELSE
             SAMP = SAMP + SINC
            END IF

            TIE1(K)   = LINE
            TIE1(K+1) = SAMP
            TIE1(K+2) = LINE+1.5
            TIE1(K+3) = SAMP+2.5
            K         = K+4

         END DO
      END DO

      CALL IWRITE_TIEPOINTS(UNIT,NAH,NAV,0,TIE1,COLCOUNT)  !WRITE TO A FILE.

C...NOW READ THE TIEPOINT DATA FROM THE FILE INTO A DIFFERENT ARRAY.

      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ') ! DO NOT ASSUME UNIT IS STILL THERE.
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR
      NAV2 = 0
      NAH2 = 0
      CALL IREAD_TIEPOINTS(UNIT,NAH2,NAV2,100,TIE2,COLCOUNT)  !READ FROM A FILE.

C...NOW CHECK THAT NO DATA WAS LOST IN THE MOVE.

      IF (NAH .NE. NAH2)  CALL MABEND('ERROR in NAH2 value')
      IF (NAV .NE. NAV2)  CALL MABEND('ERROR in NAV2 value')

      K = 0
      DO I = 1, (NAH+1)*(NAV+1)
         IF ( TIE2(I) .NE. TIE1(I) ) THEN
            K = K+1
            WRITE (PBUF,9000) I, TIE2(I), TIE1(I)
9000        FORMAT ('ERROR on tiepoint value',I3, 2F10.6)
            CALL XVMESSAGE(PBUF,' ')
         END IF
      END DO

      IF (K .EQ. 0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 1',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 1',' ')
      END IF

C test 1a:  set maxpts to 1.

      TIE2(1) = 0.0
      TIE2(5) = 0.0
      CALL IREAD_TIEPOINTS(UNIT,NAH2,NAV2,1,TIE2,COLCOUNT)  !READ FROM A FILE.
      IF ( TIE2(1) .eq. TIE1(1) .and. TIE2(5) .EQ. 0.0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 1a',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 1a',' ')
      END IF

C  TEST2 FOR ITIEPOINT : NON-GRIDDED SET OF TIEPOINTS

      CALL XVUNIT(UNIT2,'OUT',2,STATUS,' ')
      CALL XVSIGNAL(UNIT2, STATUS, 1)     ! ABORT IF ERROR


      NPTS = 5
      COLCOUNT = 4
      LINC = 10.0
      SINC = 5.0
      K = 1

      DO I=1,NPTS
         IF (I .EQ. 1) THEN
             LINE = 4.0
             SAMP = 1.0
         ELSE
             LINE = LINE + LINC
             SAMP = SAMP + SINC
         END IF

         TIE1(K)   = LINE
         TIE1(K+1) = SAMP
         TIE1(K+2) = LINE+1.5
         TIE1(K+3) = SAMP+2.5
         K         = K+4

      END DO

      CALL IWRITE_TIEPOINTS(UNIT2,0,0,NPTS, TIE1,COLCOUNT)  !WRITE TO A FILE.

C...NOW READ THE TIEPOINT DATA FROM THE FILE INTO A DIFFERENT ARRAY.

      CALL XVUNIT(UNIT2,'OUT',2,STATUS,' ') ! DO NOT ASSUME UNIT IS STILL THERE.
      CALL XVSIGNAL(UNIT2, STATUS, 1)     ! ABORT IF ERROR
      NPTS2 = 0
      CALL IREAD_TIEPOINTS(UNIT2,NAH2,NPTS2,100,TIE2,COLCOUNT) !READ FROM A FILE

C...NOW CHECK THAT NO DATA WAS LOST IN THE MOVE.

      IF (NAH2 .NE. 0)    CALL MABEND('ERROR in NAH2 value')
      IF (NPTS .NE. NPTS2)  CALL MABEND('ERROR in NPTS2 value')

      K = 0
      DO I = 1, NPTS
         IF ( TIE2(I) .NE. TIE1(I) ) THEN
            K = K+1
            WRITE (PBUF,9000) I, TIE2(I), TIE1(I)
            CALL XVMESSAGE(PBUF,' ')
         END IF
      END DO

      IF (K .EQ. 0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 2',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 2',' ')
      END IF


      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface:', ' ')

      call tzitiepoint(TIE1,NPTS,TIE2 )

      return
      END

$!-----------------------------------------------------------------------------
$ create tzitiepoint.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzitiepoint)(tie1, npts, tie2) 
int   *npts;
float *tie1, *tie2;
{
      int unit2, nah2, npts2, i, k;
      char pbuf[81];
/*  ==================================================================  */

/*  TEST 2: non-gridded set of tiepoints  */

      zvunit(&unit2,"OUT",2, 0);

      ziwrite_tiepoints(unit2, 0,0, *npts, tie1, 4);  /*  write to a file  */

      zvunit(&unit2,"OUT",2, 0);

      npts2 = 0;
      ziread_tiepoints(unit2, &nah2,&npts2,100,tie2, 4);  /* read from a file.*/

/*  Now check that no data was lost in the move.  */

      if (nah2 != 0)
         zmabend("Error in nah2 value");

      if (npts2 != (*npts))
         zmabend("Error in npts2 value");

      k = 0;  /*  error count  */
      for (i=0; i < npts2; ++i)  {
          if (tie1[k] != tie2[k])  {
             ++k;
             sprintf( pbuf, "Error on tiepoint %d  %f  %f", i,tie2[i],tie1[i]);
             zvmessage(pbuf, "");
           }
       }

       if (k == 0)
          zvmessage("Success on test 2", "");
       else
          zvmessage("Failure on test 2", "");

}
$!-----------------------------------------------------------------------------
$ create titiepoint.imake
/* Imake file for Test of VICAR subroutine ITIEPOINT */

#define PROGRAM titiepoint

#define MODULE_LIST titiepoint.f tzitiepoint.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create titiepoint.pdf
PROCESS
PARM OUT TYPE=STRING COUNT=2
END-PROC
$!-----------------------------------------------------------------------------
$ create tstitiepoint.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
! 
titiepoint (a b)
!debug
ibis-list a
label-list a
ibis-list b
label-list b
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create itiepoint.hlp
1 ITIEPOINT

    Module ITIEPOINT contains subroutines for reading and writing IBIS-2
    tiepoint files:

      IREAD_TIEPOINTS reads the tiepoint data from an IBIS2 file.
      This includes opening and closing the file.

      IWRITE_TIEPOINTS writes the tiepoint data to an IBIS2 file.
      This includes opening and closing the file.

    In the most common case,
      the TIEPOINTS array defines a geometric transformation which is applied
      to the input image to produce the output image.  The transformation is 
      defined via a grid of tiepoints.  The convention (see HELP for programs
      GEOMA and TIEPARM) is that there are NAV+1 rows of tiepoints in the grid,
      with NAH+1 tiepoints in each row.  ("row" here means a horizontally 
      arranged set of points in an image.) 
      
    In the other case the tiepoints are not constrained to form a grid.
      In this case NAH and NAV are 0 and the number of points to be written
      is specified in NPTS.
    
    The IBIS tiepoint file will have one record (IBIS row) per tiepoint.
      For the case of COLCOUNT = 4, IBIS columns 1 and 2 will contain the line
      sample coordinates of the tiepoint in the output image.  IBIS columns
      3 and 4 will contain the line and sample coordinates of the tiepoint in
      the input image.
                4 IS THE ONLY VALUE CURRENTLY SUPPORTED FOR COLCOUNT.
      
2  CALLING SEQUENCE

  FORTRAN Calling Sequence:  CALL IREAD_TIEPOINTS(UNIT,NAH,NAV,MAXPTS,
                                                  TIEPOINTS,COLCOUNT)
  C Calling Sequence:        ziread_tiepoints(unit,&nah,&nav,maxpts,
                                              tiepoints,colcount);
    Note the & for nah and nav, denoting the passing by address.

  FORTRAN Calling Sequence:  CALL IWRITE_TIEPOINTS(UNIT,NAH,NAV,NPTS,
                                                   TIEPOINTS,COLCOUNT)
  C Calling Sequence:        ziwrite_tiepoints(unit,nah,nav,npts,
                                               tiepoints,colcount);

  For either routine, ABEND is called if an error occurs.  The calling routine 
  should call XVUNIT before either routine, but the file should be closed
  when either routine is called.

2  ARGUMENTS

  IREAD_TIEPOINTS

      UNIT        VICAR unit number of IBIS file.  (input, integer)
                  (unit, maxpts, and colcount are passed by value
                  for ziread_tiepoints.)

      NAH         For gridded sets of tiepoints, there are NAH+1 tiepoints for
                  each row of tiepoints in the grid.  For non-gridded sets of
                  tiepoints, NAH is set to 0.      (output, integer)
                  (nah and nav are passed by address for ziread_tiepoints.)
      NAV         For gridded sets of tiepoints, there are NAV+1
                  rows of tiepoints in the grid.  For non-gridded sets of
                  tiepoints, NAV is set to the number of tiepoints in the
                  file, independent of MAXPTS.     (output, integer)
      MAXPTS      The maximum number of tiepoints that will be read into
                  the TIEPOINTS array.  (There are COLCOUNT array elements
                  per tiepoint.)                   (input, integer)
      TIEPOINTS   The tiepoint values read from the IBIS file.
                  (There are COLCOUNT array elements per tiepoint.)     
                                                    (output, real array)
                  The declaration is
                  REAL TIEPOINTS(nnn), where nnn >= MAXPTS*COLCOUNT,
                  or for ziread_tiepoints,
                  float *tiepoints;

      COLCOUNT    The number of data values per tiepoint = the number of
                  IBIS columns per tiepoint.  In the TIEPOINTS array, there 
                  are COLCOUNT array elements
                  per tiepoint.)                   (input, integer)
                4 IS THE ONLY VALUE CURRENTLY SUPPORTED FOR COLCOUNT.

  IWRITE_TIEPOINTS

      UNIT        VICAR unit number of IBIS file.  (input, integer)
                  (unit, nah, nav, npts, and colcount are passed by value
                  for ziwrite_tiepoints.)

      NAH         For gridded sets of tiepoints, there are NAH+1 tiepoints for
                  each row of tiepoints in the grid.  For non-gridded sets of
                  tiepoints, NAH is set to 0.      (input, integer)
      NAV         For gridded sets of tiepoints, there are NAV+1
                  rows of tiepoints in the grid.  For non-gridded sets of
                  tiepoints, NAV is set to 0.      (input, integer)
      NPTS        The number of tiepoints that will be written from
                  the TIEPOINTS array.  (There are COLCOUNT array elements
                  per tiepoint.)                   (input, integer)
      TIEPOINTS   The tiepoint values written to the IBIS file.
                  (There are COLCOUNT array elements per tiepoint.)     
                                                    (input, real array)
                  The declaration is
                  REAL TIEPOINTS(nnn), where nnn >= NPTS*COLCOUNT,
                  or for ziwrite_tiepoints,
                  float *tiepoints;

      COLCOUNT    The number of data values per tiepoint = the number of
                  IBIS columns per tiepoint.  In the TIEPOINTS array, there 
                  are COLCOUNT array elements
                  per tiepoint.)                   (input, integer)
                4 IS THE ONLY VALUE CURRENTLY SUPPORTED FOR COLCOUNT.

2  HISTORY

      Original Programmer: Steve Pohorsky     July 1995
      Current Cognizant Programmer: Steve Pohorsky
      Source Language: Fortran

 Revision History
  7-95  ...SP.... Initial release is portable (VMS and UNIX).
 12-96  ...SP.... Added C bridges for initial routines.
2  OPERATION

 IREAD_TIEPOINTS tries to read from the property label the values for
 NUMBER_OF_AREAS_HORIZONTAL and NUMBER_OF_AREAS_VERTICAL.  If both are
 found, the routine assumes that the IBIS file holds a gridded set of
 tiepoints.  Otherwise the routine assumes the set is non-gridded, returning
 0 for NAH and the number of rows in the file for NAV.
 The initial release assumes that the tiepoint data is in IBIS columns 1-4.

 If NAH*NAV is not 0, IWRITE_TIEPOINTS assumes that the TIEPOINTS array holds 
 a gridded set of (NAH+1)*(NAV+1) tiepoints.  Otherwise the routine assumes 
 the set is a non-gridded set of NPTS tiepoints.  For gridded tiepoint sets, 
 the routine writes to the property label the values for
 NUMBER_OF_AREAS_HORIZONTAL and NUMBER_OF_AREAS_VERTICAL.  
 The initial release assumes that the tiepoint data is in IBIS columns 1-4.
$ Return
$!#############################################################################
