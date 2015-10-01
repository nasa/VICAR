$!****************************************************************************
$!
$! Build proc for MIPL module omc
$! VPACK Version 1.8, Thursday, March 02, 1995, 17:43:49
$!
$! Execute by entering:		$ @omc
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
$ write sys$output "*** module omc ***"
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
$ write sys$output "Invalid argument given to omc.com file -- ", primary
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
$   if F$SEARCH("omc.imake") .nes. ""
$   then
$      vimake omc
$      purge omc.bld
$   else
$      if F$SEARCH("omc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake omc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @omc.bld "STD"
$   else
$      @omc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create omc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack omc.com -
	-s omc.f -
	-i omc.imake -
	-p omc.pdf -
	-t tstomc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create omc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C---- VICAR/IBIS PROGRAM OMC
C     6 MAR 95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
	IMPLICIT NONE
	INTEGER	INUNIT, OUTUNIT, STATUS, COUNT, DEF
	INTEGER	CLEN, NCOL, NCOL2, INIBIS, OUTIBIS, INRECORD, OUTRECORD
	INTEGER	MECOL, OMCOL, CCOL, RSCOL, VRCOL
	INTEGER I, J, K, ROW, COL, COLS(15), FULLBUF(1000)
	REAL	REALBUF(1000)
	REAL*8	MEANGLES(3), OMANGLES(3), CANGLES(3)
	REAL*8	ME(3,3), OM(3,3), C(3,3), RS(3), VR(3)
        CHARACTER*16 ORG
        CHARACTER*6 FORMAT(1024)
        CHARACTER*256 CHARBUF(1000)
	LOGICAL	XVPTST, TOPLANET, ERR
	LOGICAL OMCOR
        COMMON/OMC/OMCOR
        EQUIVALENCE (FULLBUF,REALBUF)  !just to save memory, mutually excl.

        CALL IFMESSAGE('OMC version 6-MAR-95')

C	    Get the input parameters

C		First get where everything is in the IBIS columns
	CALL XVP ('RSCOL', RSCOL, COUNT)
	CALL XVP ('OMCOL', OMCOL, COUNT)
	CALL XVP ('MECOL', MECOL, COUNT)
	CALL XVP ('CCOL', CCOL, COUNT)
	CALL XVP ('VRCOL', VRCOL, COUNT)

C -- Copy all of the columns, even those not in the list.
C
C -- Note to those porting this: this trick won't work if NC > 40,
C -- or for IBIS files with non-numeric columns ! You will need
C -- to make the output file have the same format columns as the
C -- input, copy all of the other columns over, and then  process
C -- the columns listed above.
C
C	DO I = 0, 40
C	    COLS(I) = I
C            ROWDATA(I) = 0.0 ! pad with  0's
C	ENDDO

C		Then get the transformation type desired
	TOPLANET = XVPTST('TOPLANET')
	OMCOR = XVPTST('OMCOR')



C	    Multiply the VR vector by the transpose of the ME matrix to 
C	    get the RS vector, and multiply the ME matrix by the C matrix 
C	    to get the OM matrix:
C			__     t __
C			RS = ME *VR      OM = C*ME
C
C	    Multiply the RS vector by the ME matrix to get the VR vector,
C	    and multiply the transpose of the ME matrix by the OM matrix 
C	    to get the C matrix:
C			__      __               t
C			VR = ME*RS      C = OM*ME


        CALL XVUNIT(INUNIT, 'INP', 1, STATUS, ' ')
        CALL XVUNIT(OUTUNIT, 'OUT', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(INUNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
        CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)
        CALL IBIS_FILE_GET(INIBIS,'FORMATS',FORMAT,1,NCOL)
        CALL IBIS_FILE_GET(INIBIS,'ORG',ORG,1,1)


	CALL XVPARM ('NCOL', NCOL2, COUNT, DEF, 1)
	IF (DEF .EQ. 1) NCOL2 = NCOL
	CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL2,CLEN,
     *                      FORMAT,ORG,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        DO COL=1,NCOL
C skip columns that will be updated
           IF (COL.NE.MECOL.AND.COL.NE.OMCOL.AND.COL.NE.CCOL.AND.
     *         COL.NE.RSCOL.AND.COL.NE.VRCOL) THEN
C read into buffer
              IF (FORMAT(COL)(1:1).EQ.'A') THEN
                 CALL IBIS_COLUMN_READ(INIBIS, CHARBUF, COL, 1, CLEN,
     *                                 STATUS)
              ELSE IF (FORMAT(COL)(1:1).EQ.'F') THEN
                 CALL IBIS_COLUMN_READ(INIBIS, FULLBUF, COL, 1, CLEN,
     *                                 STATUS)
              ELSE
                 CALL IBIS_COLUMN_READ(INIBIS, REALBUF, COL, 1, CLEN,
     *                                 STATUS)
              END IF
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
C write to output
              IF (FORMAT(COL)(1:1).EQ.'A') THEN
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, CHARBUF, COL, 1, CLEN,
     *                                  STATUS)
              ELSE IF (FORMAT(COL)(1:1).EQ.'F') THEN
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, FULLBUF, COL, 1, CLEN,
     *                                  STATUS)
              ELSE
                 CALL IBIS_COLUMN_WRITE(OUTIBIS, REALBUF, COL, 1, CLEN,
     *                                  STATUS)
              END IF
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
           END IF
        END DO

C setup which rows go in record
C 1  MECOL       4  OMCOL       7  CCOL       10  RSCOL       13  VRCOL
C 2  MECOL + 1   5  OMCOL + 1   8  CCOL + 1   11  RSCOL + 1   14  VRCOL + 1
C 3  MECOL + 2   6  OMCOL + 2   9  CCOL + 2   12  RSCOL + 2   15  VRCOL + 2

        DO I=1,3
           COLS(I)= MECOL+I-1
           COLS(I+3)= OMCOL+I-1
           COLS(I+6)= CCOL+I-1
           COLS(I+9)= RSCOL+I-1
           COLS(I+12)= VRCOL+I-1
        END DO


        CALL IBIS_RECORD_OPEN(INIBIS,INRECORD,'FORMAT:REAL',
     &                        COLS,15,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INUNIT,STATUS,1)
        CALL IBIS_RECORD_OPEN(OUTIBIS,OUTRECORD,'FORMAT:REAL',
     &                        COLS,15,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTUNIT,STATUS,1)
	DO ROW = 1, CLEN
	    CALL IBIS_RECORD_READ(INRECORD, REALBUF, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    MEANGLES(1) = DBLE(REALBUF(COLS(1)))
	    MEANGLES(2) = DBLE(REALBUF(COLS(2)))
	    MEANGLES(3) = DBLE(REALBUF(COLS(3)))
	    CALL FROMEULER (MEANGLES(1), MEANGLES(2), MEANGLES(3), ME)

	    IF (TOPLANET) THEN
		DO I = 1, 3
		    VR(I) = DBLE(REALBUF(COLS(13+I-1)))
		    CANGLES(I) = DBLE(REALBUF(COLS(7+I-1)))
		ENDDO
		CALL FROMEULER (CANGLES(1), CANGLES(2), CANGLES(3), C)
		DO I = 1, 3
		    RS(I) = 0.0D0
		    DO J = 1, 3
			OM(I,J) = 0.0D0
			RS(I) = RS(I) + ME(J,I)*VR(J)
			DO K = 1, 3
			    OM(I,J) = OM(I,J) + C(I,K)*ME(K,J)
			ENDDO
		    ENDDO
		ENDDO
		CALL TOEULER(OM,OMANGLES(1),OMANGLES(2),OMANGLES(3),ERR)
		IF (ERR)  CALL MABEND ('ILLEGAL ROTATION MATRIX')
		DO I = 1, 3
		    REALBUF(COLS(10+I-1)) = SNGL(RS(I))
		    REALBUF(COLS(4+I-1)) = SNGL(OMANGLES(I))
		ENDDO

	    ELSE

		DO I = 1, 3
		    RS(I) = DBLE(REALBUF(COLS(10+I-1)))
		    OMANGLES(I) = DBLE(REALBUF(COLS(4+I-1)))
		ENDDO
		CALL FROMEULER (OMANGLES(1),OMANGLES(2),OMANGLES(3),OM) 
		DO I = 1, 3
		    VR(I) = 0.0D0
		    DO J = 1, 3
			C(I,J) = 0.0D0
			VR(I) = VR(I) + ME(I,J)*RS(J)
			DO K = 1, 3
			    C(I,J) = C(I,J) + OM(I,K)*ME(J,K)
			ENDDO
		    ENDDO
		ENDDO
		CALL TOEULER (C,CANGLES(1), CANGLES(2), CANGLES(3),ERR)
		IF (ERR)  CALL MABEND ('ILLEGAL ROTATION MATRIX')
		DO I = 1, 3
		    REALBUF(COLS(13+I-1)) = SNGL(VR(I))
		    REALBUF(COLS(7+I-1)) = SNGL(CANGLES(I))
		ENDDO

	    ENDIF
	    CALL IBIS_RECORD_WRITE(OUTRECORD, REALBUF, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	ENDDO

	CALL IBIS_FILE_CLOSE(INIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
	CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

	RETURN
	END



	subroutine FromEuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! Input  - RA of z axis (degrees)
	real*8	delta	    ! Input  - Declination z axis (degrees)
	real*8	kappa	    ! Input  - rotation angle around z axis
 			    !          (3rd Euler angle) (degrees)
	real*8	c(3,3)      ! Output - Derived rotation matrix 
	real*8	cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8	cos_kappa, sin_kappa, d2r
	LOGICAL OMCOR
        COMMON/OMC/OMCOR

c  This routine performs the functional inverse of routine ToEuler.  The
c  three Euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  The 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

        d2r = dacos(-1.0d0)/180.0d0   ! degree to radian conversion PI/180
	sin_alpha = dsin(alpha*d2r)    ! dsin() is dble(sin(radians)
	cos_alpha = dcos(alpha*d2r)
	sin_delta = dsin(delta*d2r)
	cos_delta = dcos(delta*d2r)
	sin_kappa = dsin(kappa*d2r)
	cos_kappa = dcos(kappa*d2r)

	if (OMCOR) then !Use OMCOR euler angles
	c(1,1)= -sin_alpha * cos_kappa - cos_alpha * sin_delta*sin_kappa
	c(1,2)=  cos_alpha * cos_kappa - sin_alpha * sin_delta*sin_kappa
	c(1,3)=  cos_delta * sin_kappa
	c(2,1)=  sin_alpha * sin_kappa - cos_alpha * sin_delta*cos_kappa
	c(2,2)= -cos_alpha * sin_kappa - sin_alpha * sin_delta*cos_kappa
	c(2,3)=  cos_delta * cos_kappa
	c(3,1)=  cos_alpha * cos_delta
	c(3,2)=  sin_alpha * cos_delta
	c(3,3)=  sin_delta
	else
	c(1,1)=  cos_alpha * cos_kappa - sin_alpha * cos_delta*sin_kappa
	c(1,2)=  sin_alpha * cos_kappa + cos_alpha * cos_delta*sin_kappa
	c(1,3)=  sin_delta * sin_kappa
	c(2,1)= -cos_alpha * sin_kappa - sin_alpha * cos_delta*cos_kappa
	c(2,2)= -sin_alpha * sin_kappa + cos_alpha * cos_delta*cos_kappa
	c(2,3)=  sin_delta * cos_kappa
	c(3,1)=  sin_alpha * sin_delta
	c(3,2)= -cos_alpha * sin_delta
	c(3,3)=  cos_delta
	endif

	return
	end




	subroutine ToEuler (c, alpha, delta, kappa, error)
	implicit none
	real*8	c(3,3)      ! Output - Derived rotation matrix 
	real*8	alpha       ! Input  - RA of z axis (degrees)
	real*8	delta	    ! Input  - Declination z axis (degrees)
	real*8	kappa	    ! Input  - rotation angle around z axis
 			    !          (3rd Euler angle) (degrees)
	real*8	collength, rowlength,d2r,r2d
	integer i, j
	logical	error, OMCOR
        COMMON/OMC/OMCOR

c  This routine performs the functional inverse of routine FromEuler.
c  This routine takes an input rotation matrix, and computes the three Euler
c  angles representing the matrix.  (These 3 angles are called Alpha, Delta,
c  and Kappa by Mert Davies etc.)  If the matrix is not a valid rotation
c  matrix (i.e. if the length of the row and column vectors is not within
c  0.0001 of unity) then error is returned true.
c
c  The 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

        d2r = dacos(-1.0d0)/180.0d0   ! degree to radian conversion PI/180
        r2d = 180.0d0/dacos(-1.0d0)   ! degree to radian conversion PI/180

	error = .false.
	do i = 1, 3
	    collength = 0.0d0
	    rowlength = 0.0d0
	    do j = 1, 3
		collength = collength + c(i,j)**2
		rowlength = rowlength + c(j,i)**2
	    enddo
	    if (abs(collength-1.0) .gt. 0.0001) error = .true.
	    if (abs(rowlength-1.0) .gt. 0.0001) error = .true.
	enddo

	if (.not. error) then
	    if (OMCOR) then !Use OMCOR euler angles
		delta = dasin(c(3,3))*r2d
		if (dabs(c(3,1)) .gt. 1e-10) then
			alpha = datan2(c(3,2), c(3,1))*r2d
		else if (dabs(c(3,2)) .lt. 1e-10) then
			alpha = 0.0d0
		else
			alpha = sign(dble(90.0),c(3,2))
		endif
		if (alpha .lt. 0.0) alpha = alpha + 360.0
		if (dabs(c(2,3)) .gt. 1e-10) then
			kappa = datan2(c(1,3), c(2,3))*r2d
		else if (dabs(c(1,3)) .lt. 1e-10) then
			kappa = datan2( -c(1,1), -c(2,1) )*r2d
		else
			kappa = sign(dble(90.0),c(1,3))
		endif
		if (kappa .lt. 0.0) kappa = kappa + 360.0
		if (abs(dcos(delta*d2r)) * dcos(kappa*d2r) - c(2,3) 
     +		   .gt. 0.0001) kappa = 180.0 - kappa
	    else !new method
		delta = dacos(c(3,3))*r2d
		if (dabs(c(3,2)) .lt. 1e-10 .and. 
     +              dabs(c(3,1)) .lt. 1e-10) then
			alpha = 0.0
		else !atan2 well-defined
			alpha = datan2(c(3,1), -c(3,2))*r2d
		endif
		if (alpha .lt. 0.0) alpha = alpha + 360.0
		if (dabs(c(1,3)) .lt. 1e-10 .and. 
     +              dabs(c(2,3)) .lt. 1e-10) then
		    kappa = (datan2(c(1,2), c(1,1))*r2d - alpha)*c(3,3)
		else
			kappa = datan2(c(1,3), c(2,3))*r2d
		endif
		if (kappa .lt. 0.0) kappa = kappa + 360.0
		endif
	endif

	return
	end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create omc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM omc

   To Create the build file give the command:

		$ vimake omc			(VMS)
   or
		% vimake omc			(Unix)


************************************************************************/


#define PROGRAM	omc
#define R2LIB

#define MODULE_LIST omc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create omc.pdf
PROCESS HELP=*
PARM INP       TYPE=STRING
PARM OUT       TYPE=STRING
PARM NCOL      TYPE=INTEGER DEFAULT=0
PARM MECOL     TYPE=INTEGER 
PARM OMCOL     TYPE=INTEGER 
PARM CCOL      TYPE=INTEGER 
PARM RSCOL     TYPE=INTEGER 
PARM VRCOL     TYPE=INTEGER 
PARM MODE      TYPE=KEYWORD VALID=(FROMPLAN,TOPLANET) DEFAULT=TOPLANET
PARM ANGLES    TYPE=KEYWORD VALID=(OMCOR,STANDARD) DEFAULT=OMCOR
END-PROC
.TITLE
VICAR/IBIS Program "omc"
.HELP
PURPOSE

    "omc" converts camera pointing matrices (C matrices) and spacecraft 
vectors between Earth coordinates and planet coordinates.  The data are
in columns in an IBIS tabular file.  The rotation matrices are stored as
the three corresponding Eulerian angles (in degrees).


EXECUTION

  omc  A.INT B.INT  MECOL=11 RSCOL=5 OMCOL=8 VRCOL=14 CCOL=17 'FROMPLAN

  omc  B.INT C.INT  MECOL=11 VRCOL=14 CCOL=17 RSCOL=20 OMCOL=23 'TOPLANET



OPERATION

Note: all angles mentioned are measured in degrees.

    For each row in the IBIS tabular file the transformation is applied
to the data in the columns.  First the planet angles (ME angles) are 
converted into the 3 by 3 ME rotation matrix.  Depending on the transformation
direction the C angles or the OM angles (camera angles) are converted into
a rotation matrix.  Then to transform from Earth coordinates to planet
coordinates we multiply the VR vector by the transpose of the ME matrix to 
get the RS vector, and multiply the ME matrix by the C matrix to get 
the OM matrix:
		__     t __
		RS = ME *VR      OM = C*ME

Or, to transform from planet coordinates to Earth coordinates we multiply 
the RS vector by the ME matrix to get the VR vector, and multiply the 
transpose of the ME matrix by the OM matrix to get the C matrix:
		__      __               t
		VR = ME*RS      C = OM*ME

Finally the C or OM matrix is converted back into Eulerian angles, and 
the transformed spacecraft vector and camera pointing is stored in the 
output file.

    There are many ways to define the three Eulerian angles for a rotation
matrix.  The default definition used in this program of the Eulerian angles
is given implicitly by the following equations:

         |  1   4   7  |     | c(1,1)  c(1,2)  c(1,3) |
         |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
         |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |

    c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta * sin_kappa
    c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta * sin_kappa
    c(1,3) =  cos_delta * sin_kappa
    c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta * cos_kappa
    c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta * cos_kappa
    c(2,3) =  cos_delta * cos_kappa
    c(3,1) =  cos_alpha * cos_delta
    c(3,2) =  sin_alpha * cos_delta
    c(3,3) =  sin_delta

    where alpha is the first angle, delta is the second angle, and 
kappa is the third angle. This is the same method that is used in
"omcor", and "omcor2". The geometric interpretation of the rotation
based on alpha,delta and kappa is a bit nonstandard and is as follows:

  0) Start with standard right-handed coordinates x,y,z
  1) Rotate the z-y plane about the x-axis clockwise, by an
     an angle of 90-<delta>, forming x,y',z' (so that z' is
     at an angle of <delta away from y).
  2) Rotate the x,y',z' coordinate axes about the old z-axis,
     clockwise, by an angle of 90-<alpha>, forming x',y'',Z
  3) Rotate the x',y'' axis counter-clockwise about the Z axis,
     by an angle of <kappa>+180, forming axes X,Y
  4) The axes (X,Y,Z) form the new coordinate system.

It is nonstandard in that angles (0,0,0) effectively transform
(x,y,z) into (y,x,z), whereas normally this should be (x,y,z).
The system still works, however, since the rule is internally
consistent, and the TOPLANET conversion properly inverts the
FROMPLANET system. The identity matrix is (a,d,k) =(90,90,180).

.page

If the keyword 'STANDARD is invoked, a more common. easily remembered,
system of euler angles may be used, with the following interpretation:

  0) Start with x,y,z as before
  1) Rotate the x,y,z   system counter-clockwise about the x-axis
     *by* an angle of <delta>, forming x,y',z'
  2) Rotate the x,y',z' system counter-clockwise about the z-axis 
     *by* an angle of <alpha>, forming x',y'',Z
  3) Rotate the x',y'',Z  system counter-clockwise about the Z-axis
     *by* an angle of <kappa, forming X,Y,Z.
  4) The axes (X,Y,Z) form the new coordinate system.

The matrix used for 'STANDARD is as follows:

	c(1,1) =  cos_alpha * cos_kappa - sin_alpha * cos_delta * sin_kappa
	c(1,2) =  sin_alpha * cos_kappa + cos_alpha * cos_delta * sin_kappa
	c(1,3) =  sin_delta * sin_kappa
	c(2,1) = -cos_alpha * sin_kappa - sin_alpha * cos_delta * cos_kappa
	c(2,2) = -sin_alpha * sin_kappa + cos_alpha * cos_delta * cos_kappa
	c(2,3) =  sin_delta * cos_kappa
	c(3,1) =  sin_alpha * sin_delta
	c(3,2) = -cos_alpha * sin_delta
	c(3,3) =  cos_delta

.page.

Original Programmer:  Frank Evans	June 1987

Cognizant Programmer: Niles Ritter

Documentation Author: Niles Ritter

Revision History:
      Rev. A    Frank Evans		June 1987
                Original Version
      Rev. B    Niles Ritter		Jan. 1995
                Added STANDARD Euler Angles, Test Procs.
      Rev. C    A Scop (CRI)            Mar. 1995
                Made portable for UNIX

PERFORMANCE

The unported version of "omc" is 28% faster than the ported version due to
the extensive changes to IBIS-2.  The old program read in each line in its
entirety, changed the desired columns, and then wrote the new data out to
the new file.  Due to the IBIS-2 changes, the new version must first copy
all the uninvolved columns to the output file and then work with a record
consisting of only the columns involved.  Also whereas the unported version
was limited to 40 columns, the ported version is not. 

.LEVEL1
.VARIABLE INP
The input IBIS tabular file.
.VARIABLE OUT
The output IBIS tabular file.
.VARIABLE NCOL
The number of columns in the 
output tabular file.
(default - same as input).
.VARIABLE MECOL
The starting column for the 
ME matrix angles (degrees).
.VARIABLE OMCOL
The starting column for the 
OM matrix angles (degrees).
.VARIABLE CCOL
The starting column for the 
C matrix angles (degrees).
.VARIABLE RSCOL
The starting column for the 
RS vector.
.VARIABLE VRCOL
The starting column for the 
VR vector.
.VARIABLE MODE
'TOPLANET to convert from
C and VR to OM and RS.
'FROMPLAN to convert from
OM and RS to C and VR.
.VARIABLE ANGLES
Use OMCOR style angles?
.LEVEL2
.VARIABLE INP
The input IBIS tabular file containing the planet angles, camera angles,
and the spacecraft vector.
.VARIABLE OUT
The output IBIS tabular file containing the original tabular data with
the transformed camera angles and spacecraft vector laid in.
.VARIABLE NCOL
The number of columns in the output tabular file. The default is to have
the same number of columns as the input file.
.VARIABLE MECOL
The starting column for the three ME matrix angles.  The ME matrix is a
rotation matrix that transforms from planet coordinates to Earth coordinates.
These Eulerian angles may be called the planet angles. 	(ME = "Moon Earth")
.VARIABLE OMCOL
The starting column for the three OM matrix angles.  The OM matrix is a
rotation matrix that transforms from planet coordinates to camera coordinates.
(OM = "Orbiter-Moon")
.VARIABLE CCOL
The starting column for the three C matrix angles.  The C matrix is a
rotation matrix that transforms from Earth coordinates to camera coordinates.
These Eulerian angles may be called the camera angles. (C = Camera)
.VARIABLE RSCOL
The starting column for the three RS vector components.  The RS vector is
the spacecraft vector expressed in planet coordinates. 
.VARIABLE VRCOL
The starting column for the three VR vector components.  The VR vector is
the spacecraft vector expressed in Earth coordinates.
.VARIABLE MODE
MODE is a keyword parameter that specifies the direction of the transformation.
MODE=TOPLANET converts from Earth coordinates to planet coordinates 
(C and VR to OM and RS).  MODE=FROMPLAN converts from planet coordinates to 
Earth coordinates (OM and RS to C and VR).
.VARIABLE ANGLES
There are many systems for specifying "euler angles" which are used
to describe the orientation of a body in 3D with respect to fixed
x,y,z coordinates. The default method in "omc" is the same method that
is used in "omcor", and "omcor2". The geometric interpretation of the rotation
based on the angles alpha,delta and kappa is a bit nonstandard and is 
as follows:

  0) Start with standard right-handed coordinates x,y,z
  1) Rotate the z-y plane about the x-axis clockwise, by an
     an angle of 90-<delta>, forming x,y',z' (so that z' is
     at an angle of <delta away from y).
  2) Rotate the x,y',z' coordinate axes about the old z-axis,
     clockwise, by an angle of 90-<alpha>, forming x',y'',Z
  3) Rotate the x',y'' axis counter-clockwise about the Z axis,
     by an angle of <kappa>+180, forming axes X,Y
  4) The axes (X,Y,Z) form the new coordinate system.

It is nonstandard in that angles (0,0,0) effectively transform
(x,y,z) into (y,x,z), whereas normally this should be (x,y,z).
The system still works, however, since the rule is internally
consistent, and the TOPLANET conversion properly inverts the
FROMPLANET system. The identity matrix is (a,d,k) =(90,90,180).

.page

If the keyword 'STANDARD is invoked, a more common. easily remembered,
system of euler angles may be used, with the following interpretation:

  0) Start with x,y,z as before
  1) Rotate the x,y,z   system counter-clockwise about the x-axis
     *by* an angle of <delta>, forming x,y',z'
  2) Rotate the x,y',z' system counter-clockwise about the z-axis 
     *by* an angle of <alpha>, forming x',y'',Z
  3) Rotate the x',y'',Z  system counter-clockwise about the Z-axis
     *by* an angle of <kappa, forming X,Y,Z.
  4) The axes (X,Y,Z) form the new coordinate system.

In this system the euler angles (0,0,0) take (x,y,z) to (x,y,z).

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstomc.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
ibis-gen testin nc=21 nr=5 datacol=(1,2,3,4,5,6,7,8,9) +
  data=(  + ! ME angles  , OM angles  RS Planet->Camera
  	     90,90,180,   0,0,0,     1,2,3, +
  	     90,90, 90,   0,90,0,    1,2,3, +
  	     0,0,0,       0,45,0,    1,2,3, +
  	     30,45,90,    0,45,30,   1,2,3, +
  	     20,10,100,   5,8,3,     1,2,3 )

 ibis-list testin nc=9 csiz=8
 
 omc  testin testout MECOL=1 OMCOL=4 RSCOL=7 CCOL=10 VRCOL=13 'FROMPLAN
 ibis-list testout nc=9 csiz=8
 ibis-list testout cols=(10,11,12,13,14,15) csize=12 'nohead +
 preamble=(" ","|          Camera Angles            |           VR Vector          |")

!Test inverse operation:
 omc   testout testin  MECOL=1 CCOL=10 VRCOL=13 OMCOL=16 RSCOL=19 NCOL=21 'TOPLAN
 ibis-list testin cols=(4,16,5,17,6,18) csize=10 'nohead +
  preamble=(" ","|-- These pairs of columns should not differ (modulo 360) |")
 ibis-list testin cols=(7,19,8,20,9,21) csize=10 'nohead +
  preamble=(" ","|-- These pairs of columns should not differ at all       |")

! Test new STANDARD option:
ibis-gen testin nc=21 nr=5 datacol=(1,2,3,4,5,6,7,8,9) +
  data=(  + ! ME angles  , OM angles  RS Planet->Camera
  	     0,0,0,       0,0,0,     1,2,3, +
  	     0,0,90,      0,90,0,    1,2,3, +
  	     0,0,0,       0,45,0,    1,2,3, +
  	     30,45,90,    0,45,30,   1,2,3, +
  	     20,10,100,   5,8,3,     1,2,3 )

 ibis-list testin nc=9 csiz=8
 
 omc  testin testout MECOL=1 OMCOL=4 RSCOL=7 CCOL=10 +
      VRCOL=13 'FROMPLAN 'STANDARD
 ibis-list testout nc=9 csiz=8
 ibis-list testout cols=(10,11,12,13,14,15) csize=12 'nohead +
 preamble=(" ","|           Camera Angles            |            VR Vector          |")

!Test inverse operation:
 omc   testout testin  MECOL=1 CCOL=10 VRCOL=13 OMCOL=16 +
      RSCOL=19 NCOL=21 'TOPLAN  'STANDARD
 ibis-list testin cols=(4,16,5,17,6,18) csize=10 'nohead +
  preamble=(" ","|-- These pairs of columns should not differ (modulo 360)  |")
 ibis-list testin cols=(7,19,8,20,9,21) csize=10 'nohead +
  preamble=(" ","|-- These pairs of columns should not differ at all        |")

end-proc
$ Return
$!#############################################################################
