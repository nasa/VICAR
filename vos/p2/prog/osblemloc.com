$!****************************************************************************
$!
$! Build proc for MIPL module osblemloc
$! VPACK Version 1.9, Friday, March 19, 2010, 18:47:11
$!
$! Execute by entering:		$ @osblemloc
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
$ write sys$output "*** module osblemloc ***"
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
$ write sys$output "Invalid argument given to osblemloc.com file -- ", primary
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
$   if F$SEARCH("osblemloc.imake") .nes. ""
$   then
$      vimake osblemloc
$      purge osblemloc.bld
$   else
$      if F$SEARCH("osblemloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake osblemloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @osblemloc.bld "STD"
$   else
$      @osblemloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create osblemloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack osblemloc.com -mixed -
	-s osblemloc.f -
	-p osblemloc.pdf -
	-i osblemloc.imake -
	-t tstosblemloc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create osblemloc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C OSBLEMLOC --Converts image-space blemish location file to object-space
C
C          OSBLEMLOC ISBLEM OSBLEM
C
C ISBLEM and OSBLEM contain the following for each blemish:
C		blem(1,I) = index to reference reseau mark
C		blem(2,I) = line coordinate
C		blem(3,I) = sample coordinate
C		blem(4,I) = radius of blemish
c
c
c     May 1996 - ported.  BAM
c     03/2010  -lwk-  replaced testos() with xvhost()
c


      SUBROUTINE MAIN44
      COMMON/C1/BLEM(4,500),RES(2,203),OSRES(2,203),CONV(2216)
      INTEGER*4 CONV
      character*144 lab

      CALL XVMESSAGE('OSBLEMLOC version 19-Mar-2010',0)

      CALL XVUNIT(IUNIT,'INP',1,IND,0)
      CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',0)
C
C     ...Get camera S/N from blemish file label
      CALL XLGET(iunit,'HISTORY','CAMNUM',icam,ind,'FORMAT','INT',0)
      call xvsignal(iunit,ind,1)
      CALL XLGET(iunit,'HISTORY','NUMBLEM',nblem,ind2,'FORMAT','INT',0)
      call xvsignal(iunit,ind,1)
      IF (IND.NE.1.OR.IND2.NE.1) THEN
         CALL VIC1LAB(iunit,IND,blem,lab,10)
         call chkstat(ind,'Error reading blemish file header',
     -                 0,0,0)
         read (lab,90001) icam
90001    format (104x,i1)
         CALL XVGET(iunit,IND,'NS',nsb,0)
         nblem = NSB/4			!Get number of blemishes
      ENDIF
C
      CALL XVREAD(IUNIT,blem,ind,0)	!Read in the blemishes (blem)
      CALL GETRESLOC(ICAM,res,ind)	!Get nominal I.S. reslocs for camera
      CALL VGROS(ICAM,osres)		!Get nominal O.S. reslocs
      CALL GEOMAV(conv,ICAM,RES)	!Generate GEOMA parameters (CONV)
      NHOR = CONV(3) + 1
      NVER = CONV(6) + 1
C
C     ...Compute blemish coordinates by adding offsets to reseau coordinates
C     ...convert to object space, and recompute offsets.
      DO I=1,NBLEM
         M = BLEM(1,I)			!Get index of reference reseau mark
         RLINE = RES(1,M) + BLEM(2,I)	!Compute image-space coordinates
         RSAMP = RES(2,M) + BLEM(3,I)
C		Convert to object-space
         CALL TRITRA(ind,CONV(9),NHOR,NVER,rline,rsamp,1)
         BLEM(2,I) = RLINE - OSRES(1,M)	!Compute offsets
         BLEM(3,I) = RSAMP - OSRES(2,M)
      ENDDO
C
C     ...Write object-space offsets to output file
c
      CALL XVUNIT(OUNIT,'OUT',1,IND,0)
      CALL XVOPEN(OUNIT,IND,'OP','WRITE',
     -                'OPEN_ACT','SA','IO_ACT','SA',0)
      CALL XVWRIT(OUNIT,BLEM,IND,0)

c      close files
      call xvclose(iunit,istatus,0)
      call xvclose(ounit,istatus,0)

      RETURN
      END



C Get nominal reseau locations for Reseau Location File
C Input: ICAM
C Output: RES(2,202)
C
      SUBROUTINE GETRESLOC(ICAM,res,ind)
      REAL*4 res(2,202)
      integer * 4 ind

      COMMON/CRES/IFRM,JCAM,IFILT,IYEAR,IDAY

      CHARACTER*256 RFNAME



c for porting     BAM

! the below cludge is for byte swapping between alpha and unix boxes
! for alphas
      character*4 nom(4),nomu(4) 
      data NOM/'NOM4','NOM5','NOM6','NOM7'/
! for unix
      DATA nomu/'4MON','5MON','6MON','7MON'/
! better kludge:
      character*10 intfmt, realfmt

        integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status
        integer icount
        integer nrows


C
C     ...Find and open Reseau Location File

      CALL XVP('RES',rfname,ICNT)		!Get file name

      CALL XVUNIT(IUNITR,'X',1,IND,'U_NAME',RFNAME,0)

      call ibis_file_open(iunitr,ibis,'read',409,99999,
     -                        format,0,status)
      if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)

      icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
      if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)

                                  !Point to nominals for ICAM
c     call testos(ios)       ! check to swap bytes for UNIX
c     if (ios .eq. 0) then
c         call xvmessage('The OS is ALPHA',' ')
c         call mvcl(nom(icam-3),ifrm,4) ! change char to integer
c     else if (ios .eq. 1) then
c         call xvmessage('The OS is UNIX',' ')
c         call mvcl(nomu(icam-3),ifrm,4) ! change char to integer
c     end if
c  testos only knows about VMS & Unix ... replace with:

      call xvhost('NATIVE', intfmt, realfmt, status)
      if (intfmt.eq.'LOW') then
          call mvcl(nom(icam-3),ifrm,4) ! change char to integer
      else
          call mvcl(nomu(icam-3),ifrm,4) ! change char to integer
      endif

      jcam = icam
      call getlocV2(ibis,nrows,ifrm,res,ind)
      if ( ind .ne. 0 ) go to 992
      call ibis_file_close(ibis,0,status)
      return
C
C     ...Error conditions
  992 CALL XVMESSAGE(' ***Err reading nominal reseau locations',0)
      CALL ABEND
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create osblemloc.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=1
PARM OUT      TYPE=STRING  COUNT=1
PARM SIZE     TYPE=INTEGER COUNT=4      DEFAULT=(1,1,0,0)
PARM RES      TYPE=STRING  COUNT=0:1
END-PROC
.TITLE
OSBLEMLOC --Computes object-space blemish locations for Voyager cameras
.HELP
PURPOSE:

Given the image-space blemish locations for a Voyager camera, OSBLEMLOC
computes the object-space blemish locations for the camera.

EXECUTION STATEMENT:

      OSBLEMLOC  INP=ISBLEM  OUT=OSBLEM  RES=RESEAU user-parameters...

where ISBLEM is an input blemish location file containing the image-space
blemish locations, OSBLEM is an output blemish location file which will
contain the corresponding object-space blemish locations, and RESEAU is the
input RESEAU file.  Normally, the RESEAU file to be used will be 
wms_vgr:[000000]cresj.fil.
.page
OPERATION:

OSBLEMLOC requires the following inputs:

	1) The image-space blemish locations for a given camera.

	2) A Voyager Reseau Location File (for any of the planets).

The image-space blemish locations are input via a blemish location file
(ISBLEM).

The Reseau Location File must be specified via the RES parameter.  

OSBLEMLOC performs the following:

    1) The camera serial number is extracted from the label of the
       input blemish location file.

    2) The nominal image-space reseau locations for the camera are retrieved
       from the Reseau Location File.

    3) The object-space reseau locations for the camera are retrieved from
       tables internal to the program.

    4) The geometric correction parameters for converting from image-space
       to object space are computed (via a call to GEOMAV).

    5) The offsets from the blemish location file, the image-space
       coordinates for each blemish is computed.

    6) Using the geometric correction parameters, the corresponding object-space
       coordinates are computed (via calls to TRITRA).

    7) The blemish offsets in object-space are computed.

.page
BLEMISH LOCATION FILE FORMAT

The format of the input and output blemish files are identical.  They contain
a single data record.  Each blemish is defined in this record via four (conti-
guous) values:

	1) number of nearest reseau (REAL*4)
	2) line-offset from the reseau (REAL*4)
	3) sample-offset from the reseau (REAL*4)
	4) blemish radius in pixels (REAL*4)

.page
PROGRAM HISTORY:

WRITTEN BY: Gary Yagi 			Feb 22, 1988
COGNIZANT PROGRAMMER: Gary Yagi
REVISIONS:
 Jul 2  96  BAM  ...Ported; replaced old reseau file calls;
		    deleted PCA from tstpdf; pointed to new
		    default reseau file.
 Nov 27 91  FFM  ...Replace UD2 & UD4 with VGR (FR 64506).
                    Add PCA.
 Sep 12 89  GMY  ...Handle new BLEMFIX format labels.
 Jun 28 88  GMY  ...Correct conversion from IS-to-OS.

.LEVEL1
.VARIABLE INP
STRING--REQUIRED
The input blemish
location file
.VARIABLE OUT
STRING--REQUIRED
The output blemish
location file
.VARIABLE SIZE
4 INTEGERS--OPTIONAL
VICAR size field
(ignored)
.VARIABLE RES
STRING--OPTIONAL
Reseau Location File.
.LEVEL2
.VARIABLE RES
STRING--OPTIONAL
Specifies the Voyager Reseau Location File 
 - usually wms_vgr:[000000]cresj.fil.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create osblemloc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM osblemloc

   To Create the build file give the command:

		$ vimake osblemloc			(VMS)
   or
		% vimake osblemloc			(Unix)


************************************************************************/


#define PROGRAM	osblemloc
#define R2LIB

#define MODULE_LIST osblemloc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport


#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77

/*#define DEBUG		/* remove on delivery */

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstosblemloc.pdf
!TEST OF OSBLEMLOC
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
local PATH string
#local PATH string init="wms_test_work:[testdata.vgr]"
#local PATH1 string init="WMS_VGR:[000000]"
let _onfail="continue"
let $echo="no"
#if ($syschar(1) = "UNIX")
  let PATH="/project/test_work/testdata/vgr/"
#  let PATH1="/project/vgr/"
#end-if
#osblemloc &"PATH"blemloc.wa out=BLEM res=&"PATH1"cresj.fil
# (17Mar2010 - LWK) cresj.fil is no longer on the system, replace with
# cresn.fil (Neptune file, instead of Jupiter)

osblemloc &"PATH"blemloc.wa out=BLEM res=&"PATH"cresn.fil
LABEL-LIST BLEM
LIST BLEM

end-proc
$ Return
$!#############################################################################
