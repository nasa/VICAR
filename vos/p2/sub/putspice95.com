$!****************************************************************************
$!
$! Build proc for MIPL module putspice95
$! VPACK Version 1.9, Monday, December 07, 2009, 16:32:39
$!
$! Execute by entering:		$ @putspice95
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
$ write sys$output "*** module putspice95 ***"
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
$ write sys$output "Invalid argument given to putspice95.com file -- ", primary
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
$   if F$SEARCH("putspice95.imake") .nes. ""
$   then
$      vimake putspice95
$      purge putspice95.bld
$   else
$      if F$SEARCH("putspice95.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake putspice95
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @putspice95.bld "STD"
$   else
$      @putspice95.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create putspice95.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack putspice95.com -mixed -
	-s putspice95.c -
	-i putspice95.imake -
	-t tputspice95.f tzputspice95.c tputspice95.imake tputspice95.pdf -
	   tstputspice95.pdf -
	-o putspice95.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create putspice95.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
#include <string.h>
#include "spc.h"
#include "spiceinc.h"
#include "ms_defines.h"

/*=================================================*
 * Fortran-Callable Version			   *
 *=================================================*/
void FTN_NAME2(putspice95, PUTSPICE95) (project, buf, mode, ind)
 int		*project;
 void	 	*buf;
 int		*mode;
 int		*ind;
{
 buf_union_typ	cbuf;
 memcpy((char *) &cbuf, (char *) buf,
		sizeof(buf_union_typ));
 *ind = zputspice95(*project, &cbuf, *mode);
}

/*=================================================*
 * putspice95()					   *
 *=================================================*/
int zputspice95(project, buf, mode)
 int		project;
 buf_union_typ	*buf;
 int		mode;
{
 int status;
 msCkStruct ckdata;

 int		i, tint, inst_val;
 float		tfloat;
 double		tdouble;
 char		inst_name[32];
 usr_kdb_typ 	usrinfo;

 memset ((void*) inst_name, '\0', 32);
 memset ((void*) &ckdata, '\0', sizeof(ckdata));
 memset ((void*) &usrinfo, '\0', sizeof(usrinfo));

/** txh::removed because of invalid reference.
 memcpy ((char*) inst_name, (char*) &buf->intbuf[4], 4);
**/
/** txh::corrected reference **/
 memcpy ((char*) inst_name, (char*) &buf->intbuf[1], 4);

 ckdata.sc_id = buf->intbuf[0];

 if (project == CAS_SC_ID)
   {
     if ((!strncmp(inst_name, CASISSNA_STR, 4)) ||
	 (!strncmp(inst_name, CASISSNA_SUM22_STR, 4)) ||
	 (!strncmp(inst_name, CASISSNA_SUM44_STR, 4)))      
       {
	 ckdata.instrument = CASISSNA_NAIF_ID;
       }
     else if ((!strncmp(inst_name, CASISSWA_STR, 4)) ||
	 (!strncmp(inst_name, CASISSWA_SUM22_STR, 4)) ||
	 (!strncmp(inst_name, CASISSWA_SUM44_STR, 4)))      
       {
	 ckdata.instrument = CASISSWA_NAIF_ID;
       }
     else
       {
	 char msg[64];
	 memset(msg, '\0', sizeof(msg));
	 
	 sprintf(msg,"Unsupported instrument name '%s'", inst_name); 
	 zvmessage(msg, " ");
	 zvmessage ("\nPutspice95 returns with no data", " ");
	 return FAILURE;   
       }
   }
 else if ((project == GLL_SC_ID) || 
	  (project == VGR_1_SC_ID) || 
	  (project == VGR_2_SC_ID))
   {
     if (!strncmp (inst_name, "ISSN", 4)) 
       {
	 inst_val = ISSNA;
       }
     else if (!strncmp (inst_name, "ISSW", 4))
       {
	 inst_val = ISSWA;
       }
     else if (!strncmp (inst_name, "SSI", 3))
       {
	 inst_val = PLATFORM;
       }
     else
       {
	 char msg[64];
	 memset(msg, '\0', sizeof(msg));
	 
	 sprintf(msg,"Unsupported instrument name '%s'", inst_name); 
	 zvmessage(msg, " ");
	 zvmessage ("\nPutspice95 returns with no data", " ");
	 return FAILURE;   
       }

     /** txh::removed because of invalid instrument code.
	 ckdata.instrument = buf->intbuf[2];
     **/
     /** txh::corrected instrument code calculation based from ck.req **/
     ckdata.instrument = -(abs (buf->intbuf[0]) * 1000 + inst_val);
   }
 else
   {	
     char msg[64];
     memset(msg, '\0', sizeof(msg));
     
     sprintf(msg,"Unsupported project numer '%d'.", project); 
     zvmessage(msg, " ");
     zvmessage ("\nPutspice95 returns with no data", " ");
     return FAILURE;   
   }

 ckdata.system = buf->intbuf[9];
 for (i = 0; i < 6; i++) ckdata.scet[i] = buf->intbuf[2+i];
 for (i = 0; i < 3; i++) ckdata.av[i] = buf->doublebuf[37+i];
 for (i = 0; i < 9; i++) ckdata.c_matrix[i] = buf->doublebuf[40+i];
 ckdata.avFlag = 1;
 GetUsrInfo (&usrinfo, buf);
 memcpy ((void*) ckdata.seg_id, (void*)&buf->intbuf[188], 4);
 strncat (ckdata.seg_id, usrinfo.purpose, LEN_PURPOSE);
 strncat(ckdata.seg_id, usrinfo.prog_name, LEN_PROG_NAME);
 strncat(ckdata.seg_id, usrinfo.sp_ref, LEN_SP_REF);
 strncat(ckdata.seg_id, usrinfo.req_no, LEN_REQ_NO);
 if (strcmp(usrinfo.date_time, "000000000000") == 0)
    GetDateTime(usrinfo.date_time);
 strncat(ckdata.seg_id, usrinfo.date_time, LEN_DATE_TIME);
 strncat(ckdata.seg_id, usrinfo.usr_grp_id, LEN_USR_ID);
 memcpy((char*)ckdata.ck_id, (char*) &buf->intbuf[171], 4);
 memcpy((char*)ckdata.ck_source, (char*) &buf->intbuf[10], 4);

 if (mode == MODE_REMOTE) {
    switch (ckdata.sc_id) {
        case GLL_SC_ID:
            status = !msclt_gllputspice(&ckdata);
            break;
        case VGR_1_SC_ID:
            status = !msclt_vgr1putspice(&ckdata);
            break;
        case VGR_2_SC_ID:
            status = !msclt_vgr2putspice(&ckdata);
            break;
        case VIKOR_1_SC_ID:
            status = !msclt_vo1putspice(&ckdata);
            break;
        case VIKOR_2_SC_ID:
            status = !msclt_vo2putspice(&ckdata);
            break;
        case CAS_SC_ID:
            status = !msclt_casputspice(&ckdata);
            break;
        case SIM_SC_ID:
            status = !msclt_simputspice(&ckdata);
            break;
        default:
            zvmessage ("\nERROR:unknown remote mode SC_ID", " ");
            zvmessage ("\nPutspice95 returns with no data", " ");
            status = FAILURE;
            break;
        }
    }
 else if (mode == MODE_LOCAL) {
    switch (ckdata.sc_id) {
        case GLL_SC_ID:
            status = !mslcl_gllputspice(&ckdata);
            break;
        case VGR_1_SC_ID:
            status = !mslcl_vgr1putspice(&ckdata);
            break;
        case VGR_2_SC_ID:
            status = !mslcl_vgr2putspice(&ckdata);
            break;
        case VIKOR_1_SC_ID:
            status = !mslcl_vo1putspice(&ckdata);
            break;
        case VIKOR_2_SC_ID:
            status = !mslcl_vo2putspice(&ckdata);
            break;
        case CAS_SC_ID:
            status = !mslcl_casputspice(&ckdata);
            break;
        case SIM_SC_ID:
            status = !mslcl_simputspice(&ckdata);
            break;
        default:
            zvmessage ("\nERROR:unknown local mode SC_ID", " ");
            zvmessage ("\nPutspice95 returns with no data", " ");
            status = FAILURE;
            break;
        }
    }
 else {
    zvmessage("Unsupported MODE", " ");
    zvmessage ("\nPutspice95 returns with no data", " ");
    status = FAILURE;
    }

 /** txh::removed to return the correct status (i.e. 1=SUCCESS, 0=FAILUE)
 return (!status);
 **/

 return status;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create putspice95.imake
#define SUBROUTINE putspice95

#define MODULE_LIST putspice95.c

#define	FTN_STRING
#define USES_FORTRAN
#define USES_ANSI_C
#define P2_SUBLIB 

#define LIB_NETWORK

/* #define DEBUG */ /* Remove before delivery. */
$ Return
$!#############################################################################
$Test_File:
$ create tputspice95.f
C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                    PUTSPICE95
C
C  Modified 8/6/99 by Michael Brady
C  
C  Added the variable "proj_name".  Previously, the 
C  integer "project" was used as a parameter for
C  getlabcon and get proj, causing them to fail.
C
C********************************************************
C
	include 'VICMAIN_FOR'
	subroutine main44

        implicit none

        character*6 proj_name
	integer*4 mode
	character*80 imgfile
	character*10 source
	integer*4 project
	integer*4 system 
	character*10 usr_info(10)
	character*5 camera
        character*12 target_name
        character*12 temp
	double precision dbuf(100)
	integer*4 ibuf(200)
	integer*4 ind, count, def
	equivalence (dbuf, ibuf)
	integer*4 scet(6)
	integer*4 data(80)
        integer*4 unit, status, cam, fds

C	Selecting mode to tell putspice95() where to
C	write the data
	print *, 'Input mode (local = 0, remote = 1)'
	read *, mode

	call xvunit(unit, 'INP', 1, status, ' ')
	call xvopen(unit, status, 'OPEN_ACT', 'SA', ' ')
	call getproj(unit, proj_name, cam, fds, ind)
	call getlabcon(unit, proj_name, data, ind)

        call mve(4, 6, data(8), scet, 1, 1)
        call mvlc(data(25), target_name, 12)

        call xvparm('INP', imgfile, count, def, 0)
        call xvparm('SOURCE', source, count, def, 0)
	call xvparm('PROJECT', project, count, def, 0)
        call xvparm('CAMERA', camera, count, def, 0)
        call xvparm('TARGET', temp, count, def, 0)

        if (def .EQ. 0) then
           target_name=temp
        endif

	call xvparm('INSTITUTE', usr_info(1), count, def, 0)
	call xvparm('PURPOSE', usr_info(2), count, def, 0)
	call xvparm('PROG_NAME', usr_info(3), count, def, 0)
	call xvparm('SP_REF', usr_info(4), count, def, 0)
	call xvparm('REQ_NO', usr_info(5), count, def, 0)
	call xvparm('YEAR', usr_info(6), count, def, 0)
	call xvparm('MONTH_DAY', usr_info(7), count, def, 0)
	call xvparm('HOUR_MIN', usr_info(8), count, def, 0)
	call xvparm('FILE_ID', usr_info(9), count, def, 0)
	call xvparm('USR_GRP_ID', usr_info(10), count, def, 0)

	if (project .eq. -77) then
	   system = 2
	else
	   system = 1
	endif

        call xvmessage
     &     ('............................................', ' ')
        call xvmessage(' ',' ')
        call xvmessage('..................', ' ')
        call xvmessage('Calling GetSpice95', ' ')
	call getspice95(mode, project, camera, scet, target_name,
     &        system, source, usr_info, dbuf, ind)
        call xvmessage(' ',' ')

	if (ind .eq. 0) then
	   print*, 'ZGETSPICE95::Test From FORTRAN:FAILURE'
	else
           call xvparm('SOURCE', source, count, def, 0)
	   call mvcl(source, ibuf(11), 4)

        call xvmessage
     &     ('...... Testing PUTSPICE95 In Fortran .......', ' ')
	   ind  = 0
	   call putspice95(project, dbuf, mode, ind)
	   if (ind .eq. 0) then
	      print*, 'PUTSPICE95_ERROR:: Fortran Test FAILED'
	   else
              print*, 'PUTSPICE95:: Fortran Test SUCCESS'
	   endif
       endif

        call xvmessage('           ', ' ')
        call xvmessage
     &     ('...... Testing PUTSPICE95 In C .......', ' ')
        ind  = 0
        call tzputspice95(project, dbuf, mode, ind)
        if (ind .eq. 0) then
           print*, 'PUTSPICE_ERROR:: C Test FAILED'
        else
           print*, 'PUTSPICE95::C Test SUCCESS'
        endif

	return
	end
$!-----------------------------------------------------------------------------
$ create tzputspice95.c
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spc.h"
#include "spiceinc.h"

/************************************************************************/
/* Test Program For FORTRAN AND C Callable Subroutine PUTSPICE95.F      */
/************************************************************************/

void FTN_NAME(tzputspice95)(project, buf, mode, ind)
 int	*project;
 void	*buf;
 int	*mode;
 int	*ind;
{
 char		src[5];
 buf_union_typ	cbuf;
 memset(src, '\0', 5);
 memcpy((char *) &cbuf, (char *) buf,
		sizeof(buf_union_typ));

 *ind = zputspice95(*project, &cbuf, *mode);
}
$!-----------------------------------------------------------------------------
$ create tputspice95.imake
/* Imake file for Test of VICAR subroutine putspice95 */

#define PROGRAM tputspice95

#define MODULE_LIST tputspice95.f tzputspice95.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

#define FTN_STRING
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB 
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK

/* #define DEBUG */ /* Remove before delivery. */
/* #define LIB_LOCAL */ /* Remove before delivery. */

$!-----------------------------------------------------------------------------
$ create tputspice95.pdf
!PDF file for VICAR test program TPUTSPICE95
!tputspice95.pdf		6/15/1994

process help=* option=nointerrupt 
parm inp        type=string count=1
parm source     type=string count=1
parm project    type=integer count=1
parm camera     type=string count=1 default=SSI
parm target     type=string count=1 default=""
parm mode       type=integer count=1 default=1
parm institute  type=string count=1 default=MIPS
parm purpose    type=string count=1 default=NONE
parm prog_name  type=string count=1 default=*NONE*
parm sp_ref     type=string count=1 default=NONE
parm req_no     type=string count=1 default=NONE
parm year       type=string count=1 default=0000
parm month_day  type=string count=1 default=0000
parm hour_min   type=string count=1 default=0000
parm file_id    type=string count=1 default=NONE
parm usr_grp_id type=string count=1 default=*NONE*
end-proc
$!-----------------------------------------------------------------------------
$ create tstputspice95.pdf
!*****************************************************************************
! tstputspice95.pdf - unit test for putspice95
!
! Testers: please read the unit test for information!
! ftp from the vax into your local directory the test file venus.img
! located in mipldisk:[mipl.gll]
! (don't foget to say binary )
! make sure that the spiceserver is up and running.....
! If it's not ask Pam Woncik or Sam Le to start it
! get into VICAR and run tstputspice.
! You can test remote on all platforms
!*****************************************************************************
procedure help=*
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let  $autousage="none"
!***testonly***
!**************

local path1 string
local path2 string


! Tests for UNIX only

if ($syschar(1) = "VAX_VMS")
else

write "Testing CAS putspice95"
tputspice95 inp=/project/test_work/testdata/mipl/cas/n1373703343.1 +
	source=FARE project=-82 camera=ISSN

end-if
! Tests for UNIX and VMS

if ($syschar(1) = "VAX_VMS")
   let path1="wms_test_work:[testdata.mipl.gll]"
   let path2="wms_test_work:[testdata.mipl.vgr]"
else
   let path1="/project/test_work/testdata/mipl/gll/"
   let path2="/project/test_work/testdata/mipl/vgr/"
end-if

!write "Testing VGR putspice95"
!tputspice95 inp=&"path2"f1636832.fic source=SEDR project=-31 +
!   target=IO camera=ISSN

!WRITE "Testing GLL putspice95"
!tputspice95 inp=&"path1"venus.img source=FARE project=-77

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create putspice95.hlp
1. PUTSPICE95

Description:
 To update the SPICE for GLL with provenance information. If provenance
 information is not available, their default values will be used.

FORTRAN calling sequence:
	integer*4	project
	real*8		buf
	integer*4	mode
	integer*4	ind

	call putspice95(PROJECT, BUF, MODE, ind)

C calling sequence:
	#include "xvmaininc.h"
	#include "ftnbridge.h"
	#include "spc.h"
	#include "spiceinc.h"

	int		project;
	buf_union_typ	*buf;
	int		mode;
	zputspice95(project, buf, mode)

2. ARGUMENTS

   INPUT ARGUMENTS:
	project = spacecraft identification number consistence    integer*4
                  with NAIF (GLL = -77, VGR-1 = -31, VGR-2 = -32)

	buf     = 100 word buffer containing SPICE data.
		  The following item in buf must be
		  correctly filled when calling putspice95():
			- Instrument
			- SCET data
			- Source string
			- Target Name
			- C, OM & ME Matrix
			- provenance information (should all
			  be in character format). If not,
			  default values will be used.

	mode    = 0 use local spice kernels
                = 1 use MIPS spice kernel via "spiceserver"

   OUTPUT ARGUMENT:
	ind     = status of call
		  status values are defined as follows:
                  0 = Success
                 -1 = Unrecognizable Project Id
                 -2 = Error Opening Kernel File
                 -3 = Cannot Update NAIF KERNEL
                 -4 = Error Packing Seg. Descriptor
                 -5 = Error Starting New Array
                 -6 = Error Adding New Array
                 -7 = Daf-file Not Close Properly
                 -8 = Bad User Input Data

	*** For C calling sequence, zputspice95() return
		an integer as its indicator code.


   When calling putspice95, the user can specify both the source of the file
	he/she wants to update and the ck_id of the file in the usr_info
	data structure.
	If both are specified, ck_id will be used. If ck_id is specified but
	not valid, source will be used. ck_id has higher priority because
	it is more specific compared to source.

HISTORY

Written By: S Le	6-19-1995
*** Help file is originally written by Jean Lorre

   May 20, 1998   ...T.Huang...  corrected instrument-code calculation problem.

   Oct 22, 1998   ...T.Huang...   Modifed to return the correct status.
                                  (i.e. 1=SUCCESS, 0=FAILURE).
                                  The 'msclt' and 'mslcl' subroutines return
                                  0 for success and -1 for failure.
   Apr 01 99   GMY  Add (char*) to eliminate SGI compile warning messages.

   Jun 03, 1999   ...T.Huang...   Obsoleted c95.c

   Jun 24, 1999   ...T.Huang...   Corrected calls to mslcl_<proj>putspice and
                                  mapping of camera ids.
$ Return
$!#############################################################################
