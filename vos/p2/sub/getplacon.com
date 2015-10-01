$!****************************************************************************
$!
$! Build proc for MIPL module getplacon
$! VPACK Version 1.9, Monday, December 07, 2009, 16:20:42
$!
$! Execute by entering:		$ @getplacon
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
$ write sys$output "*** module getplacon ***"
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
$ write sys$output "Invalid argument given to getplacon.com file -- ", primary
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
$   if F$SEARCH("getplacon.imake") .nes. ""
$   then
$      vimake getplacon
$      purge getplacon.bld
$   else
$      if F$SEARCH("getplacon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getplacon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getplacon.bld "STD"
$   else
$      @getplacon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getplacon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getplacon.com -mixed -
	-s getplacon.f zgetplacon.c -
	-i getplacon.imake -
	-t tgetplacon.f tzgetplacon.c tgetplacon.imake tgetplacon.pdf -
	   tstgetplacon.pdf -
	-o getplacon.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getplacon.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C	
C	
C	PURPOSE: Returns planetary constants given either the planet
C	name or the planet ID# as in the sedr/spice. Note:
C	If the planet name is given the id number will be returned
C	too. If the planet name is blanks then the planet name will
C	be returned too.
C
C	call getplacon(planet,id,data,ind)
C
C	Author: 		Jean Lorre
C	Cognizant Programmer:	Jean Lorre
C	Date written:		October 1989
C
C	Revisions:	
C	
C	April 24, 1992	JFM	Planet name is no longer CaSe sensitive;
C				lowercase and uppercase names acceptable;
C				(FR 73773)
C
C

      subroutine getplacon(planet,id,data,ind)

C planet=planet name                     input  character*12
C id    =planet SEDR/SPICE id number     input  integer*4
C data  =data buffer. 20 real*4 values.  returned  real
C ind   =status. 0=OK   1=unknown planet   2=unknown id
C data buffer:
C word #    contents
C  1      equatorial radius (long) km.
C  2      equatorial radius (short) km.
C  3      polar radius   km.
C  4      longitude of long axis    deg
C  5      rotation period          days
C  6      solar range              km
C  7-20   unused

      character*12 planet
      real*4 data(20)
				      ! CCASE routine setup
      mode=1			      ! set case selection 
      max=12			      ! maximum number of characters
      do 5 I=1,20
         data(i)=0.0
 5    continue
      ind=0
      if(planet.eq.'            ')then
         call pbname(id,planet,*10)   ! return planet
      else
         call pbid(planet,id,*11)     ! return id #
      endif
12    call ccase(planet,mode,-1)      ! ensure case is UPPER
      call pbdata(planet,data,*10)    ! return data
      return
10    ind=1
      return
11    ind=2
      goto 12
      end













$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zgetplacon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void  zgetplacon(planet,id,data,ind)
char   planet[12];
int    id,*ind;
void   *data;
{
FTN_NAME2(getplacon, GETPLACON) (planet,&id,data,ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getplacon.imake
/* Imake file for VICAR subroutine getplacon */

#define SUBROUTINE getplacon

#define MODULE_LIST getplacon.f zgetplacon.c

#define P2_SUBLIB
#define LIB_SPICE

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create tgetplacon.f
c Draw dvectors from a mark file containing tiepoints.
      include 'VICMAIN_FOR'
      subroutine main44
      implicit integer*4 (a-z)
      character*5 project
      character*12 planet,labplanet
      character*36 picname
      character*20 dcfile,radfile,blemfile,shfile
      character*8 intape,outtape
      character*4 ckname,instrument,csource
      integer*4 data(80),buf(200),count,def
      real*8 rbuf(100)
      real*4 restab(2720),pldata(20),rdata(80)
      real*4 is_line,is_samp,os_line,os_samp
C     logical xvptst
      equivalence (buf,rbuf),(instrument,buf(2))
      equivalence (csource,buf(11)),(data,rdata)
      equivalence (labplanet,data(25)),(picname,data(31))
      equivalence (dcfile,data(40)),(radfile,data(45))
      equivalence (blemfile,data(50)),(shfile,data(55))
      equivalence (intape,data(60)),(outtape,data(63))

c****************
c      CALL INIT_SPICE
c****************
c/*****init_spice******/
c     call errprt('SET','NONE')
c      call xvmessage('after errprt ...',' ')
c      if (failed()) call reset()

c      call erract('SET','RETURN')
c      call xvmessage('after erract ...',' ')
c      if (failed()) call reset()

c      call vwait(1)
c      call clpool()
c      call rspool_1('BINPOOL')
c/*****end init_spice******/

C     call init_spice
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      call xvmessage('  ',' ')

      call xvmessage('GETPROJ:',' ')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call xvmessage(project,' ')
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')
      call xvmessage('  ',' ')

      call xvmessage('GETCAMCON:',' ')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call xvmessage('  ',' ')
           
      call xvmessage('GETLABCON:',' ')
      call getlabcon(unit,project,data,ind)      
      if(ind.eq.1) call prnt(4,1,ind,'warning indicator=.')
      if(ind.gt.1) call prnt(4,1,ind,'fatal indicator=.')
      if(data(1).eq.0) then
         call xvmessage('invalid label type',' ')
      else if(data(1).eq.1) then
         call xvmessage('ground calibration label',' ')
      else if(data(1).eq.2) then
         call xvmessage('flight label',' ')
      else
         call prnt(4,1,data(1),'data(1)=.')
      endif         

      call xvmessage('GETSPICE2:',' ')
      if(project.eq.'GLL  ') then
         call xvmessage('coordinate system is: J2000',' ')
      else
         call xvmessage('coordinate system is: B1950',' ')
      endif
c     CKNAME='    '
      call xvparm('CKNAME',CKNAME,count,def,1)
      if(CKNAME.eq.'    ')then
         call xvmessage('SEDR/SPICE CKNAME is defaulted to blanks',' ')
      else
         call xvmessage('SEDR/SPICE CKNAME set to '//CKNAME,' ')
      endif
      planet='            '

      call getspice2(unit,.true.,buf,ind)
      if (ind.EQ.-1000) then
	go to 100
      endif
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         call xvmessage('Central body= '//planet,' ')
      else
         call xvmessage('Target body= '//planet,' ')
      endif
      if(ind.ne.1) call prnt(4,1,ind,'fatal indicator=.')

      call xvmessage('GETPLACON:',' ')
      call xvmessage('Test from Fortran',' ')
      idnum=buf(9)
      call getplacon(labplanet,idnum,pldata,ind)
      if(ind.eq.1) call xvmessage('unrecognizable planet name',' ')
      if(ind.eq.2) call xvmessage('unrecognizable planet id#',' ')
      call xvmessage('Target planet body is:'//labplanet,' ')
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call xvmessage('   ',' ')      

      call xvmessage('GETPLACON:',' ')
      call xvmessage('Test from C',' ')
      idnum=buf(9)
      call tzgetplacon(labplanet,idnum,pldata,ind)
      if(ind.eq.1) call xvmessage('unrecognizable planet name',' ')
      if(ind.eq.2) call xvmessage('unrecognizable planet id#',' ')
      call xvmessage('Target planet body is:'//labplanet,' ')
      call prnt(4,1,idnum,    'Planet id number        .')
      call prnt(7,1,pldata(1),'equatorial radius-long  .')
      call prnt(7,1,pldata(2),'equatorial radius-short .')
      call prnt(7,1,pldata(3),'polar radius            .')
      call prnt(7,1,pldata(4),'longitude of long radius.')
      call prnt(7,1,pldata(5),'rotation period         .')
      call prnt(7,1,pldata(6),'solar range             .')
      call xvmessage('   ',' ')      

      call xvmessage('GETGEOM:',' ')
      call xvpcnt('INP',count)
      if(count.gt.1)then
         call xvunit(unit2,'INP',2,status,' ')
         call xvopen(unit2,status,'OPEN_ACT','SA',' ')
         geomsor=1
         call xvmessage('obtaining geom parameters from input file',' ')
      else
         geomsor=0
         call xvmessage('obtaining geom parameters from nominals',' ')
      endif
      call getgeom(unit2,project,camera,geomsor,
     +             restab,restab,
     +             nah,nav,ind)
      if(ind.ne.0) call xvmessage('GETGEOM: error, ind=1',' ')
      call xvmessage('    ',' ')

      call xvmessage('CONVISOS:',' ')
      call xvmessage('assume image space l,s=400.,400.',' ')
      is_line=400.
      is_samp=400.
      nph=nah+1
      npv=nav+1
      mode=1
      call convisos(project,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call xvmessage('CONVISOS: error, ind=1',' ')
      call prnt(7,1,os_line,'object space line=.')
      call prnt(7,1,os_samp,'object space samp=.')
      mode=0
      call convisos(project,is_line,is_samp,os_line,
     +              os_samp,mode,restab(9),nph,npv,ind)      
      if(ind.ne.0) call xvmessage('CONVISOS: error, ind=1',' ')
      call prnt(7,1,is_line,'image space line=.')
      call prnt(7,1,is_samp,'image space samp=.')
      call xvmessage('   ',' ')

C      if(xvptst('UPDATE'))then
C        call xvmessage('PUTSPICE2:',' ')
C        call xvparm('DESTINAT',csource,count,def,1) ! loads buf(11)
C        if(csource.eq.'    ') then
C           call xvmessage('must specify SPICE destination keyword',' ')
C           call abend
C        endif
C        call putspice2(csource,'tgetplacon',buf,ind)
C        if(ind.ne.1) call prnt(4,1,ind,'PUTSPICE: fatal indicator.')
C        if(ind.eq.1) call prnt(4,1,ind,'PUTSPICE: successful run.')
C      endif
100   if (mode.gt.0) then
      	call spcfexit(info)
      endif
      return
      end







$!-----------------------------------------------------------------------------
$ create tzgetplacon.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  Unit test C-bridge for TGETPLACON.F */
/************************************************************************/
void FTN_NAME(tzgetplacon)(planet,id,data,ind)
char planet[12];
void *data;
int *id,*ind;
{
/*  ============================================  */
      zgetplacon(planet,*id,data,ind);
}
$!-----------------------------------------------------------------------------
$ create tgetplacon.imake
/* Imake file for Test of VICAR subroutine getplacon */

#define PROGRAM tgetplacon

#define MODULE_LIST tgetplacon.f tzgetplacon.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_TAE
/*#define LIB_LOCAL*/
#define LIB_P2SUB 
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tgetplacon.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=(1:2)
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM UPDATE       TYPE=KEYWORD VALID=(UPDATE,NOUPDATE) +
  DEFAULT=NOUPDATE
PARM DESTINAT TYPE=(STRING,5) COUNT=(0:1) VALID=("DAVI ","NAV  ", +
  "FARE ","NAV2 ","NEAR ","SEDR ","NAIF ","AMOS ") DEFAULT=--
END-PROC

$!-----------------------------------------------------------------------------
$ create tstgetplacon.pdf
procedure
refgbl $echo
refgbl $syschar
body

local path1 type=string init="wms_test_work:[testdata.mipl.vgr]"
local path2 type=string init="wms_test_work:[testdata.mipl.gll]"
let _onfail="continue"
let $echo="yes"
if ($syschar(1) = "UNIX")
!
    let path1="/project/it/testdata/mipl/vgr/"
    let path2="/project/it/testdata/mipl/gll/"
end-if
tgetplacon  inp=&"path1"f1636832.geo
tgetplacon  inp=&"path2"venus.img
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create getplacon.hlp
.TITLE
VICAR2 subroutine GETPLACON

.HELP
PURPOSE:
Returns planetary constants given either the planet
name or the planet ID# as in the sedr/spice. Note:
If the planet name is given the id number will be returned
too. If the planet name is blanks then the planet name will
be returned too.

EXECUTION:
      call getplacon(planet,id,data,ind)

.page
OPERATION
      subroutine getplacon(planet,id,data,ind)
c planet=planet name                     input  character*12
c id    =planet SEDR/SPICE id number     input  integer*4
c data  =data buffer. 20 real*4 values.  returned  real
c ind   =status. 0=OK   1=unknown planet   2=unknown id
c data buffer:
c word #    contents
c  1      equatorial radius (long) km.
c  2      equatorial radius (short) km.
c  3      polar radius   km.
c  4      longitude of long axis    deg
c  5      rotation period          days
c  6      solar range              km
c  7-20   unused
.page
HISTORY

Written By: Jean Lorre        10/1/89
Cognizant Programmer: J Lorre
Revisions:
 24 apr 92  jfm  Planet name is no longer CaSe sensitive (FR 73773)
 26 jun 92  gmy  Added call to init_spice in tgetplacon (70964)
 21 Mar 94  ffm  Made portable
 24 Jun 94  ffm  Commented out  LIB_LOCAL in tgetplacon.imake (FR 85114, 85115)
                 Tested on 5 platforms. (ALPHA, VAX, SUN-0S, SGI, SOLARIS)
 23 Aug 94  ffm  Modify test program to add mode to getspice to interface
                 with SPICE_SERVER.(FR 85092, 85625, 85627).
 02 Aug 96  oam  Included initialization of data buffer to avoid different 
                 nonsense  numbers when a planet is not identified (SGI). 
                 Modified tgetplacon to call getspice2 instead of getspice.(DFR). $ Return
$ Return
$!#############################################################################
