$!****************************************************************************
$!
$! Build proc for MIPL module getspice2
$! VPACK Version 1.9, Monday, July 11, 2011, 17:45:35
$!
$! Execute by entering:		$ @getspice2
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
$ write sys$output "*** module getspice2 ***"
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
$ write sys$output "Invalid argument given to getspice2.com file -- ", primary
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
$   if F$SEARCH("getspice2.imake") .nes. ""
$   then
$      vimake getspice2
$      purge getspice2.bld
$   else
$      if F$SEARCH("getspice2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getspice2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getspice2.bld "STD"
$   else
$      @getspice2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getspice2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getspice2.com -mixed -
	-s getspice2.f zgetspice2.c putspice2.c -
	-i getspice2.imake -
	-t tgetspice2.f tzgetspice2.c tgetspice2.imake tgetspice2.pdf -
	   tputspice2.f tzputspice2.c tputspice2.imake tputspice2.pdf -
	   tstgetspice2.pdf -
	-o getspice2.hlp putspice2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getspice2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Get GETLABCON label buffer and call GETSPICE4
C
C Notes added July 2011 by lwk:
C These subroutines provide interfaces to routine getspice95, which returns
C SPICE navigation data for a flight-project image.
C Users should be aware that these routines clear the SPICE kernel pool before
C returning, so subsequent calls to SPICE routines will fail unless INIT_SPICE()
C or an equivalent routine is called.
C
      SUBROUTINE GETSPICE2(UNIT,PROVENANCE,buf,ind)
      IMPLICIT NONE
      INTEGER*4 UNIT			!Input image logical unit
      LOGICAL PROVENANCE		!Check provenance flag
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND			!Returned status

      INTEGER*4 CAMERA_SN,SCLK,DATA(80)
      CHARACTER*5 PROJECT

      CALL GETPROJ(UNIT,project,camera_sn,sclk,ind)	!Get project ID
      CALL GETLABCON(UNIT,PROJECT,data,ind)		!Get label data
      CALL GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END
C Construct fake GETLABCON buffer and use it to call GETSPICE4
C
      SUBROUTINE GETSPICE3(PROJECT,TARGET,CAMERA_SN,SCLK,SCET,
     &		PROVENANCE,buf,ind)
      IMPLICIT NONE
      CHARACTER*5 PROJECT
      CHARACTER*12 TARGET
      INTEGER*4 CAMERA_SN
      INTEGER*4 SCLK
      INTEGER*4 SCET(6)
      LOGICAL PROVENANCE		!Check provenance flag
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND			!Returned status
      INTEGER*4 LCV                     !Loop Control Variable

      INTEGER*4 DATA(80)		!Fake GETLABCON buffer

      DO LCV = 1,80
         DATA(LCV) = -999
      ENDDO

      DATA(2) = SCLK
      DATA(6) = CAMERA_SN
      CALL MVE(4,6,SCET,data(8),1,1)
      CALL MVCL(TARGET,data(25),12)
      CALL GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END


C Extract data from VICAR label and user parameters to set up call
C to GETSPICE95
C
      SUBROUTINE GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)
      IMPLICIT NONE
      CHARACTER*5 PROJECT
      LOGICAL PROVENANCE		!Check provenance flag
      INTEGER*4 DATA(80)      
      DOUBLE PRECISION BUF(100)	!Returned SPICE buffer
      INTEGER*4 IND,STATUS		!Returned status

      INTEGER*4 I,SC_ID,SYSTEM,MODE,COUNT,DEF
      INTEGER*4 SCET(6),CAMERA_SN,SCLK
      CHARACTER*11 DEFAULT
      CHARACTER*5 CAMERA
      CHARACTER*12 TARGET,USER_TARGET,CDATE
      CHARACTER*4 YEAR,MONTH_DAY,HOUR_MINUTE
      CHARACTER*10 SOURCE
      CHARACTER*6 USERID
      CHARACTER*3 GROUPID
      CHARACTER*10 USR_INFO(10)
      LOGICAL XVPTST

      SCLK = DATA(2)
      CAMERA_SN = DATA(6)

C     ....verify scet_date format
      call scet_update (project, data,status)

      CALL MVE(4,6,DATA(8),scet,1,1)
      CALL MVLC(DATA(25),target,12)
C     ....Get User parameter items
      CALL XVPARM('TARGET',user_target,count,def,0)
      IF (COUNT.EQ.1) CALL UCASE(USER_TARGET,TARGET)
      CALL XVPARM('CKNAME',source,count,def,0)
      CALL UCASE(SOURCE,source)
      CALL XVPARM('CKID',usr_info(9),count,def,0)

      IF (PROVENANCE) THEN
         DO I=1,5
            USR_INFO(I) = '          '
         ENDDO
         CALL XVPARM('INSTITUTE',usr_info(1),count,def,0)
         CALL XVPARM('PURPOSE',usr_info(2),count,def,0)
         CALL XVPARM('PROGRAM',usr_info(3),count,def,0)
         CALL XVPARM('SPKID',usr_info(4),count,def,0)
         CALL XVPARM('REQNUM',usr_info(5),count,def,0)
         CALL XVPARM('CDATE',cdate,count,def,0)
         YEAR = CDATE(1:4)
         MONTH_DAY = CDATE(5:8)
         HOUR_MINUTE = CDATE(9:12)
         USR_INFO(6) = YEAR
         USR_INFO(7) = MONTH_DAY
         USR_INFO(8) = HOUR_MINUTE
         USERID = '*NONE*'
         CALL XVPARM('USERID',userid,count,def,0)
         CALL XVPARM('GROUPID',groupid,count,def,0)
         IF (COUNT.EQ.1) USERID(4:6) = GROUPID(1:3)
         USR_INFO(10) = USERID
      ELSE
         USR_INFO(1) = 'NONE'		!institute
         USR_INFO(2) = 'NONE'		!purpose
         USR_INFO(3) = '*NONE*'		!program
         USR_INFO(4) = 'NONE'		!skid
         USR_INFO(5) = 'NONE'		!reqnum
         USR_INFO(6) = '0000'		!year
         USR_INFO(7) = '0000'		!month,day
         USR_INFO(8) = '0000'		!hour,min
         USR_INFO(10) = '*NONE*'	!userid
      ENDIF

      DO I=1,10
         CALL UCASE(USR_INFO(I),usr_info(i))
      ENDDO

      IF (PROJECT.EQ.'VO-1 ') SC_ID=-27
      IF (PROJECT.EQ.'VO-2 ') SC_ID=-30
      IF (PROJECT.EQ.'VO-1 '.OR.PROJECT.EQ.'VO-2 ') THEN
         CALL XVMESSAGE('***SPICE data not available for VO',' ')
         IND = -1
         RETURN
      ENDIF

      IF (PROJECT.EQ.'VGR-1') THEN		!Voyager 1
         SC_ID = -31
         IF (CAMERA_SN.NE.6 .AND. CAMERA_SN.NE.7) GOTO 990
         CAMERA = 'ISSW'
         IF (CAMERA_SN.EQ.7) CAMERA='ISSN'
         SYSTEM = 2
      ENDIF

      IF (PROJECT.EQ.'VGR-2') THEN		!Voyager 2
         SC_ID = -32
         IF (CAMERA_SN.NE.4 .AND. CAMERA_SN.NE.5) GOTO 990
         CAMERA = 'ISSW'
         IF (CAMERA_SN.EQ.5) CAMERA='ISSN'
         SYSTEM = 2
      ENDIF

      IF (PROJECT.EQ.'GLL  ') THEN
         SC_ID = -77
         IF (CAMERA_SN.NE.1 .AND. CAMERA_SN.NE.2) GOTO 990
         CAMERA = 'SSI1'
         IF (CAMERA_SN.EQ.2) CAMERA='SSI2'
         SYSTEM = 1
      ENDIF

      IF (PROJECT.EQ.'CASSI') THEN
         SC_ID = -82
         CAMERA = '?'
         IF (CAMERA_SN.EQ.1) CAMERA='ISSN'
         IF (CAMERA_SN.EQ.21) CAMERA='ISN2'
         IF (CAMERA_SN.EQ.41) CAMERA='ISN4'
         IF (CAMERA_SN.EQ.2) CAMERA='ISSW'
         IF (CAMERA_SN.EQ.22) CAMERA='ISW2'
         IF (CAMERA_SN.EQ.42) CAMERA='ISW4'
         IF (CAMERA.EQ.'?') GOTO 990
         SYSTEM = 1
      ENDIF

      IF (XVPTST('REMOTE')) THEN
         MODE=1
      ELSE IF (XVPTST('LOCAL')) THEN
         MODE = 0
      ELSE
         CALL XGETENV_VIC('DEFAULTSPICE',default)
         MODE = 0
         IF (DEFAULT.EQ.'REMOTESPICE') MODE=1 
      ENDIF          

      CALL GETSPICE95(MODE,SC_ID,CAMERA,SCET,TARGET,
     &          SYSTEM,SOURCE,USR_INFO,buf,ind)
      IF (IND.NE.1) CALL PRNT(4,1,IND,'***SPICE error=.')
      RETURN

  990 CALL XVMESSAGE('***Invalid camera serial number',' ')
      IND = -1		!Bad user input
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FORTRAN bridge for zgetspice3 and zgetspice4, second stage
C
      SUBROUTINE XGETSPICE4(PROJECT,N,PROVENANCE,DATA,buf,ind)
      BYTE PROJECT(1)
      INTEGER*4 N,PROVENANCE,DATA(80),IND
      REAL*8 BUF(100)

      CHARACTER*5 CPROJECT

      CALL MVLC(PROJECT,cproject,n)

      CALL GETSPICE4(CPROJECT,PROVENANCE,DATA,buf,ind)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zgetspice2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* C-bridge for FORTRAN routines GETSPICE2, GETSPICE3, GETSPICE4 */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spice89.h"
#include <string.h>


int zgetspice2(unit,provenance,buf)
int unit,provenance;
buf_union_typ *buf;
{
  int ind;

  FTN_NAME2(getspice2, GETSPICE2) (&unit,&provenance,buf->intbuf,&ind);
  return(ind);
}
int zgetspice3(project,target,camera_sn,sclk,scet,provenance,buf)
char project[6];	/* GLL, VGR-1, VGR-2 */
char target[13];	/* Target/planet for GLL/VGR respectively */
int camera_sn;		/* Camera serial number */
int sclk;		/* Frame number (FDS for VGR) */
int scet[6];		/* SCET (year,day,hour,minute,sec,msec) */
int provenance;
buf_union_typ *buf;	/* output SPICE/SEDR buffer */
{
  int ind,i,n;
  int data[80];	/* input label buffer (from GETLABCON) */
  char ztarget[13];

  for (i=0; i<80; i++)
     data[i] = -999;

  data[1] = sclk;
  data[5] = camera_sn;
  for (i=0; i<6; i++) data[7+i]=scet[i];
  strcpy(ztarget,target);
  for (i=strlen(target); i<12; i++) ztarget[i]=' ';
  strncpy((char *)&data[24],ztarget,12);
  n = strlen(project);  
  FTN_NAME2(xgetspice4,XGETSPICE4)(project,&n,&provenance,data,
						buf->intbuf,&ind);
  return(ind);
}
int zgetspice4(project,provenance,data,buf)
char *project;
int provenance;
int data[80];		/* input label buffer (from GETLABCON) */
buf_union_typ *buf;
{
  int ind,n;

  n = strlen(project);  
  FTN_NAME2(xgetspice4, XGETSPICE4) (project,&n,&provenance,data,
							buf->intbuf,&ind);
  return(ind);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create putspice2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"
#include <string.h>

/*========================================================*
 * PUTSPICE2(): FORTRAN to C bridge routine
 *========================================================*/
void FTN_NAME2(putspice2, PUTSPICE2) (char *source, char *program, int buf[200],
			int *ind, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 int maxlen;
 char csource[15];
 char cprogram[7];

 maxlen = 5;
 zsfor2c(csource,maxlen,source,&source,5,2,1, ind);
 maxlen = 7;
 zsfor2c(cprogram,maxlen,program,&source,5,2,1, ind);

 *ind = zputspice2(csource,cprogram,buf);
}
/**********************************************************************
 * zputspice2: routine to copy provenance parameters and  PUTSPICE95
 **********************************************************************/

int zputspice2(source,program,buf)
char source[5];		/* FARE, NAV, NAV2, AMOS, DAVI, NEAR, NAIF, SEDR */
char program[20];	/* Program name */
int buf[200];
{
  int sc_id;		/* Spacecraft ID: GLL=-77 */
  int mode,count,def,ind,len,i;
  char institute[5],purpose[5],reqnum[5];
  char userid[7];
  char groupid[4];
  char defaultspice[12];
  char *getenv_vic(char *);
  char *cuserid_p2();
  char *ptr;

  sc_id = buf[0];
  memset(institute,'\0',5);
  memset(purpose,'\0',5);
  memset(reqnum,'\0',5);
  memset(userid,'\0',7);
  memset(groupid,'\0',4);

  zvparm("INSTITUTE",institute,&count,&def,0,0);
  if (count == 0) strcpy (institute, "NONE");

  spaceFilledStr(institute,4);
  zvparm("PURPOSE",purpose,&count,&def,0,0);
  zvparm("REQNUM",reqnum,&count,&def,0,0);
  zvparm("CDATE",&buf[168],&count,&def,0,0);

  strncpy(userid,cuserid_p2(),6);
  len = strlen(userid);
  if (len < 4) {		/* Unix userid is only 3 chars */
     zvparm("GROUPID",groupid,&count,&def,0,0);
     if (count==0) {
        ptr = getenv_vic("GROUPID");
        if (ptr != 0) strncpy(groupid,getenv_vic("GROUPID"),3);
     }
     strncpy(&userid[3],groupid,3); 
  }
  zccase(userid,1,6);
  strncpy((char *)&buf[10],source,4);
  strncpy((char *)&buf[171],"NONE",4);
  strcpy((char *)&buf[172],purpose);
  strncpy((char *)&buf[173],program,6);
  strcpy((char *)&buf[175],reqnum);
  strncpy((char *)&buf[176],userid,6);
  strncpy((char *)&buf[188],institute,4);

  if (zvptst("remote")) mode=1;
  else if (zvptst("local")) mode=0;
  else {
     strcpy(defaultspice,getenv_vic("DEFAULTSPICE"));
     mode = 0;
     if (!strncmp(defaultspice,"REMOTESPICE",11)) mode=1;
  }

  ind = zputspice95(sc_id,buf,mode);
  return(ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getspice2.imake
/* Imake file for VICAR subroutine GETSPICE2 */
#define SUBROUTINE getspice2
#define MODULE_LIST  getspice2.f zgetspice2.c putspice2.c
#define FTN_STRING
#define USES_FORTRAN
#define USES_ANSI_C
#define P2_SUBLIB

#define LIB_NETWORK
/*#define DEBUG		/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tgetspice2.f
C****************************************************************
C   TGETSPICE2: Test program for subroutine GETSPICE2
C****************************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      integer*4 unit,status,i,ind
      integer*4 ibuf(200),jbuf(200)
      logical provenance
      character*5 project
      character*12 target
      integer*4 camera_sn,sclk,data(100),scet(6)
      double precision dbuf(100)
      equivalence (dbuf,ibuf)

      do i=1,200
         ibuf(i) = 0
         jbuf(i) = 0
      enddo

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
c     ....Testing getspice2
      provenance = .true.
      call getspice2(unit,provenance,dbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE2 FORTRAN test failed',' ')
         goto 100
      endif
c     ....Testing getspice3
      call getproj(unit,project,camera_sn,sclk,ind)
      call getlabcon(unit,project,data,ind)
      call mve(4,6,data(8),scet,1,1)
      call mvlc(data(25),target,12)
      call getspice3(project,target,camera_sn,sclk,scet,provenance,
     &		jbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE3 FORTRAN test failed',' ')
         goto 100
      endif
      call xvmessage('Comparing GETSPICE2 and GETSPICE3 output.',' ')
      call compare_bufs(ibuf,jbuf)

C     ....Testing getspice4
      call getspice4(project,provenance,data,jbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE4 FORTRAN test failed',' ')
         goto 100
      endif
      call xvmessage('Comparing GETSPICE2 and GETSPICE4 output.',' ')
      call compare_bufs(ibuf,jbuf)


      call xvmessage('GETSPICE2 FORTRAN test succeeded',' ')
      call xvmessage('             ',' ')
      call xvmessage('GETSPICE2 C-bridge test',' ')
  100 call tzgetspice2(unit,jbuf,data)

      call xvmessage('Comparing GETSPICE2 and zgetspice2 output.',' ')
      call compare_bufs(ibuf,jbuf)
      return
      end
ccccccccccccccccccccccccccccccccccccccccc
c Compare buffers.
c
      subroutine compare_bufs(ibuf,jbuf)
      integer*4 ibuf(200),jbuf(200)
      integer*4 i

      do i=1,200
         if (ibuf(i).ne.jbuf(i)) 
     &      call prnt(4,1,i,' ***buffers do not match, i=.')
      enddo
      return
      end
$!-----------------------------------------------------------------------------
$ create tzgetspice2.c
/************************************************************************/
/* Test C-bridge zgetspice2 of FORTRAN subroutine GETSPICE2
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spiceinc.h"

void compare_jbuf (int [], int []);
void printBuffer (void *);

void FTN_NAME(tzgetspice2)(unit,buf,data)
int *unit,data[80];
int buf[200];
{
 int ind,i;
 char project[6];
 char target[13];
 int camera_sn,sclk,scet[6];
 int jbuf[200];

 for (i=0; i<200; i++)
 {
    buf[i] = 0;
    jbuf[i] = 0;
 }

 for (i=0; i<80; i++)
   data[i] = -999;

 ind = zgetspice2(*unit,1,buf);
 if (ind != SUCCESS) {
    zvmessage("***ZGETSPICE2 test failed",0); 
    zabend();
 }
 printBuffer((void *)buf);

 sclk = data[1];
 camera_sn = data[5];

 zgetproj (*unit, project, &camera_sn, &sclk, &ind);
 zgetlabcon (*unit, project, data, &ind);

 for (i=0; i<6; i++) scet[i]=data[7+i];
 strncpy(target,&data[24],12);
 ind = zgetspice3(project,target,camera_sn,sclk,scet,1,jbuf);
 compare_jbuf(buf,jbuf);
 ind = zgetspice4(project,1,data,jbuf);
 compare_jbuf(buf,jbuf);
 return;
}
/************************************************************************
 * Compare buf and jbuf.						*
 ************************************************************************/
void compare_jbuf(buf,jbuf)
int buf[200],jbuf[200];
{
  int i;
  char msg[80];

  for (i=0; i<200; i++) {
      if (buf[i] != jbuf[i]) {
         sprintf(msg," ***Buffers do not match, i=%d",i);
         zvmessage(msg,0);
      }
  }
  return;
}
/*========================================================*
 * void printBuffer(): Print the content of the MIPS	  *
 *			buffer				  *
 *========================================================*/
void printBuffer(buf) 
void *buf;
{
 char str[200];
 char msg[200];
 int i;
 buf_union_typ  cbuf;
 memcpy((char *) &cbuf, (char *) buf,
                sizeof(buf_union_typ));


 sprintf(msg,"SC_ID: %d", cbuf.intbuf[0]);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[1], 4);
 str[4] = '\0';
 sprintf(msg,"Inst : %s", str);
 zvmessage(msg,0);
 sprintf(msg,"measurement time year          : %d", cbuf.intbuf[2]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time day           : %d", cbuf.intbuf[3]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time hour          : %d", cbuf.intbuf[4]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time minute        : %d", cbuf.intbuf[5]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time second        : %d", cbuf.intbuf[6]);
 zvmessage(msg,0);
 sprintf(msg,"measurement time millisec      : %d", cbuf.intbuf[7]);
 zvmessage(msg,0);
 sprintf(msg,"Target body code (target_id)   : %d", cbuf.intbuf[8]);
 zvmessage(msg,0);

 if (cbuf.intbuf[9] == J2000)
    strcpy(str, "J2000");
 else if (cbuf.intbuf[9] == B1950)
    strcpy(str, "B1950");
 sprintf(msg,"Coordinate System              : %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[10], 4);
 str[4] = '\0';
 sprintf(msg,"Source file to search          : %s", str);
 zvmessage(msg,0);

 sprintf(msg,"XYZ of SC relative central body: %g %g %g",
                        cbuf.doublebuf[15],
                        cbuf.doublebuf[16],
                        cbuf.doublebuf[17]);
 zvmessage(msg,0);
 sprintf(msg,"XYZ of picture body relative SC: %g %g %g",
                        cbuf.doublebuf[18],
                        cbuf.doublebuf[19],
                        cbuf.doublebuf[20]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from sun              : %g", cbuf.doublebuf[24]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from central body     : %g", cbuf.doublebuf[25]);
 zvmessage(msg,0);
 sprintf(msg,"SC range from picture body     : %g", cbuf.doublebuf[26]);
 zvmessage(msg,0);

 sprintf(msg,"lat & lon of sun rel pic body : %g %g",
                cbuf.doublebuf[27], cbuf.doublebuf[28]);
 zvmessage(msg,0);
 sprintf(msg,"lat & lon of sc rel pic body  : %g %g",
                cbuf.doublebuf[29], cbuf.doublebuf[30]);
 zvmessage(msg,0);

 zvmessage("C_MATRIX:",0);
 for (i=40; i<49; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"lat & lon of P5 point: %g %g",
        cbuf.doublebuf[76], cbuf.doublebuf[77]);
 zvmessage(msg,0);
 sprintf(msg,"incidence angle at P5 point: %g",
                        cbuf.doublebuf[78]);
 zvmessage(msg,0);
 sprintf(msg,"emission  angle at P5 point: %g",
                        cbuf.doublebuf[79]);
 zvmessage(msg,0);
 sprintf(msg,"phase     angle at P5 point: %g",
                        cbuf.doublebuf[80]);
 zvmessage(msg,0);
 sprintf(msg,"Hor & Vert pix size at P5: %g %g",
        cbuf.doublebuf[81], cbuf.doublebuf[82]);
 zvmessage(msg,0);

 zvmessage("ME_MATRIX:",0);
 for (i=49; i<58; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"SC Range to P5 : %g", cbuf.doublebuf[83]);
 zvmessage(msg,0);
 sprintf(msg,"North Angle    : %g", cbuf.doublebuf[67]);
 zvmessage(msg,0);

 sprintf(msg,"Picture body equat radius, long : %g", cbuf.doublebuf[12]);
 zvmessage(msg,0);
 sprintf(msg,"Picture body equat radius, short: %g", cbuf.doublebuf[13]);
 zvmessage(msg,0);
 sprintf(msg,"Picture body polar radius       : %g", cbuf.doublebuf[14]);
 zvmessage(msg,0);

 zvmessage("OM_MATRIX:",0);
 for (i=58; i<66; i+=3) {
    sprintf(msg,"   %g %g %g",
       cbuf.doublebuf[i],cbuf.doublebuf[i+1],cbuf.doublebuf[i+2]);
    zvmessage(msg,0);
 }

 sprintf(msg,"RS-Vector: %g %g %g", cbuf.doublebuf[21],
                cbuf.doublebuf[22], cbuf.doublebuf[23]);
 zvmessage(msg,0);

 sprintf(msg,"line   sub-s/c-point: %g", cbuf.doublebuf[68]);
 zvmessage(msg,0);
 sprintf(msg,"sample sub-s/c-point: %g", cbuf.doublebuf[69]);
 zvmessage(msg,0);

 memcpy((char *) str, (char *)&cbuf.intbuf[168], 4);
 str[4] = '\0';
 sprintf(msg,"Year: %s", str);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[169], 4);
 str[4] = '\0';
 sprintf(msg,"Month_day: %s", str);
 zvmessage(msg,0);
 memcpy((char *) str, (char *) &cbuf.intbuf[170], 4);
 str[4] = '\0';
 sprintf(msg,"Hour_min: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[13], 4);
 str[4] = '\0';
 sprintf(msg,"SPK_ref: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[171], 4);
 str[4] = '\0';
 sprintf(msg,"CK_ref: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[172], 4);
 str[4] = '\0';
 sprintf(msg,"Purpose: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[173], 7);
 str[7] = '\0';
 sprintf(msg,"Prog_name: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[175], 4);
 str[4] = '\0';
 sprintf(msg,"Job_req_no: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[176], 6);
 str[6] = '\0';
 sprintf(msg,"Usr_grp_id: %s", str);
 zvmessage(msg,0);

 memcpy((char *) str, (char *) &cbuf.intbuf[188], 4);
 str[4] = '\0';
 sprintf(msg,"Institution: %s", str);
 zvmessage(msg,0);
 return;
}
$!-----------------------------------------------------------------------------
$ create tgetspice2.imake
/* Imake file for Test of VICAR subroutine GETSPICE2 */
#define PROGRAM tgetspice2
#define MODULE_LIST tgetspice2.f tzgetspice2.c
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

/*********
#define DEBUG
#define LIB_LOCAL
*********/

$!-----------------------------------------------------------------------------
$ create tgetspice2.pdf
!PDF file for VICAR test program TGETSPICE2
PROCESS HELP=*
PARM INP        TYPE=STRING COUNT=1
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
END-PROC
.HELP
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

CDATE specifies the date and time the camera pointing was created.
REQNUM identifies the request number associated with the camera pointing.
PURPOSE identifies the purpose for creating the camera pointing.
PROGRAM identifies the program which created the camera pointing.
SPKID identifies the SP-kernel used to create the camera pointing.
USERID identifies the user who created the camera pointing.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

A complete list of CK and SPK IDs are located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.LEVEL1
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created

.LEVEL2
.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.
.end
$!-----------------------------------------------------------------------------
$ create tputspice2.f
C****************************************************************
C   TPUTSPICE2: Test program for subroutine PUTSPICE2
C****************************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      integer*4 unit,status,ind
      integer*4 ibuf(200)
      logical provenance
      double precision dbuf(100)
      equivalence (dbuf,ibuf)
C      character*5 source
C      integer*4 count, def

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      provenance = .false.
C      call xvparm ('CKNAME', source, count, def, 0)

      call getspice2(unit,provenance,dbuf,ind)
      if (ind .ne. 1) then
         call prnt(4,1,ind,'***GETSPICE2 failed, IND=.')
         call abend
      endif

      call xvmessage('             ',' ')
      call xvmessage( '..................................',' ')
      call xvmessage('PUTSPICE2 test',' ')
C      call putspice2(source,'TPUTSP',dbuf,ind)
      call putspice2 ('NAV2','TPUTSP',dbuf,ind)
      if (ind.ne.1) then
         call prnt(4,1,ind,'***PUTSPICE2 failed, IND=.')
      else
         call xvmessage('PUTSPICE2 test succeeded',' ')      
      endif
CC      call xvmessage('             ',' ')
CC      call xvmessage( '..................................',' ')
CC      call xvmessage('ZPUTSPICE2 test',' ')
CC      call tzputspice2(unit)
      return
      end
$!-----------------------------------------------------------------------------
$ create tzputspice2.c
/************************************************************************/
/* Test program for C subroutine zputspice2
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spiceinc.h"

void FTN_NAME(tzputspice2)(unit, source)
int *unit;
char *source;
{
 int ind,zunit;
 int buf[200];

 zunit = *unit;
 ind = zgetspice2(zunit,0,buf);
 if (ind != SUCCESS) {
    zvmessage("***Call to zgetspice2 failed",0);
    return; 
 }

 ind = zputspice2(source,"TZPUTS",buf);
 if (ind != SUCCESS) zvmessage("***Call to zputspice2 failed",0);
 else zvmessage("Call to zputspice2 succeeded",0);
 }
$!-----------------------------------------------------------------------------
$ create tputspice2.imake
/* Imake file for Test of VICAR subroutine PUTSPICE2 */
#define PROGRAM tputspice2
#define MODULE_LIST tputspice2.f tzputspice2.c
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

/*********
#define DEBUG
#define LIB_LOCAL
*********/

$!-----------------------------------------------------------------------------
$ create tputspice2.pdf
!PDF file for VICAR test program TPUTSPICE2
PROCESS HELP=*
PARM INP        TYPE=STRING COUNT=1
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=0:1			DEFAULT=--
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
END-PROC
.HELP
LOCAL AND REMOTE SPICE ACCESS:

SPICE data may be retrieved and stored from SPICE kernels stored locally
or remotely (e.g. at JPL).  Remote access is via a SPICE server.

SPICEMODE specifies whether local or remote SPICE access is to be used.
If defaulted, SPICEMODE is set to the value of the logical name (or
environmental variable) DEFAULTSPICE.


PARAMETERS FOR RETRIEVING THE INITIAL CAMERA POINTING:

Initial camera pointing data is first retrieved from predict C kernels or
from MIPS C kernels.  The following optional parameters permit the user to
specify where this initial pointing is retrieved:

CKNAME and CKID are alternative ways to specify the C kernel from which camera
pointing is to be retrieved.  For example, CKNAME=FARE or CKID=M904 specifies
that the camera pointing should be retrieved from the file MIPS_FARENC.CK.
The CKID is the unique kernel ID assigned to each C kernel.  When CKID is
specified, it overrides the CKNAME parameter.  A complete list
of kernel IDs is located in the ASCII file assigned the logical name (or
environmental variable) KERNELDB.


PARAMETERS FOR STORING THE IMPROVED CAMERA POINTING:

The following optional parameters are used to store provenance information along
with the improved (C-Smithed) camera pointing computed by the program.  This
provenance information is stored in the (C kernel) segment identifier for the
camera pointing data.  In cases where there are several versions of camera
pointing for an image in a given C kernel, this provenance information can
later be used to retrieve a specific instance of camera pointing from the
kernel.

PURPOSE identifies the purpose for creating the camera pointing.
REQNUM identifies the request number associated with the camera pointing.
CDATE specifies the date and time the camera pointing was created.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

See the level 2 help (via the TAE tutor mode) for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.
          Improved camera pointing will later be stored in the local C kernel
          specific to this program.

           'REMOTE INSTITUTE=DLR USERID=TYR retrieves SPICE data remotely
          (from MIPS) via the SPICE server.  When improved camera pointing is
          stored (at MIPS), provenance data regarding the facility (DLR) and
          user (Thomas Roatsch) who created the data is included.
 
.LEVEL1
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created
.VARI GROUPID
Optional 3-char string
Group which created camera pointing

.LEVEL2
.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  A complete list of valid
target names is located in the ASCII file assigned the logical name (or
environmental variable) BODY_IDS.
If defaulted, the target is retrieved from the VICAR label or other TBD means.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be accessed from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be accessed
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.

Note that if SPICE data is not found in LOCAL or REMOTE mode, the other mode
is attempted in order to retrieve SPICE data.  However, when improved camera
pointing data is stored, only the specified or default mode is used.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used in reading
camera pointing data:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel for reading
camera pointing data (see CKNAME parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which creates
the improved (C-Smithed) camera pointing.  If defaulted, the value of the
logical name (or environmental variable) VICAR_SITE is used.

Ex:  INSTITUTE=ASU identifies ASU as the creator of the improved camera
     pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.  REQNUM must contain exactly 4 digits.

Ex:  REQNUM=0123 identifies (somewhat) request number R000123

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.
The date string must contain exactly 12 digits.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.

If defaulted, the current date and time is used.

.VARI GROUPID
GROUPID is a three character string which identifies the group of the user
running this program to store improved camera pointing.  (The user ID is
automatically determined by the program).

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

On VMS, this parameter is ignored since the program automatically determines
the group ID of the current user.

If GROUPID is defaulted on Unix, the program uses the value of the
environmental variable GROUPID.  Note that GROUPID is not a system-defined
variable, and should be defined in your .cshrc as in the following example:

Ex:  setenv GROUPID 040
.end
$!-----------------------------------------------------------------------------
$ create tstgetspice2.pdf
!*****************************************************************************
! tstgetspice2.pdf - unit test for subroutines getspice2, zgetspice2,
!                    putspice2, and zputspice2
!*****************************************************************************
procedure help=*
  refgbl $echo
  refgbl $syschar
  refgbl $autousage
body
  let _onfail="continue"
  let $echo="yes"
  let  $autousage="none"
  local path string
  local path2 string
  local path3 string

  if ($syschar(1) = "VAX_VMS")
    let path="WMS_TEST_WORK:[TESTDATA.MIPL.GLL]"
    let path2="WMS_GLL:[SSI.UDR]"
    let path3="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
  else
    let path="/project/test_work/testdata/mipl/gll/"
    let path2="/project/gll/ssi/udr/"
    let path3="/project/test_work/testdata/mipl/vgr/"
  end-if

  WRITE "***Please read attached help file for testing instructions"
  WRITE "Testing local mode"
 tputspice2 inp=&"path"venus.img  SPICEMODE=LOCAL CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=COLO REQNUM=0123 CDATE=199603151213
 tputspice2 inp=&"path"venus.img  SPICEMODE=LOCAL CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=MOSA REQNUM=0123 CDATE=199603151213
 tputspice2 inp=&"path"venus.img  SPICEMODE=LOCAL CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=MOSA REQNUM=0456 CDATE=199603151213
 tputspice2 inp=&"path"venus.img  SPICEMODE=LOCAL CKNAME=NAIF +
            INSTITUTE=MIPS PURPOSE=MOSA REQNUM=0456 CDATE=199601221213
 tgetspice2 inp=&"path"venus.img SPICEMODE=LOCAL  CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=COLO REQNUM=0123 CDATE=199603151213
 tgetspice2 inp=&"path"venus.img SPICEMODE=LOCAL  CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=MOSA REQNUM=0123 CDATE=199603151213
 tgetspice2 inp=&"path"venus.img SPICEMODE=LOCAL  CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=MOSA REQNUM=0456 CDATE=199603151213
 tgetspice2 inp=&"path"venus.img SPICEMODE=LOCAL  CKNAME=NAIF +
            INSTITUTE=MIPS PURPOSE=MOSA REQNUM=0456 CDATE=199601221213

  WRITE "Testing remote mode"
 tputspice2 inp=&"path"venus.img  SPICEMODE=REMOTE CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=COLO REQNUM=0123 CDATE=199603151213
 tgetspice2 inp=&"path"venus.img SPICEMODE=REMOTE  CKNAME=NAIF +
            INSTITUTE=USC PURPOSE=COLO REQNUM=0123 CDATE=199603151213

 WRITE "Testing VGR remote mode"
 tgetspice2 inp=&"path3"f1636832.geo spicemode=remote ckname=SEDR target=IO

 tputspice2 inp=&"path3"f1636832.geo spicemode=remote ckname=SEDR target=IO

 WRITE "Testing VGR local mode"
 tgetspice2 inp=&"path3"f1636832.geo spicemode=local ckname=SEDR target=IO

 tputspice2 inp=&"path3"f1636832.geo spicemode=local ckname=SEDR target=IO

end-proc
.help
PROCEDURE FOR TESTING GETSPICE2:

TSTGETSPICE2.PDF tests the subroutines TGETSPICE2 and TPUTSPICE2.  The test
procedure differs from convention because it requires the compiling of two
test programs: TGETSPICE2 and TPUTSPICE2.  All required test files can be
obtained by typing "@GETSPICE2 TEST".  To compile the test programs under VMS:
   $ VIMAKE TGETSPICE2
   $ @TGETSPICE2.BLD
   $ VIMAKE TPUTSPICE2
   $ @TPUTSPICE2.BLD
or for UNIX:
   % vimake tgetspice2
   % make -f tgetspice.make
   % vimake tputspice2
   % make -f tputspice.make

First, we test using local SPICE files.
TPUTSPICE2 stores SPICE data into the SPICE kernels.  TGETSPICE2 then reads the
SPICE data from the SPICE kernels and prints what it got.  The retrieval is
done twice, using the direct FORTRAN and C-bridge.  Any difference between what
is retrieved via FORTRAN and C are reported (no differences should be found).

The above test is repeated for both REMOTE and DEFAULTSPICE cases.  REMOTE
tests using the SPICE server.  DEFAULTSPICE tests the default, which for MIPS is
the SPICE server (some installations will have local as the default).

Disregard the differences in buf(180),buf(181).  This is caused by the string
being zero terminated with garbage following, and has no effect.
.end
$ Return
$!#############################################################################
$Other_File:
$ create getspice2.hlp
1 GETSPICE2, GETSPICE3, GETSPICE4
 
VICAR subroutines GETSPICE2, GETSPICE3, and GETSPICE4 return SPICE data
for an image.  These routines are "wrappers" which call GETSPICE95.

Users should be aware that these routines clear the SPICE kernel pool before
returning, so subsequent calls to SPICE routines will fail unless INIT_SPICE()
or an equivalent routine is called.


FORTRAN calling sequences:

    CALL GETSPICE2(UNIT,PROVENANCE,buf,ind)
    CALL GETSPICE3(PROJECT,TARGET,CAMERA_SN,SCLK,SCET,PROVENANCE,buf,ind)
    CALL GETSPICE4(PROJECT,PROVENANCE,DATA,buf,ind)

where the arguments (where applicable) should be declared as follows:

      INTEGER*4 UNIT		!input logical unit number
      LOGICAL PROVENANCE	!.TRUE.=use provenance, .FALSE.=ignore it
      CHARACTER*5 PROJECT	!GLL, VGR-1, VGR-2, CASSI
      CHARACTER*12 TARGET	!Target name
      INTEGER*4 CAMERA_SN	!Camera serial number
      INTEGER*4 SCLK		!Frame number (FDS for VGR)
      INTEGER*4 SCET(6)		!SCET (year,day,hour,minute,sec,msec)
      INTEGER*4 DATA(100)	!input label buffer (from GETLABCON)
      REAL*8 BUF(100)		!buffer returned by GETSPICE95
      INTEGER*4 IND		!status returned by GETSPICE95

C calling sequences:

    ind = zgetspice2(unit,provenance,buf);
    ind = zgetspice3(project,target,camera_sn,sclk,scet,provenance,buf)
    ind = zgetspice4(project,provenance,data,buf)

        #include "xvmainc.h"
	#include "ftnbridge.h"
	#include "spiceinc.h"
	int unit;		/* logical unit number */
        int provenance;		/* 1=use provenance, 0=ignore it */
        char project[6];	/* GLL, VGR-1, VGR-2, CASSI */
        char target[13];	/* Target name */
        int camera_sn;		/* Camera serial number */
        int sclk;		/* Frame number (FDS for VGR) */
        int scet[6];		/* SCET (year,day,hour,minute,sec,msec) */
        int data[100];		/* input label buffer (from GETLABCON) */
	buf_union_typ *buf;	/* output SPICE/SEDR buffer */
        int ind;		/* return status */

In addition, the calling program must have the following parameters defined in
its PDF:  TARGET, SPICEMODE, CKNAME, CKID, INSTITUTE, PURPOSE, PROGRAM, SPKID,
REQNUM, CDATE, USERID, and GROUPID.  It is strongly urged that the parameter
definitions and accompanying help text be copied from TGETSPICE2.PDF, located
in GETSPICE2.COM.

2 INPUT ARGUMENTS:

     UNIT = logical unit number of image for which SPICE data is required.
     PROVENANCE = .FALSE. (or 0) to ignore provenance parameters, .TRUE. (or 1)
        to use it.
     PROJECT = project name (from GETPROJ)
     TARGET = target name for GLL, or planet name for VGR.
     CAMERA_SN = camera serial number (used for GLL only, where 1=full-frame,
           2=summation mode).
     SCLK = frame number (=100*RIM+MOD91 for GLL, =FDS for Voyager)
     SCET = shutter-centered Spacecraft-Event-Time
     DATA = buffer containing data for VICAR label items (via call to GETLABCON)

  OUTPUT ARGUMENTS:
  
     BUF = 100 element double precision SPICE buffer as returned by GETSPICE95.
           See the help for that routine for details.

     IND = return status (identical to GETSPICE95).
		  1 = Success
                  0 = Failure
		 -1 = Bad User Input
		 -2 = Invalid User's Kernel Id
		 -3 = buf2scet() Failed:Invalid SCET Data
          	 -4 = zutc2et() Failed:Invalid SCET Data
          	 -5 = zsctik() Failed:Invalid SCLK Tolerance
          	 -6 = GetIds2() Failed:Unknown SC instrument
          	 -7 = zpbid() Failed: Unknown Target
          	 -8 = zbodvar() Failed:Invalid Radii Returned
          	 -9 = Cmatrix2() Failed:Invalid C Matrix Returned
         	-10 = LoadSpk2() Failed:Cannot Load SPK Files
         	-11 = zspkssb() Failed:Error Getting SC Pos Vel Vector 
         	-12 = zspkssb() Failed:Cannot Get Target Pos Vel Vector
                -13 = zspkssb() Failed:Cannot Get Center Pos Vel Vector
                -14 = zspkapp() Failed:Cannot Get SC Relative Position
                -15 = zspkapp() Failed:Cannot Get Target Reltv Pstn
                -16 = zspkapp() Failed:Cannot Get Sun Reltv Pstn
                -17 = zbodeul() Failed:Cannot Get Euler Angle
                -18 = zeul2m() Failed:Bad ME-MATRIX Returned
                -19 = zmxmt() Failed:Bad OM-MATRIX Returned
                -20 = zminus() Failed:Bad Target2Sc State returned
                -21 = zreclat() Failed:Bad TCB Lon & Lat
                -22 = zgetcamcon() Failed:Bad Camera Constant
		-23 = ?????????????
                -24 = zcorcav() Failed:Abnormal Return Status(99, -1)
                -25 = zvadd() Failed:Error Adding Matrix
                -26 = zmxv() Failed:Error Multiplying Matrix
                -27 =zreclat(): Error Getting TSB Lat & Lon

2 OPERATION

GETSPICE2, GETSPICE3, GETSPICE4 are easier to use than the underlying routine,
GETSPICE95.  The choice of which routine to call depends on what information is
available to the calling program.

GETSPICE2 scans the VICAR label of the image (referenced by UNIT) via calls to
GETPROJ and GETLABCON to obtain the project name and other label data (DATA
buffer).  GETSPICE4 is then called.  The image (UNIT) must be open before
calling GETSPICE2.

GETSPICE3 handles the case where the application program does not have access
to the image (i.e. UNIT cannot be provided).  The calling program must provide
the information normally contained in the VICAR label in the argument list
(PROJECT, TARGET, CAMERA_SN, SCET).  These values are used to construct a fake
GETLABCON buffer (DATA).  GETSPICE4 is then called.

GETSPICE4 extracts the following information from the GETLABCON buffer:
PROJECT, TARGET, CAMERA_SN, SCET.  GETSPICE4 also retrieves user parameters
TARGET, SPICEMODE, CKNAME, and CKID (via calls to XVPARM) to set up the call
to GETSPICE95.  If the calling program already has access to the GETLABCON DATA
buffer, then it is more efficient to call GETSPICE4 directly since this avoids
a second call to GETLABCON.

Unless the PROVENANCE flag is set to FALSE (=0 for C), the INSTITUTE, PURPOSE,
PROGRAM, SPKID, REQNUM, CDATE, USERID and GROUPID parameters are also
retrieved and stored in the GETSPICE95 userinfo buffer.

A reason for ignoring the provenance parameters during a GETSPICE2 call is for
programs like FARENC which both retrieve and store SPICE data.  For FARENC,
the provenance parameters are not used to retrieve SPICE data but rather to
store provenance data via a call to PUTSPICE2.

2 REMOTE SPICE ACCESS

As mentioned above, the calling program must have the SPICEMODE defined in its
PDF.  If the value for SPICEMODE is 'LOCAL, then the SPICE data is retrieved
from local (disk) storage.  If the value for SPICEMODE is 'REMOTE, then the
SPICE data is retrieved via a remote server.


2 HISTORY

Original programmer: Gary Yagi Mar 15, 1996
Current cognizant programmer:  Gary Yagi
Revisions:

28 Nov 01  GMY  Add Cassini capability.
25 Aug 1998 TXH Removed subroutine 'vgr_date()' and added call to 
                'scet_update()', which is an enhanced version of 'vgr_date()'.
13 May 98  TXH  Added subroutine 'vgr_date()' to correct Voyager date formate
                input to msserver.  Note: Voyager date has 2-digit year and 
                <-999> for msec.  The new subroutine adds a 100-year date window
                for Voyager data.
29 Apr 98  GMY  Updated to access VGR SPICE data via new msserver.
01 Oct 96  GMY  Add GETSPICE3 and GETSPICE4 routines.
15 Aug 96  SMC  Added code to distinguish between Full Frame and Summation Mode 
                  when processing GLL data.  And pass it on to GETSPICE95 with 
                  the CAMERA parameter.  (DFR)
10 Jun 96  SMC  Fixed the USR_INFO case conversion.  (DFR)
29 May 96  GMY  Convert CKNAME parameter to uppercase (DFR)
01 APR 96  GMY  Update to changes in GETSPICE95 (FR.....)
$!-----------------------------------------------------------------------------
$ create putspice2.hlp
1 PUTSPICE2

VICAR subroutine PUTSPICE2 stores camera pointing data for an image.  The
routine is currently limited to Galileo.  Voyager implementation to follow.

PUTSPICE2 extracts information input via user parameters to set up the call to
PUTSPICE95.  It then calls PUTSPICE95 and returns the status from that call.

FORTRAN calling sequence:

    CHARACTER*4 CKNAME		!NAIF,FARE,NAV,NAV2,DAVI,AMOS, or NEAR
    CHARACTER*6 PROGRAM		!first 6 characters of program name
    INTEGER*4 IND		!status returned by PUTSPICE95
    REAL*8 BUF(100)		!SPICE buffer input to PUTSPICE95
    INTEGER*4 IBUF(200)
    EQUIVALENCE (BUF,IBUF)

    CALL PUTSPICE2(CKNAME,PROGRAM,BUF,ind)

  All arguments are inputs except for the return status.

C calling sequence:

    #include "xvmaininc.h"
    #include "ftnbridge.h"
    #include "spiceinc.h"
    char ckname[5];
    char program[7];
    buf_union_typ  *buf;
    int ind;

    ind = zputspice2(ckname,program,buf);

  All arguments are inputs.

In addition, the calling program must have the following parameters defined in
its PDF:  SPICEMODE, INSTITUTE, PURPOSE, REQNUM, CDATE, and GROUPID.  It is strongly
urged that the parameter definitions and accompanying help text be copied from
TPUTSPICE2.PDF, located in GETSPICE2.COM.

2 INPUT ARGUMENTS:

  CKNAME determines the C kernels into which the camera pointing will be stored:

    CKNAME	C KERNEL
    --------    -------------
    DAVI	MIPS_DAVI.CK
    NAV		MIPS_NAV.CK
    FARE	MIPS_FARENC.CK
    NAV2	MIPS_NAV2.CK
    NEAR	MIPS_NEAR.CK
    AMOS	MIPS_AMOS.CK

  PROGRAM = first 6 characters of the calling program.
  BUF = 100 element double precision SPICE buffer to be passed to PUTSPICE95.
        See the help for routine GETSPICE95 for a full description of BUF.
  
  OUTPUT ARGUMENTS:

    IND = status of call
             1 = Success
             0 = Failure
            -1 = Unrecognizable Project Id
            -2 = Error Opening Kernel File
            -3 = Cannot Update NAIF KERNEL
            -4 = Error Packing Seg. Descriptor
            -5 = Error Starting New Array
            -6 = Error Adding New Array
            -7 = Daf-file Not Close Properly
            -8 = Bad User Input Data

2 OPERATION

Before calling PUTSPICE2, the following elements of BUF must be correctly
filled:

  IBUF(1) = spacecraft ID consistent with NAIF convention (GLL=-77,
            VGR-1= -31, VGR-2= -32)
  IBUF(2) = instrument (ISSN = VGR NA, ISSW=VGR WA, SSI=GLL camera)
  IBUF(3)-IBUF(9) = spacecraft event time
  IBUF(179)-IBUF(181) = target name

Normally, these buffer locations are set by retrieving the predict SPICE data
via a previous call to GETSPICE2.

Also, the improved camera pointing must be stored in BUF(41)-BUF(49).

PUTSPICE2 uses the SPICEMODE parameter to determine whether to store the
improved camera pointing in local SPICE kernels or via the remote SPICE server.
If SPICEMODE is defaulted, the value of the logical name DEFAULTSPICE is used.

PUTSPICE2 also retrieves the INSTITUTE, PURPOSE, PROGRAM, REQNUM, CDATE, and
GROUPID parameters.  If INSTITUTE is defaulted, the value of the logical name
VICAR_SITE is used.  On VAX-VMS, the GROUPID parameter is ignored.  The
group ID is automatically extracted from the last 3 characters of the user ID.
If GROUPID is defaulted on UNIX, the value of the environment variable GROUPID
is used.

The above data is stored in BUF as follows:

  IBUF(11) = CKNAME (e.g. FARE)
  IBUF(169)-IBUF(171) = CDATE  (e.g. 199603151213)
  IBUF(173) = PURPOSE (e.g. COLOr reconstruction)
  IBUF(174)-IBUF(175) = PROGRAM (e.g. FARENC)
  IBUF(176) = REQNUM  (e.g. 0123)
  IBUF(177) = USERID  (e.g. GMY)
  IBUF(178) = GROUPID (e.g. 059)
  IBUF(189) = INSTITUTE (e.g. MIPS)

PUTSPICE95 is then called.  The C-matrix is copied from BUF and stored as a
segment in the C-kernel, along with the spacecraft ID.  The provenance
information is transferred from BUF and stored in the 40 character segment ID
as follows:

                               1         2         3         4
  Segment ID column:  1234567890123456789012345678901234567890
  Contents:           MIPSCOLOFARENCN0150123199603151213GMY059

2 HISTORY

  Original progammer:  Gary Yagi, Mar 25, 1996
  Current cognizant programmer:  Gary Yagi
  Revisions:  New
$ Return
$!#############################################################################
