$!****************************************************************************
$!
$! Build proc for MIPL module cylpatch
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:46
$!
$! Execute by entering:		$ @cylpatch
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
$ write sys$output "*** module cylpatch ***"
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
$ write sys$output "Invalid argument given to cylpatch.com file -- ", primary
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
$   if F$SEARCH("cylpatch.imake") .nes. ""
$   then
$      vimake cylpatch
$      purge cylpatch.bld
$   else
$      if F$SEARCH("cylpatch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cylpatch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cylpatch.bld "STD"
$   else
$      @cylpatch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cylpatch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cylpatch.com -mixed -
	-s cylpatch.f zcylpatch.c -
	-i cylpatch.imake -
	-t tcylpatch.f tzcylpatch.c tcylpatch.imake tcylpatch.pdf -
	   tstcylpatch.pdf -
	-o cylpatch.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cylpatch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE CYLPATCH (rdata)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                          CYLPATCH
C       ----------------                                          --------
C	General routine for computing ZC, the line intersecting the 
C       equator, CPSI, the longitude at sample 1, and CSAM00, 
C       the sample at longitude zero.
C
C	Fortran format of call:
C
C	CALL CYLPATCH (RDATA)
C
C	"C" format of call:
C
C	cylpatch (rdata);
C
C	Parameters:-
C
C	RDATA   (input/output)  REAL Input Matrix.
C
C   REVISION HISTORY
C
C      20-05-94   CRI  MSTP S/W Conversion (VICAR Porting)
C      11-05-98   RRP  Updated tcylpatch to compile under
C                 vms system by merging the divided string
C                 parameter to xvmessage to max possible
C                 string. 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     this code was lifted from MAP2 cylcen subroutine
      implicit none
      real*4 rdata(40),PIFAC
      real*4 cphi,cpsi,xc,zc,f,req,rpole,fl
      real*4 phi,sinphi,rfl,radius,CSAM00

      integer*4 l

      data PIFAC/.017453293/

      call  ifmessage ('CYLPATCH version 11-May-98') 

      call mve(4,1,rdata(39),l,1,1)  ! Check Type of Data
      if(l.ne.9)return

      cphi=rdata(3)
      cpsi=rdata(6)
      xc=rdata(1)
      zc=rdata(2)
      f=rdata(7)
      req=rdata(26)
      rpole=rdata(25)
      fl=rpole

C     THIS ROUTINE COMPUTES ZC, THE LINE INTERCEPTING THE EQUATOR,
C     CPSI = THE LONGITUDE AT SAMPLE 1, AND CSAM00 = THE SAMPLE OF LONG ZERO.

      IF(xc.EQ.1..AND.cphi.EQ.0.)GO TO 50
C        GET W. LONG AT SAMPLE 1
2        cpsi=f*(xc-1.)/req/PIFAC+cpsi
         xc=1.
         cpsi=AMOD(720.+cpsi,360.)
C        FIND LINE OF EQUATOR
         phi=cphi*PIFAC
         sinphi=SIN(phi)
         rfl=(req/fl)**2                               !new
         radius=req/(f*sqrt(rfl+(1.-rfl)*cos(phi)**2)) !new
         zc=zc+sinphi*radius                           !new

C        ROUND
         zc=FLOAT(INT(zc+0.5))
         cphi=0.
         rdata(2)=zc
         rdata(3)=cphi

C        CALCULATE SAMP OF LONG ZERO AND ROUND
50    CSAM00=req*cpsi*PIFAC/f+1.
      CSAM00=FLOAT(INT(CSAM00+0.5))

C     ADJUST LONGITUDE OF SAMPLE 1.
      cpsi=f*(CSAM00-1.)/req/PIFAC
      rdata(1)=CSAM00
      rdata(6)=cpsi
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zcylpatch.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcylpatch - Compute Line intercepting equator 	*/
/************************************************************************/
struct data
  {
  float rdata[38];
  int idata;
  float rdata40;
  };

void zcylpatch (rdata)
struct data  *rdata;			/* input structure of data */

{

FTN_NAME2(cylpatch, CYLPATCH) ( rdata); /* invoke cylpatch */

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cylpatch.imake
/***********************************************************************

                IMAKE FILE FOR VICAR SUBROUTINE cylpatch

   To Create the build file give the command:

		$ vimake cylpatch			(VMS)
   or
		% vimake cylpatch			(Unix)


************************************************************************/

#define SUBROUTINE cylpatch

#define MODULE_LIST cylpatch.f zcylpatch.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tcylpatch.f
C************************************************************************
C  This test routine is used to test CYLPATCH, a FORTRAN subroutine.
C  CLYPATCH  computes the special line and sample point and special
C  latitude and longitude points for the normal cylindrical projection.
C  This test routine builds the necessary data in a standard MAP 
C  buffer.  After testing the subroutine using this FORTRAN driver, a
C  "C" version is invoked "tzcylpatch".  tzcylpatch uses a bridge
C  "zcylpatch" to invoke CYLPATCH.  The test cases are the same, only
C  in "C".
C************************************************************************
C  RDATA is a real*4 40 element array as described in CONVEV.
C   rdata(1) - sample 
C   rdata(2) - line
C   rdata(3) - Latitude 
C   rdata(6) - Longitude 
C   rdata(7) - scale (km/pixel)
C   rdata(39) - projection type = 9
C   rdata(25) - polar radius (km)
C   rdata(26) - equatorial radius (km)
C  Some values will be changed after execution of CYLPATCH.
C   rdata(1) - special sample point
C   rdata(2) - special line point
C   rdata(3) - Latitude at sample 1
C   rdata(6) - Longitude (west) at sample 1
C  If rdata(39) is not equal to 9 (integer), then data is not for a
C  cylindrical projection. CYLPATCH returns without making any changes.
C************************************************************************
  
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      real rdata(40)
      integer idata(40),j
      character*80 msg
      equivalence(idata(1),rdata(1))
      idata(39)=9
      rdata(1)=1.
      rdata(2)=1.
      rdata(3)=85.7461
      rdata(6)=239.916
      rdata(7)=10.
      rdata(25)=1815.
      rdata(26)=1815.
      call xvmessage
     +     ('at line=1. sample=1. lati=85.7461 long=239.916',' ')
      call xvmessage('radius=1815., scal=10',' ')
      write(msg,40)(RDATA(j),j=25,26)
      call xvmessage(msg,' ')
      write(msg,50)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call cylpatch(rdata)
      call xvmessage('output should be lati=0 at line=182, samp=761 long
     *=239.916',' ')
      write(msg,41) RDATA(1)
      call xvmessage(msg,' ')
      write(msg,42) RDATA(2)
      call xvmessage(msg,' ')
      write(msg,43) RDATA(3)
      call xvmessage(msg,' ')
      write(msg,44) RDATA(6)
      call xvmessage(msg,' ')
      write(msg,52)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      rdata(1)=100.
      rdata(2)=100.
      rdata(3)=26.8586
      rdata(6)=208.6638
      call xvmessage
     +    ('at line=100,samp=100,lati=26.8586,long=208.6638',' ')
      write(msg,50)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call cylpatch(rdata)
      call xvmessage('output should be lati=0 at line=182, samp=761 long
     *=239.916', ' ')
      write(msg,41) RDATA(1)
      call xvmessage(msg,' ')
      write(msg,42) RDATA(2)
      call xvmessage(msg,' ')
      write(msg,43) RDATA(3)
      call xvmessage(msg,' ')
      write(msg,44) RDATA(6)
      call xvmessage(msg,' ')
      write(msg,52)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call xvmessage('NOW TRY "C" INTERFACE',' ')
      call tzcylpatch 
40    format ('RADII=',2f10.4)
41    format ('sample=',f12.5)
42    format ('line=  ',f12.5)
43    format ('lati=  ',f12.5)
44    format ('long=  ',f12.5)
50    format ('input  data=',5f12.7)
51    format ('            ',5f12.7)
52    format ('output data=',5f12.7)
      return
      end
$!-----------------------------------------------------------------------------
$ create tzcylpatch.c
#include "xvmaininc.h"
#include "ftnbridge.h"

#define headin  "input  data="
#define headout "output data="
#define headblk "            "

  struct data  /*Standard MAP data Structure */
      {
      float rdata[38];
      int idata;
      float rdata40;
      } data;

tzprintio (headr)   /* Print routine */
   char *headr;
  {
      int i;
      char msg[80], msg1[80], *mp, *m1p;
      mp=msg;
      m1p=msg1;
      strcpy(msg,headr);
      strcpy(msg1,headblk);
      mp = mp + 12;
      m1p = m1p + 12;
      for (i=0; i<5; i++)
         {
         (void) sprintf(mp,"%12.7f",data.rdata[i]);
         mp=mp+12;
         (void) sprintf(m1p,"%12.7f",data.rdata[i+5]);
         m1p=m1p+12;
         }
      zvmessage(msg,"");
      zvmessage(msg1,"");
      zvmessage(""," ");
   }
/************************************************************************/
/*   Main Test routine for the "C" call to cylpatch.                    */
/************************************************************************/

void FTN_NAME(tzcylpatch)() 
   {
      int i;
      char msg[80], msg1[80], *mp, *m1p;
      mp=msg;
      m1p=msg1;
      for (i=0; i<38; i++) data.rdata[i] = 0.0;
      data.rdata40=0.0;
      data.idata=9;
      data.rdata[0]=1.0;
      data.rdata[1]=1.0;
      data.rdata[2]=85.7461;
      data.rdata[5]=239.916;
      data.rdata[6]=10.0;
      data.rdata[24]=1815.0;
      data.rdata[25]=1815.0;

      zvmessage("at line=1. sample=1. lati=85.7461 long=239.916"," ");
      zvmessage("radius=1815., scal=10","");
      (void) sprintf(mp,"RADII= %9.4f %9.4f",data.rdata[24],data.rdata[25]);
      zvmessage(msg,"");
      tzprintio(headin);
      zcylpatch(&data);
      zvmessage
      ("output should be lati=0 at line=182, sample=761 long=239.916"," ");
      (void) sprintf(msg,"sample=%12.5f",data.rdata[0]);
      zvmessage(msg,"");
      (void) sprintf(msg,"line=  %12.5f",data.rdata[1]);
      zvmessage(msg,"");
      (void) sprintf(msg,"lati=  %12.5f",data.rdata[2]);
      zvmessage(msg,"");
      (void) sprintf(msg,"long=  %12.5f",data.rdata[5]);
      zvmessage(msg,"");
      tzprintio(headout);

  /* second case  */  

      data.rdata[0]=100.0;
      data.rdata[1]=100.0;
      data.rdata[2]=26.8586;
      data.rdata[5]=208.6638;

      zvmessage("at line=100,samp=100,lati=26.8586,long=208.6638"," ");
      tzprintio(headin);
      zcylpatch(&data);
      zvmessage
      ("output should be lati=0 at line=182, sample=761 long=239.916", " ");
      (void) sprintf(msg,"sample=%12.5f",data.rdata[0]);
      zvmessage(msg,"");
      (void) sprintf(msg,"line=  %12.5f",data.rdata[1]);
      zvmessage(msg,"");
      (void) sprintf(msg,"lati=  %12.5f",data.rdata[2]);
      zvmessage(msg,"");
      (void) sprintf(msg,"long=  %12.5f",data.rdata[5]);
      zvmessage(msg,"");
      tzprintio(headout);
   }
$!-----------------------------------------------------------------------------
$ create tcylpatch.imake
/* Imake file for Test of VICAR subroutine CYLPATCH */

#define PROGRAM tcylpatch

#define MODULE_LIST tcylpatch.f tzcylpatch.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_LOCAL
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tcylpatch.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstcylpatch.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
tcylpatch
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cylpatch.hlp
1 CYLPATCH
 Fixes up the CONVEV data buffer (standard MAP buffer) to conform to 
 standards for normal cylindrical projections.

 Calling Sequence:

  FORTRAN Calling Sequence:  CYLPATCH(RDATA)
  C Calling Sequence:        zcylpatch(rdata);

 Arguments: 
  RDATA is a real*4 40 element array as described in CONVEV.
   rdata(1) - sample 
   rdata(2) - line
   rdata(3) - Latitude 
   rdata(6) - Longitude 
   rdata(7) - scale (km/pixel)
   rdata(39) - projection type = 9
   rdata(25) - polar radius (km)
   rdata(26) - equatorial radius (km)
  Some values will be changed after execution of CYLPATCH.
   rdata(1) - special sample point
   rdata(2) - special line point
   rdata(3) - Latitude at sample 1
   rdata(6) - Longitude (west) at sample 1
  If rdata(39) is not equal to 9 (integer), then data is not for a
  cylindrical projection. CYLPATCH returns without making any changes.
  
2 History
  Original Programmer: Joel Mosher   16-APR-1986
  Current Cognizant Programmer: Joel Mosher
  Made portable for UNIX  RNR(CRI)   01-JUL-94 

  Source Language: Fortran

2 Operation
  The user of CONVEV often knows only the line,sample,latitude and 
longitude of a point on a planet. If those values are input to CONVEV,
errors result in the case that the input image is in the Normal 
Cylindrical projection (image type 9). For this type image, CONVEV
expects data(1) to be the sample of longitude 0 and data(6) to be the
longitude of sample 1.  If the CONVEV data buffer is in the form of the
former, CYLPATCH will convert it to the form of the latter. CYLPATCH 
should only be called once on a data buffer, otherwise the buffer 
gets bad values put in it.
  CYLPATCH computes the special line and sample point and special
latitude and longitude points for the normal cylindrical projection.

$ Return
$!#############################################################################
