$!****************************************************************************
$!
$! Build proc for MIPL module volabv2
$! VPACK Version 1.7, Tuesday, March 29, 1994, 12:49:26
$!
$! Execute by entering:		$ @volabv2
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
$ write sys$output "*** module volabv2 ***"
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
$ write sys$output "Invalid argument given to volabv2.com file -- ", primary
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
$   if F$SEARCH("volabv2.imake") .nes. ""
$   then
$      vimake volabv2
$      purge volabv2.bld
$   else
$      if F$SEARCH("volabv2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake volabv2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @volabv2.bld "STD"
$   else
$      @volabv2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create volabv2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack volabv2.com -
	-s volabv2.f zvolabv2.c -
	-i volabv2.imake -
	-t tvolabv2.f tzvolabv2.c tvolabv2.imake tvolabv2.pdf tstvolabv2.pdf -
	-o volabv2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create volabv2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C***********************************************************************
C
C     VO75 LABEL PROCESSI%NG SUBROUTINE
C
C     22 AUG 1977  ...RMR... INITIAL RELEASE
C     24-APR-1986  ...JAM... convert to Vicar2
C     10-JUL-1991  ...CCA... delete opens,closes-increase buffer to 7200
C                            add PICNO,SCET
C     29-JUL-1993  ...TLT... PORTED TO UNIX
c
C***********************************************************************
      SUBROUTINE volabv2(IND,unit,LBUF)
      INTEGER*4 LBUF(40),unit,stat,bufsize,ptr
      CHARACTER*7200 LABI
      CHARACTER*8 MISC
      data MISC/'!       '/

C-----INITIALIZE ARRAYS------------
      ind=0
      call mve(4,16,-999,lbuf,0,1)

      bufsize=0
      call xlgetlabel(unit,labi,bufsize,stat,' ')
      if(bufsize.gt.7200)then
          call prnt(4,1,bufsize,
     *    ' volab buffer not big enough for label,size=.')
          ind=20
          return
      endif
      bufsize=7200
      call xlgetlabel(unit,labi,bufsize,stat,' ')
      call chkstat(stat,' err in xlgetlabel in volabv2,stat=',1,stat,1)

C-----FLIGHT FORMAT EDR LABEL PROCESSOR------------

1000  lbuf(1)=2
      if(index(labi(1:bufsize),'VO75 1A').gt.0)then
             lbuf(2)=7
             lbuf(3)=1
             lbuf(4)=1
      else if(index(labi(1:bufsize),'VO75 1B').gt.0)then
             lbuf(2)=4
             lbuf(3)=1
             lbuf(4)=2
      else if(index(labi(1:bufsize),'VO75 2A').gt.0)then
             lbuf(2)=8
             lbuf(3)=2
             lbuf(4)=1
      else if(index(labi(1:bufsize),'VO75 2B').gt.0)then
             lbuf(2)=6
             lbuf(3)=2
             lbuf(4)=2
      else
             ind=20  ! not a valid label
             return
      endif
c
      ptr=index(labi(1:bufsize),'FILTER')
      read (labi(ptr+6:),'(BN,i2)') lbuf(8)      ! filter
c
      ptr=index(labi(1:bufsize),'EXP')
      read (labi(ptr+3:),'(BN,i5)') lbuf(9)        ! exposure
c
      ptr=index(labi(1:bufsize),' FGD ')
      if (labi(ptr+5:ptr+5) .eq. '0') lbuf(10) = 0    ! flood state
      if (labi(ptr+5:ptr+5) .eq. '1') lbuf(10) = 1
      if (labi(ptr+6:ptr+6) .eq. '0') lbuf(11) = 0    ! gain state
      if (labi(ptr+6:ptr+6) .eq. '1') lbuf(11) = 1
      if (labi(ptr+7:ptr+7) .eq. '0') lbuf(12) = 0    ! dc offset
      if (labi(ptr+7:ptr+7) .eq. '1') lbuf(12) = 1
c
      ptr=index(labi(1:bufsize),'FSC')
      read (labi(ptr+3:),'(BN,i10)') lbuf(7)            ! fsc
c
      ptr=index(labi(1:bufsize),'OET-GMT')			!oet=scet?
      read (labi(ptr+7:),'(BN,i4)') lbuf(19)      ! scet year
      read (labi(ptr+11:),'(BN,i4)') lbuf(20)      ! scet day
      read (labi(ptr+15:),'(BN,i3)') lbuf(21)      ! scet hour
      read (labi(ptr+18:),'(BN,i3)') lbuf(22)      ! scet min
      read (labi(ptr+21:),'(BN,i3)') lbuf(23)      ! scet sec
      lbuf(24)=0  				! scet millisec

c     if bad values (*) for range,fov and scale .... return -999
      ptr=index(labi(1:bufsize),'RNG=')
      if (ptr .gt. 0 .and. labi(ptr+7:ptr+7) .ne. '*') then  
             read (labi(ptr+4:),'(BN,i6)') lbuf(16)     ! range
             ptr=index(labi(1:bufsize),'HFOV=')
             read (labi(ptr+5:),'(BN,i5)') lbuf(15) ! fov width
             ptr=index(labi(1:bufsize),'VFOV=')
             read (labi(ptr+5:),'(BN,i5)') lbuf(14)     ! fov height
             ptr=index(labi(1:bufsize),'SCL=')
             read (labi(ptr+4:),'(BN,i5)') lbuf(13)     ! scale in m/pxl
      endif
c
      call mvcl(misc(1:),lbuf(17),8)
      ptr=index(labi(1:bufsize),' PICNO')
      if (ptr .gt. 0) call mvcl(labi(ptr+7:),lbuf(17),8)
c
c      call xvclose(unit,stat,' ')
c      call chkstat(stat,' ERROR IN XVCLOSE,IND=',1,stat,1)
c
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zvolabv2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of VOLABV2                                         */
/************************************************************************/

void zvolabv2(ind,unit,lbuff) 
  int    *ind;     /* returned status (output) */
  int    unit;    /* the unit of the file to be read (input) */
  void  *lbuff;   /* a 40 longword workspace buffer  (input) */


{
FTN_NAME(volabv2)(ind,&unit,lbuff); 
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create volabv2.imake
/* Imake file for VICAR subroutine VOLABV2 */

#define SUBROUTINE volabv2

#define MODULE_LIST volabv2.f zvolabv2.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tvolabv2.f
C--------------------------------------------------------------
C THIS IS A TEST OF MODULE VOLABV2
C 
C PORTED TO UNIX 7/29/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      character*8 msg
      INTEGER*4 BUF(40)
      call xvmessage('**************fortran callable***********',' ')
c
      CALL XVUNIT(UNIT,'INP',1,STAT,' ')

      CALL CHKSTAT(STAT,' ERR IN XVNIT, STAT=',1,STAT,1)
      CALL XVOPEN(UNIT,STAT,' ')
      CALL CHKSTAT(STAT,' ERR IN XVOPEN, STAT=',1,STAT,1)
c
      CALL VOLABV2(IND,UNIT,BUF)
c
      CALL PRNT(4,1,IND,' IND=.')
      CALL PRNT(4,1,BUF(1), ' DATA FORMAT    = .')
      CALL PRNT(4,1,BUF(2), ' CAMERA SERIAL #= .')
      CALL PRNT(4,1,BUF(3), ' SERIAL #  =      .')
      CALL PRNT(4,1,BUF(4), ' CAMERA   =       .')
      CALL PRNT(4,1,BUF(5), ' FRAME #  =       .')
      CALL PRNT(4,1,BUF(7), ' FSC #    =       .')
      CALL PRNT(4,1,BUF(8), ' FILTER POS  =    .')
      CALL PRNT(4,1,BUF(9), ' EXPOSURE    =    .')
      CALL PRNT(4,1,BUF(10),' FLOOD STATE  =   .')
      CALL PRNT(4,1,BUF(11),' GAIN STATE   =   .')
      CALL PRNT(4,1,BUF(12),' DC OFFSET STATE= .')
      CALL PRNT(4,1,BUF(13),' SCALE IN M/PIXEL=.')
      CALL PRNT(4,1,BUF(14),' FOV HEIGHT   =   .')
      CALL PRNT(4,1,BUF(15),' FOV WIDTH    =   .')
      CALL PRNT(4,1,BUF(16),' RANGE        =   .')
      msg = '        '
      CALL MVLc(BUF(17),MSG,8)
      CALL XVMESSAGE(' PICNO=       ',' ')
      CALL xvmessage(MSG,'                  ')

      CALL PRNT(4,1,BUF(19),' SCET YEAR    =   .')
      CALL PRNT(4,1,BUF(20),' SCET DAY     =   .')
      CALL PRNT(4,1,BUF(21),' SCET HOUR    =   .')
      CALL PRNT(4,1,BUF(22),' SCET MIN     =   .')
      CALL PRNT(4,1,BUF(23),' SCET SEC     =   .')
      CALL PRNT(4,1,BUF(24),' SCET MS      =   .')

      call xvmessage('**************c callable***********',' ')
c

      CALL tzVOLABV2(IND,UNIT,BUF)
c
      CALL PRNT(4,1,IND,' IND=.')
      CALL PRNT(4,1,BUF(1), ' DATA FORMAT    = .')
      CALL PRNT(4,1,BUF(2), ' CAMERA SERIAL #= .')
      CALL PRNT(4,1,BUF(3), ' SERIAL #  =      .')
      CALL PRNT(4,1,BUF(4), ' CAMERA   =       .')
      CALL PRNT(4,1,BUF(5), ' FRAME #  =       .')
      CALL PRNT(4,1,BUF(7), ' FSC #    =       .')
      CALL PRNT(4,1,BUF(8), ' FILTER POS  =    .')
      CALL PRNT(4,1,BUF(9), ' EXPOSURE    =    .')
      CALL PRNT(4,1,BUF(10),' FLOOD STATE  =   .')
      CALL PRNT(4,1,BUF(11),' GAIN STATE   =   .')
      CALL PRNT(4,1,BUF(12),' DC OFFSET STATE= .')
      CALL PRNT(4,1,BUF(13),' SCALE IN M/PIXEL=.')
      CALL PRNT(4,1,BUF(14),' FOV HEIGHT   =   .')
      CALL PRNT(4,1,BUF(15),' FOV WIDTH    =   .')
      CALL PRNT(4,1,BUF(16),' RANGE        =   .')
      msg = '        '
      CALL MVLc(BUF(17),MSG,8)
      CALL XVMESSAGE(' PICNO=       ',' ')
      CALL xvmessage(MSG,'                  ')

      CALL PRNT(4,1,BUF(19),' SCET YEAR    =   .')
      CALL PRNT(4,1,BUF(20),' SCET DAY     =   .')
      CALL PRNT(4,1,BUF(21),' SCET HOUR    =   .')
      CALL PRNT(4,1,BUF(22),' SCET MIN     =   .')
      CALL PRNT(4,1,BUF(23),' SCET SEC     =   .')
      CALL PRNT(4,1,BUF(24),' SCET MS      =   .')

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzvolabv2.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVOLABV2.F */
/************************************************************************/

void FTN_NAME(tzvolabv2)(ind,unit,lbuff) 
  int   *ind;     /* returned status (output) */
  int   *unit;    /* the unit of the file to be read (input) */
  void  *lbuff;   /* a 40 longword workspace buffer  (input) */

{
       zvolabv2(ind,*unit,lbuff);
}

$!-----------------------------------------------------------------------------
$ create tvolabv2.imake
/* Imake file for Test of VICAR subroutine volabv2 */

#define PROGRAM tvolabv2

#define MODULE_LIST tvolabv2.f tzvolabv2.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define LIB_LOCAL*/ 
/*must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
$!-----------------------------------------------------------------------------
$ create tvolabv2.pdf
!*****************************************************************************
! TVOLABV2.PDF - pdf for test program TVOLABV2.F for the subroutine VOLABV2
!*****************************************************************************
PROCESS
parm inp type=string
END-PROC
$!-----------------------------------------------------------------------------
$ create tstvolabv2.pdf
!****************************************************************************
! TSTVOLABV2_VMS.PDF, unit test procedure for subroutine VOLABV2.F.
!   Testers: please read the unit test for information!
!
!   This pdf assumes that if you are not on a unix host, then you are on
!   a  vax-vms host.
!*****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="no"
write "******************************"
write "          NOTE TO TESTER:"
write "******************************"
Write  " "
Write  " The Following Test Data are handled separately for VMS and UNIX. "
Write  " Currently for the UNIX, in order to run the program, the data"
Write  " files MUST be copied to the LOCAL directory where the program"
Write  " resides."
write " "
refgbl $syschar

local img1       type=string             !...218s01.img
local img2    type=string             !...218s02.img

if ($syschar(1) = "UNIX")
  let img1 = "218s01.img"
  let img2 = "218s02.img"
else ! VAX format
  let img1 = "VIKING:218s01.img"
  let img2 = "VIKING:218s02.img"
end-if
let  $echo="yes"

label-list @img1
tvolabv2 inp=@img1

label-list @img2
tvolabv2 inp=@img2

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create volabv2.hlp
1 VOLABV2
 
  Subroutine to read Viking Orbiter labels

  FORTRAN Calling Sequence:  	INTEGER*4 IND,UNIT,LBUF(40)
				
				CALL VOLABV2( IND, UNIT, LBUF)

  C Calling Sequence:  		int ind,unit,lbuf[40];
				
				zvolabv2( &ind, unit, lbuf)

2 ARGUMENTS

      IND   - an indicator returned   
            IND can have the values of Fortran error messages or
                 IND=0   , normal return
                 IND =20 , the input label could not be determined.
            any other value of IND is a FORTRAN error
      UNIT  - the unit of the file to be read, input
      LBUF  - a 40 longword workspace buffer, output
       word          parameter
        1            data format
        2            camera serial number
        3            spacecraft serial number
        4            camera  1=a 2=b
        5            frame number
        6            unused
        7            Frame Start Count (FSC)
        8            filter position
        9            exposure
       10            flood state
       11            gain state
       12            DC offset state
       13            scale in m/pixel
       14            FOV  height
       15            FOV width
       16            range
       17-18         PICNO   (ascii)
       19            OET (SCET?) YEAR - 1900
       20            OET (SCET?) DAY
       21            OET (SCET?) HOUR
       22            OET (SCET?) MINUTE
       23            OET (SCET?) SECOND
       24            OET (SCET?) MILLISECOND (ALWAYS ZERO)
       25-40         unused

2 METHOD 

  VOLABV2 opens the input file for input,reads the first two
  thousand bytes, extracts the parameters listed, and then closes
  the input file. Certain parameters may not have valid values (*) in 
  the label (or may be missing) in which case the value will be 
  returned as -999.  If PICNO, is missing, it will be returned as the 
  string '!       '.

2 HISTORY

 Ported to UNIX: Thuy Truong  29-JUL-1993

 Original Programmer: Reuben Ruiz 21-MAY-1975

 Current Cognizant Programmer: Charles Avis

 Revision: 1     24-APR-1986 convert to Vicar2
           2     10-JUL-1991 delete opens and closes, increase buffer to 7200
                             ADD SCET
	   3     29-JUL-1993 ported to UNIX
           4     29-MAR-1994  ...TLT... Fixed tstvolabv2.pdf for FR83061.
$ Return
$!#############################################################################
