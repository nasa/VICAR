$!****************************************************************************
$!
$! Build proc for MIPL module pbdata
$! VPACK Version 1.9, Wednesday, September 22, 2010, 19:24:22
$!
$! Execute by entering:		$ @pbdata
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
$ write sys$output "*** module pbdata ***"
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
$ write sys$output "Invalid argument given to pbdata.com file -- ", primary
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
$   if F$SEARCH("pbdata.imake") .nes. ""
$   then
$      vimake pbdata
$      purge pbdata.bld
$   else
$      if F$SEARCH("pbdata.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pbdata
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pbdata.bld "STD"
$   else
$      @pbdata.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pbdata.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pbdata.com -mixed -
	-s pbdata.f zpbdata.c xpbdata.f -
	-i pbdata.imake -
	-t tpbdata.f tzpbdata.c tpbdata.imake tpbdata.pdf tstpbdata.pdf -
	   tstpbdata_old.log tstpbdata.log_solos -
	-o pbdata.hlp pbdata_old.f
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pbdata.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Returns picture body data given pb name (12 characters)

c BUF contains:
c  1-3. Triaxial radii (RA,RB,RC) in km
c  4. Longitude of longest axis (LORA in VICAR, BODY_LONG_AXIS in SPICE), in degrees
c  5. Rotation period in days
c  6. Mean solar distance in km

c In the current implementation, 1-3 and 5 are retrieved from SPICE.
c 4 (LORA) was always zero in the old PBDATA and is zero in all current SPICE kernels
c (and is also deprecated by NAIF), so is simply set to zero.
c 6 (solar distance) is hard-coded in the same manner as was done in the old PBDATA.

c The old code for PBDATA is included in pbdata.com as pbdata_old.f

      SUBROUTINE PBDATA(name,BUF,*)

      REAL*4 BUF(*)
      REAL*4 SRANGE(11)
      character*12 name
      real*8 rad(3)
      logical found, failed
      external failed

C Mean solar ranges of the planets (AU) plus Gaspra & Ida:
      data SRANGE/ 0.387098, 0.723331, 1.0, 1.523679, 5.2027, 9.546,
     &   19.2, 30.09, 39.5, 2.2016, 2.9485/

      call bodn2c( name, id, found)
      if (.not.found) return 1

c  initialize buffer to zero
      do i=1,6
        buf(i) = 0.0
      enddo

c  get radii from SPICE:
      call bodvrd( name, 'RADII', 3, n, rad)
      if (failed()) then
        call reset()
      else
        do i=1,n
          buf(i) = rad(i)
        enddo
      endif

c  get orientation of Prime Meridian, 2nd entry is rotation rate in deg/day:
      call bodvrd( name, 'PM', 3, n, rad)
      if (failed()) then
        call reset()
      else
        buf(5) = 360.0/rad(2)
      endif

c  for ID = 1-9:  use SRANGE(ID)
c         = 100-999:  use SRANGE(ID/100)
c  for Gaspra/Ida: special code.
      buf(6) = 0.0
      k = id
      if (name.eq.'GASPRA') then
        k = 10
      elseif (name.eq.'IDA') then
        k = 11
      elseif (k.ge.10) then
        k = k/100
        if (k.lt.1 .or. k.gt.9) return
      endif
      BUF(6) = 149597871.D0*SRANGE(K)	!Solar range in km

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpbdata.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/*
Bridge for PBDATA in C, called from C
*/
int zpbdata(name,buf)

char *name;
float *buf;
{
   int i, status;
   i=strlen(name);

   status = FTN_NAME(xpbdata) (name, &i, buf );
   return status;
 }
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xpbdata.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  Bridge to PBDATA, in Fortran

      INTEGER FUNCTION xpbdata(name, i, buf)
     
      real*4 buf(20)
      byte name(1)
      integer i
      character*12 text

      text=' '

      if (i.gt.12) call xvmessage('xpbdata, string is too long',' ')

C     Transformation to Fortran-string
      call mvlc(name, text, i)
      call pbdata(text,buf,*100)
      xpbdata = 1
      goto 999

 100  xpbdata = 0

 999  return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pbdata.imake
/* Imake file for VICAR subroutine PBDATA   */

#define SUBROUTINE  pbdata

#define MODULE_LIST  pbdata.f  zpbdata.c xpbdata.f

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

/*#define DEBUG	/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tpbdata.f
C Test for routine PBDATA
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      CHARACTER*80 BUF
      REAL*4 D(20)
      INTEGER NTARGETS
      character*12 name(65)
      data ntargets/65/
      data name/   'MERCURY     ','VENUS       ','EARTH       ',
     + 'MOON        ','MARS        ','PHOBOS      ','DEIMOS      ',
     + 'JUPITER     ','IO          ','EUROPA      ','GANYMEDE    ',
     + 'CALLISTO    ','AMALTHEA    ','HIMALIA     ','ELARA       ',
     + 'PASIPHAE    ','SINOPE      ','LYSITHEA    ','CARME       ',
     + 'ANANKE      ','LEDA        ','THEBE       ','ADRASTEA    ',
     + 'METIS       ','SATURN      ','MIMAS       ','ENCELADUS   ',
     + 'TETHYS      ','DIONE       ','RHEA        ','TITAN       ',
     + 'HYPERION    ','IAPETUS     ','PHOEBE      ','JANUS       ',
     + 'EPIMETHEUS  ','HELENE      ','TELESTO     ','CALYPSO     ',
     + 'ATLAS       ','PROMETHEUS  ','PANDORA     ','URANUS      ',
     + 'ARIEL       ','UMBRIEL     ','TITANIA     ','OBERON      ',
     + 'MIRANDA     ','CORDELIA    ','OPHELIA     ','BIANCA      ',
     + 'CRESSIDA    ','DESDEMONA   ','JULIET      ','PORTIA      ',
     + 'ROSALIND    ','BELINDA     ','PUCK        ','NEPTUNE     ',
     + 'TRITON      ','NEREID      ','PLUTO       ','CHARON      ',
     + 'GASPRA      ','IDA'/

      call xvmessage('********FORTRAN CALLABLE VERSION****',' ')
      call xvmessage('1       TARGET  TARGET                  
     +         ROTATION     SOLAR',' ')
      call xvmessage('        NUMBER   NAME       A       B   
     + C       PERIOD      RANGE',' ')
      BUF(1:80) = ' '

      call init_spice

      DO 10 I=1,65
      CALL PBDATA(NAME(I),D,*8)
      call reset()
    8 CALL PBID(NAME(I),ID,*10)		!SEDR ID
      call reset()
      WRITE (BUF(4:11),'(I8)') ID !Target number
      buf(16:23) = name(i)             !Target name
      WRITE (BUF(26:32),'(F7.1)') D(1) !RA
      WRITE (BUF(34:40),'(F7.1)') D(2) !RB
      WRITE (BUF(42:48),'(F7.1)') D(3) !RC
      WRITE (BUF(50:61),'(F12.7)') D(5) !Rotation period
      WRITE (BUF(63:78),'(E16.10)') D(6) !Solar range
      CALL XVMESSAGE(BUF,' ')
   10 CONTINUE
C
c********C CALLABLE VERSION****
      call tzpbdata()

   20 RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzpbdata.c
#include "xvmaininc.h"
#include "ftnbridge.h"

#define SUCCESS 1
#define FAILURE 0

void FTN_NAME(tzpbdata)()
{
  char buf[132];
  float d[20];
  int datastat,idstat,id,i;
  int ntargets = 65;
  char name[65][13];
  char binpool[132];

  /* initialize planetary body names array */

  strcpy(name[0],  "MERCURY") ; 
  strcpy(name[1],  "VENUS")   ;
  strcpy(name[2],  "EARTH")   ;
  strcpy(name[3],  "MOON")   ;
  strcpy(name[4],  "MARS")   ;
  strcpy(name[5],  "PHOBOS")   ;
  strcpy(name[6],  "DEIMOS")   ;
  strcpy(name[7],  "JUPITER")   ;
  strcpy(name[8],  "IO")   ;
  strcpy(name[9],  "EUROPA")   ;
  strcpy(name[10], "GANYMEDE")   ;
  strcpy(name[11], "CALLISTO")   ;
  strcpy(name[12], "AMALTHEA")   ;
  strcpy(name[13], "HIMALIA")   ;
  strcpy(name[14], "ELARA")   ;
  strcpy(name[15], "PASIPHAE")   ;
  strcpy(name[16], "SINOPE")   ;
  strcpy(name[17], "LYSITHEA")   ;
  strcpy(name[18], "CARME")   ;
  strcpy(name[19], "ANANKE")   ;
  strcpy(name[20], "LEDA")   ;
  strcpy(name[21], "THEBE")   ;
  strcpy(name[22], "ADRASTEA")   ;
  strcpy(name[23], "METIS")   ;
  strcpy(name[24], "SATURN")   ;
  strcpy(name[25], "MIMAS")   ;
  strcpy(name[26], "ENCELADUS")   ;
  strcpy(name[27], "TETHYS")   ;
  strcpy(name[28], "DIONE")   ;
  strcpy(name[29], "RHEA")   ;
  strcpy(name[30], "TITAN")   ;
  strcpy(name[31], "HYPERION")   ;
  strcpy(name[32], "IAPETUS")   ;
  strcpy(name[33], "PHOEBE")   ;
  strcpy(name[34], "JANUS")   ;
  strcpy(name[35], "EPIMETHEUS")   ;
  strcpy(name[36], "HELENE")   ;
  strcpy(name[37], "TELESTO")   ;
  strcpy(name[38], "CALYPSO")   ;
  strcpy(name[39], "ATLAS")   ;
  strcpy(name[40], "PROMETHEUS")   ;
  strcpy(name[41], "PANDORA")   ;
  strcpy(name[42], "URANUS")   ;
  strcpy(name[43], "ARIEL")   ;
  strcpy(name[44], "UMBRIEL")   ;
  strcpy(name[45], "TITANIA")   ;
  strcpy(name[46], "OBERON")   ;
  strcpy(name[47], "MIRANDA")   ;
  strcpy(name[48], "CORDELIA")   ;
  strcpy(name[49], "OPHELIA")   ;
  strcpy(name[50], "BIANCA")   ;
  strcpy(name[51], "CRESSIDA")   ;
  strcpy(name[52], "DESDEMONA")   ;
  strcpy(name[53], "JULIET")   ;
  strcpy(name[54], "PORTIA")   ;
  strcpy(name[55], "ROSALIND")   ;
  strcpy(name[56], "BELINDA")   ;
  strcpy(name[57], "PUCK")   ;
  strcpy(name[58], "NEPTUNE")   ;
  strcpy(name[59], "TRITON")   ;
  strcpy(name[60], "NEREID")   ;
  strcpy(name[61], "PLUTO")   ;
  strcpy(name[62], "CHARON")   ;
  strcpy(name[63], "GASPRA")   ;
  strcpy(name[64], "IDA")   ;


  zvmessage("********C CALLABLE VERSION****","");
  zvmessage("1       TARGET  TARGET                              ROTATION     SOLAR","");
  zvmessage("        NUMBER   NAME       A       B       C       PERIOD      RANGE","");

  for (i=0;i<65;i++)
    {
    datastat = zpbdata(name[i],d);
    /*zprnt(4,1,&datastat," ZPBDATA return:");*/
    if(datastat == FAILURE) zmabend("error in pbdata","");
    idstat = zpbid(name[i],&id);		/*SEDR ID*/
    /*zprnt(4,1,&datastat," ZPBID return:");*/
    if(idstat == FAILURE) zmabend("error in pbid","");

    sprintf(buf,"%11d    %-9s %7.1f %7.1f %7.1f %12.7f %16.10E",id,
	    name[i],d[0],d[1],d[2],d[4],d[5]);
    zvmessage(buf,"");
    }

    return;
}
$!-----------------------------------------------------------------------------
$ create tpbdata.imake
#define PROGRAM tpbdata

#define MODULE_LIST tpbdata.f  tzpbdata.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P1SUB
#define LIB_SPICE

/*#define DEBUG	/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
$!-----------------------------------------------------------------------------
$ create tpbdata.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpbdata.pdf
procedure
refgbl $echo
body
let _onfail="continue"
!let $echo="yes"
WRITE "*************NOTE TO TESTERS:********************"
WRITE "Differences in testlogs may exist between platforms"
WRITE "due to round off differences and differences in"
WRITE "the display of character strings"
WRITE " "
WRITE "Test of subroutine PBDATA"
tpbdata
end-proc
$!-----------------------------------------------------------------------------
$ create tstpbdata_old.log
tstpbdata
*************NOTE TO TESTERS:********************
Differences in testlogs may exist between platforms
due to round off differences and differences in
the display of character strings
 
Test of subroutine PBDATA
Beginning VICAR task tpbdata
********FORTRAN CALLABLE VERSION****
1       TARGET  TARGET                                     ROTATION     SOLAR
        NUMBER   NAME       A       B              C       PERIOD      RANGE
        199    MERCURY    2439.7  2439.7  2439.7   58.6462250 0.5790904000E+08  
        299    VENUS      6137.0  6137.0  6137.0 -243.0249939 0.1082087760E+09  
        399    EARTH      6378.1  6378.1  6356.8    0.9972696 0.1495978720E+09  
        301    MOON       1737.4  1737.4  1737.4   27.3216610 0.1495978720E+09  
        499    MARS       3397.0  3397.0  3375.0    1.0256937 0.2279391360E+09  
        401    PHOBOS       13.4    11.2     9.2    0.3189102 0.2279391360E+09  
        402    DEIMOS        7.5     6.1     5.2    1.2624408 0.2279391360E+09  
        599    JUPITER   71492.0 71492.0 66854.0    0.4135383 0.7783128960E+09  
        501    IO         1830.0  1818.7  1815.3    1.7691379 0.7783128960E+09  
        502    EUROPA     1565.0  1565.0  1565.0    3.5511811 0.7783128960E+09  
        503    GANYMEDE   2634.0  2634.0  2634.0    7.1545529 0.7783128960E+09  
        504    CALLISTO   2403.0  2403.0  2403.0   16.6890182 0.7783128960E+09  
        505    AMALTHEA    131.0    73.0    67.0    0.4981800 0.7783128960E+09  
        506    HIMALIA      85.0    85.0    85.0  250.6000214 0.7783128960E+09  
        507    ELARA        40.0    40.0    40.0  260.1000061 0.7783128960E+09  
        508    PASIPHAE     18.0    18.0    18.0  735.0000610 0.7783128960E+09  
        509    SINOPE       14.0    14.0    14.0  758.0000610 0.7783128960E+09  
        510    LYSITHEA     12.0    12.0    12.0  260.0000305 0.7783128960E+09  
        511    CARME        15.0    15.0    15.0  692.0000610 0.7783128960E+09  
        512    ANANKE       10.0    10.0    10.0  617.0000610 0.7783128960E+09  
        513    LEDA          5.0     5.0     5.0  240.0000000 0.7783128960E+09  
        514    THEBE        55.0    55.0    45.0    0.0000000 0.7783128960E+09  
        515    ADRASTEA     13.0    10.0     8.0    0.0000000 0.7783128960E+09  
        516    METIS        20.0    20.0    20.0    0.0000000 0.7783128960E+09  
        699    SATURN    60268.0 60268.0 54364.0    0.4440093 0.1428061184E+10  
        601    MIMAS       210.3   197.4   192.6    0.9424218 0.1428061184E+10  
        602    ENCELADU    256.2   247.3   244.0    1.3702180 0.1428061184E+10  
        603    TETHYS      523.0   523.0   523.0    1.8878026 0.1428061184E+10  
        604    DIONE       560.0   560.0   560.0    2.7369156 0.1428061184E+10  
        605    RHEA        764.0   764.0   764.0    4.5175028 0.1428061184E+10  
        606    TITAN      2575.0  2575.0  2575.0   15.9454479 0.1428061184E+10  
        607    HYPERION    180.0   140.0   112.5   21.2800045 0.1428061184E+10  
        608    IAPETUS     718.0   718.0   718.0   79.3308487 0.1428061184E+10  
        609    PHOEBE      115.0   110.0   105.0  550.4000244 0.1428061184E+10  
        610    JANUS        97.0    95.0    77.0    0.0000000 0.1428061184E+10  
        611    EPIMETHE     69.0    55.0    55.0    0.0000000 0.1428061184E+10  
        612    HELENE       17.5    17.5    17.5    0.0000000 0.1428061184E+10  
        613    TELESTO      15.2    12.5     7.5    0.0000000 0.1428061184E+10  
        614    CALYPSO      15.0     8.0     8.0    0.0000000 0.1428061184E+10  
        615    ATLAS        18.5    17.2    13.5    0.0000000 0.1428061184E+10  
        616    PROMETHE     74.0    50.0    34.0    0.0000000 0.1428061184E+10  
        617    PANDORA      55.0    44.0    31.0    0.0000000 0.1428061184E+10  
        799    URANUS    25559.0 25559.0 24973.0   -0.7183333 0.2872279296E+10  
        701    ARIEL       581.1   577.9   577.7   -2.5203791 0.2872279296E+10  
        702    UMBRIEL     584.7   584.7   584.7   -4.1441765 0.2872279296E+10  
        703    TITANIA     788.9   788.9   788.9   -8.7058649 0.2872279296E+10  
        704    OBERON      761.4   761.4   761.4  -13.4632320 0.2872279296E+10  
        705    MIRANDA     240.4   234.2   232.9   -1.4134792 0.2872279296E+10  
        706    CORDELIA     13.0    13.0    13.0    0.0000000 0.2872279296E+10  
        707    OPHELIA      15.0    15.0    15.0    0.0000000 0.2872279296E+10  
        708    BIANCA       21.0    21.0    21.0    0.0000000 0.2872279296E+10  
        709    CRESSIDA     31.0    31.0    31.0    0.0000000 0.2872279296E+10  
        710    DESDEMON     27.0    27.0    27.0    0.0000000 0.2872279296E+10  
        711    JULIET       42.0    42.0    42.0    0.0000000 0.2872279296E+10  
        712    PORTIA       54.0    54.0    54.0    0.0000000 0.2872279296E+10  
        713    ROSALIND     27.0    27.0    27.0    0.0000000 0.2872279296E+10  
        714    BELINDA      33.0    33.0    33.0    0.0000000 0.2872279296E+10  
        715    PUCK         77.0    77.0    77.0    0.0000000 0.2872279296E+10  
        899    NEPTUNE   25269.0 25269.0 24800.0    0.7441667 0.4501400064E+10  
        801    TRITON     1352.6  1352.6  1352.6   -5.8768539 0.4501400064E+10  
        802    NEREID      170.0   170.0   170.0  360.1624451 0.4501400064E+10  
        999    PLUTO      1162.0  1162.0  1162.0   -6.3870559 0.5909115904E+10  
        901    CHARON      606.0   606.0   606.0   -6.3872299 0.5909115904E+10  
    9511010    GASPRA        9.0     5.5     5.0    0.0000000 0.3293546880E+09  
    2431010    IDA          28.0    12.0    10.5    0.0000000 0.4410893120E+09  
********C CALLABLE VERSION****
1       TARGET  TARGET                              ROTATION     SOLAR
        NUMBER   NAME       A       B       C       PERIOD      RANGE
        199    MERCURY    2439.7  2439.7  2439.7   58.6462250 5.7909040000E+07
        299    VENUS      6137.0  6137.0  6137.0 -243.0249939 1.0820877600E+08
        399    EARTH      6378.1  6378.1  6356.8    0.9972696 1.4959787200E+08
        301    MOON       1737.4  1737.4  1737.4   27.3216610 1.4959787200E+08
        499    MARS       3397.0  3397.0  3375.0    1.0256937 2.2793913600E+08
        401    PHOBOS       13.4    11.2     9.2    0.3189102 2.2793913600E+08
        402    DEIMOS        7.5     6.1     5.2    1.2624408 2.2793913600E+08
        599    JUPITER   71492.0 71492.0 66854.0    0.4135383 7.7831289600E+08
        501    IO         1830.0  1818.7  1815.3    1.7691379 7.7831289600E+08
        502    EUROPA     1565.0  1565.0  1565.0    3.5511811 7.7831289600E+08
        503    GANYMEDE   2634.0  2634.0  2634.0    7.1545529 7.7831289600E+08
        504    CALLISTO   2403.0  2403.0  2403.0   16.6890182 7.7831289600E+08
        505    AMALTHEA    131.0    73.0    67.0    0.4981800 7.7831289600E+08
        506    HIMALIA      85.0    85.0    85.0  250.6000214 7.7831289600E+08
        507    ELARA        40.0    40.0    40.0  260.1000061 7.7831289600E+08
        508    PASIPHAE     18.0    18.0    18.0  735.0000610 7.7831289600E+08
        509    SINOPE       14.0    14.0    14.0  758.0000610 7.7831289600E+08
        510    LYSITHEA     12.0    12.0    12.0  260.0000305 7.7831289600E+08
        511    CARME        15.0    15.0    15.0  692.0000610 7.7831289600E+08
        512    ANANKE       10.0    10.0    10.0  617.0000610 7.7831289600E+08
        513    LEDA          5.0     5.0     5.0  240.0000000 7.7831289600E+08
        514    THEBE        55.0    55.0    45.0    0.0000000 7.7831289600E+08
        515    ADRASTEA     13.0    10.0     8.0    0.0000000 7.7831289600E+08
        516    METIS        20.0    20.0    20.0    0.0000000 7.7831289600E+08
        699    SATURN    60268.0 60268.0 54364.0    0.4440093 1.4280611840E+09
        601    MIMAS       210.3   197.4   192.6    0.9424218 1.4280611840E+09
        602    ENCELADUS   256.2   247.3   244.0    1.3702180 1.4280611840E+09
        603    TETHYS      523.0   523.0   523.0    1.8878026 1.4280611840E+09
        604    DIONE       560.0   560.0   560.0    2.7369156 1.4280611840E+09
        605    RHEA        764.0   764.0   764.0    4.5175028 1.4280611840E+09
        606    TITAN      2575.0  2575.0  2575.0   15.9454479 1.4280611840E+09
        607    HYPERION    180.0   140.0   112.5   21.2800045 1.4280611840E+09
        608    IAPETUS     718.0   718.0   718.0   79.3308487 1.4280611840E+09
        609    PHOEBE      115.0   110.0   105.0  550.4000244 1.4280611840E+09
        610    JANUS        97.0    95.0    77.0    0.0000000 1.4280611840E+09
        611    EPIMETHEUS    69.0    55.0    55.0    0.0000000 1.4280611840E+09
        612    HELENE       17.5    17.5    17.5    0.0000000 1.4280611840E+09
        613    TELESTO      15.2    12.5     7.5    0.0000000 1.4280611840E+09
        614    CALYPSO      15.0     8.0     8.0    0.0000000 1.4280611840E+09
        615    ATLAS        18.5    17.2    13.5    0.0000000 1.4280611840E+09
        616    PROMETHEUS    74.0    50.0    34.0    0.0000000 1.4280611840E+09
        617    PANDORA      55.0    44.0    31.0    0.0000000 1.4280611840E+09
        799    URANUS    25559.0 25559.0 24973.0   -0.7183333 2.8722792960E+09
        701    ARIEL       581.1   577.9   577.7   -2.5203791 2.8722792960E+09
        702    UMBRIEL     584.7   584.7   584.7   -4.1441765 2.8722792960E+09
        703    TITANIA     788.9   788.9   788.9   -8.7058649 2.8722792960E+09
        704    OBERON      761.4   761.4   761.4  -13.4632320 2.8722792960E+09
        705    MIRANDA     240.4   234.2   232.9   -1.4134792 2.8722792960E+09
        706    CORDELIA     13.0    13.0    13.0    0.0000000 2.8722792960E+09
        707    OPHELIA      15.0    15.0    15.0    0.0000000 2.8722792960E+09
        708    BIANCA       21.0    21.0    21.0    0.0000000 2.8722792960E+09
        709    CRESSIDA     31.0    31.0    31.0    0.0000000 2.8722792960E+09
        710    DESDEMONA    27.0    27.0    27.0    0.0000000 2.8722792960E+09
        711    JULIET       42.0    42.0    42.0    0.0000000 2.8722792960E+09
        712    PORTIA       54.0    54.0    54.0    0.0000000 2.8722792960E+09
        713    ROSALIND     27.0    27.0    27.0    0.0000000 2.8722792960E+09
        714    BELINDA      33.0    33.0    33.0    0.0000000 2.8722792960E+09
        715    PUCK         77.0    77.0    77.0    0.0000000 2.8722792960E+09
        899    NEPTUNE   25269.0 25269.0 24800.0    0.7441667 4.5014000640E+09
        801    TRITON     1352.6  1352.6  1352.6   -5.8768539 4.5014000640E+09
        802    NEREID      170.0   170.0   170.0  360.1624451 4.5014000640E+09
        999    PLUTO      1162.0  1162.0  1162.0   -6.3870559 5.9091159040E+09
        901    CHARON      606.0   606.0   606.0   -6.3872299 5.9091159040E+09
    9511010    GASPRA        9.0     5.5     5.0    0.0000000 3.2935468800E+08
    2431010    IDA          28.0    12.0    10.5    0.0000000 4.4108931200E+08
ex
slogoff
$!-----------------------------------------------------------------------------
$ create tstpbdata.log_solos
tstpbdata
*************NOTE TO TESTERS:********************
Differences in testlogs may exist between platforms
due to round off differences and differences in
the display of character strings
 
Test of subroutine PBDATA
Beginning VICAR task tpbdata
********FORTRAN CALLABLE VERSION****
1       TARGET  TARGET                                     ROTATION     SOLAR
        NUMBER   NAME       A       B              C       PERIOD      RANGE
        199    MERCURY    2439.7  2439.7  2439.7   58.6462250 0.5790904000E+08  
        299    VENUS      6051.8  6051.8  6051.8 -243.0184784 0.1082087760E+09  
        399    EARTH      6378.1  6378.1  6356.8    0.9972696 0.1495978720E+09  
        301    MOON       1737.4  1737.4  1737.4   27.3216610 0.1495978720E+09  
        499    MARS       3397.0  3397.0  3375.0    1.0259567 0.2279391360E+09  
        401    PHOBOS       13.4    11.2     9.2    0.3189102 0.2279391360E+09  
        402    DEIMOS        7.5     6.1     5.2    1.2624408 0.2279391360E+09  
        599    JUPITER   71492.0 71492.0 66854.0    0.4135383 0.7783128960E+09  
        501    IO         1829.4  1819.3  1815.7    1.7691379 0.7783128960E+09  
        502    EUROPA     1564.1  1561.2  1560.9    3.5511811 0.7783128960E+09  
        503    GANYMEDE   2632.4  2632.3  2632.4    7.1545529 0.7783128960E+09  
        504    CALLISTO   2409.4  2409.2  2409.3   16.6890182 0.7783128960E+09  
        505    AMALTHEA    131.0    73.0    67.0    0.4981793 0.7783128960E+09  
        506    HIMALIA      85.0    85.0    85.0    0.0000000 0.7783128960E+09  
        507    ELARA        40.0    40.0    40.0    0.0000000 0.7783128960E+09  
        508    PASIPHAE     18.0    18.0    18.0    0.0000000 0.7783128960E+09  
        509    SINOPE       14.0    14.0    14.0    0.0000000 0.7783128960E+09  
        510    LYSITHEA     12.0    12.0    12.0    0.0000000 0.7783128960E+09  
        511    CARME        15.0    15.0    15.0    0.0000000 0.7783128960E+09  
        512    ANANKE       10.0    10.0    10.0    0.0000000 0.7783128960E+09  
        513    LEDA          5.0     5.0     5.0    0.0000000 0.7783128960E+09  
        514    THEBE        50.0    50.0    50.0    0.6745358 0.7783128960E+09  
        515    ADRASTEA     13.0    10.0     8.0    0.2982605 0.7783128960E+09  
        516    METIS        20.0    20.0    20.0    0.2947788 0.7783128960E+09  
        699    SATURN    60268.0 60268.0 54364.0    0.4440092 0.1428061184E+10  
        601    MIMAS       209.1   196.2   191.4    0.9424218 0.1428061184E+10  
        602    ENCELADU    256.3   247.3   244.6    1.3702180 0.1428061184E+10  
        603    TETHYS      535.6   528.2   525.8    1.8878026 0.1428061184E+10  
        604    DIONE       560.0   560.0   560.0    2.7369156 0.1428061184E+10  
        605    RHEA        764.0   764.0   764.0    4.5175028 0.1428061184E+10  
        606    TITAN      2575.0  2575.0  2575.0   15.9454479 0.1428061184E+10  
        607    HYPERION    180.0   140.0   112.5    0.0000000 0.1428061184E+10  
        608    IAPETUS     718.0   718.0   718.0   79.3308487 0.1428061184E+10  
        609    PHOEBE      115.0   110.0   105.0    0.3867500 0.1428061184E+10  
        610    JANUS        97.0    95.0    77.0    0.6946642 0.1428061184E+10  
        611    EPIMETHE     69.0    55.0    55.0    0.6943229 0.1428061184E+10  
        612    HELENE       16.0    16.0    16.0    2.7352006 0.1428061184E+10  
        613    TELESTO      15.0    12.5     7.5    1.8878024 0.1428061184E+10  
        614    CALYPSO      15.0     8.0     8.0    1.8880370 0.1428061184E+10  
        615    ATLAS        18.5    17.2    13.5    0.6016988 0.1428061184E+10  
        616    PROMETHE     74.0    50.0    34.0    0.6129861 0.1428061184E+10  
        617    PANDORA      55.0    44.0    31.0    0.6285036 0.1428061184E+10  
        799    URANUS    25559.0 25559.0 24973.0   -0.7183333 0.2872279296E+10  
        701    ARIEL       581.1   577.9   577.7   -2.5203788 0.2872279296E+10  
        702    UMBRIEL     584.7   584.7   584.7   -4.1441765 0.2872279296E+10  
        703    TITANIA     788.9   788.9   788.9   -8.7058659 0.2872279296E+10  
        704    OBERON      761.4   761.4   761.4  -13.4632320 0.2872279296E+10  
        705    MIRANDA     240.4   234.2   232.9   -1.4134792 0.2872279296E+10  
        706    CORDELIA     13.0    13.0    13.0   -0.3350331 0.2872279296E+10  
        707    OPHELIA      15.0    15.0    15.0   -0.3764089 0.2872279296E+10  
        708    BIANCA       21.0    21.0    21.0   -0.4345771 0.2872279296E+10  
        709    CRESSIDA     31.0    31.0    31.0   -0.4635701 0.2872279296E+10  
        710    DESDEMON     27.0    27.0    27.0   -0.4736511 0.2872279296E+10  
        711    JULIET       42.0    42.0    42.0   -0.4930660 0.2872279296E+10  
        712    PORTIA       54.0    54.0    54.0   -0.5131958 0.2872279296E+10  
        713    ROSALIND     27.0    27.0    27.0   -0.5584589 0.2872279296E+10  
        714    BELINDA      33.0    33.0    33.0   -0.6235247 0.2872279296E+10  
        715    PUCK         77.0    77.0    77.0   -0.7618321 0.2872279296E+10  
        899    NEPTUNE   24764.0 24764.0 24341.0    0.6712500 0.4501400064E+10  
        801    TRITON     1352.6  1352.6  1352.6   -5.8768539 0.4501400064E+10  
        802    NEREID      170.0   170.0   170.0    0.0000000 0.4501400064E+10  
        999    PLUTO      1195.0  1195.0  1195.0   -6.3872461 0.5909115904E+10  
        901    CHARON      593.0   593.0   593.0   -6.3872461 0.5909115904E+10  
    9511010    GASPRA        9.1     5.2     4.4    0.2934197 0.3293546880E+09  
    2431010    IDA          26.8    12.0     7.6   -0.1930680 0.4410893120E+09  
********C CALLABLE VERSION****
1       TARGET  TARGET                              ROTATION     SOLAR
        NUMBER   NAME       A       B       C       PERIOD      RANGE
        199    MERCURY    2439.7  2439.7  2439.7   58.6462250 5.7909040000E+07
        299    VENUS      6051.8  6051.8  6051.8 -243.0184784 1.0820877600E+08
        399    EARTH      6378.1  6378.1  6356.8    0.9972696 1.4959787200E+08
        301    MOON       1737.4  1737.4  1737.4   27.3216610 1.4959787200E+08
        499    MARS       3397.0  3397.0  3375.0    1.0259567 2.2793913600E+08
        401    PHOBOS       13.4    11.2     9.2    0.3189102 2.2793913600E+08
        402    DEIMOS        7.5     6.1     5.2    1.2624408 2.2793913600E+08
        599    JUPITER   71492.0 71492.0 66854.0    0.4135383 7.7831289600E+08
        501    IO         1829.4  1819.3  1815.7    1.7691379 7.7831289600E+08
        502    EUROPA     1564.1  1561.2  1560.9    3.5511811 7.7831289600E+08
        503    GANYMEDE   2632.4  2632.3  2632.4    7.1545529 7.7831289600E+08
        504    CALLISTO   2409.4  2409.2  2409.3   16.6890182 7.7831289600E+08
        505    AMALTHEA    131.0    73.0    67.0    0.4981793 7.7831289600E+08
        506    HIMALIA      85.0    85.0    85.0    0.0000000 7.7831289600E+08
        507    ELARA        40.0    40.0    40.0    0.0000000 7.7831289600E+08
        508    PASIPHAE     18.0    18.0    18.0    0.0000000 7.7831289600E+08
        509    SINOPE       14.0    14.0    14.0    0.0000000 7.7831289600E+08
        510    LYSITHEA     12.0    12.0    12.0    0.0000000 7.7831289600E+08
        511    CARME        15.0    15.0    15.0    0.0000000 7.7831289600E+08
        512    ANANKE       10.0    10.0    10.0    0.0000000 7.7831289600E+08
        513    LEDA          5.0     5.0     5.0    0.0000000 7.7831289600E+08
        514    THEBE        50.0    50.0    50.0    0.6745358 7.7831289600E+08
        515    ADRASTEA     13.0    10.0     8.0    0.2982605 7.7831289600E+08
        516    METIS        20.0    20.0    20.0    0.2947788 7.7831289600E+08
        699    SATURN    60268.0 60268.0 54364.0    0.4440092 1.4280611840E+09
        601    MIMAS       209.1   196.2   191.4    0.9424218 1.4280611840E+09
        602    ENCELADUS   256.3   247.3   244.6    1.3702180 1.4280611840E+09
        603    TETHYS      535.6   528.2   525.8    1.8878026 1.4280611840E+09
        604    DIONE       560.0   560.0   560.0    2.7369156 1.4280611840E+09
        605    RHEA        764.0   764.0   764.0    4.5175028 1.4280611840E+09
        606    TITAN      2575.0  2575.0  2575.0   15.9454479 1.4280611840E+09
        607    HYPERION    180.0   140.0   112.5    0.0000000 1.4280611840E+09
        608    IAPETUS     718.0   718.0   718.0   79.3308487 1.4280611840E+09
        609    PHOEBE      115.0   110.0   105.0    0.3867500 1.4280611840E+09
        610    JANUS        97.0    95.0    77.0    0.6946642 1.4280611840E+09
        611    EPIMETHEUS    69.0    55.0    55.0    0.6943229 1.4280611840E+09
        612    HELENE       16.0    16.0    16.0    2.7352006 1.4280611840E+09
        613    TELESTO      15.0    12.5     7.5    1.8878024 1.4280611840E+09
        614    CALYPSO      15.0     8.0     8.0    1.8880370 1.4280611840E+09
        615    ATLAS        18.5    17.2    13.5    0.6016988 1.4280611840E+09
        616    PROMETHEUS    74.0    50.0    34.0    0.6129861 1.4280611840E+09
        617    PANDORA      55.0    44.0    31.0    0.6285036 1.4280611840E+09
        799    URANUS    25559.0 25559.0 24973.0   -0.7183333 2.8722792960E+09
        701    ARIEL       581.1   577.9   577.7   -2.5203788 2.8722792960E+09
        702    UMBRIEL     584.7   584.7   584.7   -4.1441765 2.8722792960E+09
        703    TITANIA     788.9   788.9   788.9   -8.7058659 2.8722792960E+09
        704    OBERON      761.4   761.4   761.4  -13.4632320 2.8722792960E+09
        705    MIRANDA     240.4   234.2   232.9   -1.4134792 2.8722792960E+09
        706    CORDELIA     13.0    13.0    13.0   -0.3350331 2.8722792960E+09
        707    OPHELIA      15.0    15.0    15.0   -0.3764089 2.8722792960E+09
        708    BIANCA       21.0    21.0    21.0   -0.4345771 2.8722792960E+09
        709    CRESSIDA     31.0    31.0    31.0   -0.4635701 2.8722792960E+09
        710    DESDEMONA    27.0    27.0    27.0   -0.4736511 2.8722792960E+09
        711    JULIET       42.0    42.0    42.0   -0.4930660 2.8722792960E+09
        712    PORTIA       54.0    54.0    54.0   -0.5131958 2.8722792960E+09
        713    ROSALIND     27.0    27.0    27.0   -0.5584589 2.8722792960E+09
        714    BELINDA      33.0    33.0    33.0   -0.6235247 2.8722792960E+09
        715    PUCK         77.0    77.0    77.0   -0.7618321 2.8722792960E+09
        899    NEPTUNE   24764.0 24764.0 24341.0    0.6712500 4.5014000640E+09
        801    TRITON     1352.6  1352.6  1352.6   -5.8768539 4.5014000640E+09
        802    NEREID      170.0   170.0   170.0    0.0000000 4.5014000640E+09
        999    PLUTO      1195.0  1195.0  1195.0   -6.3872461 5.9091159040E+09
        901    CHARON      593.0   593.0   593.0   -6.3872461 5.9091159040E+09
    9511010    GASPRA        9.1     5.2     4.4    0.2934197 3.2935468800E+08
    2431010    IDA          26.8    12.0     7.6   -0.1930680 4.4108931200E+08
exit
slogoff
$ Return
$!#############################################################################
$Other_File:
$ create pbdata.hlp
1  PBDATA

  PBDATA will return the planet radii, axial rotation rate, and 
  solar range of a specified planetary body.

2  CALLING SEQUENCE

   Fortran calling sequence:

       CALL PBDATA(NAME,buf,*nnn)

       CHARACTER*12 NAME	!Input target-body name
       REAL*4 BUF(20)		!Output physical constants.
       *nnn			!Statement label of an alternate return address

    NAME must be left justified and padded on the right with blanks.
    The alternate return is taken if an the input NAME is invalid.

   C calling sequence:

       status = zpbdata(name,buf);

       char name[12];		!Input target-body name
       float buf([20];		!Output physical constants.
       int status;		!0 = failure, 1 = success

2  OPERATION

   The physical constants are retrieved from SPICE, except for
   solar range, which is hard-coded within the source file.
   
   PBDATA will fill BUF with the following data:

        Word       Physical constant (MAP2 symbol and unit)
        1          equatorial radius at long. LORA (RA, km)
        2          equatorial radius at long. LORA+90 (RB, km)
        3          polar radius  (RC, km)
        4          long. of long axis  RA  (LORA, deg)
        5          axial rotation period  (days)
        6          solar range (km)
        7-20       unused

   (Note:  LORA is set to zero, as this was the case in the old code that used
   hard-coded values for all parameters, and it is deprecated by NAIF SPICE.)

2  HISTORY

       Original Programmer: Gary Yagi, 23 July 1980
       Current Cognizant Programmer: Lucas Kamp
       Source Language:  Fortran and C
       Revisions:

   21 Sep 10     lwk    Restore SPICE calls, per Cassini request.
   22 AUG 94     GMY    Remove all SPICE calls (FR 85627)
   22 FEB 94     TLT    Ported to Unix
   10 Nov 91     GMY    Get constants from SPICE P-constants kernel
   31 Oct 90     GMY    Update radii for Venus, Triton, & Nereid
   25 oct 89     jjl    change name to character*12
                        & numbering to GLL system.
   09 Sept 88 ...SMT... Update all radii and rotation rates
   24 June 87 ...GMY... Change Mars polar radius
   11 June 87 ...GMY... Add Solar range
   11 June 87 ...GMY... Update Saturn and Uranus satellite data
   25 Jan 86  ...GMY... Update Uranus radii and rotation rate
   05 AUG  83   ...CCA...    CONVERT TO VAX
   02 SEPT 82   ...CCA...    UPDATE RADII AND ROTATION RATES
   23 AUG 81   ...GMY...    UPDATE ID AND MIMAS RADII
   05 JUNE 81 ...GMY...  UPDATE SATURN VALUES BASED ON VGR1 DATA
   21 AUG 80   ...GMY...    INITIAL RELEASE
$!-----------------------------------------------------------------------------
$ create pbdata_old.f
C Returns picture body data given pb name (12 characters)
C
      SUBROUTINE PBDATA(name,BUF,*)

      REAL*4 BUF(*)
      INTEGER N,J,K
      INTEGER*4 id(65)
      REAL*4 SRANGE(11)
      REAL*4 RADPE(4,65),
     +       radpe_1(4,16),radpe_2(4,16),radpe_3(4,16),radpe_4(4,17)
      equivalence(radpe_1,radpe(1,1))
      equivalence(radpe_2,radpe(1,17))
      equivalence(radpe_3,radpe(1,33))
      equivalence(radpe_4,radpe(1,49))
      
      REAL*4 ROT(65)
      character*12 p8,pname(65)
      character*12 name
      data n/65/
      data id/
c mercury
     + 1,
c venus
     + 2,
c earth
     + 3,3,
c mars
     + 4,4,4,
c jupiter
     + 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
c saturn
     + 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
c uranus
     + 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
c neptune
     + 8,8,8,
c pluto
     + 9,9,
c gaspra
     + 10,
c ida
     + 11/

       Data Pname /   'MERCURY     ','VENUS       ','EARTH       ',
     + 'MOON        ','MARS        ','PHOBOS      ','DEIMOS      ',
     + 'JUPITER     ','IO          ','EUROPA      ','GANYMEDE    ',
     + 'CALLISTO    ','AMALTHEA    ','HIMALIA     ','ELARA       ',
     + 'PASIPHAE    ','SINOPE      ','LYSITHEA    ','CARME       ',
     + 'ANANKE      ','LEDA        ','THEBE       ','ADRASTEA    ',
     + 'METIS       ','SATURN      ','MIMAS       ','ENCELADUS   ',
     + 'TETHYS      ','DIONE       ','RHEA        ','TITAN       ',
     + 'HYPERION    ','IAPETUS     ','PHOEBE      ','JANUS       ',
     + 'EPIMETHEUS  ','HELENE      ','TELESTO     ','CALYPSO     ',
     + 'ATLAS       ','PROMETHEUS  ','PANDORA     ','URANUS      ',
     + 'ARIEL       ','UMBRIEL     ','TITANIA     ','OBERON      ',
     + 'MIRANDA     ','CORDELIA    ','OPHELIA     ','BIANCA      ',
     + 'CRESSIDA    ','DESDEMONA   ','JULIET      ','PORTIA      ',
     + 'ROSALIND    ','BELINDA     ','PUCK        ','NEPTUNE     ',
     + 'TRITON      ','NEREID      ','PLUTO       ','CHARON      ',
     + 'GASPRA      ','IDA         ' /
C
C Planet radii ra,rb,rc,lora in km & degrees
C where ra = (major) equatorial radius at longitude lora
C       rb = equatorial radius at longitude lora + 90 degrees
C       rc = polar radius (km)
      data RADPE_1/
C        MERCURY                         VENUS
     *2439.7,2439.7,2439.7,0.,        6137.,6137.,6137.,0.,
C        EARTH                           MOON
     *6378.14,6378.14,6356.75,0.,     1737.4,1737.4,1737.4,0.,
C        MARS                            PHOBOS
     *3397.,3397.,3375.,0.,             13.4,11.2,9.2,0.,
C        DEIMOS
     *7.5,6.1,5.2,0.,
C        JUPITER                         IO
     *71492.,71492.,66854.,0.,         1830.,1818.7,1815.3,0.,
C        EUROPA                         GANYMEDE
     *1565.,1565.,1565.,0.,            2634.,2634.,2634.,0.,
C        CALLISTO                       AMALTHEA
     *2403.,2403.,2403.,0.,            131.,73.,67.,0.,
C        HIMALIA                        ELARA
     *85.,85.,85.,0.,                  40.,40.,40.,0.,
C        PASIPHAE
     *18.,18.,18.,0./

      data radpe_2/
c        SINOPE
     *14.,14.,14.,0.,
C        LYSITHEA                       CARME
     *12.,12.,12.,0.,                  15.,15.,15.,0.,
C        ANANKE                         LEDA
     *10.,10.,10.,0.,                   5.,5.,5.,0.,
C        THEBE                          ADRASTEA
     *55.,55.,45.,0.,                    13.,10.,8.,0.,
C        METIS
     *20.,20.,20.,0.,
C        SATURN
     *60268.,60268.,54364.,0.,
C        MIMAS                          ENCELADUS
     *210.3,197.4,192.6,0.,            256.2,247.3,244.,0.,
C        TETHYS                         DIONE
     *523.,523.,523.,0.,               560.,560.,560.,0.,
C        RHEA                           TITAN
     *764.,764.,764.,0.,               2575.,2575.,2575.,0.,
C        HYPERION             
     *180.,140.,112.5,0./

      data radpe_3/
c          IAPETUS
     *718.,718.,718.,0.,
C        PHEOBE                         JANUS
     *115.,110.,105.,0.,                97.,95.,77.,0.,
C       EPIMETHEUS                      HELENE
     *69.,55.,55.,0.,                    17.5,17.5,17.5,0.,
C       TELESTO                         CALYPSO
     *15.2,12.5,7.5,0.,                  15.,8.,8.,0.,
C       ATLAS                           PROMETHEUS
     *18.5,17.2,13.5,0.,                74.,50.,34.,0.,
C       PANDORA
     *55.,44.,31.,0.,
C       URANUS 			        ARIEL
     *25559.0,25559.0,24973.0,0.,      581.1,577.9,577.7,0.,
C       UMBRIEL			         TITANIA
     *584.7,584.7,584.7,0.,            788.9,788.9,788.9,0.,
C       OBERON                          MIRANDA
     *761.4,761.4,761.4,0.,            240.4,234.2,232.9,0./

      data radpe_4/
C       CORDELIA                        OPHELIA
     *13.0,13.0,13.0,0.,               15.0,15.0,15.0,0.,
C       BIANCA                          CRESSIDA
     *21.,21.,21.,0.,                  31.,31.,31.,0.,
C       DESDEMONA                       JULIET
     *27.,27.,27.,0.,                  42.,42.,42.,0.,
C       PORTIA                          ROSALIND
     *54.,54.,54.,0.,                  27.,27.,27.,0.,
C       BELINDA                         PUCK
     *33.,33.,33.,0.,                  77.,77.,77.,0.,
C       NEPTUNE                         TRITON
     *25269.,25269.,24800.,0.,         1352.6,1352.6,1352.6,0.,
C       NEREID
     *170.,170.,170.,0.,
C       PLUTO                           CHARON
     *1162.,1162.,1162.,0.,            606.,606.,606.,0.,
C       GASPRA                          IDA
     *9.,5.5,5.,0.,                    28.,12.,10.5,0./


C AXIAL ROTATION RATE (DEGREES PER DAY)
      data ROT/
C MERCURY,VENUS,EARTH,MOON
     &6.1385025,-1.4813291,360.9856235,13.1763581,
C MARS,PHOBOS,DEIMOS
     &350.9819830,1128.8444790,285.161903,
C JUPITER,IO,EUROPA,GANYMEDE,CALLISTO,
     &870.536,203.4889538,101.3747235,50.3176081,21.5710715,
C AMALTHEA,HIMALIA,ELARA,PASIPHAE,SINOPE,LYSITHEA,
     &722.6303746,1.4365522,1.384083,0.4897959,0.474934,1.3846153,
C CARME,ANANKE,LEDA,THEBE,ADRASTEA,METIS
     &0.5202312,0.5834683,1.5,0.,0.,0.,
C SATURN,MIMAS,ENDELADUS,TETHYS,DIONE,
     &810.7939024,381.994555,262.7318996,190.6979085,131.5349316,
C RHEA,TITAN,HYPERION,IAPETUS,PHOEBE,JANUS,EPIMETHEUS,HELENE,TELESTO,
     &79.6900478,22.5769768,16.91729,4.5379572,0.6540697,0.,0.,0.,0.,
C CALYPSO,ATLAS,PROMETHEUS,PANDORA,
     &0.,0.,0.,0.,
C URANUS,ARIEL,UMBRIEL,TITANIA,OBERON,
     &-501.1600928,-142.8356681,-86.8688923,-41.3514316,-26.7394932,
C MIRANDA,CORDELIA,OPHELIA,BIANCA,CRESSIDA,DESDEMONA,JULIET,PORTIA,
     &-254.6906892,0.,0.,0.,0.,0.,0.,0.,
C ROSALIND,BELINDA,PUCK,NEPTUNE,TRITON,NEREID,PLUTO,CHARON
     &0.,0.,0.,483.7625981,-61.2572684,0.999549,-56.364,-56.3624607,
C GASPRA,IDA
     &0.,0./

C Mean solar ranges of the planets (AU)...
      data SRANGE/0.387098,0.723331,1.0,1.523679,5.2027,9.546,
     &   19.2,30.09,39.5,2.2016,2.9485/
C

      p8=name
C
      DO 50 J=1,N
      IF(PNAME(J).EQ.P8) GOTO 51
   50 CONTINUE
      RETURN1
C
   51 CALL MVE(7,4,RADPE(1,J),BUF,1,1)
      IF (ROT(J) .EQ. 0) THEN 
        BUF(5) = 0
      ELSE 
        BUF(5) = 360.D0 / ROT(J)	!Rotation period in days
      ENDIF
      K = ID(J)
      BUF(6) = 149597871.D0*SRANGE(K)	!Solar range in km
      RETURN
      END
$ Return
$!#############################################################################
