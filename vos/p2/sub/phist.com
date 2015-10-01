$!****************************************************************************
$!
$! Build proc for MIPL module phist
$! VPACK Version 1.8, Friday, July 11, 1997, 09:20:27
$!
$! Execute by entering:		$ @phist
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
$ write sys$output "*** module phist ***"
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
$ write sys$output "Invalid argument given to phist.com file -- ", primary
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
$   if F$SEARCH("phist.imake") .nes. ""
$   then
$      vimake phist
$      purge phist.bld
$   else
$      if F$SEARCH("phist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake phist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @phist.bld "STD"
$   else
$      @phist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create phist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack phist.com -
	-s phist.f zphist.c -
	-i phist.imake -
	-t tphist.f tzphist.c tphist.imake tphist.pdf tstphist.pdf -
	-o phist.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create phist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Routine to list integer histogram array
C
      SUBROUTINE PHIST(FREQ,NS,ILOW,IHIGH,ISPIKE,IMODE)
      INTEGER NS,ILOW,IHIGH,ISPIKE,IMODE,COUNT
      INTEGER FREQ(1),MAXT/7.4752E+05/
      CHARACTER*131 LISTO,MSG
      DATA LISTO /' '/
C
      LISTO(131:131) = '*'
      N = IHIGH - ILOW + 1
      IF(N.LE.0.OR.NS.LE.0) GOTO 1000
      D = 1./NS
      R = 100.*D
      msg = ' GRAY      FREQ   PERCENT   0        10        20' //
     C'        30        40        50        60        70        80'//
     C'        90       100'
      CALL xvmessage (msg, ' ')
      N1 = N - 1
   10 MAXS = MAXT
C     ....Search for n+1st highest freq, ignoring lowest and highest levels.
      DO J=1,ISPIKE+1
      MAX = 0
         DO I=2,N1
            IF (FREQ(I).GT.MAX.AND.FREQ(I).LT.MAXS) MAX=FREQ(I)
         ENDDO
      MAXS = MAX
      ENDDO

      IF (MAX.EQ.0) THEN		!If max frequency is zero
         IF (ISPIKE+1.GT.1) THEN        !and spikes.gt.1 then
            ISPIKE = ISPIKE - 1         !reduce number of spikes
            GOTO 10                     !and try again
         ELSE
            MAX=MAX0(FREQ(1),FREQ(N))   !otherwise, use the ends.
            IF (MAX.EQ.0) GOTO 1001     !If all levels zero, print err.
         ENDIF
      ENDIF

      IFLAG = 0
      AVG = 0.
      SIGMA = 0.
      RI = 0.
C
      DO 905 I=1,N
      NCHAR = 129
      IFREQ = FREQ(I)
      IF(IFREQ.GT.0.OR.IMODE.GT.0) GOTO 900
      IF(IFLAG.EQ.0) CALL XVMESSAGE(' ',' ') 
      IFLAG = 1
      GOTO 905
  900 IFLAG = 0
      RFREQ = IFREQ
      AVG = AVG + RFREQ*RI
      SIGMA = SIGMA + RFREQ*RI*RI
      PERCEN = RFREQ*R
      IVAL = MAX0(100*IFREQ/MAX+1,1)
      IF(IVAL.LE.101) GOTO 902
      IVAL = 101
      NCHAR = 131
902   WRITE (LISTO(1:6),'(I5)') ILOW+I-1
      WRITE (LISTO(11:18),'(I5)') IFREQ
      WRITE (LISTO(17:25),'(F8.3)') PERCEN+.0005
      LISTO(30:129) = ' '
      DO COUNT = 39,129,10
           LISTO(COUNT:COUNT)='+'
      ENDDO
      DO IUNCON  =  29,29+IVAL-1
          LISTO(IUNCON:IUNCON) = '*'
      ENDDO
      CALL XVMESSAGE(LISTO(1:NCHAR),' ')
  905 RI = I
C
      SIGMA =(SIGMA - AVG*AVG*D)*D
      IF(SIGMA.GT.0.) SIGMA =SQRT(SIGMA)
    	AVGREY = AVG*D
        CALL XVMESSAGE(' ',' ')
        WRITE(MSG,100)AVGREY
        CALL XVMESSAGE(MSG,' ')
  100   FORMAT('AVERAGE GRAY LEVEL = ', E11.4)
        CALL XVMESSAGE(' ',' ')

        WRITE(MSG,200)SIGMA
        CALL XVMESSAGE(MSG,' ')
  200   FORMAT('STANDARD DEVIATION = ', E11.4)
        CALL XVMESSAGE(' ',' ')

        WRITE(MSG,300)NS
        CALL XVMESSAGE(MSG,' ')
  300   FORMAT('NUMBER OF ELEMENTS = ', I9)
        CALL XVMESSAGE(' ',' ')
      RETURN
C
 1000 CALL XVMESSAGE(' **ERR IN PHIST ARGUMENT LIST',' ')
      CALL XVMESSAGE(' NO HISTOGRAM WILL BE PRINTED',' ')
      RETURN
 1001 CALL XVMESSAGE(' ***Histogram contains all zeroes',' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zphist.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the PHIST C Subroutine for the phist.f program  */
/*  which produces a histogram.                             */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zphist(freq,ns,ilow,ihigh,ispike,imode)

int *freq,ns,ilow,ihigh,ispike,imode;

{
     FTN_NAME(phist)(freq,&ns,&ilow,&ihigh,&ispike,&imode);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create phist.imake
/* Imake file for VICAR subroutine PHIST */

#define SUBROUTINE phist

#define MODULE_LIST phist.f zphist.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tphist.f
C TPHIST IS A PROGRAM USED TO TEST THE SUBROUTINE PHIST WHICH PRODUCES
C  A HISTOGRAM

      SUBROUTINE TPHIST()

      INTEGER FREQ(20)/1,2,3,5,5,6,7,8,9,0,1,5,3,4,5,6,7,8,9,0/
      INTEGER FREQ2(255),FREQ3(255),FREQ4(255),FREQ5  
      DIMENSION FREQ5(0:254)

      ILOW = 0
      IHIGH = 19
      ISPIKE = 1
      IMODE = 0
      NS = 20
      CALL PHIST(FREQ,NS,ILOW,IHIGH,ISPIKE,IMODE)
      DO 5 I = 1,255
5	  FREQ2(I) = I
      ILOW = 0
      IHIGH = 254
      ISPIKE = 1
      IMODE = 0
      NS=254
      CALL PHIST(FREQ2,NS,ILOW,IHIGH,ISPIKE,IMODE)
      DO I=1,99
        FREQ3(I) = 0
      END DO
      DO 15 I = 100,255
15	  FREQ3(I) = I
      ILOW = 0
      IHIGH = 154
      ISPIKE = 125
      IMODE = 0
      NS=154
      CALL PHIST(FREQ3,NS,ILOW,IHIGH,ISPIKE,IMODE)
      DO 25 I = 1,100
25	  FREQ4(I) = I
      ILOW = 0
      IHIGH = 99
      ISPIKE = 0
      IMODE = 0
      NS=100
      CALL PHIST(FREQ4,NS,ILOW,IHIGH,ISPIKE,IMODE)
      DO 35 I = 0,254
35	  FREQ5(I) = I
      ILOW = 0
      IHIGH = 254
      ISPIKE = 5
      IMODE = 0
      NS=255
      CALL PHIST(FREQ5,NS,ILOW,IHIGH,ISPIKE,IMODE)
      END
$!-----------------------------------------------------------------------------
$ create tzphist.c
/*  This is the test program for the C-Callable portion of PHIST */
/*  which is a subroutine that produces a histogram.             */

#include "vicmain_c"
#include "ftnbridge.h"

void main44()
{
     int freq[] = { 1,2,3,5,5,6,7,8,9,0,1,5,3,4,5,6,7,8,9,0};
     int freq2[256],freq3[256],freq4[256],freq5[256];
     int ilow,ihigh,ispike,imode,ns,i;

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     ilow = 0;
     ihigh = 19;
     ispike = 1;
     imode = 0;
     ns= 20;

     zphist(freq,ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 254;
     ispike = 1;
     imode = 0;
     ns= 254;

     for (i=1; i<256; i++)
     {
          freq2[i] = i;
     }

     zphist(&freq2[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 154;
     ispike = 125;
     imode = 0;
     ns= 154;

     for (i=1; i<100; i++)
     {
          freq3[i]=0;
     }

     for (i=100; i<256; i++)
     {
          freq3[i] = i;
     }

     zphist(&freq3[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 99;
     ispike = 0;
     imode = 0;
     ns= 100;

     for (i=1; i<101; i++)
     {
          freq4[i] = i;
     }

     zphist(&freq4[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 254;
     ispike = 5;
     imode = 0;
     ns= 255;

     for (i=0; i<255; i++)
     {
          freq5[i] = i;
     }

     zphist(&freq5[0],ns,ilow,ihigh,ispike,imode);

     zvmessage("Test the FORTRAN interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tphist)();

}
$!-----------------------------------------------------------------------------
$ create tphist.imake
/* Imake file for Test of VICAR subroutine PHIST */

#define PROGRAM tphist

#define MODULE_LIST tphist.f tzphist.c

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_ANSI_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tphist.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstphist.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar

body
local dir string
let $autousage="none"
let _onfail="continue"

let $echo="no"
write "General Test"
write ""
let $echo="yes"
tphist

let $echo="no"
write "ENTROPY test with Cassini input"
write ""
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
end-if
let $echo="yes"
entropy &"dir"sum2.1 'phist 'zeroes

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create phist.hlp
1  PHIST

PURPOSE: Prints out a histogram in a format similar to the VICAR program
         LIST. The number of histogram levels to be printed, and the number 
         of spikes within the histogram may be specified. Zero frequency
         levels may be printed where needed.

USAGE:   CALL PHIST(HIS, NFREQ, LOW, HIGH, NSPIKE, MODE)

PARAMETERS:

      HIS    = Fullword integer histogram array.

      NFREQ  = Fullword integer, total number of sample frequencies

      LOW    = Fullword integer, is the optional lower value of the DN range
               included within the histogram (Default: LOW = 0).

      HIGH   = Fullword integer, is the optional upper value of the DN range
               included within the histogram (Default: HIGH = 225).

      NSPIKE = Fullword integer, is the optional number of spikes within the 
               histogram. The LOW and HIGH DN values are excluded when 
               searching for spikes (Default: NSPIKE = 0).

      MODE   = MODE = 1 if zero frequecy levels are to be listed, 
               MODE = 0 otherwise (Default MODE = 0). Optional arguement.


      NOTE: ALL OPTIONALS REQUIREMENTS ARE NOW REQUIRED ARGUMENTS.

2 NOTES

  EXAMPLE

  In this example, a histogram containing 512 grey levels will be 
  printed out.

         INTEGER HIS(512)
                  .
                  .
                  .
         NFREQ = 0
         DO I = 1,512
            NFREQ = NFREQ + HIS(I)
         ENDDO
         LOW = 0
         HIGH = 511
         NSPIKE = 5
         MODE = 0
    
         CALL PHIST(HIS, NFREQ, LOW, HIGH, NSPIKE, MODE) 

HISTORY

Original Programmer: H. J. Frieden, 2 June 1974
Current Cognizant Programmer: Lucas W. Kamp, 1 May 1983
Source Language: Fortran
Revision:
  11 Jul 97  T. Huang  ...Changed GRAY index from a 4-character display
                          to a 5-character display for Cassini support.
                          Cassini has data range (-4095..4095).
                       ...Modified tphist.imake to use ANSI C.  This allows
                          tzphist.c to be compiled under HP.
  24 Oct 96  S. Chang  ...Fixed histogram legend display.  (FR89363)
   1 SEP 93  D. Knight ...Ported to Unix
  14 Jan 89  G. Yagi   ...Reduce spikes of it results in 0 max frequency.
  21 JUN 84  C. Avis   ...ADDED '.'S TO PRNT CALLS
  18 MAY 83  L. Kamp   ...CONVERTED TO VAX.
  04 MAR 75  G. Yagi   ...Added four optional arguements, calls to ARGQ and MVE.
$ Return
$!#############################################################################
