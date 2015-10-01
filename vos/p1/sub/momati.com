$!****************************************************************************
$!
$! Build proc for MIPL module momati
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:16
$!
$! Execute by entering:		$ @momati
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
$ write sys$output "*** module momati ***"
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
$ write sys$output "Invalid argument given to momati.com file -- ", primary
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
$   if F$SEARCH("momati.imake") .nes. ""
$   then
$      vimake momati
$      purge momati.bld
$   else
$      if F$SEARCH("momati.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake momati
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @momati.bld "STD"
$   else
$      @momati.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create momati.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack momati.com -mixed -
	-s momati.f zmomati.c momati_c.c -
	-i momati.imake -
	-t tmomati.f tzmomati.c tmomati.imake tmomati.pdf tstmomati.pdf -
	-o momati.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create momati.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C/*   22 JULY 80   ...GMY...    INITIAL RELEASE */
c     Sept. 1992   ...WPL... Ported for UNIX Conversion
c
      SUBROUTINE MOMATI(OAL,OAS,SSL,SSS,SCALE,FL,SCLO,SCLA,ANGN,RANGE,
     &A,RS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(3,3),RS(3)
c
C THIS ROUTINE COMPUTES THE PLANET TO CAMERA ROTATION MATRIX (A) AND THE
C VECTOR FROM PLANET CENTER TO SPACECRAFT (RS), EXPRESSED IN THE PLANET
C SYSTEM. A VECTOR V IN THE PLANET SYSTEM IS THEN RELATED TO THE SAME
C VECTOR VP IN THE CAMERA SYSTEM by
c
C          VP = A*V + RS   ,   Note  A is a 3x3  Matrix
c
C REQUIRED NAVIGATION INFORMATION ...
C  OAL,OAS=LINE,SAMPLE LOCATION OF OPTIC AXIS
C  SSL,SSS=LINE,SAMPLE LOCATION OF PLANET CENTER
C  SCLA=PLANETOCENTRIC LATITUDE OF S/C (DEG)
C  SCLO=WEST LONGITUDE OF S/C
C  ANGN=NORTH ANGLE AT OPTIC AXIS INTERCEPT POINT
C  RANGE=RANGE FROM S/C TO PLANET CENTER
C  SCALE=SCALE IN PIXELS/MM
C  FL=FOCAL LENGTH IN MM
C
      PI = 3.141592653589D0
      RAD = PI/180.D0
      CLA = DCOS(SCLA*RAD)
      SLA = DSIN(SCLA*RAD)
      CLON = DCOS(SCLO*RAD)
      SLON = DSIN(SCLO*RAD)
C
C          THIS SECTION COMPUTES ROTATIONS A AND B
C          FIRST COMPUT RS VECTOR IN CAMERA SYSTEM
c
      X = SSS - OAS
      Y = SSL - OAL
      Z = FL*SCALE
      D = DSQRT(X*X+Y*Y+Z*Z)
c
C          CHECK FOR SPECIAL CASES
c
      ICASE = 0
c
C          INITIALLY, NA=0, A=90, B=0 (CASE 1)
c
      CNA = 1.D0
      SNA = 0.D0
      CRA = 0.D0
      SRA = 1.D0
      IF(SCLA.LT.0.D0) SRA=-1.D0
      CRB = 1.D0
      SRB = 0.D0
      D1 = DSQRT(X*X+Y*Y)
      IF(DABS(SCLA).NE.90.D0) GOTO 3
      IF(D1.EQ.0.D0) GOTO 5
C
C          CASE 2  SCLA=90, (OAL,OAS).NE.(SSL,SSS)
c
      ICASE = 2
      CLON = -CLON
      SLON = -SLON
      CNA = Y/D1
      SNA = -X/D1
      IF(SCLA.GT.0.) GOTO 10
      CNA = -CNA
      SNA = -SNA
      GOTO 10
C
C
3     IF(ANGN.NE.-999.) GOTO 6
      IF(D1.EQ.0.D0) GOTO 5
c
C          CASE 3  SPIN AXIS PARALLEL TO OPTIC AXIS
c
      CNA = Y/D1
      SNA = -X/D1
      IF(SCLA.LT.0.) GOTO 4
      CNA = -CNA
      SNA = -SNA
4     CLA = DABS((-X*SNA+Y*CNA)/D)
      SLA = Z/D
      IF(SCLA.LT.0.) SLA=-SLA
      GOTO 15
C
C          CASE 1  SCLA=90, (OAL,OAS)=(SSL,SSS)
5     CLA = 0.D0
      SLA = -1.D0
      IF(SCLA.LT.0.) GOTO 15
      SLA = 1.D0
      CLON = -CLON
      SLON = -SLON
      GOTO 15
C
C          NORMAL CASE
c
6     CNA = DCOS(ANGN*RAD)
      SNA = DSIN(ANGN*RAD)
C          ROTATE ABOUT Z-AXIS SO THAT NORTH IS UP IN IMAGE
10    CX = (X*CNA + Y*SNA)/D
      CY = (-X*SNA+ Y*CNA)/D
      CZ = Z/D
c
C          ROTATE ABOUT X-AXIS SO THAT -Y AXIS IS PARALLEL TO SPIN AXIS.
c
      D = CY*CY + CZ*CZ
      G = D - SLA*SLA
      IF(G.LT.0.) G=0.
      G = DSQRT(G)
      CRA = (CY*SLA+CZ*G)/D
      SRA = (CZ*SLA-CY*G)/D
      IF(ICASE.NE.0) GOTO 15
      CZ = CZ*CRA - CY*SRA
      D = DSQRT(CX*CX+CZ*CZ)
      CRB = CZ/D
      SRB = CX/D
C
C          THIS SECTION COMPUTES RESULTANT ROTATION MATRIX
C          A MATRIX IS INITIALLY NORTH ANGLE ROTATION ABOUT Z-AXIS
c
C     changed by Roatsch, March 2000, was
c15    CALL ZIA(A,18)
15    Continue
      A(1,3) = 0.d0
      A(2,3) = 0.d0
      A(3,1) = 0.d0
      A(3,2) = 0.d0
      
c     continues original code      
      A(1,1) = CNA
      A(2,1) = SNA
      A(1,2) = -SNA
      A(2,2) = CNA
      A(3,3) = 1.D0
c
C          ROTATE ABOUT X-AXIS SO THAT -Y AXIS IS PARALLEL TO SPIN AXIS.
C          A = M1*A
      DO 20 I=1,3
      A2 = CRA*A(I,2) + SRA*A(I,3)
      A(I,3) = -SRA*A(I,2) + CRA*A(I,3)
20    A(I,2) = A2

c
C          ROTATE ABOUT Y-AXIS SO THAT PLANET CENTER LIES IN Y-Z PLANE.
C          A = M2*A
c
      DO 30 I=1,3
      A1 = CRB*A(I,1) - SRB*A(I,3)
      A(I,3) = SRB*A(I,1) + CRB*A(I,3)
30    A(I,1) = A1
C          INTERCHANGE COORDINATES SO THAT
C              XP = -Z, YP=X, ZP=-Y
      DO 40 I=1,3
      A1 = A(I,1)
      A2 = A(I,2)
      A(I,1) = -A(I,3)
      A(I,2) = A1
40    A(I,3) = -A2
c
C          ROTATE ABOUT Z-AXIS SO THAT X-AXIS POINTS TO LON 0, LAT 0.
C          A = M3*A
c
      DO 50 I=1,3
      A1 = CLON*A(I,1) + SLON*A(I,2)
      A(I,2) = -SLON*A(I,1) + CLON*A(I,2)
50    A(I,1) = A1
C
C          COMPUTE VECTOR FROM PLANET CENTER TO S/C, IN PLANET SYSTEM.
c
      RS(1) = RANGE*CLA*CLON
      RS(2) = -RANGE*CLA*SLON
      RS(3) = RANGE*SLA
c
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zmomati.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "momati.h"
void zmomati(double oal, double oas,double ssl,double sss,double scale,
	     double fl,double sclo,double scla,double angn,double range,
	     void *a,void *rs)
{
momati_c(oal,oas,ssl,sss,scale,fl,sclo,scla,angn,range,a,rs);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create momati_c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* momati_c rewritten from Fortan
   Thomas Roatsch, DLR, 5-Mar-2001 */
   

/*  22 JULY 80   ...GMY...    INITIAL RELEASE
    Sept. 1992   ...WPL... Ported for UNIX Conversion

THIS ROUTINE COMPUTES THE PLANET TO CAMERA ROTATION MATRIX (A) AND THE
VECTOR FROM PLANET CENTER TO SPACECRAFT (RS), EXPRESSED IN THE PLANET
SYSTEM. A VECTOR V IN THE PLANET SYSTEM IS THEN RELATED TO THE SAME
VECTOR VP IN THE CAMERA SYSTEM by

         VP = A*V + RS   ,   Note  A is a 3x3  Matrix

REQUIRED NAVIGATION INFORMATION ...
 OAL,OAS=LINE,SAMPLE LOCATION OF OPTIAXIS
 SSL,SSS=LINE,SAMPLE LOCATION OF PLANET CENTER
 SCLA=PLANETOCENTRILATITUDE OF S/(DEG)
 SCLO=WEST LONGITUDE OF S/C
 ANGN=NORTH ANGLE AT OPTIAXIS INTERCEPT POINT
 RANGE=RANGE FROM S/TO PLANET CENTER
 SCALE=SCALE IN PIXELS/MM
 FL=FOCAL LENGTH IN MM */

#include <math.h>

void momati_c(double oal, double oas, double ssl, double sss,
              double scale, double fl, double sclo, double scla,
              double angn, double range, double a[3][3], double rs[3])

{

double pi, rad;
double cla, sla, clon, slon;
double x,y,z,d;
int    icase,i;
double cna,sna,cra,sra,crb,srb,d1;
double cx,cy,cz,g,a1,a2;

pi = 3.141592653589;
rad = pi/180.0;
cla = cos(scla*rad);
sla = sin(scla*rad);
clon = cos(sclo*rad);
slon = sin(sclo*rad);

/* THIS SECTION COMPUTES ROTATIONS A AND B
   FIRST COMPUT RS VECTOR IN CAMERA SYSTEM */
   
x = sss - oas;
y = ssl - oal;
z = fl*scale;
d = sqrt(x*x+y*y+z*z);

/* CHECK FOR SPECIAL CASES */
icase = 0;


/* INITIALLY, NA=0, A=90, B=0 (CASE 1) */

cna = 1.0;
sna = 0.0;
cra = 0.0;
sra = 1.0;
if (scla < 0) sra=-1.0;
crb = 1.0;
srb = 0.0;
d1 = sqrt(x*x+y*y);
if(fabs(scla) != 90) goto go_3;
if(d1 == 0) goto go_5;

/* CASE 2  SCLA=90, (OAL,OAS).NE.(SSL,SSS) */

icase = 2;
clon = -clon;
slon = -slon;
cna = y/d1;
sna = -x/d1;
if(scla > 0) goto go_10;
cna = -cna;
sna = -sna;
goto go_10;

go_3: 

if(angn != -999.) goto go_6;
if(d1 == 0) goto go_5;

/* CASE 3  SPIN AXIS PARALLEL TO OPTIC AXIS */

cna = y/d1;
sna = -x/d1;
if(scla < 0) goto go_4;
cna = -cna;
sna = -sna;

go_4:     

cla = fabs((-x*sna+y*cna)/d);
sla = z/d;
if(scla < 0.0) sla=-sla;
goto go_15;

/* CASE 1  SCLA=90, (OAL,OAS)=(SSL,SSS) */
go_5:     

cla = 0.0;
sla = -1.0;
if(scla < 0) goto go_15;
sla = 1.0;
clon = -clon;
slon = -slon;
goto go_15;

/* NORMAL CASE */
go_6:

cna = cos(angn*rad);
sna = sin(angn*rad);
/* ROTATE ABOUT Z-AXIS SO THAT NORTH IS UP IN IMAGE */
go_10:

cx = (x*cna + y*sna)/d;
cy = (-x*sna+ y*cna)/d;
cz = z/d;

/* ROTATE ABOUT X-AXIS SO THAT -Y AXIS IS PARALLEL TO SPIN AXIS. */

d = cy*cy + cz*cz;
g = d - sla*sla;
if(g < 0) g=0.0;
g = sqrt(g);
cra = (cy*sla+cz*g)/d;
sra = (cz*sla-cy*g)/d;
if(icase != 0) goto go_15;
cz = cz*cra - cy*sra;
d = sqrt(cx*cx+cz*cz);
crb = cz/d;
srb = cx/d;

/* THIS SECTION COMPUTES RESULTANT ROTATION MATRIX
   A MATRIX IS INITIALLY NORTH ANGLE ROTATION ABOUT Z-AXIS */

go_15:

a[2][0] = 0.0;
a[2][1] = 0.0;
a[0][2] = 0.0;
a[1][2] = 0.0;
      
a[0][0] = cna;
a[0][1] = sna;
a[1][0] = -sna;
a[1][1] = cna;
a[2][2] = 1.0;

/* ROTATE ABOUT X-AXIS SO THAT -Y AXIS IS PARALLEL TO SPIN AXIS .
   A = M1*A */
for (i=0; i<3; i++)
   {
   a2 = cra*a[1][i] + sra*a[2][i];
   a[2][i] = -sra*a[1][i] + cra*a[2][i];
   a[1][i] = a2;
   }
   
/* ROTATE ABOUT Y-AXIS SO THAT PLANET CENTER LIES IN Y-Z PLANE.
   A = M2*A */

for (i=0; i<3; i++)
   {  
   a1 = crb*a[0][i] - srb*a[2][i];
   a[2][i] = srb*a[0][i] + crb*a[2][i];
   a[0][i] = a1;
   }

/* INTERCHANGE COORDINATES SO THAT
   XP = -Z, YP=X, ZP=-Y */

for (i=0; i<3; i++)
   {
   a1 = a[0][i];
   a2 = a[1][i];
   a[0][i] = -a[2][i];
   a[1][i] = a1;
   a[2][i] = -a2;
   }
/* ROTATE ABOUT Z-AXIS SO THAT X-AXIS POINTS TO LON 0, LAT 0.
   A = M3*A */

for (i=0; i<3; i++)
   {
   a1 = clon*a[0][i] + slon*a[1][i];
   a[1][i] = -slon*a[0][i] + clon*a[1][i];
   a[0][i] = a1;
   }

/* COMPUTE VECTOR FROM PLANET CENTER TO S/C, IN PLANET SYSTEM.*/

rs[0] = range*cla*clon;
rs[1] = -range*cla*slon;
rs[2] = range*sla;

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create momati.imake
/* Imake file for VICAR subroutine MOMATI*/

#define SUBROUTINE  momati

#define MODULE_LIST momati.f zmomati.c momati_c.c  

#define P1_SUBLIB

#define USES_C
#define USES_FORTRAN

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$ Return
$!#############################################################################
$Test_File:
$ create tmomati.f
        Include 'VICMAIN_FOR'
c
C-----THIS PROGRAM TESTS SUBROUTINE FORMOM (IBM FARENC)
        Subroutine  Main44
        common/c1/language_flag
	REAL*4 MAP(40)
        Character*10  C
C
C-----SET UP THE TEST INPUT BUFFER
	MAP(25) = 66773.
	MAP(26) = 71400.
	MAP(27) = 1500.1904
	MAP(28) = 500.
	MAP(29) = 500.
	MAP(30) = 84.821431
	MAP(31) = 3.4825339
	MAP(32) = 116.72441
	MAP(33) = -121.600
	MAP(34) = 1662.700
	MAP(35) = 160.8923
	MAP(38) = 14967727.
	I = 5
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(36), 4)
c	CALL MVL(I,MAP(36),4)
	I = 7
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(37), 4)
c	CALL MVL(I,MAP(37),4)
	I = 8
        Call MVLC(I, C, 4)
        Call MVCL(C, MAP(39), 4)
c	CALL MVL(I,MAP(39),4)
	MAP(40) = 0.
	CALL PRNT(7,11,MAP(25),' MAP(25-35) = . ')
	CALL PRNT(4,2,MAP(36),' MAP(36-37) = . ')
	CALL PRNT(7,1,MAP(38),' MAP(38) = . ')
	CALL PRNT(4,2,MAP(39),' MAP(39-40) = . ')

        CALL XVMESSAGE('Test of Fortran version',' ')
        language_flag = 1
C-----CALL THE SUBROUTINE TO BE TESTED
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
C-----CALL THE SUBROUTINE TO BE TESTED
        language_flag = 2
        CALL XVMESSAGE('Test of C version via zmomati',' ')
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
C-----CALL THE SUBROUTINE TO BE TESTED
        language_flag = 0
        CALL XVMESSAGE('Test of C version directly',' ')
	CALL FORMOM(MAP)
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
	CALL PRNT(8,9,MAP,' OM MATRIX = . ')
	CALL PRNT(8,3,MAP(19),' RS VECTOR = . ')
c
        Return
	END



C     THIS IS IBM SUBROUTINE FARENC   -----NAME CHANGE-----
C     2 FEB 83   ...CCA...     INITIAL RELEASE
c       Sep 92   ...WPL...     Ported for UNIX Conversion
c
      Subroutine  FORMOM(DATA)
c
C Routine to set up CAMERA Pointing Geometry INFO for calculation of 
C the Planet-to-Camera ROTATION MATRIX (OM).
c
      IMPLICIT REAL*8 (A-Z)
      common/c1/language_flag
      integer*4 language_flag
      REAL*4 DATA(1)

C
      PI = 3.141592653589793D0
      RADDEG = 180.D0 / PI
      DEGRAD = PI / 180.D0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      LAT = DATA(31)
      LON = DATA(32)
      LSS = DATA(33)
      SSS = DATA(34)
      NA = DATA(35)
      D = DATA(38)
c
c  Convert from Geodetic to Geocentric Latitude
c
      IF(DABS(LAT).EQ.90.D0) GOTO 10
      RP = DATA(25)
      RE = DATA(26)
      E = RE/RP
      LAT = DATAN(DTAN(LAT*DEGRAD)/E**2)*RADDEG

10    if (language_flag .eq.1) then
          Call MOMATI(OAL,OAS,LSS,SSS,SCALE,FL,LON,LAT,NA,D,DATA,
     &                DATA(19))
      else 
          Call TZMOMATI(language_flag,OAL,OAS,LSS,SSS,SCALE,FL,
     &	   LON,LAT,NA,D,DATA,DATA(19))
      endif
      Return
      END

$!-----------------------------------------------------------------------------
$ create tzmomati.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzmomati)(m,oal,oas,ssl,sss,scale,fl,sclo,scla,angn,range,a,rs)
int *m;
double *oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range;
void *a, *rs;
{
/*  ============================================  */
  if (*m) {
    zvmessage(" Calling zmomati ...","");
    zmomati(*oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range,a,rs);
  }
  else {
    zvmessage(" Calling momati_c directly ...","");
    momati_c(*oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range,a,rs);
  }
}
$!-----------------------------------------------------------------------------
$ create tmomati.imake
/* IMAKE file for Test of VICAR subroutine  MOMATI  */

#define PROGRAM  tmomati

#define MODULE_LIST tmomati.f tzmomati.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define   LIB_RTL         
#define   LIB_TAE           
#define   LIB_P2SUB         

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$!-----------------------------------------------------------------------------
$ create tmomati.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstmomati.pdf
Procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULES MOMATI AND FORMOM (IBM FARENC)
!THE TEST PROGRAM SETS UP A DATA BUFFER AND CALLS FORMOM.
!FORMOM REFORMATS THE BUFFER AND CALLS MOMATI.
!ELEMENTS 25-40 OF THE BUFFER ARE INPUT VALUES.
!ELEMENTS 1-24 ARE THE OUTPUT VALUES.
!USING THE SAME INPUT BUFFER THE OUTPUT VALUES ON THE IBM ARE:
! OM -.83183 .31319 .458217 .448535 -.106932 .8873447
!    .32691 .943649 -.05153 
! RS  -6721453.83  -13349977.3  795527.9343
TMOMATI
Let $echo="no"
End-proc
$ Return
$!#############################################################################
$Other_File:
$ create momati.hlp
1  MOMATI

     To compute the planet to spacecraft rotation matrix (OM) and
     displacement vector (RS) in the MAP2 far encounter mode.

2  CALLING SEQUENCE:

     CALL MOMATI(LO,SO,LSS,SSS,SCALE,FL,SSLON,SSLAT,NA,RANGE,OM,RS)

2  ARGUMENTS:

     LO       Optical axis line        object space pixels
     SO       Optical axis sample      object space pixels
     LSS      Sub spacecraft point     object space line
     SSS      Sub spacecraft point     object space sample
     SCALE    Picture scale            object space pixels/mm.
     FL       Focal length             mm.
     SSLON    Sub spacecraft point     longitude, degrees west
     SSLAT    Sub spacecraft point     geocentric latitude, degrees
     NA       North angle, degrees clockwise from up of spin axis
     RANGE    Distance planet center to spacecraft, mm.
     OM       OM matrix                9 elements
     RS       RS vector                3 elements

     All arguments must be in REAL*8 format.  The OM matrix and 
     RS vector are computed and returned by the program.  All
     other arguments are inputs to the program.

     Restrictions:

     Subroutines called:  ZIA, DCOS, DSIN, DSQRT

     Core requirements:  2540 bytes


2  OPERATION

     The program differs from the subroutine MOMATV only in the 
     definition of the north angle.  This subroutine uses a north
     angle which is consistent with the north angle currently
     available in the Voyager SEDR.

2  HISTORY

     Original Programmer: Gary Yagi, 4 July 1980
     Current Cognizant Programmer: Gary Yagi
     Documentation Author: Gary Yagi
     Source Language: Fortran
     Revision: 1, 10 September 1980

     Revision Summary:

     This revision corrects for an error in the original documentation.
     The arguments SSLON and SSLAT are inverted.  No change has been
     made to the program itself. 

     Ported for UNIX Conversion:  W.P. Lee,   October 1992
     Added C bridge            :  F.M. Moss,  January 1994 (FR 76816)
$ Return
$!#############################################################################
