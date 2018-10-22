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
