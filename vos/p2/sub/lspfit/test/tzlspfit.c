#include "xvmaininc.h"
#include "zlspfit.h"
#include "ftnbridge.h"

#define MAX_ROW 100
#define MAX_COL 4

void zptran (int, double [], int, float [][MAX_COL]);
int zlfit (int, float [][MAX_COL], int, double [], double [], double [], 
           double, double, int);


void FTN_NAME(tzlspfit) (void)
{
   int n, ifit, imax, ier;
   float pts[MAX_ROW][MAX_COL];
   double mom[36], umom[12];
   double pi, dtr, rtd, drms, dmax;

   double coef[12] = 
       {1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

   zvmessage ("*** Testing C-bridge interfaces ***","");
   pi = 3.141592653589793;
   dtr = pi/180.0;
   rtd = 180.0/pi;
   drms = 0.0;
   dmax = 0.0;
   imax = 0;

   pts[0][0] = 1.0;
   pts[0][1] = 1.0;
   pts[1][0] = 1.0;
   pts[1][1] = 400.0;
   pts[2][0] = 1.0;
   pts[2][1] = 800.0;
 
   pts[3][0] = 400.0;
   pts[3][1] = 1.0;
   pts[4][0] = 400.0;
   pts[4][1] = 400.0;
   pts[5][0] = 400.0;
   pts[5][1] = 800.0;
 
   pts[6][0] = 800.0;
   pts[6][1] = 1.0;
   pts[7][0] = 800.0;
   pts[7][1] = 400.0;
   pts[8][0] = 800.0;
   pts[8][1] = 800.0;

   n = 9;

   ifit = 1;
   zptran (ifit, coef, n, pts);
   if (zlfit (ifit, pts, n, coef, mom, umom, drms, dmax, imax))
      zmabend ("***TZLSPFIT task cancelled.");

   coef[0] = 6.0;
   coef[1] = 5.0;
   coef[2] = 4.0;
   coef[3] = 3.0;
   coef[4] = 2.0;
   coef[5] = 1.0;
   coef[6] = 12.0;
   coef[7] = 11.0;
   coef[8] = 10.0;
   coef[9] = 9.0;
   coef[10] = 8.0;
   coef[11] = 7.0;

   ifit = 9;
   zptran (ifit, coef, n, pts);
   if (zlfit (ifit, pts, n, coef, mom, umom, drms, dmax, imax))
      zmabend ("***TZLSPFIT task cancelled.");

   zvmessage ("TZLSPFIT task completed.","");
   return; 
}


void zptran (int ifit, double coef[], int n, float pts[][MAX_COL])
{
   int npow, nu, i;
   float x, y, u, v;
   float blank[132];
   char msg[132];
 
   for (i=0; i<132; i++)
      blank[i] = 0.;
   npow = ((ifit-7) > 1) ? (ifit - 7) : 1;
   nu = (npow+1) * (npow+2);
   zprnt (8, nu, coef, "INITIAL COEF=.");
   for (i=0; i < n; i++)
   {
      x = pts[i][0];
      y = pts[i][1];
      zpolytran (npow, coef, x, y, &u, &v);
      zmove (blank, msg, 132);
      sprintf (msg, "%10.1f %10.1f %10.1f %10.1f ", x, y, u, v);
      zvmessage (msg, "");
      pts [i][2] = u;
      pts [i][3] = v;
 
   }
}
 
 
int zlfit (int ifit, float pts[][MAX_COL], int n, double coef[], double mom[],
            double umom[], double drms, double dmax, int imax)
{
   int npow, nu, ier;
   double temp=0.0;
 
   ier = -1;
   npow = ((ifit-7) > 1) ? (ifit - 7) : 1;
   nu = (npow+1) * (npow+2) / 2;
   if (nu > n) return ier;
   zmoment (npow, (float *)pts, n, mom, umom);
   zmve (8, 12, &temp, coef, 0, 1);
   zprnt (4, 1, &ifit, "IFIT=.");
   if (ifit < 8)
      ier = zclfit (ifit, mom, umom, coef);
   else
      ier = zlspfit (npow, mom, umom, coef);
   zrmsfit (0,npow,(float *)pts,n,coef,&drms,&dmax,&imax);
   zprnt (8, 2*nu, coef, "COEF=.");
   zprnt (8, 1, &drms, "RMS=.");
   zprnt (8, 1, &dmax, "MAX ERROR=.");
   return ier;
}

