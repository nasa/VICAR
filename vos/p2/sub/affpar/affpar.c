/* #######################################################################
  
   affinpar() function determines coefficients for an affin transformation,
              and calculates the transformation for a given point.
                 
            call sequence:
                 
                 st = affinpar (lne_in, smp_in,
                                n,  
                                lne_0, smp_0, 
                                lne_1, lne_1, 
                               &lne_out, &smp_out,
                                a);
                    
                 with:   
                   IN: 
                     double	lne_in		line coordinate in system 0; 
						which is to transfer in the 
						system 1
                     double     smp_in		sample coordinate in system 0; 
						which is to transfer in the 
						system 1
                     int        n		number of the points,
                     double *   lne_0[i]	line coordinate vector for 
						system 0,
                     double *   smp_0[i]	sample coordinate vector for 
						system 0,
                     double *   lne_1[i]	line coordinate vector for 
						system 1,
                     double *   smp_1[i]	sample coordinate vector for 
						system 1,
                   OUT:
                     double *   lne_out		lne_in coordinate in system 1 
						transfered from system 0
                                     lne_out = a[0]*lne_in + a[1]*smp_in + a[2]
                     double     smp_out		smp_in coordinate in system 1 
						transfered from system 0
                                     smp_out = a[3]*lne_in + a[4]*smp_in + a[5]

                     double *   a[i]		coefficient vector (6 unknown 
						parameters).

                   return value: 

                     st = 1 if everything is alright;
		            else st < 1

             the mathematic modell:
             
                 lne_1[i] = lne_0[i] * a[0] + smp_0[i] * a[1] + a[2] , 
                 smp_1[i] = lne_0[i] * a[3] + smp_0[i] * a[4] + a[5] . 


   WRITTEN BY: F.Wewel,   DLR     17-Jun-1997 
               Version:   1.201   21-Oct-1997
   ####################################################################### */

#include "affpar.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static double *d_alloc (long, int *);
static double inv_mtrx (double *, double *, int);
static int affin1_koff (double *, double *, double *, double *, double *,
                        double *, double *, double *, int, double, int);
static int affkof_2 (double *, double *, double *, double *, double *);
/* static int inv_33_aff  (double *, double *);  */
static void mtrxmul (double *, double *, double *, int, int, int);

int affinpar (double    lne_in,     double     smp_in,
              int       no_points, 
              double   *lne_0,      double    *smp_0,
              double   *lne_1,      double    *smp_1,
              double   *lne_out,    double    *smp_out,
              double   *affcoef)
   {
   int              status;
   int              anz, i, ix;
   double           z0, s0, z1, s1;
   double           *x0=NULL, *y0, *x1, *y1, *p, aff[6], *vz, *vs;
   int              iter;
   double           iter_eps;
   double           ds, gew;
   double           radius = 100;

   if (no_points < 1) { /* ----- 1 identical point ==> offsets a[2], a[5] */

       affcoef[1] = affcoef[2] = affcoef[3] = affcoef[5] = 0.0;
       affcoef[0] = affcoef[4] = 1.0;

       return (-1);
       }
   else if (no_points == 1) {
   
       affcoef[0] = 1.0;
       affcoef[1] = 0.0;
       affcoef[3] = 0.0;
       affcoef[4] = 1.0;
       
       affcoef[2] = lne_1[0] - lne_0[0];
       affcoef[5] = smp_1[0] - smp_0[0];
   
       *lne_out = lne_in + affcoef[2];
       *smp_out = smp_in + affcoef[5];

       return (1);
       }
   else if (no_points == 2) {
   
       status = affkof_2(lne_0, smp_0, lne_1, smp_1, aff);

   }
   else {
   
     iter     = 1;
     iter_eps = 0.05;
       
/* ----------------------------------------------
                                  Speicherplatz fuer die Berechnung der 
                                  Affintransformations-Koeffizienten */


     x0 = d_alloc((long) (7 * no_points), &status);
     if_EOF_return

     y0 = x0 + no_points;
     x1 = y0 + no_points;
     y1 = x1 + no_points;
      p = y1 + no_points;
     vz =  p + no_points;
     vs = vz + no_points;

  /* ---------------------------------------------
                 Robuste Ausgleichung fuer die Affinkoef.-Berechnung */
      ix = 0;
      for (i = 0; i < no_points; i++) {
  
          z0 = (double) lne_0[i];
          s0 = (double) smp_0[i];

          if (z0 > 0.0 && s0 > 0.0) {
             z1 = (double) lne_1[i];
             s1 = (double) smp_1[i];
             if ( z1 > 0.0 && s1 > 0.0) {

                ds = sqrt ((z0 - lne_in) * (z0 - lne_in) 
                         + (s0 - smp_in) * (s0 - smp_in));
                                             
                if (fabs(ds) < EPS_SCHWARZ) ds = EPS_SCHWARZ;
                
                gew = radius / ds;
                                
                if (fabs(gew) < EPS_SCHWARZ) gew = EPS_SCHWARZ;

                x0[ix] = z0;
                y0[ix] = s0;
                x1[ix] = z1;
                y1[ix] = s1;
                 p[ix] = gew;
 
                ix++;
               }
            }
        }

     anz = ix;
  /* ---------------------------------------------------
                         Iterative Schaetzung der Affinkoeffizienten */
     status =  affin1_koff (x0, y0, x1, y1, p, vz, vs, aff, 
                            iter, iter_eps, anz);
     }
      
   if (status != 1) return(status);

   affcoef[0] = aff[0]; 
   affcoef[1] = aff[1]; 
   affcoef[2] = aff[2]; 
   affcoef[3] = aff[3]; 
   affcoef[4] = aff[4]; 
   affcoef[5] = aff[5]; 

/* ------------------------------------------------------
                                   input lne/smp in the output system */
   *lne_out = aff[0] * lne_in + aff[1] * smp_in + aff[2];
   *smp_out = aff[3] * lne_in + aff[4] * smp_in + aff[5];

   if (no_points > 2)
       free (FREE_PTR x0);

   return(1);
   }



/* -------------------------------------------------------------------
   Funktion     : affin1_koff
   Enthalten in : /home/franz
   Erstellt     : August 1994                            Version : 1.0
   Autor        : Franz Wewel
   -------------------------------------------------------------------
   Funktion zur Berechnung der Affinkoeffizienten aff[0], ..., aff[5]
   xn = aff[0] * xa + aff[1] * ya + aff[2]
   yn = aff[3] * xa + aff[4] * ya + aff[5]

   Die Berechnung erfolgt iterativ durch eine robuste Ausgleichung
   (Maximum Likelihood).
   -------------------------------------------------------------------
   Parameter :
   Ein/Aus:
    ->  xa    double *      Vektor der x-Koordinaten im System a
    ->  ya    double *      Vektor der y-Koordinaten im System a
    ->  xn    double *      Vektor der x-Koordinaten im System n
    ->  yn    double *      Vektor der y-Koordinaten im System n
    ->  p     double *      Apriori Gewichte der "Beobachtungen"
                            xn/yn (und nach der robusten Schaetzung)
    ->  v_x   double *      Restklaffung in X-Richtung 
    ->  v_y   double *      Restklaffung in Y-Richtung 
    ->  aff   double *      Vektor der Affinkoeffizienten
        itrmax int          maximale Anzahl an Iterationen 
        eps    double       Abbruch der Iteration wenn Aenderungen 
                            der Aff.-Koeff. kleiner als eps.
        n     long          Anzahl der Beobachtungen
   ------------------------------------------------------------------
   Return:        1 falls alles OK
               != 1 falls Fehler auftritt status dann siehe aveb1.h
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------ */
static int affin1_koff (double       *xa, 
			double       *ya,
			double       *xn,
			double       *yn,
			double       *p,
			double       *v_x,
			double       *v_y,
			double       *aff, 
			int           itrmax,
			double        eps,
			int           n)
   {
   int       i, iter, status, step;
   int       u;
   double    det;
   double    *ata, *atl, *qxx;
   double    vx, vy, vv;

   double    a0, a1, a2, a3, a4, a5;
   double    b0, b1, b2, b3, b4, b5;
   double    xi, yi, xii, yii, wi, xw, yw;

   u = 3;
/* ---------------------- Speicherplatz fuer tmp - felder */
   ata = d_alloc ((long)(u * u), &status);
   if (status < 1) return(status);
   atl = d_alloc ((long)u * 2, &status);
   if (status < 1) return(status);
   qxx = d_alloc ((long)(u * u), &status);
   if (status < 1) return(status);

   for (step = 0; step < itrmax ; step++) {

    /* ------------------------------------------------------------------
       Aufbau des Normalgleichungssystems
       ------------------------------------------------------------------ */
       a0 = a1 = a2 = a3 = a4 = a5 = 0.0;
       b0 = b1 = b2 = b3 = b4 = b5 = 0.0;

       for (i = 0; i < n; i++) {
           xi  = xa[i];
           yi  = ya[i];
           xii = xn[i];
           yii = yn[i];
           wi  =  p[i];

           xw  = xi * wi;
           yw  = yi * wi;

           a0 += xi * xw;
           a1 += xi * yw;
           a2 += xw;
           a3 += yi * yw;
           a4 += yw;
           a5 += wi;

           b0 += xw * xii;
           b1 += yw * xii;
           b2 += wi * xii;
           b3 += xw * yii;
           b4 += yw * yii;
           b5 += wi * yii;
           }

       ata[0] = a0;   ata[1] = a1;   ata[2] = a2;
       ata[3] = a1;   ata[4] = a3;   ata[5] = a4;
       ata[6] = a2;   ata[7] = a4;   ata[8] = a5;

       atl[0] = b0;
       atl[1] = b1;
       atl[2] = b2;
       atl[3] = b3;
       atl[4] = b4;
       atl[5] = b5;

       if (a0 < EPS_SCHWARZ || a3 < EPS_SCHWARZ || a5 < EPS_SCHWARZ) {
        /* ############### */
           free(FREE_PTR ata);
           free(FREE_PTR atl);
           free(FREE_PTR qxx);
        /* ############### */
           return (MA_SCHWARZ);
           }
    /* ------------------------------------------------------------------
       Berechnung der Normalgleichungsinversen qxx
       ------------------------------------------------------------------ */
       det = inv_mtrx (ata, qxx, u);
       if (fabs(det) < EPS_DET) {
        /* ############### */
           free(FREE_PTR ata);
           free(FREE_PTR atl);
           free(FREE_PTR qxx);
        /* ############### */
           return (DET_0);
           }
    /* ------------------------------------------------------------------
       Berechnung der Verbesserungen und der Unbekannten
       ------------------------------------------------------------------ */
       mtrxmul (qxx, atl,   aff,    u, u, 1);
       mtrxmul (qxx, atl+u, aff+u,  u, u, 1);

       a0 = aff[0];
       a1 = aff[1];
       a2 = aff[2];
       a3 = aff[3];
       a4 = aff[4];
       a5 = aff[5];
    /* ---------------------------------------------------------------
       Berechnung neuer Gewichte fuer die Iteration
       --------------------------------------------------------------- */
       iter = 0;
       if (step < itrmax - 1)  {
       for (i = 0; i < n; i++) {
           xi = xa[i];
           yi = ya[i];
           vx = a0 * xi + a1 * yi + a2 - xn[i];
           vy = a3 * xi + a4 * yi + a5 - yn[i];
           vv = sqrt(vx * vx + vy * vy);

/* */
           wi = p[i];
/* */
           if (step < 3) {
               p[i] = 1.0 / sqrt(1.0 + vv);
               iter = 1;
              }
           else {
              p[i] = exp(-vv / 2.0);
              if (fabs(wi  - p[i]) > eps) iter = 1;
              }
           }
           }
       if (!iter) break;
       }                  /* ------------- Iteration */



/* -------------------------------- Berechnung der Restklaffungen */
   for (i = 0; i < n; i++) {
        xi = xa[i];
        yi = ya[i];
        v_x[i] = a0 * xi + a1 * yi + a2 - xn[i];
        v_y[i] = a3 * xi + a4 * yi + a5 - yn[i];

        }


/* ############### */
   free(FREE_PTR ata);
   free(FREE_PTR atl);
   free(FREE_PTR qxx);
/* ############### */

   return (OK);
   }




/* ###################################################################
   Funktion     : inv_33_aff
   Enthalten in : /home/franz/mars94
   Erstellt     : Sept  1994                            Version : 1.0
   Autor        : Franz Wewel
   -------------------------------------------------------------------
   Funktion berechnet den inversen Wert der Affinkoeffizienten von a[0-5]
   -------------------------------------------------------------------
   Include :
   -------------------------------------------------------------------
   Parameter :
   Ein:
    --> a      double *
   Aus:
    --> a_1    double *
   ------------------------------------------------------------------
   Return : 1 wen ok; -1 wenn Determinate == 0
   ------------------------------------------------------------------ 
   Funktionen : 
   ################################################################### */
#if 0		/* not used */
static int inv_33_aff (double   *a,
		       double   *a_1)
   {
   double   a0, a1, a2, a3, a4, a5;
   double   det;

   a0 = *a;
   a1 = *(a+1);
   a2 = *(a+2);
   a3 = *(a+3);
   a4 = *(a+4);
   a5 = *(a+5);

   det = a0 * a4 - a1 * a3;
   if (fabs(det) > EPS)
      {
      *a_1       =  a4 / det;
      *(a_1 + 1) = -a1 / det;
      *(a_1 + 2) =  (a1 * a5 - a2 * a4) / det;
      *(a_1 + 3) = -a3 / det;
      *(a_1 + 4) =  a0 / det;
      *(a_1 + 5) =  (a2 * a3 - a0 * a5) / det;
      }
   else
      {
      return (-1);
      }
   return (1);
   }
#endif


/* ###################################################################
   Funktion     : d_alloc
   Enthalten in : /home/franz/mars94
   Erstellt     : Sept  1994                            Version : 1.0
   Autor        : Franz Wewel
   -------------------------------------------------------------------
   Funktion allokiert Speicherplatz fuer den Datentyp double
   -------------------------------------------------------------------
   Include :
   -------------------------------------------------------------------
   Parameter :
   Ein:
        buffer_size  long *     Anzahl der Vektorelemente
    --> status       int  *     Funktionstatus wenn erfolgreich
                                *status = 1 sonst *status = MCLERR
   ------------------------------------------------------------------
   Return :  liefert Zeiger auf (double *)Vektor
   ------------------------------------------------------------------ 
   Funktionen : malloc();
   ################################################################### */
static double *d_alloc (long      buffer_size,
			int      *ptr_status)
   {
/* ------------------------------------------------------------------
   Variablen
   ------------------------------------------------------------------ */
   double  *ptr_buffer;
/* ------------------------------------------------------------------
   Speicherplatz bereitstellen
   ------------------------------------------------------------------ */
   ptr_buffer = (double *)malloc((size_t)(buffer_size*sizeof(double)));

   if (!(double *)ptr_buffer) {
      *ptr_status = MCLERR;
      }
   else {
      *ptr_status = 1;
      }

   return (ptr_buffer);
   }



/* ###################################################################
   Funktion     : mtrxmul
   Enthalten in : 
   Erstellt     : August 1990                            Version : 1.0
   Autor        : 
   -------------------------------------------------------------------
   Funktion zur Multiplikation zweier Matrizen

   Die Funktion fuehrt die Multiplikation zwischen den Matrizen a und b
   durch. Die Ergebnismatrix ist c. Der Anwender hat zu gewaehrleisten,
   das genuegend Speicherplatz an der Stelle, auf die der Zeiger c 
   weisst, vorhanden ist. Die Multiplikation kann fuer beliebig grosse
   Matrizen durchgefuehrt werden.
   -------------------------------------------------------------------
   Include :      
   -------------------------------------------------------------------
   Parameter :
   Ein:
   --> a      double     Zeiger auf den Vektor der Matrix a [m][n]
   --> b      double     Zeiger auf den Vektor der Matrix b [n][l]
       m      int        Anzahl der Zeilen der Matrix a
                            "    "     "    "     "   c
       n      int        Anzahl der Spalten der Matrix a
                            "    "  Zeilen   "    "    b
       l      int        Anzahl der Spalten der Matrix b
                            "     "   "      "    "    c

   Aus:
   --> c      double     Zeiger auf den Vektor der Ergebnismatrix
                         c [m][l]
   ------------------------------------------------------------------
   Return :  %
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken :
   ################################################################### */
static void mtrxmul (double     *a, 
		     double     *b, 
		     double     *c,
		     int         m, 
		     int         n, 
		     int         l)
   {
/* -----------------------------------------------------------------
   Variablen
   ----------------------------------------------------------------- */
   register int        i, j, k;
   double              *a0, *b0, *c0, *a1, *b1;

   for (k = 0; k < l; k++)             /* Schleife ueber die Spalten */
       {
       b0 = b + k;
       for (j = 0; j < m; j++)         /* Schleife ueber die Zeilen  */
           {
           a0 = a + j * n;
           c0 = c + j * l + k;
           *c0 = 0.0;
           for (i = 0; i < n; i++)
               {
               a1 = a0 + i;
               b1 = b0 + i * l;

               *c0 += *a1 * *b1;
               }
           }
       }
    }




/* ###################################################################
   Funktion     : inv_mtrx
   Enthalten in : /home/franz/lib/a_lib.a
   Erstellt     : Oktober 1996                          Version : 2.1
   Autor        : Franz Wewel
   ------------------------------------------------------------------
   Funktion zur Berechnung der Inversen einer symetrischen 
   Matrix nach dem Gauss-Jordan-Verfahren.

   mtrx_e * mtrx_i = E;
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name        Dim         Typ          Bedeutung

    --> mtrx_e      n, n      double *       symetr. Matrix die
                                             invertiert wird
        n                     int 
   Aus:
    --> mtrx_i                               Invertierte Matrix
   ------------------------------------------------------------------
   Return : 
                det             double          Determinante der inv-
                                                vertierten Matrix
   ------------------------------------------------------------------
   Variablen :
       x              double          Hilfsgroesse
       i, j, k        register int    Laufvariable
       p1             register int    Hilfsgroesse Position in Vektor
   ------------------------------------------------------------------
   Bibliotheken :  
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
   ################################################################### */
static double inv_mtrx (double   *mtrx_e,
			double   *mtrx_i,
			int       n)
   {
   double            x;
   double           *a0, *a1, *a2, *a3, *a4, *diag;
   register int      i, j, k, p1, p2;
/* double --------- det */
/* ------------------------------------------------------------------
   Umkopieren der Eingangsmatrix in die Ergebnismatrix
   ------------------------------------------------------------------ */
   j = n*n;
   a0 = mtrx_i;
   a1 = mtrx_e;
   for (i = 0; i < j; i++)
       {
       *a0++ = *a1++;
       }
/* ------------------------------------------------------------------
   Inversion
   det = 1.0;
   ------------------------------------------------------------------ */
   for (i = 0; i < n; i++)
       {
       diag = mtrx_i + i * n + i;
       x = *diag;

       if (fabs(x) < EPS_X) return (0.0);
/*
       det *= x;
*/
       *diag = 1.0; 
        
       a0 = mtrx_i + i;

       a1 = mtrx_i + i * n;

       for (j = i; j < n; j++)
           {
           a2   = a0 + j * n;
           *a2 /= x;
           }

       for (k = 0; k < n; k++)
           {
           if (k-i < 0 ) 
              {
              a2 = a1 + k;
              x = *a2;
              }
           else if (k-i > 0)
              {
              a2 = a0 + k * n;
              if (fabs(*diag) < EPS_X) return (0.0);
              x = *a2 / *diag;
              }
           else 
              {
              continue;
              }
    
           a3 = mtrx_i + k;
           for (j = k; j < n; j++)
               {
               p1 = j * n;
               a2 = a3 + p1;
               if (i < j)
                  {
                  a4   = a0 + p1;
                  *a2 -= x * *a4;
                  }
               else if (i == j)
                  {
                  *a2 = -x *  *diag;
                  }
               else if  (i > j)
                  {
                  a4   = a1 + j;
                  *a2 += x * *a4 * *diag;
                  }
               }
           }
        }
/* -----------------------------------------------------------------------
   Umspeichern der symetrischen Elemente 
   ----------------------------------------------------------------------- */
   for (i = 0; i < n; i++)
       {
       for (j = i+1; j < n; j++) 
           {
           p1 = i * n + j;
           a0 = mtrx_i + p1;
           p2 = j * n + i;
           a1 = mtrx_i + p2;
           *a0 = *a1;
           }
       }
   return (1.0); /* ######## return (det); ######## */
   }




/* -------------------- Transformation ueber zwei identische Punkte */
static int affkof_2(double    *xa,
		    double    *ya,
		    double    *xn,
		    double    *yn,
		    double    *ak)
   {
   double     xaa, yaa, xae, yae;
   double     xna, yna, xne, yne;
   double     dxa, dya, dxn, dyn;
   double     o, a, nenner;
   double     x0, y0;
/* ----------------------------- Koordinaten des Alt-Systems */
   xaa =  xa[0]; 
   yaa =  ya[0]; 
   xae =  xa[1]; 
   yae =  ya[1]; 
/* ----------------------------- Koordinaten des Neu-Systems */
   xna = (double) xn[0]; 
   yna = (double) yn[0]; 
   xne = (double) xn[1]; 
   yne = (double) yn[1]; 

   ak[0] = 1.0;
   ak[1] = 0.0;
   ak[2] = 0.0;
   ak[3] = 0.0;
   ak[4] = 1.0;
   ak[5] = 0.0;

   dxa = xae - xaa;
   dya = yae - yaa;
   dxn = xne - xna;
   dyn = yne - yna;
   nenner = dya * dya + dxa * dxa;

   if (fabs(nenner) > EPS)
      {
      o  = (dxa * dyn - dya * dxn) / nenner;
      a  = (dya * dyn + dxa * dxn) / nenner;
      y0 = yna - o * xaa - a * yaa;
      x0 = xna + o * yaa - a * xaa;
      }
   else if (fabs(dxa) > EPS)
      {
      o = dxn / dxa;
      a = o;
      y0 = yna - o * yaa;
      x0 = xna - a * xaa;
      }
   else if (fabs(dya) > EPS)
      {
      o = dyn / dya;
      a = o;
      y0 = yna - o * yaa;
      x0 = xna - a * xaa;
      }
   else
      {
      return(-1);
      }

   ak[0] =  a;
   ak[1] = -o;
   ak[2] = x0;
   ak[3] =  o;
   ak[4] =  a;
   ak[5] = y0;

   return(1);
   }

