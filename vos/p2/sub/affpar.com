$!****************************************************************************
$!
$! Build proc for MIPL module affpar
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:21
$!
$! Execute by entering:		$ @affpar
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
$ write sys$output "*** module affpar ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to affpar.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   if F$SEARCH("affpar.imake") .nes. ""
$   then
$      vimake affpar
$      purge affpar.bld
$   else
$      if F$SEARCH("affpar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake affpar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @affpar.bld "STD"
$   else
$      @affpar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create affpar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack affpar.com -mixed -
	-s affpar.c -
	-i affpar.imake -
	-t taffpar.pdf taffpar.imake taffpar.c tstaffpar.tpf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create affpar.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create affpar.imake
#define SUBROUTINE affpar
#define MODULE_LIST affpar.c

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create taffpar.pdf
Process help=*

PARM   LNE_IN  TYPE=REAL    COUNT=1  VALID=(-1.0:100000.0)  DEFAULT=750.0

PARM   SMP_IN  TYPE=REAL    COUNT=1  VALID=(-1.0:100000.0)  DEFAULT=750.0


END-PROC
.Title
 Test program for the affpar function               Version: 1.0.7


.HELP

   test program tstaffpar call the function affpar().
 
            function determines coefficients for an affin transformation,
            and calculates the transformation for a given point.
                 
            call sequence:
                 
                 st = affpar (lne_in, smp_in,
                                n,  
                                lne_0, smp_0, 
                                lne_1, lne_1, 
                               &lne_out, &smp_out,
                                a);
                    
                 with:   
                   IN: 
                     double     lne_in   = line   coordinate in system 0; which is
                                           to transfer in the system 1
                     double     smp_in   = sample coordinate in system 0; which is
                                           to transfer in the system 1
                     int        n        = number of the points,
                     double *   lne_0[i] = line   coordinate vector for system 0,
                     double *   smp_0[i] = sample coordinate vector for system 0,
                     double *   lne_1[i] = line   coordinate vector for system 1,
                     double *   smp_1[i] = sample coordinate vector for system 1,
                   OUT:
                     double *   lne_out  = line   coordinate in system 1 of the lne_in
                                           coordinate (system 0)
                     double     smp_out  = sample coordinate in system 1 of the lne_in
                                           coordinate (system 0)
                     double *   a[i]     = coefficient vector (6 unknown 
                                           parameters).
                 return value: 
                        st = 1 if everything is allright;
                   else st < 1
                   
             the mathematic modell:
             
                 lne_1[i] = lne_0[i] * a[0] + smp_0[i] * a[1] + a[2] , 
                 smp_1[i] = lne_0[i] * a[3] + smp_0[i] * a[4] + a[5] . 

   WRITTEN BY: F.Wewel, DLR     17-Jun-1997

.LEVEL1
.VARI LNE_IN
 line coordinate to transform
.VARI  SMP_IN
 sample coordinate to transform


.End
$!-----------------------------------------------------------------------------
$ create taffpar.imake
#define PROGRAM taffpar
#define MODULE_LIST taffpar.c 

#define TEST

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create taffpar.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "vicmain_c"

#define  TEST_FILE        "tstaffpar.tpf"

extern int affinpar (double, double, int,   double *,  double  *,  double  *,  
                     double *, double   *, double   *, double   *);



/* #######################################################################
 
   test program tstaffpar call the function affinpar().
 
            function determines coefficients for an affin transformation.
                 
            call sequence:
                 
                 st = affinpar (lne_in, smp_in,
                                n,  
                                lne_0, smp_0, 
                                lne_1, lne_1, 
                               &lne_out, &smp_out,
                                a);
                    
                 with:   
                   IN: 
                     double     lne_in   = line   coordinate in system 0; which is
                                           to transfer in the system 1
                     double     smp_in   = sample coordinate in system 0; which is
                                           to transfer in the system 1
                     int        n        = number of the points,
                     double *   lne_0[i] = line   coordinate vector for system 0,
                     double *   smp_0[i] = sample coordinate vector for system 0,
                     double *   lne_1[i] = line   coordinate vector for system 1,
                     double *   smp_1[i] = sample coordinate vector for system 1,
                   OUT:
                     double *   lne_out  = line   coordinate in system 1 of the lne_in
                                           coordinate (system 0)
                     double     smp_out  = sample coordinate in system 1 of the lne_in
                                           coordinate (system 0)
                     double *   a[i]     = coefficient vector (6 unknown 
                                           parameters).
                 return value: 
                        st = 1 if everything is allright;
                   else st < 1
                   
             the mathematic modell:
             
                 lne_1[i] = lne_0[i] * a[0] + smp_0[i] * a[1] + a[2] , 
                 smp_1[i] = lne_0[i] * a[3] + smp_0[i] * a[4] + a[5] . 
 
 
   ####################################################################### */
void main44 ()
   {
   FILE    *fp;
   char     fnam_0[100], fnam_1[100];
   char     string[200], *res;
   int      no_points;
   int      ok;
   int      i;
   int      pid;
   int      status;
   double  *lne_0, *smp_0, *lne_1, *smp_1;
   double   affcoef[6];   
   double   lne_in,  smp_in;
   double   lne_out, smp_out;
   double   lne_11, smp_11;
   float    f_val;
   int      count;
   
   zveaction("usa", "");
/* ------------------------------------
                                   correlation parameter */
   status = zvp("LNE_IN", &f_val, &count);
   if (status <= 0) zabend();
   lne_in = (double) f_val;


   status = zvp("SMP_IN", &f_val, &count);
   if (status <= 0) zabend();
   smp_in = (double) f_val;



/* -------------------------------------------------------------
                                opening the file of conjugate points  */
   fp = fopen (TEST_FILE, "r");
   if (fp == (FILE*)NULL) {
   
       sprintf (string, " ### %s: %s ###", TEST_FILE, strerror(errno));
       
       zvmessage (string, "");
       zabend();
       } 
   
/* -------------------------------------------------------------
                                reading the file of conjugate points  */
   fscanf (fp, "%s", fnam_0);
   fscanf (fp, "%s", fnam_1);
   
   fscanf (fp, "%d", &no_points);
   
   
   smp_0 = (double *) malloc(sizeof(double) * no_points * 4);
   if (smp_0 == (double *)NULL) exit(0);
   
   lne_0 = smp_0 + no_points;
   smp_1 = lne_0 + no_points;
   lne_1 = smp_1 + no_points;
   
   
   ok = 1;
   while (ok) {
       
       res = fgets (string, 200, fp);
       
       if (res == (char *)NULL) exit(0);
       
       
       if (!strncmp(string, "****", 4)) ok = 0;
       
       }
   
   
/* -------------------------------------------------------------
               reading sample and line values of the conjugate points  */
   for (i = 0; i < no_points; i++) {
          
        res = fgets (string, 200, fp);
        if (res == (char *)NULL) exit(0);
       
        
        status = sscanf (string, "%d %lf  %lf %lf %lf", &pid,
                                                         smp_0 + i, lne_0 + i,
                                                         smp_1 + i, lne_1 + i);
           
        } 


/* -------------------------------------------------------------
                                        compute the affin coefficients  */

   status = affinpar (lne_in, smp_in, 
                      no_points,  lne_0,  smp_0,  lne_1,  smp_1, 
                      &lne_out, &smp_out, affcoef);


   printf ("\n\n");

     
/* -------------------------------------------------------------
                                          print the affin coefficients  */
   printf ("\n affin coefficient to transform from system 0 (in) to system 1(out)\n");
   for (i = 0; i < 6; i++) {
            
        printf ("affcoef[%d] %12.5f\n", i, affcoef[i]);
           
        } 
   printf ("\n\n");

   
   
/* -------------------------------------------------------------
                                           print the conjugate points  */
                                           
   printf ("\n\n");
   printf ("    i    smp_0    lne_0    smp_1   lne_1     smp_11  lne_11,  diff_smp, diff_lne\n");
   printf ("    (with:  smp_11, lne_11     = calculate with the affin coefficients)\n");
   printf ("    (with:  diff_smp, diff_lne = difference input data and calculated data)\n\n");
   
   for (i = 0; i < no_points; i++) {
        
        lne_11 = affcoef[0] * lne_0[i] + affcoef[1] * smp_0[i] + affcoef[2];
        smp_11 = affcoef[3] * lne_0[i] + affcoef[4] * smp_0[i] + affcoef[5];

        printf ("%5d %8.2lf %8.2lf %8.2lf %8.2lf  %8.2lf %8.2lf %8.2lf %8.2lf\n", 
                i, smp_0[i], lne_0[i], smp_1[i], lne_1[i],
                   smp_11,  lne_11,  smp_1[i]-smp_11, lne_1[i]-lne_11);
           
        } 
   printf ("\n\n");
   printf (" input  coordinate system 0: lne: %7.2lf smp: %7.2lf\n", lne_in, smp_in);
   printf (" output coordinate system 1: lne: %7.2lf smp: %7.2lf\n", lne_out, smp_out);
   printf ("\n\n");



   free ((void *) smp_0);
   fclose (fp);
   }

$!-----------------------------------------------------------------------------
$ create tstaffpar.tpf
f1409702.nd2_1
f1409702.s12_1
      100
          system - 0             system - 1
PID    smp - 0   lne - 0     smp - 1   lne - 1
****
   1     33.00     52.00     600.00    252.00
   2    453.00     34.00    1019.00    247.00
   3    765.00     28.00    1335.00    248.00
   4   1123.00     34.00    1692.00    258.00
   5   1435.00     15.00    2007.00    249.00
   6   1798.00     32.00    2367.00    267.00
   7   2235.00     43.00    2802.00    279.00
   8   2858.00     45.00    3424.00    282.00
   9   3398.00    172.00    3957.00    394.00
  10   4085.00    153.00    4641.00    384.00
  11   4509.00     66.00    5075.00    322.00 
  12     42.00    465.00     596.00    635.00 
  13    499.00    486.00    1058.00    653.00 
  14    971.00    529.00    1532.00    701.00 
  15   1394.00    414.00    1949.00    603.00 
  16   1749.00    472.00    2310.00    670.00 
  17   2308.00    529.00    2872.00    727.00 
  18   2683.00    543.00    3247.00    743.00 
  19   3109.00    562.00    3674.00    762.00 
  20   3456.00    498.00    4022.00    710.00 
  21   3984.00    513.00    4550.00    732.00 
  22   4220.00    537.00    4785.00    760.00 
  23   4549.00    587.00    5115.00    814.00 
  24    153.00    910.00     707.00    999.00 
  25    514.00    943.00    1066.00   1030.00 
  26    888.00   1002.00    1449.00   1096.00 
  27   1114.00   1071.00    1686.00   1165.00 
  28   1592.00   1092.00    2165.00   1202.00 
  29   2059.00   1045.00    2631.00   1173.00
  30   2603.00   1103.00    3176.00   1212.00 
  31   2986.00   1000.00    3549.00   1097.00
  32   3492.00    987.00    4056.00   1099.00
  33   3895.00   1090.00    4470.00   1215.00
  34   4198.00   1076.00    4776.00   1221.00
  35   4566.00    996.00    5141.00   1167.00
  36    151.00   1469.00     713.00   1460.00
  37    579.00   1509.00    1141.00   1498.00
  38    895.00   1401.00    1465.00   1428.00
  39   1414.00   1665.00    1999.00   1672.00
  40   1669.00   1544.00    2239.00   1586.00
  41   2149.00   1550.00    2718.00   1588.00
  42   2537.00   1584.00    3110.00   1621.00
  43   2993.00   1515.00    3558.00   1558.00
  44   3518.00   1521.00    4091.00   1605.00
  45   3871.00   1555.00    4448.00   1623.00
  46   4330.00   1454.00    4902.00   1590.00
  47   4572.00   1472.00    5148.00   1619.00
  48    272.00   1844.00     851.00   1830.00
  49    552.00   1801.00    1124.00   1776.00
  50    946.00   1951.00    1538.00   1907.00
  51   1580.00   1820.00    2152.00   1798.00
  52   2004.00   1914.00    2588.00   1874.00
  53   2222.00   1826.00    2796.00   1791.00 
  54   2593.00   1958.00    3190.00   1939.00 
  55   3053.00   1888.00    3640.00   1904.00 
 5600  3539.00   1900.00    4135.00   1956.00 
  57   3852.00   1977.00    4459.00   2072.00
  58   4122.00   1945.00    4727.00   2071.00
  59   4449.00   1957.00    5062.00   2117.00
  60    286.00   2213.00     924.00   2191.00
 6100  1033.00   2277.00    1675.00   2281.00
  62   1365.00   2503.00    2053.00   2500.00
  63   1479.00   2202.00    2118.00   2272.00
  64   1657.00   2305.00    2315.00   2385.00
  65   1586.00   2576.00    2284.00   2559.00
  66   2176.00   2415.00    2848.00   2385.00
  67   2428.00   2308.00    3067.00   2254.00
  68   2126.00   2009.00    2733.00   1973.00
  69   2898.00   2384.00    3563.00   2360.00
  70   3290.00   2364.00    3953.00   2361.00 
  71   3735.00   2484.00    4432.00   2562.0
  72   4107.00   2666.00    4852.00   2800.00
  73   4427.00   2609.00    5157.00   2771.00
  74    398.00   2851.00    1170.00   2920.00
  75    858.00   2835.00    1624.00   2899.00
  76   1538.00   2825.00    2295.00   2847.00
  77   1976.00   3138.00    2813.00   3168.00
  78   2580.00   3071.00    3394.00   3080.00
  80   3065.00   2854.00    3825.00   2862.00
  81    481.00   3505.00    1405.00   3653.00
  82    948.00   3542.00    1873.00   3685.00
  83   1405.00   3426.00    2311.00   3540.00
  84   1909.00   3548.00    2832.00   3653.00
  85   2489.00   3565.00    3414.00   3664.00
  86   2667.00   3589.00    3593.00   3696.00
  87   3236.00   3587.00    4165.00   3709.00
  88   3754.00   3644.00    4692.00   3802.00
  89   4125.00   3848.00    5072.00   4027.00
  90    564.00   3973.00    1514.00   4167.00
  91   1038.00   3940.00    1988.00   4115.00
  92   1552.00   3916.00    2499.00   4073.00
  93   2075.00   3979.00    3025.00   4133.00
  94   2485.00   3980.00    3437.00   4130.00
  95   2935.00   3977.00    3887.00   4136.00
  96   3406.00   3991.00    4355.00   4169.00
  97   3716.00   3979.00    4664.00   4162.00
  98   4226.00   3941.00    5173.00   4126.00
  99   3329.00   1223.00    3899.00   1292.00
 100   3245.00   1661.00    3832.00   1695.00
 101   3729.00   1429.00    4296.00   1504.00
$ Return
$!#############################################################################
