/* --------------------------------------------------------------------
   kqkkor

   PURPOSE

   Subroutine kqkkor carries out calculus associated with tiepoint
   "correlation" in the match mode.  This routine accepts a number of
   parameters needed for the matching, such as matching patch sizes, the
   search area, and the requested point accuracy and returns information on
   the results of matching, such as correlation coefficients before and after
   LSM fitting, point accuracy, as well as pixel position of the matched point.

   AUTHOR

   Franz Wewel, DLR NE-PE, Berlin, Germany

   REVISION HISTORY

   Version:	1.001	wewel	Jan., 1997
		1.010a	wewel	Oct., 1997
				
   11/21/97 vxp		Declared all local functions as static and moved 
			their definition out of the include file.

   ------------------------------------------------------------------ */
#include "kqkkor.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>

/*** Routine definitions        ***/

static int  zoom_dim (int, int, double *, int *, int *);
static int  kqkkor_ip (INT_MATRIX *, INT_MATRIX *, int, int, KORPAR *, ERG_0 *);
static int zoom_kof_ref (double *, int, int, double *, double *);
static int zoom_kof_vgl (double *, int, int, int *, int *,
                         double *, double *);
static int zoom_in_int (INT_MATRIX *, INT_MATRIX *, double *, int);
static int bilinear_ip_int (int *, double, double, int, int);
static int near_nb_int (int *, double, double, int, int);
static int inv_33_aff (double *, double *);
static double dmin4 (double, double, double, double);
static double dmax4 (double, double, double, double);
static int masz_check (double *);
static double aff_trf (double, double, double *);
static double pmkkor (INT_MATRIX *, INT_MATRIX *, int,
                      int, int, int *, int *);
static double pmkkof_1 (INT_MATRIX *, INT_MATRIX *, int, int, int, int);
static int mtrx_2_mtrx (INT_MATRIX *, INT_MATRIX *, int, int);
static double *d_alloc(long, int *);
static int aveb1 (double *, double *, double *, double *, double *,
                  double *, long, long);
static void mtrxmul (double *, double *, double *, int, int, int);
static int ausgl_mtrx (double **, double **, double **, double **,
                       double **, long, long);
static void free_mtrx (double *, double *,  double *, double *, double *);
static double inv_mtrx1 (double *, double *, long);
static int ata_atl (double *, double *, long, long, double *, double *);
static void unb_verb (double *, double *, double *, long, double *, double *);
static void start_value (double *, double *, double *, long, long, long, long,
                         double *, double *, double *, double *, double *,
                         double *, double *, double *);
static void beovek (double *, INT_MATRIX *, INT_MATRIX *);
static void a_mtrx (INT_MATRIX *, INT_MATRIX *,
                    double *, double *, long, long);
static void mp_fehler (double, double, double *, double, long, long, double *,
                       double *);
static int pruf_1 (double, double, double, double,
                   double, double, double, double,
                   double, double, double, double);
static int rsmp_1 (double *, INT_MATRIX *, INT_MATRIX *,
                   double, double, double *,
                   double *, double, double, double, double);
static void ini_erg (ERG_0   *);
static void check_par (KORPAR, KORPAR *);
static int  erg_check (int, KORPAR *, int, int, ERG_0 *);
static int  cp_check_mtrx (int, INT_MATRIX *, INT_MATRIX *,
                           INT_MATRIX *, INT_MATRIX *, KORPAR *, double *);
static int check_mtrx (INT_MATRIX *, INT_MATRIX *);


/* ###################################################################
   Function: dim_muster_such
   Date:     Sep  1996                                 Version: 1.00
   By:       Franz Wewel
   -------------------------------------------------------------------
   Funktion bestimmt die mindest Grossen fur die Bildmatrizen fur
   die Funktion kqkkor().
   ------------------------------------------------------------------- 
   Include:  "kqkkor.h"
   -------------------------------------------------------------------
   Parameter :

     Name      Type          In/Out   Description

   --> par     KORPAR *       IN      Steuerparameter fuer die Produkt
                                      Momenten und die Kleinste Quadrate 
                                      Korrelation der Funktion kqkkor().
          int    suchfns              Suchfenster in Zeilen- und
                                      Spaltenrichtung
          int    pmkdim               Fenstergroesse fuer die Produkt
                                      Momemnten Korrelation
          int    kqkdim               Fenstergroesse fuer die Kleinste
                                      Quadrate Korrelation
          double affin[6]             Affinkoeffizienten fuer die 
                                      Naehrungsweisebestimmung der
                                      Affinverzerrung zwischen Referenz
                                      (R) und Vergleichsmatrix (V) mit:
                                      row (V) = affin[0] * row (R) 
                                              + affin[1] * col (R)
                                      col (V) = affin[3] * row (R) 
                                              + affin[4] * col (R)
                                      die Translation Koeffizienten 
                                      affin[2], affin[5] werden nicht
                                      verwendet
          float  lsmacu               maximal erlaubte Punktfehler aus
                                      der LSM. 
                                      !!! Parameter wird in
                                          dim_muster_such() nicht ver=
                                          wendet.   !!!
          float  minpmk               mindest Wert fuer die PMK-Korrel-
                                      ation, um LSM zu starten.
                                      !!! Parameter wird in
                                          dim_muster_such() nicht ver=
                                          wendet.   !!!


   --> muster  INT_MATRIX *   OUT     Referenzmatrix
          int   dimz                  Anzahl der Zeilen der Referenz-
                                      matrix. Wert ist immer ungerade. 
          int   dims                  Anzahl der Spalten der Referenz-
                                      matrix. Wert ist immer ungerade.
          int  *ptr_m                 Zeiger auf die Bilddaten der 
                                      Referenzmatrix.
                                      !!! Parameter wird in
                                          dim_muster_such() nicht ver=
                                          wendet.   !!!



   --> such    INT_MATRIX *   OUT     Vergleichsmatrix
          int   dimz                  Anzahl der Zeilen der Vergleichs-
                                      matrix. Wert ist immer ungerade. 
          int   dims                  Anzahl der Spalten der Vergleichs-
                                      matrix. Wert ist immer ungerade. 
          int  *ptr_m                 Zeiger auf die Bilddaten der 
                                      Vergleichsmatrix.
                                         !!! Parameter wird in
                                          dim_muster_such() nicht ver=
                                          wendet.   !!!
   ------------------------------------------------------------------
   Return :     %
   ################################################################### */
void dim_muster_such (KORPAR        *par,
                      INT_MATRIX    *muster, 
                      INT_MATRIX    *such)
   {
   int           mode, status;
   KORPAR        par_1;
   double        aa[6];
   
   
/* ------------------------------------------- 
                                        Eingabe Parameter aktualisieren */
    check_par (*par, &par_1);

/* --------------------------
            Maszstabs-, Affin - Unterschied Referenz und Vergleichsbild */
    mode = masz_check (par_1.affin); 

/* -----------------------------------
                               Matrizendimensionen bei gleichem Maszstab */
    muster->dimz = par_1.kqkdim;
    muster->dims = par_1.kqkdim;

    such->dimz = (KQK_RAND  + par_1.kqkdim / 2 + 1 + par_1.suchfns / 2) * 2 + 1;
    such->dims = (KQK_RAND  + par_1.kqkdim / 2 + 1 + par_1.suchfns / 2) * 2 + 1;

    if (mode == 1) {         /* --------- Referenzmatrize hat gegenuber
                                          der Vergleichsmatrize eine hoehere
                                          Pixelaufloesung */
       status = inv_33_aff (par_1.affin, aa);

       zoom_dim (muster->dimz,  muster->dims, aa, 
                &muster->dimz, &muster->dims);

       }
    else if (mode == 2) {    /* --------- Vergleichsmatrize hat gegenuber
                                          der Referenzmatrize eine hoehere
                                          Pixelaufloesung */
       zoom_dim (such->dimz,  such->dims, par_1.affin, 
                &such->dimz, &such->dims);

       }

/* -------------------------------------------------------
                            Dimensionierung der Bildmatrizen = ungerade */
   muster->dimz = muster->dimz % 2 == 0 ? muster->dimz+1 : muster->dimz;
   muster->dims = muster->dims % 2 == 0 ? muster->dims+1 : muster->dims;

   such->dimz = such->dimz % 2 == 0 ? such->dimz+1 : such->dimz;
   such->dims = such->dims % 2 == 0 ? such->dims+1 : such->dims;

   }


/* ###################################################################
   Function: kqkkor
   Date:     Jun  1996                               Version: 1.02
   By:       Franz Wewel
   -------------------------------------------------------------------
   Die Funktion kqkkor() fuhrt Kreuz- und Kleinste Quadrate Korrelation
   zwischen zwei Bildmatrizen durch. Als Ergbnis der Korrelation erhalt
   man den Verschiebungsvektor zwischen den Bildmatrizen bezogen auf 
   deren Mittelpixel. Des weiteren erhalt man ein Masz der Uberein-
   stimmung zwischen den Matrizen.
   ------------------------------------------------------------------- 
   Include:  "kqkkor.h"
   -------------------------------------------------------------------
   Parameter :

     Name      Type          In/Out   Description

   --> par     KORPAR *       IN      Steuerparameter fuer die Produkt
                                      Momemnten und die Kleinste
                                      Quadrate Korrelation
          int    suchfns              Suchfenster in Zeilen- und
                                      Spaltenrichtung
          int    pmkdim               Fenstergroesse fuer die Produkt
                                      Momemnten Korrelation
          int    kqkdim               Fenstergroesse fuer die Kleinste
                                      Quadrate Korrelation
          double affin[6]             Affinkoeffizienten fuer die 
                                      Naehrungsweisebestimmung der
                                      Affinverzerrung zwischen Referenz
                                      (R) und Vergleichsmatrix (V) mit:
                                      row (V) = affin[0] * row (R) 
                                              + affin[1] * col (R)
                                      col (V) = affin[3] * row (R) 
                                              + affin[4] * col (R)
                                      die Translation Koeffizienten 
                                      affin[2], affin[5] werden nicht
                                      verwendet
          float  lsmacu               maximal erlaubte Punktfehler aus
                                      der LSM
          float  minpmk               mindest Wert fuer die PMK-Korrel-
                                      ation, um LSM zu starten


   --> muster  INT_MATRIX *   IN      Referenzmatrix
          int   dimz                  Anzahl der Zeilen der Referenz-
                                      matrix
                                      !!! Anzahl muss unbedingt ungerade 
                                          sein !!!
          int   dims                  Anzahl der Spalten der Referenz-
                                      matrix
                                      !!! Anzahl muss unbedingt ungerade 
                                          sein !!!
          int  *ptr_m                 Zeiger auf die Bilddaten der 
                                      Referenzmatrix


   --> such    INT_MATRIX *   IN      Vergleichsmatrix
          int   dimz                  Anzahl der Zeilen der Vergleichs-
                                      matrix
                                      !!! Anzahl muss unbedingt ungerade 
                                          sein !!!
          int   dims                  Anzahl der Spalten der Vergleichs-
                                      matrix
                                      !!! Anzahl muss unbedingt ungerade 
                                          sein !!!
          int  *ptr_m                 Zeiger auf die Bilddaten der 
                                      Vergleichsmatrix
   

   --> erg_out     ERG *    OUT       PMK-,LSM Ergebnis Werte
          float  dz                   Verschiebung des Mittelpunktes
                                      der Vergleichsmatrix gegenueber 
                                      dem Mittelpunkt der Referenz-
                                      matrix in Zeilenrichtung 
          float  ds                   Verschiebung des Mittelpunktes
                                      der Vergleichsmatrix gegenueber 
                                      dem Mittelpunkt der Referenz-
                                      matrix in Spaltenrichtung
          float  pmk                  Korrelationswert (PMK) vor der
                                      Kleinsten Quadrate Anpassung
          float  kqk                  Korrelationswert (PMK) nach der
                                      Kleinsten Quadrate Anpassung
          float  mp                   statistischer Punktfehler aus
                                      der Kleinsten Quadrate Korrelation
   ------------------------------------------------------------------
   Return :     %
   ################################################################### */
void kqkkor (KORPAR        *par,
             INT_MATRIX    *muster, 
             INT_MATRIX    *such, 
             ERG           *erg_out)
   {
   int           status = 1;
   int           mode;
   int           such_fns_2;
   double        a1[6];
   int           vz = 0, vs = 0;
   double        z0, s0, z1, s1;
   KORPAR        par_1;
   INT_MATRIX    muster_1, such_1;
   ERG_0         erg;

/* ------------------------------------------- 
                                        Eingabe Parameter aktualisieren */
   check_par (*par, &par_1);
/* ------------------------------------------------ 
                                               Eingabe Matrizen checken */
   status = check_mtrx (muster, such);

/* ------------------------------------------- 
                                   Initialisierung der Ergebnisstruktur */
   ini_erg (&erg);
   muster_1.ptr_m = (int *)NULL;
   such_1.ptr_m   = (int *)NULL;

/* --------------------------
            Maszstabs-, Affin - Unterschied Referenz und Vergleichsbild */
   mode = masz_check (par_1.affin); 

/* ------------------------------------------
                     Kopieren der Orginal Matrizen in die Lokale Matrize */
   if (status == OK) {
       status = cp_check_mtrx (mode, muster, &muster_1,
                                     such,   &such_1,
                                     &par_1, a1);
       }
/* ------------------------------------------
                                                  cross correlation */
   if (status == OK) {
      such_fns_2 = par_1.suchfns / 2 > 1 ? par_1.suchfns : 1;
      erg.pmk = pmkkor (&muster_1, &such_1, par_1.pmkdim, 
                         such_fns_2, such_fns_2, &vz, &vs);

      status = erg.pmk < par_1.minpmk ? MINPMK : OK_PMK;
      }

/* ------------------------------------------
                                              least square matching */
   if (status == OK_PMK) {
       status = kqkkor_ip (&muster_1, &such_1, vz, vs, &par_1, &erg);
       }
/* ------------------------------------------
                                                      outcome check */
   status = erg_check (status, &par_1, vz, vs, &erg);
/* ----------------------------------------------
                 back transformation patch such_1 --> patch suchma */
   if (status  == 1 && mode == 2) {
      z1 = (double) (such_1.dimz / 2 + 1) + erg.dz;
      s1 = (double) (such_1.dims / 2 + 1) + erg.ds;

      z0 = a1[0] * z1 + a1[1] * s1 + a1[2];
      s0 = a1[3] * z1 + a1[4] * s1 + a1[5];

      erg.dz = z0 - (double) (such->dimz / 2 + 1);
      erg.ds = s0 - (double) (such->dims / 2 + 1);
      }

    erg_out->dz  = (float)erg.dz;
    erg_out->ds  = (float)erg.ds;
    erg_out->pmk = (float)erg.pmk;
    erg_out->kqk = (float)erg.kqk;
    erg_out->mp  = (float)erg.mp;

 /* ---------------------------------------------- free workspace */
   if (muster_1.ptr_m && (mode == 1 || mode == 2)) {
       free ((void *) muster_1.ptr_m);
       muster_1.ptr_m = (int *)NULL;
       }
   if (such_1.ptr_m && mode == 2) {
       free ((void *) such_1.ptr_m);
       such_1.ptr_m = (int *)NULL;
       }
                 /*  ---- this is the end ----  */
   }











/* ###################################################################
   Funktion     : 
   Enthalten in : 
   Erstellt     : August 1994                           Version : 2.1
   Autor        : Franz Wewel
   geaendert    : August 96  
   -------------------------------------------------------------------
   Funktion berechnet 
   -------------------------------------------------------------------
   Include :      
   -------------------------------------------------------------------
   Parameter :
   -------------------------------------------------------------------
   Return :    
   -------------------------------------------------------------------
   Bibliotheken :    
   ################################################################### */
static int  zoom_kof_vgl (double    *k0,
			  int        dimz_in,
			  int        dims_in,
			  int       *dimz_out,
			  int       *dims_out,
			  double    *a0, 
			  double    *a1)
     {
     int             status, i;
     double          z0, s0 , z1, s1 , z2, s2, z3, s3;
     double          za, sa;
     double          minz, maxz, mins, maxs;
     int             dimz, dims;
 
      za = (double) dimz_in;
      sa = (double) dims_in;
 
      z0 = aff_trf(1.0, 1.0, k0);
      s0 = aff_trf(1.0, 1.0, k0+3);
 
      z1 = aff_trf(1.0, sa, k0);
      s1 = aff_trf(1.0, sa, k0+3);
 
      z2 = aff_trf(za, 1.0, k0);
      s2 = aff_trf(za, 1.0, k0+3);
 
      z3 = aff_trf(za, sa, k0);
      s3 = aff_trf(za, sa, k0+3);
 
      minz = dmin4 (z0, z1, z2, z3);
      maxz = dmax4 (z0, z1, z2, z3);
 
      mins = dmin4 (s0, s1, s2, s3);
      maxs = dmax4 (s0, s1, s2, s3);
 
   /* ----------------- Dimension des neuen Bildes */
      dimz = (int) (maxz - minz) + 2;
      dims = (int) (maxs - mins) + 2;

     *dimz_out = dimz % 2  == 0 ? dimz + 1 : dimz;
     *dims_out = dims % 2  == 0 ? dims + 1 : dims;
 
   /* ----------------- Update der Offsets der Affintransf. */
     for (i = 0; i < 6; i++) {
         a0[i] = k0[i];
         }
      a0[2] -= minz - 1.0;
      a0[5] -= mins - 1.0;
   /* ----------------- Inverse Affintransf. Parameter*/
      status = inv_33_aff (a0, a1);
    
      return(status);
      }
 
 
 

/* 
  Function berechnet Affinkoeffizienten und die inversen Koeffizienten
  um den Mittelpunkt eines Bildpatches auf einen Punkt des Zielsystems
  zu transferrieren 
*/
static int zoom_kof_ref (double    *k0,
			 int       dimz,
			 int       dims,
			 double    *a0, 
			 double    *a1)
   {
   double      dim_z, dim_s;
   int         status;

   dim_z = (double) (dimz / 2 + 1);
   dim_s = (double) (dims / 2 + 1);

   a0[0] = k0[0];
   a0[1] = k0[1];
   a0[2] = dim_z; 
   a0[3] = k0[3];
   a0[4] = k0[4];
   a0[5] = dim_s; 

   status = inv_33_aff (a0, a1);

   return(status);
   }




/* -----------------------------------------
   Funktion transformiert (resample "bilinear") Integer Bilddaten 
   mittels Affin Transformation 
   
       mode    int     mode = 0   falls mtr_out ausserhalb von
                                  mtr_in dann return (EOF)
                       mode = 1   falls mtr_out ausserhalb von
                                  mtr_in dann erhalten die Grau-  
                                  werte die ausserhalb liegen den 
                                  Wert 0
                       ------------------------------------------- */
static int zoom_in_int (INT_MATRIX    *mtr_in,
			INT_MATRIX    *mtr_out,
			double        *kt,
			int            mode)
   {
   int          *ptr_e;
   int           zei_e, spa_e;
   int          *ptr_a;
   int           zei_a, spa_a;
   int           zz, ss;
   int          *ip_i;
   register int       iz, is;
   register double    k0, k1, k2, k3, k4, k5;
   register double    z1, s1, z2, s2, z2_z, s2_z;

   ptr_e = mtr_in->ptr_m;
   zei_e = mtr_in->dimz;
   spa_e = mtr_in->dims;
   ptr_a = mtr_out->ptr_m;
   zei_a = mtr_out->dimz;
   spa_a = mtr_out->dims;

   k0 = kt[0];
   k1 = kt[1];
   k2 = kt[2];
   k3 = kt[3];
   k4 = kt[4];
   k5 = kt[5];

   if (mode == 1) {
      z1 = 1.0;
      s1 = 1.0;
      zz = (long) (k0 * z1 + k1 * s1 + k2);
      ss = (long) (k3 * z1 + k4 * s1 + k5);
  /* ######################################### */
      if (zz < 0 || zz > zei_e) return(EOF);
      if (ss < 0 || ss > spa_e) return(EOF);

  /* ######################################### */
      z1 = (double)zei_a;
      s1 = 1.0;
      zz = (long) (k0 * z1 + k1 * s1 + k2);
      ss = (long) (k3 * z1 + k4 * s1 + k5);
  /* ######################################### */
      if (zz < 0 || zz > zei_e) return(EOF);
      if (ss < 0 || ss > spa_e) return(EOF);

  /* ######################################### */
      z1 = (double)zei_a;
      s1 = (double)spa_a;
      zz = (long) (k0 * z1 + k1 * s1 + k2);
      ss = (long) (k3 * z1 + k4 * s1 + k5);
  /* ######################################### */
      if (zz < 0 || zz > zei_e) return(EOF);
      if (ss < 0 || ss > spa_e) return(EOF);

  /* ######################################### */
      z1 = 1.0;
      s1 = (double)spa_a;
      zz = (long) (k0 * z1 + k1 * s1 + k2);
      ss = (long) (k3 * z1 + k4 * s1 + k5);
  /* ######################################### */
      if (zz < 0 || zz > zei_e) return(EOF);
      if (ss < 0 || ss > spa_e) return(EOF);
  /* ######################################### */
      }


   for (iz = 0; iz < zei_a; iz++)
       {
       ip_i =  ptr_a + iz * spa_a;
       z1   = (double)iz + 1.0; 

       z2_z = k0 * z1 + k2;
       s2_z = k3 * z1 + k5;

       for (is = 0; is < spa_a; is++)
           {
        /* -----------------------------------------------------------
           Koordinaten im Ergebnisfile
           ----------------------------------------------------------- */
           s1 = (double)is + 1.0;
        /* -----------------------------------------------------------
           Koordinaten im Eingangsfile
           ----------------------------------------------------------- */
           z2 = z2_z + k1 * s1;
           s2 = s2_z + k4 * s1;

           *ip_i++ = bilinear_ip_int(ptr_e, z2, s2, zei_e, spa_e);

           }
       }

   return(1);
   }


/* ###################################################################
   Funktion     : bilinear_ip_int
   Enthalten in : /home/franz/lib/f_lib.a
   Erstellt     : August 1990                           Version : 2.0
   Autor        : Franz Wewel
   geaendert    : Aug 94                       !! float --> double !!
   ------------------------------------------------------------------
   Die Funktion fuehrt das Resampling nach dem verfahren der bi - 
   linearen Interpolation durch.

   Der Grauwert des Suchpixels wird aus den gewichtetem Mittel der 
   Grauwerte der vier benachbarten Bildpunkte im Eingangsbild
   ermittelt. Die Mittelung erfolgt in Abhaengigkeit von der
   Entfernung der Nachbarpixel zum Suchpixel.
   Die Funktion liefert den interpolierten Grauwert oder den Wert 0,
   wenn der Bildpunkt ausserhalb der Bildgrenzen des Eingangsbild 
   liegt.
   -------------------------------------------------------------------
   Include :      
   -------------------------------------------------------------------
   Parameter :
   Ein:
   --> ipic_vektor  int  * Zeiger auf den Vektor der Bilddaten des 
                           Eingangsbildes
       zeile       double   Ungerade Zeilen - Position des Bildpunktes 
                           im Eingangsbild
       spalte      double   Ungerade Spalten - Position des Bildpunktes 
                           im Eingangsbild
       anz_zeilen  int     Anzahl der Zeilen des im Vektor gespeicher =
                           ten Bildauschnittes
       anz_spalten int     Anzahl der Spalten des im Vektor gespeicher=
                           ten Bildauschnittes
   -------------------------------------------------------------------
   Return :  Die Funktion liefert den interpolierten Grauwert oder
             den Wert 0, falls der Bildpunkt ausserhalb des
             Bildausschnittes liegt.
   -------------------------------------------------------------------
   Variablen :
   -------------------------------------------------------------------
   Bibliotheken :    
   ################################################################### */
static int bilinear_ip_int (int        *ipic_vektor,
			    double      zeile, 
			    double      spalte,
			    int         anz_zeilen, 
			    int         anz_spalten)
   {
/* ------------------------------------------------------------------
   Variablen
   ------------------------------------------------------------------ */
   register int     pos0_invektor, pos1_invektor, 
                    pos2_invektor, pos3_invektor;
   double           delta_zeile, delta_spalte, gw5, gw6, gd;
   int              gw;
   int              izei, ispa, gw1, gw2, gw3, gw4;
   int              in_patch;
/* ------------------------------------------------------------------
   Bilineare Interpolation
   ------------------------------------------------------------------ */
   izei = (int)zeile;
   ispa = (int)spalte;

   in_patch =  izei < anz_zeilen && ispa < anz_spalten
            && izei > 0 && ispa > 0;
   if (in_patch) {
   /* ---------------------------------------------------------------
      Position der Nachbarpixel im Vektor bestimmen
      --------------------------------------------------------------- */
      pos0_invektor = ((izei - 1)*anz_spalten) - 1 + ispa;
      pos1_invektor = pos0_invektor + 1;
      pos2_invektor = pos0_invektor + anz_spalten;
      pos3_invektor = pos2_invektor + 1;

      delta_zeile  = zeile  - (double) izei;
      delta_spalte = spalte - (double) ispa;
   /* ---------------------------------------------------------------
      Grauwerte der 4 benachbarten Bildpunkte
      --------------------------------------------------------------- */
      gw1 = *(ipic_vektor + pos0_invektor);
      gw2 = *(ipic_vektor + pos1_invektor);
      gw3 = *(ipic_vektor + pos2_invektor);
      gw4 = *(ipic_vektor + pos3_invektor);
   /* ----------------------------------------------------------------
      Grauwerte interpoliert in Zeilenrichtung
      --------------------------------------------------------------- */
      gd  = (double) (gw2 - gw1);
      gw5 = (double)  gw1 + delta_spalte * gd;
      gd  = (double) (gw4 - gw3);
      gw6 = (double)  gw3 + delta_spalte * gd;
   /* ---------------------------------------------------------------
      bilinear interpolierte Grauwert
      --------------------------------------------------------------- */
      gw  = (int) (gw5 + delta_zeile * (gw6 - gw5) + 0.4999);

      }
    else {
    /* --------------------------------------------------------------
       Bildpunkt liegt ausserhalb des Bildausschnittes
       -------------------------------------------------------------- */
       gw = near_nb_int (ipic_vektor, zeile,  spalte, 
                         anz_zeilen, anz_spalten);
       }
    return (gw);
    }

/* ---------------------------------------
  resampling - nearest neigbour
  ---------------------------------------- */
static int near_nb_int (int        *ipic_vektor, 
			double      zeile, 
			double      spalte,
			int         anz_zeilen, 
			int         anz_spalten)
   {
   int      zei_i, spa_i;
   int      gw;
/* ------------------------------------------------------------------
   GW im File
   ------------------------------------------------------------------ */
   if (zeile <= anz_zeilen && spalte <= anz_spalten && 
       zeile > 0.5001 && spalte > 0.5001)
      {
      zei_i = (int)(zeile  + 0.49999) - 1;
      spa_i = (int)(spalte + 0.49999) - 1;
     
      gw = *(ipic_vektor + zei_i * anz_spalten + spa_i);
      }
    else 
       {
    /* ---------------------------------------------------------------
       Bildpunkt liegt ausserhalb des Bildausschnittes
       -------------------------------------------------------------- */
       gw = 0;
       }
    return (gw);
    }
    




/* ###################################################################
   Funktion liefert den kleinsten Wert von x0 ... x3
   ################################################################### */
static double dmin4 (double x0, 
		     double x1,
		     double x2,
		     double x3)
   {
   x0 = x0 < x1 ? x0 : x1;
   x2 = x2 < x3 ? x2 : x3;

   return (x0 < x2 ? x0 : x2);
   }

/* ###################################################################
   Funktion liefert den groessten Wert von x0 ... x3
   ################################################################### */
static double dmax4 (double  x0,
		     double  x1,
		     double  x2,
		     double  x3)
   {
   x0 = x0 > x1 ? x0 : x1;
   x2 = x2 > x3 ? x2 : x3;

   return (x0 > x2 ? x0 : x2);
   }

/* ------------------------------------
   Invertierung von Affinkoeffizienten 
   ------------------------------------ */
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

   if (fabs(det) > 1E-10) {
      *a_1       =  a4 / det;
      *(a_1 + 1) = -a1 / det;
      *(a_1 + 2) =  (a1 * a5 - a2 * a4) / det;
      *(a_1 + 3) = -a3 / det;
      *(a_1 + 4) =  a0 / det;
      *(a_1 + 5) =  (a2 * a3 - a0 * a5) / det;
      }
   else  {
      return (-1);
      }
   return (1);
   }



/* ############################################################
   Funktion berechnet an Hand der Affinkoeffizienten den
   Maszstabsunterschied zwischen zwei Bildern.

   Funktion liefert den Wert mode zurueck:

      mode = 0:   aff[0], aff[4] bzw aff[1], aff[3], sind 
                  ungefaehr 1.0 bzw 0.0. Die Differenz ist kleiner
                  als AFF_MAX.
      mode = 1:   Referenzbild hat gegenueber dem Vergleichsbild
                  die hoehere Aufloesung.
      mode = 2:   Vergleichsbild hat gegenueber dem Referenzbild
                  die hoehere Aufloesung.
   ############################################################ */
static int masz_check (double *aff) 
   {
   int         mode;
   int         m1, m2, m3, m4;
   double      x1, y1;
   double      masz = 1.0;
   double      nn;

   m1 = fabs(aff[0] - 1.0) < AFF_MAX;
   m2 = fabs(aff[4] - 1.0) < AFF_MAX;
   m3 = fabs(aff[1]) < AFF_MAX;
   m4 = fabs(aff[3]) < AFF_MAX;
   
   if (m1 && m2 && m3 && m4) {

      mode = 0;

      }
   else {

      x1 = aff[0] + aff[1];
      y1 = aff[3] + aff[4];

      nn = x1 * x1 + y1 * y1;

      if (fabs(nn) < EPS1) {
         mode = 0;                  /* - failure ?, nevertheless 
                                                go at it with mode 0 */
         }
      else {

         masz = sqrt (2.0 / nn);
     /* ----------------
                   mode = 1 --> Referenzbild, 
                   mode = 2 --> Vergleichsbild hat hoehere Aufloesung */

         mode = masz > 1.0 + AFF_MAX ? 1 : 2;
         }
       }

   return (mode);
   }


/* Affin - Transformation */ 
static double aff_trf(double z, double s, double *a)
   {
   double    x;
 
   x = a[0] * z + a[1] * s + a[2];
   return(x);
   }
 

/* ###################################################################
   Funktion     : kqkkor_ip.c
   Enthalten in : /home/franz/lib/quellen/korr
   Erstellt     : Oktober 1991                          Version : 1.0
   Autor        : Franz Wewel
   ------------------------------------------------------------------
   Funktion zur Korrelation nach der Methode der Kleinsten Quadrate

   Die Funktion kqk_kor steuert den vollstaendigen Algorithmus der 
   Kleinsten Quadrate Korrelation.  Fuer die KQK muessen die Bilderaus-
   schnitte die verglichen werden sollen auf mindestens 2 Pixel ange-
   naehert in Muster und Vergleichs- (Such) matrix gegeben sein.
   Die Subpixeleinpassung erfogt durch eine 6 parametrige Affintrans-
   transformation. Die Umbildung erfolgt durch eine billineare Inter-
   polation. Bei der Ausgleichung werden zusaetzlich zwei Kontrast-
   parameter als Unbekannte angesetzt.
   --> g2(x2, y2) = c * g1(x1, y1) + d
             mit: -> x1 = a0 + a1 * x + a2 * y;
                  -> y1 = b0 + b1 * x + b2 * y; 
   Die Einpassug erfolgt Iterativ. Zwei Abbruchkriterien steuern
   die Iteration:
   1.) Die Iteration wird abgebrochen wenn die absolut Werte der
   Aenderungen dz(alt) - dz(neu) bzw ds(alt) - ds(neu) kleiner als
   EPS1 sind (!!! damit wirkt sich EPS1 auch auf die Genauigkeit der
   Subpixeleinpassung aus !!!). Oder
   2.) Die maximale Anzahl an Iterationen von ITER_MAX erreicht wird.
   ------------------------------------------------------------------
   Include      :      kqkkor.h 
   ------------------------------------------------------------------
   Parameter    :
   Ein:
      suchma          INT_MATRIX *   Suchmatrix
            ->zei1    long           %
            ->spa1    long           %
            ->dimz    long           Anz. der Zeilen der Such-Matrix
            ->dims    long            "    "  Spalten "    "    " 
            ->ptr_m   int *          Zeiger auf Vektor der Bilddaten
      musterma        INT_MATRIX *   Mustermatrix
            ->zei1    long           %
            ->spa1    long           %
            ->dimz    long           Anz. der Zeilen der Muster-Matrix
            ->dims    long            "    "  Spalten "    "      " 
            ->ptr_m   int *          Zeiger auf Vektor der Bilddaten

      info            KORPAR *       Steuerdaten fuer Kleinste Quadrate
                                     Korrelation
            ->mintxt  double         Minimale Textur (wird in kqkkor nicht
                                     genutzt).
            ->suchb   int            Suchbereich der PMK-Korelation (wird 
                                     in kqkkor nicht genutzt).
            ->pmkdim  int            Fenstergroesse fuer PMK-Korelation
                                     (3 < pmkdim < 51 ! ungerade Zahl !)
            ->kqkdim  int            Fenstergroesse fuer KQK-Korelation
                                     (11 < kqkdim < 51 ! ungerade Zahl !)
            ->itermx  int            Abbruchkriterium fuer maximale 
                                     Anzahl an Iterationen (2 < itermx < 15).
            ->lsmacu  float          Abbruchkriterium der Iteration 
                                     Minimale Aenderung der Position
            ->max_mp  float          Maximale erlaubte Pkt-Fehler.


   Aus:
      erg       ERG_0 *      Struktur der Ergebnisdaten
         ->dz   double     Subpixelverschiebung in Zeilenrichtung
         ->ds   double           "              "  Spaltenr.
         ->pmk  double     Korrelationswert fuer PMK
         ->kqk  double     PMK-Wert fuer eingepasste Matrix nach der
                           Kleinsten Quadrate Korrelation
         ->mp   double     mittlere Punktfehler aus der Ausgleichung
                         (!!! dabei nicht Abbruchkriterium EPS1 
                               vergessen !!!)
   ------------------------------------------------------------------
   Return :     OK              Punkt wurde erfolgreich uebertragen
                anderer Wert    Fehler
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name                    Typ             enthalten in
   ################################################################### */
static int kqkkor_ip (INT_MATRIX      *musterma, 
		      INT_MATRIX      *suchma,
		      int              vz, 
		      int              vs, 
		      KORPAR          *info,
		      ERG_0           *erg)
   {
   int           status=1, true=0, iter=0, itr;
   double        min_z, min_s, max_z, max_s;
   double        offx, offy;
   double        xy[4], yx[4];
   double        dz, ds;
   double        zeialt, spaalt, zeineu = 0.0, spaneu = 0.0;
   double        m0, mp, sumvv;
   double        lsmacu, aculsm, max_mp;
   int           itermx;
   double        ga, gb;
   long          dimpmk, dimkqk;
   double        zneu = 0.0, sneu = 0.0;
   double        *amat, *dg, *x0, *x, *qxx;
   int           dimz, dims;
   INT_MATRIX    verglma;
   int           u, n;                      /* Anzahl der Unbekannten,
                                                          Beobachtungen */
/* ------------------------------------------- Variablen initialisieren */
   erg->kqk = 0.0;
   erg->dz  = 0.0;
   erg->ds  = 0.0;
   erg->mp  = 0.0;

   dimz = musterma->dimz;
   dims = musterma->dims;

   n = (dimz - 2) * (dims - 2);
   u = U;

   itermx = ITERMAX;
   itr    = itermx + 1;

   max_mp = info->lsmacu;
   max_mp = max_mp < LSMACU ? LSMACU : max_mp;

   lsmacu = max_mp;
   aculsm = lsmacu / 2.0;

   dimkqk = info->kqkdim;
   dimpmk = info->pmkdim;
/* ---------------------------------- Vergleichsmatrix initialisieren */
   verglma.dimz  = dimz;
   verglma.dims  = dims;
   verglma.ptr_m = (int *)malloc(sizeof(int) * dimz * dims);
   if (!verglma.ptr_m) return(MCLERR1);


   status = mtrx_2_mtrx (suchma, &verglma, vz, vs);
   if (status < 1) return(EOF);

/* ----------------------------------------------------------------------
   erg->pmk = pmkkof_1 (&verglma, musterma, dimpmk, dimpmk, 0, 0);
   if (erg->pmk < info->minpmk) return(MINPMK);
   ---------------------------------------------------------------------- */

/* ----------------------------------------------------------------------
   Speicherplatz fuer Ausgleichungsmatrizen allokieren
   ---------------------------------------------------------------------- */
   status = ausgl_mtrx (&amat, &dg, &x0, &x, &qxx, u, n);
   if_EOF_return;
/* ----------------------------------------------------------------------
   Startwerte der Korrelation
   ---------------------------------------------------------------------- */
   start_value (x0, xy, yx, dimz, dims, suchma->dimz, suchma->dims,
                &offx, &offy, &min_z, &min_s, &max_z, &max_s, &ga, &gb);

   offx += vz;                /* ------------ Verschiebung aus  PMK */
   offy += vs;
/* ----------------------------------------------------------------------
   Beginn der Iteration
   ---------------------------------------------------------------------- */ 
   do { 
      zeialt = zeineu;
      spaalt = spaneu;
      iter++;
   /* --------------------------------------------------------------------
      Aufstellen der Koeffizienten - Matrix a und des Beobachtungsvektors
      -------------------------------------------------------------------- */
      a_mtrx (musterma, &verglma, amat, dg, n, u);
   /* --------------------------------------------------------------------
      Ausgleichung nach vermittelnen Beobachtungen
      -------------------------------------------------------------------- */ 
      status = aveb1 (amat, dg, x0, x, qxx, &sumvv, u, n);
      if (status != OK) {
          free_mtrx (amat, dg, x0, x, qxx);
          free((void *) verglma.ptr_m);
          return (status);
          }
   /* ---------------------------------------------------------------------
      Grauwertzuweisung fuer Vergleichsmatrix (Resampling)
      Interpolation aus den Grauwerten der Suchmatrix
      --------------------------------------------------------------------- */
      ga = x[6] + x[7] * ga;
      gb = x[7] * gb;
      x[6] = ga;
      x[7] = gb;

      status = rsmp_1 (x, &verglma, suchma, offx, offy, xy, yx,
                       min_z, min_s, max_z, max_s);
      if (status == AUSERHALB){
          free_mtrx (amat, dg, x0, x, qxx);
          free((void *) verglma.ptr_m);
          return (AUSERHALB);
          } 
   /* ---------------------------------------------------------------------
      Neue Zeilen/Spalten-Position
      --------------------------------------------------------------------- */
      zeineu = (xy[0] + xy[1] + xy[2] + xy[3]) / 4.0;
      spaneu = (yx[0] + yx[1] + yx[2] + yx[3]) / 4.0;

      dz = zeineu - zeialt;
      ds = spaneu - spaalt;
      dz = fabs(dz);
      ds = fabs(ds);

      if (dz <= lsmacu && ds <= lsmacu) {
         zneu = zeineu;
         sneu = spaneu;
         itr  = 1;
         lsmacu = dz > ds ? dz : ds;
         }

      true = (dz > aculsm || ds > aculsm) && iter < itermx;

      } while (true);  

/* ----------------- ###################### ------------------------------
   Punktfehler des Homologenpunktpaares
   ----------------- ###################### ------------------------------ */
   mp_fehler (zneu, sneu, qxx, sumvv, u, n, &m0, &mp);


/* ---------------------------------------------------------------------
   Korrelationskoeffizient
   erg->kqk = pmkkof (musterma, &verglma);
   --------------------------------------------------------------------- */
   erg->kqk = pmkkof_1 (&verglma, musterma, dimpmk, dimpmk, 0, 0);
   erg->dz  = zneu;
   erg->ds  = sneu;
   erg->mp  = mp;
/* ---------------------------------------------------------------------
                                          free memory 
                                          Change: Okt. 97 Version: 1.010a */
   free_mtrx (amat, dg, x0, x, qxx);    
   free((void *) verglma.ptr_m);

/* ################################################
   if (erg->kqk < info->minkqk) return(MINKQK);
   ################################################ */
   if (itr >= itermx)       return (ZUVIEL_ITER);
   if (erg->kqk < erg->pmk) return (ZUVIEL_ITER);
   if (mp  >= max_mp)       return (GRMP);

   return(OK_KQK);
   }


/* ###################################################################
   Initialisierung der Startwerte fuer die Kleinste-Quadrate-Korrelation
   ################################################################### */
static void start_value (double         *x0,
			 double         *xy, 
			 double         *yx,
			 long            dimz_m, 
			 long            dims_m, 
			 long            dimz_s, 
			 long            dims_s, 
			 double         *offx, 
			 double         *offy,
			 double         *min_z, 
			 double         *min_s, 
			 double         *max_z, 
			 double         *max_s,
			 double         *gw_a, 
			 double         *gw_b)
   { 
   long    delta_dimz_2, delta_dims_2;

   delta_dimz_2 = (dimz_s - dimz_m) / 2;
   delta_dims_2 = (dims_s - dims_m) / 2;

   x0[0] = x0[2] = x0[3] = x0[4] = x0[6] = 0.0;
   x0[1] = x0[5] = x0[7] = 1.0;
   xy[0] = (double) (-dimz_m / 2);
   xy[1] =  xy[0];
   xy[2] = -xy[0];
   xy[3] = -xy[0];
   yx[0] = (double) (-dims_m / 2);
   yx[1] = -yx[0];
   yx[2] =  yx[0];
   yx[3] = -yx[0];

   *min_z = 1.0;
   *min_s = 1.0;
   *max_z = (double) (dimz_m + 2  * delta_dimz_2 - 1);
   *max_s = (double) (dims_m + 2  * delta_dims_2 - 1);
   *offx  = (double)((dimz_m / 2) + delta_dimz_2 + 1);
   *offy  = (double)((dims_m / 2) + delta_dims_2 + 1);

   *gw_a = 0.0;
   *gw_b = 1.0;
   }



/* ###################################################################
   Funktion     : a_mtrx.c
   Enthalten in : 
   Erstellt     : Oktober 1991                          Version : 1.0
   Autor        : Franz Wewel
   ------------------------------------------------------------------
   Die Funktion a_mtrx stellt die Koeffizienten fuer die Ausgleichung
   nach der Kleinsten Quadrate Methode auf.

   Die Funktion a_mtrx mittelt korrespondierenden Grauwerte von Muster- 
   und Vergleichsmatrix. Aus den Mitteln werden die Differenzenquotien-
   ten berechnet mit denen die Grauwertgradienten der Koeffizientenma-
   trix angenaehert werden.
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name       Dim          Typ             Bedeutung

    --> musterma   dimz,dimsp   INT_MATRIX *    Mustermatrix
    --> verglma    dimz,dims    INT_MATRIX *    Vergl.matrix
    --> dg         DIMU         double *        Beobachtungsvektor
        n                       long            Anzahl der Beobachtungen
        u                       long            Anzahl der Unbekannten
   Aus:
    --> amat       u,n          double *        A-Matrix
   ------------------------------------------------------------------
   Return :     ----         
   ------------------------------------------------------------------
   Variablen :
                gradx           double          Gradient Spaltenrichtung
                grady           double          Gradient Zeilenrichtung
                gw              long            Grauwerte
                gw2, gw4, gw5                   zur Berechnung
                gw6, gw8        double          der Koeffizienten       
                i, j, l, m      register long   Laufvariablen
                ix              register long   Laufvariable Spalte
                iy              register long   Laufvariable Zeile
                delta_system    register long   Systemoffset
                p, p1           register long   Hilfsvariable Position
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
        beovek()        void            k_lib.a
   ################################################################### */
static void a_mtrx (INT_MATRIX      *musterma, 
		    INT_MATRIX      *verglma,
		    double          *amat,
		    double          *dg,
		    long             n, 
		    long             u)
   {
   register long   ix, iy, p, m, delta_system;
   long            dimz, dims, dims0, dx, dy;
   int      	   gw2, gw4, gw5, gw6, gw8;
   double          gradx, grady;
   double          *a0;
   int             *mp0, *vp0;
/* -------------------------------------------------------------------
   Grauwertdifferenzen Vergleichsmatrix - Suchmatrix
   = Vektor der Beobachtungen
   ------------------------------------------------------------------- */
   beovek (dg, musterma, verglma);

   dimz = musterma->dimz - 1;
   dims0= musterma->dims;
   dims = dims0 - 1;
   delta_system = dimz / 2;
/* -----------------------------------------------------------------------
   Aufbau der Koeffizientenmatrix amat
   Schleifen ueber Elemente des Korrelationsfensters
   ----------------------------------------------------------------------- */
   m  = 0;
   mp0 = musterma->ptr_m;
   vp0 = verglma->ptr_m;
   for (ix = 1; ix < dimz; ix++)   /* Schleife uber die Zeilen */
       {
       dx = ix - delta_system;
       for (iy = 1; iy < dims; iy++) /* "        "   " Spalten */
	   {
           p   = (ix-1) * dims0 + iy;
           gw2 = *(mp0+p) + *(vp0+p);

           p  += dims0 - 1; 
           gw4 = *(mp0+p) + *(vp0+p);

           p++;
           gw5 = *(mp0+p) + *(vp0+p);

           p++;
           gw6 = *(mp0+p) + *(vp0+p);

           p  += dims0 - 1;
           gw8 = *(mp0+p) + *(vp0+p);

           gradx = (double)(gw8 - gw2) / 4.0; /* gradi. in Zeilenrichtung */
           grady = (double)(gw6 - gw4) / 4.0; /* gradi.  " Spalten    "   */
        /* ---------------------------------------------------------------
           Berechnung der Koeffizienten der Beobachtung m
           --------------------------------------------------------------- */
           dy  = iy - delta_system;
           p   = m * u;
           a0    = amat + p;
           *a0++ = gradx;  
           *a0++ = gradx * (double) dx;
	   *a0++ = gradx * (double) dy;
	   *a0++ = grady;
	   *a0++ = grady * (double) dx;
	   *a0++ = grady * (double) dy;
	   *a0++ = 1.0;
	   *a0   = (double)gw5 / 2.0;
           m++;
           }
       }
   }


/* ###################################################################
   Funktion     : beovek.c
   Enthalten in : 
   Erstellt     : Oktober 1991                          Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   Funktion zur Berechnung des Beobachtungsvektors  

   Die Funktion beobachtungsvektor berechnet den Beobachtungsvektor dg
   als Differenz der Grauwerte von Mustermatrix und Vergleichsmatrix
   ------------------------------------------------------------------
   Include      :     
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name          Typ             Bedeutung

    --> musterma      INT_MATRIX *   Zeiger auf Mustermatr.
    --> verglma       INT_MATRIX *   Zeiger auf Vergl.matr.
   Aus:
        dg            double *        Beobachtungsvektor
   ------------------------------------------------------------------
   Return :     ----
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
   ################################################################### */
static void beovek (double	     dg[],
		    INT_MATRIX     *musterma, 
		    INT_MATRIX     *verglma)
   {
   long    ix, iy, p, dimz, dims, dy, dims0;
   int     *mp0, *vp0, *mp, *vp, di;
   double  *bv;

   bv   = dg;
   dimz = musterma->dimz - 1;
   dims = musterma->dims - 1;
   dims0= musterma->dims;
   mp0  = musterma->ptr_m;
   vp0  = verglma->ptr_m;
   for (iy = 1; iy < dimz; iy++)
       {
       dy  = iy * dims0;
       for (ix=1; ix < dims; ix++)
	   {
	   p   = dy + ix;
           mp  = mp0 + p;
           vp  = vp0 + p;
           di  = *mp - *vp;
	   *bv = (double) di;
           bv++;
           }	
       }
   }



/* ###################################################################
   Punktfehler aus Ausgleichungsergebnis der KQK
   ################################################################### */
static void mp_fehler (double      x, 
		       double      y,
		       double     *qxx,
		       double      sumvv,
		       long        u, 
		       long        n,
		       double     *m0, 
		       double     *mp)
   {
   double   f[2][U], qff[2], hv;
   long     i, j, k;
   long     p;
/* ---------------------------------------------------------------------
   Indizieren der F - Matrix
   --------------------------------------------------------------------- */
   for (i = 0; i < u; i++)
       {
       f[0][i] = 0.0;
       f[1][i] = 0.0;
       } 
   f[0][0] = f[1][3] = 1.0;
   f[0][1] = f[1][4] = x;
   f[0][2] = f[1][5] = y;
/* ---------------------------------------------------------------------
   Berechnung der Diagonalelemente der QFF-Matrix
   --------------------------------------------------------------------- */
   qff[0] = qff[1] = 0.0;
   for (i = 0; i < 2; i++)
       {
       for (j = 0; j < u; j++)
           {
           for (k = 0; k < u; k++)
               {
               p = j * u + k;
               qff[i] += f[i][k] * *(qxx + p) * f[i][j];
               }
           }
       }
/* ---------------------------------------------------------------------
   Fehlerrechnung 
   --------------------------------------------------------------------- */
   hv  = sumvv / (double) (n - u);
   *m0 = sqrt(hv);
   hv  = fabs(qff[0]) + fabs(qff[1]); 
   *mp = *m0 * sqrt(hv); 
   }




/* ###################################################################
   Programm     : rsmp_1
   Enthalten in : /home/franz/lib/quellen/korr/rsmp_1.c
   Erstellt     : August 1990                                Version : 1.0
   Autor        : Franz Wewel
   ---------------------------------------------------------------------
   Die Funktion resamp fuehrt das Resampling eines Bildblockes des
   Ausgangsbildes durch.
   Die Transformation erfolgt zeilenweise, innerhalb des Blockes, durch die 
   indirekte Methode. Innerhalb einer Zeile wird nur das Anfangspixel und 
   das Endpixel mittels parametrischen Entzerrungsansatz durch die Funktion
   trans1 in nichtganzzahlige Koordinaten des Systems des Eingangsbildes
   uberfuehrt. Die Koordinatenwerte fuer die anderen Punkte dieser Zeile 
   werden durch Interpolation ermittelt.
   Die Grauwerte zu diesen Koordinatenwerten, werden durch eine bilineare
   Interpolation berechnet und in das eindimensionales Feld fuer die
   Ergebnisdaten abgespeichert.
   -----------------------------------------------------------------------
   Include :      %
   -----------------------------------------------------------------------
   Return :       %
   ------------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------------
   Bibliotheken : 
   ################################################################### */
static int rsmp_1 (double        *a,
		   INT_MATRIX    *verglma, 
		   INT_MATRIX    *suchma, 
		   double         offx, 
		   double         offy,
		   double        *xzei, 
		   double        *yspa,
		   double         min_z, 
		   double         min_s, 
		   double         max_z, 
		   double         max_s)
   {
   register long    izei, ispa;
   register int     gw_i;
   int              status = 1;
   double           gw, zei, spa; 
   double           x1, y1, x2, y2, x3, y3, x4, y4;
   double           dzei_1, dspa_1, dzei_2, dspa_2;
   register int     *v_ip, dimz, dims, *s_ip;
   long             s_dimz, s_dims;

   s_dimz = suchma->dimz;
   s_dims = suchma->dims;
   s_ip   = suchma->ptr_m;

   dimz = (int) (verglma->dimz - 1);
   dims = (int) (verglma->dims - 1);
   v_ip = verglma->ptr_m;

   x1 = offx + a[0] + a[1] * xzei[0] + a[2] * yspa[0];
   y1 = offy + a[3] + a[4] * xzei[0] + a[5] * yspa[0];
   x2 = offx + a[0] + a[1] * xzei[1] + a[2] * yspa[1];
   y2 = offy + a[3] + a[4] * xzei[1] + a[5] * yspa[1];
   x3 = offx + a[0] + a[1] * xzei[2] + a[2] * yspa[2];
   y3 = offy + a[3] + a[4] * xzei[2] + a[5] * yspa[2];
   x4 = offx + a[0] + a[1] * xzei[3] + a[2] * yspa[3];
   y4 = offy + a[3] + a[4] * xzei[3] + a[5] * yspa[3];
   status = pruf_1 (x1, y1, x2, y2, x3, y3, x4, y4,  
                    min_z, min_s, max_z, max_s);
   if (status == FEHLER) return (AUSERHALB);

   dzei_1 = (x2 - x1) /  (double)dimz;
   dspa_1 = (y2 - y1) /  (double)dims;
   dzei_2 = (x3 - x1) /  (double)dimz;
   dspa_2 = (y3 - y1) /  (double)dims;

   for (izei = 0; izei <= dimz; izei++)  
       {
       zei = x1 + (double) izei * dzei_2;
       spa = y1 + (double) izei * dspa_2;

       for (ispa = 0;  ispa <= dims; ispa++)
           {
        /* ---------------------------------------------------------------
           Berechnung des Grauwertes im Resamplten - Bild
           durch bilineare - Interpolation
           --------------------------------------------------------------- */
           gw = bilinear_ip_int (s_ip, zei, spa, s_dimz, s_dims);

	   gw_i = (int) (a[6] + a[7] * gw + 0.49);

           *v_ip++ = (int) gw_i;

           zei += dzei_1; 
           spa += dspa_1; 
           }                    
       }  
   xzei[0] = x1 - offx;
   yspa[0] = y1 - offy;  
   xzei[1] = x2 - offx;
   yspa[1] = y2 - offy;                     
   xzei[2] = x3 - offx;
   yspa[2] = y3 - offy; 
   xzei[3] = x4 - offx;
   yspa[3] = y4 - offy;  
   return (OK);
   }



/* ###################################################################
   Funktion     : pruf_1
   Enthalten in : 
   Erstellt     : Oktober 1991                          Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   Die Funktion pruef prueft ob die transformierte Vergleichsmatrix
   vollstaendig in der Suchmatrix liegt
   ------------------------------------------------------------------
   Include      :  
   ------------------------------------------------------------------
   Parameter    :
   Ein:
                Name            Typ             Bedeutung

                a, b            double *        geometr. Transformations-
                                                parameter
                verglma         Matrix *        Vergleichsmatix
   Aus:
   ------------------------------------------------------------------
   Return :     OK      transformierte Vergleichsmatrix liegt voll-
                        staendig in der Suchmatrix 
                FEHLER  wenn nicht
   ------------------------------------------------------------------
   Variablen :
                max_z           double          groesste zu transf. Zeile
                max_sp          double          groesste zu transf. Spalte
                transzei        double          gr. transformierte Zeile
                transspa        double          gr. transformierte Spalte
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :     -----   
         dmax4()        double      /home/franz/lib/a_lib.a
         dmin4()        double      /home/franz/lib/a_lib.a
   ################################################################### */
static int pruf_1 (double   x0, 
		   double   y0, 
		   double   x1, 
		   double   y1, 
		   double   x2, 
		   double   y2, 
		   double   x3, 
		   double   y3,
		   double   min_z, 
		   double   min_s, 
		   double   max_z, 
		   double   max_s)
   {
   double     xmin, ymin, xmax, ymax;

   xmin = dmin4(x0, x1, x2, x3);
   ymin = dmin4(y0, y1, y2, y3);
   xmax = dmax4(x0, x1, x2, x3);
   ymax = dmax4(y0, y1, y2, y3);

   if (xmin < min_z || ymin < min_s || xmax > max_z || ymax > max_s)
      {
      return(FEHLER);
      }
   return(OK);
   }


/* ------------------------------------------------------------------
   Funktion     : pmkkof
   Enthalten in : 
   Erstellt     : Nov 1990                          Version : 1.1
   Autor        : Franz Wewel   
   Geaendert    : Stephan Fick  Oktober 1991
   ------------------------------------------------------------------
   Funktion zur Untersuchung der Korrelationsguete

   Die Funktion pmkkof berechnet den Produktmomentenkorrelationskoeffi-
   zient(PMK) rho nach Bravais und Pearson fuer einen Ausschnitt zweier
   gegebener Bildauscchnitte.
   ------------------------------------------------------------------
   Include      : 
   ------------------------------------------------------------------
   Parameter    :

   Ein:
    --> bima1  INT_MATRIX *        Zeiger auf linke  Matrix
    --> bima3  INT_MATRIX *        Zeiger auf rechte Matrix
        dinz   long
        dims   long
   Aus:
   ------------------------------------------------------------------
   Return : Korrelationskoeffizienten rho.
   ------------------------------------------------------------------
   Bibliotheken :    -lm    -- sqrt()
   ------------------------------------------------------------------ */
static double pmkkof_1 (INT_MATRIX     *bima1, 
			INT_MATRIX     *bima3, 
			int             dimz, 
			int             dims,
			int             vz, 
			int             vs)
   {
/* ---------------------------------------------------------------------
   Variablen
   --------------------------------------------------------------------- */
   long             dimz1, dims1, dimz3, dims3;
   double 	    s11, s13, s33;
   long             n, i, j, i1, i3, j1, j3;
   double           rho;                          /*   Ausgabeparameter  */
   int              *ip_1, *ip_3, *iz1, *iz3;
   double           pix1, pix3;
   double           sum1, sum3, sum11, sum33, sum13; 
/* --------------------------------------------------------------------
   Anfangswerte
   -------------------------------------------------------------------- */
   sum1  = 0;
   sum3  = 0;
   sum11 = 0;
   sum33 = 0;
   sum13 = 0;
   ip_1 = bima1->ptr_m;
   ip_3 = bima3->ptr_m;
   dimz1= bima1->dimz;
   dimz3= bima3->dimz;
   dims1= bima1->dims;
   dims3= bima3->dims;

   if (dimz > dimz1) dimz = dimz1;
   if (dimz > dimz3) dimz = dimz3;
   if (dims > dims1) dims = dims1;      
   if (dims > dims3) dims = dims3;      
/* -------------------------------------------------------------------
   Stichprobenumfang
   ------------------------------------------------------------------- */
   n =  dimz *  dims; 
   i1 = (dimz1 - dimz) / 2 + vz;
   i3 = (dimz3 - dimz) / 2;
   j1 = (dims1 - dims) / 2 + vs;
   j3 = (dims3 - dims) / 2;
/* -------------------------------------------------------------------
   Aufsummierung
   ------------------------------------------------------------------- */
   iz1 = ip_1 + i1 * dims1 + j1;
   iz3 = ip_3 + i3 * dims3 + j3;
   for (i = 0; i < dimz; i++)
       {
       for (j = 0; j < dims; j++)
           {
           pix1 =  (double) *(iz1 + j);
           pix3 =  (double) *(iz3 + j);

           sum1  += pix1;
           sum3  += pix3;
           sum11 += pix1 * pix1;
           sum33 += pix3 * pix3;
           sum13 += pix1 * pix3;
           }
       iz1 += dims1;
       iz3 += dims3;
       }
/* -------------------------------------------------------------------
   Berechnung der Korrelationskoeffizieneten
   ------------------------------------------------------------------- */
   s13 = sum13 - (sum1 * sum3) / (double) n;
   s11 = sum11 - (sum1 * sum1) / (double) n;
   s33 = sum33 - (sum3 * sum3) / (double) n;
/* -------------------------------------------------------------------
   Verifikation
   ------------------------------------------------------------------- */
   if (s11 * s33 <= 0.0)
      {
      rho = 0.0;
      }
   else
      {
      rho = s13 / sqrt (s11 * s33);
      }
   return (rho);
   }


/* ------------------------------------
   Steuern der Kreuz Korrelation 
   ------------------------------------ */
static double pmkkor (INT_MATRIX *musterma, INT_MATRIX *suchma, int dimpmk, 
		      int bzei, int bspa, int *dz, int *ds)
   {
   register int        iz, is;
   register int        vz, vs;
   int                 dim0, dim1;
   double              rho_a = -2.0, rho_n;

   vz = 0;
   vs = 0;

   dim0 = musterma->dimz;
   dim1 =   suchma->dimz;
   if (dim1 < dim0) return(-1);
   dim0 = (dim1 - dim0) / 2;
   bzei =  bzei > dim0 ? dim0 : bzei;

   dim0 = musterma->dims;
   dim1 =   suchma->dims;
   if (dim1 < dim0) return(-1);
   dim0 = (dim1 - dim0) / 2;
   bspa =  bspa > dim0 ? dim0 : bspa;

/* ---------------------------------
                                 Schleife ueber den PMK Suchbereich */
   for (iz = -bzei; iz <= bzei; iz++) {
       for (is = -bspa; is <= bspa; is++) {
           rho_n = pmkkof_1(suchma, musterma, dimpmk, dimpmk, iz, is);

           if (rho_n > rho_a) {
               rho_a = rho_n;
               vz = iz;
               vs = is;
               }
           }
        }

   *dz = vz;
   *ds = vs;

   return(rho_a);
   }





/* ------------------------------------------------
   Kopiere teile der Matrix 0 in die Matrix 2
   ------------------------------------------------ */
static int mtrx_2_mtrx (INT_MATRIX *mtrx_0, INT_MATRIX *mtrx_1, int iz, int is)
   {
   int     i, j, dimz, dims, dims_z, dims_s, offz, offs;
   int     *ip,  *ip_s, *ip_v;

   dims_z  = mtrx_0->dimz;
   dims_s  = mtrx_0->dims;
   dimz    = mtrx_1->dimz;
   dims    = mtrx_1->dims;

   offz = (dims_z - dimz) / 2 + iz;
   offs = (dims_s - dims) / 2 + is;
 
   if (offz < 0) return(EOF);
   if (offs < 0) return(EOF);
   if (offz + dimz > dims_z) return(EOF);
   if (offs + dims > dims_s) return(EOF);

   ip   = mtrx_0->ptr_m + offz * dims_s + offs;
   ip_s = ip;
   ip_v = mtrx_1->ptr_m;
   
   for (i = 0; i < dimz; i++)
       {
       for (j = 0; j < dims; j++)
           {
           *ip_v = *ip_s;
           ip_v++;
           ip_s++;
           }
       ip  += dims_s;
       ip_s = ip;
       }
   return(1);
   }






/* ###################################################################
   Funktion     : aveb1
   Enthalten in :
   Erstellt     : Mai 1992                             Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   Funktion zur Ausgleichung nach vermittelnden Beobachtungen
  
   Ausgleichung ohne Gewichtsansatz
   (Abgespeckte Version von aveb ==> Komplikationen beim auftreten 
    von grob unterschiedlichen Werten in A-Matrix)
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name    Dim     Typ       Bedeutung
    --> a       n, u    double *  Koeffizientenmatrix
    --> l       n       double *  Beobachtungsvektor
    --> x0      u       double *  Naehrung d Unbek.
        u               long      Anz der Unbekannten
        n               long      Anz der Beobachtungen
   Aus:
    --> x       u       double *  ausgegl. Unbek.
    --> qxx     u, u    double *  Inverse der Normalgl. (ata) 
   ------------------------------------------------------------------
   Return :     ----         
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken :
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
   ################################################################### */
static int aveb1 (double       *a, 
		  double       *l, 
		  double       *x0,
		  double       *x, 
		  double       *qxx,
		  double       *sumvv,
		  long          u, 
		  long          n)
   {
   int       i, status;
   double    det;
   double    *ata, *atl, *dx, *v;



/* ---------------------- Speicherplatz fuer tmp - felder */
   ata = d_alloc ((long)(u * u), &status);
   if (status < 1) return(status);
   atl = d_alloc ((long)u, &status);
   if (status < 1) return(status);
   dx = d_alloc ((long)u, &status);
   if (status < 1) return(status);
   v = d_alloc ((long)n, &status);
   if (status < 1) return(status);

   *sumvv = 0.0;
/* ------------------------------------------------------------------
   Aufbau des Normalgleichungssystems
   ------------------------------------------------------------------ */
   status = ata_atl (a, l, u, n, ata, atl);
   if (status == MA_SCHWARZ) {
    /* ############### */
       free((void *) ata);
       free((void *) atl);
       free((void *) dx);
       free((void *) v);
    /* ############### */
       return (MA_SCHWARZ);
       }
/* ------------------------------------------------------------------
   Berechnung der Normalgleichungsinversen qxx
   ------------------------------------------------------------------ */
   det = inv_mtrx1 (ata, qxx, u);
   if (fabs(det) < EPS_DET1) {
    /* ############### */
       free((void *) ata);
       free((void *) atl);
       free((void *) dx);
       free((void *) v);
    /* ############### */
       return (DET_0);
       }
/* ------------------------------------------------------------------
   Berechnung der Verbesserungen und der Unbekannten
   ------------------------------------------------------------------ */
   unb_verb (atl, qxx, x0, u, dx, x);
/* -------------------------------------------------------------------
   Berechnung der Verbesserungen
   ------------------------------------------------------------------- */
   mtrxmul (a, dx, v, (int)n, (int)u, 1);
   for (i = 0; i < n; i++)
       {
       *(v + i) -= *(l + i);
       *sumvv += *(v + i) * *(v + i);
       }

/* ############### */
   free((void *) ata);
   free((void *) atl);
   free((void *) dx);
   free((void *) v);
/* ############### */

   return (OK);
   }




/* ###################################################################
   Funktion     : ata_atl
   Enthalten in : 
   Erstellt     : Mai 1992                             Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   Die Funktion ata_atl stellt das Normalgleichungssystem der Ausglei-
   chung auf
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name       Dim     Typ             Bedeutung
    --> a          n, u    double *        Koeffizientenmatrix
    --> l          n       double *        Beobachtungsvektor
        u                  long            Anz der Unbekannten
        n                  long            Anz der Beobachtungen
   Aus:
    --> ata        u, u    double *        Ata - Matrix
    --> atl        u       double *        Atl - Vektor
   Externe Parameter:
        OK              1
        MA_SCHWARZ
        EPS_SCHWARZ1
   ------------------------------------------------------------------
   Return :     ----         
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
   ################################################################### */
static int ata_atl (double     *a,
		    double     *l,
		    long        u,
		    long        n,
		    double     *ata,
		    double     *atl)
   {
   double          *a0, *a1, *ata0, *ata2, *atli, *ataj;
   double          ata1, atl0;
   register long   i, j, k, ku;
/* ------------------------------------------------------------------
   Berechnung von ata (Gausschetransf.)
   ------------------------------------------------------------------ */
   for (i = 0; i < u; i++)                /* i: Zeilenindex a */
       {
       ata0 = ata + i * u;
       a0   = a + i;
       for (j = i; j < u; j++)             /* j: Spaltenindex a' */
           {
           ata1 = 0.0;
           ataj = ata0 + j;
           a1   = a + j;
           for (k = 0; k < n; k++)         /* k: Zeilen/Spaltenindex  */
               {
               ku  = k * u;
               ata1 += *(a0+ku) * *(a1+ku);
               }
           *ataj = ata1;
           }
       }
/* -----------------------------------------------------------------
   Ausstieg aus der Ausgleichung wenn die Matrizen ungeeignet sind
   ----------------------------------------------------------------- */
   for (i = 0; i < u; i++)      
       {
       ku = i*u + i;
       if (*(ata + ku) < EPS_SCHWARZ1) return (MA_SCHWARZ);
       }
/* -----------------------------------------------------------------
   Umspeichern der symmetrischen Elemente
   ----------------------------------------------------------------- */
   for (i = 0; i < u; i++)
       {
       k = i * u;
       ata0 = ata + k;
       ata2 = ata + i;
       for (j = i; j < u; j++)
           {
           ku = j * u;
           a0 = ata2 + ku;
           a1 = ata0 + j; 
           *a0= *a1;
           }
        }
/* -----------------------------------------------------------------
   Berechnung von atl 
   ----------------------------------------------------------------- */
   for (i = 0; i < u; i++)
       {
       atl0 = 0.0;
       atli = atl + i;
       a0   = a + i;
       for (j = 0; j < n; j++)
           {
           ku = j * u;
           atl0 += *(a0 + ku) * *(l + j);
           }
       *atli = atl0;
       }
   return (OK);
   }


/* ###################################################################
   Funktion     : inv_mtrx1
   Enthalten in : /home/franz/lib/a_lib.a
   Erstellt     : Oktober 1991                          Version : 1.0
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
        n                     long
   Aus:
    --> mtrx_i                               Invertierte Matrix
   ------------------------------------------------------------------
   Return : 
                det             double          Determinante der inv-
                                                vertierten Matrix
   ------------------------------------------------------------------
   Variablen :
       x              double          Hilfsgroesse
       i, j, k        register long   Laufvariable
       p1             register long   Hilfsgroesse Position in Vektor
   ------------------------------------------------------------------
   Bibliotheken :  
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
   ################################################################### */
static double inv_mtrx1 (double   *mtrx_e,
			 double   *mtrx_i,
			 long      n)
   {
   double           x;
   double           *a0, *a1, *a2, *a3, *a4, *diag;
   int              i, j, k, p1, p2;
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

       if (fabs(x) < EPS_X1) return (0.0);
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
              if (fabs(*diag) < EPS_X1) return (0.0);
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


/* ###################################################################
   Funktion     : unb_verb
   Enthalten in : 
   Erstellt     : Mai 1992                              Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name     Dim       Typ            Bedeutung
    --> atl      u         double *       
    --> qxx      u*u       double *       
    --> x0       u
        u                  long          Anz der Unbek.
   Aus:
    --> dx       u         double *      
    --> x        u         double *      
   ------------------------------------------------------------------
   Return :     
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name                    Typ             enthalten in
   ################################################################### */
static void unb_verb (double   *atl,
               double   *qxx,
               double   *x0,
               long      u, 
               double   *dx,
               double   *x)
   {
   long      i;
/* ------------------------------------------------------------------
   Berechnung der Unbekannten - Zuschlage
   ------------------------------------------------------------------ */
   mtrxmul (qxx, atl, dx,  (int)u, (int)u, 1);
/* ------------------------------------------------------------------
   Verbesserungen den Naehrungswerten zuschlagen
   ------------------------------------------------------------------ */ 
   for (i = 0; i < u; i++)
       {
       x[i] = x0[i] + dx[i];
       }
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
   Funktion     : ausgl_mtrx 
   Enthalten in : 
   Erstellt     : Mai 1992                             Version : 1.0
   Autor        : 
   ------------------------------------------------------------------
   Die Funktion aveb_mtrx stellt den Speicherplatz fuer die Vektoren
   und Matrizen fuer die Ausgleichung mit dem Programm aveb zur
   Verfuegung. 
   ------------------------------------------------------------------
   Include      :      
   ------------------------------------------------------------------
   Parameter    :
   Ein:
        Name    Dim     Typ        Bedeutung
    --> a       n, u    double **  Koeffizientenmatrix
    --> l       n       double **  Beobachtungsvektor
    --> x0      u       double **  Naehrung d Unbek.
    --> qxx     u, u    double **  Inverse der Normalgl. (ata)
        u               long       Anz der Unbekannten
        n               long       Anz der Beobachtungen
   Aus:
    --> x       u       double **  ausgegl. Unbek.
   ------------------------------------------------------------------
   Return :     EOF falls Fehler beim allokieren von Speicherplatz
                auftritt.         
   ------------------------------------------------------------------
   Variablen :
   ------------------------------------------------------------------
   Bibliotheken : 
   ------------------------------------------------------------------
   Unterprogramme :             
        Name            Typ             enthalten in
        d_alloc ()      
   ################################################################### */
static int ausgl_mtrx (double       **a, 
		       double       **l, 
		       double       **x0, 
		       double       **x, 
		       double       **qxx,
		       long           u, 
		       long           n)
   {
   int       status;

/* ------------------------------------------------------------------
   Speicherplatz fur die Ausgleichungsmatrizen
   ------------------------------------------------------------------ */
   *a = d_alloc ((long)(u * n), &status);
   if (status < 1) return(status);
   *l = d_alloc ((long)n, &status);
   if (status < 1) return(status);
   *x0 = d_alloc ((long)u, &status);
   if (status < 1) return(status);
   *x  = d_alloc ((long)u, &status);
   if (status < 1) return(status);
   *qxx = d_alloc ((long)(u * u), &status);
   if (status < 1) return(status);

   return (1);
   }

static void free_mtrx (double       *a, 
		       double       *l, 
		       double       *x0, 
		       double       *x, 
		       double       *qxx)
   {
/* ############### */
   free ((void *) a);
   free ((void *) l);
   free ((void *) x0);
   free ((void *) x);
   free ((void *) qxx);
/* ############### */

   }





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
                                *status = 1 sonst *status = MCLERR1
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

   if (!(double *)ptr_buffer)
      {
      *ptr_status = MCLERR1;
      }
   else
      *ptr_status = 1;

   return (ptr_buffer);
   }

static void ini_erg (ERG_0   *erg)
   {
   erg->dz  = 0.0;
   erg->ds  = 0.0;
   erg->pmk = -1.0;
   erg->kqk = -1.0;
   erg->mp  = -1.0;
   }       



/* ------------------------------------------
   Konsistent Check der Eingabe Parameter
   ------------------------------------------ */
static void check_par (KORPAR par, KORPAR *par_1)
   {
   par_1->suchfns  =    par.suchfns < 1 ? DEF_SUCHB       :    par.suchfns;
   par_1->suchfns  = par_1->suchfns > MAX_SUCH ? MAX_SUCH : par_1->suchfns;


   par_1->pmkdim   =    par.pmkdim % 2 == 0  ? par.pmkdim - 1 :    par.pmkdim;
   par_1->pmkdim   = par_1->pmkdim < PMK_DIM ? DEF_PMKDIM     : par_1->pmkdim;
   par_1->pmkdim   = par_1->pmkdim > PMK_MAX ? PMK_MAX        : par_1->pmkdim;

   par_1->kqkdim   =    par.kqkdim % 2 == 0  ? par.kqkdim - 1 :    par.kqkdim;
   par_1->kqkdim   = par_1->kqkdim < KQK_DIM ? DEF_KQKDIM     : par_1->kqkdim;
   par_1->kqkdim   = par_1->kqkdim > KQK_MAX ? KQK_MAX        : par_1->kqkdim;

   par_1->pmkdim   = par_1->pmkdim > par_1->kqkdim ? par_1->kqkdim
                                                   : par_1->pmkdim;

   par_1->minpmk   = par.minpmk <= 0.0 ? DEF_MINPMK : par.minpmk;

   par_1->lsmacu   = par.lsmacu < LSMACU  ?  LSMACU : par.lsmacu + 0.0049;

   par_1->affin[0] = par.affin[0];
   par_1->affin[1] = par.affin[1];
   par_1->affin[2] = 0.0;
   par_1->affin[3] = par.affin[3];
   par_1->affin[4] = par.affin[4];
   par_1->affin[5] = 0.0;
   }


/* --------------------------------------------------
   Ergebnis Matching Daten Auswerten
   -------------------------------------------------- */
static int erg_check (int       st,
		      KORPAR   *par,
		      int       vz,
		      int       vs,
		      ERG_0      *erg)
   {
   int         status = -1;
   double      dz, ds;
   
   switch (st) {

        case OK_KQK:
             erg->dz += vz;
             erg->ds += vs;

             status = 1;
             break;

        case OK_PMK:
        case ZUVIEL_ITER:
        case GRMP:
        case MA_SCHWARZ:
        case DET_0:
        case AUSERHALB:

             erg->dz = vz;
             erg->ds = vs;

             erg->kqk = -1.0;

             status = 1;
             break;

        case EOF:
        case MCLERR1:
             erg->dz  =  0.0;
             erg->ds  =  0.0;
             erg->pmk = -1.0;
             erg->kqk = -1.0;
             erg->mp  = -1.0;
             status   = -1;
             break;

       case MINPMK:
       default:
             erg->dz  =  0.0;
             erg->ds  =  0.0;
             erg->kqk = -1.0;
             erg->mp  = -1.0;
             status   = -1;
             break;
       }
   return (status);
   }




static int cp_check_mtrx (int mode, 
			  INT_MATRIX   *muster,
			  INT_MATRIX   *muster_1,
			  INT_MATRIX   *such,
			  INT_MATRIX   *such_1,
			  KORPAR       *par_1,
			  double       *a1)
   {
   int         status = 1;
   double      a0[6], affin_1[6];


/* ------------------------------------------
                                            Mustermatrix initialisieren */
   muster_1->dimz  = par_1->kqkdim;
   muster_1->dims  = par_1->kqkdim;
   muster_1->ptr_m = (int *) malloc (sizeof(int)   *
                                     muster_1->dimz * muster_1->dims);
   if (muster_1->ptr_m == (int *)NULL) {
       fprintf (stderr, " *** failure in kqkkor-function: %s\n",
                strerror(errno));
       return(MCLERR1);
       }

/* ------------------------------ kaum Affin bzw Maszstabs Unterschiede */
   if (mode == 0) {
      *such_1   = *such;      
       status = mtrx_2_mtrx (muster, muster_1, 0, 0);
       if_EOF_return
       }
/* ---------------------- mode == 1   -->   Referenzbild transformieren */
   if (mode == 1) {

      status = zoom_kof_ref (par_1->affin, 
                             muster_1->dimz, 
                             muster_1->dims, 
                             a0, a1);
      if_EOF_return

      a1[2] += muster->dimz / 2 + 1;
      a1[5] += muster->dims / 2 + 1;
   /* ----------------------------------
                                       resampling a new reference matrix */
      status = zoom_in_int (muster, muster_1, a1, 0);
      if_EOF_return

      *such_1 = *such;
      }
/* -------------------- mode == 2   -->   Vergleichsbild transformieren */
   if (mode == 2) {

       status = mtrx_2_mtrx (muster, muster_1, 0, 0);
       if_EOF_return

       status = inv_33_aff (par_1->affin, affin_1);
       if_EOF_return

       status = zoom_kof_vgl  (affin_1, 
                               such->dimz,      such->dims,
                             &(such_1->dimz), &(such_1->dims), 
                               a0, a1);
       if_EOF_return

       such_1->ptr_m = (int *) malloc (sizeof(int) * such_1->dimz 
                                                   * such_1->dims);
       if (such_1->ptr_m == (int *)NULL) {
           fprintf (stderr, " *** failure in kqkkor-function: %s\n",
                                  strerror(errno));
           return(MCLERR1);
           }

       status = zoom_in_int (such, such_1, a1, 0);

       }
   return(status);
   }


static int check_mtrx (INT_MATRIX    *muster, 
		       INT_MATRIX    *such)
   {
   if (muster->dimz % 2 == 0) return(EOF); 
   if (muster->dims % 2 == 0) return(EOF); 

   if (such->dimz % 2 == 0) return(EOF); 
   if (such->dims % 2 == 0) return(EOF); 

   return (1);
   }




static int zoom_dim (int  dimz, int  dims, double  *aff, 
		     int  *dimz_out, int  *dims_out)
   {
   double      a0, a1, a3, a4;
   double      zz, ss, dz, ds;
   double      z_min, s_min, z_max, s_max;
   double      zz_1, ss_1, zz_2, ss_2, 
               zz_3, ss_3, zz_4, ss_4;


   a0 = aff[0];
   a1 = aff[1];
   a3 = aff[3];
   a4 = aff[4];
/* ######################################### */
   zz   = 1.0;
   ss   = 1.0;
   zz_1 = a0 * zz + a1 * ss;
   ss_1 = a3 * zz + a4 * ss;

/* ######################################### */
   zz = (double) dimz;
   ss = 1.0;
   zz_2 = a0 * zz + a1 * ss;
   ss_2 = a3 * zz + a4 * ss;

/* ######################################### */
   zz = (double) dimz;
   ss = (double) dims;
   zz_3 = a0 * zz + a1 * ss;
   ss_3 = a3 * zz + a4 * ss;

/* ######################################### */
   zz   = 1.0;
   ss   = (double) dims;
   zz_4 = a0 * zz + a1 * ss;
   ss_4 = a3 * zz + a4 * ss;


   z_min = dmin4 (zz_1, zz_2, zz_3, zz_4);
   s_min = dmin4 (ss_1, ss_2, ss_3, ss_4);
   z_max = dmax4 (zz_1, zz_2, zz_3, zz_4);
   s_max = dmax4 (ss_1, ss_2, ss_3, ss_4);

   dz = z_max - z_min + 1.0;
   ds = s_max - s_min + 1.0;


   *dimz_out = (int) dz;
   *dims_out = (int) ds;

   return 1;

   }








int write_int (char *fnam, unsigned int dz, unsigned int ds, int *ip_i)
   {
   FILE        *fp;
   int          zei, spa, status=1;
   char        *buf;
   unsigned int   i, anz;
   unsigned char *ip;
   
   ip = (unsigned char *)malloc (dz * ds * sizeof(unsigned char));
   if (ip == NULL) return (EOF);


   anz = dz * ds;

   for (i = 0; i < anz; i++) {
       *(ip + i) = (unsigned char) *(ip_i + i);
       }

   zei = dz;
   spa = ds;
   buf = (char *) ip;

   fp = fopen (fnam, "wb");
   if (fp == (FILE *)NULL) return(EOF);   

   fwrite ("TUUT", sizeof(char), 4, fp);
   fwrite ((char *) &zei, sizeof(long), 1, fp);
   fwrite ((char *) &spa, sizeof(long), 1, fp);
   rewind (fp);
   fseek (fp, spa, 0);
       
   fwrite (buf, sizeof(char), zei*spa, fp);
       
   fclose (fp);

   return (1);
   }

