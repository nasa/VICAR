$!****************************************************************************
$!
$! Build proc for MIPL module kqkkor
$! VPACK Version 1.9, Thursday, June 10, 1999, 10:24:36
$!
$! Execute by entering:		$ @kqkkor
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
$ write sys$output "*** module kqkkor ***"
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
$ write sys$output "Invalid argument given to kqkkor.com file -- ", primary
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
$   if F$SEARCH("kqkkor.imake") .nes. ""
$   then
$      vimake kqkkor
$      purge kqkkor.bld
$   else
$      if F$SEARCH("kqkkor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake kqkkor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @kqkkor.bld "STD"
$   else
$      @kqkkor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create kqkkor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack kqkkor.com -
	-s kqkkor.c -
	-i kqkkor.imake -
	-t tkqkkor.pdf tkqkkor.imake tkqkkor.c tkqkkor.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create kqkkor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create kqkkor.imake
#define SUBROUTINE kqkkor
#define MODULE_LIST kqkkor.c

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tkqkkor.pdf
Process help=*

PARM   SUCHB   TYPE=INTEGER COUNT=1  VALID=(1:20)  DEFAULT=10

PARM   PMKDIM  TYPE=INTEGER COUNT=1  VALID=(5:31)  DEFAULT=9

PARM   KQKDIM  TYPE=INTEGER COUNT=1  VALID=(9:51)  DEFAULT=17

PARM   LSMACU  TYPE=REAL    COUNT=1  VALID=(0.07:1.0) DEFAULT=0.1

PARM   MINPMK  TYPE=REAL    COUNT=1  VALID=(0.0:1.0)  DEFAULT=0.5


END-PROC
.Title
 Test program for the kqkkor function               Version: 1.0.7


.HELP

   kqkkor function for product momentum correlation (PMK) and least square
   matching (LSM) of two image patches.

   WRITTEN BY: TUB     30-Jun-1996

.LEVEL1
.VARI SUCHB
search area of the PMK correlation. 

.VARI  PMKDIM
patch size for the PMK correlation

.VARI  KQKDIM
patch size for the LSM correlation

.VARI  LSMACU
threshold value for the accuracy of the LSM correlation

.VARI  MINPMK 
threshold for the PMK correlation

.End
$!-----------------------------------------------------------------------------
$ create tkqkkor.imake
#define PROGRAM tkqkkor
#define MODULE_LIST tkqkkor.c 

#define MAIN_LANG_C
#define USES_ANSI_C

#define TEST

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create tkqkkor.c
#include "kqkkor.h"
#include "tkqkkor.h"


/* #################################################################
   test program -kqkkor-
   ################################################################# */
void main44()
   {
   int               status;
   int               count;
   int               i_val;
   float             f_val;
   INT_MATRIX        musterma;
   INT_MATRIX        suchma;
   KORPAR            info;
   ERG               erg;
   double            zz, ss, zz_0, ss_0, zz_1, ss_1;
   char              string[100];


   zveaction("usa", "");
/* ------------------------------------
                                   correlation parameter */
   /* Search Window */
   status = zvp("SUCHB", &i_val, &count);
   if (status <= 0) zabend();
   info.suchfns = i_val;

   /* Pathc size PMK */
   status = zvp("PMKDIM", &i_val, &count);
   if (status <= 0) zabend();
   info.pmkdim  = i_val;

   /* Patch size LSM */
   status = zvp("KQKDIM", &i_val, &count);
   if (status <= 0) zabend();
   info.kqkdim  = i_val;

   /* Accuracy */
   status = zvp("LSMACU", &f_val, &count);
   if (status <= 0) zabend();
   info.lsmacu  = f_val;

   /* Correlation threshold */
   status = zvp("MINPMK", &f_val, &count);
   if (status <= 0) zabend();
   info.minpmk  = f_val;

/* ------------------------------------
               initialisation of the test image patches */
   ini_tstpic (&Patch_1, &Patch_2);

/* -------------------------------------------
                     affin coefficients: reference --> search image 
	use defaults 100 010 or use hwaffinpar() if user said so */
   info.affin[0] = (float)Affin_0[0];
   info.affin[1] = (float)Affin_0[1];
   info.affin[2] = (float)Affin_0[2];
   info.affin[3] = (float)Affin_0[3];
   info.affin[4] = (float)Affin_0[4];
   info.affin[5] = (float)Affin_0[5];

/* #####################################################################
                    function calculates the minum dimensions for
                    the reference and the saerch patch
   ##################################################################### */
   dim_muster_such (&info, &musterma, &suchma);
	/* info - affine tr. parameters
	musterma - out - type INT_MATRIX.  Note that ptr_m has to be 
	calculated later, only min dimension is returned */

	/* same for suchma only it is search image, whereas musterma 
	is a reference image */

/* ----------------------------------------------------------- results */
   sprintf (string,
           "\n   -----------------------------------------------------------"); 
   zvmessage (string,  "");
   sprintf (string, 
           "    cross & least square matching: reference -> search patch"); 
   zvmessage (string,  "");
   sprintf (string, 
           "   -----------------------------------------------------------"); 
   zvmessage (string,  "");

   printf ("\n minum dimensioning of the reference patch:    dimz:%6d dims:%6d\n", 
           musterma.dimz, musterma.dims);

   printf (" minum dimensioning of the search image patch: dimz:%6d dims:%6d\n\n", 
           suchma.dimz, suchma.dims);

/* -----------------------------------------------
                           initialize the reference and search patch.
                           reading the pixel - data in a patch dimensionied
                           greater or equal to the dimension calculated in 
                           the function  dim_muster_such() */
   musterma.dimz   = Patch_1.dimz;
   musterma.dims   = Patch_1.dims;
   musterma.ptr_m  = Patch_1.ptr_m;

   suchma.dimz     = Patch_2.dimz;
   suchma.dims     = Patch_2.dims;
   suchma.ptr_m    = Patch_2.ptr_m; 

   printf ("\n actual dimension of the reference patch:    dimz:%6d dims:%6d\n", 
           musterma.dimz, musterma.dims);

   printf (" actual dimension of the search image patch: dimz:%6d dims:%6d\n\n", 
           suchma.dimz, suchma.dims);

/* ----------------------------------------------
                                     center of the reference matrix */
   zz = (double) (musterma.dimz / 2 + 1);
   ss = (double) (musterma.dims / 2 + 1);
   sprintf (string, " center pixel of the reference matrix: row %7.2lf col %7.2lf ", 
                    zz, ss); 
   zvmessage (string,  "");

   zz_0 = Affin_0[0] * zz + Affin_0[1] * ss + Affin_0[2];
   ss_0 = Affin_0[3] * zz + Affin_0[4] * ss + Affin_0[5];

/* ----------------------------------------------
                                     center of the search matrix */
   zz = (double) (suchma.dimz / 2 + 1);
   ss = (double) (suchma.dims / 2 + 1);
   sprintf (string, " center pixel of the search matrix   : row %7.2lf col %7.2lf ", 
                    zz, ss); 
   zvmessage (string,  "");


/* ------------------------------------------------
                                       cross & least square matching */
/* The main calculation is happening here */
   kqkkor (&info, &musterma, &suchma, &erg);
	/* erg - result 
		dz - line shift from the suchma
		ds - sample shift from the suchma
		pmk - cross-correlation coeficient
		kqk - cross-corelation coef. for sub-pixel accuracy 
		mp - accuracy for subpixel accuracy (0.1) */


/* ------------------------------------------------
                              cross & least square matching outcome */

   sprintf (string, "\n matching results: \0");
   zvmessage (string,  "");


   sprintf (string, "\n sub pixel displacement in line direction:      dz   %7.2f", erg.dz);
   zvmessage (string,  "");

   sprintf (string, " sub pixel displacement in sample direction:    ds   %7.2f", erg.ds);
   zvmessage (string,  "");

   sprintf (string, " correlation coefficient before LSM fitting:    pmk  %7.2f", erg.pmk);
   zvmessage (string,  "");

   sprintf (string, " correlation coefficient after LSM fitting:     kqk  %7.2f", erg.kqk);
   zvmessage (string,  "");

   sprintf (string, " point accuracy:                                mp   %7.2f\n", erg.mp);
   zvmessage (string,  "");

   zz_1 = (double) (suchma.dimz / 2 + 1) + (double) erg.dz;
   ss_1 = (double) (suchma.dims / 2 + 1) + (double) erg.ds;

/* Coordinates of the conjugate point based on relative coordinates of 
   the search area matrix */

   sprintf (string, " conjugate point:"); 
   zvmessage (string,  "");

   sprintf (string, "     reference - matrix:     row %7.2lf col %7.2lf  corresponds with", zz, ss); 
   zvmessage (string,  "");

   sprintf (string, "        search - matrix:     row %7.2lf col %7.2lf", zz_1, ss_1); 
   zvmessage (string,  "");


   sprintf (string, "\n difference to the known test target values: d_row: %7.2lf;  d_col %7.2lf", 
                    zz_0 - zz_1, ss_0 - ss_1); 
   zvmessage (string,  "");

   zvmessage ("",  "");

   zvmessage (" this is the end",  "");
   zvmessage ("",  "");
   }

$!-----------------------------------------------------------------------------
$ create tkqkkor.h
/*
C test program for kqkkor 
*/

#include <stdio.h>
#include <stdlib.h>

#include "vicmain_c"


/* ##################################################


 Include File fuer 2 Test-Bildpatches:

 Affinverzerrung von Patch 1 --> Patch 2:
     a0[0] =   0.6347
     a0[1] =  -0.0755
     a0[2] =  26.5383
     a0[3] =   0.0917
     a0[4] =   0.7056
     a0[5] =  10.9131

 Affinverzerrung von Patch 2 --> Patch 1:
     a1[0] =   1.5516
     a1[1] =   0.1660
     a1[2] = -42.9876
     a1[3] =  -0.2016
     a1[4] =   1.3957
     a1[5] =  -9.8797

 INT_MATRIX   Patch_1,  Patch_2;

 Patch_1.dimz  =   55   
 Patch_1.dims  =   61   
 Patch_1.ptr_m  = "Integer Zeiger auf den Vektor der Bilddaten"

 Patch_2.dimz  =   77   
 Patch_2.dims  =   71   
 Patch_2.ptr_m  = "Integer Zeiger auf den Vektor der Bilddaten"


 Aufruf zur Initialisierung der Integer-Matrix Patch_1 bzw Patch_2:
      ini_tstpic(&Patch_1, &Patch_2);

   ################################################## */

 struct int_mtrx 
           {                       
           int     dimz;           
           int     dims;           
           int    *ptr_m;          
           } Patch_1, Patch_2; 


 double  Affin_0[] =  {  0.6347,
                        -0.0755,
                        26.5383,
                         0.0917,
                         0.7056,
                        10.9131 };

 double  Affin_1[] =  {  1.5516,
                         0.1660,
                       -42.9876,
                        -0.2016,
                         1.3957,
                        -9.8797 };

 int Bild_1[] = {   87,
  93,  77,  67,  84,  76,  61,  67,  49,  37,  65,
  73, 109, 107, 131, 115, 110, 113, 130, 144, 138,
 155, 149, 151, 132, 131, 157, 170, 157, 147, 134,
 125, 127,  99,  77,  74,  87, 104,  86,  55,  58,
  64,  77,  71,  48,  84,  85,  91, 103,  86,  87,
  91, 102,  96,  89,  93, 128, 170, 175, 179, 187,
  82,  99,  92,  66,  81,  88,  70,  50,  23,  55,
  65,  92, 132, 141, 142, 133, 106,  87, 103, 113,
 115, 132, 154, 154, 119, 123, 147, 168, 170, 148,
 106,  89,  98,  84,  76,  79,  80,  91,  81,  70,
  68,  51,  79,  73,  52,  78,  91,  87,  89, 104,
  97, 110, 100, 107, 106, 109, 129, 175, 183, 179,
 182,  81,  90,  87,  79,  83,  58,  41,  37,  33,
  54,  85, 112, 154, 145, 131, 121,  84,  79, 107,
 121,  97, 103, 135, 147, 120, 108, 132, 148, 160,
 144, 106, 100, 101,  98,  82,  59,  71,  83,  76,
  81,  59,  66,  91,  65,  68,  85,  96, 108, 117,
 135, 121, 134, 125, 127, 109, 112, 108, 146, 175,
 169, 171,  87,  83,  71,  67,  61,  52,  49,  57,
  45,  72,  93, 127, 167, 166, 145, 119,  71,  86,
 109, 126, 108, 103, 108, 104, 100,  98, 108, 136,
 141, 128, 138, 140, 129, 102,  75,  61,  62,  64,
  81,  81,  51,  65,  91,  59,  34,  66, 105, 121,
 130, 132, 140, 154, 161, 163, 144, 144, 140, 147,
 139, 126, 151,  86,  79,  75,  57,  51,  71,  70,
  73,  67,  75,  48,  88, 147, 175, 161, 130, 120,
 121, 119, 128, 112,  90,  83,  92, 101, 110, 107,
 116, 132, 130, 134, 135, 125,  91,  67,  59,  75,
  62,  88,  77,  49,  55,  53,  43,  23,  39,  83,
  88, 121, 117, 142, 141, 161, 185, 179, 183, 188,
 189, 166, 141, 133,  71,  70,  63,  58,  61,  75,
  80,  73,  85,  79,  54,  63, 132, 156, 177, 170,
 139, 122, 122, 116,  92,  97, 100, 118, 108, 114,
 109, 106, 118, 121, 102, 108,  99,  83,  77,  64,
  87,  72,  79,  60,  52,  59,  54,  43,  30,  47,
  39,  56,  79,  95, 112, 113, 140, 155, 173, 189,
 216, 219, 201, 163, 156,  65,  52,  50,  77,  65,
  65,  79,  87,  83,  71,  60,  68, 114, 144, 162,
 176, 141,  98, 128, 133, 111, 103,  98, 107, 106,
 103,  85,  89, 107, 110, 104, 117, 107,  85,  80,
  82,  91,  71,  79,  73,  77,  88,  80,  80,  58,
  35,  44,  74,  79,  91, 100, 137, 147, 141, 146,
 174, 196, 207, 204, 180, 165,  52,  54,  68,  84,
  73,  73,  91, 100, 105,  84,  75,  85,  94, 116,
 130, 143, 137, 121, 136, 137, 109, 103, 132, 115,
  97,  99,  95, 105, 116, 121, 110,  91,  94,  97,
  89,  90,  94,  53,  57,  86,  90,  79,  66,  67,
  51,  47,  57,  68, 101, 118, 112, 118, 144, 165,
 157, 165, 190, 188, 187, 161, 158,  69,  89,  65,
  64,  72,  80,  82,  90, 117, 106,  68,  63,  67,
 103, 141, 152, 133, 128, 140, 133, 116, 112, 124,
 111, 104,  92,  95, 118, 130, 134, 118,  91,  92,
 110,  88,  95, 106,  96,  58,  92,  82,  73,  70,
  62,  63,  75,  73,  67,  91, 107, 105, 117, 134,
 156, 173, 178, 175, 166, 161, 155, 172,  88,  92,
  77,  65,  70,  85,  85,  73, 101,  85,  63,  46,
  74, 119, 152, 170, 166, 131, 141, 140, 135, 134,
 126, 111, 103,  82,  88, 104, 125, 116,  93,  86,
  97, 101,  81, 111, 112, 128,  96,  90,  74,  77,
  78,  75,  76,  76,  84,  68,  77, 104, 106, 124,
 145, 167, 184, 194, 173, 125, 119, 147, 162,  73,
  85,  82,  70,  73,  85,  70,  77,  76,  77,  65,
  61,  80,  97, 128, 158, 183, 154, 155, 157, 146,
 113,  90,  92, 100,  95,  97, 113, 129, 111,  86,
 101,  93, 106,  87, 123, 138, 134, 121,  95,  95,
  94,  85,  68,  76,  81,  83,  64,  73,  87, 105,
 104, 114, 147, 197, 193, 139,  81,  97, 145, 163,
  71,  64,  73,  87,  75,  76,  68,  72,  79, 106,
 115,  88,  85,  83, 118, 176, 180, 186, 165, 155,
 146, 120,  75,  90, 102,  87,  88, 117, 120, 106,
 103, 121, 115, 107, 116, 124, 117, 128, 105,  93,
 105,  93,  74,  70,  81,  84,  84,  60,  75,  78,
  93, 108, 114, 127, 142, 113,  74,  77,  84, 133,
 173,  71,  61,  71,  85,  76,  68,  77,  55,  74,
 106, 112,  66,  68,  79, 104, 155, 170, 176, 163,
 165, 134, 119,  97, 100, 110,  97,  83,  77,  99,
 108, 112, 107, 117, 122, 119,  94, 117, 132, 125,
  95,  77,  70,  73,  86,  88,  93,  95,  50,  75,
  74,  78, 101, 103, 101,  71,  84,  65,  78,  82,
 125, 177,  61,  74,  69,  75,  68,  58,  68,  64,
  86,  70,  84,  66,  61,  54,  66, 106, 160, 144,
 131, 124, 126, 116, 121, 107, 107, 109, 101,  80,
  87, 106, 109, 100, 102, 136, 134, 110, 107, 118,
 109,  98,  63,  56,  78, 102, 117,  96,  74,  44,
  61,  79,  87,  91,  73,  42,  47,  74,  79,  83,
  79, 116, 178,  83,  78,  70,  72,  72,  54,  64,
  74,  93,  74,  68,  85,  88,  76,  72,  80, 122,
 117, 116, 118, 125,  92, 105, 101, 107, 118,  97,
  99,  97, 113, 105,  89, 102, 124, 127, 120, 127,
 117, 128,  92,  58,  70, 106, 103, 105, 102,  86,
  70,  77,  78,  70,  63,  43,  25,  49,  73,  83,
  88,  85, 140, 183,  81,  83,  65,  58,  64,  63,
  81,  90,  71,  88,  67,  68,  80,  83,  70,  45,
 106, 123, 129, 138, 144, 105, 103, 107, 113, 106,
  92,  86,  96, 109, 109, 107, 132, 142, 126, 123,
 132, 120, 131,  88,  62,  79, 108,  95,  84,  83,
  81,  76,  60,  54,  36,  37,  29,  25,  65,  71,
  75,  91,  88, 124, 176,  57,  58,  59,  51,  59,
  73,  80,  91,  65,  72,  54,  58,  76,  70,  55,
  58,  74,  91, 142, 143, 139, 132, 106,  92,  97,
  89,  80,  93,  98, 124, 112, 123, 139, 149, 132,
 110, 125, 114, 122,  91,  82,  64, 100, 100,  94,
  85,  83,  69,  42,  42,  21,  19,  20,  31,  53,
  55,  85,  96,  94, 105, 165,  50,  54,  61,  62,
  67,  82,  92,  77,  62,  71,  62,  71,  86,  83,
  67,  62,  65,  75, 118, 128, 125, 123, 119, 102,
  96,  78,  82,  87,  85,  97,  85,  99, 141, 129,
 125, 108, 118, 106, 102,  91,  93,  78, 109, 107,
  85,  89,  92,  46,  17,  18,  11,   6,  15,  32,
  59,  70,  99,  99,  77,  40,  72,  63,  69,  60,
  70,  56,  85,  91,  79,  82,  82,  86,  88,  78,
  80,  72,  58,  41,  49, 106, 121, 112, 109, 128,
 121, 111, 101, 112, 120, 106,  93,  88,  83, 111,
 113, 113, 105,  98,  86,  70,  70, 103,  95, 102,
 113, 100,  84,  64,  27,  16,   4,   8,  18,  21,
  36,  59,  77, 102,  80,  46,  23,   7,  64,  53,
  53,  67,  58,  69,  71,  77,  74,  73, 101,  88,
  73,  83,  90,  80,  48,  56, 107, 120, 113, 123,
 111, 107, 105, 125, 124, 112, 108, 108, 109, 112,
 105, 109, 125, 121, 105,  85,  65,  63, 102, 110,
 109, 110,  96,  59,  33,  24,  17,  13,  13,  15,
  39,  50,  74,  73,  79,  63,  31,  16,  14,  83,
  65,  52,  61,  64,  64,  58,  64,  69,  80,  70,
  75,  78,  81,  93,  93,  80,  58,  99, 122, 117,
 135, 114,  99, 103, 122, 127, 115, 102,  89,  89,
 122, 110, 112, 106, 116, 108,  75,  59,  90, 120,
 128, 109,  80,  52,  31,  20,  18,  19,  18,  15,
  22,  60,  70,  80,  81,  73,  45,  24,  20,  12,
  88,  77,  64,  55,  67,  66,  80,  80,  86,  96,
  88,  88,  75,  79,  92,  93,  88,  71,  66,  99,
 131, 137, 102, 107, 111, 100, 117, 130, 110,  96,
 105, 134, 126, 121, 101, 102,  87,  77,  80, 123,
 125,  97,  62,  30,  17,  16,  12,  10,  13,  17,
  12,  23,  67,  74,  80,  64,  38,  28,  21,  16,
  13,  77,  87, 101,  83,  67,  73,  79,  89,  84,
  88,  88,  76,  65,  76,  82,  97,  92,  79,  68,
  80, 106, 145, 131, 137, 121, 101, 104, 105, 108,
 106, 110, 122, 105, 111,  86,  70,  82,  74,  85,
 120,  80,  37,  21,  13,  16,  16,  14,   4,   5,
   6,   5,  37,  62,  73,  81,  48,  16,  15,  12,
  17,  14,  78, 118, 128, 130,  95,  86,  84,  80,
  70,  75,  74,  66,  73,  80,  86,  89,  83,  78,
  78,  82, 117, 152, 125, 121, 129, 117, 102, 103,
 114, 106, 105, 108, 115, 107,  88,  79,  82,  68,
  71,  88,  52,  15,   5,  19,  21,  14,  13,  13,
   5,  10,  19,  56,  77,  78,  61,  19,   9,  21,
  17,  21,  19,  96, 131, 111, 124, 114,  88,  85,
  70,  74,  77,  71,  78,  77,  89,  81,  89,  76,
  85,  91,  91, 101, 117, 120, 123, 132, 108,  90,
 106, 110, 109, 103,  94,  99, 103,  87,  63,  71,
  75,  73,  54,  44,  28,  24,  15,  15,  17,  17,
  20,  16,  29,  39,  69,  92,  84,  38,  20,   5,
  12,  17,  16,  16,  85, 123,  99, 101,  98,  97,
 104,  90,  82,  69,  85,  86,  85,  86,  79,  87,
  66,  84,  97,  87,  86,  70,  90, 117, 124,  92,
  87, 106, 106, 106, 109, 103, 115,  99,  77,  70,
  91,  99,  61,  33,  23,  16,  21,  20,  19,  25,
  15,  15,  22,  40,  43,  53,  70,  66,  22,  14,
  13,   8,   7,  10,  20,  57,  73, 108, 106,  94,
  98, 104,  96,  91,  85,  89,  97,  96,  89,  93,
  86,  85,  94,  79,  70,  57,  56,  94, 121, 128,
  92,  88, 101, 104, 101,  96, 103, 120,  99,  85,
  84,  97,  92,  42,  25,  22,  12,  17,  28,  23,
  22,   1,  12,  23,  40,  47,  53,  57,  32,  10,
  17,  19,  19,  15,  12,  11,  42,  37,  57,  84,
 108, 110,  99, 107, 112, 112,  93,  95, 108,  86,
  84, 100,  84,  86,  61,  60,  51,  64,  99, 112,
 133, 104,  83, 112, 113, 113, 105, 105, 123,  97,
  77,  63,  60,  51,  27,  21,  13,  17,  16,  21,
  31,  19,   7,  11,  23,  43,  60,  64,  40,  23,
  17,  18,  18,  16,   8,  16,  14,  62,  62,  84,
 121, 132, 118, 107, 121, 109,  90,  91, 103,  99,
  77,  73,  87,  85,  82,  64,  46,  66,  76,  78,
  87, 106, 109, 110, 107,  99, 119, 124, 110, 104,
  77,  61,  77,  49,  30,  22,  18,  22,  20,  25,
  15,  12,   9,  11,  16,  27,  49,  70,  57,  24,
  14,  16,  17,  17,   7,  17,  20,  16,  83,  95,
 106, 105, 108, 112, 108, 102, 106,  84,  92,  96,
  89,  67,  75,  91,  80,  49,  34,  40,  69, 105,
  88,  68,  89, 116, 116, 112, 103, 102, 134, 119,
  88,  70,  84,  69,  37,  28,  21,  20,  24,  15,
  19,  19,  14,  17,  23,  14,  25,  49,  48,  30,
  15,  11,   9,  13,  18,   3,  11,  26,  18,  97,
  85,  83,  82,  86,  94, 102, 109, 110,  88,  90,
  85,  96,  77,  84,  93,  79,  53,  33,  38,  51,
 104,  94,  84,  74,  92,  90,  92,  82,  82, 106,
 125, 105,  55,  64,  54,  36,  18,  19,  23,  14,
  16,  16,   8,   5,   6,  26,  23,  42,  53,  35,
  19,  11,  19,  14,  10,  15,   8,  22,  20,  16,
 112,  96,  80,  66,  66,  62,  81, 107, 115, 102,
 104,  99, 103,  85,  93,  97,  69,  54,  36,  45,
  81, 114, 110, 111, 107,  99, 100, 106, 121, 113,
  99, 108,  64,  46,  38,  23,  23,  21,  19,  27,
  22,  20,  14,   9,   6,   4,  26,  50,  54,  43,
  32,  15,  16,  20,  13,  10,  13,   7,  14,  11,
  20,  95,  87,  75,  68,  62,  71,  93, 106, 117,
 109, 126, 108, 118, 105,  92,  75,  50,  43,  45,
 103, 174, 183, 163, 156, 142, 122,  90, 101, 147,
 139, 130, 107,  48,  29,  17,  20,  25,  19,  25,
  28,  15,  17,  17,  12,   9,   8,  21,  45,  53,
  29,  16,  10,   9,  12,  15,  17,   9,  11,  19,
  19,  14,  94,  86,  76,  79,  65,  75, 100, 101,
 101, 110, 129, 120, 103, 103,  87,  77,  57,  38,
  44, 151, 230, 239, 227, 208, 179, 164, 138, 128,
 152, 165, 178, 108,  48,  29,  31,  27,  34,  21,
  27,  27,  17,  17,  17,  19,  16,  12,  17,  37,
  35,  17,  16,  17,  20,  15,  18,  12,   7,  10,
  17,  14,  10,  75,  77,  97,  95,  57,  78,  92,
  91,  83,  89, 123, 121,  97,  97,  75,  83,  62,
  46,  71, 200, 244, 246, 244, 244, 236, 221, 213,
 210, 214, 228, 218, 128,  53,  39,  30,  30,  30,
  20,  19,  20,  14,  22,  24,  15,  13,  16,  18,
  26,  20,   6,  14,  14,  23,  14,  19,   7,  10,
  15,   7,  10,  15,  81,  72,  84,  82,  81,  86,
  86,  84,  75,  97, 113, 122, 104,  94,  73,  59,
  48,  43, 128, 225, 242, 243, 244, 249, 245, 244,
 244, 248, 247, 246, 231, 128,  45,  24,  28,  22,
  20,  16,  19,  11,  13,  13,  14,  11,  14,  14,
  29,  27,  32,  27,  21,  11,   7,  16,  17,  12,
  10,   8,  10,  14,  14,  65,  58,  82,  89,  81,
  72,  82,  88,  80,  85,  94, 116, 116,  93,  82,
  48,  41,  63, 184, 241, 240, 245, 247, 250, 249,
 250, 246, 247, 249, 246, 216, 113,  59,  55,  40,
  26,  23,  14,  18,  16,  23,  12,  13,  16,  18,
  18,  19,  23,  33,  30,  28,  23,  22,  23,  16,
   5,   8,   8,  16,  20,  18,  69,  85, 101,  82,
  89,  91,  91,  81,  81,  94,  82,  92,  99,  84,
  64,  35,  40,  96, 223, 246, 245, 249, 249, 250,
 251, 250, 246, 243, 243, 226, 148,  78,  61,  79,
  58,  49,  39,  23,  22,  32,  25,  20,  17,  21,
  16,  17,  17,  16,  28,  31,  22,  25,  25,  29,
  25,  19,  19,  19,  18,  18,  11,  68,  88, 102,
  88,  94,  88,  79,  74,  75,  85,  87,  90,  92,
  82,  53,  24,  41, 141, 241, 246, 249, 248, 246,
 248, 247, 231, 200, 199, 212, 152,  87,  65,  63,
  70,  50,  53,  51,  45,  34,  25,  17,  24,  19,
  27,  19,  16,  22,  15,  21,  22,  20,  20,  15,
  30,  37,  27,  41,  30,  12,  13,  13,  70,  62,
  80,  80,  88,  84,  74,  78,  86,  67,  65,  75,
  84,  66,  45,  37,  77, 197, 244, 246, 247, 247,
 245, 246, 242, 212, 159, 119, 134,  94,  61,  65,
  71,  56,  41,  57,  51,  54,  47,  28,  15,  20,
  22,  22,  15,  15,  17,   9,  10,  12,  19,  21,
  20,  28,  29,  24,  35,  31,  28,  26,  20,  79,
  75,  97,  92,  84,  84,  79,  70,  79,  67,  63,
  73,  63,  48,  44,  47, 118, 219, 245, 244, 247,
 247, 247, 239, 204, 168, 167, 124, 125, 104,  73,
  64,  72,  64,  59,  59,  52,  60,  39,  33,  32,
  33,  27,  22,  15,  10,   9,  16,  19,  15,  12,
   9,  17,  25,  35,  26,  17,  17,  26,  31,  23,
  64,  80,  96,  81,  92,  87,  81,  66,  73,  82,
  56,  63,  57,  57,  40,  61, 174, 229, 240, 241,
 245, 244, 235, 195, 112, 121, 166, 134, 132, 117,
 127, 105,  93,  80,  68,  54,  49,  48,  40,  39,
  37,  40,  33,  28,  17,  17,  18,  21,  22,  17,
  20,  17,  10,  19,  31,  21,  14,  14,  20,  17,
  10,  70,  71,  76,  72,  88,  83,  68,  67,  67,
  77,  81,  71,  72,  48,  38,  90, 209, 241, 240,
 243, 234, 218, 170, 127,  62,  95, 173, 141, 127,
 118, 121, 120, 127, 123, 111,  79,  67,  56,  57,
  63,  45,  45,  42,  41,  23,  31,  27,  14,  19,
  19,  17,  19,  14,  13,  20,  15,   7,  10,  24,
  35,  34,  80,  67,  87,  88,  93,  74,  73,  81,
  73,  59,  78,  78,  60,  50,  52, 129, 230, 244,
 238, 229, 161, 111, 116, 106,  37,  90, 184, 151,
 127, 120, 118, 127, 138, 134, 135, 113, 114, 110,
  84,  67,  53,  48,  43,  34,  29,  37,  29,  16,
  18,  12,   9,   5,  14,  10,  10,  15,   9,  15,
  24,  22,  25,  77,  72,  92,  94,  73,  72,  78,
  82,  85,  71,  84,  83,  61,  60,  84, 165, 232,
 236, 225, 183,  89,  79,  94,  81,  31,  92, 199,
 154, 126, 136, 128, 138, 134, 134, 130, 132, 129,
 139, 116, 104,  86,  83,  55,  42,  30,  33,  36,
  32,  20,  13,  17,  24,  16,  12,  12,  14,  10,
  13,  24,  17,  15,  70,  80,  88,  75,  64,  67,
  76,  78,  75,  83, 114,  82,  47,  41, 109, 208,
 230, 217, 170,  96,  61,  71,  76,  40,  23, 100,
 201, 155, 138, 140, 136, 124, 131, 129, 112, 128,
 121, 135, 124, 130, 110, 111, 101,  81,  58,  56,
  48,  40,  28,  29,  30,  48,  20,  10,  12,  19,
  11,  12,  21,  19,  10,  69,  78,  95,  63,  80,
  86,  82,  69,  60,  88,  82,  42,  46,  46, 142,
 221, 212, 157,  88,  43,  44,  67,  57,  22,  27,
  84, 163, 157, 137, 151, 142, 129, 135, 130, 117,
 121, 129, 134, 137, 126, 124, 129, 126, 121, 117,
  92,  64,  52,  43,  40,  20,  17,  20,  16,   6,
  12,   9,  10,  10,   6,   9,  65,  72,  90,  71,
  87,  85,  77,  62,  73,  92,  46,  22,  44,  73,
 175, 196, 133,  55,  38,  40,  39,  81,  55,  20,
  17,  43, 107, 149, 143, 142, 152, 135, 148, 140,
 119, 130, 140, 132, 144, 133, 129, 128, 135, 133,
 124, 112, 100,  91,  91,  57,  21,  20,  20,  10,
   5,   7,   9,  10,   9,   9,   8,  63,  58,  82,
  76,  81,  69,  79,  71,  68,  82,  54,  43,  40,
 101, 175, 136,  73,  38,  39,  39,  52,  74,  31,
  23,  23,  45,  97, 155, 148, 138, 147, 137, 146,
 143, 136, 133, 133, 132, 135, 133, 132, 145, 137,
 117, 115, 116, 127, 123, 112, 122,  98,  74,  30,
  15,  17,  18,  14,  17,  16,  14,  14,  80,  79,
  86,  85,  84,  79,  82,  89,  92,  91,  57,  46,
  41,  84, 106,  78,  57,  41,  24,  36,  50,  47,
  25,  28,  19,  45,  74, 144, 148, 136, 147, 132,
 151, 163, 143, 125, 135, 127, 148, 136, 141, 132,
 127,  92,  85, 106, 115,  99, 121, 129, 130, 120,
  88,  68,  53,  41,  31,  25,  22,  10,   9,  77,
  82,  87,  97,  80,  85,  87,  73,  94,  87,  60,
  40,  46,  68,  56,  33,  29,  23,  19,  30,  55,
  36,  19,  15,  19,  19,  54, 102, 130, 127, 141,
 129, 151, 162, 148, 126, 124, 118, 145, 135, 128,
 128, 129, 123, 106, 126,  88,  90, 122, 114, 119,
 114, 115, 110, 108, 111,  68,  42,  24,  17,  12,
  71,  68,  73,  79,  74,  73,  70,  77,  91,  69,
  42,  25,  34,  49,  37,  29,  28,  29,  36,  47,
  64,  18,  13,  27,  20,  18,  21,  29,  67,  85,
  96, 113, 148, 152, 143, 116, 117, 130, 144, 131,
 131, 123, 132, 142, 125, 125, 110, 113, 125, 124,
 111, 119, 123, 129, 124, 129, 121, 105,  85,  60,
  24,  60,  68,  78,  69,  86,  75,  70,  80,  82,
  33,  18,  21,  32,  38,  25,  40,  36,  43,  44,
  62,  51,  15,  14,  21,  31,  22,  14,   7,  11,
  15,  38,  61, 116, 135, 138, 127, 139, 137, 142,
 145, 158, 139, 151, 157, 135, 137, 120, 119, 145,
 134, 124, 128, 110, 127, 162, 135, 136, 134, 105,
 109, 100,  60,  74,  87,  75,  70,  64,  70,  76,
  56,  20,  23,  16,  24,  29,  24,  37,  20,  38,
  40,  54,  32,  14,  11,  24,  17,  17,  16,  10,
  17,   6,  12,  20,  44,  82, 123, 124, 122, 126,
 129, 135, 133, 108, 145, 172, 149, 137, 134, 122,
 142, 132, 130, 117, 117, 128, 141, 129, 103, 108,
 114, 112, 110,  52,  55,  72,  75,  66,  57,  55,
  51,  22,  30,  23,  14,  25,  42,  30,  38,  19,
  29,  42,  59,  37,  16,  21,  30,  18,  11,  10,
  10,  17,  17,  13,  12,  18,  23,  40,  61,  80,
  87, 113, 103,  68,  77,  91,  85, 119, 140, 137,
 121, 143, 142, 132, 126, 128, 129, 118, 108,  85,
  64,  71,  83, 108, 0 };

 int Bild_2[] = {   57,
  75,  91,  91, 113, 136, 140, 154, 165, 163, 129,
 116,  99,  93,  89,  78,  86,  84,  84,  89,  56,
  68, 105, 126, 128, 135, 119,  98,  90,  85,  92,
  96, 109,  87,  66,  57,  72,  90,  84, 121, 156,
 150, 178, 181, 179, 175, 158, 132, 135,  90,  71,
  49,  54,  56,  67,  90,  55,  26,  57, 171, 199,
 193, 194, 172, 175, 163, 145, 141, 136, 184, 229,
  81,  57,  81,  85,  88,  98, 110, 140, 155, 150,
 127, 109, 113, 134, 108,  97, 109,  99, 105,  88,
  88,  89,  90, 104, 110,  93, 103, 110, 114,  94,
  99, 121, 144,  87,  47,  56,  77, 116, 131, 138,
 190, 147, 154, 135, 141, 149, 171, 134, 125,  90,
  47,  39,  48,  39,  45,  68,  47,  25,  54, 105,
 141, 166, 173, 187, 198, 183, 169, 163, 155, 203,
 210, 128,  89,  97,  93,  80,  99, 115, 102, 111,
 121, 131, 128, 135, 165, 155, 117, 126, 114, 101,
  96, 106, 108, 100,  99,  93,  78,  92, 100, 106,
  90, 107, 126, 132,  92,  46,  75, 123, 161, 162,
 170, 174, 163, 162, 154, 146, 125, 129, 145, 110,
  95,  66,  39,  41,  49,  44,  52,  35,  21,  55,
  98, 107, 117, 116, 118, 126, 128, 152, 154, 166,
 208, 203, 148, 164, 113,  94,  80,  94,  99,  98,
 100, 111, 135, 138, 136, 142, 181, 162, 147, 114,
 105, 121, 123, 115, 110,  90,  93,  89,  83, 103,
 115, 115, 123, 137, 149, 135, 105,  92,  93, 119,
 159, 186, 167, 155, 156, 164, 162, 149, 154, 142,
 122,  91,  83,  76,  54,  62,  49,  45,  25,  25,
  77, 104, 107, 108,  97, 112, 125, 103, 109, 109,
 123, 139, 136, 132, 175, 146,  91,  78,  67,  62,
  84, 105, 115, 111, 118, 119, 144, 166, 156, 129,
 132, 139, 142, 130, 134, 107,  96,  95,  93,  88,
  91,  97,  96, 127, 140, 138, 141, 145, 105,  99,
 106, 161, 179, 149, 146, 136, 138, 134, 134, 144,
 127,  99,  92,  91,  93,  79,  73,  66,  43,  16,
  21,  77, 105, 113, 110, 112,  98,  95, 113, 111,
  97, 100, 117, 126, 153, 134, 191, 131,  55,  82,
 107, 112,  94, 112, 126, 120, 125, 131, 146, 151,
 145, 154, 149, 159, 140, 130, 102, 116, 121, 110,
 110,  88,  82,  94, 125, 158, 147, 126, 103,  82,
  82, 115, 161, 149, 121, 163, 140, 127, 135, 133,
 145, 118, 100, 107, 106, 110, 102,  83,  78,  62,
  34,  32,  93, 107, 104, 101, 105, 112, 107, 118,
 109, 108, 108, 118, 123, 189, 165, 165, 164,  84,
  85, 119, 114,  97,  91, 104, 110, 132, 125, 121,
 117, 147, 145, 153, 171, 153, 150, 138, 140, 143,
 130, 129, 108,  84,  87, 116, 143, 155, 133, 116,
  72,  76, 111, 113,  98, 115, 134, 111, 115, 132,
 146, 178, 141, 113, 116, 107, 102,  88,  85,  71,
  64,  51,  70,  95,  99, 110, 110, 108, 102,  97,
 112, 111, 104, 115, 112, 126, 197, 197, 162, 131,
 105,  76,  74,  89,  90,  85,  88, 104, 105, 116,
 135, 119, 120, 138, 146, 154, 154, 173, 146, 152,
 148, 150, 137, 136, 122, 111,  83,  76, 107, 141,
 137,  96,  65,  73,  67,  74,  93, 117, 104,  87,
 117, 130, 142, 140, 124, 122, 102,  98,  88,  90,
  68,  59,  56,  80, 107, 100,  94,  90,  88, 104,
 102, 102, 114, 110, 115, 118, 133, 183, 204, 211,
 159, 159, 126,  59,  93, 112,  95,  87,  87,  97,
  96, 108, 124, 118, 100, 123, 148, 175, 170, 159,
 158, 162, 147, 130, 125, 138, 123, 113,  89,  70,
  95, 103,  77,  53,  63,  64,  82,  92, 104, 105,
 101, 117, 125, 110, 103, 114, 125,  90,  91,  94,
  76,  69,  61,  48,  86, 119, 129, 120, 109,  96,
 108,  87,  99, 109, 103, 102, 107, 139, 175, 178,
 233, 217, 148, 139,  54,  67, 109,  92,  88,  90,
  89,  91, 105, 119, 138, 128, 107, 118, 164, 174,
 179, 185, 175, 168, 141, 119, 122, 138, 143, 137,
  88,  82,  71,  81,  77, 104,  92,  94, 110, 101,
 112, 117, 122, 107,  87,  85,  95, 113,  91,  79,
  87,  80,  62,  50,  60, 115, 134, 140, 133, 127,
 126, 147, 128, 132, 133, 123, 106, 107, 135, 182,
 159, 228, 231, 167, 139, 115,  60,  88, 104,  88,
  88,  66,  85, 100, 111, 149, 158, 123, 120, 125,
 147, 168, 172, 162, 172, 168, 146, 129, 142, 139,
 154, 146, 123, 100,  77,  75, 104,  91, 102, 105,
  84,  65,  86,  91,  73,  79,  87,  88,  77,  79,
  80,  98,  81,  52,  50,  80, 125, 137, 153, 144,
 127, 146, 144, 153, 145, 137, 141, 138, 128, 141,
  96, 158, 207, 217, 196, 117, 140,  85,  64,  78,
  68,  70,  72, 112, 107, 114, 117, 129, 118, 121,
 136, 122, 123, 142, 155, 168, 171, 153, 135, 130,
 115, 147, 164, 166, 146,  95,  83,  97,  84, 109,
 100,  75,  58,  51,  48,  69,  81,  85,  76,  71,
  70,  72,  62,  57,  46,  39,  81, 118, 126, 128,
 142, 130, 134, 147, 146, 147, 139, 147, 136, 136,
 143, 126, 195, 203, 200, 188, 137,  98, 110,  59,
  72, 123,  88, 104, 121, 119, 126, 112, 120, 132,
 125, 124, 110, 105, 122, 134, 152, 160, 172, 167,
 139, 123, 131, 171, 200, 189, 161, 126, 107,  85,
  84,  86,  71,  50,  33,  60,  62,  87,  92,  69,
  52,  53,  55,  67,  52,  56,  52,  74, 101, 117,
 118, 124, 127, 133, 138, 156, 131, 131, 136, 138,
 137, 137, 198, 209, 189, 187, 183, 159,  99,  94,
  76,  70, 110,  91, 103, 120, 132, 137, 112, 119,
 123, 130, 128, 120, 126, 125, 148, 147, 146, 169,
 195, 183, 180, 156, 180, 164, 177, 193, 161, 155,
 114,  58,  50,  60,  57,  61,  71,  61,  72,  66,
  53,  49,  38,  67,  73,  65,  58,  48,  62,  70,
  89,  97, 117, 136, 140, 137, 134, 136, 125, 114,
 119, 136, 159, 195, 188, 180, 180, 183, 122,  81,
  75,  77,  71,  91, 101, 118, 115, 121, 125, 108,
 111, 106, 135, 135, 119, 136, 141, 148, 153, 146,
 174, 171, 171, 192, 187, 179, 141, 133, 166, 176,
 173, 128,  74,  43,  43,  37,  50,  63,  59,  55,
  60,  46,  48,  63,  76,  64,  63,  52,  38,  42,
  50,  44,  29,  40,  68,  80, 100, 110, 139, 140,
 128, 123, 150, 171, 187, 168, 139, 114,  74,  64,
  90,  90,  83,  76,  86, 101, 129, 117, 114, 108,
 104, 105, 126, 146, 138, 129, 128, 138, 139, 129,
 133, 158, 167, 156, 180, 184, 184, 171, 151, 126,
 147, 170, 103,  63,  51,  57,  36,  35,  36,  49,
  47,  48,  39,  73,  97,  98,  75,  54,  51,  40,
  56,  49,  36,  19,   7,  15,  14,  17,  29,  63,
  77, 114, 127, 166, 168, 157,  99,  59,  59,  82,
  82,  94,  81,  77,  77,  76,  69,  92, 118, 119,
 126,  89, 103, 125, 141, 130, 164, 137, 125, 121,
 105, 123, 143, 161, 157, 151, 162, 194, 201, 181,
 142, 113, 111,  91,  64,  90, 103,  78,  58,  36,
  38,  24,  35, 100, 107, 101,  89,  85,  52,  51,
  40,  56,  52,  40,  23,  16,  17,  15,  15,  10,
  12,  18,  66, 124, 163, 160,  53,  49,  71,  72,
  99,  87,  80,  77,  91,  75,  63,  56,  70, 108,
 140, 137,  97, 100, 104, 103, 100, 138, 131, 148,
 140, 127, 130, 129, 156, 161, 154, 160, 160, 181,
 178, 150,  98,  75,  70,  64,  76, 131, 100,  63,
  50,  34,  34,  49,  88,  80,  79,  81,  83,  77,
  68,  58,  66,  44,  51,  43,  18,  21,  32,  20,
  13,  14,  39, 116, 140, 134, 151,  63,  75,  84,
  83,  91,  76,  74,  71,  75,  77,  76,  76,  78,
 105, 141, 136,  92, 101, 108, 107, 106, 114, 131,
 141, 128, 137, 133, 135, 140, 149, 147, 145, 160,
 163, 154, 140, 111,  81,  70,  55,  62, 104, 102,
  64,  46,  60,  98, 106,  88,  84,  69,  67,  91,
 102, 103,  89, 100,  67,  53,  44,  21,  19,  20,
  15,  18,  47, 112, 153, 133, 158, 172,  76,  72,
  90,  94,  96,  83,  90,  88,  76,  78,  81,  92,
  83,  88, 127, 160, 127,  87, 110, 128, 130, 138,
 131, 132, 130, 142, 134, 121, 132, 148, 142, 118,
 135, 141, 128, 109,  99,  71,  67,  55,  69,  65,
  58,  44,  41,  84, 108, 113, 116, 100,  86,  89,
 101, 101, 113, 103, 118, 116,  91,  73,  56,  61,
  48,  23,  43,  94, 131, 141, 153, 170, 182,  78,
  74,  87,  96,  88,  83,  79,  90,  70,  73,  86,
  84,  72,  70, 119, 179, 148, 125, 128, 130, 124,
 126, 110, 128, 149, 139, 150, 136, 144, 150, 137,
 130, 120, 127, 133,  94, 100,  70,  91,  76,  80,
  62,  55,  52,  40,  67,  98,  93, 108, 119, 112,
 112, 129, 134, 137, 129, 140, 118, 104, 102,  83,
 104, 116,  60,  80,  95, 115, 141, 161, 177, 199,
  78,  78,  85,  70,  64,  80,  71,  73,  83,  69,
  70,  65,  63,  55,  80, 127, 154, 140, 157, 148,
 129, 117, 114, 142, 149, 134, 147, 153, 146, 151,
 157, 145, 131, 103, 110,  91,  87,  70,  91,  66,
  72,  94,  91,  75,  64,  80,  94, 101, 114, 121,
 133, 145, 164, 161, 178, 191, 184, 174, 164, 147,
 145, 147, 142, 139, 137, 147, 160, 170, 180, 186,
 194,  72,  73,  72,  74,  83,  86,  98,  84,  87,
  75,  85,  78,  77,  57,  72,  82,  99, 123, 138,
 116,  86,  90, 127, 158, 149, 157, 163, 148, 156,
 143, 138, 133, 135, 111, 104,  90,  89,  54,  65,
  58,  75,  75,  61,  75,  92,  93,  91,  95,  96,
  90, 117, 172, 179, 183, 194, 204, 209, 212, 197,
 193, 180, 179, 178, 177, 182, 183, 187, 180, 184,
 191, 193,  82,  77,  71,  78, 101,  93,  78,  85,
  87,  78,  82,  65,  84,  87,  64,  49,  84, 125,
 107,  93,  92,  91, 132, 140, 135, 155, 151, 144,
 145, 142, 136, 144, 128, 129, 102,  80,  89,  86,
  64,  58,  70,  72,  59,  86,  89,  96,  99, 113,
 113, 108, 112, 144, 172, 164, 179, 189, 195, 204,
 212, 210, 208, 205, 196, 194, 191, 192, 192, 188,
 200, 212, 184,  93,  88,  91,  85,  91,  87,  77,
  88,  86,  87,  85,  71,  75, 103,  77,  59,  66,
  83,  91, 103, 118, 111, 134, 135, 128, 133, 141,
 148, 136, 154, 165, 150, 125, 112,  91,  77,  80,
  89,  76,  65,  68,  73,  69,  90, 110, 126, 133,
 150, 160, 150, 154, 164, 154, 137, 142, 167, 180,
 180, 182, 173, 186, 197, 178, 188, 195, 182, 192,
 207, 206, 202, 188,  90,  89,  97,  90,  81,  70,
  87,  85,  94, 103,  84,  80,  65,  84,  76,  64,
  65,  48,  73, 105, 114, 122, 110, 110, 126, 125,
 146, 154, 120, 136, 159, 154, 109, 100, 101,  77,
  63,  70,  80,  60,  66,  66,  31,  60,  92, 121,
 125, 132, 156, 172, 189, 217, 204, 166, 160, 173,
 180, 164, 175, 172, 182, 174, 184, 203, 204, 179,
 181, 193, 193, 198, 201,  79,  75,  74,  82,  77,
  73,  87,  82,  85,  94,  97,  89,  81,  70,  82,
  66,  45,  48,  71, 120, 143, 132,  98,  84, 115,
 100, 111, 122, 105, 105, 133, 135, 134, 136, 109,
  70,  64,  68,  83,  59,  57,  51,  35,  44,  51,
  77,  95, 123, 147, 147, 168, 194, 193, 163, 157,
 162, 171, 184, 196, 200, 184, 175, 194, 207, 206,
 204, 201, 198, 203, 204, 216,  77,  71,  77,  88,
  77,  71,  73,  64,  68,  76,  81,  85,  91,  76,
  73,  45,  41,  52,  90, 139, 165, 142, 100, 101,
 120, 115,  90,  90, 103, 110, 110, 121, 112, 108,
  95,  80,  76,  80,  78,  75,  84,  74,  63,  45,
  60,  90, 115, 113, 135, 161, 175, 174, 153, 150,
 166, 163, 161, 159, 153, 162, 172, 179, 196, 216,
 227, 216, 203, 205, 217, 214, 215,  87,  80,  74,
  78,  67,  68,  62,  75,  72,  78,  90,  87,  78,
  65,  57,  64,  70,  70,  55,  91, 155, 171, 153,
 126, 122, 108, 100, 103, 107, 101,  89, 109, 113,
 102,  96,  93,  91,  88,  62,  89,  79,  70,  64,
  73,  77,  74, 101, 111, 134, 162, 192, 158,  92,
 128, 165, 168, 163, 159, 166, 158, 165, 156, 157,
 175, 180, 185, 185, 195, 213, 231, 228,  92,  86,
  79,  67,  72,  64,  62,  74,  82,  87,  90,  78,
  71,  60,  61,  76,  79,  82,  64,  67, 124, 151,
 159, 123, 133, 127, 105, 128, 106,  96, 101, 125,
 131, 101,  93,  99, 100, 113, 112,  92,  82,  83,
  71,  78,  83,  66,  80,  99, 110, 127, 133,  85,
  78, 103, 168, 173, 181, 158, 172, 183, 184, 176,
 165, 157, 151, 167, 157, 157, 184, 216, 216,  88,
  86,  86,  76,  67,  56,  69,  81,  86,  92,  87,
  64,  53,  67,  72,  75,  96, 106,  83,  75,  83,
 125, 151, 133, 138, 134, 126, 126, 108,  88,  92,
 118, 114,  91,  95, 104, 109, 129, 125,  98, 103,
  83,  74,  85,  90,  54,  75,  78,  98,  82,  58,
  76,  82,  88, 158, 181, 180, 182, 184, 180, 176,
 173, 170, 162, 160, 155, 164, 159, 153, 166, 170,
  85,  84,  83,  70,  72,  71,  77,  78,  74,  69,
  66,  58,  80,  65,  69,  82,  83, 101,  84,  53,
  74, 124, 159, 172, 151, 157, 135,  93,  92,  95,
  90, 118, 109, 107, 115, 117, 113, 110, 129, 108,
  73,  65,  92, 115,  87,  55,  71,  77,  68,  36,
  47,  74,  86,  88, 149, 190, 182, 189, 174, 184,
 175, 179, 180, 184, 185, 170, 177, 177, 161, 170,
 169,  85,  88,  94,  81,  79,  84,  78,  77,  68,
  68,  54,  75,  87,  80,  70,  82,  71,  76,  88,
  86,  83,  91, 150, 178, 176, 161, 135, 105, 100,
 106,  93,  80,  99, 109, 100, 125, 130, 116, 118,
 116,  69,  75, 104,  95,  87,  77,  59,  43,  31,
  25,  47,  58,  90,  92,  94, 136, 175, 184, 174,
 199, 185, 175, 189, 197, 192, 182, 185, 179, 181,
 172, 168,  74,  79,  90,  90,  85,  80,  74,  87,
  70,  58,  66,  71,  69,  71,  84,  74,  73,  65,
  99, 102,  66,  68,  98, 152, 145, 126, 126, 109,
 106, 108, 107,  98, 102, 108, 102, 131, 127, 125,
 122, 124,  82,  69, 100,  96,  86,  74,  32,  22,
   9,  16,  44,  71,  98,  66,  24,  15,  41,  64,
  89, 121, 154, 164, 180, 201, 220, 195, 189, 179,
 183, 173, 171,  77,  88,  93, 107,  84,  73,  63,
  70,  52,  64,  55,  69,  66,  70,  77,  64,  65,
  71,  79,  74,  82,  78,  72,  96, 120, 126, 139,
 104, 104, 108,  94,  89, 100, 117, 119, 141, 132,
 110, 114, 103,  90,  87, 106, 103,  87,  55,  20,
   7,  13,  30,  53,  73,  78,  46,  20,  13,  14,
  24,  16,  23,  24,  39,  50,  77, 151, 170, 177,
 193, 191, 189, 180,  85,  42,  51,  88,  78,  66,
  55,  52,  55,  64,  69,  66,  83,  75,  67,  66,
  69,  88,  77,  69,  67,  77,  60,  64,  87, 138,
 136, 128, 109,  97,  79,  90,  93,  92,  88, 116,
 113, 109,  96,  73,  68, 105, 109, 100,  60,  26,
  19,  17,  17,  47,  72,  77,  53,  29,  18,  13,
  16,  15,  16,  13,  10,  15,  15,  15,  21,  38,
  43,  80, 127, 147, 172,  46,  25,  60,  80,  82,
  57,  54,  67,  65,  70,  76,  65,  67,  64,  55,
  60,  78,  85,  65,  65,  70,  84,  72,  60,  56,
 106, 119, 111, 123, 112, 114, 119, 109, 107, 109,
 108, 112, 115, 105,  68,  93, 120,  92,  43,  19,
  13,   9,  12,   9,  47,  71,  72,  25,  17,  17,
  18,  14,  15,  15,  14,  14,  12,  11,  13,  12,
   7,  13,  16,  17,  20,  44,  96,  57,  70,  96,
  83,  48,  69,  63,  72,  71,  78,  69,  53,  58,
  63,  62,  86,  84,  81,  83,  89,  76,  85,  80,
  53,  92, 120, 124, 115, 101, 115, 124, 113,  95,
 111, 128, 118,  95,  85,  77, 109,  79,  27,  15,
  17,  14,  11,   7,  20,  65,  82,  46,  14,  11,
  16,  16,  13,  14,  14,  10,  10,  11,  12,  11,
  16,  14,  20,  18,  14,  14,  15, 214, 142,  83,
  80,  78,  73,  78,  82,  84,  66,  68,  74,  68,
  57,  56,  62,  67,  67,  70,  78,  73,  77,  85,
  93,  83,  70,  97, 131, 119, 121, 109, 106, 106,
 107, 109, 112, 109,  85,  81,  69,  74,  51,  22,
  17,  15,  18,  18,  23,  40,  57,  68,  27,  15,
  14,  12,  12,  14,  10,   9,  11,  11,  13,  14,
  14,  13,  15,  21,  16,  17,  12,  19, 234, 204,
 117,  83,  82,  65,  66,  77,  86,  69,  65,  65,
  70,  76,  59,  60,  66,  80,  84,  93,  86,  71,
  77,  90,  90,  76,  79, 119, 138, 121, 125, 102,
 105, 110, 104,  96, 102,  82,  74,  91,  55,  26,
  15,  21,  22,  18,   9,  24,  45,  56,  43,  19,
  18,  17,  11,  17,  13,  11,  10,  12,  14,  16,
  15,  15,  15,  17,  13,  19,  14,  17,   9, 220,
 201, 152, 105,  60,  57,  46,  68,  83,  84,  71,
  83,  82,  81, 101,  92,  79,  82,  79,  75,  73,
  72,  84,  85,  80,  85,  91,  96, 100, 114, 122,
  90, 104, 105, 103, 104, 109,  86,  85,  83,  35,
  20,  16,  17,  26,  16,  11,  24,  52,  62,  23,
  14,  15,  15,   9,  25,  15,  14,  19,  16,  19,
  14,  14,  14,  13,  19,  13,  13,  16,  12,  10,
 204, 216, 207, 150,  65,  51,  56,  72,  75,  72,
  77,  85,  61,  94, 121, 125, 104,  88,  77,  75,
  80,  84,  86,  82,  79,  88,  82,  65,  61, 108,
 129,  94, 101, 111, 109, 106, 109,  75,  71,  43,
  23,  19,  20,  22,  16,  15,  21,  23,  50,  34,
  14,  16,  12,  15,  13,  16,  18,  11,  12,  21,
  23,  20,  13,  17,  17,  17,  12,  13,  14,  14,
  16, 206, 221, 207, 172,  79,  59,  54,  62,  76,
  73,  72,  88,  49,  83, 108, 103,  96, 101,  96,
  90,  90,  97,  94,  87,  93,  85,  61,  56,  70,
  87, 104, 109, 110, 101, 116, 121,  88,  76,  68,
  33,  21,  22,  16,  16,   7,   5,  27,  46,  46,
  28,  14,  17,  14,  11,  12,  19,  13,  12,  16,
  16,  14,  12,  17,  15,   8,  17,  18,  11,  14,
  16,  12, 180, 174, 198, 177, 114,  77,  78,  74,
  72,  45,  90,  88,  68,  50,  50,  75, 110, 108,
 111, 108,  93, 102,  90,  74,  88,  71,  42,  50,
  94,  88,  79, 100,  96,  87,  87, 112,  91,  49,
  35,  24,  20,  26,  19,  17,  11,   8,  19,  44,
  32,  16,  16,  16,  17,   9,  12,  11,  13,  11,
  16,  21,  11,  18,  14,   9,  20,  19,  21,  15,
  19,  20,  17, 159, 108, 137, 151, 134,  87,  76,
  79,  80,  60,  84,  86,  88,  73,  73, 102, 115,
 112, 105, 106,  87,  91,  91,  79,  92,  69,  39,
  44,  91, 107, 109, 105,  98, 118, 131, 119,  67,
  28,  22,  29,  22,  27,  17,  18,  18,  14,  17,
  28,  14,  14,  14,  15,  18,  11,   9,  12,  15,
  10,  13,  15,  13,  14,  16,  15,  20,  19,  17,
  11,  15,  19,  16, 130, 109,  82, 128, 129, 108,
  90,  95,  84,  68,  67,  74,  69,  79,  88,  83,
  81,  82,  97, 112, 100, 102, 106,  92,  87,  54,
  44, 100, 188, 191, 181, 159, 134, 137, 173, 186,
  86,  38,  30,  30,  19,  18,  14,  16,  13,  14,
  20,  26,  32,  27,  21,  22,  19,  10,  14,  17,
  16,   9,  12,  13,  15,  14,  15,   8,  10,  16,
  16,  12,  16,  16,  16,  96, 101,  80, 100, 140,
 156, 107,  77,  65,  62,  75,  73,  66,  95,  97,
  77,  66,  68,  96, 109, 109, 123, 109, 101,  81,
  60,  46, 146, 241, 244, 244, 235, 227, 232, 240,
 232, 104,  37,  32,  23,  16,  17,  21,  14,  16,
  17,  17,  17,  28,  26,  23,  22,  32,  26,  36,
  19,  18,  15,  19,  19,  16,  18,  10,  12,  12,
  15,  12,   7,  11,  16,  15,  65,  79,  74,  87,
 151, 177, 138,  71,  54,  84,  87,  75,  67,  84,
  91,  80,  80,  70,  94,  91,  88, 122, 109,  96,
  72,  54,  53, 183, 240, 244, 249, 248, 248, 246,
 245, 201,  88,  67,  60,  47,  34,  28,  24,  22,
  21,  21,  16,  16,  14,  15,  20,  20,  29,  26,
  24,  24,  31,  21,  24,  29,  28,  30,  14,  16,
  15,  12,   6,  11,  16,  11,  13,  56,  83,  78,
  82, 131, 184, 191, 120,  75,  91,  87,  59,  72,
  80,  78,  82,  84,  83,  85,  84,  85,  99, 116,
  99,  74,  42,  77, 222, 245, 249, 249, 250, 235,
 213, 212, 120,  67,  66,  51,  55,  52,  48,  26,
  23,  24,  19,  11,  11,  19,  16,  15,  13,  22,
  25,  13,  16,  25,  25,  23,  23,  28,  32,  24,
  18,  15,  13,  12,  13,  13,  10,   9,  42,  75,
  83,  87, 112, 172, 201, 179, 116,  94, 110,  85,
  83,  96,  66,  73,  87,  85,  88,  82,  85,  85,
  92,  88,  58,  30, 117, 242, 247, 247, 246, 244,
 204, 141, 129,  91,  66,  71,  62,  58,  52,  44,
  37,  38,  35,  28,  21,  23,  18,  19,  17,  15,
  12,  15,  10,  16,  23,  23,  10,  16,  16,  17,
  14,  21,  22,  26,  28,  21,  23,  18,  14,  45,
  76, 110, 106, 109, 143, 200, 217, 170, 108,  88,
  78,  81,  60,  65,  90,  94,  93,  84,  76,  81,
  70,  75,  75,  49,  48, 159, 242, 245, 247, 242,
 191, 140, 156, 132, 118, 118, 109, 103,  83,  65,
  61,  64,  48,  44,  35,  32,  31,  20,  15,  14,
  18,  13,  13,  12,  13,  21,  13,  10,  20,  18,
  17,  19,  15,  15,  14,  20,  28,  25,  26,  23,
  33,  62,  94, 137, 179, 150, 179, 220, 214, 148,
  84,  78,  69,  60,  67,  67,  88,  86,  83,  74,
  77,  68,  66,  58,  48,  60, 196, 238, 242, 232,
 185, 108,  83, 174, 137, 120, 121, 134, 134, 127,
 122, 123, 102,  86,  77,  57,  44,  46,  38,  28,
  29,  34,  16,   8,  12,  10,   9,   8,  11,  13,
  16,  12,  11,   8,  11,  12,  11,  15,  16,  18,
  14,  42,  56,  97, 163, 206, 196, 157, 200, 231,
 190, 106,  78,  68,  70,  69,  76,  92,  86,  86,
  71,  68,  78,  76,  69,  47,  93, 226, 241, 227,
 132, 105,  87,  61, 185, 146, 134, 131, 131, 131,
 118, 124, 134, 129, 119, 123, 119, 117,  86,  69,
  65,  33,  20,  16,   8,  11,  13,  14,  13,   8,
  15,  17,  15,   6,  14,  17,  15,  11,  11,  12,
  15,   7,  55,  65,  85, 138, 197, 200, 157, 145,
 218, 218, 133,  58,  59,  67,  70,  72,  82,  87,
  79,  75,  79,  65,  81,  67,  62, 130, 227, 219,
 148,  68,  74,  42,  45, 155, 155, 143, 143, 131,
 135, 118, 131, 133, 141, 131, 133, 132, 119, 116,
 122, 113, 117,  97,  61,  49,  39,  31,  24,  12,
  10,  10,  11,  11,  13,  13,  13,  15,  15,   7,
  12,  16,  11,  62,  72,  79,  97, 163, 209, 199,
 145, 182, 212, 167,  96,  71,  75,  84,  75,  87,
  85,  69,  76,  78,  81, 104,  57,  44, 158, 216,
 150,  61,  42,  69,  31,  21,  74, 145, 144, 146,
 137, 145, 137, 132, 132, 141, 136, 135, 119,  91,
 112,  99, 116, 118, 116, 116, 115, 119,  99,  82,
  60,  33,  33,  32,  20,  18,   7,  12,  13,  11,
   8,  17,  24,  14,  52,  75,  84,  89, 115, 185,
 209, 179, 153, 201, 209, 171,  73,  77,  83,  71,
  87,  66,  82,  82,  66,  79,  59,  31,  66, 176,
 141,  51,  39,  47,  69,  27,  22,  56, 126, 147,
 141, 133, 154, 151, 126, 123, 141, 133, 127, 131,
 129, 125, 111, 123, 128, 120, 116, 133, 146, 130,
 118, 110, 106, 102,  99,  69,  50,  41,  16,  13,
  11,  12,  10,  15,  14,  63,  81, 112, 100,  93,
 127, 202, 208, 164, 190, 220, 201, 113,  62,  66,
  65,  75,  77,  82,  76,  71,  75,  63,  44,  67,
 119,  74,  44,  24,  43,  42,  20,  18,  21,  63,
  99, 105, 113, 148, 147, 122, 128, 140, 142, 148,
 148, 156, 138, 128, 130, 134, 128, 120, 128, 123,
  95,  69,  79, 110, 126, 114, 120, 120, 110,  90,
  79,  67,  39,  26,  10,  17,  43,  58,  86,  92,
  94,  93, 172, 208, 193, 174, 212, 226, 169,  84,
  64,  72,  77,  85,  84,  82,  82,  93,  73,  42,
  51,  53,  31,  28,  32,  52,  31,  14,  25,  21,
  14,  11,  15,  38,  76, 108, 125, 123, 123, 125,
 100, 102, 102, 128, 137, 130, 138, 129, 128, 135,
 116, 121, 123,  97, 110, 136, 132, 120, 130, 123,
 131, 121, 118,  90,  25,  13,  19,  31,  57,  77,
  95,  92,  81, 115, 187, 194, 171, 183, 231, 206,
 126,  62,  71,  77,  82,  78,  74,  74,  88,  48,
  25,  34,  33,  38,  36,  42,  56,  26,  12,  22,
  16,  14,  13,  14,  13,  16,  23,  37,  44,  53,
  60,  27,  43,  39,  90, 133, 138, 133, 123, 135,
 136, 143, 134, 129, 114, 121, 144, 141, 142, 151,
 135, 131, 129, 138, 123,  35,  16,  18,  42,  49,
  63,  74,  70,  66,  91, 165, 207, 182, 138, 198,
 226, 178, 105,  63,  66,  78,  77,  71,  73,  66,
  21,  19,  23,  32,  35,  20,  36,  58,  30,  16,
  22,  18,  11,  13,  14,  14,  13,  21,  22,  16,
  16,  15,  19,  26,  37,  81, 111, 128, 131, 132,
 161, 135, 142, 131, 128, 126, 122, 129, 136, 143,
 151, 135, 135, 125, 131, 128,  63,  27,  26,  50,
  47,  45,  65,  76,  80,  73, 114, 204, 197, 155,
 168, 229, 209, 132,  74,  61,  76,  72,  60,  52,
  31,  26,  24,  20,  25,  31,  27,  49,  58,  24,
  16,  19,  16,  16,  12,  15,  17,  17,  17,  10,
  12,  11,  13,  19,  20,  16,  23,  38,  33,  55,
  75,  96, 129, 142, 132, 120, 134, 139, 139, 140,
 145, 154, 135, 129, 134, 134, 133,  67,  27,  25,
  64,  56,  43,  70,  73,  79,  75, 103, 170, 203,
 185, 164, 201, 231, 186, 106,  50,  63,  69,  62,
  36,  18,  17,  17,  27,  33,  26,  37,  55,  45,
  22,  20,  17,  13,   9,  13,  14,  11,  18,  16,
  14,  10,  11,  13,  13,  13,  15,  16,  22,  10,
  14,  18,  20,  42,  72, 103, 107, 129, 112, 131,
 140, 136, 133, 144, 127, 138, 142, 143, 102,  24,
  24,  65,  55,  44,  65,  67,  83,  85,  84,  93,
 177, 189, 164, 161, 221, 211, 158,  94,  60,  61,
  61,  30,  16,  15,  24,  35,  33,  32,  42,  77,
  46,  16,  20,  12,  13,  17,  14,  17,  18,  16,
  19,  18,  13,  14,  22,  14,  13,  16,  11,  16,
  12,  16,  12,  15,  16,  20,  23,  34,  53,  57,
  95, 121, 129, 135, 142, 144, 140, 139, 145, 121,
  31,  19,  85,  46,  43,  58,  79,  75,  86, 102,
  81, 133, 184, 175, 150, 156, 201, 212, 170,  74,
  52,  38,  22,  16,  14,  21,  32,  33,  30,  51,
  74,  29,  12,  11,  12,  13,   8,  17,  13,  15,
  12,  13,  12,  17,  12,  15,  12,  13,  13,  15,
  16,  12,  20,  21,  22,  18,  13,  12,  15,  14,
   9,  14,  30,  48,  77, 115, 133, 132, 134, 131,
 138,  40,  20, 101,  67,  45,  52,  58,  78,  77,
 100,  81,  94, 106, 128, 138, 125, 157, 198, 219,
 129,  31,  19,  16,  14,  19,  29,  24,  25,  23,
  49,  48,  19,  12,  17,  19,  14,  16,  16,  16,
  11,  11,  10,  17,  25,  25,  12,  18,  14,  16,
  14,  14,  13,  18,  17,  20,  20,  19,  14,  13,
  16,  17,  13,  21,  14,  19,  24,  34,  56,  77,
 100, 119,  79,  28,  93,  83,  75,  50,  52,  59,
  66,  84,  87,  74,  81,  94,  98,  87, 115, 199,
 250, 219,  53,  15,  14,  17,  30,  42,  40,  25,
  19,  37,  39,  17,  13,  15,  24,  11,  10,   7,
  13,  17,  12,  14,  14,  19,  20,  15,  11,   9,
  16,  15,  12,  18,  15,  15,  19,  15,  14,  13,
  14,  19,  18,  14,  23,  15,  14,  14,  22,  16,
  19,  23,  30,  56,  57,  81,  81,  75,  46,  49,
  43,  59,  57,  69,  77,  71,  82,  81,  73, 102,
 157, 191, 193,  82,  15,   9,  14,  25,  26,  27,
  23,  25,  34,  36,  20,  16,   7,   8,  15,  13,
  11,  18,  16,  14,  17,  24,  20,  16,  13,  13,
  17,  18,  17,  11,  18,  18,   9,  17,  13,  19,
  16,  15,  14,  21,  19,  22,  10,  16,  19,  12,
  11,  17,  16,  32, 104, 125,  79,  77,  81,  63,
  47,  34,  44,  38,  61,  69,  50,  52,  53,  56,
  67,  99, 117, 148, 100,  29,   9,  11,  22,  32,
  31,  24,  25,  27,  19,  14,  16,  14,  13,  17,
   7,  15,  17,  22,  17,  19,  19,  13,  15,  12,
  13,  19,  15,  13,  12,  11,  13,   6,  16,  17,
  21,  15,  14,  15,  16,  20,  17,  14,  17,  18,
  18,  18,  15,  21,  71, 112, 117,  72,  87,  89,
  66,  59,  50,  52,  44,  64,  64,  45,  39,  45,
  45,  65,  70,  92,  98,  84,  64,  30,  16,  20,
  22,  31,  24,  23,  42,  20,  19,  18,  15,  13,
  14,   7,  13,  19,  16,  15,  21,  15,  22,  17,
  16,   8,  19,  18,  16,  14,  14,  11,  11,  12,
  13,  17,  18,  16,  16,  19,  23,  19,  13,  15,
  17,  11,  21,  22,  40,  91, 114, 132,  84,  77,
  82,  86,  51,  76,  79,  38,  43,  57,  51,  39,
  49,  49,  44,  50,  60,  73,  74,  55,  37,  21,
  23,  26,  27,  29,  35,  40,  15,  14,  15,  11,
   8,  12,  17,  11,  16,  13,   9,  10,  16,  19,
  21,  21,  10,  17,  16,  12,  15,  17,  13,  15,
  17,  14,  10,  20,  13,  13,  14,  18,  16,  11,
  12,  16,  16,  16,  34, 106, 137, 159, 155,  81,
  77,  66,  65,  38,  35,  39,  31,  29,  46,  47,
  55,  59,  52,  37,  33,  54,  68,  67,  37,  37,
  21,  19,  23,  27,  30,  45,  32,  14,  15,  11,
  13,  20,  15,   6,  10,  18,  18,  18,  18,  13,
  15,  15,  13,   9,   7,  14,  18,  16,  18,  21,
  16,  18,  16,  13,  16,  13,  12,   9,  13,  20,
  15,  13,  15,  22,  21,  72, 180, 196, 212, 200,
  83,  79,  58,  48,  41,  30,  12,  16,  19,  26,
  30,  61,  63,  43,  31,  30,  43,  62,  57,  64,
  69,  44,  23,  25,  23,  34,  46,  25,   9,   9,
  11,  19,  12,  13,  15,  17,  16,  17,  15,  18,
  20,  12,  16,  12,  11,  13,  16,  15,  18,  22,
  26,  18,  19,  24,  14,  17,  15,  14,  15,  13,
  15,  16,  15,  14,  15,  21,  68, 151, 172, 210,
 218,  75,  63,  46,  16,  26,  29,  20,  18,  11,
  14,  23,  48,  60,  33,  28,  34,  40,  50,  64,
  93,  76,  67,  35,  23,  24,  18,  19,  17,   7,
  12,  12,  14,  13,  19,  12,  11,  11,   9,   9,
   7,  15,   9,  14,  10,  15,  15,  12,  12,   9,
  14,  15,  15,  18,  14,  17,  16,  11,  18,  18,
  17,  14,  10,  14,  14,  19,  41,  98, 136, 175,
 206, 198,  58,  24,  29,  28,  22,  14,  13,  14,
  15,  19,  21,  33,  43,  25,  26,  29,  45,  50,
  81, 107,  92,  71,  31,  33,  19,  11,  20,  29,
  20,  18,  15,  19,  20,  19,  10,  10,  10,   7,
  15,  15,  14,  12,  13,  16,  18,  14,  15,  11,
  16,  18,  14,  14,  18,  18,  22,  17,  11,  15,
  18,  20,  10,   7,  14,  13,  18,  44,  98, 140,
 178, 198, 146,  38,  25,  25,  25,  20,  18,  12,
  13,  17,  15,  16,  28,  37,  16,  26,  20,  38,
  55,  64,  78,  69,  61,  49,  36,  22,  19,  20,
  24,  23,  21,  30,  36,  27,  26,  17,  14,  19,
  14,  13,  12,  16,  12,  22,  24,  19,  12,  13,
  14,  13,  12,  15,  13,  26,  20,  18,  20,  12,
  12,  22,  21,  15,  13,  14,  16,  56, 110, 116,
 159, 172, 138, 121,  26,  21,  18,  19,  14,  11,
  11,  21,  19,   7,  18,  22,  21,  16,  18,  21,
  45,  50,  63,  49,  60,  57,  37,  26,  38,  27,
  49,  38,  42,  22,  19,  24,  24,  21,  18,   7,
   7,  10,  12,  15,  15,  10,  11,  15,  15,  17,
  15,  19,  18,  18,  18,  16,  19,  14,  12,  15,
  13,  14,  24,  17,  27,  17,  16,  32, 118, 160,
 138, 137, 106,  74,  85,  16,  27,  23,  18,  15,
  19,  16,  17,  15,  10,  19,  15,  12,  11,  15,
  27,  30,  42,  45,  38,  63,  62,  27,  30,  52,
  30,  52,  57,  58,  46,  22,  20,  20,  15,  11,
  17,  15,  17,  15,  17,  15,  20,  24,  24,  15,
  15,  10,  11,  20,  20,  18,  15,  16,  18,  15,
  16,  19,  18,  17,  17,  17,  13,  23,  54, 142,
 123, 121, 103,  43,  42,  72, 0 };

 #define Patch_1_dimz    55;
 #define Patch_1_dims    61;
 #define Patch_2_dimz    77;
 #define Patch_2_dims    71;

       static void ini_tstpic (struct int_mtrx *pp_1, struct int_mtrx *pp_2) 
                     {                                 
                     pp_1->dimz  = Patch_1_dimz;    
                     pp_1->dims  = Patch_1_dims;    
                     pp_1->ptr_m = Bild_1;          
                     pp_2->dimz  = Patch_2_dimz;    
                     pp_2->dims  = Patch_2_dims;    
                     pp_2->ptr_m = Bild_2;          
                     }

$ Return
$!#############################################################################
