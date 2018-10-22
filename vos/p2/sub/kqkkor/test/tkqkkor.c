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

