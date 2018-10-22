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

