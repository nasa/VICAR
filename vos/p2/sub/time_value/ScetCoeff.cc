/***********************************************************************
*  ScetCoeff.cc  
*
*  Subroutines for sclk2scet & scet2sclk programs.
*
*  History:
*      March 29, 2001 (Hyun H. Lee)
*         Separated ScetCoeff implementation from sclk2scet.
*         Added get_values_with_scet method.
*         Renamed get_values to get_values_with_sclk.
***********************************************************************/
#include "time_value.h"
#include "ScetCoeff.h"
#include <iomanip>
#include <math.h>

void ScetCoeff::reset()
{
        delete[] sclk0; delete[] sclkrate;
        for (int i = 0; i < num; i++) delete scet0[i];
        delete[] scet0;
        num = 0;
}

int ScetCoeff::load_coeff(char *filename)
{
   char buffer[100], *p;

   std::ifstream infile(filename);
   if (!infile ) {
     std::cout << "Unable to load file " << filename << std::endl;
      return -1;
   }

   int get_rec = 0;
   int ast = 0;
   int ct = 0;
   std::streampos pos;
   while (infile.getline(p = buffer, sizeof(buffer))) {
      if (*p == '*') {
         ast = 1;
         pos = infile.tellg();
      }
      else if (ast) {
         get_rec = 1;
         ast = 0;
      }

      if (get_rec && *p != 'C') ct++;
   }
   if (!ct) {
     std::cout << "No Records found in Coefficient File" << std::endl;
      return -1;
   }
   reset();

   infile.close();
   infile.open(filename);
   infile.seekg(pos);
   sclk0 = new double[ct];
   sclkrate = new double[ct];
   scet0 = new char*[ct];
   num = ct;
   for (int i = 0; i < num; i++) {
      scet0[i] = new char[22];
      infile.getline(p = buffer, sizeof(buffer));
      int ct = 0;
      char *str;
      while ((str = strtok(p, " "))) {
         switch(ct) {
            case 0:
                     sclk0[i] = atof(str);
                     break;
            case 1:

                     strcpy(scet0[i], str);
                     break;
            case 3:
                     sclkrate[i] = atof(str);
                     break;
            default:
                     break;
                     // do nothing
         } // end switch
         ct++;
         p = 0;
      } // end while
   } // end for

   return 0;
}

int ScetCoeff::get_values_with_sclk(double insclk0, double &sclk0_out,
                                           double &rate_out, char *scet0_out)
{
   int ret = 0;
   int i;
   for (i = 0; i < num; i++) {
      if (sclk0[i] > insclk0) break;
   }

   if (i == 0) ret = -1;
   else {
      int inx = i-1;
      int d = (int)sclk0[inx];  // get whole number
      double f = ((sclk0[inx] - d) * 1000);     // get fraction
      /*  You can't use an int here.  The int is truncated and you loose precision.   */
      /*  int f = (int)((sclk0[inx] - d) * 1000);       // get fraction */
      double df = f / 256.0;
      sclk0_out = d + df;
      rate_out = sclkrate[inx];
      strcpy(scet0_out, scet0[inx]);
   }
   return ret;
}

int ScetCoeff::get_values_with_scet(char *scet0_in, double &sclk0_out,
                                           double &rate_out, char *scet0_out)
{

   int ret = 0;
   ScEventTime stime;
   stime.set_scet_time(scet0_in);
   double in_sec = stime.get_time_in_sec();

   int i;
   for (i=0; i<num; i++) {
      stime.set_scet_time(scet0[i]);
      double out_sec = stime.get_time_in_sec();

      if (out_sec > in_sec) break;
   }

   if (i == 0) ret = -1;
   else {
      int inx = i-1;
      int d = (int)sclk0[inx];  // get whole number
      double f = ((sclk0[inx] - d) * 1000);     // get fraction
      /*  You can't use an int here.  The int is truncated and you loose precision.   */
      /*  int f = (int)((sclk0[inx] - d) * 1000);       // get fraction */
      double df = f / 256.0;
      sclk0_out = d + df;
      rate_out = sclkrate[inx];
      strcpy(scet0_out, scet0[inx]);
   }
   return ret;
}

