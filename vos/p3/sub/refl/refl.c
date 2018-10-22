
#include <stdio.h>
#include <math.h>
#include "const.h"


/*---------------------------------------------------------------------------*/
/* read in wavelength-reflectance value pairs from the user specified file   */
/* there should be one pair per line; tab or space delimited                 */

int zreadref (wave, refl)
     float  wave[1000], refl[1000];
{
  
  int  eof, npts, status, count;
  char reflfile[64], inbuf[LineLength];
  FILE *rfile;
  
  npts = 0;
  
  status = zvp ("REFLFILE", reflfile, &count);
  
  rfile = fopen(reflfile,"r");
  
  while (getline_vicar(inbuf, LineLength, rfile, &eof) >= 0 && !eof)
    if (sscanf(inbuf, "%f %f", &wave[npts], &refl[npts]) == 2)
      npts++;
  
  
  fclose(rfile);
  
  return npts;
}


/*---------------------------------------------------------------------------*/
/* use default values for reflectance if the user doesn"t specify anything   */

int zbldref(salb, wave, refl)
     float  salb;
     float  wave[1000], refl[1000];
{
  
  wave[0] = 0.1;
  wave[1] = 100.0;
  refl[0] = salb;
  refl[1] = salb;
  
  return 2;          /* npts == 2, is number of values in the arrays */
}

/*---------------------------------------------------------------------------*/
/* interpolate input or defaulted reflectance values to produce an array     */
/* with spacing and step size defined by user parms v1, v2 and dv            */

int zinterp_refl (start_wave, end_wave, wave_step, in_pts, wave, refl, specalb)
     float  start_wave, end_wave, wave_step;
     int in_pts;
     float  wave[1000], refl[1000], specalb[8000];
{
  int i, j, out_pts;
  float  ratio, wave_i;
  
  /*----- convert from wavelength to wavenumbers (micrometers to cm -1 -----*/
  for (j = 0; j < in_pts; j++)
    wave[j] = 10000.0 / wave[j];
  
  out_pts = (int ) ceil((end_wave - start_wave + wave_step) / wave_step);
  
  j = in_pts - 1;
  wave_i = start_wave;
  
  for (i = 0; i < out_pts; i++) {
    
    while (wave[j] < wave_i && j > 0)
      j -= 1;
    
    ratio = (wave[j] - wave_i) / (wave[j] - wave[j+1]);
    specalb[i] = ratio * refl[j+1] + (1 - ratio) * refl[j];
    
    wave_i += wave_step;
  }
  return out_pts;
}

