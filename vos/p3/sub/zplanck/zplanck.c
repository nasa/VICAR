
/*---------------------------------------------------------------------------*/
/*   this is a c version of the fortran subroutine planck;                   */
/*   given wavelength and temperature, return radiance by Planck's Law       */


#include <math.h>

double zplanck (wavelength, temperature)
 double  wavelength,                    /* wavelength in microns             */
         temperature;                   /* temperature in degrees Kelvin     */
{

   double  c1   =  3.74151e-16,         /* 'constants'                       */
           c2   =  0.0143879,
           fact =  1.0e-6,
           pi   =  3.14159265;

   double  a, b, radiance;

   if (temperature > 0.0) {
      wavelength = wavelength * fact;       /* convert um to m               */
      a = pow(wavelength,5.0);
      b = exp(c2 / (wavelength*temperature));
      radiance = c1 / (a * (b-1.0));

      radiance = (radiance * fact) / pi;    /* W/(m*m*m) to W/(m*m*sr*um)    */
   }
   else
      radiance=0.0;

   return radiance;
}

