/*---------------------------------------------------------------------------*/
/*   this is a c version of the fortran subroutine plkinv;                   */
/*   given wavelength, radiance; return temp by inversion of Planck's Law    */

#include <math.h>

double zplkinv (wavelength, radiance)
 double  wavelength,                    /* wavelength in um (microns)        */
         radiance;                      /* radiance in Watts/(m*m*sr*um)     */
{
   double  c1   = 3.74151e-16,          /* constants                         */
           c2   = 0.0143879,
           fact = 1.0e-6,
           pi   = 3.14159265;

   double  temperature, a, b;

   if (radiance > 0.0) {
      wavelength = wavelength * fact;     /*  convert um to m                */
      radiance = pi * radiance / fact;    /*  W/(m*m*sr*um) to W/(m*m*m)     */
      a = pow(wavelength,5.0);
      b = c1 / (a * radiance);
      temperature = c2 /(wavelength * log(b+1.0));
   }
   else
      temperature = 0.0;

   return temperature;                    /* temperature is in degrees K     */
}
