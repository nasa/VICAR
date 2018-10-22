/*****************************************************************************/
/* Write output image line.  If output data format is not REAL, round of the */
/* DN value and truncate as necessary to fit output data format.	     */

#include "pixstat.h"
#include "zvproto.h"

void output(int ounit,
            float *mean,
            int nso,
            double mindn,
            double maxdn)
{
  int i;
  float dn;

  if (maxdn != 0.) {		/* skip if output data format is REAL */
     for (i=0; i<nso; i++) {
         dn = mean[i];
         if (dn < 0.) {
            dn -= (float)0.5;		/* round down */
            if (dn < ((float)mindn)) dn=(float)mindn;
         }
         else {
            dn += (float)0.5;		/* round up */
            if (dn > ((float)maxdn)) dn=(float)maxdn;
         }
         mean[i] = dn;
     }
  }
  zvwrit(ounit,mean,NULL);
}
/*****************************************************************************/
/* Write output image line.  If output data format is not REAL, round of the */
/* DN value and truncate as necessary to fit output data format.	     */

void output2(int ounit,
            float *mean,
            double *moment,
            int nso,
            double mindn,
            double maxdn,
            double scale)
{
  int i;
  double dn;

  if (maxdn == 0.) {
     for (i=0; i<nso; i++) mean[i]=(float)(scale*moment[i]);
  }
  else {
     for (i=0; i<nso; i++) {
         dn = scale*moment[i];
         if (dn < 0.) {
            dn -= 0.5;		/* round down */
            if (dn < mindn) dn=mindn;
         }
         else {
            dn += 0.5;		/* round up */
            if (dn > maxdn) dn=maxdn;
         }
         mean[i] = (float)dn;
     }
  }
  zvwrit(ounit,mean,NULL);
}
