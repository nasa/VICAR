/*****************************************************************************/
/* Compute mean and second moment of image line.                             */
/*****************************************************************************/
#include "pixstat.h"

void unit_filter2(
  int opcode,		/* 2=moment, 3=variance, 4=sigma */
  float *vs,		/* input column sums --- formerly int */
  double *vs2,		/* input column sums**2 */
  float *mean,          /* mean of image line */
  double *moment,       /* second moment of image line */
  int ns,	        /* number of samples on image line */
  int nlw,		/* filter window height */
  int nsw)		/* filter window width */
{
  int s,z,nswh,n0;
  float sum;
  double sum2,narea;

  narea = nlw*nsw;
  nswh = nsw/2;
  n0 = nswh + 1;

	/* Compute sum of first area */
  sum = vs[0];
  sum2 = vs2[0];
  for (s=1; s<=nswh; s++) {
     sum += 2*vs[s];
     sum2 += 2*vs2[s];
  }

	/* Left margin */
  for (s=0; s<nswh; s++) {
     mean[s] = sum/(float)narea;
     moment[s] = sum2/(float)narea;
     sum = sum - vs[nswh-s] + vs[s+n0];
     sum2 = sum2 - vs2[nswh-s] + vs2[s+n0];
  }

	/* Middle pixstatels */
  for (s=nswh; s<ns-n0; s++) {
     mean[s] = sum/(float)narea;
     moment[s] = sum2/(float)narea;
     sum = sum - vs[s-nswh] + vs[s+n0];
     sum2 = sum2 - vs2[s-nswh] + vs2[s+n0];
  }

	/* Right margin */
  z = 2;
  for (s=ns-n0; s<ns; s++) {
     mean[s] = sum/(float)narea;
     moment[s] = sum2/narea;
     sum = sum - vs[s-nswh] + vs[ns-z];
     sum2 = sum2 - vs2[s-nswh] + vs2[ns-z];
     z++;
  }
  if (opcode == 2) return;
  if (opcode == 3) {
     for (s=0; s<ns; s++) moment[s]=moment[s]-mean[s]*mean[s];
  }
  else {
     for (s=0; s<ns; s++) {
        moment[s]=moment[s]-mean[s]*mean[s];
        if (moment[s] > 0.) moment[s]=sqrt(moment[s]);
        else moment[s]=0.;
     }
  }
}
