/*****************************************************************************/
/* 1-D unit filter							     */
/*****************************************************************************/
#include "pixstat.h"


void unit_filter(
  float *vs,		/* input column sums --- formerly int */
  float *obuf,          /* output image line */
  int ns,	        /* number of samples on image line */
  int nsw,		/* filter window size */
  double scale)		/* dn = scale*dn + offset */
{
  int s,z,nswh,n0;
  float sum;

  nswh = nsw/2;
  n0 = nswh + 1;

	/* Compute sum of first area */
  sum = vs[0];
  for (s=1; s<=nswh; s++) sum+=2*vs[s];

	/* Left margin */
  for (s=0; s<nswh; s++) {
     obuf[s] = sum*(float)scale;
     sum = sum - vs[nswh-s] + vs[s+n0];
  }

	/* Middle pixstatels */
  for (s=nswh; s<ns-n0; s++) {
     obuf[s] = sum*(float)scale;
     sum = sum - vs[s-nswh] + vs[s+n0];
  }

	/* Right margin */
  z = 2;
  for (s=ns-n0; s<ns; s++) {
     obuf[s] = sum*(float)scale;
     sum = sum - vs[s-nswh] + vs[ns-z];
     z++;
  }
}
