#include "cartoTieUtils.h"

void rot90(double* tie, int nrot)
{
   int i,j;
   double tmp0,tmp1;
   
   for (i=0;i<nrot;i++)
      {
      tmp0 = tie[0];
      tmp1 = tie[1];
      for (j=0;j<6;j++) tie[j] = tie[j+2];
      tie[6] = tmp0;
      tie[7] = tmp1;
      }
    
   return;
}

void swp90(double* tie, int nrot)
{
   int i;
   double tmp4,tmp5;
   
   /* this is swapping lines for samples for 90 deg rotate */
   
   for (i=0;i<nrot;i++)
      {
      tmp4 = tie[4];
      tmp5 = tie[5];
      tie[2] = tmp5;
      tie[4] = tmp5;
      tie[5] = tmp4;
      tie[7] = tmp4;
      }
    
   return;
}

void flip(double* tie)
{
   double tmp0,tmp1;
   
   tmp0 = tie[0];
   tmp1 = tie[1];
   tie[0] = tie[6];
   tie[1] = tie[7];
   tie[6] = tmp0;
   tie[7] = tmp1;
   
   tmp0 = tie[2];
   tmp1 = tie[3];
   tie[2] = tie[4];
   tie[3] = tie[5];
   tie[4] = tmp0;
   tie[5] = tmp1;
    
   return;
}
