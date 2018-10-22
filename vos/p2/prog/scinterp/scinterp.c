#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "cartoTaeUtils.h"
#include "atteph_convert.h"

/*  interpolate spacecraft quaternions to sc2rpc vectors   A. Zobrist  1/11/06   */

void main44(void)
{
   int ct,def,iret;
   double patt_norm, satt_norm;
   int    i;
   double peph[5],patt[5],seph[5],satt[5],imtime,vec[8];
   
   zifmessage("scinterp version 2017-06-20");
   
   /* get the parameters */
   
   zvparmd("peph",peph,&ct,&def,5,0);
   zvparmd("patt",patt,&ct,&def,5,0);
   zvparmd("seph",seph,&ct,&def,5,0);
   zvparmd("satt",satt,&ct,&def,5,0);
   zvparmd("imtime",&imtime,&ct,&def,1,0);
   
   /* convert and output */
   
   printf("peph %f %f %f %f %f\n",peph[0],peph[1],peph[2],
       peph[3],peph[4]);
   printf("patt %f %f %f %f %f\n",patt[0],patt[1],patt[2],
       patt[3],patt[4]);
   printf("seph %f %f %f %f %f\n",seph[0],seph[1],seph[2],
       seph[3],seph[4]);
   printf("satt %f %f %f %f %f\n",satt[0],satt[1],satt[2],
       satt[3],satt[4]);
   printf("imtime %f\n",imtime);
   
   patt_norm = (double) 0.0;
   for (i = 0; i < 4; i++) {
     patt_norm += patt[i]*patt[i];
   }
   
   satt_norm = (double) 0.0;
   for (i = 0; i < 4; i++) {
     satt_norm += satt[i]*satt[i];
   }
  
   printf("patt_norm = %.15f\n", sqrt(patt_norm));
   printf("satt_norm = %.15f\n", sqrt(satt_norm));
   
   patt_norm = sqrt(patt_norm);
   satt_norm = sqrt(satt_norm);
   for (i = 0; i < 4; i++) {
     patt[i] = patt[i]/patt_norm;
     satt[i] = satt[i]/satt_norm;
   }
   
   iret = atteph_convert8(peph,patt,seph,satt,imtime,vec);

   /* Note: 0-th element of vec should also be 0 */
   mq_out_real("w_t_sv1",vec[1]);
   mq_out_real("w_t_sv2",vec[2]);
   mq_out_real("w_t_sv3",vec[3]);
   mq_out_real("w_q_sv0",vec[4]);
   mq_out_real("w_q_sv1",vec[5]);
   mq_out_real("w_q_sv2",vec[6]);
   mq_out_real("w_q_sv3",vec[7]);

   return;
}
