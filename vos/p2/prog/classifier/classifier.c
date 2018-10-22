#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "gsl/gsl_errno.h"
#include "ImageUtils.h"
#include "ibishelper.h"
#include "cartoClassUtils.h"

#define KMEANS 0
#define EXPMAX 1

void main44(void)
{
   gsl_vector *dns;
   int i, j, status, dumdef, dumcnt, inpCnt, outCnt, nClasses, isExpMax, gnu_errno;
   double *dist;
   VICAR_IMAGE **inps, *out1, *out2;
   IBISStruct *ibis;
   CLASS **classes;
   CLASS_TMP *tmp;

   zifmessage("classifier version 2017-08-03");

   isExpMax = zvptst("EXPMAX");
   printf("isExpMax: %d\n", isExpMax);
   status = zvpcnt("inp", &inpCnt);
   inps = (VICAR_IMAGE**)malloc(sizeof(VICAR_IMAGE*)*(inpCnt));
   for(i = 0; i < inpCnt; i++) inps[i] = getVI_inp(i+1);
   status = zvpcnt("out", &outCnt);
   out1 = getVI_out("HALF", 1, inps[0]->nl, inps[0]->ns);
   if(outCnt == 2)
      out2 = getVI_out("REAL", 2, inps[0]->nl, inps[0]->ns);

   ibis = IBISHELPER_openIBIS("classibis", 1, "read");
   classes = CARTOCLASS_loadClassesFromIBIS(ibis);
   for(i = 0; i < ibis->nr; i++) CARTOCLASS_printClass(classes[i]);

   nClasses = ibis->nr;
   dist = (double*)malloc(sizeof(double)*nClasses);
   for(i = 0; i < nClasses; i++)
   {
      int id;

      sscanf(classes[i]->name, "CLASS %d", &id);
      CARTOCLASS_setID(classes[i], id);
      //      CARTOCLASS_printClass(classes[i]);
   }
   tmp = CARTOCLASS_getClassTmp(inpCnt);

   dns = gsl_vector_alloc(inpCnt);
   for(i = 0; i < inps[0]->nl; i++)
   {
      int k;

      if(!(i%1000)) printf("---> Processing line %d.\n", i);
      for(j = 0; j < inpCnt; j++) readVicarImageLine(inps[j], i);

      for(j = 0; j < inps[0]->ns; j++)
      {
         double minDist;
         int minIndex;

         for(k = 0; k < inpCnt; k++) 
            gsl_vector_set(dns, k, inps[k]->buffer[j]);

         //         dist[0] = CARTOCLASS_getMahalanobisDist(classes[0], dns, tmp);
         if(!isExpMax)
            dist[0] = CARTOCLASS_getEuclideanDist(classes[0], dns, tmp);
         else
         {
            if(i == 90 && j == 405)
               printf("here\n");
            gnu_errno = CARTOCLASS_calcGaussianDensityAtX(classes[0], dns, tmp, &(dist[0]));
            assert(gnu_errno == GSL_SUCCESS);
         }

         minDist = dist[0];
         minIndex = 0;
         for(k = 1; k < nClasses; k++)
         {
            if(!classes[k]->isValidCov) continue;
            //            dist[k] = CARTOCLASS_getMahalanobisDist(classes[k], dns, tmp);
            if(!isExpMax)
               dist[k] = CARTOCLASS_getEuclideanDist(classes[k], dns, tmp);
            else
            {
               gnu_errno = CARTOCLASS_calcGaussianDensityAtX(classes[k], dns, tmp, &(dist[k]));
//               printf("errno: %d\n", gnu_errno);
               assert(gnu_errno == GSL_SUCCESS);
            }

/*
            printf("line: %d samp: %d dns1: %lf dns2: %lf dist: %lf minDist: %lf class: %s minClass: %s\n",
                   i, j, gsl_vector_get(dns, 0), gsl_vector_get(dns, 1), dist[k],
                   minDist, classes[k]->name, classes[minIndex]->name);
*/

            if(!isExpMax && minDist > dist[k])
            {
               minDist = dist[k];
               minIndex = k;
            }
            else if(isExpMax && minDist < dist[k])
            {
               minDist = dist[k];
               minIndex = k;
            }
         }

         //         for(k = 0; k < nClasses; k++) printf("Distance for class %d: %lf minDist: %lf minIndex: %d\n", classes[k]->id, dist[k], minDist, minIndex);
         //         return;

//         printf("minDist: %lf minIndex: %d\n", minDist, minIndex);
         out1->buffer[j] = classes[minIndex]->id;
         if(outCnt == 2) out2->buffer[j] = minDist;
      }

      writeVicarImageLine(out1, i);
      if(outCnt == 2) writeVicarImageLine(out2, i);
   }

   deleteAndCloseImage(&out1);
   if(outCnt == 2) deleteAndCloseImage(&out2);
}
