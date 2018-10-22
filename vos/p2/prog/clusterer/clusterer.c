#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>

#include "zifmessage.h"
#include "ImageUtils.h"
#include "gsl/gsl_vector.h"
#include "gsl/gsl_matrix.h"
#include "ibishelper.h"
#include "cartoClassUtils.h"
#include "cartoLinkedList.h"
#include "zmabend.h"

/**********************************************/
typedef struct
{
   int line, samp;
   gsl_vector *dns;
}SAMPLE;

/**********************************************/
typedef struct
{
   CLASS *c;
   SAMPLE **samples;
   gsl_vector *oldMeans;
   int nSamples;
   int alive;
}CLUSTER;

/**********************************************/
typedef struct
{
   gsl_vector *diff;
   double *deltas;
}TMP;

/**********************************************/
void printSample(SAMPLE* s)
{
   int i;

   printf("line: %d samp: %d\n", s->line, s->samp);
   for(i = 0; i < s->dns->size; i++) printf("%lf ", gsl_vector_get(s->dns, i));
   printf("\n");
}

/**********************************************/
void printCluster(CLUSTER *c)
{
   int i;

   printf("-----> CLUSTER: %d nSamples: %d\n", c->c->id, c->nSamples);
   for(i = 0; i < c->c->means->size; i++) printf("%lf ", gsl_vector_get(c->c->means, i));
   printf("\n");
   for(i = 0; i < c->oldMeans->size; i++) printf("%lf ", gsl_vector_get(c->oldMeans, i));
   printf("\n");
   //   for(i = 0; i < c->nSamples; i++) printSample(c->samples[i]);
}

/**********************************************/
TMP* getTMP(int nClusters, int nDim)
{
   TMP *tmp = (TMP*)malloc(sizeof(TMP));

   tmp->diff = gsl_vector_alloc(nDim);
   tmp->deltas = (double*)malloc(sizeof(double)*nClusters);

   return tmp;
}

/**********************************************/
void freeTMP(TMP **tmp)
{
   gsl_vector_free((*tmp)->diff);
   free((*tmp)->deltas);

   free(*tmp);
}

/**********************************************/
SAMPLE** readSamples(int *nSamples, int *nDim)
{
   int i, j;
   IBISStruct *inp;
   SAMPLE** samples;

   inp = IBISHELPER_openIBIS("inp", 1, "read");

   *nDim = inp->nc-2;
   *nSamples = inp->nr;
   samples = (SAMPLE**)malloc(sizeof(SAMPLE*)*(*nSamples));

   for(i = 0; i < *nSamples; i++)
   {
      SAMPLE* sample = (SAMPLE*)malloc(sizeof(SAMPLE));
      sample->line = IBISHELPER_getInt(inp, 0, i);
      sample->samp = IBISHELPER_getInt(inp, 1, i);
      sample->dns = gsl_vector_alloc(*nDim);
      for(j = 0; j < *nDim; j++)
         gsl_vector_set(sample->dns, j, IBISHELPER_getDouble(inp, j+2, i));
      samples[i] = sample;
   }

   IBISHELPER_closeIBIS(&inp);
   return samples;
}

/**********************************************/
void setBoundingBox(SAMPLE** samples, double* mins, double* maxes, int nSamples, int nDim)
{
   int i, j;

   for(i = 0; i < nDim; i++)
   {
      mins[i] = DBL_MAX;
      maxes[i] = DBL_MIN;
   }

   for(i = 0; i < nSamples; i++)
   {
      for(j = 0; j < nDim; j++)
      {
         double dn = gsl_vector_get(samples[i]->dns, j);

         if(mins[j] > dn) mins[j] = dn;
         if(maxes[j] < dn) maxes[j] = dn;
      }
   }
}

/**********************************************/
void initializeCluster(CLUSTER* cluster, int id, double* mins, double* maxes, int nDim)
{
   int i, j;
   gsl_vector *means;

   cluster->c = CARTOCLASS_getClass();
   CARTOCLASS_setID(cluster->c, id);
   means = gsl_vector_alloc(nDim);
   for(j = 0; j < means->size; j++)
      gsl_vector_set(means, j, rand()/(double)RAND_MAX*(mins[j]+maxes[j]));

   CARTOCLASS_setMean(cluster->c, means);
}

/**********************************************/
void binSamplesIntoClusters(CLUSTER** clusters, SAMPLE** samples, int nClusters, int nSamples)
{
   int i, j, minIndex;
   double minDist;
   CLASS_TMP* tmp;

   tmp = CARTOCLASS_getClassTmp(clusters[0]->c->means->size);
   //   printf("here1\n");
   for(i = 0; i < nClusters; i++) clusters[i]->nSamples = 0;

   //   printf("here2\n");
   for(i = 0; i < nSamples; i++)
   {
      minDist = DBL_MAX;
      for(j = 0; j < nClusters; j++)
      {
         double dist;

         if(!(clusters[j]->alive)) continue;
         dist = CARTOCLASS_getEuclideanDist(clusters[j]->c, samples[i]->dns, tmp);
         //         printf("i: %d j: %d dist: %lf\n", i, j, dist);
         if(minDist > dist)
         {
            minDist = dist;
            minIndex = j;
         }
      }
      //      printf("here3 minIndex: %d minDist: %lf nSamples: %d\n", minIndex, minDist, clusters[minIndex]->nSamples);

      clusters[minIndex]->samples[(clusters[minIndex]->nSamples)++] = samples[i];
   }
   //   printf("here4\n");
}

/**********************************************/
void calculateCentroid(CLUSTER* cluster)
{
   int i, j;

   //   printf("calculateCentroid1\n");
   if(cluster->oldMeans == NULL) cluster->oldMeans = gsl_vector_alloc(cluster->c->means->size);
   gsl_vector_memcpy(cluster->oldMeans, cluster->c->means);

   //   printf("calculateCentroid2\n");
   if(cluster->nSamples == 0) return;
   for(i = 0; i < cluster->c->means->size; i++) gsl_vector_set(cluster->c->means, i, 0.);
   //   printf("calculateCentroid3\n");
   for(i = 0; i < cluster->nSamples; i++)
      gsl_vector_add(cluster->c->means, cluster->samples[i]->dns);
   //   printf("calculateCentroid4\n");
   for(i = 0; i < cluster->c->means->size; i++)
      gsl_vector_set(cluster->c->means, i, gsl_vector_get(cluster->c->means, i)/(double)cluster->nSamples);
}

/**********************************************/
double l2Norm1(gsl_vector *diff)
{
   double sum;
   int i;

   sum = 0;
   for(i = 0; i < diff->size; i++) sum += pow(gsl_vector_get(diff, i), 2.);

   return sqrt(sum);
}

/**********************************************/
int converged(CLUSTER** clusters, TMP *tmp, int nClusters, double convergenceThreshold, int iteration)
{
   int i;
   int changed;
   double totalChanged;;

   totalChanged = 0.;
   //   printf("inside converged 1\n");
   for(i = 0; i < nClusters; i++)
   {
      //      printf("here 1\n");
      calculateCentroid(clusters[i]);
      //      printf("here 2\n");
      gsl_vector_memcpy(tmp->diff, clusters[i]->c->means);
      gsl_vector_sub(tmp->diff, clusters[i]->oldMeans);
      tmp->deltas[i] = l2Norm1(tmp->diff);
      totalChanged += tmp->deltas[i];
      //      printf("delta: %lf total changed: %lf\n", tmp->deltas[i], totalChanged);
   }

   //   printf("inside converged 2\n");
   printf("iter: %d total changed: %lf theshold: %lf\n", iteration, totalChanged, convergenceThreshold);
   return totalChanged < convergenceThreshold;
}

/**********************************************/
void printIteration(CLUSTER** clusters, TMP *tmp, int nClusters, int nIteration)
{
   int i;
   double sum = 0.;

   printf("\nITERATION: %d\n", nIteration);
   printf("====================================\n");
   for(i = 0; i < nClusters; i++)
   {
      printCluster(clusters[i]);
      printf("diff: %lf\n", tmp->deltas[i]);
      sum += tmp->deltas[i];
   }
   printf("=========================\n");
   printf("total shift: %lf %d\n", sum, nIteration);
}

/**********************************************/
CLUSTER** getClusters(SAMPLE** samples, int nClusters, int nSamples, int nDim, TMP *tmp, double thresh, int maxIterations, int *convergedFlag)
{
   int i, nIterations;
   double *mins, *maxes;
   CLUSTER** clusters;
   srand(0);

   clusters = (CLUSTER**)malloc(sizeof(CLUSTER*)*nClusters);
   for(i = 0; i < nClusters; i++)
   {
      clusters[i] = (CLUSTER*)malloc(sizeof(CLUSTER));
      clusters[i]->samples = (SAMPLE**)malloc(sizeof(SAMPLE*)*nSamples);
      clusters[i]->oldMeans = NULL;
      clusters[i]->alive = 1;
   }
   mins = (double*)malloc(sizeof(double)*nDim);
   maxes = (double*)malloc(sizeof(double)*nDim);

   printf("setting bounding box\n");
   setBoundingBox(samples, mins, maxes, nSamples, nDim);

   printf("initializing clusters\n");
   for(i = 0; i < nClusters; i++) initializeCluster(clusters[i], i+1, mins, maxes, nDim);
   printf("binning clusters\n");
   binSamplesIntoClusters(clusters, samples, nClusters, nSamples);
   nIterations = 1;
   *convergedFlag = converged(clusters, tmp, nClusters, thresh, nIterations);

   // kill initially empty clusters
   for(i = 0; i < nClusters; i++)
      if(clusters[i]->alive && !(clusters[i]->nSamples))
      {
        //            printf("Cluster %d thrown out\n", i);
         clusters[i]->alive = 0;
      }

   while(!(*convergedFlag) && nIterations < maxIterations)
   {
     //      printIteration(clusters, tmp, nClusters, nIterations);
      binSamplesIntoClusters(clusters, samples, nClusters, nSamples);
      ++nIterations;
      *convergedFlag = converged(clusters, tmp, nClusters, thresh, nIterations);
   }

   //   printIteration(clusters, tmp, nClusters, nIterations);

   return clusters;
}

/**********************************************/
int calculateCov(CLUSTER* cluster, int nDim)
{
   int i, j, k, nonZeroFlag;

   if(cluster->c->cov_matrix == NULL)
      cluster->c->cov_matrix = gsl_matrix_alloc(nDim, nDim);

   if(cluster->nSamples == 0)
   {
      for(i = 0; i < nDim; i++)
         for(j = 0; j < nDim; j++)
            gsl_matrix_set(cluster->c->cov_matrix, i, j, 0.);

      return 0;
   }

   nonZeroFlag = 0;
   for(i = 0; i < nDim; i++)
      for(j = 0; j < nDim; j++)
      {
         double cov = 0.;

         for(k = 0; k < cluster->nSamples; k++)
            cov += (gsl_vector_get(cluster->samples[k]->dns, i) - gsl_vector_get(cluster->c->means, i))*(gsl_vector_get(cluster->samples[k]->dns, j) - gsl_vector_get(cluster->c->means, j));
         cov /= (double)cluster->nSamples;

         if(fabs(cov) > 10E-10) nonZeroFlag = 1;
         gsl_matrix_set(cluster->c->cov_matrix, i, j, cov);
      }

   return nonZeroFlag;
}

/**********************************************/
void outIBIS(CLUSTER **clusters, int nClusters, int nDim, int convergedFlag)
{
   int i, j, k, nc, nValidClusters, rowIndex, outCnt, nSamples, status;
   int *validClusters;
   IBISPrep *outPrep, *outPrep2;
   IBISStruct *out;
   char tmpName[20];
   char buf[200];

   validClusters = (int*)calloc(nClusters, sizeof(int));
   nValidClusters = 0;
   for(i = 0; i < nClusters; i++)
   {
      validClusters[i] = calculateCov(clusters[i], nDim);
      if(validClusters[i]) nValidClusters++;
   }

   outPrep = IBISHELPER_openIBIS_out2("out", 1, nValidClusters);
   nc = 3 + nDim + nDim*(nDim+1)/2;
   IBISHELPER_addColumn(outPrep, "A12");
   for(i = 1; i < nc; i++) IBISHELPER_addColumn(outPrep, "REAL");
   out = IBISHELPER_getIBISStruct(&outPrep);

   printf("========================================\n");
   sprintf(buf, "FINAL RESULT total num of clusters: %d converged: %s", nValidClusters, convergedFlag?"YES":"NO");
   zifmessage(buf);
   printf("========================================\n");
   rowIndex = 0;

   for(i = 0; i < nClusters; i++)
   {
      int index = 0;

      if(!validClusters[i]) continue;

      // write id
      //      printf("Cluster %d\n", clusters[i]->c->id);
      sprintf(tmpName, "CLASS %d", clusters[i]->c->id);
      CARTOCLASS_setClassName(clusters[i]->c, tmpName);
      IBISHELPER_setString(out, index++, rowIndex, tmpName);
      IBISHELPER_setDouble(out, index++, rowIndex, clusters[i]->nSamples);
      IBISHELPER_setDouble(out, index++, rowIndex, clusters[i]->c->nDim);

      // write means
      for(j = 0; j < nDim; j++)
         IBISHELPER_setDouble(out, index++, rowIndex, gsl_vector_get(clusters[i]->c->means, j));

      // write cov matrix
      for(j = 0; j < nDim; j++)
         for(k = 0; k <= j; k++)
         {
            IBISHELPER_setDouble(out, index++, rowIndex, gsl_matrix_get(clusters[i]->c->cov_matrix, j, k));
            //            printf("%lf ", gsl_matrix_get(clusters[i]->c->cov_matrix, j, k));
         }

      //      printf("\n");
      rowIndex++;
   }

   IBISHELPER_closeIBIS(&out);

   // Output2
   status = zvpcnt("out", &outCnt);
   if(status != 1) zmabend("Error while getting number of output ibis files.\n");
   if(outCnt == 1)
   {
      free(validClusters);
      return;
   }

   nSamples = 0;
   for(i = 0; i < nClusters; i++) if(validClusters[i]) nSamples += clusters[i]->nSamples;
   //   printf("outIBIS 1\n");
   outPrep2 = IBISHELPER_openIBIS_out2("out", 2, nSamples);
   //   printf("outIBIS 2\n");
   IBISHELPER_addColumn(outPrep2, "A12");
   //   printf("outIBIS 3\n");
   nc = nDim + 3;
   for(i = 1; i < nc; i++) IBISHELPER_addColumn(outPrep2, "REAL");
   //   printf("outIBIS 4\n");
   out = IBISHELPER_getIBISStruct(&outPrep2);
   //   printf("outIBIS 5\n");
   rowIndex = 0;
   for(i = 0; i < nClusters; i++)
   {
      if(!(validClusters[i])) continue;

      for(j = 0; j < clusters[i]->nSamples; j++)
      {
         IBISHELPER_setString(out, 0, rowIndex, clusters[i]->c->name);
         //         printf("1 j: %d\n", j);
         SAMPLE *s = clusters[i]->samples[j];
         //         printf("2 j: %d\n", j);
         IBISHELPER_setDouble(out, 1, rowIndex, s->line);
         //         printf("3 j: %d\n", j);
         IBISHELPER_setDouble(out, 2, rowIndex, s->samp);
         //         printf("4 j: %d\n", j);
         for(k = 0; k < nDim; k++) IBISHELPER_setDouble(out, 3+k, rowIndex, gsl_vector_get(s->dns, k));
         //         printf("5 j: %d\n", j);
         ++rowIndex;
      }
   }
   //   printf("outIBIS 10\n");
   IBISHELPER_closeIBIS(&out);
   //   printf("outIBIS 11\n");
   free(validClusters);
   //   printf("outIBIS 12\n");
}

/**********************************************/
void main44(void)
{
   int status, dumdef, dumcnt, nClusters, nSamples, nDim, i, maxIterations, convergedFlag;
   double thresh;
   IBISStruct *out;
   SAMPLE **samples;
   CLUSTER **clusters;
   TMP *tmp;

   zifmessage("clusterer version 2017-08-03");

   status = zvp("NCLUSTERS", &nClusters, &dumcnt);
   if(status != 1) zmabend("Error while acquiring cluster count parameter.\n");
   status = zvp("MAXITER", &maxIterations, &dumcnt);
   if(status != 1) zmabend("Error while acquiring max iterations.\n");
   status = zvparmd("CONVTHRESH", &thresh, &dumcnt, &dumdef, 1, 0);
   if(status != 1) zmabend("Error while acquiring convergence threshold parameter.\n");
   //   printf("theshold: %lf\n", thresh);

   printf("getting samples\n");
   samples = readSamples(&nSamples, &nDim);
   printf("getting temporary\n");
   tmp = getTMP(nClusters, nDim);
   printf("getting clusters\n");
   clusters = getClusters(samples, nClusters, nSamples, nDim, tmp, thresh, maxIterations, &convergedFlag);
   printf("writing output\n");
   outIBIS(clusters, nClusters, nDim, convergedFlag);
   freeTMP(&tmp);
}
