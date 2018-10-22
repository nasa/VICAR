#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "gsl/gsl_vector.h"
#include "gsl/gsl_matrix.h"
#include "gsl/gsl_permutation.h"
#include "gsl/gsl_blas.h"
#include "gsl/gsl_errno.h"
#include "gsl/gsl_linalg.h"
#include "cartoClassUtils.h"
#include "ibishelper.h"
#include "zmabend.h"

/***********************************************/
CLASS* CARTOCLASS_getClass()
{
   CLASS *c = (CLASS*)malloc(sizeof(CLASS));
   c->name = NULL;
   c->means = NULL;
   c->cov_matrix = NULL;
   c->cov_matrix_inv = NULL;
   c->nDim = -1;

   return c;
}

/***********************************************/
void CARTOCLASS_printVector(gsl_vector *v)
{
   int i;

   for(i = 0; i < v->size; i++) printf("%lf ", gsl_vector_get(v, i));
   printf("\n");
}

/***********************************************/
void CARTOCLASS_printMatrix(gsl_matrix *m)
{
   int i, j;

   for(i = 0; i < m->size1; i++)
   {
      for(j = 0; j < m->size2; j++)
         printf("%.9lf ", gsl_matrix_get(m, i, j));
      printf("\n");
   }
   printf("\n");
}

/***********************************************/
CLASS* CARTOCLASS_getClassByName(char *name, gsl_vector *means, gsl_matrix *cov)
{
   CLASS *c;
   int gnu_errno;

   c = (CLASS*)malloc(sizeof(CLASS));
   c->name = (char*)malloc(strlen(name) + 1);
   c->id = -1;
   strcpy(c->name, name);
   c->means = means;
   c->cov_matrix = cov;
   c->nDim = means->size;
   //   CARTOCLASS_checkCovMat(c);
   // if not successful set flag
   //   gsl_set_error_handler_off();
   //   if(c->cov_matrix_inv = CARTOCLASS_getMatrixInv(c->cov_matrix) != GSL_SUCCESS)
   //     c->isValidCov = 0; 
   //   else c->cov_matrix_inv = NULL;
   c->cov_matrix_inv = gsl_matrix_alloc(cov->size1, cov->size2);
   gnu_errno = CARTOCLASS_getMatrixInv(c->cov_matrix, c->cov_matrix_inv);
   if(gnu_errno == GSL_SUCCESS || gnu_errno == GSL_ERUNAWAY)
      c->isValidCov = 1;
   else
      c->isValidCov = 0;

   //   CARTOCLASS_printMatrix(c->cov_matrix_inv);

   return c;
}

/**********************************************/
CLASS* CARTOCLASS_getClassByID(int id, gsl_vector *means, gsl_matrix *cov)
{
   CLASS *c;
   int gnu_errno;

   c = (CLASS*)malloc(sizeof(CLASS));
   
   c->id = id;
   c->name = NULL;
   c->means = means;
   c->cov_matrix = cov;
   c->nDim = means->size;
   /*
   CARTOCLASS_checkCovMat(c);
   if(c->isValidCov) c->cov_matrix_inv = CARTOCLASS_getMatrixInv(c->cov_matrix);
   else c->cov_matrix_inv = NULL;
   */
   // if not successful set flag
   c->cov_matrix_inv = gsl_matrix_alloc(cov->size1, cov->size2);
   gnu_errno = CARTOCLASS_getMatrixInv(c->cov_matrix, c->cov_matrix_inv);
   if(gnu_errno == GSL_SUCCESS || gnu_errno == GSL_ERUNAWAY)
      c->isValidCov = 1;
   else
      c->isValidCov = 0;

   return c;
}

/***********************************************/
void CARTOCLASS_checkCovMat(CLASS *c)
{
   int i, j, allZeros;

   for(i = 0; i < c->nDim; i++)
   {
      allZeros = 1;
      for(j = 0; j < c->nDim; j++)
         if(fabs(gsl_matrix_get(c->cov_matrix, i, j)) > 10E-10)
         {
            allZeros = 0;
            break;
         }

      if(allZeros)
      {
         c->isValidCov = 0;
         return;
      }
   }

   c->isValidCov = 1;
}

/***********************************************/
void CARTOCLASS_setCovMat(CLASS *c, gsl_matrix *cov_matrix)
{
   c->cov_matrix = cov_matrix;

   CARTOCLASS_checkCovMat(c);
}

/***********************************************/
void CARTOCLASS_setMean(CLASS *c, gsl_vector *means)
{
   c->means = means;
   if(c->nDim == -1) c->nDim = means->size;

   return;
}

/***********************************************/
void CARTOCLASS_copyMean(CLASS *c, gsl_vector *means)
{
   if(c->means == NULL)
      c->means = gsl_vector_alloc(means->size);

   gsl_vector_memcpy(c->means, means);
}

/***********************************************/
void CARTOCLASS_copyCovMat(CLASS *c, gsl_matrix *cov_matrix)
{
   if(c->cov_matrix == NULL)
      c->cov_matrix = gsl_matrix_alloc(cov_matrix->size1, cov_matrix->size2);

   gsl_matrix_memcpy(c->cov_matrix, cov_matrix);
}

/**********************************************/
void CARTOCLASS_setID(CLASS *c, int id)
{
   c->id = id;
}

/**********************************************/
void CARTOCLASS_setClassName(CLASS *c, char *name)
{
   if(c->name != NULL) free(c->name);
   c->name = (char*)malloc(strlen(name)+1);
   strcpy(c->name, name);
}

/***********************************************/
void checkAllocs(CLASS *c, CLASS_TMP *tmp)
{
   if(c->cov_matrix_inv == NULL)
      c->cov_matrix_inv = gsl_matrix_alloc(c->cov_matrix->size1, c->cov_matrix->size2);

   if(tmp->LU == NULL)
      tmp->LU = gsl_matrix_alloc(c->cov_matrix_inv->size1, c->cov_matrix_inv->size2);
   if(tmp->LU_Perm == NULL)
      tmp->LU_Perm = gsl_permutation_alloc(c->cov_matrix_inv->size1);
}

/***********************************************/
int CARTOCLASS_calcGaussianDensityAtX(CLASS *c, gsl_vector *x, CLASS_TMP *tmp, double *dist)
{
  /* LINE A and LINE B below were originally reversed in order, and
     the LU_det call was missing its signum parameter. So I (WLB) added
     signum, and reversed the lines, based on the assumption that
     LU_decomp initializes signum, and LU_det uses it. Note that
     LU_decomp does have side effects in its first arg, the
     matrix. There are no users of this function in vdev. PK, the
     author notes:

     You can check if the code is doing what we want by checking against python implementation.

     >>> from scipy.spatial.distance import mahalanobis
     >>> u = [1, 2]
     >>> v = [0, 0]
     >>> VI = [[2, 1], [2, 3]]
     >>> mahalanobis(u, v, VI)
     4.4721359549995796
     >>> VI = [[2, 1], [2, 2]]
     >>> mahalanobis(u, v, VI)
     4.0
     >>> VI = [[2, 1], [2, -2]]
     >>> mahalanobis(u, v, VI)
     0.0
     >>> VI = [[2, 1], [2, 1]]
     >>> mahalanobis(u, v, VI)
     3.4641016151377544

     Results from line mahaDist = CARTOCLASS_getMahalanobisDist(c, x, tmp); should yield the same results.
   */

   int gnu_errno, dim;
   int signum = 0;
   double det, mahaDist;

   checkAllocs(c, tmp);
   gsl_set_error_handler_off();
   gsl_matrix_memcpy(tmp->LU, c->cov_matrix);
   gnu_errno = gsl_linalg_LU_decomp(tmp->LU, tmp->LU_Perm, &signum); /* LINE B */
   det = gsl_linalg_LU_det(tmp->LU,signum); /* added signum, reversed order with previous line; LINE A */
//   printf("inside gauss dens at x err: %d\n", gnu_errno);
   if(gnu_errno != GSL_SUCCESS) return gnu_errno;
   mahaDist = CARTOCLASS_getMahalanobisDist(c, x, tmp);
   dim = x->size;

//   printf("piterm: %lf sqrt: %lf exp: %.9lf maha: %lf\n", 1/pow(2*M_PI, dim/2.), sqrt(det), exp(-0.5*mahaDist), mahaDist);

   *dist = 1/pow(2*M_PI, dim/2.)*1/sqrt(det)*exp(-0.5*mahaDist);

   return gnu_errno;
}

/**********************************************/
int CARTOCLASS_getMatrixInv(gsl_matrix *m, gsl_matrix *inv)
{
   gsl_matrix *mCopy;
   gsl_permutation *p;
   int gnu_errno;
   int signum;

   gsl_set_error_handler_off();

   p = gsl_permutation_alloc(m->size1);
   mCopy = gsl_matrix_alloc(m->size1, m->size2);
   gsl_matrix_memcpy(mCopy, m);
   //   CARTOCLASS_printMatrix(mCopy);

   gnu_errno = gsl_linalg_LU_decomp(mCopy, p, &signum);
   //   CARTOCLASS_printMatrix(mCopy);
   if(gnu_errno != GSL_SUCCESS)
   {
      gsl_matrix_free(mCopy);
      gsl_permutation_free(p);
      return gnu_errno;
   }
   gnu_errno = gsl_linalg_LU_invert(mCopy, p, inv);
   //   CARTOCLASS_printMatrix(inv);
   //   printf("errno: %d\n");
   gsl_permutation_free(p);
   gsl_matrix_free(mCopy);

   return gnu_errno;
}

/**********************************************/
int CARTOCLASS_setCovInv(CLASS *c, CLASS_TMP *tmp)
{
   int gnu_errno;
   int signum;

   checkAllocs(c, tmp);
   gsl_set_error_handler_off();
   gsl_matrix_memcpy(tmp->LU, c->cov_matrix);
   gnu_errno = gsl_linalg_LU_decomp(tmp->LU, tmp->LU_Perm, &signum);
   if(gnu_errno != GSL_SUCCESS)
      return gnu_errno;
   gnu_errno = gsl_linalg_LU_invert(tmp->LU, tmp->LU_Perm, c->cov_matrix_inv);
   return gnu_errno;
}

/**********************************************/
void CARTOCLASS_printClass(CLASS *class)
{
   int i, j;

   printf("name: %s\n", class->name);
   printf("means: ");
   for(i = 0; i < class->means->size; i++)
      printf("%lf ", gsl_vector_get(class->means, i));
   printf("\n");
   printf("covariance matrix:\n");
   for(i = 0; i < class->nDim; i++)
   {
      for(j = 0; j < class->nDim; j++)
         printf("%5.8lf\t", gsl_matrix_get(class->cov_matrix, i, j));
      printf("\n");
   }

   if(class->cov_matrix_inv == NULL) return;
   printf("inverse of covariance matrix:\n");
   for(i = 0; i < class->nDim; i++)
   {
      for(j = 0; j < class->nDim; j++)
         printf("%5.8lf\t", gsl_matrix_get(class->cov_matrix_inv, i, j));
      printf("\n");
   }
}

/**********************************************/
CLASS_TMP* CARTOCLASS_getClassTmp(int size)
{
   CLASS_TMP* tmp = (CLASS_TMP*)malloc(sizeof(CLASS_TMP));

   tmp->diff = gsl_vector_alloc(size);
   tmp->result = gsl_matrix_alloc(1, size);
   tmp->result2 = gsl_matrix_alloc(1, 1);
   tmp->LU = NULL;
   tmp->LU_Perm = NULL;

   return tmp;
}

/**********************************************/
void CARTOCLASS_freeClassTmp(CLASS_TMP** t)
{
   gsl_matrix_free((*t)->result);
   gsl_matrix_free((*t)->result2);
   gsl_vector_free((*t)->diff);
   if((*t)->LU != NULL) gsl_matrix_free((*t)->LU);
   if((*t)->LU_Perm != NULL) gsl_permutation_free((*t)->LU_Perm);
   free(*t);
}

/**********************************************/
double CARTOCLASS_getEuclideanDist(CLASS *class, gsl_vector *dns, CLASS_TMP *tmp)
{
   int i;
   gsl_matrix_view diffView, diffView2;

   if(class->means == NULL)
      zmabend("Mean for class was not set before calling getEuclideanDist.\n");

   gsl_vector_memcpy(tmp->diff, dns);
   gsl_vector_sub(tmp->diff, class->means);
   //   CARTOCLASS_printVector(dns);
   diffView = gsl_matrix_view_vector(tmp->diff, 1, dns->size);
   diffView2 = gsl_matrix_view_vector(tmp->diff, 1, dns->size);

   gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1, &diffView.matrix, &diffView2.matrix, 0, tmp->result2);

   return sqrt(gsl_matrix_get(tmp->result2, 0, 0));
}

/**********************************************/
double CARTOCLASS_getMahalanobisDist(CLASS *class, gsl_vector *dns, CLASS_TMP *tmp)
{
   int i, j;
   double distance;
   gsl_matrix_view diffView;

   if(!class->isValidCov)
      zmabend("Does not have a valid covariance matrix.\n");
   if(class->cov_matrix_inv == NULL)
      zmabend("Covariance matrix inverse was not set before calling getMahalanobisDist.\n");

   //   printf("getMahalanobisDist 1 len: %d len: %d\n", tmp->diff->size, dns->size);
   gsl_vector_memcpy(tmp->diff, dns);
   //   printf("getMahalanobisDist 2\n");
   gsl_vector_sub(tmp->diff, class->means);
   diffView = gsl_matrix_view_vector(tmp->diff, 1, dns->size);

   /*
   for(i = 0; i < dns->size; i++) printf("%lf ", gsl_vector_get(dns, i));
   printf("\n");
   printf("inverse: %d %d\n", class->cov_matrix_inv->size1, class->cov_matrix_inv->size2);
   for(i = 0; i < class->cov_matrix_inv->size1; i++)
      for(j = 0; j < class->cov_matrix_inv->size2; j++)
         printf("%lf ", gsl_matrix_get(class->cov_matrix_inv, i, j));
   printf("\n");
   */

   gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1, &diffView.matrix, class->cov_matrix_inv, 0, tmp->result);
   gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1, tmp->result, &diffView.matrix, 0, tmp->result2);

   distance = sqrt(fabs(gsl_matrix_get(tmp->result2, 0, 0)));

   return distance;
}

/**********************************************/
CLASS** CARTOCLASS_loadClassesFromIBIS(IBISStruct *ibis)
{
   int i, nDim, colIndex;
   char name[20];
   CLASS **classes;
   gsl_vector *means;
   gsl_matrix *cov_mat;

   classes = (CLASS**)malloc(sizeof(CLASS*)*ibis->nr);
   for(i = 0; i < ibis->nr; i++)
   {
      int j, k;

      IBISHELPER_getString(ibis, name, 0, i);
      nDim = IBISHELPER_getInt(ibis, 2, i);

      means = gsl_vector_alloc(nDim);
      cov_mat = gsl_matrix_alloc(nDim, nDim);

      for(j = 0; j < nDim; j++)
      {
         // printf("loadClassesFromIBIS j: %d\n", j);
         gsl_vector_set(means, j, IBISHELPER_getDouble(ibis, j+3, i));
      }

      colIndex = 3 + nDim;
      for(j = 0; j < nDim; j++)
         for(k = 0; k <= j; k++)
         {
            double cov;
            //            printf("1. ndim: %d colIndex: %d %d %d\n", nDim, colIndex, j, k);
            cov = IBISHELPER_getDouble(ibis, colIndex++, i);
            //            printf("2. colIndex: %d %d %d\n", colIndex, j, k);

            //            printf("cov: %.9lf j: %d k: %d\n", cov, j, k);
            gsl_matrix_set(cov_mat, j, k, cov);
            gsl_matrix_set(cov_mat, k, j, cov);
         }

      //      printf("here\n");
      classes[i] = CARTOCLASS_getClassByName(name, means, cov_mat);
      classes[i]->nSamples = IBISHELPER_getInt(ibis, 1, i);
      //      CARTOCLASS_printClass(classes[i]);
   }

   return classes;
}

/**********************************************/
void CARTOCLASS_freeClass(CLASS **c)
{
   if((*c)->name != NULL) free((*c)->name);

   if((*c)->means != NULL) gsl_vector_free((*c)->means);
   if((*c)->cov_matrix != NULL) gsl_matrix_free((*c)->cov_matrix);
   if((*c)->cov_matrix_inv != NULL) gsl_matrix_free((*c)->cov_matrix_inv);

   free(*c);
}
