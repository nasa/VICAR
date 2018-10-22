#ifndef CARTOCLASSUTILS
#define CARTOCLASSUTILS

#include "gsl/gsl_vector.h"
#include "gsl/gsl_permutation.h"
#include "gsl/gsl_matrix.h"
#include "ibishelper.h"

/////////////////////////////////////////////////
typedef struct
{
   char *name;
   int id;
   int nSamples;
   gsl_vector *means;
   gsl_matrix *cov_matrix;
   gsl_matrix *cov_matrix_inv;
   int isValidCov;
   int nDim;
}CLASS;

/////////////////////////////////////////////////
typedef struct
{
   gsl_vector *diff;
   gsl_matrix *result, *result2;
   gsl_matrix *LU;
   gsl_permutation *LU_Perm;
}CLASS_TMP;

/***********************************************/
CLASS* CARTOCLASS_getClass();

/***********************************************/
CLASS* CARTOCLASS_getClassByName(char *name, gsl_vector *means, gsl_matrix *cov);

/**********************************************/
CLASS* CARTOCLASS_getClassByID(int id, gsl_vector *means, gsl_matrix *cov);

/***********************************************/
void CARTOCLASS_setMean(CLASS *c, gsl_vector *means);

/***********************************************/
void CARTOCLASS_setCovMat(CLASS *c, gsl_matrix *cov_matrix);

/***********************************************/
void CARTOCLASS_copyMean(CLASS *c, gsl_vector *means);

/***********************************************/
void CARTOCLASS_copyCovMat(CLASS *c, gsl_matrix *cov_matrix);

/**********************************************/
void CARTOCLASS_setID(CLASS *c, int id);

/**********************************************/
void CARTOCLASS_setClassName(CLASS *c, char *name);

/**********************************************/
int CARTOCLASS_getMatrixInv(gsl_matrix *m, gsl_matrix *inv);

/**********************************************/
void CARTOCLASS_printClass(CLASS *class);

/**********************************************/
CLASS_TMP* CARTOCLASS_getClassTmp(int size);

/**********************************************/
void CARTOCLASS_freeClassTmp(CLASS_TMP** t);

/**********************************************/
double CARTOCLASS_getEuclideanDist(CLASS *class, gsl_vector *dns, CLASS_TMP *tmp);

/**********************************************/
double CARTOCLASS_getMahalanobisDist(CLASS *class, gsl_vector *dns, CLASS_TMP *tmp);

/**********************************************/
CLASS** CARTOCLASS_loadClassesFromIBIS(IBISStruct *ibis);

/***********************************************/
void CARTOCLASS_checkCovMat(CLASS *c);

/***********************************************/
int CARTOCLASS_calcGaussianDensityAtX(CLASS *c, gsl_vector *x, CLASS_TMP *tmp, double *det);

/***********************************************/
void CARTOCLASS_freeClass(CLASS **c);
#endif
