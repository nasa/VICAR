/* Matrix-Vector-Etc functions.						*/
/* This is a package from Gerhard Klimeck of section 381.  It performs	*/
/* memory management for matrices and vectors in C.  See the .c file	*/
/* for documentation.							*/
/* The original was named "mve.h"; renamed to "mvec.h" to avoid		*/
/* confusion with the VICAR mve subroutine.				*/
/* Installed into VICAR 2/2003 by B. Deen				*/

#ifndef MVEC_H
#define MVEC_H 1

#include "xvmaininc.h"		/* just for __cplusplus */
#ifdef __cplusplus
extern "C" {
#endif

#define mve_sint_ptr(p) (( int *) p)
#define mve_svdim(p)    *mve_sint_ptr((p-2))
#define mve_srdim(p)    mve_svdim(p)
#define mve_scdim(p)    *mve_sint_ptr((p-4))

#define mve_int_ptr(p) ((int *) p)
#define mve_vdim(p)    *mve_int_ptr((p-1))
#define mve_rdim(p)    mve_vdim(p)
#define mve_cdim(p)    *mve_int_ptr((p-2))

typedef short int   *mve_sivectr;
typedef mve_sivectr *mve_simatrix;
typedef int         *mve_ivectr;
typedef double      *mve_dvectr;
typedef mve_dvectr  *mve_dmatrix;
typedef float       *mve_fvectr;
typedef mve_fvectr  *mve_fmatrix;


mve_sivectr mve_Sivectr       ( int  n);
void        mve_rm_sivectr    ( mve_sivectr  *v_ptr);
mve_sivectr mve_check_sivectr ( mve_sivectr v, int n);


mve_simatrix mve_Simatrix       ( int  n, int m);
void         mve_rm_simatrix    ( mve_simatrix  *a_ptr);
mve_simatrix mve_check_simatrix ( mve_simatrix a, int n, int m);

mve_ivectr  mve_Ivectr          ( int  n);
void        mve_rm_ivectr       ( mve_ivectr  *v_ptr);
mve_ivectr  mve_check_ivectr    ( mve_ivectr v, int n);

mve_dvectr   mve_Dvectr         ( int  n);
void         mve_rm_dvectr      ( mve_dvectr  *v_ptr);
mve_dvectr   mve_check_dvectr   ( mve_dvectr v, int n);

mve_dmatrix  mve_Dmatrix        ( int  n,  int m);
void         mve_rm_dmatrix     ( mve_dmatrix  *a_ptr);
mve_dmatrix  mve_check_dmatrix  ( mve_dmatrix a, int n, int m);

mve_fvectr   mve_Fvectr         ( int  n);
void         mve_rm_fvectr      ( mve_fvectr  *v_ptr);
mve_fvectr   mve_check_fvectr   ( mve_fvectr v, int n);

mve_fmatrix  mve_Fmatrix        ( int  n,  int m);
void         mve_rm_fmatrix     ( mve_fmatrix  *a_ptr);
mve_fmatrix  mve_check_fmatrix  ( mve_fmatrix a, int n, int m);

#ifdef __cplusplus
}
#endif

#endif
