/* Matrix-Vector-Etc functions.                                         */
/* This is a package from Gerhard Klimeck of section 381.  It performs  */
/* memory management for matrices and vectors in C.			*/
/* The original was named "mve.c"; renamed to "mvec.c" to avoid         */
/* confusion with the VICAR mve subroutine.                             */
/* Installed into VICAR 2/2003 by B. Deen                               */

/*
  Copyright JPL (2001)
  Author: Gerhard Klimeck
  Source code augmented from original NEMO version, similar to numerical recipes.
*/

/* 
   mve = matrix-vector-etc

   This file contains various vector and matrix definitions.
   All of the data structures are dynamically defined.

   The datastructures have associated construction and 
   destruction function calls.

   There are "check" functions that compare the present memory available
   in a data structure to a requested size.   If the two sizes do not match	
   the original memory is deallocated an a new object is created.
   The original object is returned if the size check was successful.

   All the data objects carry an explicit size information with them
   in that the needed pointer memory is increased by 
   a few bytes, the size information is written into 
   the first few bytes and the pointer is advanced just beyond that size 
   information.

   All the data is allocated consecutive, without auxiliary pointers
   in the center of the data chunk.  That is essential for some
   of the data management with MPI.
*/

#include "mvec.h"

#include <stdio.h>
#include <stdlib.h>


/* Create a vector of short int with length n */
mve_sivectr mve_Sivectr ( int  n)
{
  mve_sivectr v;

  v = (short int *) calloc ((n + 2), sizeof (short int));

  if (v == NULL)
    fprintf (stderr, "mve_Sivectr unable to allocate memory.");

  v += 2;
  mve_svdim (v) = n;
  return v;
}      

/* Destroy the memory associated with a vector of short int. */   
void mve_rm_sivectr ( mve_sivectr  *v_ptr)
{
  mve_sivectr  v;
  
  if (!v_ptr || !(v=*v_ptr))
    return;
  v -= 2;
  free (v);
  *v_ptr = NULL;
  return;
}

/* Check the size of vector of short int against a desired
   length n.  If the length of the vector does not match 
   the desired length, destroy the vecor and allocate the desired
   memory. */
mve_sivectr mve_check_sivectr (mve_sivectr v, int n)
{
    if (v != NULL)
	if (mve_svdim (v) != n)
	{
	    mve_rm_sivectr (&v);
	}
    if (v == NULL)
	v = mve_Sivectr (n);
    return v;
}

/* Creade a matrix of short int of dimension n x m.
   The second index is the fast running one.
   The associated memory is allocated in ONE CHUNK. */
mve_simatrix mve_Simatrix (  int  n,  int m)
{
  mve_simatrix a;
  int i, j;

  a = (mve_simatrix) malloc ((n + 6) * sizeof (mve_sivectr));
  if (a == NULL) fprintf (stderr, "mve_Simatrix unable to allocate memory.");
  a += 6;
  mve_srdim (a) = n;
  mve_scdim (a) = m;
  *a = mve_Sivectr (n * m);
  for (i = 0; i < n; i++)
    {
      a[i] = a[0] + i * m;
      for (j = 0; j < m; j++)
	a[i][j] = 0;
    }
  return a;
} 


/* Destroy a matrix of short int */
void mve_rm_simatrix ( mve_simatrix  *a_ptr)
{
  mve_simatrix a;
  if (!a_ptr || !(a=*a_ptr))
    return;

  mve_rm_sivectr (a);
  free (a - 6);
  
  *a_ptr = NULL;
  return;
} 

/* Check the dimensions of a matrix against a requested size and
   return a matrix of desired size - either the original on, if the sizes
   match, or a new matrix (destroying the old one).
*/
mve_simatrix mve_check_simatrix (mve_simatrix a, int n, int m)
{
    if (a != NULL)
	if (mve_srdim (a) != n || mve_scdim (a) != m)
	{
	  fprintf(stderr,"Resizing mve_simatrix from (%d,%d) to (%d,%d)\n",mve_srdim (a),mve_scdim (a),n,m);
	    mve_rm_simatrix (&a);
	}
    if (a == NULL)
	a = mve_Simatrix (n, m);
    return a;
}


/* Create a vector of int with length n */
mve_ivectr mve_Ivectr (   int  n)
{
  mve_ivectr v;

  v = (int *) calloc ((n + 1), sizeof (int));

  if (v == NULL)
    fprintf (stderr, "mve_Ivectr unable to allocate memory.");

  v += 1;
  mve_vdim (v) = n;
  return v;
}  

       
/* Destroy the memory associated with a vector of int. */   
void mve_rm_ivectr ( mve_ivectr  *v_ptr)
{
  mve_ivectr  v;
  
  if (!v_ptr || !(v=*v_ptr))
    return;
  v -= 1;
  free (v);
  *v_ptr = NULL;
  return;
}


/* Check the size of vector of int against a desired
   length n.  If the length of the vector does not match 
   the desired length, destroy the vecor and allocate the desired
   memory. */
mve_ivectr mve_check_ivectr (mve_ivectr v, int n)
{
    if (v != NULL)
	if (mve_vdim (v) != n)
	{
	    mve_rm_ivectr (&v);
	}
    if (v == NULL)
	v = mve_Ivectr (n);
    return v;
}



/* Create a vector of double with length n */
mve_dvectr mve_Dvectr (   int  n)
{
  mve_dvectr v;

  v = (double *) calloc ((n + 1), sizeof (double));

  if (v == NULL)
    fprintf (stderr, "mve_Dvectr unable to allocate memory.");

  v += 1;
  mve_vdim (v) = n;
  return v;
}  

       
/* Destroy the memory associated with a vector of double. */   
void mve_rm_dvectr ( mve_dvectr  *v_ptr)
{
  mve_dvectr  v;
  
  if (!v_ptr || !(v=*v_ptr))
    return;
  v -= 1;
  free (v);
  *v_ptr = NULL;
  return;
}


/* Check the size of vector of double against a desired
   length n.  If the length of the vector does not match 
   the desired length, destroy the vecor and allocate the desired
   memory. */
mve_dvectr mve_check_dvectr (mve_dvectr v, int n)
{
    if (v != NULL)
	if (mve_vdim (v) != n)
	{
	    mve_rm_dvectr (&v);
	}
    if (v == NULL)
	v = mve_Dvectr (n);
    return v;
}


/* Creade a matrix of double of dimension n x m.
   The second index is the fast running one.
   The associated memory is allocated in ONE CHUNK. */
mve_dmatrix mve_Dmatrix (   int  n,  int m)
{
  mve_dmatrix a;
  int i, j;

  a = (mve_dmatrix) malloc ((n + 3) * sizeof (mve_dvectr));
  if (a == NULL) fprintf (stderr, "mve_Dmatrix unable to allocate memory.");
  a += 3;
  mve_rdim (a) = n;
  mve_cdim (a) = m;
  
  *a = mve_Dvectr (n * m);
  for (i = 0; i < n; i++)
    {
      a[i] = a[0] + i * m;
      for (j = 0; j < m; j++)
	a[i][j] = 0;
    }
  return a;
} 


/* Destroy a matrix of double */
void mve_rm_dmatrix ( mve_dmatrix  *a_ptr)
{
  mve_dmatrix a;
  if (!a_ptr || !(a=*a_ptr))
    return;

  mve_rm_dvectr (a);
  free (a - 3);
  
  *a_ptr = NULL;
  return;
} 


/* Check the dimensions of a matrix against a requested size and
   return a matrix of desired size - either the original on, if the sizes
   match, or a new matrix (destroying the old one).
*/
mve_dmatrix mve_check_dmatrix (mve_dmatrix a, int n, int m)
{
    if (a != NULL)
	if (mve_rdim (a) != n || mve_cdim (a) != m)
	{
	  fprintf(stderr,"Resizing mve_dmatrix from (%d,%d) to (%d,%d)\n",mve_rdim (a),mve_cdim (a),n,m);
	    mve_rm_dmatrix (&a);
	}
    if (a == NULL)
	a = mve_Dmatrix (n, m);
    return a;
}

/* Create a vector of float with length n */
mve_fvectr mve_Fvectr (   int  n)
{
  mve_fvectr v;

  v = (float *) calloc ((n + 1), sizeof (float));

  if (v == NULL)
    fprintf (stderr, "mve_Fvectr unable to allocate memory.");

  v += 1;
  mve_vdim (v) = n;
  return v;
}  

       
/* Destroy the memory associated with a vector of float. */   
void mve_rm_fvectr ( mve_fvectr  *v_ptr)
{
  mve_fvectr  v;
  
  if (!v_ptr || !(v=*v_ptr))
    return;
  v -= 1;
  free (v);
  *v_ptr = NULL;
  return;
}


/* Check the size of vector of double against a desired
   length n.  If the length of the vector does not match 
   the desired length, destroy the vecor and allocate the desired
   memory. */
mve_fvectr mve_check_fvectr (mve_fvectr v, int n)
{
    if (v != NULL)
	if (mve_vdim (v) != n)
	{
	    mve_rm_fvectr (&v);
	}
    if (v == NULL)
	v = mve_Fvectr (n);
    return v;
}


/* Creade a matrix of float of dimension n x m.
   The second index is the fast running one.
   The associated memory is allocated in ONE CHUNK. */
mve_fmatrix mve_Fmatrix (   int  n,  int m)
{
  mve_fmatrix a;
  int i, j;

  a = (mve_fmatrix) malloc ((n + 3) * sizeof (mve_fvectr));
  if (a == NULL) fprintf (stderr, "mve_Fmatrix unable to allocate memory.");
  a += 3;
  mve_rdim (a) = n;
  mve_cdim (a) = m;
  
  *a = mve_Fvectr (n * m);
  for (i = 0; i < n; i++)
    {
      a[i] = a[0] + i * m;
      for (j = 0; j < m; j++)
	a[i][j] = 0;
    }
  return a;
} 


/* Destroy a matrix of double */
void mve_rm_fmatrix ( mve_fmatrix  *a_ptr)
{
  mve_fmatrix a;
  if (!a_ptr || !(a=*a_ptr))
    return;

  mve_rm_fvectr (a);
  free (a - 3);
  
  *a_ptr = NULL;
  return;
} 


/* Check the dimensions of a matrix against a requested size and
   return a matrix of desired size - either the original on, if the sizes
   match, or a new matrix (destroying the old one).
*/
mve_fmatrix mve_check_fmatrix (mve_fmatrix a, int n, int m)
{
    if (a != NULL)
	if (mve_rdim (a) != n || mve_cdim (a) != m)
	{
	  fprintf(stderr,"Resizing mve_fmatrix from (%d,%d) to (%d,%d)\n",mve_rdim (a),mve_cdim (a),n,m);
	    mve_rm_fmatrix (&a);
	}
    if (a == NULL)
	a = mve_Fmatrix (n, m);
    return a;
}
