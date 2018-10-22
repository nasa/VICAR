#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "zmabend.h"
#include "cartoSortUtils.h"
#include "cartoStrUtils.h"

/***************************************************/
/* This function is a helper function for          */
/* get*SortIndices to swap the indices.            */
/***************************************************/
void swap(int *array, int i, int j)
{
   int tmp;

   tmp = array[i];
   array[i] = array[j];
   array[j] = tmp;
}

/***************************************************/
/* This function is a helper function for          */
/* get*SortIndices to compare different data       */
/* types.                                          */
/*                                                 */
/* IN: void *array - buffer containing data to     */
/*                   compare                       */
/*     int index1 - index of element to compare    */
/*     int index2 - index of element to compare    */
/*     int type - type of elements in array        */
/*           1 = char                              */
/*           2 = short int                         */
/*           3 = int                               */
/*           4 = float                             */
/*           5 = double                            */
/*           6 = long int                          */
/*           7 = unsigned char                     */
/*           8 = unsigned short int                */
/*           9 = unsigned int                      */
/*           10 = unsigned long int                */
/*                                                 */
/* RETURN: -1 if index1 is greater than index2     */
/*         0  if equal                             */
/*         1  if index2 is greater than index1     */
/***************************************************/
int compare(void *array, int index1, int index2, int type)
{
   switch(type)
   {
      case CART_CHAR:
         if(((char*)array)[index1] == ((char*)array)[index2]) return 0;
	 if(((char*)array)[index1] > ((char*)array)[index2]) return -1;
         if(((char*)array)[index1] < ((char*)array)[index2]) return 1;
      case CART_SHORT:
         if(((short int*)array)[index1] == ((short int*)array)[index2]) return 0;
	 if(((short int*)array)[index1] > ((short int*)array)[index2]) return -1;
         if(((short int*)array)[index1] < ((short int*)array)[index2]) return 1;
      case CART_INT:
         if(((int*)array)[index1] == ((int*)array)[index2]) return 0;
	 if(((int*)array)[index1] > ((int*)array)[index2]) return -1;
         if(((int*)array)[index1] < ((int*)array)[index2]) return 1;
      case CART_FLOAT:
         if(((float*)array)[index1] == ((float*)array)[index2]) return 0;
	 if(((float*)array)[index1] > ((float*)array)[index2]) return -1;
         if(((float*)array)[index1] < ((float*)array)[index2]) return 1;
      case CART_DOUBLE:
         if(((double*)array)[index1] == ((double*)array)[index2]) return 0;
	 if(((double*)array)[index1] > ((double*)array)[index2]) return -1;
         if(((double*)array)[index1] < ((double*)array)[index2]) return 1;
      case CART_LONG:
         if(((long int*)array)[index1] == ((long int*)array)[index2]) return 0;
	 if(((long int*)array)[index1] > ((long int*)array)[index2]) return -1;
         if(((long int*)array)[index1] < ((long int*)array)[index2]) return 1;
      case CART_USHORT:
         if(((unsigned short int*)array)[index1] == ((unsigned short int*)array)[index2]) return 0;
	 if(((unsigned short int*)array)[index1] > ((unsigned short int*)array)[index2]) return -1;
         if(((unsigned short int*)array)[index1] < ((unsigned short int*)array)[index2]) return 1;
      case CART_UINT:
         if(((unsigned int*)array)[index1] == ((unsigned int*)array)[index2]) return 0;
	 if(((unsigned int*)array)[index1] > ((unsigned int*)array)[index2]) return -1;
         if(((unsigned int*)array)[index1] < ((unsigned int*)array)[index2]) return 1;
      case CART_ULONG:
         if(((unsigned long int*)array)[index1] == ((unsigned long int*)array)[index2]) return 0;
	 if(((unsigned long int*)array)[index1] > ((unsigned long int*)array)[index2]) return -1;
         if(((unsigned long int*)array)[index1] < ((unsigned long int*)array)[index2]) return 1;
   }

   return CANNOT_COMPARE;
}

/***************************************************/
/* This function performs selection sort on the    */
/* unsorted array and stores the SORTED ORDER      */
/* INDICES into indices array.  This function      */
/* does not move around the data but only returns  */
/* what the sorted index would be inside indices.  */
/*                                                 */
/* IN: void *unsorted - buffer containing unsorted */
/*     data                                        */
/*     int n - number of entries in unsorted buf   */
/*     int type - type of elements in unsorted     */
/*                buffer                           */
/*           1 = char                              */
/*           2 = short int                         */
/*           3 = int                               */
/*           4 = float                             */
/*           5 = double                            */
/*           6 = long int                          */
/*           7 = unsigned char                     */
/*           8 = unsigned short int                */
/*           9 = unsigned int                      */
/*           10 = unsigned long int                */
/*                                                 */
/* OUT: int *indices - returns sorted index order  */
/***************************************************/
void getSelectionSortIndices(void *unsorted, int *indices, int n, int type)
{
   int i;

   for(i = 0; i < n; i++) indices[i] = i;

   for(i = 0; i < n-1; i++)
   {
      int j, minv;
      minv = i;

      for(j = i+1; j < n; j++)
      {
         int cmpResult;

	 cmpResult = compare(unsorted, indices[minv], indices[j], type);
	 assert(cmpResult != CANNOT_COMPARE);

	 if(cmpResult == -1) minv = j;
      }

      if(i != minv) swap(indices, i, minv);
   }
}

void sort8( double * buf, int * ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sort88( double * buf, int * ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com; sorts a vector (x,y,x,y,x,y...) on x
      then on y */
      
      int l,m,k,j,iptr;
      double ibufx,ibufy;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l25;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l45;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      ibufx = buf[0];
      ibufy = buf[1];
      iptr = ptr[0];
      buf[0] = buf[2*l];
      buf[1] = buf[2*l+1];
      ptr[0] = ptr[l];
      buf[2*l] = ibufx;
      buf[2*l+1] = ibufy;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sortrec4( int * key, int * ptr, int len )
{
   int i,*temp;
   
   if (len<2) return;
   if ((temp=(int *)malloc(4*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void sortrec88( double * key, int * ptr, int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(16*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) 
      {
      temp[i*2] = key[i*2];
      temp[i*2+1] = key[i*2+1];
      }
   for (i=0;i<len;i++)
      {
      key[i*2] = temp[ptr[i]*2-2];
      key[i*2+1] = temp[ptr[i]*2-1];
      }
   free(temp);
   return;
}

void sortrec8( double *key, int* ptr,int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(8*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void sortretn8( double *key, int* ptr, int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(8*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[ptr[i]-1] = temp[i];
   free(temp);
   return;
}

void sort4(int *buf, int *ptr, int n)
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sort7( float *buf, int *ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}
