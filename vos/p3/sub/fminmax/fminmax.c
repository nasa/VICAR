#include <stdlib.h>
#include <string.h>


/*---------------------------------------------------------------------------*/
int fileminmax (unit, format, nl, ns, filemin, filemax,zeroes)
  char *format;    /* format of input file                              */
  int    unit,       /* vicar unit number of file, must be open for read  */
         nl,         /* number of lines of input file                     */
         ns,         /* number of samples of input file                   */
         zeroes;     /* use zeroes as data ?                              */
  double *filemin,   /* returned value - minimum value found              */
         *filemax;   /* returned value - maximum value found              */
{
   int stat;

   if (strcmp(format,"BYTE") == 0)
      stat = byteminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"HALF") == 0)
      stat = halfminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"FULL") == 0)
      stat = fullminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"REAL") == 0)
      stat = realminmax (unit, nl, ns, filemin, filemax,zeroes);
   else if (strcmp(format,"DOUB") == 0)
      stat = doubminmax (unit, nl, ns, filemin, filemax,zeroes);

   return stat;
}


/*---------------------------------------------------------------------------*/
int byteminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   char  *buf;
   int line, samp, status, first=0;
   double value;

   buf = (char *)malloc(sizeof(*buf)*ns);
   memset (buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(char *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free (buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int halfminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   short int  *buf;
   int line, samp, status, first=0;
   double value;

   buf = (short *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(short *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}


/*---------------------------------------------------------------------------*/
int fullminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   int  *buf;
   int line, samp, status, first=0;
   double value;
   
   buf = (int *)malloc(sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(int *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int realminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   float  *buf;
   int line, samp, status, first=0;
   double value;
   
   buf = (float *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = (double)(*(float *)(buf + samp));
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}

/*---------------------------------------------------------------------------*/
int doubminmax (unit, nl, ns, filemin, filemax,zeroes)
  int unit, nl, ns, zeroes;
  double *filemin, *filemax;
{
   double  *buf, value;
   int line, samp, status, first=0;
   
   buf = (double *)malloc(sizeof(*buf)*ns);
   memset ((char *)buf, 0, sizeof(*buf)*ns);

   for (line = 1; line <= nl; line++) {
      status = zvread(unit,buf,"LINE",line,"");
      for (samp = 0; samp < ns; samp++) {
         value = *(double *)(buf + samp);
         if (zeroes || value != 0) {
            if (!first) {
               first = 1;
               *filemax = value;
               *filemin = value;
            }
            *filemax = *filemax >= value ? *filemax : value;
            *filemin = *filemin <= value ? *filemin : value;
	 }
      }
   }
   free ((char *)buf);
   return first;
}
