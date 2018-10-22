#include <math.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "cartoTaeUtils.h"

#define NOTFOUND   0
#define NORTH      1
#define SOUTH      2
#define EAST       3
#define WEST       4
#define N_COORDS   4
#define N_DMS      8
#define MAX_STRING 100
#define POSITIVE   1
#define NEGATIVE   0

/*  parses gtstring and returns then in TAE TCL variables
                                          P. Kim  4/17/09   */

typedef struct
{
   int ns;
   int ew;
}COORDINATE;

typedef struct
{
   int degrees;
   int minutes;
   int seconds;
   int posNeg;
}DMS;

/********************************************************/
double getDecimalDegree(int num)
{
   int degree, min, sec;

   degree = num/10000;
   min = (num/100)%100;
   sec = num%100;

   /*
   printf("*******************\n");
   printf("num: %d\n", num);
   printf("deg: %d\n", degree);
   printf("min: %d\n", min);
   printf("sec: %d\n", sec);
   */

   return degree + min/(double)60 + sec/(double)3600;
}

/********************************************************/
DMS* getDMS(double num)
{
   DMS* dms;

   dms = (DMS*)malloc(sizeof(DMS));

   if(num < .0)
      dms->posNeg = NEGATIVE;
   else
      dms->posNeg = POSITIVE;

   num = fabs(num);
   dms->degrees = (int)num;
   num -= dms->degrees;
   num = num*60;
   dms->minutes = (int)num;
   num -= dms->minutes;
   num = num*60;
   dms->seconds = (int)num;

   return dms;
}

/********************************************************/
void errorOut(char *orig)
{
   printf("Incorrect igeolo string encountered.\n");
   printf("IGEOLO STRING: %s\n", orig);
   zabend();
}

/********************************************************/
void stringToVal(char *orig)
{
   int i, j;
   COORDINATE coords[N_COORDS];
   char *input, *strptr;
   int found;

   input = orig;
   strptr = input;
   for(i = 0; i < N_COORDS; i++)
   {
      found = NOTFOUND;
      for(j = 0; j < strlen(strptr); j++)
      {
         if(strptr[j] == 'N')
         {
            found = NORTH;
            break;
         }
         if(strptr[j] == 'S')
         {
            found = SOUTH;
            break;
         }
      }
      if(!found) errorOut(orig);
      //      printf("input: %s\n", strptr);
      sscanf(input, "%d", &(coords[i].ns));
      if(found == SOUTH) coords[i].ns *= -1;
      strptr += j+1;
      input = strptr;

      found = 0;
      for(j = 0; j < strlen(strptr); j++)
      {
         if(strptr[j] == 'E')
         {
            found = EAST;
            break;
         }
         if(strptr[j] == 'W')
         {
            found = WEST;
            break;
         }
      }
      if(!found) errorOut(orig);
      //      printf("input: %s\n", strptr);
      sscanf(input, "%d", &(coords[i].ew));
      if(found == WEST) coords[i].ew *= -1;
      strptr += j+1;
      input = strptr;
   }

   mq_out_real("n1", getDecimalDegree(coords[0].ns));
   mq_out_real("n2", getDecimalDegree(coords[1].ns));
   mq_out_real("n3", getDecimalDegree(coords[2].ns));
   mq_out_real("n4", getDecimalDegree(coords[3].ns));
   mq_out_real("e1", getDecimalDegree(coords[0].ew));
   mq_out_real("e2", getDecimalDegree(coords[1].ew));
   mq_out_real("e3", getDecimalDegree(coords[2].ew));
   mq_out_real("e4", getDecimalDegree(coords[3].ew));
}

/********************************************************/
void valToString()
{
   int i, status, cnt, def;
   char *str;
   DMS* dms[N_DMS]; // 1st 4 is ns, last 4 is ew, total 8
   double n1, n2, n3, n4, e1, e2, e3, e4;

   status = zvparmd("n1inp", &n1, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("n2inp", &n2, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("n3inp", &n3, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("n4inp", &n4, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("e1inp", &e1, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("e2inp", &e2, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("e3inp", &e3, &cnt, &def, 1, 0);
   assert(status == 1);
   status = zvparmd("e4inp", &e4, &cnt, &def, 1, 0);
   assert(status == 1);

   dms[0] = getDMS(n1);
   dms[1] = getDMS(n2);
   dms[2] = getDMS(n3);
   dms[3] = getDMS(n4);
   dms[4] = getDMS(e1);
   dms[5] = getDMS(e2);
   dms[6] = getDMS(e3);
   dms[7] = getDMS(e4);

   str = calloc(sizeof(char), MAX_STRING);
   for(i = 0; i < N_COORDS; i++)
   {
      char coord[MAX_STRING];
      sprintf(coord, "%d%02d%02d%c", dms[i]->degrees, dms[i]->minutes,
                                 dms[i]->seconds, (dms[i]->posNeg)?'N':'S');
      strcat(str, coord);
      sprintf(coord, "%d%02d%02d%c", dms[i+4]->degrees, dms[i+4]->minutes,
                                 dms[i+4]->seconds, (dms[i+4]->posNeg)?'E':'W');
      strcat(str, coord);

      free(dms[i]);
      free(dms[i+4]);
   }

   mq_out_string("out", str, MAX_STRING);
   free(str);
}

/********************************************************/
void main44(void)
{
   int cnt, def;
   char orig[MAX_STRING];

   zifmessage("gtigeolo version 2017-08-08");
   
   /* get the input string */   
   zvparm("inp", orig, &cnt, &def, 1, 0);

   if(strlen(orig)) stringToVal(orig);
   else valToString();
}
