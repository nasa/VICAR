#include "vicmain_c.h"
#include "applic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zifmessage.h"
#include "gsl/gsl_vector.h"
#include "ImageUtils.h"
#include "ibishelper.h"
#include "cartoLinkedList.h"

/**********************************************/
typedef struct
{
   int line, sample;
   gsl_vector *dns;
}SAMPLE;

/**********************************************/
void printSample(SAMPLE *sample)
{
   int i;

   printf("line: %d sample: %d\n", sample->line, sample->sample);
   printf("dn: ");
   for(i = 0; i < sample->dns->size; i++) printf("%lf ", gsl_vector_get(sample->dns, i));
   printf("\n");
}

/**********************************************/
void freeSample(SAMPLE **sample)
{
   gsl_vector_free((*sample)->dns);
   free(*sample);
}

/**********************************************/
VICAR_IMAGE** getVicarImages(int cnt)
{
   int i;
   VICAR_IMAGE **inps;

   inps = (VICAR_IMAGE**)malloc(sizeof(VICAR_IMAGE*)*cnt);
   for(i = 0; i < cnt; i++)
      inps[i] = getVI_inp(i+1);

   return inps;
}

/**********************************************/
void readSamples(LINKEDLIST *samples, VICAR_IMAGE **inps, int nDim, int nSamples)
{
   int i, j;
   struct node *n, *prev;
   SAMPLE *s, *prevS;

   n = samples->head;
   s = (SAMPLE*)(n->data);
   for(j = 0; j < nDim; j++)
   {
      readVicarImageLine(inps[j], s->line);
      gsl_vector_set(s->dns, j, inps[j]->buffer[s->sample]);
   }
   for(i = 1; i < samples->size; i++)
   {
      prev = n;
      n = n->next;
      prevS = s;
      s = (SAMPLE*)(n->data);

      if(s->line != prevS->line)
         for(j = 0; j < nDim; j++) readVicarImageLine(inps[j], s->line);

      for(j = 0; j < nDim; j++)
         gsl_vector_set(s->dns, j, inps[j]->buffer[s->sample]);
   }
}

/**********************************************/
void removeZeros(LINKEDLIST *samples)
{
   int i, removed;
   SAMPLE *s;
   struct node *n;

   //   printSample((SAMPLE*)samples->tail->data);
   //   printf("tail: %x tail next: %x\n", samples->tail, samples->tail->next);
   n = samples->head;
   while(n != NULL)
   {
      s = (SAMPLE*)n->data;
      //printSample(s);
      removed = 0;
      for(i = 0; i < s->dns->size; i++)
      {
         if(gsl_vector_get(s->dns, i) < 10E-10)
         {
            struct node *t;

            t = n;
            n = n->next;
            LinkedList_removeNode(samples, t);
            free(t);
            freeSample(&s);
            removed = 1;

            break;
         }
      }

      if(!removed) n = n->next;
   }
}

/**********************************************/
int compare_sample_by_line(const SAMPLE **s1, const SAMPLE **s2)
{
   if((*s1)->line < (*s2)->line)
      return -1;
   else if((*s1)->line > (*s2)->line)
      return 1;

   return 0;
}

/**********************************************/
LINKEDLIST* getStratifiedSamples(VICAR_IMAGE **inps, int nDim, int nGridX, int nGridY)
{
   LINKEDLIST *samples, *sortedSamples;
   int gridX, gridY, i;
   SAMPLE **readSampleIndices;

   gridX = inps[0]->ns/nGridX;
   gridY = inps[0]->nl/nGridY;

   samples = LinkedList_getLinkedList();
   for(i = 0; i < nGridX; i++)
   {
      int j;
      int x, y;

      for(j = 0; j < nGridY; j++)
      {
         SAMPLE *s;

         x = gridX*i + rand()%gridX;
         y = gridY*j + rand()%gridY;

         s = (SAMPLE*)malloc(sizeof(SAMPLE));
         s->line = y;
         s->sample = x;
         s->dns = gsl_vector_alloc(nDim);
         LinkedList_addWithRank(samples, s, s->line);
      }
   }
   printf("--> sorting\n");
   //   sortedSamples = LinkedList_sortAscending(&samples);
   sortedSamples = LinkedList_bigMemSortAscending(&samples);
   printf("--> reading\n");
   readSamples(sortedSamples, inps, nDim, nGridX*nGridY);
   //   printf("here1\n");

   //   for(i = 0; i < sortedSamples->size; i++) printSample(LinkedList_get(sortedSamples, i));

   return sortedSamples;
}

/**********************************************/
void outIBIS(LINKEDLIST *samples, int nDim, int nr)
{
   int nc, i, j;
   char **formats;
   IBISStruct *out;
   struct node** array;

   //   printf("outIBIS1 %d\n", samples->size);
   array = (struct node**)malloc(sizeof(struct node*)*samples->size);
   //   printf("outIBIS2\n");
   LinkedList_setNodeArray(samples, array);
   //   for(i = 0; i < samples->size; i++) printSample((SAMPLE*)(array[i]->data));

   nc = nDim+2;
   //   printf("outIBIS3\n");
   formats = (char**)malloc(nc*sizeof(char*));
   //   printf("outIBIS4\n");
   for(i = 0; i < nc; i++) formats[i] = "REAL";
   //   printf("outIBIS5\n");
   out = IBISHELPER_openIBIS_out(formats, 1, nr, nc);
   //   printf("outIBIS6\n");
   for(i = 0; i < nr; i++)
   {
      //      printf("outIBIS7 %d\n", i);
      SAMPLE *sample = (SAMPLE*)(array[i]->data);
      //      printf("outIBIS8\n");
      IBISHELPER_setDouble(out, 0, i, sample->line);
      //      printf("outIBIS9\n");
      IBISHELPER_setDouble(out, 1, i, sample->sample);
      //      printf("outIBIS10\n");
      for(j = 2; j < nc; j++)
         IBISHELPER_setDouble(out, j, i, (double)gsl_vector_get(sample->dns, j-2));
      //      printf("outIBIS11\n");
   }
   //   printf("outIBIS12\n");
   IBISHELPER_closeIBIS(&out);
   //   printf("outIBIS13\n");

   free(formats);
   free(array);
}

/**********************************************/
void main44(void)
{
   long seed;
   int status, i, inpCnt, dumdef, dumcnt;
   int nGridX, nGridY;
   char sampleType[20];
   char lowerType[20];
   VICAR_IMAGE **inps;
   IBISStruct *out;
   LINKEDLIST *samples;
   //   struct node **nodeArray;

   zifmessage("sampler version 2017-08-11");

   status = zvpcnt("inp", &inpCnt);
   if(status != 1) zmabend("Error getting input count.\n");
   inps = getVicarImages(inpCnt);

   status = zvp("ngridx", &nGridX, &dumcnt);
   if(status != 1) zmabend("Error getting gridX parameter.\n");
   status = zvp("ngridy", &nGridY, &dumcnt);
   if(status != 1) zmabend("Error getting gridY parameter.\n");
   status = zvp("seed", &seed, &dumcnt);
   if(status != 1) zmabend("Error getting seed parameter.\n");
   srand(seed);

   status = zvparm("sampletype", sampleType, &dumdef, &dumcnt, 1, 20);
   if(status != 1) zmabend("Error getting sample type.\n");

   IBISHELPER_lowerString(lowerType, sampleType);
   if(!strcmp("stratified", lowerType))
      samples = getStratifiedSamples(inps, inpCnt, nGridX, nGridY);

   //   gsl_vector_set(((SAMPLE*)(samples->tail->data))->dns, 0, 0.);
   if(zvptst("REMZEROS")) removeZeros(samples);

   /*
   nodeArray = (struct node**)malloc(sizeof(struct node*)*samples->size);
   LinkedList_setNodeArray(samples, nodeArray);
   for(i = 0; i < samples->size; i++) printSample((SAMPLE*)(nodeArray[i]->data));
   */

   printf("--> writing\n");
   outIBIS(samples, inpCnt, samples->size);
}
