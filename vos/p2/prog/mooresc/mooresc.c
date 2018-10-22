#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/************************************************************************/
/* program mooresc                                                      */
/************************************************************************/
/*  07-00 ...alz... initial version                     */
/************************************************************************/

int *pstack,*istack,*vstack,debug;

void propagate(ival,moorebuf,previx,i,dfeather,tns,currix)
   int ival,previx,i,dfeather,tns,currix;
   short int **moorebuf;
{
   int ptr,tp,ti,tv,upix;

   ptr = 1;
   pstack[0] = previx;
   istack[0] = i;
   vstack[0] = ival;
   
   /* only 3 directions needed for this Moore algorithm because of the
   downward sweep of the line read/write */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      moorebuf[tp][ti] = tv;
      if (ti+1<tns)
         {
         if ((int)moorebuf[tp][ti+1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti+1;
            vstack[ptr++] = tv+1;
            }
         }
      if (ti>0)
         {
         if ((int)moorebuf[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      upix = (tp-1+dfeather)%dfeather;
      if (upix!=currix)
         {
         if ((int)moorebuf[upix][ti]>tv+1)
            {
            pstack[ptr] = upix;
            istack[ptr] = ti;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   debug = 0;
   return;
}

void leftprop(ival,moorebuf,currix,i,tns)
   int ival,currix,i,tns;
   short int **moorebuf;
{
   int ptr,tp,ti,tv;

   ptr = 1;
   pstack[0] = currix;
   istack[0] = i;
   vstack[0] = ival;
            
   /* only 1 direction needed for leftward propagation, this could
   be more efficient, but it was easier to copy the code above */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      if(ti < 0) zmabend("-1 detected.\n");
      moorebuf[tp][ti] = tv;
      if (ti>0)
         {
         if ((int)moorebuf[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   return;
}

void main44(void)
{
   int i,j,k,ip,dmax,count,i_unit,nl,ns,o_unit2,outcnt;
   int o_unit,dmaxp1,iline,previx,currix,status,numpix,*pixval;
   short int ***inbuf,***moore,newmoore,prevmoore,*outbuf;
   short int *readbuf,lookup[65536];
   int *outbits;
   char fmt_str[10];
   
   zifmessage("mooresc version 2017-06-06");
   
   /* fetch params */
   
   zvp("DMAX",&dmax,&count);
   dmaxp1 = dmax+1; /* this is a shutoff value */
   zvp("NUMPIX",&numpix,&count);
   mz_alloc1((unsigned char **)&pixval,numpix,4);
   zvp("PIXVAL",pixval,&count);
   if (count!=numpix) zmabend("you must explicitly state numpix and pixval");
   for (i=0;i<65536;i++) lookup[i] = 0;
   for (i=0;i<numpix;i++) lookup[pixval[i]] = i+1;
   
   /* open input image file */

   status = zvunit( &i_unit, "INP", 1, NULL);
   status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
      "U_FORMAT","HALF", NULL);
   zvget(i_unit,"FORMAT",fmt_str, NULL);
   if (strcmp(fmt_str,"BYTE")&&strcmp(fmt_str,"HALF")) 
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   zvget(i_unit,"NL",&nl,"NS",&ns, NULL);
       
   /* dynamically allocate storage */
   
   mz_alloc3((unsigned char ****)&inbuf,numpix,dmaxp1,ns,2);
   mz_alloc3((unsigned char ****)&moore,numpix,dmaxp1,ns,2);
   /* printf("alloc %d\n",numpix*dmaxp1*ns*4); */
   mz_alloc1((unsigned char **)&outbuf,ns,2);
   mz_alloc1((unsigned char **)&outbits,ns,4);
   mz_alloc1((unsigned char **)&readbuf,ns,2);
   for (ip=0;ip<numpix;ip++)
      for (i=0;i<dmaxp1;i++)
         for (j=0;j<ns;j++)
            {
            inbuf[ip][i][j] = 0;
            moore[ip][i][j] = (short int)dmaxp1;
            }
   
   mz_alloc1((unsigned char **)&pstack,ns*dmaxp1,4);
   mz_alloc1((unsigned char **)&istack,ns*dmaxp1,4);
   mz_alloc1((unsigned char **)&vstack,ns*dmaxp1,4);
   
   /* open output image file */
   
   status=zvunit(&o_unit,"OUT",1,NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
   
   /* open second output image file, if count=2 */
   
   status = zvpcnt("out",&outcnt);
   if (outcnt==2)
      {
      if (numpix<=8) strcpy(fmt_str,"BYTE");
         else if (numpix<=16) strcpy(fmt_str,"HALF");
         else strcpy(fmt_str,"FULL");
      status=zvunit(&o_unit2,"OUT",2,NULL);
      status=zvopen(o_unit2,"U_NL",nl,"U_NS",ns,"O_FORMAT",fmt_str,
	   "U_FORMAT","FULL","OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
      }

   /* roll the barrels, performing the Moore algorithm */
   
   debug = 1;
   currix = dmax;
   for (iline=0;iline<nl+dmaxp1;iline++)
      {
      previx = currix;
      currix = (currix+1)%dmaxp1;
      
      if (iline>=dmaxp1)
         {
         for (ip=0;ip<numpix;ip++)
            {
            for (j=0;j<ns;j++)
               {
               if (inbuf[ip][currix][j]==0)
                  {
                  if (moore[ip][currix][j]<dmaxp1)
                     inbuf[ip][currix][j] = 1;
                  }
               else inbuf[ip][currix][j] = 1;
               }
            }
         for (j=0;j<ns;j++)
            {
            outbuf[j] = 0;
            if (outcnt>=2) outbits[j] = 0;
            for (ip=0;ip<numpix;ip++)
               if (inbuf[ip][currix][j]>0)
                  {
                  outbuf[j]++;
                  if (outcnt>=2) outbits[j] += 1<<ip;
                  }
            }
         zvwrit(o_unit,outbuf,"LINE",iline-dmax,"NSAMPS",ns,NULL);
         if (outcnt>=2)
            zvwrit(o_unit2,outbits,"LINE",iline-dmax,"NSAMPS",ns,NULL);
         /*for debug:zvwrit(o_unit,inbuf[0][currix],"LINE",
                  iline-dmax,"NSAMPS",ns,NULL);*/
         } 
      if (iline>=nl) continue;
      
      for (ip=0;ip<numpix;ip++)
         {
         if (ip==0) 
            {
            status = zvread(i_unit,readbuf,"LINE",iline+1, NULL);
            for (k=0;k<numpix;k++)
               for (j=0;j<ns;j++)
                  inbuf[k][currix][j] = 0;
             for (j=0;j<ns;j++)
                {
                k = lookup[readbuf[j]];
                if (k>0) inbuf[k-1][currix][j] = readbuf[j];
                }
            }
         
         prevmoore = dmaxp1;
         for (j=0;j<ns;j++)
            {
            if (inbuf[ip][currix][j]==0)
               {
               newmoore = MIN(dmaxp1,MIN(prevmoore,moore[ip][previx][j])+1);
               }
            else
               {
               newmoore = 1;
               if (moore[ip][previx][j]>newmoore+1)
                  propagate(newmoore+1,moore[ip],previx,j,dmaxp1,ns,currix);
               if ((prevmoore>newmoore+1)&&j>0)
                  leftprop(newmoore+1,moore[ip],currix,j-1,dmaxp1,ns);
               }
            moore[ip][currix][j] = newmoore;
            prevmoore = newmoore;
            }
         }
      }
   
   /* close and return */
   
   mz_free3((unsigned char ***)inbuf,numpix,dmaxp1);
   mz_free3((unsigned char ***)moore,numpix,dmaxp1);
   zvclose(i_unit,NULL);
   zvclose(o_unit,NULL);
   
   return;
}
