#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
/*#include "cartoVicarProtos.h"*/

/************************************************************************/
/* program aggrg                                                        */
/************************************************************************/
/* 00-06 ...alz... initial version                                      */
/* see pdf for history continuagion                                     */
/************************************************************************/

void main44(void)
{
   int i,j,sumcol[40],tocol[40],sumcount,tocount,dummy;
   int unit,ibis,status,clen,agcol,indexcol,indx,icol,outptr;
   int *ixdat,agcolisnum,agcolwid=0,rst,il,iu,outcol;
   short int *conbuf;
   double sum,*agdat,*iodat;
   char *p,cformat[7],*agdatstr;
           
   zifmessage("aggrg version Fri Dec  5 2014");
   
   /* get the basic parameters */
   
   status = zvp("agcol",&agcol,&dummy);
   status = zvp("indexcol",&indexcol,&dummy);
   zvparm("sumcol",sumcol,&sumcount,&dummy,40,0);
   zvparm("tocol",tocol,&tocount,&dummy,40,0);
   if (tocol[0]==0) tocount = 0;
   
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&conbuf,clen,2);
   if (indexcol>0) mz_alloc1((unsigned char **)&ixdat,clen,4);
   
   /* control column can be numeric or alpha */
   
   status = IBISColumnGet(ibis,"FORMAT",cformat,agcol);
   if (status!=1) IBISSignal(ibis,status,1);
   if (cformat[0]=='A')
      {
      agcolisnum = 0;
      agcolwid = ms_num(&cformat[1])+1;
      mz_alloc1((unsigned char **)&agdatstr,clen,agcolwid);
      status = IBISColumnRead(ibis,agdatstr,agcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   else
      {
      agcolisnum = 1;
      mz_alloc1((unsigned char **)&agdat,clen,8);
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",agcol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*) agdat,agcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
      
   /* calculate the control column */
   
   for (i=0;i<clen;i++) conbuf[i] = 0;
   if (agcolisnum)
      {
      rst = agdat[0];
      for (i=0;i<clen;i++)
         {
         if (rst!=agdat[i])
	    {
	    rst = agdat[i];
	    conbuf[i-1] = 1;
	    }
         }
      conbuf[clen-1] = 1;
      free(agdat);
      }
   else
      {
      p = &agdatstr[0];
      for (i=0;i<clen;i++)
         {
         if (strcmp(p,&agdatstr[i*agcolwid])!=0)
	    {
	    p = &agdatstr[i*agcolwid];
	    conbuf[i-1] = 1;
	    }
         }
      conbuf[clen-1] = 1;
      free(agdatstr);
      }
   mz_alloc1((unsigned char **)&iodat,clen,8);
   
   /* Read, sum, and output columns to the ibis interface file */
   
   for (indx=0;indx<sumcount;indx++)
      {
      icol = sumcol[indx];
      status = IBISColumnGet(ibis,"FORMAT",cformat,icol);
      if (status!=1) IBISSignal(ibis,status,1);
      if (cformat[0]=='A') zmabend("Can't sum an alpha column");
      
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",icol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*) iodat,icol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
   
      /* sum the column*/
      
      iu = -1;
      for (outptr=0;outptr<clen;outptr++)
         {
         il = iu+1;
         sum = 0.0;
         for (i=il;i<clen;i++)
            {
            sum += iodat[i];
            if (conbuf[i])
               {
               iu = i;
               for (j=il;j<=iu;j++) iodat[j] = sum;
               if (indx==0&&indexcol>0)
                  for (j=il;j<=iu;j++) ixdat[j] = outptr+1;
               break;
               }
            }
         }
      if (indx<tocount)
         {
         outcol = tocol[indx];
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",outcol);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else outcol = icol;
      status = IBISColumnWrite(ibis,(char*) iodat,outcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   if (indexcol>0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","FULL",indexcol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*) ixdat,indexcol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
  
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
