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
#include "cartoSortUtils.h"

#define MAXCOLS 200

/************************************************************************/
/* program sort                                                      */
/************************************************************************/
/*  00-09 ...alz... initial version                     */
/************************************************************************/

void sorta(buf,wid,ptr,n)
     char *buf;
     int wid,*ptr,n;
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      char *ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;
      if ((ibuf=(char *)malloc(wid))==NULL) zmabend("malloc failed");
      
 l10: k = m;
      strcpy(ibuf,&buf[k*wid]);
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&strcmp(&buf[(j+1)*wid],&buf[j*wid])>0) j++;
      if (strcmp(&buf[j*wid],ibuf)<1) goto l25;
      strcpy(&buf[k*wid],&buf[j*wid]);
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: strcpy(&buf[k*wid],ibuf);
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      strcpy(ibuf,&buf[k*wid]);
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&strcmp(&buf[(j+1)*wid],&buf[j*wid])>0) j++;
      if (strcmp(&buf[j*wid],ibuf)<1) goto l45;
      strcpy(&buf[k*wid],&buf[j*wid]);
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: strcpy(&buf[k*wid],ibuf);
      ptr[k] = iptr;
      strcpy(ibuf,&buf[0]);
      iptr = ptr[0];
      strcpy(&buf[0],&buf[l*wid]);
      ptr[0] = ptr[l];
      strcpy(&buf[l*wid],ibuf);
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;
      
      free(ibuf);
      return;
}

void sortreca(key,wid,ptr,len)
   char *key;
   int wid,*ptr,len;
{
   char *temp;
   int i;
   
   if (len<2) return;
   if ((temp=(char *)malloc(wid*len))==NULL) zmabend("malloc failed");
   for (i=0;i<len;i++) strncpy(&temp[i*wid],&key[i*wid],wid);
   for (i=0;i<len;i++) strncpy(&key[i*wid],&temp[(ptr[i]-1)*wid],wid);
   free(temp);
   return;
}

void main44(void)
{
   int i,icol,k,ascend,sortcol[20],sortcount,dummy,indexcol,unit;
   int ibis,status,clen,ncol,pu,tcs,ksv,oldcs,filwid,tcx,ku;
   int *cx,*cs;
   double *iodat;
   char coltype[MAXCOLS][6],*iodatstr;
 
   zifmessage("sort version Wed Jan  2 2008");
   
   /* get the basic parameters */
   
   ascend = zvptst("ascend");
   zvparm("sortcol",sortcol,&sortcount,&dummy,MAXCOLS,0);
   zvp("indexcol",&indexcol,&dummy);
   
   /* open the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"formats",coltype,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&iodat,clen,8);
   mz_alloc1((unsigned char **)&cx,clen,4);
   mz_alloc1((unsigned char **)&cs,clen,4);
   
   /* read each sort column and sort, keep the result in cx
      cs keeps track of the identical key groups */

   for (i=0;i<clen;i++) { cx[i] = i+1; cs[i] = 1; }
   for (icol=0;icol<sortcount;icol++)
      {
      if (coltype[sortcol[icol]-1][0]!='A')     /* numeric column */
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",sortcol[icol]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)iodat,sortcol[icol],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         if (icol>0) sortrec8(iodat,cx,clen);
         
         for (k=0;k<clen;)
	    {
	    pu = k; tcs = cs[k];
	    while (cs[pu+1]==tcs&&pu<clen-1) pu++;
	    sort8(&iodat[k],&cx[k],pu-k+1);
	    k = pu+1;
	    }
         ksv = 1; oldcs = cs[0];
         for (k=1;k<clen;k++)
	    {
	    if (iodat[k]!=iodat[k-1])
	       {
	       ksv += 1;
	       goto nxtcs;
	       }
	    if (cs[k]!=oldcs) ksv += 1;
	    nxtcs: oldcs = cs[k]; cs[k] = ksv;
	    }
	 } /* end of numeric case */
      else     /* alpha column */
         {
         filwid = ms_num(&coltype[sortcol[icol]-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen,filwid);
         
         status = IBISColumnRead(ibis,iodatstr,sortcol[icol],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         if (icol>0) sortreca(iodatstr,filwid,cx,clen);
                  
         for (k=0;k<clen;)
	    {
	    pu = k; tcs = cs[k];
	    while (cs[pu+1]==tcs&&pu<clen-1) pu++;
	    sorta(&iodatstr[k*filwid],filwid,&cx[k],pu-k+1);
	    k = pu+1;
	    }
         ksv = 1; oldcs = cs[0];
         for (k=1;k<clen;k++)
	    {
	    if (strcmp(&iodatstr[k*filwid],&iodatstr[(k-1)*filwid])!=0)
	       {
	       ksv += 1;
	       goto nxtcs2;
	       }
	    if (cs[k]!=oldcs) ksv += 1;
	    nxtcs2: oldcs = cs[k]; cs[k] = ksv;
	    }
	 free(iodatstr);
         } /* end of alpha case */
      } /* end of loop over columns */
  
   /* not ascending, reverse cs and cx */
   
   if (!ascend)
      {
      for (k=0;k<clen/2;k++)
	 {
	 ku = clen-k-1;
	 tcs = cs[k]; cs[k] = cs[ku]; cs[ku] = tcs;
	 tcx = cx[k]; cx[k] = cx[ku]; cx[ku] = tcx;
	 }
      }

   /* move the entire file by the pointers cx, place cs in the indexcol
      if selected */

   for (icol=0;icol<ncol;icol++)
      {
      if (coltype[icol][0]!='A')     /* numeric column */
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",icol+1);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)iodat,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         
         if (icol+1!=indexcol) sortrec8(iodat,cx,clen);
         else for (i=0;i<clen;i++) iodat[i] = (double)cs[i];
         
         status = IBISColumnWrite(ibis,(char*)iodat,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else     /* alpha column */
         {
         filwid = ms_num(&coltype[icol][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen,filwid);
         
         status = IBISColumnRead(ibis,iodatstr,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         
         sortreca(iodatstr,filwid,cx,clen);
         
         status = IBISColumnWrite(ibis,iodatstr,icol+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         free(iodatstr);
         }
      }
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   return;
}
