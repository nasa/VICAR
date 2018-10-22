#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoVicarProtos.h"*/
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MAXCOLS 500
#define MAXSUMCOL 200
#define MAXBYARCOL 100

/************************************************************************/
/* program aggrg2                                                       */
/************************************************************************/
/* 00-06 ...alz... initial version                                      */
/* see pdf for history continuation                                     */
/************************************************************************/

void main44(void)
{
   int i,icol,il,iu,sumcount,byarcount,dummy,unit1,unit2,ibis1,ibis2;
   int agcol,count,area,sumcol[MAXSUMCOL],byar[MAXBYARCOL],swtch;
   int clen1,clen2,ncol1,ncol2,status,uptr,sptr,agcase,outptr;
   int agcolisnum,areacolisnum,agcolwid=0,areacolwid=0,colwid;
   short int *conbuf,x2;
   double *agdat,*areadat,*iodat,val=0,valmax=0,rst;
   char *p=0,*q,cformat[MAXCOLS][6],*agdatstr,*areadatstr,*iodatstr;
  
   zifmessage("aggrg2 version Thu Jan  3 2008");
   
   zvp("agcol",&agcol,&count);
   zvp("area",&area,&count);
   zvparm("sumcol",sumcol,&sumcount,&dummy,20,0);
   zvparm("byar",byar,&byarcount,&dummy,20,0);
   
   /* open the input file and read in the control columns */
   
   status = zvunit(&unit1,"inp",1, NULL);
   status = IBISFileOpen(unit1,&ibis1,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",cformat,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&agdat,clen1,8);
   mz_alloc1((unsigned char **)&areadat,clen1,8);
   mz_alloc1((unsigned char **)&conbuf,clen1,2);
   
   /* control column can be numeric or alpha */
   
   status = IBISColumnGet(ibis1,"FORMAT",cformat[0],agcol);
   if (status!=1) IBISSignal(ibis1,status,1);
   if (cformat[0][0]=='A')
      {
      agcolisnum = 0;
      agcolwid = ms_num(&cformat[0][1])+1;
      mz_alloc1((unsigned char **)&agdatstr,clen1,agcolwid);
      status = IBISColumnRead(ibis1,agdatstr,agcol,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   else
      {
      agcolisnum = 1;
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",agcol);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char*)agdat,agcol,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   
   /* area column can be numeric or alpha */
    
   status = IBISColumnGet(ibis1,"FORMAT",cformat[0],area);
   if (status!=1) IBISSignal(ibis1,status,1);
   if (cformat[0][0]=='A')
      {
      areacolisnum = 0;
      areacolwid = ms_num(&cformat[0][1])+1;
      mz_alloc1((unsigned char **)&areadatstr,clen1,areacolwid);
      status = IBISColumnRead(ibis1,areadatstr,area,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   else
      {
      areacolisnum = 1;
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",area);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char*)areadat,area,1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   
   /* set up conbuf with breaks in data */
   
   if (agcolisnum)
      {
      swtch = 3;
      rst = agdat[0];
      for (i=0;i<clen1;i++)
         {
         if (rst!=agdat[i])
	    {
	    rst = agdat[i];
	    swtch = 7-swtch;
	    }
         conbuf[i] = swtch;
         }
      }
   else
      {
      swtch = 3;
      p = &agdatstr[0];
      for (i=0;i<clen1;i++)
         {
         if (strcmp(p,&agdatstr[i*agcolwid])!=0)
	    {
	    p = &agdatstr[i*agcolwid];
	    swtch = 7-swtch;
	    }
         conbuf[i] = swtch;
         }
      free(agdatstr);
      }
   
   /* change conbuf to 2 bit at end of each group, 1 bit for the
      maximum in each group */

   clen2 = 0;
   for (i=0;i<clen1;)
      {
      clen2 +=1;
      uptr = i; sptr = i;
      x2 = conbuf[i];
      conbuf[i] = 0;
      if (areacolisnum) valmax = areadat[i];
         else p = &areadatstr[i*areacolwid];
      while (uptr<clen1-1 && conbuf[uptr+1]==x2)
	 {
	 uptr += 1;
	 conbuf[uptr] = 0;
	 if (areacolisnum)
	    {
	    val = areadat[uptr];
	    if (val>valmax) { valmax = val; sptr = uptr; }
	    }
	 else
	    {
	    q = &areadatstr[uptr*areacolwid];
	    if (strcmp(q,p)>0) { q = p; sptr = uptr; }
	    }
	 }
      conbuf[uptr] +=2;
      conbuf[sptr] +=1;
      i = uptr+1;
      }
   
   free(agdat);
   free(areadat);
   mz_alloc1((unsigned char **)&iodat,clen1,8);
   
   /* open the output file */
   
   ncol2 = ncol1;
   for (icol=1;icol<=ncol1;icol++)
      {
      status = IBISColumnGet(ibis1,"FORMAT",cformat[icol-1],icol);
      if (status!=1) IBISSignal(ibis1,status,1);
     }
   status = zvunit(&unit2,"out",1, NULL);
   status = IBISFileUnit(unit2,&ibis2,"write",ncol2,clen2,(char*)cformat,"column");
   status = IBISFileUnitOpen(ibis2);
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')
         {
         status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis1,status,1);
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      }
   /*status = IBISFileOpen(ibis2,&ibis,"write",4,clen,0,0);*/
   
   /* read each selected column, aggregate using conbuf control,
      then write out the column */
   
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')  /* numeric case */
         {
         status = IBISColumnRead(ibis1,(char*)iodat,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         agcase = 1;
         for (i=0;i<sumcount;i++) if (sumcol[i]==icol) agcase = 2;
         for (i=0;i<byarcount;i++) if (byar[i]==icol) agcase = 3;
         
         iu = -1;
         for (outptr=0;outptr<clen2;outptr++)
            {
            il = iu+1;
            switch(agcase)
               {
               case 1: val = iodat[il]; break;
               case 2: val = 0.0; break;
               case 3: val = 0.0; break;
               }
            for (i=il;i<clen1;i++)
               {
               switch(agcase)
                  {
                  case 1: val = MAX(val,iodat[i]); break;
                  case 2: val += iodat[i]; break;
                  case 3: if (conbuf[i]%2==1) val = iodat[i]; break;
                  }
               if (conbuf[i]>1)
                  {
                  iodat[outptr] = val;
                  iu = i;
                  break;
                  }
               }
            }
         
         status = IBISColumnWrite(ibis2,(char*)iodat,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      else     /* alphabetic case */
         {
         colwid = ms_num(&cformat[icol-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen1,colwid);
         status = IBISColumnRead(ibis1,iodatstr,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         agcase = 1;
         for (i=0;i<sumcount;i++) if (sumcol[i]==icol) agcase = 2;
         for (i=0;i<byarcount;i++) if (byar[i]==icol) agcase = 3;
         if (agcase==2) zmabend("Can't sum an alpha column");
         
         iu = -1;
         for (outptr=0;outptr<clen2;outptr++)
            {
            il = iu+1;
            switch(agcase)
               {
               case 1: p = &iodatstr[il*colwid];
               }
            for (i=il;i<clen1;i++)
               {
               switch(agcase)
                  {
                  case 1: if (strcmp(p,&iodatstr[i*colwid])<=0)
                             p = &iodatstr[i*colwid]; break;
                  case 3: if (conbuf[i]%2==1) p = &iodatstr[i*colwid]; break;
                  }
               if (conbuf[i]>1)
                  {
                  strcpy(&iodatstr[outptr*colwid],p);
                  iu = i;
                  break;
                  }
               }
            }
         
         status = IBISColumnWrite(ibis2,iodatstr,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         
         free(iodatstr);
         }
      }
         
   
   /* print in/out file lengths */
   
   printf("%d records in, %d records out\n",clen1,clen2);
   
   /* close files */

   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
  
   return;
}
