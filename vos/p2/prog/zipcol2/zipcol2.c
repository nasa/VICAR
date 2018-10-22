#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MAXCOLS 200

/************************************************************************/
/* program zipcol2                                                      */
/************************************************************************/
/*  00-04 ...alz... initial version                     */
/*  Sat Dec 29 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

int xcmp(a1,a2)
   double a1,a2;
{
   if (a1>a2) return(1);
   else if (a1<a2) return(-1);
   else return(0);
}

void main44(void)
{
   int i,j,icol,incol[20],outcol[30],filecol[50],incount,outcount;
   int nullcount,nullstrcount,dummy,filwid1,filwid2;
   int unit1,unit2,ibis1,ibis2,status,clen1,clen2,nonull;
   int p,pl,pu,filecount,tst,ncol1,ncol2,nulltrcount,nullstrtrcount;
   int *cl,*cu;
   char nullstr[30][99],*filedatstr,*iodatstr;
   char coltype1[MAXCOLS][6],coltype2[MAXCOLS][6];
   double *iodat,*filedat,nullval[30];
   
   zifmessage("zipcol2 version 2016-06-09");
   
   /* get the basic parameters */
   
   nonull = zvptst("nonull");
   
   zvparm("incol",incol,&incount,&dummy,20,0);
   zvparm("outcol",outcol,&outcount,&dummy,30,0);
   zvparm("file",filecol,&filecount,&dummy,50,0);
   if (filecount!=incount+outcount)
      zmabend("column counts incorrect");
   zvparmd("null",nullval,&nullcount,&dummy,30,0);
   zvparm("nulstr",nullstr,&nullstrcount,&dummy,30,99);
         
   /* open the ibis interface files */

   status = zvunit(&unit1,"inp",1, NULL);
   status = IBISFileOpen(unit1,&ibis1,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",coltype1,1,MAXCOLS,6);
   
   status = zvunit(&unit2,"inp",2, NULL);
   status = IBISFileOpen(unit2,&ibis2,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   IBISFileGet(ibis2,"nr",&clen2,1,1,0);
   IBISFileGet(ibis2,"nc",&ncol2,1,1,0);
   IBISFileGet(ibis2,"formats",coltype2,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&iodat,clen1,8);
   mz_alloc1((unsigned char **)&filedat,clen2,8);
   mz_alloc1((unsigned char **)&cl,clen2,4);
   mz_alloc1((unsigned char **)&cu,clen2,4);
   
   /* read each matching pair of index columns. match them
      and keep track of destination ranges in cl and cu */
            
   for (i=0;i<clen2;i++) { cl[i] = 0; cu[i] = clen1-1; }
   for (icol=0;icol<incount;icol++)
      {
      if (coltype1[incol[icol]-1][0]!='A')     /* numeric column */
         {
         if (coltype2[filecol[icol]-1][0]=='A')
            zmabend("error matching up alpha and numeric data columns-1");
         status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",incol[icol]);
         if (status!=1) IBISSignal(ibis1,status,1);
         status = IBISColumnRead(ibis1,(char*) iodat,incol[icol],1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",filecol[icol]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnRead(ibis2,(char*) filedat,filecol[icol],1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         for (i=1;i<clen2;i++)
            {
            tst = xcmp(filedat[i-1],filedat[i]);
	    if (tst>0&&cl[i-1]==cl[i]&&cu[i-1]==cu[i]&&cu[i]>=cl[i])
	       zmabend("file 2 index column data (numeric) not sorted ascending");
            }
         for (i=0;i<clen2;i++)
	    {
	    pl = cl[i]; pu = cu[i];
	    if (pl>pu) continue;
	    p = (pu+pl)>>1;
	    while ((pu-pl)>1)
	       {
	       tst = xcmp(iodat[p],filedat[i]);
	       if (tst<0) { pl = p; p = (p+pu)>>1; }
		      else { pu = p; p = (p+pl)>>1; }
	       }
	    tst = xcmp(iodat[pl],filedat[i]);
	    if (tst>=0) pu = pl;
	    cl[i] = pu;
	    pl = cl[i]; pu = cu[i];
	    p = (pu+pl)>>1;
	    while ((pu-pl)>1)
	       {
	       tst = xcmp(iodat[p],filedat[i]);
	       if (tst<=0) { pl = p; p = (p+pu)>>1; }
		       else { pu = p; p = (p+pl)>>1; }
	       }
	    tst = xcmp(iodat[pu],filedat[i]);
	    if (tst<=0) pl = pu;
	    cu[i] = pl;
	    if (cl[i]==pl)
	       {
	       tst = xcmp(iodat[pl],filedat[i]);
	       if (tst!=0) cu[i] = pl-1;
	       }
	    } /* end of column processing */
	 } /* end of numeric case */
      else     /* alpha column */
         {
         if (coltype2[filecol[icol]-1][0]!='A')
            zmabend("error matching up alpha and numeric data columns-2");
         filwid1 = ms_num(&coltype1[incol[icol]-1][1])+1;
         filwid2 = ms_num(&coltype2[filecol[icol]-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen1,filwid1);
         mz_alloc1((unsigned char **)&filedatstr,clen2,filwid2);
         
         status = IBISColumnRead(ibis1,iodatstr,incol[icol],1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
	 for (i=0;i<clen1;i++) iodatstr[i*filwid1+filwid1-1]='\0';
         
         status = IBISColumnRead(ibis2,filedatstr,filecol[icol],1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
	 for (i=0;i<clen2;i++) filedatstr[i*filwid2+filwid2-1]='\0';

         for (i=1;i<clen2;i++)
            {
            tst = strcmp(&filedatstr[(i-1)*filwid2],&filedatstr[i*filwid2]);
	    if (tst>0&&cl[i-1]==cl[i]&&cu[i-1]==cu[i]&&cu[i]>=cl[i])
               zmabend("file 2 index column data (alpha) not sorted ascending");
            }
                  
         for (i=0;i<clen2;i++)
	    {
	    pl = cl[i]; pu = cu[i];
	    if (pl>pu) continue;
	    p = (pu+pl)>>1;
	    while ((pu-pl)>1)
	       {
	       tst = strcmp(&iodatstr[p*filwid1],&filedatstr[i*filwid2]);
	       if (tst<0) { pl = p; p = (p+pu)>>1; }
		      else { pu = p; p = (p+pl)>>1; }
	       }
	    tst = strcmp(&iodatstr[pl*filwid1],&filedatstr[i*filwid2]);
	    if (tst>=0) pu = pl;
	    cl[i] = pu;
	    pl = cl[i]; pu = cu[i];
	    p = (pu+pl)>>1;
	    while ((pu-pl)>1)
	       {
	       tst = strcmp(&iodatstr[p*filwid1],&filedatstr[i*filwid2]);
	       if (tst<=0) { pl = p; p = (p+pu)>>1; }
		       else { pu = p; p = (p+pl)>>1; }
	       }
	    tst = strcmp(&iodatstr[pu*filwid1],&filedatstr[i*filwid2]);
	    if (tst<=0) pl = pu;
	    cu[i] = pl;
	    if (cl[i]==pl)
	       {
	       tst = strcmp(&iodatstr[pl*filwid1],&filedatstr[i*filwid2]);
	       if (tst!=0) cu[i] = pl-1;
	       }
	    } /* end of column processing */
	 free(iodatstr);
         free(filedatstr);
         } /* end of alpha case */
      } /* end of loop over columns */
   
   /* move the data columns by the pointers in cl and cu,
      don't forget NULL and NONULL */
   
   nulltrcount = 0; nullstrtrcount = 0;
   for (icol=0;icol<outcount;icol++)
      {
      if (coltype1[outcol[icol]-1][0]!='A')     /* numeric column */
         {
         if (coltype2[filecol[icol+incount]-1][0]=='A')
            zmabend("error matching up alpha and numeric data columns-3");
         if (nonull)
            {
            status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",outcol[icol]);
            if (status!=1) IBISSignal(ibis1,status,1);
            status = IBISColumnRead(ibis1,(char*) iodat,outcol[icol],1,clen1);
            if (status!=1) IBISSignal(ibis1,status,1);
            }
      
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",filecol[icol+incount]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnRead(ibis2,(char*) filedat,filecol[icol+incount],1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         
         p = 0;
         for (i=0;i<clen2;i++)
	    {
	    pl = cl[i]; pu = cu[i];
	    if (pl>pu) continue;
	    if (!nonull) for (j=p;j<pl;j++) iodat[j] = nullval[nulltrcount];
	    for (j=pl;j<=pu;j++) iodat[j] = filedat[i];
	    p = pu+1;
	    }
         if (!nonull) for (j=p;j<clen1;j++) iodat[j] = nullval[nulltrcount];
         
         status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",outcol[icol]);
         if (status!=1) IBISSignal(ibis1,status,1);
         status = IBISColumnWrite(ibis1,(char*) iodat,outcol[icol],1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         nulltrcount++;
         }
      else     /* alpha column, extra space in iodatstr for longer filedatstr strings */
         {
         if (coltype2[filecol[icol+incount]-1][0]!='A')
            zmabend("error matching up alpha and numeric data columns-4");
         filwid1 = ms_num(&coltype1[outcol[icol]-1][1])+1;
         filwid2 = ms_num(&coltype2[filecol[icol+incount]-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen1+filwid2/filwid1+1,filwid1);
         mz_alloc1((unsigned char **)&filedatstr,clen2,filwid2);
       
         if (nonull)
            {
            status = IBISColumnRead(ibis1,iodatstr,outcol[icol],1,clen1);
            if (status!=1) IBISSignal(ibis1,status,1);
	    for (i=0;i<clen1;i++) iodatstr[i*filwid1+filwid1-1]='\0';
            }
         
         status = IBISColumnRead(ibis2,filedatstr,filecol[icol+incount],1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
	 for (i=0;i<clen2;i++) filedatstr[i*filwid2+filwid2-1]='\0';
         
         p = 0;
         for (i=0;i<clen2;i++)
	    {
	    pl = cl[i]; pu = cu[i];
	    if (pl>pu) continue;
	    if (!nonull) for (j=p;j<pl;j++)
	       strcpy(&iodatstr[j*filwid1],nullstr[nullstrtrcount]);
	    for (j=pl;j<=pu;j++)
	       strcpy(&iodatstr[j*filwid1],&filedatstr[i*filwid2]);
	    p = pu+1;
	    }
         if (!nonull) for (j=p;j<clen1;j++)
	       strcpy(&iodatstr[j*filwid1],nullstr[nullstrtrcount]);
         
         status = IBISColumnWrite(ibis1,iodatstr,outcol[icol],1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         free(iodatstr);
         free(filedatstr);
         nullstrtrcount++;
         }
      }
    
   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
  
   return;
}
