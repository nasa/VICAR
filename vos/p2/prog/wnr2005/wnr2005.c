#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <string.h>

#include "cartoMemUtils.h"

#define MAXSIZE            4096
#define MAXSIZE2           2*4096

/*
C  PROGRAM WIENER  --  APPLY WIENER FILTER TO A FOURIER TRANSFORM

c  Nov 1996  JJL
C  2-JAN-95 ..CRI..   MSTP S/W Conversion (VICAR Porting)
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C  APR-85  ...LWK...  INITIIAL VERSION, FROM IBM PGM 'RESTORE'
C  MAY-85  ...LWK...  ADDED AP CODE, FROM IBM PGM 'GPFILT'
C  Jan-07  ...ALZ...  CONVERTED TO C, ADDED NSR INPUT
C  Sep-07  ...ALZ...  INVERTED NSR ARRAY INPUT TO SNR, CONSISTENT W 
  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup
*/

void main44(void)
{
   int iun[4],oun,nids,status,pcount,lin;
   float sn,rnorm=0,psfnor=0,outnor=0,mtf,factor=0;
   float *im,*otf,*restor,*ratio,*snrbuf;
   char fmt[9];
   
   int i,ir,ic,nl=0,ns=0,nl1,ns1,snrimgcase;
   float tmp1,tmp2,denom,ratior,ratioc;
   double dnumer,ddenom;
   
   zifmessage("wnr2005 version 2016-06-09");
   
   /* open & check inputs */
   
   snrimgcase = zvptst("snrimg");
   status = zvpcnt("inp",&nids);
   for (i=0;i<nids;i++)
      {
      status = zvunit(&iun[i],"INP",i+1, NULL);
      status = zvopen(iun[i],"OPEN_ACT","SA","IO_ACT","SA", NULL);
      zvget(iun[i],"FORMAT",fmt,"NL",&nl1,"NS",&ns1, NULL);
      if (strcmp(fmt,"COMPLEX")!=0 && strcmp(fmt,"COMP")!=0 &&
          !(snrimgcase && i==nids-1))
         zmabend("ALL INPUT FILES MUST BE COMPLEX, EXCEPT NSR REAL");
      if (strcmp(fmt,"REAL")!=0 && snrimgcase && i==nids-1)
         zmabend("NSR INPUT FILE MUST BE REAL");
      if (i==0) { nl = nl1; ns = ns1; }
      else if (nl!=nl1 || ns!=ns1)
         zmabend("ALL INPUT FILES MUST BE SAME SIZE");
      }
      
   /* malloc arrays */

   mz_alloc1((unsigned char **)&im,MAXSIZE2,4);
   mz_alloc1((unsigned char **)&otf,MAXSIZE2,4);
   mz_alloc1((unsigned char **)&restor,MAXSIZE2,4);
   mz_alloc1((unsigned char **)&ratio,MAXSIZE2,4);
   if (snrimgcase) mz_alloc1((unsigned char **)&snrbuf,MAXSIZE,4);

   /* open outputs */
   
   status = zvunit(&oun,"OUT",1, NULL);
   status=zvopen(oun,"U_NL",nl,"U_NS",ns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
	
   /* get parameters */
   
   zvp("SN",&sn,&pcount);
   sn = 1.0/(sn*sn);
   if (zvptst("wiener"))
      {
      factor=1.0/(1.0+sn);
      factor=1.0/factor;
      }
   
   /* process image */
   
   for (lin=0;lin<nl;lin++)
      {
      status = zvread(iun[0],im,"LINE",lin+1, NULL);
      status = zvread(iun[1],otf,"LINE",lin+1, NULL);
      if (snrimgcase) status = zvread(iun[nids-1],snrbuf,"LINE",lin+1, NULL);
      
      if (lin==0)
         {
         rnorm = otf[0];
         psfnor = im[0]; /* alz noted use of i not 1 in old wiener program */
         }
      
      /* normalize the otf */
      
      for (i=0;i<ns*2;i++) otf[i] = otf[i]/rnorm;
         
      /* Apply direct otf  filter */
      
      if (zvptst("direct"))
  	 for (i=0;i<ns;i++)
            {
            ir = 2*i; ic = 2*i+1;
            restor[ir] = otf[ir]*im[ir]-otf[ic]*im[ic];
            restor[ic] = otf[ir]*im[ic]+otf[ic]*im[ir];
   	    } 

      /* Apply wiener restoration filter */

      if (zvptst("wiener"))
         {
         for (i=0;i<ns;i++)
            {
            ir = 2*i; ic = 2*i+1;
            if (snrimgcase)
               {
               sn = 1.0/(snrbuf[i]*snrbuf[i]);
               factor = 1.0+sn;
               }
            tmp1 = factor/(otf[ir]*otf[ir]+otf[ic]*otf[ic]+sn);
            restor[ir] = tmp1*(im[ir]*otf[ir]+im[ic]*otf[ic]);
            restor[ic] = tmp1*(-im[ir]*otf[ic]+im[ic]*otf[ir]);
            }
/*cccccc      if(L.eq.1)restor(1)=cmplx(psfnor,0.0) from old fortran */
         }

      /* Obtain ratio restoration filter */
      
      if (zvptst("ratio"))
         {
         for (i=0;i<ns;i++)
            {
            ir = 2*i; ic = 2*i+1;
            if (snrimgcase) sn = 1.0/(snrbuf[i]*snrbuf[i]);
            denom = otf[ir]*otf[ir]+otf[ic]*otf[ic];
            if (denom!=0.0)
               {
               tmp1 = 1.0/(denom*psfnor);
               ratior = (im[ir]*otf[ir]+im[ic]*otf[ic])*tmp1;
               ratioc = (im[ic]*otf[ir]-im[ir]*otf[ic])*tmp1;
               tmp2 = ratior*ratior+ratioc*ratioc+sn;
               restor[ir] = ratior/tmp2;
               restor[ic] = -ratioc/tmp2;
               }
            else
               {
               restor[ir] = 0.0;
               restor[ic] = 0.0;
               }
            }
         }

      /* Apply amplitude restoration filter */

      if (zvptst("amplitude"))
         {
         for (i=0;i<ns;i++)
            {
            ir = 2*i; ic = 2*i+1;
            if (snrimgcase) sn = 1.0/(snrbuf[i]*snrbuf[i]);
            dnumer = (double)(im[ir]*im[ir]+im[ic]*im[ic]);
            ddenom = (double)(otf[ir]*otf[ir]+otf[ic]*otf[ic]);
            mtf = sqrt(dnumer)/(psfnor*sqrt(ddenom+0.00001));
            restor[ir] = im[ir]*mtf/(mtf*mtf+sn);
            restor[ic] = im[ic]*mtf/(mtf*mtf+sn);
            }
         if (lin==0)
            {
            restor[0] = psfnor;
            restor[1] = 0.0;
            }
         }

      /* Optionally APPLY MTF OF DESIRED OUTPUT */
      
      if (nids-snrimgcase==3)
         {
         status = zvread(iun[2],otf,"LINE",lin+1, NULL);
         if (lin==0) outnor = otf[0];
         for (i=0;i<ns;i++)
            {
            ir = 2*i; ic = 2*i+1;
            restor[ir] = (restor[ir]*otf[ir]+restor[ic]*otf[ic])/outnor;
            restor[ic] = (restor[ic]*otf[ir]-restor[ir]*otf[ic])/outnor;
            }
         if (lin==0)
            {
            restor[0] = psfnor;
            restor[1] = 0.0;
            }
         }
      zvwrit(oun,restor,"NSAMPS",ns,"LINE",lin+1, NULL);
      }
      
   for (i=0;i<nids;i++) zvclose(iun[i], NULL);
   zvclose(oun, NULL);
   return;
}
