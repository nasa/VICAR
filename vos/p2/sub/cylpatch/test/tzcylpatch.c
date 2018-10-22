#include "xvmaininc.h"
#include "ftnbridge.h"

#define headin  "input  data="
#define headout "output data="
#define headblk "            "

  struct data  /*Standard MAP data Structure */
      {
      float rdata[38];
      int idata;
      float rdata40;
      } data;

tzprintio (headr)   /* Print routine */
   char *headr;
  {
      int i;
      char msg[80], msg1[80], *mp, *m1p;
      mp=msg;
      m1p=msg1;
      strcpy(msg,headr);
      strcpy(msg1,headblk);
      mp = mp + 12;
      m1p = m1p + 12;
      for (i=0; i<5; i++)
         {
         (void) sprintf(mp,"%12.7f",data.rdata[i]);
         mp=mp+12;
         (void) sprintf(m1p,"%12.7f",data.rdata[i+5]);
         m1p=m1p+12;
         }
      zvmessage(msg,"");
      zvmessage(msg1,"");
      zvmessage(""," ");
   }
/************************************************************************/
/*   Main Test routine for the "C" call to cylpatch.                    */
/************************************************************************/

void FTN_NAME(tzcylpatch)() 
   {
      int i;
      char msg[80], msg1[80], *mp, *m1p;
      mp=msg;
      m1p=msg1;
      for (i=0; i<38; i++) data.rdata[i] = 0.0;
      data.rdata40=0.0;
      data.idata=9;
      data.rdata[0]=1.0;
      data.rdata[1]=1.0;
      data.rdata[2]=85.7461;
      data.rdata[5]=239.916;
      data.rdata[6]=10.0;
      data.rdata[24]=1815.0;
      data.rdata[25]=1815.0;

      zvmessage("at line=1. sample=1. lati=85.7461 long=239.916"," ");
      zvmessage("radius=1815., scal=10","");
      (void) sprintf(mp,"RADII= %9.4f %9.4f",data.rdata[24],data.rdata[25]);
      zvmessage(msg,"");
      tzprintio(headin);
      zcylpatch(&data);
      zvmessage
      ("output should be lati=0 at line=182, sample=761 long=239.916"," ");
      (void) sprintf(msg,"sample=%12.5f",data.rdata[0]);
      zvmessage(msg,"");
      (void) sprintf(msg,"line=  %12.5f",data.rdata[1]);
      zvmessage(msg,"");
      (void) sprintf(msg,"lati=  %12.5f",data.rdata[2]);
      zvmessage(msg,"");
      (void) sprintf(msg,"long=  %12.5f",data.rdata[5]);
      zvmessage(msg,"");
      tzprintio(headout);

  /* second case  */  

      data.rdata[0]=100.0;
      data.rdata[1]=100.0;
      data.rdata[2]=26.8586;
      data.rdata[5]=208.6638;

      zvmessage("at line=100,samp=100,lati=26.8586,long=208.6638"," ");
      tzprintio(headin);
      zcylpatch(&data);
      zvmessage
      ("output should be lati=0 at line=182, sample=761 long=239.916", " ");
      (void) sprintf(msg,"sample=%12.5f",data.rdata[0]);
      zvmessage(msg,"");
      (void) sprintf(msg,"line=  %12.5f",data.rdata[1]);
      zvmessage(msg,"");
      (void) sprintf(msg,"lati=  %12.5f",data.rdata[2]);
      zvmessage(msg,"");
      (void) sprintf(msg,"long=  %12.5f",data.rdata[5]);
      zvmessage(msg,"");
      tzprintio(headout);
   }
