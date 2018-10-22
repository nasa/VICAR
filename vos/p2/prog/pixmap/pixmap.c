#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoVicarProtos.h"*/
#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoGtUtils.h"

/************************************************************************/
/* program pixmap                                                      */
/************************************************************************/
/*  99-08 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int i,cols[4],maptopix,pixtomap,ix,inpcnt,unit,colcount,coldef;
   int ibis,status,clen,labnl,labns,len,igcount,igdef;
   char *labelstr;
   double *xpar,*ypar,t[6],tinv[6],x,y,corner[4],ignore[2];
   double xmag,ymag;
           
   zifmessage("pixmap version Thu Jan  3 2008");
   
   /* get the basic parameters and calculate the mapping */
   
   maptopix = zvptst("maptopix");
   pixtomap = zvptst("pixtomap");
   if (maptopix&&pixtomap)
      zmabend("Only one keyword for mapping direction can be given");
   if (!maptopix&&!pixtomap)
      zmabend("One keyword for mapping direction must be given");
   
   if (maptopix) ix = 0; else ix = 2;
   zvparm("mapcols",&cols[ix],&colcount,&coldef,2,0);
   zvparm("pixcols",&cols[2-ix],&colcount,&coldef,2,0);
   status = zvpcnt("inp",&inpcnt);
   if (inpcnt!=2) zmabend("Requires two input files");
   zvparmd("ignore",ignore,&igcount,&igdef,2,0);
   if (igcount!=2) zmabend("Ignore param requires two values");
   
   printf("converting columns (%d,%d) to columns (%d,%d)\n",cols[0],
       cols[1],cols[2],cols[3]);
   
   /* calculate the mapping */
   
   status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t,tinv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from GeoTIFF label");
   if (ix==0) for (i=0;i<6;i++) t[i] = tinv[i];
   
   /* read in points from the ibis interface file */

   status = zvunit(&unit,"inp",1,NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   mz_alloc1((unsigned char **)&xpar,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   
   for (i=0;i<4;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   status = IBISColumnRead(ibis,(char*)xpar,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)ypar,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* calculate the output data */
   
   xmag = 0.001; ymag = 0.001;
   for (i=0;i<clen;i++)
      {
      xmag = MAX(fabs(xpar[i]),xmag);
      ymag = MAX(fabs(ypar[i]),ymag);
      }
   xmag *= 1.0e-14; ymag *= 1.0e-14;
   for (i=0;i<clen;i++)
      {
      x = xpar[i];
      y = ypar[i];
      if (fabs(x-ignore[0])<xmag&&fabs(y-ignore[1])<ymag)
         {
         xpar[i] = x;
         ypar[i] = y;
         }
      else
         {
         xpar[i] = x*t[0]+y*t[1]+t[2];
         ypar[i] = x*t[3]+y*t[4]+t[5];
         }
      }
   
   /* Output points to the ibis interface file */
   
   if (clen>0) status = IBISColumnWrite(ibis,(char*)xpar,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   if (clen>0) status = IBISColumnWrite(ibis,(char*)ypar,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
