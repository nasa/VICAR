#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "defines.h"
#include "cartoGtUtils.h"
#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoLsqUtils.h"
/*#include "cartoVicarProtos.h"*/

/*  GeoTIFF file creation routine   A. Zobrist    8/4/99   */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */

int strncmp_p_ins(p,q,n)
   char *p,*q; int n;
{
   char *ptemp; int i;
   
   if ((ptemp=(char *)malloc(n))==NULL) zmabend("malloc failed");
   for (i=0;i<n;i++) ptemp[i] = toupper(p[i]);
   return strncmp(ptemp,q,n);
}

void main44(void)
{
   int i,len,add,addtie,tiecnvrt,gtcount,gtdef,inpcnt;
   int tiecount,ier,firstpoint,scaletype=0,twofilecase;
   int labnl,labns,i_unit,o_unit,status,nline,nsamp;
   int infilecase,pixsiz,rectfit,outcnt;
   char *labelstr,gtparms[40][200],buf[300];
   char *p,*imbuf;
   double img1[9],img2[9],map[6],ddummy,xmain,xcross,xtot;
   
   /* initialize, fetch params, two input files a special case */

   zifmessage("gtgen version Fri Jan 11 2008");
   
   status = zvpcnt("inp",&inpcnt);
   status = zvpcnt("out",&outcnt);
   if (inpcnt>=1)
      {
      status = zvunit( &i_unit, "INP",1,NULL);
      status = zvopen( i_unit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
      zvget(i_unit,"NL",&nline,"NS",&nsamp,"PIX_SIZE",&pixsiz,NULL);
      infilecase = 1;
      }
   else 
      {
      nline = 1;
      nsamp = 1;
      pixsiz = 1;
      infilecase = 0;
      }
   if (inpcnt==2)
      {
      status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
      if (status!=1) zmabend("second input file problem");
      len = strlen(labelstr);
      for (i=0;i<=len;i++) labelstr[i] = toupper(labelstr[i]);
      if (len<2) zmabend("There was no GeoTIFF label in second data input");
      twofilecase = 1;
      }
   else twofilecase = 0;
   
   add = zvptst("ADD");
   tiecnvrt = zvptst("TIECNVRT");
   addtie = add&&tiecnvrt;
   rectfit = zvptst("RECTFIT");
   if (add&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (add&&!infilecase)
      zmabend("Add not allowed for no input file case");
   if (tiecnvrt&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (add) printf("add mode on\n"); else printf("add mode off\n");
   if (tiecnvrt) printf("tiecnvrt mode on\n"); else printf("tiecnvrt mode off\n");
   zvparm("geotiff",gtparms,&gtcount,&gtdef,40,200);
   if (gtcount>0&&twofilecase)
      zmabend("No parameters allowed for two input file case");
   if (twofilecase) goto skipgtparms;
   
   mz_alloc1((unsigned char **)&labelstr,4000,1);
   strcpy(labelstr,"");
   if (tiecnvrt)
      {
      tiecount = 0;
      for (i=0;i<gtcount;i++)
         if (strncmp_p_ins(gtparms[i],"MODELTIEPOINTTAG",16)==0)
            {
            p = &gtparms[i][18];
            img1[tiecount] = ms_dnum(&p); p++;
            img1[tiecount+3] = ms_dnum(&p); p++;
            img1[tiecount+6] = 1.0;
            img2[tiecount] = img1[tiecount];
            img2[tiecount+3] = img1[tiecount+3];
            img2[tiecount+6] = 1.0;
            ddummy = ms_dnum(&p); p++;
            map[tiecount] = ms_dnum(&p); p++;
            map[tiecount+3] = ms_dnum(&p); p++;
            tiecount++;
            if (tiecount==3) break;
            }
      dgauss(img1,map,3,1.e-14,&ier);
      if (ier!=0) zmabend("Tiepoints collinear");
      dgauss(img2,&map[3],3,1.e-14,&ier);
      if (ier!=0) zmabend("Tiepoints collinear");
      xmain = fabs(map[0])+fabs(map[4]);
      xcross = fabs(map[1])+fabs(map[3]);
      xtot = xmain+xcross;
      scaletype = xcross/xtot<1.e-10;
      if (rectfit)
         {
         scaletype = xcross<xtot;
         if (scaletype)
            {
            map[1] = 0.0;
            map[3] = 0.0;
            }
         else
            {
            map[0] = 0.0;
            map[4] = 0.0;
            }
         }
      }
   
   /* logic for linefeeds is:  before any item except first  */
   
   firstpoint = 1;
   for (i=0;i<gtcount;i++)
      {
      len = strlen(gtparms[i]);
      if (!tiecnvrt||!(strncmp_p_ins(gtparms[i],"MODELTIEPOINTTAG",16)==0))
         {
         if (i>0) strcat(labelstr,"\n");
         sprintf(buf,"%s",gtparms[i]);
         strcat(labelstr,buf);
         }
      else
         {
         if (firstpoint)
            {
            if (i>0) strcat(labelstr,"\n");
            if (scaletype)
               {
               sprintf(buf,"%s",gtparms[i]);
               strcat(labelstr,buf);
               strcat(labelstr,"\nModelPixelScaleTag=");
               scalefmt(buf,map[0],-map[4]);
               strcat(labelstr,buf);
               }
            else
               {
               strcat(labelstr,"ModelTransformationTag=");
               trnsfmt(buf,map);
               strcat(labelstr,buf);
               }
            }
         firstpoint = 0;
         }
      }
   
   /* process the output, two cases: creating a new 1 x 1 or copying
   first input image */
   
   skipgtparms:
   if (outcnt==0&&inpcnt==1) goto updatecase;
   status = zvunit(&o_unit,"OUT",1,NULL);
   status=zvopen(o_unit,"U_NL",nline,"U_NS",nsamp,
		"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
   mz_alloc1((unsigned char **)&imbuf,nsamp*pixsiz+16,1);
   
   if (infilecase) for (i=0;i<nline;i++)
      {
      status = zvread(i_unit,imbuf,"LINE",i+1,"SAMP",1,"NSAMPS",nsamp,NULL);
      status = zvwrit(o_unit,imbuf,"LINE",i+1,"SAMP",1,"NSAMPS",nsamp,NULL);
      }
      
   /* now put labelstr in the state label under GeoTIFF, map param is a dummy
      here, is not used */
   
   zvclose(o_unit,NULL);
   gtreplab("OUT",1,labelstr,add+addtie,0,map,"","");
      
   return;
   
   /* update case operates on input */
   
   updatecase:
   zvclose(i_unit,NULL);
   gtreplab("INP",1,labelstr,add+addtie,0,map,"","");
      
   return;
   
}
