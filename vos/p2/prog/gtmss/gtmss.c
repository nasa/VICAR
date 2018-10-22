#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoGtUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  image left to right concatenate   A. Zobrist    10/28/99   */

void main44(void)
{
   int i,img,iout,jout,inpcnt,vunit[48],inl[48],ins[48],pixsiz;
   int bignline=0,bignsamp,vsize[4],sizepcnt,sizedef,outnline=0;
   int inptr[48],tpixsiz=0,status,o_unit,dummy,outnsamp;
   int labnl,labns,len,rot1,rot2,gtfirst,tolerct,tolerdf;
   int sctype1,sctype2,mapck,overlap1;
   char *labelstr1,*labelstr2;
   unsigned char *outbuf;
   double t[6],tinv[6],r[6],rinv[6],corner[4],rcorner[4];
   double toler,xcorner,ycorner,scorner,offsamp;
   double scale11,scale12,scale21,scale22;
   
   zifmessage("gtmss version 2015-09-10");
   
   /* get some parms */
   
   zvparm("SIZE",vsize,&sizepcnt,&sizedef,4,0);
   if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
   if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
   if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
   if (vsize[3]==0) zvp("NS",&vsize[3],&dummy);
   mapck = zvptst("MAPCK");
   overlap1 = zvptst("OVERLAP1");
   zvparmd("toler",&toler,&tolerct,&tolerdf,1,0);
   
   /* first the geotiff comparisons */
   
   status = gtgetlab("inp",1,&labelstr1,&labnl,&labns);
   gtfirst = status==1;
   if (gtfirst)
      {
      if (vsize[0]!=1||vsize[1]!=1||vsize[2]!=0||vsize[3]!=0)
         zmabend("Can't use size parms with GeoTIFF");
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      }
   status = zvpcnt("inp",&inpcnt);
   offsamp = (double)labns;
   if (overlap1) offsamp = offsamp-1.0;
   if (gtfirst&&mapck) for (img=1;img<inpcnt;img++)
      {
      status = gtgetlab("inp",img+1,&labelstr2,&labnl,&labns);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, i-th input");
      len = strlen(labelstr2);
      for (i=0;i<len;i++) labelstr2[i] = toupper(labelstr2[i]);
      status = gtrect(labelstr1,(double)1.0e-12);
      if (status!=1)
         zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
      status = gtrect(labelstr2,(double)1.0e-12);
      if (status!=1)
         zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
      status = geofix(labelstr2,r,rinv,labnl,labns,rcorner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, i-th input");
      status = gtmapcom(labelstr1,labelstr2);
      if (status!=1) zmabend("Mappings not compatible");
      rot1 = gtgetrot(labelstr1);
      rot2 = gtgetrot(labelstr2);
      if (rot1!=rot2)
         zmabend("Different rotations for two inputs, use GTROTATE");
      gtgetscl(labelstr1,&sctype1,&scale11,&scale12);
      gtgetscl(labelstr2,&sctype2,&scale21,&scale22);
      if (sctype1!=sctype2) /* this is redundant, see rotation */
         zmabend("Different rotations for two inputs, use GTROTATE");
      if (fabs(scale11-scale21)>toler*fabs(scale11))
         zmabend("Different scales for two inputs, use GTSIZE");
      if (fabs(scale12-scale22)>toler*fabs(scale12))
         zmabend("Different scales for two inputs, use GTSIZE");
      xcorner = r[0]+r[1]*(1.0-offsamp)+r[2];
      ycorner = r[3]+r[4]*(1.0-offsamp)+r[5];
      scorner = tinv[3]*xcorner+tinv[4]*ycorner+tinv[5];
      printf("Mapping discrepancy at seam %d = %15.12f\n",img,scorner-1.0);
      if (fabs(scorner-1.0)>toler)
         zmabend("Mapping across seam exceeds tolerance parameter");
      offsamp += (double)labns;
      if (overlap1) offsamp = offsamp-1.0;
      }
   if (gtfirst&&mapck)
      printf("All mapping discrepancies within tolerance\n");
      
   
   /* then OK to simply cat the files, the GeoTIFF label of the
   first image simply becomes the label for the whole image */
   
   bignsamp = 0;
   for (i=0;i<inpcnt;i++)
      {
      status = zvunit(&vunit[i],"INP",i+1, NULL);
      status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT","SA", NULL);
      zvget(vunit[i],"NL",&inl[i],"NS",&ins[i],"PIX_SIZE",&pixsiz, NULL);
      if (overlap1&&i<inpcnt-1) ins[i]--;
      
      /* resolve input samples */
      if (vsize[3]!=0) outnsamp = MIN(vsize[3],ins[i]-vsize[1]+1);
      else outnsamp = ins[i];
      if (vsize[3]>ins[i]-vsize[1]+1)
         printf("\nOutput samples truncated to match input size, input %d\n\n",
                  i+1);
      ins[i] = outnsamp;
      
      /* resolve input lines */
      if (vsize[2]!=0) outnline = MIN(vsize[2],inl[i]-vsize[0]+1);
      else outnline = inl[i];
      if (vsize[2]>inl[i]-vsize[0]+1)printf(
         "\nOutput lines truncated to match input size, input %d\n\n",i+1);
      inl[i] = outnline;
      
      if (i==0)
         {
         bignline = inl[0];
         inptr[0] = 0;
         tpixsiz = pixsiz;
         }
      else
         {
         if (inl[i]!=bignline) zmabend("Images must have same NL");
         if (pixsiz!=tpixsiz) zmabend("Images must have same pixel size");
         inptr[i] = inptr[i-1]+ins[i-1]*pixsiz;
         }
      bignsamp += ins[i];
      }
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",bignline,"U_NS",bignsamp,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   /* dynamically allocate the buffer */
   
   mz_alloc1((unsigned char **)&outbuf,bignsamp*pixsiz,1);
   
   /* read the input lines and concat for output */
   
   for (iout=0;iout<outnline;iout++)
      {
      for (jout=0;jout<inpcnt;jout++)
         {
         status = zvread(vunit[jout],&outbuf[inptr[jout]],
            "LINE",iout+vsize[0],"SAMP",vsize[1],
            "NSAMPS",ins[jout], NULL);
         }
      zvwrit(o_unit,outbuf,"LINE",iout+1,"SAMP",1,"NSAMPS",bignsamp, NULL);
      }
   
   
   for (i=0;i<inpcnt;i++) zvclose(vunit[i], NULL);
   zvclose(o_unit, NULL);
   return;
}
