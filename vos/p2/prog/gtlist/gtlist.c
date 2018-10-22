#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"

/*  GeoTIFF file list routine   A. Zobrist    8/16/99   */

static char msgBuf[10000];

void main44(void)
{
   int i,gtholder,nl,ns,rot,elen,nlen,pcount,pdef,status,len;
   char *labelstr,*p,*printlabel;
   double map[6],voff,vl,vs,gl,gs,east,north;
   double mapunitm,mapinch,scalefrac,horizpix=0;
   double mapinv[6],corner[4];
   
   /* initialize, fetch params */

   zifmessage("gtlist version 2016-01-13");
   
   status = gtgetlab("inp",1,&labelstr,&nl,&ns);
   if (status!=1)
      zmabend("Failed to read GeoTIFF label");
      
   len = strlen(labelstr);
   if ((printlabel=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
   for (i=0;i<=len;i++)
      {
      printlabel[i] = labelstr[i];
      labelstr[i] = toupper(labelstr[i]);
      }
   if (nl==1&&ns==1)
      {
      zvparm("listnl",&nl,&pcount,&pdef,1,0);
      zvparm("listns",&ns,&pcount,&pdef,1,0);
      gtholder = 0;
      }
   else gtholder = 1;
   status = geofix(labelstr,map,mapinv,nl,ns,corner);
   if (status!=1)
      {
	zifmessage("No mapping in GeoTIFF label");
	zifmessage("The GeoTIFF label is:");
	zifmessage(printlabel);
      return;
      }
   rot = gtgetrot(labelstr);
      
   zvparmd("mapunitm",&mapunitm,&pcount,&pdef,1,0);
   zvparmd("mapinch",&mapinch,&pcount,&pdef,1,0);
     
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   
   /* printing section */
   
   zifmessage("VICAR GeoTIFF LABEL LIST");
   if (!gtholder)
      {
      zifmessage("The file is a standalone VICAR GeoTIFF label");
      sprintf(msgBuf, "A hypothetical %d x %d VICAR image will", nl, ns);
      zifmessage(msgBuf);
      zifmessage("be used to illustrate the mapping of corner points.");
      }
   
   zifmessage("The VICAR GeoTIFF label is:");
   zifmessage(printlabel);
   
   if (voff<0.75)
     zifmessage("The image raster is an 'area' type");
   else
     zifmessage("The image raster is a 'point' or 'post' type");
   
   zifmessage("The centers of the corner pixels are:");
   zifmessage("VICAR-line    -samp GeoTIFF-samp    -line            East           North");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*((double)nl-1.0)+1.0;
      vs = (double)(i%2)*((double)ns-1.0)+1.0;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      sprintf(msgBuf, "%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f",
         vl,vs,gs,gl,elen,east,nlen,north);
      zifmessage(msgBuf);
      }          
   
   zifmessage("The outer corners of the corner pixels are:");
   zifmessage("VICAR-line    -samp GeoTIFF-samp    -line            East           North");
   
   for (i=0;i<4;i++)
      {
      vl = (double)(i/2)*(double)nl+0.5;
      vs = (double)(i%2)*(double)ns+0.5;
      gl = vl-voff;
      gs = vs-voff;
      east = map[0]*vl+map[1]*vs+map[2];
      elen = MAX(12-(int)(log10(fabs(east)+.9)),1);
      north = map[3]*vl+map[4]*vs+map[5];
      nlen = MAX(12-(int)(log10(fabs(north)+.9)),1);
      sprintf(msgBuf, "%10.1f%9.1f%13.1f%9.1f %15.*f %15.*f",
         vl,vs,gs,gl,elen,east,nlen,north);
      zifmessage(msgBuf);
      }          
   
   zifmessage("The rotation of the image relative to an E-N geographic frame is:");
   switch (rot)
      {
      case 0:  zifmessage("rotation 0\n369\n258\n147"); break;
      case 1:  zifmessage("rotation 1\n123\n456\n789"); break;
      case 2:  zifmessage("rotation 2\n741\n852\n963"); break;
      case 3:  zifmessage("rotation 3\n987\n654\n321"); break;
      case 4:  zifmessage("rotation 4\n963\n852\n741"); break;
      case 5:  zifmessage("rotation 5\n789\n456\n123"); break;
      case 6:  zifmessage("rotation 6\n147\n258\n369"); break;
      case 7:  zifmessage("rotation 7\n321\n654\n987"); break;
      default:
         zifmessage("NOT ALIGNED WITH EAST-NORTH COORDINATE SYSTEM");
      }
   
   zifmessage("The scale units of the image are (ignoring sign):");
   switch (rot)
      {
      case 0: case 2: case 4: case 6:
         elen = MAX(13-(int)(log10(fabs(map[4])+.9)),1);
         sprintf(msgBuf, "1 sample = %15.*f map units north",elen,fabs(map[4]));
	 zifmessage(msgBuf);
         sprintf(msgBuf, "1 line   = %15.*f map units east",elen,fabs(map[0]));
	 zifmessage(msgBuf);
         horizpix = (double)nl*map[0];
         break;
      case 1: case 3: case 5: case 7:
         elen = MAX(13-(int)(log10(fabs(map[1])+.9)),1);
         sprintf(msgBuf, "1 sample = %15.*f map units east",elen,fabs(map[1]));
	 zifmessage(msgBuf);
         sprintf(msgBuf, "1 line   = %15.*f map units north",elen,fabs(map[3]));
	 zifmessage(msgBuf);
         horizpix = (double)ns*map[1];
         break;
      default:
         zifmessage("SEE TRANSFORMATION MATRIX");
      }
   
   scalefrac = fabs((horizpix*mapunitm*39.0)/mapinch);
   sprintf(msgBuf, "The scale fraction is 1 /%9.1f",scalefrac);
   zifmessage(msgBuf);
   sprintf(msgBuf, "(assuming mapunit = %f meters and the map is %f inches)",
           mapunitm,mapinch);
   zifmessage(msgBuf);
      
   return;
}
