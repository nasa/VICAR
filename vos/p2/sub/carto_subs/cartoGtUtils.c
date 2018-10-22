#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "zmabend.h"
#include "zvproto.h"

#include "cartoGtUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

/*================================================================

int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*/

int gtgetlab( char * inp, int instance, char ** labelstr, int * nl, int * ns )
{
   int i,status,geounit;
   int maxlen,nelement,len;
   char *buf,valformat[9],vformat[9];
   char svalue[133],key[33];
   
   /* malloc large temporary buffer for reading the string */
   
   mz_alloc1((unsigned char **)&buf,1000001,1);
   
   /* open file */
   
   status = zvunit(&geounit,inp,instance,NULL);
   status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA",NULL);
      
   strcpy(buf,"");
   do
      {
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      if (strcmp(key,"NL")==0)
         {
	   status=zlget(geounit,"SYSTEM",key,(char*)nl,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      if (strcmp(key,"NS")==0)
         {
	   status=zlget(geounit,"SYSTEM",key,(char*)ns,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      status=zlinfo(geounit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         strcat(buf,key);
         strcat(buf,"=");
         strcat(buf,svalue);
         strcat(buf,"\n");
         }
      }
   while (1);
   status = zvclose(geounit,NULL);
   
   /* resave in smaller buffer */
   
   len = strlen(buf);
   if (((*labelstr)=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
   strcpy(*labelstr,buf);
   
   free(buf);
   if (strlen(*labelstr)<1) return 0; else return 1;
}

/*================================================================

int invertmap

invertmap calculates the inverse of a six-vector double precision
map.

function return:
     ier from the dgauss call (see dgauss)

arguments:
      1. map: double[6] map;
	 (input) coefficients to convert pixel to coord OR
	 COORD TO PIXEL
      2. invmap: double[6] invmap;
	 (output) coefficients to convert the other way
*/

int invertmap( double * t, double * tinv)
{
   int i,ier;
   double work[9];
   
   for (i=0;i<2;i++)
      {
      work[0] = t[2];
      work[1] = 100.0*t[1]+t[2];
      work[2] = 100.0*t[0]+t[2];
      work[3] = t[5];
      work[4] = 100.0*t[4]+t[5];
      work[5] = 100.0*t[3]+t[5];
      work[6] = 1.;
      work[7] = 1.;
      work[8] = 1.;
      if (i==0)
         {
         tinv[0] = 0.0;
         tinv[1] = 0.0;
         tinv[2] = 100.0;
         }
      else
         {
         tinv[3] = 0.0;
         tinv[4] = 100.0;
         tinv[5] = 0.0;
         }
      dgauss(work,&tinv[i*3],3,1.e-14,&ier);
      }
   
   return ier;
}

/*================================================================

int geofix

geofix translates label coordinates into a linear transformation
vectors that can be used for VICAR pixel-to-map or map-to-pixel
conversions.  If the file is a VICAR image then the mapping of 
the corner points is also returned (the (1,1) and (nline,nsamp)
centers of the corner pixels).

The convention for the transforms is (line,samp) -> (East,North)
for the map and (East,North) -> (line,samp) for the invmap
for convenience in working with VICAR.

Note that VICAR pixel referencing is different than GeoTIFF
pixel referencing (both "area" and "point"/"post" types).

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. map: double[6] map;
	 (output) coefficients to convert pixel to coord
      3. invmap: double[6] invmap;
	 (output) coefficients to convert coord to pixel
      4. nl: int nl;
	 (input) number of lines in vicar image to calc corner
      5. ns: int ns;
	 (input) number of samples in vicar image to calc corner
      6. corner: double[4] corner;
	 (output) the mapping of the corners (.5,.5,nl+.5,ns+.5)
	 if the file is a VICAR image, else zero
*/

int geofix( char * labelstr, double * map, double * invmap, int nl, int ns, double * corner)
{
   int i,vtype;
   int ireturn;
   char *p;
   double tie[4],voff,ddummy,scale[2];
   
   for (i=0;i<6;i++) { map[i] = 0.; invmap[i] = 0.; }
   map[0] = 1.; map[5] = 1.;
   invmap[0] = 1.; invmap[5] = 1.;
   ireturn = 1;
   
   vtype = nl!=(-1);
      
   /* read the model transformation or get the scale, etc.  Note
   reversal of matrix from samp-line to line-samp */
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
     
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   if (vtype)
      {
      corner[0] = 0.5*map[0]+0.5*map[1]+map[2];
      corner[1] = 0.5*map[3]+0.5*map[4]+map[5];
      corner[2] = ((double)nl+0.5)*map[0]+((double)ns+0.5)*map[1]+map[2];
      corner[3] = ((double)nl+0.5)*map[3]+((double)ns+0.5)*map[4]+map[5];
      }
   else for (i=0;i<4;i++) corner[i] = (double)0.0;
   
   invertmap(map,invmap);
   
   closem:
   
   /*for (i=0;i<6;i++) printf("map trans[%d] %f\n",i+1,map[i]);
   for (i=0;i<6;i++) printf("inv trans[%d] %f\n",i+1,invmap[i]);*/
   
   return ireturn;
}

/*================================================================

int gtrect

gtrect tests whether the mapping is "rectangular".  This means 
that the GeoTIFF label has the keyword MODELPIXELSCALETAG or that
it has the keyword MODELTRANSFORMATIONTAG and the upper left part
of the transformation has two 0.0 in diagonal formation.  To allow
for a slight inaccuracy in a calculated transformation, a 
parameter eps is provided for values very close to 0. It actually
ratios with the main terms, see code below.

function return:
     int, 1 if mapping is rectangular, else 0

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. eps: double eps;
	 (input) tolerance about zero for the off-diagonals
	 to still consider the mapping rectangular. It is a
	 ratio to the largest term.  suggest 1.0e-12.
*/

int gtrect( char * labelstr, double eps )
{
   char *p;
   double map[4],ddummy,largest,thresh;
   
   /* read the model transformation or read if scale */
   
   p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
   if (p!=0)
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      return 1;
      }
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[0] = fabs(ms_dnum(&p)); p++;
      map[1] = fabs(ms_dnum(&p)); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = fabs(ms_dnum(&p)); p++;
      map[3] = fabs(ms_dnum(&p));
      largest = MAX(map[0],map[1]);
      largest = MAX(largest,map[2]);
      largest = MAX(largest,map[3]);
      thresh = eps*largest;
      if (map[0]<thresh&&map[3]<thresh) return 1;
      if (map[1]<thresh&&map[2]<thresh) return 1;
      }
   return 0;
}

int gtcompval( char * p1, char * p2 )
{
   int charseq,stcomment;
   char *p,*q;
   double dval1,dval2,epsl,epsu;
   
   epsl = 1.0-1.0e-12;
   epsu = 1.0+1.0e-12;
   p = p1; q = p2;
   charseq = 0;
   stcomment = 0;
   do
      {
      while (*p==' ') p++;
      while (*q==' ') q++;
      if (*p=='\n') return 1;
      else if (*p==(char)0) return 1;
      else if (stcomment&&*p=='(') return 1;
      else if (isalpha(*p)||(isdigit(*p)&&charseq==1))
         {
         charseq = 1;
         stcomment = 1;
         if (*q==*p) { p++; q++; }
         else return 0;
         }
      else if (*p=='('||*p==')'||*p==','||*p==')'||*p=='=')
         {
         charseq = 0;
         if (*q=='('||*q==')'||*q==','||*q==')'||*q=='=') { p++; q++; }
            else return 0;
         }
      else if (isdigit(*p)||*p=='.'||*p=='-')
         {
         stcomment = 1;
         dval1 = ms_dnum(&p); 
         dval2 = ms_dnum(&q);
         if (dval1<0.0) { dval1 = -dval1; dval2 = -dval2; }
         if (dval1<dval2*epsl||dval1>dval2*epsu) return 0;
         }
      else return 0;
      }
   while (1);
}

/*================================================================

int gtmapcom

gtmapcom tests whether the two mappings are compatible.  This is
defined as having the same values for a list of attributes that
pertain to mappings.  The list is taken from the GeoTIFF spec
rev 1.0.  If the "value" contains parenthetical material, it is
not required to be the same.  Vector values are required to be
the same.  There is a tolerance of 1 part in 1e-12 on all numeric
values.

If one of the listed attributes is present in one label it must
also be present in the other label.  This prevents default values
from making two labels incompatible, or for alternate keywords
to do the same.

function return:
     int, 1 if mappings are compatible, else 0

arguments:
      1. labelstr1: char *labelstr;
	 (input) string containing the first GeoTIFF label
      1. labelstr2: char *labelstr2;
	 (input) string containing the second GeoTIFF label
*/

int gtmapcom( char * labelstr1, char * labelstr2 )
{
#define numattrib 45
   int iattrib,status;
   char *p1,*p2;
   char attrib[numattrib][34] = {"GTRASTERTYPEGEOKEY","GTMODELTYPEGEOKEY",
     "GEOGRAPHICTXGEOKEY","GEOGGEODETICDATUMGEOKEY","GEOGPRIMEMERIDIANGEOKEY",
     "GEOGLINEARUNITSGEOKEY","GEOGLINEARUNITSIZEGEOKEY","GEOGANGULARUNITSGEOKEY",
     "GEOGANGULARUNITSIZEGEOKEY","GEOGELLIPSOIDGEOKEY","GEOGSEMIMAJORAXISGEOKEY",
     "GEOGSEMIMINORAXISGEOKEY","GEOGINVFLATTENINGGEOKEY","GEOGAZIMUTHUNITSGEOKEY",
     "GEOGPRIMEMERIDIANLONGGEOKEY","PROJECTEDCSTYPEGEOKEY","PROJECTIONGEOKEY",
     "PROJCOORDTRANSGEOKEY","PROJLINEARUNITSGEOKEY","PROJLINEARUNITSIZEGEOKEY",
     "PROJSTDPARALLEL1GEOKEY","PROJSTDPARALLEL2GEOKEY","PROJNATORIGINLONGGEOKEY",
     "PROJNATORIGINLATGEOKEY","PROJFALSEEASTINGGEOKEY","PROJFALSENORTHINGGEOKEY",
     "PROJFALSEORIGINLONGGEOKEY","PROJFALSEORIGINLATGEOKEY","PROJFALSEORIGINEASTINGGEOKEY",
     "PROJFALSEORIGINNORTHINGGEOKEY","PROJCENTERLONGGEOKEY","PROJCENTERLATGEOKEY",
     "PROJCENTEREASTINGGEOKEY","PROJCENTERNORTHINGGEOKEY","PROJSCALEATNATORIGINGEOKEY",
     "PROJSCALEATCENTERINGEOKEY","PROJAZIMUTHANGLEGEOKEY","PROJSTRAIGHTVERTPOLELONGGEOKEY",
     "PROJSTDPARALLELGEOKEY","PROJORIGINLONGGEOKEY","PROJORIGINLATGEOKEY",
     "PROJSCALEATORIGINGEOKEY","VERTICALCSTYPEGEOKEY","VERTICALDATUMGEOKEY",
     "VERTICALUNITSGEOKEY"};
   /*int numattrib = 45;*/
   
   /* loop over attributes in string 1, finding match in string 2,
   the second part of the loop does the reverse check */
   
   for (iattrib=0;iattrib<numattrib;iattrib++)
      {
      p1 = ms_find(labelstr1,attrib[iattrib]);
      if (p1!=0)
         {
         p2 = ms_find(labelstr2,attrib[iattrib]);
         if (p2==0)
            {
            printf("Missing attribute in label 2: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      p2 = ms_find(labelstr2,attrib[iattrib]);
      if (p2!=0)
         {
         p1 = ms_find(labelstr1,attrib[iattrib]);
         if (p1==0)
            {
            printf("Missing attribute in label 1: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      }
   
   return 1;
}

/*================================================================

int gtgetrot

gtgetrot gets the rotation number of the GeoTIFF label.  A standard
VICAR rotation is 1.  See the help PDF for VICAR routine GTLIST for
information on the other rotations.

function return:
     int, a number from 0 to 8 designating the rotation relative to
     an (East,North) coordinate system.

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
*/

int gtgetrot( char * labelstr )
{
   int rot=0;
   char *p;
   double map[6],tie[4],scale[2],ddummy,xmain,xcross,xtot,voff;
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
      
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   xmain = fabs(map[0])+fabs(map[4]);
   xcross = fabs(map[1])+fabs(map[3]);
   xtot = xmain+xcross;
   
   if (xmain/xtot<1.e-10)
      {
      if (map[1]>0.0&&map[3]>0.0) rot = 5;
      else if (map[1]>0.0&&map[3]<0.0) rot = 1;
      else if (map[1]<0.0&&map[3]>0.0) rot = 3;
      else if (map[1]<0.0&&map[3]<0.0) rot = 7;
      }
   else if (xcross/xtot<1.e-10)
      {
      if (map[0]>0.0&&map[4]>0.0) rot = 0;
      else if (map[0]>0.0&&map[4]<0.0) rot = 6;
      else if (map[0]<0.0&&map[4]>0.0) rot = 4;
      else if (map[0]<0.0&&map[4]<0.0) rot = 2;
      }
   else rot = -1;
   
   return rot;
}

/*================================================================

gtreplab

gtreplab writes a GeoTIFF label into the property part of VICAR label

function return : void

arguments:
      1. fileparm: input, char *fileparm;
         name of the file to put the label, usually "INP"
      2. nfile: input, int nfile;
         which file of fileparm, set to 1 if only one file
      3. labstr: input, char *labstr;
         string containing new GeoTIFF label, comma separated,
         see GTGEN document for format details, and TSTGTGEN.PDF
         for examples
      4. add: input, int add;
         0 - then the old label is deleted and labstr
             becomes the new label
         1 - then the old label is kept and added to,
      5. replflag: input, int replflag;
         0 - no processing of coord-pixel mapping
         1 - coord-pixel mapping replaced w/ next three params
         2 - coord-pixel mapping replaced w/ next three params, but
             MODELPIXELSCALETAG and MODELTRANSFORMATIONTAG type labels
             are swapped due to rotation
      6. tinv: input, double tinv[6];
         will recalculate every MODELTIEPOINTTAG using this transf
      7. scalestr: input, char *scalestr;
         will replace an occurrence of MODELPIXELSCALETAG with this
      8. transstr: input, char *transstr;
         will replace an occurrence of MODELTRANSFORMATIONTAG with this
      
*/

void gtreplab( char * fileparm, int nfile, char * labstr, int add, int replflag, double * tinv, char * scalestr, char * transstr )
{
   int geounit,nelement,maxlen,status,n;
   char key[33],valformat[9],temp1[100],temp2[133],buf[133];
   char ttemp1[100],ttemp2[133];
   char *p,*q,*p1,*q1,tbuf[30];
   double dummy,coord1,coord2,pline,psamp;
   
   status=zvunit(&geounit,fileparm,nfile,NULL);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA",NULL);
   if (add!=1) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
         &maxlen,&nelement,"PROPERTY","GEOTIFF",
         "ERR_ACT"," ",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      
      if (add==2&&strcmp(key,"MODELPIXELSCALETAG")!=0
         &&strcmp(key,"MODELTIEPOINTTAG")!=0
         &&strcmp(key,"MODELTRANSFORMATIONTAG")!=0) continue;
      
      status=zldel(geounit,"PROPERTY",key,
         "ERR_ACT","SA","PROPERTY","GEOTIFF",NULL);
      }
   while (1);
   p = labstr;
   q = p+strlen(labstr);
   do
      {
      n = grab(p,'=',temp1);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      n = grab(p,'\n',temp2);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      
      if (replflag>0&&strcmp(temp1,"MODELPIXELSCALETAG")==0)
         {
         if (replflag==1) strcpy(temp2,scalestr);
         else
            {
            strcpy(temp1,"MODELTRANSFORMATIONTAG");
            strcpy(temp2,transstr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTRANSFORMATIONTAG")==0)
         {
         if (replflag==1) strcpy(temp2,transstr);
         else
            {
            strcpy(ttemp1,"MODELPIXELSCALETAG");
            strcpy(ttemp2,scalestr);
            status=zladd(geounit,"PROPERTY",ttemp1,ttemp2,
               "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
               "PROPERTY","GEOTIFF",NULL);
            /* add a tiepoint, in case transformation label didn't have one */
            strcpy(temp1,"MODELTIEPOINTTAG");
            p1 = &temp2[1];
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            coord1 = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            coord2 = ms_dnum(&p1); p1++;
            psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
            pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
            nicelen("(",psamp,buf);
            nicelen(",",pline,tbuf);
            strcat(buf,tbuf);
            strcat(buf,",0.0");
            nicelen(",",coord1,tbuf);
            strcat(buf,tbuf);
            nicelen(",",coord2,tbuf);
            strcat(buf,tbuf);
            strcat(buf,",0.0)");
            strcpy(temp2,buf);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTIEPOINTTAG")==0)
         {
         p1 = &temp2[1];
         dummy = ms_dnum(&p1); p1++;
         dummy = ms_dnum(&p1); q1 = p1; p1++;
         dummy = ms_dnum(&p1); p1++;
         coord1 = ms_dnum(&p1); p1++;
         coord2 = ms_dnum(&p1);
         psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
         pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
         nicelen("(",psamp,buf);
         nicelen(",",pline,tbuf);
         strcat(buf,tbuf);
         strcat(buf,q1);
         strcpy(temp2,buf);
         }
      
      status=zladd(geounit,"PROPERTY",temp1,temp2,
         "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
         "PROPERTY","GEOTIFF",NULL);
      }
   while (p<q);
   zvclose(geounit, NULL);
   return;
}

/*================================================================

int gtgetscl

gtgetscl gets the scale factors of the GeoTIFF label.  There are two
types of scale that can be returned:

The first is for the four rotations that can be represented by the
MODELTIEPOINTTAG-MODELPIXELSCALETAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips".  The
odd thing about this scale is that the increasing "lines" coordinate
to the South is denoted by a positive scale factor.

The second is for the four rotations that have to be represented
by the MODELTRANSFORMATIONTAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips" of a
single ninety degree rotate.

function return:
     void

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. sctype: int *sctype;
	 (output) type of scale factors (see above)
	 1 - from MODELPIXELSCALETAG
	 2 - from MODELTRANSFORMATIONTAG
      3. scale1: double *scale1;
	 (output) scale factor 1
      4. scale2: double *scale2;
	 (output) scale factor 2
*/

void gtgetscl( char * labelstr, int * sctype, double * scale1, double * scale2 )
{
   char *p;
   double ddummy;
   
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      *sctype = 2;
      ddummy = ms_dnum(&p); p++;
      *scale1 = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p); p++;
     }
   else
      {
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      *sctype = 1;
      *scale1 = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p);
      }
    
   return;
}
