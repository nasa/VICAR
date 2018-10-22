/* rewritten in C by Thomas Roatsch, DLR, 13-Mar-2001 */

/* This routine should never be called directly by an application
   program.  It is used by the map label readers to process IBM (old-style)
   map projection labels.

   Arguments: UNIT   - input I*4 - unit of file to be searched
              DATA   - output *4 (40) - MAP data buffer
              return value  status code
  History

   Original Programmer: 		L.W.Kamp
   Current Cognizant Programmer:	L.W.Kamp
   Date written:			18 September 1993 (based on SEARCV2)
   Source Language: 			FORTRAN

   Revisions:				

   19sep93 -lwk- this is the old SEARCV2 code with a simplified parameter
		 list, to be called by MP_LABEL_READ and the new SEARCV2
   01dec93 -lwk- replaced XLGETLABEL by multiple XLGETs
   11jan94 -lwk- fixed bugs in Mercator case and for cases where map labels
		 are not in the last task of the label
   16feb94 -lwk- fixed bug getting SCALE in simple cylindrical
   25apr94 -jfm- revised test pdf to be useful on all platforms
   13mar95 -jfm- added retrieval of PXLS/DEG to functionality
		 for projections: MERCATOR, TRANSVERSE MERCATOR, 
		 CYLINDRICAL EQUAL AREA, and SIMPLE CYLINDRICAL.
   04apr95 -lwk- fixed "Instances" in xlhinfo/xlget calls
   26apr95 -ffm- initialize n  to 0 if CANNOT_FIND_KEY(-38)
		 initialize bufsize to 0 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <zvproto.h>
#include "find_hist_key.h"
#include "momati.h"
#include "mp_routines.h"

#define NHST   100  /* Maximum number of VICAR label tasks  */

/* amod from FORTRAN */
float myamod (float a, float b)
{
a = a - ((int)(a/b)) * b;

return a;
}

/* get lat, long, line, samp, scale */
int getllsls(char *map4, char *map5, float *data)
{
char   *value;
char   helpstring[255];

value = strstr(map4,"S="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[2],"%f",&data[0]);
   }
else return -7;
value = strstr(map4,"L="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[2],"%f",&data[1]);
   }
else return -7;
value = strstr(map4,"LAT="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[4],"%f",&data[2]);
   }
else return -7;
value = strstr(map4,"LONG="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[5],"%f",&data[5]);
   }
else return -7;
value = strstr(map5,"SCALE="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[6],"%f",&data[6]);
   }
else return -7;
if (data[2] < 0) data[7]=-1;
return 1;
}

/* get north */
int getnorth(char *map5, float *data)
{
char   *value;
char   helpstring[255];

value = strstr(map5,"NORTH="); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[6],"%f",&data[8]);
   return 1;
   }
else return -7;
}

int searcv3_c( int unit, float data[SEARCV3_BUFLEN], int idata[SEARCV3_BUFLEN])
{
/* NOTE:  in the original code, when multiple tasks had performed map
   projections in the history, the Radii are taken from the *first* one
   encountered (even though the current map projection is taken from the
   last)!  This is considered to be a bug (although it will be very rare
   to change the radius of the target body when reprojecting) and has
   been revised in this version. 

    LABEL SEARCH FOR MAP3 PARAMETERS:
      DATA(1)      X         SPECIAL SAMPLE POINT
      DATA(2)      Z         SPECIAL LINE POINT
      DATA(3)      THETA0    SPECIAL LATITUDE POINT DEG.
      DATA(4)      THETA1    LATITUDE OF SPECIAL PARALLEL DEG.
      DATA(5)      THETA2    LATITUDE OF SPECIAL PARALLEL DEG.
      DATA(6)      LAMDA0    SPECIAL LONGITUDE DEG.
      DATA(7)      F         SCALE KM/PIXEL
      DATA(8)      CAS       +1 IF VISIBLE POLE IS N.  -1 IF SOUTH
      DATA(9)      PSI       NORTH ANGLE DEG.
      DATA(10)     RES	     MAP RESOLUTION IN PXLS/DEG
      DATA(25)               polar radius
      DATA(26)               equatorial radius
      DATA(39)     PROJECTION TYPE OF PICTURE I*4
                   POLAR ORTHOGRAPHIC	        = 1
                   OBLIQUE ORTHOGRAPHI    	= 2
                   POLAR STEREOGRAPHI   	= 3
                   OBLIQUE STEREOGRAPHC	        = 4
                   LAMBERT		        = 5
                   MERCATOR		        = 6
                   IMAGE SPACE		        = 7
                   OBJECT SPACE		        = 8
                   NORMAL CYLINDRICAL	        = 9
                   SIMPLE CYLINDRICAL	        = 10
                   OBLIQUE SIMPLE CYLINDRICAL	= 11
                   SINUSOIDAL		        = 12
                   OBLIQUE SINUSOIDAL	        = 13
                   MOLLWEIDE		        = 14
		   TRANSVERSE MERCATOR	        = 15
                   PERSPECTIVE                  = 16 */


char   helpstring[255], task[255];
char   *value;
int    ihelp,status,lauf;
float  rbuf[4];
char   a;
char   map3[255],map4[255],map5[255],map6[255],map7[255];
double om[3][3],rs[3];
double *help = (double *)data;
int    instances[NHST],nhist;
char    tasks[NHST][33];
      
/* clear arrays */
for (lauf=0; lauf<SEARCV3_BUFLEN; lauf++) 
   {
   data[ lauf] = 0;
   idata[lauf] = 0;
   }
   
status = find_hist_key(unit, "MAP003", 1, task, &ihelp);
if (status != 1) return -1;
 status = zlget(unit, "HISTORY", "MAP003", map3, "HIST", task, NULL); 

data[7] = 1;
idata[38] = 0;

if(strstr(map3,"*** POLAR ORTHOG") != NULL) idata[38] =  1;
if(strstr(map3,"*** ORTHOGRAPHIC") != NULL) idata[38] =  2;
if(strstr(map3,"*** POLAR STEREO") != NULL) idata[38] =  3;
if(strstr(map3,"*** STEREOGRAPHI") != NULL) idata[38] =  4;
if(strstr(map3,"*** LAMBERT CONF") != NULL) idata[38] =  5;
if(strstr(map3,"***MERCATOR PROJ") != NULL) idata[38] =  6;
if(strstr(map3,"*** CYLINDRICAL")  != NULL) idata[38] =  9;
if(strstr(map3,"*** SIMPLE CYLIN") != NULL) idata[38] = 10;
if(strstr(map3,"*** OBLIQUE SIMP") != NULL) idata[38] = 11;
if(strstr(map3,"*** SINUSOIDAL P") != NULL) idata[38] = 12;
if(strstr(map3,"*** OBLIQUE SINU") != NULL) idata[38] = 13;
if(strstr(map3,"*** MOLLWEIDE PR") != NULL) idata[38] = 14;
if(strstr(map3,"***TRANS MERCATO") != NULL) idata[38] = 15;
if(strstr(map3,"*** PERSPECTIVE ") != NULL) idata[38] = 16;
if(strstr(map3,"*** OBJECT SPACE") != NULL) idata[38] = 16;
if (idata[38] == 0) return -2;


 status = zlget(unit, "HISTORY", "MAP002", helpstring, "HIST", task, NULL); 
if (status != 1) return -3;
value = strstr(helpstring,"RADII"); 
if (value != NULL)
   {
   strcpy(helpstring,value);
   sscanf(&helpstring[7],"%f%c%f%c%f",&rbuf[0],&a,&rbuf[1],&a,&rbuf[2]);
   }
else return -4;
data[24] = rbuf[2];                  /* polar radius */
data[25] = (rbuf[0] + rbuf[1]) /2.0; /* equatorial radius */

 status = zlget(unit, "HISTORY", "MAP004", map4, "HIST", task, NULL); 
if (status != 1) return -5;
if (idata[38] != 6)
   {
     status = zlget(unit, "HISTORY", "MAP005", map5, "HIST", task, NULL); 
   if (status != 1) return -6;
   }
   
switch(idata[38])
   {
   case 1: /* POLAR ORTHOGRAPHIC PROJECTION. #1 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      break;

   case 2: /* OBLIQUE ORTHOGRAPHIC PROJECTION. #2 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      status = getnorth(map5, data);
      if (status != 1) return status;
      break;

   case 3: /* POLAR STEREOGRAPHIC PROJECTION. #3 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      break;

   case 4: /* OBLIQUE STEREOGRAPHIC PROJECTION. #4 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      status = getnorth(map5, data);
      if (status != 1) return status;
      break;

   case 12: /* SINUSOIDAL PROJECTION. #12 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      data[7] = 1; /* is set to -1 in getllsls */
      break;

   case 13: /* OBLIQUE SINUSOIDAL PROJECTION. #13 */
     status = zlget(unit, "HISTORY", "MAP006", map6, "HIST", task, NULL); 
      if (status != 1) return -8;
      status = zlget(unit, "HISTORY", "MAP007", map7, "HIST", task, NULL); 
      if (status != 1) return -9;
      value = strstr(map4,"S="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[0]);
         }
      else return -7;
      value = strstr(map4,"L="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[1]);
         }
      else return -7;
      value = strstr(map5,"LAT="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[4],"%f",&data[2]);
         }
      else return -7;
      value = strstr(map5,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map6,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[3]);
         }
      else return -7;
      value = strstr(map7,"SCALE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[6]);
         }
      else return -7;
      break;

   case 14: /* MOLLWEIDE PROJECTION. #14 */
      status = getllsls(map4, map5, data);
      if (status != 1) return status;
      data[2] = 0;
      data[7] = 1; /* is set to -1 in getllsls */
      break;

   case 11: /* OBLIQUE SIMPLE CYLINDRICAL PROJECTION. #11 */
     status = zlget(unit, "HISTORY", "MAP006", map6, "HIST", task, NULL); 
      if (status != 1) return -8;
      status = zlget(unit, "HISTORY", "MAP007", map7, "HIST", task, NULL); 
      if (status != 1) return -9;
      value = strstr(map4,"S="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[0]);
         }
      else return -7;
      value = strstr(map4,"L="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[1]);
         }
      else return -7;
      value = strstr(map5,"LAT="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[4],"%f",&data[2]);
         }
      else return -7;
      value = strstr(map5,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map6,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[3]);
         }
      else return -7;
      value = strstr(map7,"SCALE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[6]);
         }
      else return -7;
      break;

   case 5: /* LAMBERT PROJECTION #5 */
      value = strstr(map3,"LONG "); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map4,"PARALLELS "); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[10],"%f",&data[3]);
         }
      else return -7;
      value = strstr(map4,"AND "); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[4],"%f",&data[4]);
         }
      else return -7;
      value = strstr(map5,"IS"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[6]);
         }
      else return -7;
      status = zlget(unit, "HISTORY", "MAP006", map6, "HIST", task, NULL); 
      if (status != 1) return -8;
      value = strstr(map6,"THE SOUTH POLE IS AT LINE"); 
      if (value != NULL)
         {
          data[7] = -1;
          data[2] = -90;
         }
      else 
         {
         value = strstr(map6,"THE NORTH POLE IS AT LINE"); 
         if (value != NULL)
            {
            data[7] = 1;
            data[2] = 90;
            }
         else return -7;
         }
      value = strstr(map6,"LINE"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[4],"%f",&data[1]);
         }
      else return -7;
      value = strstr(map6,"SAMPLE"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[0]);
         }
      else return -7;
      break;

   case 6: /* MERCATOR PROJECTION #6 */
      data[0]=1;
      data[1]=1;
      data[7]=1;
      value = strstr(map3,"LAT="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[4],"%f",&data[2]);
         }
      else return -7;
      value = strstr(map3,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map4,"EQUATOR ="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[9],"%f",&data[9]);
         }
      else return -7;
      value = strstr(map4,"DEG OR"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[6]);
         }
      else return -7;
      break;

   case 15: /* TRANSVERSE MERCATOR PROJECTION #15 */

     value = strstr(map4,"S="); 
     if (value != NULL)
        {
        strcpy(helpstring,value);
        sscanf(&helpstring[2],"%f",&data[0]);
        }
     else return -7;
     value = strstr(map4,"L="); 
     if (value != NULL)
        {
        strcpy(helpstring,value);
        sscanf(&helpstring[2],"%f",&data[1]);
        }
     else return -7;
     value = strstr(map4,"LAT="); 
     if (value != NULL)
        {
        strcpy(helpstring,value);
        sscanf(&helpstring[4],"%f",&data[2]);
        }
     else return -7;
     value = strstr(map4,"LONG="); 
     if (value != NULL)
        {
        strcpy(helpstring,value);
        sscanf(&helpstring[5],"%f",&data[5]);
        }
     else return -7;
      value = strstr(map5,"MERIDIAN ="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[10],"%f",&data[9]);
         }
      else return -7;
      value = strstr(map5,"OR"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[6]);
         }
      else return -7;
      break;

   case 16: /* OBJECT SPACE MAP3 LABEL  PERSPECTIVE PROJECTION #16 */
     status = zlget(unit, "HISTORY", "MAP006", map6, "HIST", task, NULL); 
      if (status != 1) return -8;
      status = zlget(unit, "HISTORY", "MAP007", map7, "HIST", task, NULL); 
      if (status != 1) return -8;
      value = strstr(map4,"S/C LAT="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[8],"%f",&data[30]);
         }
      else return -7;
      value = strstr(map4,"S/C LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[9],"%f",&data[31]);
         }
      else return -7;
      value = strstr(map4,"S/C LINE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[9],"%f",&data[32]);
         }
      else return -7;
      value = strstr(map5,"S/C SAMPLE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[11],"%f",&data[33]);
         }
      else return -7;
      value = strstr(map5,"S/C RANGE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[10],"%f",&data[37]);
         }
      else return -7;
      value = strstr(map6,"FOCAL="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[26]);
         }
      else return -7;
      value = strstr(map6,"MM="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[3],"%f",&data[29]);
         }
      else return -7;
      value = strstr(map6,"ANGLE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[34]);
         }
      else return -7;
      value = strstr(map7,"LINE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[27]);
         }
      else return -7;
      value = strstr(map7,"SAMPLE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[7],"%f",&data[28]);
         }
      else return -7;
      momati_c((double)data[27],(double)data[28],(double)data[32],
               (double)data[33],(double)data[29],(double)data[26],
               (double)data[31],(double)data[30],(double)data[34],
               (double)data[37],om,rs);
      help[0] = om[0][0];
      help[1] = om[0][1];
      help[2] = om[0][2];
      help[3] = om[1][0];
      help[4] = om[1][1];
      help[5] = om[1][2];
      help[6] = om[2][0];
      help[7] = om[2][1];
      help[8] = om[2][2];
      help[9] = rs[0];
      help[10]= rs[1];
      help[11]= rs[2];
      break;

   case 9: /* CYLINDRICAL PROJECTION #9 */
      data[7] = 1;
      value = strstr(map4,"AT S="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[0]);
         }
      else return -7;
      value = strstr(map4,"L="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[2],"%f",&data[1]);
         }
      else return -7;
      value = strstr(map4,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map5,"EQUATOR ="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[9],"%f",&data[9]);
         }
      else return -7;
      value = strstr(map5,"DEG OR"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[6]);
         }
      else return -7;
      break;
      
   case 10: /* SIMPLE CYLINDRICAL PROJECTION #10 */
      value = strstr(map4,"LINE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[1]);
         }
      else return -7;
      value = strstr(map4,"SAMPLE="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[7],"%f",&data[0]);
         }
      else return -7;
      value = strstr(map4,"LATI="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[2]);
         }
      else return -7;
      value = strstr(map4,"LATI="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[2]);
         }
      else return -7;
      value = strstr(map4,"LONG="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[5],"%f",&data[5]);
         }
      else return -7;
      value = strstr(map5,"EQUATOR ="); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[9],"%f",&data[9]);
         }
      else return -7;
      value = strstr(map5,"DEG OR"); 
      if (value != NULL)
         {
         strcpy(helpstring,value);
         sscanf(&helpstring[6],"%f",&data[6]);
         }
      else return -7;

    }  /* end of switch */

if (idata[38] == 0)
   {
   /* here if no map labels:  check for object space tasks */
     zlhinfo(unit,tasks[0],instances,&nhist,NULL);
   for (lauf=0; lauf < nhist; lauf++)
      if ( (!strcmp(tasks[lauf],"GEOM"))
        || (!strcmp(tasks[lauf],"FARENC")) )
           {
           idata[38] = 8; /* object space */ 
           return 1;
           } 
   idata[38] = 7;
   return 1; 
   }

data[5] = myamod(data[5], 360.0);
return 1;
}
