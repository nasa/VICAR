$!****************************************************************************
$!
$! Build proc for MIPL module searcv3
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:18
$!
$! Execute by entering:		$ @searcv3
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module searcv3 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to searcv3.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("searcv3.imake") .nes. ""
$   then
$      vimake searcv3
$      purge searcv3.bld
$   else
$      if F$SEARCH("searcv3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake searcv3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @searcv3.bld "STD"
$   else
$      @searcv3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create searcv3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack searcv3.com -mixed -
	-s searcv3_c.c -
	-i searcv3.imake -
	-t tsearcv3.c tsearcv3.imake tsearcv3.pdf tstsearcv3.pdf -
	-o searcv3.hlp searcv3.f
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create searcv3_c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create searcv3.imake
/* Imake file for VICAR subroutine SEARCV3 */

#define SUBROUTINE searcv3

#define MODULE_LIST searcv3_c.c

#define USES_ANSI_C

#define P1_SUBLIB

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$ Return
$!#############################################################################
$Test_File:
$ create tsearcv3.c
#include <stdio.h>
#include "vicmain_c"

#define VICARrtlSUCCESS	0
#define	ABENDif(x) 	if (x) zabend()

/*

TSEARCV3

Test program for SEARCV3

*/

void main44()
{
int	i,j,k;
int	status;

int	count;
int	instance;
int	unit;
char	string[200];

int	int_data[40];
float	real_data[40];

/*

Clear arrays

*/

memset(int_data,0,160);
memset(real_data,0,160);

/*

Get unit number of file and open for reading of projection label

*/


status = zvpcnt("INP",&count);
ABENDif( status<VICARrtlSUCCESS );

for( instance=1; instance<=count; instance++ )
	{
	status = zvunit( &unit,"INP",instance,0 );
	ABENDif( status<VICARrtlSUCCESS );

	status = zvopen( unit,0 );
	ABENDif( status<VICARrtlSUCCESS );

	/*

	Call SEARCV3 to read IBM-style projection label

	*/

	status=searcv3_c( unit,real_data,int_data);
	ABENDif( status<VICARrtlSUCCESS );

	if( instance==2 )
		{
		zvmessage(" "," ");
		zvmessage(" *** 2nd Input *** "," ");
		zvmessage(" "," ");
		}

	switch( int_data[38] ) 	{

	case 1:	

	sprintf(string," PROJECTION TYPE: POLAR ORTHOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 2:

	sprintf(string," PROJECTION TYPE: OBLIQUE ORTHOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 3:	

	sprintf(string," PROJECTION TYPE: POLAR STEREOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 4:	

	sprintf(string," PROJECTION TYPE: OBLIQUE STEREOGRAPHIC (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 5:	

	sprintf(string," PROJECTION TYPE: LAMBERT (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 6:	

	sprintf(string," PROJECTION TYPE: MERCATOR (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 7:	

	sprintf(string," PROJECTION TYPE: IMAGE SPACE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	zvmessage(" *** Unsupported by this test program"," ");
	break;

	case 8:	

	sprintf(string," PROJECTION TYPE: OBJECT SPACE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	zvmessage(" *** Unsupported by this test program"," ");
	break;			

	case 9:	

	sprintf(string," PROJECTION TYPE: NORMAL CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 10:	

	sprintf(string," PROJECTION TYPE: SIMPLE CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 11:	

	sprintf(string,
		" PROJECTION TYPE: OBLIQUE SIMPLE CYLINDRICAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 12:	

	sprintf(string," PROJECTION TYPE: SINUSOIDAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 13:	

	sprintf(string," PROJECTION TYPE: OBLIQUE SINUSOIDAL (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 14:	

	sprintf(string," PROJECTION TYPE: MOLLWEIDE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 15:	

	sprintf(string," PROJECTION TYPE: TRANSVERSE MERCATOR (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;

	case 16:	

	sprintf(string," PROJECTION TYPE: PERSPECTIVE (element 39:%d)",
		int_data[38]);
	zvmessage(string," ");
	break;		
			}

	switch( int_data[38] ) 	{

	case 7:
	case 8:

	break;
	
	case 16:	

	zvmessage(" "," ");
	sprintf(string,"OM matrix\t| %f\t%f\t%f |",
		real_data[0],real_data[2],real_data[4]);
	zvmessage(string," ");
	sprintf(string,"\t\t| %f\t%f\t%f |",
		real_data[6],real_data[8],real_data[10]);
	zvmessage(string," ");
	sprintf(string,"\t\t| %f\t%f\t%f |",
		real_data[12],real_data[14],real_data[16]);
	zvmessage(string," ");
	zvmessage(" "," ");

	sprintf(string,"RS vector = { %f %f %f }",
		real_data[18],real_data[20],real_data[22]);
	zvmessage(string," ");
	zvmessage(" "," ");

        sprintf(string," FOCAL LENGTH (MM)           (27) = %f",real_data[26]);
	zvmessage(string," ");
	sprintf(string," OPTICAL AXIS LINE           (28) = %f",real_data[27]);
	zvmessage(string," ");
	sprintf(string," OPTICAL AXIS SAMPLE         (29) = %f",real_data[28]);
	zvmessage(string," ");
	sprintf(string," OBJECT SPACE SCALE          (30) = %f",real_data[29]);
	zvmessage(string," ");
	sprintf(string," SUB SPACECRAFT LATITUDE     (31) = %f",real_data[30]);
	zvmessage(string," ");
	sprintf(string," SUB SPACECRAFT LATITUDE     (32) = %f",real_data[31]);
	zvmessage(string," ");
	sprintf(string," SUB S/C OBJECT SPACE LINE   (33) = %f",real_data[32]);
	zvmessage(string," ");
	sprintf(string," SUB S/C OBJECT SPACE SAMPLE (34) = %f",real_data[33]);
	zvmessage(string," ");
	sprintf(string," NORTH ANGLE                 (35) = %f",real_data[34]);
	zvmessage(string," ");
	sprintf(string," PLANET ID                   (36) = %f",real_data[35]);
	zvmessage(string," ");
	sprintf(string," VIDICON SERIAL NUMBER       (37) = %f",real_data[36]);
	zvmessage(string," ");
	sprintf(string," DISTANCE TO PLANET          (38) = %f",real_data[37]);
	zvmessage(string," ");
	break;

	default:

	zvmessage(" "," ");
	for( i=0;i<9;i++ )
		{
		j = i + 1;
		sprintf(string," DATA                (%d) = %f",
			j,real_data[i]);
		zvmessage(string," ");
		}

	zvmessage(" "," ");
	sprintf(string," SAMPLE              (1) = %f",real_data[0]);
	zvmessage(string," ");
	sprintf(string," LINE                (2) = %f",real_data[1]);
	zvmessage(string," ");
	sprintf(string," LATITUDE            (3) = %f",real_data[2]);
	zvmessage(string," ");
	sprintf(string," LONGITUDE           (6) = %f",real_data[5]);
	zvmessage(string," ");
	sprintf(string," SCALE (KM/PIXEL)    (7) = %f",real_data[6]);
	zvmessage(string," ");
	sprintf(string," CAS                 (8) = %f",real_data[7]);
	zvmessage(string," ");
	sprintf(string," NORTH ANGLE         (9) = %f",real_data[8]);
	zvmessage(string," ");

	break;
			}

	sprintf(string," POLAR RADIUS       (25) = %f",real_data[24]);
	zvmessage(string," ");
	sprintf(string," EQUATORIAL RADIUS  (26) = %f",real_data[25]);
	zvmessage(string," ");

	switch ( int_data[38] )	{

		case 5:

		sprintf(string," LATITUDE OF 1 SPECIAL PARALLEL (4) = %f",
			real_data[3]);
		zvmessage(string," ");
		sprintf(string," LATITUDE OF 2 SPECIAL PARALLEL (5) = %f",
			real_data[4]);
		zvmessage(string," ");
		break;

		case 11:
		case 13:

		sprintf(string," OBLIQUE LONGITUDE OF ROTATION (4) = %f",
			real_data[3]);
		zvmessage(string," ");
		break;

	
		case 6:
		case 9:
		case 10:
		case 15:

		sprintf(string," MAP RESOLUTION (PXL/DEG) (10) = %f",
			real_data[9]);
		zvmessage(string," ");
		break;	
	
		default:

		break;
				}
	}
}
$!-----------------------------------------------------------------------------
$ create tsearcv3.imake
/* Imake file for VICAR program TSEARCV3 */

#define PROGRAM tsearcv3

#define MODULE_LIST tsearcv3.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$!-----------------------------------------------------------------------------
$ create tsearcv3.pdf
PROCESS
PARM INP            TYPE=STRING     COUNT=1:2    
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsearcv3.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
write " "
write "********************************************************"
write " "
write "Test Procedure for MIPS VICAR routine SEARCV3"
write " "
write "********************************************************"
write " "
write " NOTE TO TESTERS: MAP3 is not ported as of April 1994"
write "                  therefore data must be generated on"
write "                  the VAX VMS system, then transferred"
write "                  to the UNIX or Alpha systems."
write " "
write "                  The following image files must be in"
write "                  your working directory:"
write " "
write " 			F1636832.CYLINDRICAL"
write " 			F1636832.LAMBERT"
write " 			F1636832.MERCATOR"
write " 			F1636832.MOLLWEIDE"
write " 			F1636832.OBLIQUE_ORTHO"
write " 			F1636832.OBLIQUE_RECT"
write " 			F1636832.OBLIQUE_SINU"
write " 			F1636832.OBLIQUE_STEREO"
write " 			F1636832.ORTHOGRAPHIC2"
write " 			F1636832.PERSPECTIVE"
write " 			F1636832.PERSPECTIVE2"
write " 			F1636832.POLAR_ORTHO"
write " 			F1636832.POLAR_STEREO"
write "				F1636832.RECTANGULAR"
write "				F1636832.SINUSOIDAL"
write "				F1636832.TRANSVERSE_MERC"
write "				F1636832.ZOOMED"
write " "
write " 		These files can be found on SITOD20B"
write " 		in the TEST_DATA.GLL directory."
write " "
write "********************************************************"
let $echo="yes"

let $echo="no"
write " "
write " Test of polar orthographics projection"
write " "
let $echo="yes"

label-list f1636832.polar_ortho
tsearcv3 f1636832.polar_ortho

let $echo="no"
write " "
write " Test of polar stereographic projection"
write " "
let $echo="yes"

label-list f1636832.polar_stereo
tsearcv3 f1636832.polar_stereo

let $echo="no"
write " "
write " Test of oblique orthographic projection"
write " "
let $echo="yes"

label-list f1636832.oblique_ortho
tsearcv3 f1636832.oblique_ortho

let $echo="no"
write " "
write " Test of oblique stereographic projection"
write " "
let $echo="yes"

label-list f1636832.oblique_stereo
tsearcv3 f1636832.oblique_stereo

let $echo="no"
write " "
write " Test of lambert projection"
write " "
let $echo="yes"

label-list f1636832.lambert
tsearcv3 f1636832.lambert

let $echo="no"
write " "
write " Test of mercator projection"
write " "
let $echo="yes"

label-list f1636832.mercator
tsearcv3 f1636832.mercator

let $echo="no"
write " "
write " Test of cylindrical projection"
write " "
let $echo="yes"
 
label-list f1636832.cylindrical
tsearcv3 f1636832.cylindrical

let $echo="no"
write " "
write " Test of simple cylindrical (rectangular) projection"
write " "
let $echo="yes"

label-list f1636832.rectangular
tsearcv3 f1636832.rectangular

let $echo="no"
write " "
write " Test of oblique simple cylindrical projection"
write " "
let $echo="yes"

label-list f1636832.oblique_rect
tsearcv3 f1636832.oblique_rect

let $echo="no"
write " "
write " First test of perspective projection"
write " "
let $echo="yes"

label-list f1636832.perspective
tsearcv3 f1636832.perspective

let $echo="no"
write " "
write " Test of sinusoidal projection"
write " "
let $echo="yes"

label-list f1636832.sinusoidal
tsearcv3 f1636832.sinusoidal

let $echo="no"
write " "
write " Test of oblique sinusoidal projection"
write " "
let $echo="yes"

label-list f1636832.oblique_sinu
tsearcv3 f1636832.oblique_sinu

let $echo="no"
write " "
write " Test of mollweide projection"
write " "
let $echo="yes"

label-list f1636832.mollweide
tsearcv3 f1636832.mollweide

let $echo="no"
write " "
write " Test of transverse mercator projection"
write " "
let $echo="yes"

label-list f1636832.transverse_merc
tsearcv3 f1636832.transverse_merc

let $echo="no"
write " "
write " Second test of the perspective projection"
write " "
let $echo="yes"

label-list f1636832.perspective2
tsearcv3 f1636832.perspective2

let $echo="no"
write " "
write " Test that SEARCV3 zeroes its buffer correctly:"
write " (if not, then the second run of tsearcv3 will give incorrect results)"
write " "
write " Also, once SIZE function is redelivered with its capability"
write " to update the MAP buffer, then it should be included in the"
write " test script to create f1636832.zoomed (zoom=-2)."
write " "
let $echo="yes"

tsearcv3 (f1636832.orthographic2,f1636832.zoomed)
tsearcv3 (f1636832.zoomed,f1636832.orthographic2)

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create searcv3.hlp
1 MIPS VICAR routine SEARCV3

  ***

  THIS ROUTINE SHOULD NEVER BE CALLED DIRECTLY BY AN APPLICATION PROGRAM.  

  ***

  It is used by the map label readers to process IBM (old-style)
  map projection labels.

2 Calling Sequence

  CALL SEARCV3( UNIT, DATA, DATA, STATUS)

  Arguments: UNIT   - input I*4 - unit of file to be searched
             DATA   - output *4 (40) - MAP data buffer
             STATUS - output I*4 - status code
2 History

  Original Programmer: 		L.W.Kamp
  Current Cognizant Programmer: L.W.Kamp
  Date written:			18 September 1993 (based on SEARCV2)
  Source Language: 		FORTRAN
  Revisions:			

  16feb94 -lwk- fixed bug getting SCALE in simple cylindrical

  December 1994	JFM: ERR_ACT option of blanks (ZVEACTION override)
  added to VICARL RTL routines used in SEARVC3.  This was done make the
  MP_ROUTINES software immune to application programs using ZVEACTION.
  (FR 85665)

  March 1995 JFM:  Added retrieval of map resolution in pixels per degree,
  array element 10.  This is retrieved for the following projections:
  Mercator (#6), Transverse Mercator (#15), Cylindrical Equal-Area (Normal)
  (#9), and Simple Cylindrical (#10).

  04apr95 -lwk- fixed "Instances" in xlhinfo/xlget calls

  26 April 1995 FFM:  Initialize nc to 0 if status = -38 from XLGET;
  Initialize bufsize to 0.  The initializations are needed for ANDES 
  platform (FR 85609).

2 Operation

	SEARCV3 will search a picture label and return a buffer
	containing MAP3 projection parameters or whether the 
	picture is raw or geometrically corrected.  The returned
	buffer is compatible with conventions for the MAP and
	CONVEV subroutines.

	Only DATA(39) will be modified if the image is not a
	map-projection, ie: has no map2 labels.

	The file must be opened prior to calling SEARCV3.

	The following label words imply an object space
	(geometrically corrected) image:  GEOMA, MGEOM, GEOM
	GEOMAR, FARENC,FARENCX, FARENCY, FARENCZ.
	
	If the label contains the radii of the planet, that will
	be stored in the data buffer as words 25 and 26.

2 Arguments

  UNIT - INTEGER*4 -     This is the unit of the file whose label is to 
                         be searched for MAP3 labels
  DUM  - not used
  WORK   not used

  DATA -INTEGER*4(40) -  This is the standard MAP data buffer.  
                         Values derived from the label are returned here.
                         It is input twice because some items are real 
                         and some are integer.  Note that map resolution
			 (element 10) is only available for the following
			 projections: MERCATOR, TRANSVERSE MERCATOR, 
			 CYLINDRICAL EQUAL AREA, and SIMPLE CYLINDRICAL.

  Normally the following is returned in DATA:

	DATA(1) = SPECIAL SAMPLE POINT             R*4
	DATA(2) = SPECIAL LINE POINT               R*4
	DATA(3) = SPECIAL LATITUDE POINT           R*4
	DATA(4) = LATITUDE OF SPEC PARALLEL (DEG)  R*4
	DATA(5) = LATITUDE OF SPEC PARALLEL  (DEG) R*4
	DATA(6) = SPECIAL LONGITUDE (WEST) (DEG)   R*4
	DATA(7) = SCALE (KILOMETERS/PIXEL)         R*4
	DATA(8) = VISIBLE POLE  1=N -1=S           R*4
	DATA(9) = NORTH ANGLE                      R*4
	DATA(10)= RESOLUTION (PIXELS/DEGREE)	   R*4
	DATA(25)= POLAR RADIUS (KM)                R*4
	DATA(26)= EQUATORIAL RADIUS (KM)           R*4
	DATA(39)= PROJECTION TYPE                  I*4
			1=POLAR ORTHOGRAPHIC
			2=OBLIQUE    "
			3=POLAR STEREOGRAPHIC
			4=OBLIQUE    "
			5=LAMBERT
			6=MERCATOR
			7=RAW UNCORRECTED IMAGE
			8=GEOMETRICALLY CORRECTED IMAGE
			9=NORMAL CYLINDRICAL
		       10=SIMPLE     "
                       11=OBLIQUE SIMPLE CYLINDRICAL
                       12=SINUSOIDAL
                       13=OBLIQUE SINUSOIDAL
                       14=MOLLWEIDE
                       15=TRANSVERSE MERCATOR
                       16=PERSPECTIVE

  For the case when the projection type is #16 then DATA contains:

       DATA(1-18)=OM matrix*             Real*8
       DATA(19-24)=RS vector*            Real*8
       DATA(31)=s/c latitude             Real*4
       DATA(32)=s/c longitude            Real*4
       DATA(33)=line                     Real*4
       DATA(34)=sample                   Real*4
       DATA(38)=Range to target body     Real*4
       DATA(27)=focallength              Real*4
       DATA(30)=scale in pixels/mm.      Real*4
       DATA(35)=North angle              Real*4
       DATA(28)=optical axis line        Real*4
       DATA(29)=optical axis sample      Real*4
       DATA(39)=16

  * NOTE: PREVIOUS IMPLEMENTATION OF "DATA" ARRAY FOR PASSING
	  DOUBLE PRECISION VALUES (REAL*8) IN ELEMENTS OF A REAL
	  (REAL*4) ARRAY IS NOT PORTABLE.  ONLY ON THE VAX/VMS
	  SYSTEM (unported system) WILL THE VALUES OF THE OM MATRIX 
 	  AND RS VECTOR BE CORRECT.

  For the case when the projection type is #7 then DATA contains:

       DATA(39)=7    only (image space). You must call the SPICE/SEDR yourself

  For the case when the projection type is #8 then DATA contains:

       DATA(39)=8    only (object space). You must call the SPICE/SEDR yourself

$!-----------------------------------------------------------------------------
$ create searcv3.f
c
c MIPS VICAR routine SEARCV3
c
c  This routine should never be called directly by an application
c  program.  It is used by the map label readers to process IBM (old-style)
c  map projection labels.
c
c Calling Sequence
c
c  CALL SEARCV3( UNIT, DATA, DATA, STATUS)
c
c  Arguments: UNIT   - input I*4 - unit of file to be searched
c             DATA   - output *4 (40) - MAP data buffer
c             STATUS - output I*4 - status code
c History
c
c  Original Programmer: 		L.W.Kamp
c  Current Cognizant Programmer:	L.W.Kamp
c  Date written:			18 September 1993 (based on SEARCV2)
c  Source Language: 			FORTRAN
c
c  Revisions:				
c
c  19sep93 -lwk- this is the old SEARCV2 code with a simplified parameter
c		 list, to be called by MP_LABEL_READ and the new SEARCV2
c  01dec93 -lwk- replaced XLGETLABEL by multiple XLGETs
c  11jan94 -lwk- fixed bugs in Mercator case and for cases where map labels
c		 are not in the last task of the label
c  16feb94 -lwk- fixed bug getting SCALE in simple cylindrical
c  25apr94 -jfm- revised test pdf to be useful on all platforms
c  13mar95 -jfm- added retrieval of PXLS/DEG to functionality
c		 for projections: MERCATOR, TRANSVERSE MERCATOR, 
c		 CYLINDRICAL EQUAL AREA, and SIMPLE CYLINDRICAL.
c  04apr95 -lwk- fixed "Instances" in xlhinfo/xlget calls
c  26apr95 -ffm- initialize nc to 0 if CANNOT_FIND_KEY(-38)
c		 initialize bufsize to 0 

c
      SUBROUTINE SEARCV3( UNIT, DATA, IDATA, ISTAT)
c
c  NOTE:  in the original code, when multiple tasks had performed map
c  projections in the history, the Radii are taken from the *first* one
c  encountered (even though the current map projection is taken from the
c  last)!  This is considered to be a bug (although it will be very rare
c  to change the radius of the target body when reprojecting) and has
c  been revised in this version.

C   LABEL SEARCH FOR MAP3 PARAMETERS:
C     DATA(1)      XC        SPECIAL SAMPLE POINT
C     DATA(2)      ZC        SPECIAL LINE POINT
C     DATA(3)      THETA0    SPECIAL LATITUDE POINT DEG.
C     DATA(4)      THETA1    LATITUDE OF SPECIAL PARALLEL DEG.
C     DATA(5)      THETA2    LATITUDE OF SPECIAL PARALLEL DEG.
C     DATA(6)      LAMDA0    SPECIAL LONGITUDE DEG.
C     DATA(7)      F         SCALE KM/PIXEL
C     DATA(8)      CAS       +1 IF VISIBLE POLE IS N.  -1 IF SOUTH
C     DATA(9)      PSI       NORTH ANGLE DEG.
C     DATA(10)     RES	     MAP RESOLUTION IN PXLS/DEG
C     DATA(25)               polar radius
C     DATA(26)               equatorial radius
C     DATA(39)     PROJECTION TYPE OF PICTURE I*4
C                  POLAR ORTHOGRAPHIC	        = 1
C                  OBLIQUE ORTHOGRAPHIC   	= 2
C                  POLAR STEREOGRAPHIC  	= 3
C                  OBLIQUE STEREOGRAPHC	        = 4
C                  LAMBERT		        = 5
C                  MERCATOR		        = 6
C                  IMAGE SPACE		        = 7
C                  OBJECT SPACE		        = 8
C                  NORMAL CYLINDRICAL	        = 9
C                  SIMPLE CYLINDRICAL	        = 10
C                  OBLIQUE SIMPLE CYLINDRICAL	= 11
C                  SINUSOIDAL		        = 12
C                  OBLIQUE SINUSOIDAL	        = 13
C                  MOLLWEIDE		        = 14
C		   TRANSVERSE MERCATOR	        = 15
C                  PERSPECTIVE                  = 16
C
      parameter (maxtasks=100)

      CHARACTER*16 MERCAT,LAMBER
      CHARACTER*16 ORTHOG,STEREO
      CHARACTER*16 CYLIND,PSTER
      CHARACTER*16 PORTH,OBCYL
      CHARACTER*16 LATL
      CHARACTER*16 MOLL
      CHARACTER*16 SINU
      CHARACTER*16 OBSINU
      CHARACTER*16 TVERSMER
      CHARACTER*16 PERSPECTIVE
      CHARACTER*16 OBJECTSP
      CHARACTER*8 GEOMA,MGEOM,GEOM
      CHARACTER*8 GEOMAR,FARENC
      CHARACTER*8 FARENX
      CHARACTER*8 FARENY,FARENZ
      CHARACTER*8 NORTH,SOUTH
      CHARACTER*10 MAP2

      REAL*4 DATA(40),RBUF(4)

      INTEGER IDATA(40),UNIT
      INTEGER BUFSIZE
c
c  max buffer needed is 7 MAPxxx labels of 70 bytes each, plus 100 bytes pad:
c
      parameter (maxbuf=600)
      CHARACTER*600 WORK
      character*6 mlab
      character*32 tasks(maxtasks)
      integer inst(maxtasks)

      LOGICAL maptask,more

C was previously in type declaration
C changed by Roatsch
      MERCAT = '***MERCATOR PROJ'
      LAMBER = '*** LAMBERT CONF'
      ORTHOG = '*** ORTHOGRAPHIC'
      STEREO = '*** STEREOGRAPHI'
      CYLIND = '*** CYLINDRICAL'
      PSTER  = '*** POLAR STEREO'
      PORTH  = '*** POLAR ORTHOG'
      OBCYL  = '*** OBLIQUE SIMP'
      LATL   = '*** SIMPLE CYLIN'
      MOLL   = '*** MOLLWEIDE PR'
      SINU   = '*** SINUSOIDAL P'
      OBSINU = '*** OBLIQUE SINU'
      TVERSMER = '***TRANS MERCATO'
      PERSPECTIVE = '*** PERSPECTIVE '
      OBJECTSP = '*** OBJECT SPACE'
      GEOMA   = 'GEOMA   '
      MGEOM   = 'MGEOM   '
      GEOM    = 'GEOM    '
      GEOMAR  = 'GEOMAR  '
      FARENC  = 'FARENC  '
      FARENX  = 'FARENCX '
      FARENY  = 'FARENCY '
      FARENZ  = 'FARENCZ '
      NORTH   = 'NORTH   '
      SOUTH   = 'SOUTH   '
      MAP2    = 'MAP2 LABEL'
      
c
c  determine how many tasks are present and search them for MAP labels:
c
      ntasks = maxtasks		! on input, set to max. value
      call xlhinfo( unit, tasks, inst, ntasks, istat, 
     & 			'ERR_ACT', ' ', ' ')
c     replace call to chkstat, roatsch march 2000
c      call chkstat( istat,' ** too many tasks, SEARCV3 ***',1)
      if (status.ne.1) then
         call xvmessage(' ** too many tasks, SEARCV3 ***',' ')
         call abend
      endif
      
c     continues original code

      mlab = 'MAP001'
      imap = 1		! index of MAPxxx, which is never reset
      bufsize = 0
      do i=1,ntasks
	j = 1		! index in WORK buffer, reset each new task
	more = .true.
	maptask = .false.
	do while (more)
	  call xlget( unit, 'HISTORY', mlab, work(j:), istat, 'LENGTH',
     &     nc, 'FORMAT', 'STRING', 'HIST', tasks(i), 'INSTANCE', 
     &     inst(i), 'ERR_ACT', ' ', ' ')
	  if (istat.eq.-38) nc=0
	  if (j+nc-1.gt.maxbuf-100) then
	    call xvmessage('*** SEARCV3: label buffer overflow ***',' ')
	    j = maxbuf-99-nc
	  endif
	  if (istat.eq.1) then
	    maptask = .true.
	    j = j+nc
	    if (j.gt.maxbuf-150) more = .false.
	    imap = imap+1
	    write(mlab(4:),'(i3.3)') imap	! force leading zeroes
	  else
	    more = .false.
	  endif
	enddo
	if (maptask) then
	  if (j.le.maxbuf) work(j:) = ' '	! clear end of buffer
	  bufsize = j-1
	endif
      enddo
      if (bufsize.le.0) then
c	call xvmessage(' MAP3 LABELS NOT FOUND',' ')
	go to 950	! leave ISTAT as XLGET set it
      endif

      istat = 1
      DATA(8)=1.

      if (INDEX(WORK,PORTH).gt.0) then
	idata(39) = 1
      else if(INDEX(WORK,ORTHOG).gt.0) then
	idata(39) = 2
      else if(INDEX(WORK,PSTER).gt.0) then
	idata(39) = 3
      else if(INDEX(WORK,STEREO).gt.0) then
	idata(39) = 4
      else if(INDEX(WORK,LAMBER).gt.0) then
	idata(39) = 5
      else if(INDEX(WORK,MERCAT).gt.0) then
	idata(39) = 6
      else if(INDEX(WORK,CYLIND).gt.0) then
	idata(39) = 9
      else if(INDEX(WORK,LATL).gt.0) then
	idata(39) = 10
      else if(INDEX(WORK,OBCYL).gt.0) then
	idata(39) = 11
      else if(INDEX(WORK,SINU).gt.0) then
	idata(39) = 12
      else if(INDEX(WORK,OBSINU).gt.0) then
	idata(39) = 13
      else if(INDEX(WORK,MOLL).gt.0) then
	idata(39) = 14
      else if(INDEX(WORK,TVERSMER).gt.0) then
	idata(39) = 15
      else if(INDEX(WORK,PERSPECTIVE).gt.0 .or.
     &        INDEX(WORK,OBJECTSP).gt.0) then
	idata(39) = 16
      else			! could not locate a projection in label
	istat = -1
	go to 9500
      ENDIF
c
c  get radii
c
      KK=INDEX(WORK(:BUFSIZE),'RADII=(')
      read(work(kk+7:),'(3(f8.1,1x))') (rbuf(i),i=1,3)
      data(25) = rbuf(3)                  ! polar radius
      data(26) = (rbuf(1)+rbuf(1))/2.            ! equatorial radius

 6190 CONTINUE      
      L=IDATA(39)
      GO TO(6210,6220,6230,6240,7000,8000,9500,9500,9000,9100,
     1      6280,6250,6260,6270,8100,8500),L
      go to 9500	! (should be a redundant check)

 6210 CONTINUE
C
C     POLAR ORTHOGRAPHIC PROJECTION. #1
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6220 CONTINUE
C
C     OBLIQUE ORTHOGRAPHIC PROJECTION. #2
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3,16x,f7.3)') (rbuf(i),i=1,2)
      DATA(7)=RBUF(1)		! SCALE
      DATA(9)=RBUF(2)		! NORTH ANGLE
      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6230 CONTINUE
C
C     POLAR STEREOGRAPHIC PROJECTION. #3
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6240 CONTINUE
C
C     OBLIQUE STEREOGRAPHIC PROJECTION. #4
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3,16x,f7.3)') (rbuf(i),i=1,2)
      DATA(7)=RBUF(1)		! SCALE
      DATA(9)=RBUF(2)		! NORTH ANGLE

      IF(DATA(3).LT.0.)DATA(8)=-1.
      GO TO 10000 

 6250 CONTINUE
C
C     SINUSOIDAL PROJECTION. #12
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      GO TO 10000 

 6260 CONTINUE
C
C     OBLIQUE SINUSOIDAL PROJECTION. #13
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)')
     & (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      II=INDEX(work(:BUFSIZE),'OBLIQUE POLE AT LAT')
      read(work(ii+20:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! LATITUDE
      DATA(6)=RBUF(2)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'OBLIQUE LONG')
      read(work(ii+13:),'(f7.3)') rbuf(1)
      DATA(4)=RBUF(1)		! ROTATION

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      GO TO 10000 

 6270 CONTINUE
C
C     MOLLWEIDE PROJECTION. #14
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,18x,f7.3)') (rbuf(i),i=1,3)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(6)=RBUF(3)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE
      GO TO 10000 

 6280 CONTINUE
C
C     OBLIQUE SIMPLE CYLINDRICAL PROJECTION. #11
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1)') (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      II=INDEX(work(:BUFSIZE),'OBLIQUE POLE AT LAT')
      read(work(ii+20:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! LATITUDE
      DATA(6)=RBUF(2)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'OBLIQUE LONG')
      read(work(ii+13:),'(f7.3)') rbuf(1)
      DATA(4)=RBUF(1)		! ROTATION

      II=INDEX(work(:BUFSIZE),'SCALE')
      read(work(ii+6:),'(f8.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      GO TO 10000 

 7000 CONTINUE
C
C     LAMBERT PROJECTION #5
C
      II=INDEX(work(:BUFSIZE),'*** C.M. AT LONG')
      read(work(ii+17:),'(f7.3)') rbuf(1)
      DATA(6)=RBUF(1)		! LONG

      II=INDEX(work(:BUFSIZE),'STANDARD PARALLELS')
      read(work(ii+19:),'(f7.3,6x,f7.3)') (rbuf(i),i=1,2)
      DATA(4)=RBUF(1)		! PAR1
      DATA(5)=RBUF(2)		! PAR2

      II=INDEX(work(:BUFSIZE),'THE SCALE AT STD PARALL')
      read(work(ii+31:),'(f7.3)') rbuf(1)
      DATA(7)=RBUF(1)		! SCALE

      II=INDEX(work(:BUFSIZE),'THE SOUTH POLE IS AT LINE ')
      IJ=INDEX(work(:BUFSIZE),'THE NORTH POLE IS AT LINE ')
      IK=MAX0(II,IJ)
      IF(IK.EQ.II)THEN  !SOUTH
         DATA(8)=-1.
         DATA(3)=-90.
      ELSE              !NORTH
         DATA(8)=1.
         DATA(3)=90.
      ENDIF
      ii = ik
      read(work(ii+26:),'(f7.1,11x,f7.1)') (rbuf(i),i=1,2)
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE

      GO TO 10000 

 8000 CONTINUE
C
C     MERCATOR PROJECTION #6
C
      DATA(1)=1.
      DATA(2)=1.
      DATA(8)=1.
      ii = index(work(:bufsize),mercat)
      read(work(ii+42:),'(f7.3,9x,f7.3)') (rbuf(i),i=1,2)
      DATA(3)=RBUF(1)		! latitude
      DATA(6)=RBUF(2)		! longitude

      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR ')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 8100 CONTINUE
C
C     TRANSVERSE MERCATOR PROJECTION #15
C
      II=INDEX(work(:BUFSIZE),'AT PROJ. CENTER L')
      read(work(ii+18:),'(f8.1,3x,f8.1,5x,f7.3,6x,f7.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATITUDE
      DATA(6)=RBUF(4)		! LONGITUDE

      II=INDEX(work(:BUFSIZE),'SCALE AT MERIDIAN ')
      read(work(ii+19:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 8500 CONTINUE
C
C     OBJECT SPACE MAP3 LABEL  PERSPECTIVE PROJECTION #16
C
      II=INDEX(work(:BUFSIZE),'S/C LAT')
      read(work(ii+8:),'(f8.4,10x,f8.4,10x,f8.4)') (rbuf(i),i=1,3)
      DATA(31)=RBUF(1)		! S/C LAT
      DATA(32)=RBUF(2)		! S/C LONG
      DATA(33)=RBUF(3)		! LINE

      II=INDEX(work(:BUFSIZE),'S/C SAMPLE')
      read(work(ii+11:),'(f8.2,11x,1pe15.9)') (rbuf(i),i=1,2)
      DATA(34)=RBUF(1)		! SAMPLE
      DATA(38)=RBUF(2)		! RANGE

      II=INDEX(work(:BUFSIZE),'FOCAL')
      read(work(ii+6:),'(f9.3,10x,f8.4,13x,f7.3)') (rbuf(i),i=1,3)
      DATA(27)=RBUF(1)		! FOCAL
      DATA(30)=RBUF(2)		! PIXEL/MM
      DATA(35)=RBUF(3)		! NORTH ANGLE

      II=INDEX(work(:BUFSIZE),'OPTICAL AXIS LINE')
      read(work(ii+18:),'(f8.3,8x,f8.3)') (rbuf(i),i=1,2)
      DATA(28)=RBUF(1)		! optical axis line
      DATA(29)=RBUF(2)		! optical axis sample

c
c     compute OM matrix & RS vector
c
c     Note: The use of elements 1 through 18 of the real*4 array
c	    to store 9 real*8 values of the OM matrix and 3 real*8
c	    RS vector values is not portable.
c 
      call momati(dble(data(28)),dble(data(29)),dble(data(33)),
     +            dble(data(34)),dble(data(30)),dble(data(27)),
     +            dble(data(32)),dble(data(31)),dble(data(35)),
     +            dble(data(38)),data(1),data(19))
      return

 9000 CONTINUE
C
C     CYLINDRICAL PROJECTION #9
C
      DATA(3)=0.
      DATA(8)=1.
      II=INDEX(work(:BUFSIZE),'AT S')
      read(work(ii+5:),'(f8.1,3x,f8.1,30x,f9.3)') (rbuf(i),i=1,3)
      DATA(1)=RBUF(1)		! SAMPLE
      DATA(2)=RBUF(2)		! LINE
      DATA(6)=RBUF(3)		! LONG
   
      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

 9100 CONTINUE
C
C     SIMPLE CYLINDRICAL PROJECTION #10
C
      II=INDEX(work(:BUFSIZE),'AT LINE')
      read(work(ii+8:),'(f9.2,9x,f9.2,7x,f8.3,7x,f8.3)') rbuf
      DATA(2)=RBUF(1)		! LINE
      DATA(1)=RBUF(2)		! SAMPLE
      DATA(3)=RBUF(3)		! LATI
      DATA(6)=RBUF(4)		! LONG

      II=INDEX(work(:BUFSIZE),'SCALE AT EQUATOR')
      read(work(ii+18:),'(f11.3,13x,f7.3)') (rbuf(i),i=1,2)
      DATA(10)=RBUF(1)		! map resolution
      DATA(7)=RBUF(2)		! map scale

      GO TO 10000 

c
c  here if no map labels:  check for object space tasks:
c
 9500 do i=1,ntasks
	if (index(tasks(i),'GEOM').gt.0 .or. tasks(i).eq.'FARENC') then
	  idata(39)=8   ! object space
          return
	endif
      enddo
      idata(39)=7	! image space
      return

10000 CONTINUE
      DATA(6)=AMOD(DATA(6),360.)
      RETURN
      END
$ Return
$!#############################################################################
