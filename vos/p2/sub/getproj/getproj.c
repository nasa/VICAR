/**********************************************************************

    vicar2 subroutine GETPROJ

    Returns the project id from an image with unit number.

    unit   - unit number of file containing picture (input)  integer*4
    project- spacecraft identity                    (output) character*5
             valid are: GLL VGR-1 VGR-2 VIKOR MAR10 MAR-9
                        WFPC1 (original)
                        WFPC2 (after first optics upgrade)
	                CASSI    (Cassini)
			MPF   (Mars Pathfinder)
    camera - camera serial number                   (output) integer*4
             For WFPC, 1=PC 2=WF cameras
             For GLL, 1=SSI 2=SSI summation
             For CASSI, 1=ISSNA, 2=ISSWA, 21=ISSNA SUM2, 22=ISSWA SUM2,
                       41=ISSNA SUM4, 42=ISSWA SUM4
	     For MPF, 0 always (add if needed, better via MPF label routines)
    fds    - image number                           (output) integer*4
             For VGR is the fds count
             For GLL is the sclk count
	     For WFPC, 0=fds not applicable
             For CASSI, is the sclk count
	     For MPF, 0 always (add if needed, better via MPF label routines)
    ind    - 0=normal   1=error                     (output) integer*4

   Revision History
    SP   2-97  Changed to use the C calling sequence for zable97.  Added "!"
               before strcmp for Cassini camera determination; then changed
               to use strncmp, because character data in structure is not null-
               terminated.
   VRH   4-01  Changed camera to reflect summation mode
   VRH   4-03  Cassini Tour projects will have 'CASSINI-HUYGENS' as MISSION.
**********************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "cas_isslab.h"
#include "strcasecmp.h"
#include "getproj.h"
#include "zvproto.h"
#include "prnt.h"
#include <string.h>
#include <stdio.h>

/*
* declarations of functions used by getproj
*/
void zable86(int *ind, int unit, int *buf);
void zable77v2(int *ind, int unit, void *arra);
void zvolabv2(int *ind, int unit, void *lbuff);
void zable97(int *ind, int unit, void *lab);

/*---------------------------------------------------------------------------*/
/* fortran-callable version                                                  */
/*---------------------------------------------------------------------------*/
void FTN_NAME2(getproj, GETPROJ) (int *unit, char *project, int *camera,
			int *fds, int *ind, ZFORSTR_PARAM)
#if 0
  int     *unit; /* the unit of the file to be read (input)  */
  char *project; /* spacecraft identity             (output) */
  int   *camera; /* camera serial number            (output) */
  int      *fds; /* image number                    (output) */
  int      *ind; /* returned status                 (output) */
#endif
{
  ZFORSTR_BLOCK
  char proj[6];
  int length;

  length = 5;

/* 5 args for getproj, project is 2nd arg and 1st string */

   zgetproj(*unit,proj,camera,fds,ind);

   zsc2for(proj, length, project, &unit, 5, 2, 1, ind);
   return;
}
/*---------------------------------------------------------------------------*/
/* c-callable version                                                        */
/*---------------------------------------------------------------------------*/
  void    zgetproj(unit,project,camera,fds,ind)

      char *project;
      int  *ind, unit, *camera, *fds;

{
      char labi[3600],*ist;
      int bufsize,stat,iind;
      int buf[160],vgrarr[10];
      able97_typ casbuf;
      int status;

      bufsize = 3600;
      stat = zlgetlabel(unit,labi,&bufsize);
/* check status */
      if(stat != 0 && stat != 1)
	{
	  zvmessage("ERROR IN ZLGETLABEL, STAT =","");
	  zprnt(4,1,&stat,".");
	  return;
	}  

      *ind = 0;

/* galileo flight label */

      if(strstr(labi,"MISSION='GALILEO'") != NULL)
	{

          strcpy(project,"GLL  ");
          buf[0] = 5;

          zable86(ind,unit,buf);
    
           if(*ind != 0)
	     {
              zvmessage("GETPROJ: ABLE86 label error","");  
              *ind = 1;
	     }

          *fds = buf[1];
          if (buf[4]==1 || buf[4]==5)
            *camera = 2;
          else
            *camera = 1;
	  return;
	}     

/* galileo calibration label*/

       if(strstr(labi,"LAB01='GLL/SSI") != NULL)
	 {
           strcpy(project, "GLL  ");
           buf[0] = 5;
           zable86(ind,unit,buf);
           if(*ind != 0)
	   {
              zvmessage("GETPROJ: ABLE86 label error","");  
              *ind = 1;
           }

          *fds = buf[1];

          if (buf[4]==1 || buf[4]==5)
            *camera=2;
          else
            *camera=1;
	  return;
	 } 

/* voyager 2 */

       if(strstr(labi,"VGR-2") != NULL)
	 {
           strcpy(project, "VGR-2");
           vgrarr[0] = 10;
           zable77v2(ind,unit,vgrarr);
           if(*ind != 0)
	   {
              zvmessage("GETPROJ: ABLE77V2 label error","");
             *ind = 1;
           }
          *camera = vgrarr[5];
          *fds = vgrarr[1];
	  return;
	 }
/* voyager 1 */

       if(strstr(labi,"VGR-1") != NULL)
	 {
           strcpy(project, "VGR-1");
           vgrarr[0] = 10;
           zable77v2(ind,unit,vgrarr);
           if(*ind != 0)
	   {
             zvmessage("GETPROJ: ABLE77V2 label error","");
            *ind = 1;
           }
           *camera = vgrarr[5];
           *fds = vgrarr[1];
	   return;
	 }
/* Space Telescope WFPC1 */

       if(strstr(labi,"INSTRUME=  WFPC") != NULL)
	 {
            strcpy(project,"WFPC1");
            if(strstr(labi,"CAMERA  =  PC") != NULL) 
	       *camera = 1;
            else if(strstr(labi,"CAMERA  =  WF") != NULL) 
	       *camera = 2;
            else
	     {
               zvmessage("GETPROJ: WFPC1 cannot find camera id","");
               *ind = 1;
	     }
	    *fds = 0;
	    return;
          }

/* Space Telescope WFPC2
   The actual key is unknown at this time (WFPC2) */

       if(strstr(labi,"INSTRUME=  WFPC2") != NULL)
	 {
            strcpy(project,"WFPC2");
            if(strstr(labi,"CAMERA  =  PC") != NULL)
               *camera = 1;
            else if(strstr(labi,"CAMERA  =  WF") != NULL)
               *camera = 2;
            else
	     {
               zvmessage("GETPROJ: WFPC2 cannot find camera id","");
               *ind = 1;
	     }
	    *fds = 0;
	    return;
          }

/* viking orbiter */

       if((strstr(labi,"VIS ") != NULL) ||
               (strstr(labi,"VO75 ") != NULL))
	{
            strcpy(project,"VIKOR");
            zvolabv2(ind,unit,buf);
            if(*ind != 0 || buf[0] == 0) 
	    {
              zvmessage("GETPROJ: VOLABV2 label error","");
             *ind = 1;
            }
           *camera = buf[1];
           *fds = buf[6];
	   return;
	 }	   
/* mariner 10 */

       if(strstr(labi,"MVM73") != NULL)
	 {
            ist = strstr(labi,"MVM73");
            strcpy(project, "MAR10");
           *camera = 1;

            if(strncmp(ist+89,"B",1) == 0)
	        *camera = 2;
	    iind = sscanf(ist+13,"%d",fds);
            *ind = 1;                       /*error */
	    if(iind  >  0) 
	      *ind=0;	                   /* no error */
	    return;
	 }
/* mariner 9 */

       if(strstr(labi,"MARINER 9") != NULL)
	 {
	    ist = strstr(labi,"MARINER 9");
            strcpy(project, "MAR-9");
           *camera = 1;
            if(strncmp(ist+14,"B",1) == 0) 
	       *camera = 2;
            iind = sscanf(ist + 58,"%d",fds);
	   *ind = 1;		   /* error */
	    if(iind  >  0)
	       *ind = 0;          /* no error */
	    return;
	 }

/* Cassini Orbiter */

      if((strstr(labi,"MISSION_NAME='CASSINI'") != NULL) ||
         (strstr(labi,"MISSION_NAME='CASSINI-HUYGENS'") != NULL))
	{
          strcpy(project,"CASSI");
          zable97(ind,unit,&casbuf);
    
          if(*ind != 0)
	     {
              zvmessage("GETPROJ: ABLE97 label error","");  
              *ind = 1;
	     }
          *fds = casbuf.sclk;
          if(!strncmp(casbuf.camera,"ISSNA",5))
             {
              *camera = 1;
              if(!strncmp(casbuf.mode,"SUM2",4))  *camera = 21;
              if(!strncmp(casbuf.mode,"SUM4",4))  *camera = 41;
             }
          else if(!strncmp(casbuf.camera,"ISSWA",5))
             {
              *camera = 2;
              if(!strncmp(casbuf.mode,"SUM2",4))  *camera = 22;
              if(!strncmp(casbuf.mode,"SUM4",4))  *camera = 42;
             }
          else
            {
              zvmessage("GETPROJ: label error, must be ISSNA or ISSWA ","");  
              *ind = 1;
            }
	  return;
	}     

/* Use "correct" label access for the more modern missions... */
/* Don't use "labi" below this point, it is re-used for a temporary buffer */

	status = zlget(unit, "PROPERTY", "MISSION_NAME", labi,
			"property", "telemproc",
			"format", "string", "err_act", "", NULL);
	if (status == 1)
	  {

/* Mars Pathfinder */

	    if (strcasecmp(labi, "MARS PATHFINDER") == 0)
	      {
		strcpy(project, "MPF");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
	  }

/* Of course, the "multimission" mision name labels keep changing... */

	status = zlget(unit, "PROPERTY", "MISSION_NAME", labi,
			"property", "identification",
			"format", "string", "err_act", "", NULL);

	if (status == 1)
	  {

/* Mars 98 Lander */

	    if (strcasecmp(labi, "MARS SURVEYOR 98") == 0)
	      {
		strcpy(project, "M98");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars 01 Lander Testbed */

	    if (strcasecmp(labi, "M01" ) == 0)
	      {
		strcpy(project, "M01");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars FIDO Testbed */

	    if (strcasecmp(labi, "FIDO-TESTBED" ) == 0)
	      {
		strcpy(project, "FIDO");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
/* Mars MER mission*/

	    if (strcasecmp(labi, "MARS EXPLORATION ROVER" ) == 0)
	      {
		strcpy(project, "MER");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars Phoenix mission*/

	    if ((strcasecmp(labi, "PHOENIX LANDER" ) == 0) ||
		(strcasecmp(labi, "PHOENIX") == 0))
	      {
		strcpy(project, "PHX");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars MSL mission*/

	    if (strcasecmp(labi, "MARS SCIENCE LABORATORY" ) == 0)
	      {
		strcpy(project, "MSL");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }

/* Mars InSight mission*/

	    if ((strcasecmp(labi, "INSIGHT LANDER" ) == 0) ||
		(strcasecmp(labi, "INSIGHT") == 0))
	      {
		strcpy(project, "NSYT");

		*ind = 0;
		*camera = 0;
		*fds = 0;
		return;
	      }
	}


/* unrecognizable project */

        zvmessage("GETPROJ: unrecognizable project","");
        *ind = 1;
}

