#include "xvmaininc.h"
#include "ftnbridge.h"
#include "applic.h"
#include "zvproto.h"
#include <ctype.h>
#include <string.h>

/* prototypes: */
int zgllcalname( char *, int *,  int *, int *, int *, int *, int *);
int gllradcal( char *, int *,  int *);
int glldccal( char *, int *,  int *, int *, int *, int *);
int gllblemcal( char *, int *,  int *, int *);
int gllsocal( char *);

/****************************************************************************
  These external variables are only needed by the routines in this
  module.  Once a calibration catelog has been implemented these static
  variables can be removed (of course the code needs to be changed too).
*****************************************************************************/

static char	*fltr_strg[] = {"CLR", "GRN", "RED", "VLT", 
                                "756" ,"968", "727", "889"},
		*gain_strg[] = {"?", "1", "2", "3", "4"},
		*mode_strg[] = {"F", "S"},
		*type_strg[] = {".NUL", ".CAL", ".DC", ".BLM", ".SO"},
		*rate_strg[] = {"?", "2", "8", "30", "60", "15"};

/****************************************************************************
		             GLLCALNAME

   Returns a calibration file name based on various parameters.
*****************************************************************************
                        Fortran-Callable Version 
*****************************************************************************/

void FTN_NAME (gllcalname)(file_name,type,status,filter,frame_rate,gain,fibe, 
                           rmode,FORSTR_PARAM)
  char	*file_name;	/*  Returned file name	*/
  int	*type,		/*  Requested calibration file type	*/
			/*	1 - Radiometric		*/
			/*	2 - Dark Current	*/
			/*	3 - Blemish		*/
			/*	4 - Shutter Offset	*/
	*status,	/*  Returned status indicator	*/
	*filter,	/*  Filter	*/
	*frame_rate,	/*  Frame rate (Indirectly includes frame mode)	*/
	*gain,		/*  Gain state	*/
	*fibe,		/*  Clock_invert; Blem_prot & Ext_exposure	*/
        *rmode;		/*  SSI readout mode (0=not applicable,1=sample,
			    2=contiguous)	*/
FORSTR_DEF
{
   FORSTR_BLOCK
   char c_string [256];

   /* Obtain the filename of the proper calibration file from the set of
   passed-in defining parameters */

   *status = zgllcalname (c_string,type,filter,frame_rate,gain,fibe,rmode);

   /* Convert the returned c_string file name to a FORTRAN string 
   before returning to the calling FORTRAN program*/

   sc2for (c_string, 0, file_name, &file_name, 8, 1, 1);
   zccase(file_name, -1, strlen(file_name));

   return;
}

/****************************************************************************
                         C-Callable Version
*****************************************************************************/

int zgllcalname (file_name,type,filter,frame_rate,gain,fibe,rmode)
  char	*file_name;	/*  Returned file name	*/
  int	*type,		/*  Requested calibration file type	*/
			/*	1 - Radiometric		*/
			/*	2 - Dark Current	*/
			/*	3 - Blemish		*/
			/*	4 - Shutter Offset	*/
	*filter,	/*  Filter	*/
	*frame_rate,	/*  Frame rate (Indirectly includes frame mode)	*/
	*gain,		/*  Gain state	*/
	*fibe,		/*  Clock_invert; Blem_prot & Ext_exposure	*/
	*rmode;		/*  SSI readout mode (0=not applicable,1=sample,
			    2=contiguous) 	*/
{ 

  int   status;	        /*  Returned status indicator	*/
  int   frame_mode;     /* Frame mode */

  /***  Detemine frame mode from frame_rate parameter  ***/
  /***  0 - Full Frame;  1 - Summation mode         ***/
  if (*frame_rate==1 || *frame_rate==5) frame_mode=1;
  else frame_mode=0;

  /***  Call routines for different supported calibration files  ***/

  switch (*type) {

    case 1: /***  Radiometric Calibration File  ***/
            status = gllradcal(file_name,filter,&frame_mode);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 2: /***  Dark_Current Calibration File  ***/
            status = glldccal(file_name,&frame_mode,gain,frame_rate,fibe,rmode);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 3: /***  Blemish Calibration File  ***/
            status = gllblemcal(file_name,filter,&frame_mode,gain);
            zccase(file_name, -1, strlen(file_name));
            break;

    case 4: /***  Shutter Offset Calibration File  ***/
            status = gllsocal(file_name);
            zccase(file_name, -1, strlen(file_name));
            break;

    default:
            zvmessage ("GLLCALNAME - Invalid calibration file type requested","");
            status = FALSE; /***  SOME BAD NEWS INDICATOR  ***/

         break;
  }

  return status;
}

/****************************************************************************
				GLLRADCAL

  Returns the radiometric calibration file based on filter position
  and frame mode.
*****************************************************************************/

int gllradcal(file_name,filter,frame_mode)
  char	*file_name;
  int	*filter,
	*frame_mode;
{
  if ((*filter < 0 || *filter > 7) || (*frame_mode < 0 || *frame_mode > 1))
  { zvmessage ("GLLRADCAL - Invalid filter position or frame mode","");
    return FALSE;
  }

  strcpy(file_name,fltr_strg[*filter]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,type_strg[1]);
  return TRUE;
}

/****************************************************************************
				GLLDCCAL

  Returns the Dark Current calibration file name based on frame mode,
  gain state, frame rate and FIBE flags.
*****************************************************************************/

int glldccal(file_name,frame_mode,gain,frame_rate,fibe,rmode)
  char	*file_name;
  int	*frame_mode,
	*gain,
	*frame_rate,
	*fibe,
	*rmode;
{
  if ((*frame_mode < 0 || *frame_mode > 1) || (*gain < 1 || *gain > 4) ||
      (*frame_rate < 1 || *frame_rate > 5))
  { zvmessage ("GLLDCCAL - Invalid frame mode, gain state or frame rate","");
    return FALSE;
  }
  strcpy(file_name,gain_strg[*gain]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,rate_strg[*frame_rate]);
  if (((*fibe % 1000)/100) == 1) strcat(file_name,"I");
  if (((*fibe % 100)/10) == 1) strcat(file_name,"B");
  if (((*fibe % 10) == 1) || ((*fibe / 100000) == 1)) strcat(file_name,"X");
  if (*rmode == 1) strcat(file_name,"R");
  if (*rmode == 2) strcat(file_name,"C");
  strcat(file_name,type_strg[2]);
  return TRUE;
}

/****************************************************************************
				GLLBLEMCAL

  Returns the Blemish calibration file name based on the filter position,
  frame mode and gain state.
*****************************************************************************/
int gllblemcal(file_name,filter,frame_mode,gain)
  char	*file_name;
  int	*filter,
	*frame_mode,
	*gain;
{
  if ((*filter < 0 || *filter > 7) || (*frame_mode < 0 || *frame_mode > 1) ||
      (*gain < 1 || *gain > 4))
  { zvmessage 
        ("GLLBLEMCAL - Invalid filter position, frame mode or gain state","");
    return FALSE;
  }
  strcpy(file_name,fltr_strg[*filter]);
  strcat(file_name,gain_strg[*gain]);
  strcat(file_name,mode_strg[*frame_mode]);
  strcat(file_name,type_strg[3]);
  return TRUE;
}

/****************************************************************************
				GLLSOCAL

	Returns the Shutter Offset calibration file name (only one for now).
*****************************************************************************/
int gllsocal(file_name)
  char	*file_name;
{
  strcpy(file_name,"CALIBRATION");
  strcat(file_name,type_strg[4]);
  return TRUE;
}

