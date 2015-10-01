/*	TEK_ConfigDevice - description
 *
 *	Purpose: Configure the Tektronix display terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_ConfigDevice(Unit, config)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		config:	Four integer array of configuration information...
 *			the last element is currently unused
 *
 *	Possible Error Codes:
 *		none
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_ConfigDevice(Unit, config)
int *Unit, config[4];
{
  long	jterm;
  int	lut, imp, color, maxval;
  int	size, status, address, line, samp;

  if (DEV_TYPE == TEK_4237)
  {
    if (config[1] == IMP_1024)
    {
      N_SAMPS = 1024;
      N_LINES = 1024;
    }
    else
      return(INVALID_DEVICE_REQ);

    if (config[2] == VIDEO_1024)
    {
      if (config[1] == IMP_1024)
      {
        VIDEO_SAMPLES = 1024;
        VIDEO_LINES = 1024;
      }
      else
        return(INVALID_DEVICE_REQ);
    }
    else
      return(INVALID_DEVICE_REQ);
  }

  else					/* Tektronix Stereo Mode */
  {
    if (config[1] == IMP_1024_512)
    {
      N_SAMPS = 1024;
      N_LINES = 512;
    }
    else if (config[1] == IMP_512)
    {
      N_SAMPS = 512;
      N_LINES = 512;
    }
    else
      return(INVALID_DEVICE_REQ);

    if (config[2] == VIDEO_1024_512)
    {
      if (config[1] == IMP_1024_512)
      {
        VIDEO_SAMPLES = 1024;
        VIDEO_LINES = 512;
      }
      else
        return(INVALID_DEVICE_REQ);
    }
    else if (config[2] == VIDEO_512)
    {
      if (config[1] == IMP_512)
      {
        VIDEO_SAMPLES = 512;
        VIDEO_LINES = 512;
      }
      else
        return(INVALID_DEVICE_REQ);
    }
    else
      return(INVALID_DEVICE_REQ);
  }

  /*  Allocate shared memory for the image planes and look-up tables.  */

  size = (N_IMPS * N_LINES * N_SAMPS) + ((2*N_LUTS) * (MAX_LUT_VALUE+1));
  switch(DEV_TYPE)
  {
    case TEK_4237:
      status = Attach_Shmem(*Unit, size, &TEK_LUTS, SHMEM_TEK);
      if ((status != SUCCESS) && (status != SHMEM_ALREADY_ACTIVE))
        return(status);

      TEK_OVLY_LUTS = (char *)TEK_LUTS + ((N_LUTS) * (MAX_LUT_VALUE+1));
      TEK_IMPS = (char *)TEK_LUTS + ((2*N_LUTS) * (MAX_LUT_VALUE+1));

      /*  Fill image plane look-up tables  */

      for (lut=1, imp=1; lut <= N_LUTS; lut++)
      {
        for (color = 0; color <= MAX_LUT_VALUE; color++)
          TEK_LUT(lut, color) = color;
        WHICH_IMP(lut) = imp;
        TEK_WHICH_IMP(lut) = imp;
        if (config[0] == FULL_COLOR)
          imp++;
      }

      /*  Fill overlay plane look-up table  */

      for (color = 0; color <= MAX_LUT_VALUE; color++)
      {
        if (color & 0x01)			/*  red      */
            TEK_OVLY_LUT(TEK_RED, color) += 255;

        if (color & 0x02)			/*  green    */
            TEK_OVLY_LUT(TEK_GREEN, color) += 255;

        if (color & 0x04)			/*  blue     */
            TEK_OVLY_LUT(TEK_BLUE, color) += 255;

        if (color & 0x08)			/*  white    */
        {
          TEK_OVLY_LUT(TEK_RED, color) += 255;
          TEK_OVLY_LUT(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT(TEK_BLUE, color) += 255;
        }

        if (color & 0x10)			/*  magenta  */
        {
          TEK_OVLY_LUT(TEK_RED, color) += 255;
          TEK_OVLY_LUT(TEK_BLUE, color) += 255;
        }

        if (color & 0x20)			/*  yellow   */
        {
          TEK_OVLY_LUT(TEK_RED, color) += 255;
          TEK_OVLY_LUT(TEK_GREEN, color) += 255;
        }

        if (color & 0x40)			/*  cyan     */
        {
          TEK_OVLY_LUT(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT(TEK_BLUE, color) += 255;
        }

        maxval = MAX(TEK_OVLY_LUT(TEK_RED, color),
                     MAX(TEK_OVLY_LUT(TEK_GREEN, color),
                         TEK_OVLY_LUT(TEK_BLUE, color)));

        if (maxval > 255)			/*  need to adjust range  */
        {
          TEK_OVLY_LUT(TEK_RED, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT(TEK_GREEN, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT(TEK_BLUE, color) *= (255.0 / (double)maxval);
        }

        if (color & 0x80)			/*  black  */
        {
          TEK_OVLY_LUT(TEK_RED, color) /= 2;	/*  cut intensity in half  */
          TEK_OVLY_LUT(TEK_GREEN, color) /= 2;
          TEK_OVLY_LUT(TEK_BLUE, color) /= 2;
        }
      }
      break;
    case TEK_3D_LEFT:
      status = Detach_Shmem(*Unit, &TEK_LUTS_3D_LEFT, SHMEM_TEK);
      if (status == SUCCESS)
        status = Attach_Shmem(*Unit, size, &TEK_LUTS_3D_LEFT, SHMEM_TEK);
      if ((status != SUCCESS) && (status != SHMEM_ALREADY_ACTIVE))
        return(status);

      TEK_OVLY_LUTS_LEFT = (char *)TEK_LUTS_3D_LEFT+((N_LUTS)*(MAX_LUT_VALUE+1));
      TEK_IMPS_3D_LEFT = (char *)TEK_LUTS_3D_LEFT+((2*N_LUTS)*(MAX_LUT_VALUE+1));

      /*  Fill image plane look-up tables  */

      for (lut=1, imp=1; lut <= N_LUTS; lut++)
      {
        for (color=0; color <= MAX_LUT_VALUE; color++)
          TEK_LUT_3DL(lut, color) = color;
        WHICH_IMP(lut) = imp;
        TEK_WHICH_IMP(lut) = imp;
        if (config[0] == FULL_COLOR)
          imp++;
      }

      /*  Fill overlay plane look-up table  */

      for (color = 0; color <= MAX_LUT_VALUE; color++)
      {
        if (color & 0x01)			/*  red      */
            TEK_OVLY_LUT_L(TEK_RED, color) += 255;

        if (color & 0x02)			/*  green    */
            TEK_OVLY_LUT_L(TEK_GREEN, color) += 255;

        if (color & 0x04)			/*  blue     */
            TEK_OVLY_LUT_L(TEK_BLUE, color) += 255;

        if (color & 0x08)			/*  white    */
        {
          TEK_OVLY_LUT_L(TEK_RED, color) += 255;
          TEK_OVLY_LUT_L(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT_L(TEK_BLUE, color) += 255;
        }

        if (color & 0x10)			/*  magenta  */
        {
          TEK_OVLY_LUT_L(TEK_RED, color) += 255;
          TEK_OVLY_LUT_L(TEK_BLUE, color) += 255;
        }

        if (color & 0x20)			/*  yellow   */
        {
          TEK_OVLY_LUT_L(TEK_RED, color) += 255;
          TEK_OVLY_LUT_L(TEK_GREEN, color) += 255;
        }

        if (color & 0x40)			/*  cyan     */
        {
          TEK_OVLY_LUT_L(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT_L(TEK_BLUE, color) += 255;
        }

        maxval = MAX(TEK_OVLY_LUT_L(TEK_RED, color),
                     MAX(TEK_OVLY_LUT_L(TEK_GREEN, color),
                         TEK_OVLY_LUT_L(TEK_BLUE, color)));

        if (maxval > 255)			/*  need to adjust range  */
        {
          TEK_OVLY_LUT_L(TEK_RED, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT_L(TEK_GREEN, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT_L(TEK_BLUE, color) *= (255.0 / (double)maxval);
        }

        if (color & 0x80)			/*  black  */
        {
          TEK_OVLY_LUT_L(TEK_RED, color) /= 2;	/*  cut intensity in half  */
          TEK_OVLY_LUT_L(TEK_GREEN, color) /= 2;
          TEK_OVLY_LUT_L(TEK_BLUE, color) /= 2;
        }
      }
      break;
    case TEK_3D_RIGHT:
      status = Detach_Shmem(*Unit, &TEK_LUTS_3D_RIGHT, SHMEM_TEK);
      if (status == SUCCESS)
        status = Attach_Shmem(*Unit, size, &TEK_LUTS_3D_RIGHT, SHMEM_TEK);
      if ((status != SUCCESS) && (status != SHMEM_ALREADY_ACTIVE))
        return(status);

      TEK_OVLY_LUTS_RIGHT=(char *)TEK_LUTS_3D_RIGHT+((N_LUTS)*(MAX_LUT_VALUE+1));
      TEK_IMPS_3D_RIGHT=(char *)TEK_LUTS_3D_RIGHT+((2*N_LUTS)*(MAX_LUT_VALUE+1));

      /*  Fill image plane look-up tables  */

      for (lut=1, imp=1; lut <= N_LUTS; lut++)
      {
        for (color=0; color <= MAX_LUT_VALUE; color++)
          TEK_LUT_3DR(lut, color) = color;
        WHICH_IMP(lut) = imp;
        TEK_WHICH_IMP(lut) = imp;
        if (config[0] == FULL_COLOR)
          imp++;
      }

      /*  Fill overlay plane look-up table  */

      for (color = 0; color <= MAX_LUT_VALUE; color++)
      {
        if (color & 0x01)			/*  red      */
            TEK_OVLY_LUT_R(TEK_RED, color) += 255;

        if (color & 0x02)			/*  green    */
            TEK_OVLY_LUT_R(TEK_GREEN, color) += 255;

        if (color & 0x04)			/*  blue     */
            TEK_OVLY_LUT_R(TEK_BLUE, color) += 255;

        if (color & 0x08)			/*  white    */
        {
          TEK_OVLY_LUT_R(TEK_RED, color) += 255;
          TEK_OVLY_LUT_R(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT_R(TEK_BLUE, color) += 255;
        }

        if (color & 0x10)			/*  magenta  */
        {
          TEK_OVLY_LUT_R(TEK_RED, color) += 255;
          TEK_OVLY_LUT_R(TEK_BLUE, color) += 255;
        }

        if (color & 0x20)			/*  yellow   */
        {
          TEK_OVLY_LUT_R(TEK_RED, color) += 255;
          TEK_OVLY_LUT_R(TEK_GREEN, color) += 255;
        }

        if (color & 0x40)			/*  cyan     */
        {
          TEK_OVLY_LUT_R(TEK_GREEN, color) += 255;
          TEK_OVLY_LUT_R(TEK_BLUE, color) += 255;
        }

        maxval = MAX(TEK_OVLY_LUT_R(TEK_RED, color),
                     MAX(TEK_OVLY_LUT_R(TEK_GREEN, color),
                         TEK_OVLY_LUT_R(TEK_BLUE, color)));

        if (maxval > 255)			/*  need to adjust range  */
        {
          TEK_OVLY_LUT_R(TEK_RED, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT_R(TEK_GREEN, color) *= (255.0 / (double)maxval);
          TEK_OVLY_LUT_R(TEK_BLUE, color) *= (255.0 / (double)maxval);
        }

        if (color & 0x80)			/*  black  */
        {
          TEK_OVLY_LUT_R(TEK_RED, color) /= 2;	/*  cut intensity in half  */
          TEK_OVLY_LUT_R(TEK_GREEN, color) /= 2;
          TEK_OVLY_LUT_R(TEK_BLUE, color) /= 2;
        }
      }
      break;
  }

  for (imp = 1; imp <= N_IMPS; imp++)
  {
    AW_LEFT(imp) = 1;
    AW_TOP(imp) = 1;
    AW_RIGHT(imp) = N_SAMPS;
    AW_BOTTOM(imp) = N_LINES;
    TEK_ZOOM(imp) = 1;
    TEK_DW_LEFT(imp) = 1;
    TEK_DW_TOP(imp) = 1;
    TEK_ANGLE(imp) = TEK_NO_ANGLE;
    TEK_WRITTEN(imp) = FALSE;
  }

  llinit(&jterm);			/* Start STI */
  llcode(TEK);				/* Set mode to TEK */
  llkblk(TEK_ON);			/* Lock keyboard */

  llmove(0, 0);
  llrnvw(TEK_ALL_VIEWS);		/* Refresh screen */

  llkblk(TEK_OFF);			/* Unlock keyboard when finished */
  llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
  llstop();

  return (SUCCESS);
}
