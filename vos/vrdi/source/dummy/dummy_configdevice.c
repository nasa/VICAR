/*	Dummy_ConfigDevice - description
 *
 *	Purpose: Configure the dummy display terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_ConfigDevice(Unit, config)
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

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "dummy_dev.h"

FUNCTION Dummy_ConfigDevice(Unit, config)
int *Unit, config[4];
{
  int	lut, imp, color, maxval, cursor;
  int	size, status, address, line, samp;
  int   old_samps, old_lines;

  old_samps = N_SAMPS;
  old_lines = N_LINES;

  if (config[1] == IMP_512)
  {
    N_SAMPS = 512;
    N_LINES = 512;
  }
  else if (config[1] == IMP_1024)
  {
    N_SAMPS = 1024;
    N_LINES = 1024;
  }
  else if (config[1] == IMP_640_480)
  {
    N_SAMPS = 640;
    N_LINES = 480;
  }
  else if (config[1] == IMP_640_512)
  {
    N_SAMPS = 640;
    N_LINES = 512;
  }
  else if (config[1] == IMP_1024_512)
  {
    N_SAMPS = 1024;
    N_LINES = 512;
  }
  else if (config[1] == IMP_1280_1024)
  {
    N_SAMPS = 1280;
    N_LINES = 1024;
  }
  else if (config[1] == IMP_2048)
  {
    N_SAMPS = 2048;
    N_LINES = 2048;
  }
  else if (config[1] == IMP_4096)
  {
    N_SAMPS = 4096;
    N_LINES = 4096;
  }
  else
    return(INVALID_DEVICE_REQ);


  if (config[2] == VIDEO_512)
  {
    VIDEO_SAMPLES = 512;
    VIDEO_LINES = 512;
  }
  else if (config[2] == VIDEO_1024)
  {
    VIDEO_SAMPLES = 1024;
    VIDEO_LINES = 1024;
  }
  else if (config[2] == VIDEO_640_480)
  {
    VIDEO_SAMPLES = 640;
    VIDEO_LINES = 480;
  }
  else if (config[2] == VIDEO_640_512)
  {
    VIDEO_SAMPLES = 640;
    VIDEO_LINES = 512;
  }
  else if (config[2] == VIDEO_1024_512)
  {
    VIDEO_SAMPLES = 1024;
    VIDEO_LINES = 512;
  }
  else if (config[2] == VIDEO_1280_1024)
  {
    VIDEO_SAMPLES = 1280;
    VIDEO_LINES = 1024;
  }
  else
    return(INVALID_DEVICE_REQ);

  /*  Allocate shared memory for the image planes and look-up tables.  */

  size = (N_IMPS * N_LINES * N_SAMPS) + ((2*N_LUTS) * (MAX_LUT_VALUE+1));

  if (old_lines != N_LINES || old_samps != N_SAMPS) {
     status = Detach_Shmem(*Unit, &DUMMY_LUTS, SHMEM_DUMMY);
     if (status == SUCCESS)
        status = Attach_Shmem(*Unit, size, &DUMMY_LUTS, SHMEM_DUMMY);
  }
  else {
     status = Attach_Shmem(*Unit, size, &DUMMY_LUTS, SHMEM_DUMMY);
  }

  if ((status != SUCCESS) && (status != SHMEM_ALREADY_ACTIVE))
    return(status);

  DUMMY_OVLY_LUTS = (unsigned char *)DUMMY_LUTS + ((N_LUTS)*(MAX_LUT_VALUE+1));
  DUMMY_IMPS = (unsigned char *)DUMMY_LUTS + ((2*N_LUTS) * (MAX_LUT_VALUE+1));

  /*  Fill image plane look-up tables  */

  for (lut=1, imp=1; lut <= N_LUTS; lut++)
  {
    for (color = 0; color <= MAX_LUT_VALUE; color++)
      DUMMY_LUT(lut, color) = color;
    WHICH_IMP(lut) = imp;
    if (config[0] == FULL_COLOR)
      imp++;
  }

  /*  Fill overlay plane look-up table  */

  for (color = 0; color <= MAX_LUT_VALUE; color++)
  {
    int red, green, blue;
    red = DUMMY_OVLY_LUT(DUMMY_RED, color);
    green = DUMMY_OVLY_LUT(DUMMY_GREEN, color);
    blue = DUMMY_OVLY_LUT(DUMMY_BLUE, color);

    if (color & 0x01)			/*  red      */
      red += 255;

    if (color & 0x02)			/*  green    */
      green += 255;

    if (color & 0x04)			/*  blue     */
      blue += 255;

    if (color & 0x08)			/*  white    */
    {
      red += 255;
      green += 255;
      blue += 255;
    }

    if (color & 0x10)			/*  magenta  */
    {
      red += 255;
      blue += 255;
    }

    if (color & 0x20)			/*  yellow   */
    {
      red += 255;
      green += 255;
    }

    if (color & 0x40)			/*  cyan     */
    {
      green += 255;
      blue += 255;
    }

    maxval = MAX(red, MAX(green, blue));

    if (maxval > 255)			/*  need to adjust range  */
    {
      red *= (255.0 / (double)maxval);
      green *= (255.0 / (double)maxval);
      blue *= (255.0 / (double)maxval);
    }

    if (color & 0x80)			/*  black  */
    {
      red /= 2;				/*  cut intensity in half  */
      green /= 2;
      blue /= 2;
    }
    DUMMY_OVLY_LUT(DUMMY_RED, color) = red;
    DUMMY_OVLY_LUT(DUMMY_GREEN, color) = green;
    DUMMY_OVLY_LUT(DUMMY_BLUE, color) = blue;
  }

  for (imp = 1; imp <= N_IMPS; imp++)
  {
    AW_LEFT(imp) = 1;
    AW_TOP(imp) = 1;
    AW_RIGHT(imp) = N_SAMPS;
    AW_BOTTOM(imp) = N_LINES;
  }

  for (cursor = 1; cursor <= N_CURSORS; cursor++)
  {
    DUMMY_CURSOR_X(cursor) = 1;
    DUMMY_CURSOR_Y(cursor) = 1;
  }

  return (SUCCESS);
}
