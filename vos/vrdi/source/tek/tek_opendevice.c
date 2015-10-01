/*	TEK_OpenDevice - description
 *
 *	Purpose: Open an I/O channel to the Tektronix display terminal
 *
 *	Written by: Paul Bartholomew
 *	Date:	    September 18, 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = TEK_OpenDevice(Unit)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *		CANNOT_ALLOC_DEVICE : I/O channel could not be opened.
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdalloc.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_OpenDevice(Unit)
int *Unit;
{
  char	actual[20], logical[20];
  long	jterm, icmry[2], isrfry[TEK_MAX_PLANES], idisry[3];
  long	status=SUCCESS, axis[3], trig[3], gen[3], view[3];
  int	array_len, i, trackball_rates[14], mode;

  strcpy(actual, "TEKTERM");
  actual[8] = '\0';
  strcpy(logical, "TT");
  logical[2] = '\0';
  status = DefineLogicalName(actual, logical, VMS_PROCESS_TABLE);

  if (status == SUCCESS)
  {
    if (DEV_TYPE == TEK_4237)
      mode = TEK_MONO_VIEW;
    else
      mode = TEK_STEREO_VIEW;

    loadcmap(mode);			/* Set optimal color map */
    llinit(&jterm);			/* Start STI */
    llcode(TEK);			/* Set mode to TEK */
    llkblk(TEK_ON);			/* Lock keyboard */

    llvwpt(0, 0, TEK_MAX_SCREEN_SPACE, TEK_MAX_SCREEN_SPACE);
    llwind(0, 0, TEK_MAX_SCREEN_SPACE, TEK_MAX_SCREEN_SPACE);
    llrnvw(TEK_ALL_VIEWS);		/* Refresh screen */

    llcord(MODE12BIT, REPORT_SIZE);	/* Set coordinate mode */
    lpalu(ALU_SET);			/* Set ALU pixel mode */

    llwmsg(ALL_SEGMENTS, TEK_SET_MODE);

    array_len = 14;
    trackball_rates[0] = 15;
    trackball_rates[1] = 0;
    for (i = 1; i <= 12; i++)
      trackball_rates[i+1] = i * 16;
    llgnrt(array_len, trackball_rates);

    llerth(ERR_LEVEL_3);		/* Set error message reporting */
    llpasg(3, "P0:", 4, "TRAP");	/* Assign trackball to port 0 */
    axis[0] = 29;
    axis[1] = 30;
    trig[0] = -32500;
    llmgin(10, 2, axis, 1, trig, 0, gen, 0, view);
    llengn(TEK_TRACKBALL, 0);		/* Enable trackball */
    llcrgn(TEK_TRACKBALL, 0);		/* Set segment to be a cursor */
    llmove(0, 0);

    array_len = 1;
    isrfry[0] = 8;			/* 8 bits for each color (RGB) */
    lldfsf(array_len,isrfry);		/* Define surface, assign planes */

    llclmd(TEK_MACHINE_RGB, TEK_ADDITIVE, TEK_NORMAL_COLOR);
    array_len = 3;
    idisry[0] = TEK_SINGLE_BUFFER;
    idisry[1] = TEK_TRUE_COLOR;
    idisry[2] = mode;
    lldsmd(array_len,idisry);		/* Set display mode */

    llbgcl(0, 0, 0);			/* Set background color transparent */
    llvwat(TEK_ALL_SURFACES, 0, TRUE_WHITE);  /* Set cursor color */

    lllntp(TEK_PIXEL_WIDE);		/* Set line type */
    lllnwd(TEK_PIXEL_WIDE);		/* Set line width */
    lllnst(TEK_SOLID);			/* Set line style */

    icmry[0] = 1;
    lldcmd(1, icmry);			/* Set dialog area color model */
    lldain(TRUE_WHITE, 0, 0);		/* Set dialog area text color */

    llkblk(TEK_OFF);			/* Unlock keyboard when finished */
    llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
    llstop();

    status = TEK_ConfigDevice(Unit, DIB[*Unit]->DefaultConfig);
  }

  return (status);
}
