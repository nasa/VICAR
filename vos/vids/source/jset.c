/* JSET.C -- subroutines for the jset commands (subcommands)
 */
#include "VIDSdefs.h"
TAEVariable *GetVariable();

/************************************************************************/
/* jset_range_do -- code for the JSET-RANGE command.
 * This subr will set the valid data range for a file, setting the
 * saved slope and offset into the structure for that file.
 * Alternatively, it can set the default slope and offset for files
 * that don't have an explicit range set.
 */
int jset_range_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  FileInfo	*theFile;
  char		*fileName;
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  float		low, high;	/* the low and high values for the file	*/
  double	diff;
  FileInfo	*findFileSlot();

  v = GetVariable(env, "LOW");
  if (v == NULL) return FAIL;
  low = RVAL(*v, 0);

  v = GetVariable(env, "HIGH");
  if (v == NULL) return FAIL;
  high = RVAL(*v, 0);

  diff = high - low;
  if (diff == 0.0 && (low != 0.0 || high != 0.0))	/* if equal and non-0 */
    diff = 0.0001;			/* prevent divide by 0 below */

  v = GetVariable(env, "INP");
  if (v == NULL) return FAIL;
  if (v->v_count == 0)			/* INP was left off, so set default */
  {
    if (low == 0.0 && high == 0.0)
    {
      env->default_slope = 1.0;		/* reset default to (0,255) range */
      env->default_offset = 0.0;
    }
    else
    {
      env->default_slope = 255.0 / diff;
      env->default_offset = -255.0 * low / diff;
    }
  }
  else				/* filename given, so set individual file */
  {
    fileName = SVAL(*v, 0);

    theFile = findFileSlot(env, fileName);
    if (theFile == NULL)
        ABORT(FAIL, "Insufficient memory to prepare file information",
              "VIDS-NOMEM");

    if (low == 0.0 && high == 0.0)		/* restore default */
    {
      theFile->scale.slope = 0.0;
      theFile->scale.offset = 0.0;
    }
    else
    {
      theFile->scale.slope = 255.0 / diff;
      theFile->scale.offset = -255.0 * low / diff;
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* GetDefDataRange gets the default data range and returns it in the
 * parameters.  This routine is needed so the default range can be
 * variables local to this module instead of global.
 */
GetDefDataRange(env, slope, offset)
  VIDSEnvironment *env;			/* the VIDS environment */
  float *slope;				/* returned slope of range */
  float *offset;			/* returned offset of range */
{
  *slope = env->default_slope;
  *offset = env->default_offset;
  return;
}

/************************************************************************/
/* jset_message_do sets what messages will be printed to the user terminal.
 * Used to switch between silent and verbose.
 */
int jset_message_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  char		*degree;
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  
  v = GetVariable(env, "DEGREE");
  if (v == NULL) return FAIL;
  degree = SVAL(*v, 0);

  switch (degree[0] & 0xDF)	/* check the first letter in upper case */
  {
    case 'V' :		/* Verbose - All messages print			*/
        SetMessage(Verbose); break;
    case 'I' :		/* Inform - informational messages print	*/
        SetMessage(Inform); break;
    case 'S' :		/* Silent - no messages print			*/
        SetMessage(Silent); break;
  }
  return SUCCESS;
}
/************************************************************************/
/* jset_cursor_do -- code for the JSET-CURSOR command.  This routine will
 * set the cursor form and blink to that specified by the user.
 */
int jset_cursor_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  int		status;
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int		x,y;
  ColorTriplet	color;		/* RGB triplet for cursor color */
  int		plane;		/* plane # for IMP or FILE coordinates */
  int		planecnt;

  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  if (v->v_count == 2)
  {
    x = IVAL(*v, 1);
    y = IVAL(*v, 0);

    v = GetVariable(env, "COORD");
    if (v == NULL) return FAIL;

    if (EQUAL(SVAL(*v,0), "IMP")) {		/* Image plane coordinates */
      status = GetPlaneList(env, &plane, &planecnt, False);  /* only 1 plane */
      if (status != SUCCESS)
        return status;
      CursIMP2Raw(env, plane, x, y, &x, &y);
    }
    else if (EQUAL(SVAL(*v,0), "FILE")) {	/* File coordinates */
      status = GetPlaneList(env, &plane, &planecnt, False);
      if (status != SUCCESS)
        return status;
      status = CursFile2Raw(env, plane, x, y, &x, &y);
      if (status != SUCCESS)
        ABORT(FAIL, "Requested coordinates are not currently displayed.",
		 "VIDS-COORDOFF");
    }

    zdcset(env->devUnit, env->cursor.number, x, y); /* Raw falls thru to here */
  }

  v = GetVariable(env, "FORM");
  if (v == NULL) return FAIL;
  if (v->v_count != 0) env->cursor.form = IVAL(*v, 0);

  v = GetVariable(env, "BLINK");
  if (v == NULL) return FAIL;
  if (v->v_count != 0) env->cursor.blink = IVAL(*v, 0);

  v = GetVariable(env, "ONOFF");
  if (v == NULL) return FAIL;

  if (EQUAL(SVAL(*v, 0), "OFF"))
    HideCursor(env);
  else
    DisplayCursor(env);

  v = GetVariable(env, "RGB");		/* overrides COLOR keyword */
  if (v == NULL) return FAIL;
  if (v->v_count != 0)
  {
    color.red = IVAL(*v, 0);
    color.green = IVAL(*v, 1);
    color.blue = IVAL(*v, 2);
    status = zdccolor(env->devUnit, env->cursor.number,
			color.red, color.green, color.blue);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to set cursor color", "VIDS-VRDIERR");
  }
  else
  {
    v = GetVariable(env, "COLOR");
    if (v == NULL) return FAIL;
    if (v->v_count != 0)
    {
      StringToPseudo(SVAL(*v, 0), &color);
      status = zdccolor(env->devUnit, env->cursor.number,
			color.red, color.green, color.blue);
      if (status != SUCCESS)
        ABORT(FAIL, "Unable to set cursor color", "VIDS-VRDIERR");
    }
  }

  return SUCCESS;
}
/************************************************************************/
/* jset_size_do -- code for the jset-size command.  Will set the display
 * device size to use (for devices which support it).
 */
int jset_size_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  char		*degree;
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int		impnl, impns;	/* number of lines/samps per imp	*/
  int		vidnl, vidns;	/* number of lines/samps for video	*/
  int		status;		/* status holder			*/
  int		i;

  impnl = impns = vidnl = vidns = 0;

  v = GetVariable(env, "VIDEO");
  if (v == NULL) return FAIL;
  if (v->v_count >= 1) vidnl = IVAL(*v, 0);
  if (v->v_count >= 2) vidns = IVAL(*v, 1);

  v = GetVariable(env, "IMPS");
  if (v == NULL) return FAIL;
  if (v->v_count >= 1) impnl = IVAL(*v, 0);
  if (v->v_count >= 2) impns = IVAL(*v, 1);

  status = SetSize(env, vidnl, vidns, impnl, impns);
  ShowSize(env);
  for (i=0; i<MAXPLANES; i++)		/* image data maybe no longer valid */
    InvalHist(&env->planes[i]);
  return status;
}

/************************************************************************/
/* jset_color_do -- code for the JSET-COLOR command.  This routine will
 * set the graphics drawing color and mask to that specified by the user.
 */
int jset_color_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int i, count;
  GraphColor col;
  GraphColor StringToColor();
  char colorstring[STRINGSIZ+1];

  v = GetVariable(env, "COLOR");
  if (v == NULL) return FAIL;
  count = v->v_count;
  if (count > 0)		/* get color blend to use */
  {
    strcpy(colorstring,"");
    for (i=0; i<count; i++)	/* build up the color string */
    {
      strcat(colorstring, SVAL(*v, i));
      strcat(colorstring, " ");
    }
    env->drawColor = zdgcolor(env->devUnit, colorstring);
  }
  else
  {
    v = GetVariable(env, "RGB");	/* color not specified, use RGB value */
    if (v == NULL) return FAIL;
    if (v->v_count > 0)			/* must be 0 or 3 values */
    {
      env->drawColor = zdgrgb(env->devUnit, IVAL(*v,0),IVAL(*v,1),IVAL(*v,2));
    }
    else
    {
      v = GetVariable(env, "DN");	/* neither specified, use DN value */
      if (v == NULL) return FAIL;
      if (v->v_count > 0)
        env->drawColor = IVAL(*v, 0);
    }
  }

  v = GetVariable(env, "BLEND");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v, 0), "BLEND"))
    env->drawMask = env->drawColor;
  else
    env->drawMask = 0xFF;

  if (env->drawColor == zdgcolor(env->devUnit, "Transparent"))
    env->drawMask = 0xFF;	/* TRANSPARENT color needs full mask */

  return SUCCESS;
}
