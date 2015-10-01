#include "VIDSdefs.h"

#include <math.h>
#include <ctype.h>

#define PI 3.141592654
#define	ROUND( x )	((int) (x < 0 ? (x-0.5) : (x+0.5)))

#define TSIZE	7		/* default height of text */

TAEVariable *GetVariable();

/* Current text state.  Note that in the VRDI, all units share the same
 * font info.  But, the font info is process-private, i.e. not in shared
 * memory, so we don't have to worry about the application changing it.
 */

static struct TextAttr CurrentAttr = {0, -1, 1.0, 0.0};

/************************************************************************/
/* jtext_do allows the user to write text on the graphics plane.
 */
int jtext_do(env)
  VIDSEnvironment	*env;
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int status;
  char *original;			/* original input string	*/
  char parsed[STRINGSIZ+1];		/* parsed output string		*/
  int nchars;				/* length of parsed output str	*/
  int flag;				/* orientation flag for zdttext	*/
  Point location;			/* location to draw		*/
  int x[5], y[5];			/* box coords for rubber band	*/
  double radian;
  int length;

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  v = GetVariable(env, "TEXT");
  if (v == NULL) return FAIL;
  original = SVAL(*v,0);

  status = UserText(env);
  if (status != SUCCESS)
    return status;

  ParseJTextString(original, parsed, &nchars);

  v = GetVariable(env, "ORIENT");
  if (v == NULL) return FAIL;
  flag = 1;				/* lower left */
  if (EQUAL(SVAL(*v,0), "CENTER"))
    flag = 2;				/* bottom center */
  else if (EQUAL(SVAL(*v,0), "RIGHT"))
    flag = 3;				/* lower right */

  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  if (v->v_count == 2)			/* user gave coordinates */
  {
    location.v = IVAL(*v, 0);		/* line coord */
    location.h = IVAL(*v, 1);		/* sample coord */
  }
  else
  {
    NotifyUser(Inform, "", "Select point at which to place the text:");
/*    status = GetPoint(env, env->grafIMP, &location);  */

    /* Set up the box for rubber-banding */

    zdtlength(&length, nchars, parsed);		/* get length in pix of text */
    radian = env->UserAttr.angle * PI / 180.0;

    switch (flag)
    {
      case 1:
        x[0] = 0;
        y[0] = 0;
        break;
      case 2:
        x[0] = - ROUND(cos(radian) * length / 2);
        y[0] = ROUND(sin(radian) * length / 2);
        break;
      case 3:
        x[0] = - ROUND(cos(radian) * length);
        y[0] = ROUND(sin(radian) * length);
        break;
    }

    x[1] = x[0] + ROUND(cos(radian) * (length-1));
    y[1] = y[0] - ROUND(sin(radian) * (length-1));

    x[2] = x[1] - ROUND(sin(radian) * (env->UserAttr.size-1));
    y[2] = y[1] - ROUND(cos(radian) * (env->UserAttr.size-1));

    x[3] = x[0] - ROUND(sin(radian) * (env->UserAttr.size-1));
    y[3] = y[0] - ROUND(cos(radian) * (env->UserAttr.size-1));

    x[4] = x[0];
    y[4] = y[0];

    status = RubberShape(env, env->grafIMP, 5, x, y, &location, NULL);

    if (status != SUCCESS)
      return status;
  }

  status = zdtcolor(env->drawColor, 0);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text color", "VIDS-VRDIERR");

  status = zdtmask(env->drawMask);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");

  /* Don't check status because we may get some invalid characters */

  zdttext(env->devUnit, env->grafIMP, location.h, location.v, flag,
	  nchars, parsed);

  return SUCCESS;

}

/************************************************************************/
/* ParseJTextString copies the original string to the parsed string,
 * interpreting escape characters as it goes.  Escape characters are
 * a backslash followed by a one-, two-, or three-digit number.  The
 * given number is put into the text as a character, allowing the user
 * to specify the funny characters in the VRDI fonts.  If more than three
 * numbers follow the backslash, the extra numbers are treated as normal
 * ASCII characters, so you can say for example "\042234" to get a funny
 * character (42) followed by the ascii string "234"
 */
ParseJTextString(original, parsed, len)
  char *original;			/* input string */
  char *parsed;				/* output string */
  int *len;				/* output string length */
{
  int val;

  *len = 0;

  while (*original != '\0')
  {
    if (*original != '\\')
      parsed[(*len)++] = *original++;
    else				/* backslash, look for digits */
    {
      val = 0;
      original++;			/* skip \ */
      if (*original == '\\')		/* two \'s in a row, so use one */
        parsed[(*len)++] = *original++;
      else
      {
        if (isdigit(*original))			/* first digit */
        {
          val = val*10 + (*original - '0');
          original++;			/* skip first */
          if (isdigit(*original))		/* second digit */
          {
            val = val*10 + (*original - '0');
            original++;			/* skip second */
            if (isdigit(*original))		/* third digit */
            {
              val = val*10 + (*original - '0');
              original++;		/* skip third */
            }
          }
        }
        parsed[(*len)++] = val;
      }	/* end else */
    }	/* end else */
  }	/* end while */

  return;
}

/************************************************************************/
/* jset_text_do will allow the user to set the current text attributes
 * for either user-written text or system-written text.
 */
int jset_text_do(env)
  VIDSEnvironment	*env;
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int status;

  v = GetVariable(env, "USER");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v,0), "USER"))
    status = GetJSetTextParms(env, &env->UserAttr);
  else
  {
    status = GetJSetTextParms(env, &env->SystemAttr);
    if (env->SystemAttr.angle != 0.0)
    {
      env->SystemAttr.angle = 0.0;
      ABORT(FAIL, "Sorry, system text can not be rotated", "VIDS-NOROTATE");
    }
  }
  return status;
}

/************************************************************************/
/* GetJSetTextParms gets the parameters for JSET-TEXT into the appropriate
 * TextAttr structure.
 */
int GetJSetTextParms(env,attr)
  VIDSEnvironment	*env;
  TextAttr		*attr;
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int status;

  v = GetVariable(env, "FONT");
  if (v == NULL) return FAIL;
  if (v->v_count > 0)			/* there's a value present */
    attr->font = IVAL(*v, 0);		/* error checked later */

  v = GetVariable(env, "SIZE");
  if (v == NULL) return FAIL;
  if (v->v_count > 0)			/* value present */
  {
    if (IVAL(*v, 0) <= 0)
      ABORT(FAIL, "The SIZE parameter must be greater than zero", "VIDS-NOTPOS");
    attr->size = IVAL(*v, 0);
  }

  v = GetVariable(env, "ANGLE");
  if (v == NULL) return FAIL;
  if (v->v_count > 0)			/* value present */
  {
    attr->angle = RVAL(*v, 0);
  }

  v = GetVariable(env, "ASPECT");
  if (v == NULL) return FAIL;
  if (v->v_count > 0)			/* value present */
  {
    if (RVAL(*v, 0) <= 0.0)
      ABORT(FAIL, "The ASPECT parameter must be greater than zero", "VIDS-NOTPOS");
    attr->aspect = RVAL(*v, 0);
  }

  return SUCCESS;
}

/************************************************************************/
/* SystemTextHeight returns the current height in pixels of the system text.
 */
int SystemTextHeight(env)
  VIDSEnvironment	*env;
{
  if (env->SystemAttr.size == -1)	/* it hasn't been set yet */
  {
    env->SystemAttr.size = TSIZE;	/* use default height */
    if (VideoLines(env) > 512)
      env->SystemAttr.size *= 2;	/* double height for hi-res display */
  }

  return env->SystemAttr.size;
}

/************************************************************************/
/* SystemText sets up the system text attributes for use.
 */
int SystemText(env)
  VIDSEnvironment	*env;
{

  return SetTextAttr(env, &env->SystemAttr);
}

/************************************************************************/
/* UserText sets up the user's text attributes for use.
 */
int UserText(env)
  VIDSEnvironment	*env;
{

  return SetTextAttr(env, &env->UserAttr);
}

/************************************************************************/
/* SetTextAttr sets the VRDI and the CurrentText structure the the text
 * attributes requested by the input TextAttr struct.
 */
int SetTextAttr(env,attr)
  VIDSEnvironment	*env;
  TextAttr		*attr;
{
  int status;

  if (attr->font != CurrentAttr.font)
  {
    NotifyUser(Verbose, "", "Loading font number %d.", attr->font);
    status = zdtfont(attr->font);
    CurrentAttr.font = attr->font;
    if (status != SUCCESS)
    {
      zdtfont(0);
      CurrentAttr.font = 0;
      ABORT(FAIL, "Can't set up the requested font", "VIDS-VRDIERR");
    }
  }

  if (attr->size == -1)			/* it hasn't been set yet */
  {
    attr->size = TSIZE;			/* use default height */
    if (VideoLines(env) > 512)
      attr->size *= 2;			/* double height for hi-res display */
  }

  if (attr->size != CurrentAttr.size ||
		attr->aspect != CurrentAttr.aspect)
  {
    status = zdtsize(attr->size, attr->aspect);	/* aspect should be real*4! */
    CurrentAttr.size = attr->size;
    CurrentAttr.aspect = attr->aspect;
    if (status != SUCCESS)
    {
      CurrentAttr.size = TSIZE;		/* use default height */
      if (VideoLines(env) > 512)
        CurrentAttr.size *= 2;		/* double height for hi-res display */
      zdtsize(CurrentAttr.size, 1.0);
      CurrentAttr.aspect = 1.0;
      ABORT(FAIL, "Can't set up the requested text size or aspect ratio", "VIDS-VRDIERR");
    }
  }

  if (attr->angle != CurrentAttr.angle)
  {
    status = zdtrotate(attr->angle);	/* angle should be real*4! */
    CurrentAttr.angle = attr->angle;
    if (status != SUCCESS)
    {
      zdtrotate(0.0);
      CurrentAttr.angle = 0.0;
      ABORT(FAIL, "Can't set up the requested text rotation", "VIDS-VRDIERR");
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* RotateText sets an explicit one-time-only text rotation.  Cleared on the
 * next SetTextAttr() call.
 */
int RotateText(env, angle)
  VIDSEnvironment	*env;
  float		angle;
{
  int status;

  status = zdtrotate(angle);		/* angle should be real*4! */
  CurrentAttr.angle = angle;

  return status;
}

/************************************************************************/
/* Initialize a single text attribute structure.
 */
int InitTextAttr(attr)
  TextAttr	*attr;
{
  attr->font = 0;
  attr->size = -1;
  attr->aspect = 1.0;
  attr->angle = 0.0;

  return SUCCESS;
}
