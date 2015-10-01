/* LUTutils.c contains utilities related to the handling of lookup tables.
 */
#include "VIDSdefs.h"
int	rampedLUT[256];			/* global copies of our luts	*/

/************************************************************************/
/* InitLuts performs a one-time intialization for the lut handling
 * routines.
 */
int InitLuts()
{
  return RampLut(rampedLUT);
}
/************************************************************************/
int InitGLuts(env)
  VIDSEnvironment	*env;
{
  int status;

  if (!HasGraphics(env))
    return SUCCESS;

  status = zdglinit(env->devUnit, 1);

  GetGrColors(env);

  return status;
}
/************************************************************************/
int GetGrColors(env)
  VIDSEnvironment	*env;
{
  int unit;

  unit = env->devUnit;

  env->graphColors[NoColor] =	zdgcolor(unit,"Transparent");
  env->graphColors[Red] =	zdgcolor(unit,"Red");
  env->graphColors[Green] =	zdgcolor(unit,"Green");
  env->graphColors[Blue] =	zdgcolor(unit,"Blue");
  env->graphColors[White] =	zdgcolor(unit,"White");
  env->graphColors[Magenta] =	zdgcolor(unit,"Magenta");
  env->graphColors[Yellow] =	zdgcolor(unit,"Yellow");
  env->graphColors[Cyan] =	zdgcolor(unit,"Cyan");
  env->graphColors[Black] =	zdgcolor(unit,"Black");

  env->graphColors[System] =	zdgcolor(unit,"White");
  env->graphColors[Rubber] =	zdgcolor(unit,"Cyan");
  env->graphColors[Special] =	zdgcolor(unit,"Transparent");
  env->graphColors[LastColor] =	zdgcolor(unit,"Transparent");

  env->drawColor = GC(White);		/* set default drawing colors */
  env->drawMask = env->drawColor;

  return SUCCESS;
}
/************************************************************************/
/* NewLut will create a pointer to a new lut associated with
 * the given image plane.  If a lut already exists, it is re-used, 
 * otherwise a new lut is allocated and ramped.
 */
int *NewLut(env, imp)
  VIDSEnvironment	*env;
  int			imp;
{
  int *lut;
  
  lut = env->planes[imp].lut;
  if (lut != NULL)
    return lut;
  lut = malloc(256 * sizeof(int));
  if (lut == NULL)
    ABORT(NULL,"Sorry, insuficient memory to modify lookup table","VIDS-INSUFMEM");
  env->planes[imp].lut = lut;
  RampLut(lut);
  return lut;
}
/************************************************************************/
/* CurrentLut will return a pointer to the current lut associated with
 * the given image plane.
 */
int *CurrentLut(env, imp)
  VIDSEnvironment	*env;
  int			imp;
{
  if (env->planes[imp].lut == NULL)
    return rampedLUT;
  return (env->planes[imp].lut);
}
/************************************************************************/
/* RampLut will ramp a 256 element lookup table.
 */
int RampLut(theLUT)
  int theLUT[256];
{
  int i;
  
  for (i = 0; i < 256; i++)
    theLUT[i] = i;
  return SUCCESS;
}
/************************************************************************/
/* LinearLut will map the numbers (low,high) to (0,255) in the given
 * lut.
 */
int LinearLut(theLUT, low, high)
  int theLUT[256];
  float low,high;
{
  register int i, j;
  float m,b;		/* slope, offset terms	*/
  
  if (high == low) low = low - .00001;
  m = 255.0 / (high - low);
  b = (-m) * low + 0.5;
  for (i = 0; i < 256; i++)
  {
    j = (int) (m * (float) i + b);
    if (j < 0) j = 0;
    if (j > 255) j = 255;
    theLUT[i] = j;
  }
  return SUCCESS;
}
/************************************************************************/
/* LinearLutRange will map the numbers (low,high) to (start,stop) in the
 * given lut.
 */
int LinearLutRange(theLUT, low, high, start, stop)
  int theLUT[256];
  int low,high;				/* input values (x-axis, LUT index) */
  int start,stop;			/* output values (y-axis, LUT values) */
{
  int temp;
  register int i, j;
  float m,b;		/* slope, offset terms	*/

  if (low > high)			/* make sure low <= high */
  {
    temp = low;    low = high;    high = temp;
    temp = start;  start = stop;  stop = temp;
  }
  if (high == low)
  {
    theLUT[low] = start;
    return SUCCESS;
  }

  m = (float)(stop - start) / (float)(high - low);
  b = (float)start - (m * (float)low) + 0.5;
  for (i = low; i <= high; i++)
  {
    j = (int) (m * (float)i + b);
    if (j < 0) j = 0;
    if (j > 255) j = 255;
    theLUT[i] = j;
  }
  return SUCCESS;
}
/************************************************************************/
/* GetGLuts will fill the supplied CLut with the current graphics
 * look-up table.
 */
int GetGLuts(env, clut)
  VIDSEnvironment *env;
  CLut *clut;
{
  zdglread(env->devUnit, 1, clut->red,
  			    clut->green,
  			    clut->blue);
  return SUCCESS;
}
/************************************************************************/
/* SendLuts will send all the active luts to the current display device.
 */
int SendLuts(env)
  VIDSEnvironment	*env;
{
  if (env->isColor)
    SetColorMode(env);
  else if (env->isBW)
    SetBWMode(env);
  else if (env->isPseudo)
    SetPseudoMode(env);
  return SUCCESS;
}
