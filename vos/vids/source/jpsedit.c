#include "VIDSdefs.h"

/* JPSEDIT will edit a pseudocolor table.
 */

static int range[2];		/* range of DN's to change		*/
static PSTable *pstable;	/* The pseudocolor table to edit	*/

int jpsedit_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i,status;			/* temporary increment, status variables*/
  ColorTriplet color;		/* color to change them to		*/
  Boolean userange, usecolor;	/* True iff corresponding parm given	*/
  Boolean useimage;		/* True iff we write on the image plane	*/
  Boolean drawlabels;		/* True iff draw continuous RGB labels	*/
  Region *rregion;		/* Display region for Range		*/
  Region *cregion;		/* Display region for Color		*/
  PSTable savepstable;		/* In case the user cancels the color picker */
  PSTable origpstable;		/* In case the user cancels entire operation */
  Boolean savehex;		/* True if hex image is saved		*/
  int updateps();		/* Function to pass to pickcolor().	*/
  Boolean HasGraphics();
  PSTable *NewPSTable();

  status = GetJpseditParms(env, range, &userange, &color, &usecolor,
			&useimage, &drawlabels, &rregion, &cregion);
  if (status != SUCCESS)
    return status;

  if (!env->isPseudo)
    SetPseudoMode(env);

  pstable = NewPSTable(env, env->bwIMP);

  BlockMove(pstable, &origpstable, sizeof(PSTable));

  if (userange != True || usecolor != True)
  {
    if (HasGraphics(env) != True)
    {
      ABORT(FAIL,
	   "Graphics plane not available, so no interactive editing is allowed",
	   "VIDS-NOGRAPH");
    }
    ShowGraphics(env);
    InvalHist(&env->planes[env->grafIMP]);
  }

  savehex = False;
  if (!userange)
    savehex = True;	/* save hex image data if we call color picker twice */

  status = SUCCESS;

  while (status == SUCCESS)
  {
    if (!userange)			/* user did not give the range */
    {					/* Get the range */
      status = GetPseudoRange(env, range, rregion, useimage);
      if (status == REJECT)
      {					/* Done */
        status = SUCCESS;
        break;
      }
      if (status != SUCCESS)		/* including CANCEL */
        break;
    }

    if (usecolor != True)
    {
      BlockMove(pstable, &savepstable, sizeof(PSTable));
      i = range[0];			/* set up initial color */
      color.red = pstable->red[i];
      color.green = pstable->green[i];
      color.blue = pstable->blue[i];
      status = PickColor(env, &color, cregion, useimage, drawlabels, updateps,
			savehex);
      if (status != SUCCESS)
      {
        BlockMove(&savepstable, pstable, sizeof(PSTable));
      }
    }
    if (status == SUCCESS)
    {
      for (i=range[0]; i<=range[1]; i++)
      {
        pstable->red[i] = color.red;
        pstable->green[i] = color.green;
        pstable->blue[i] = color.blue;
      }
    }
    if (status == CANCEL && !userange)
      status = SUCCESS;		/* go to next loop on cancel in color picker */

    SendLuts(env);

    if (userange)	/* Range given on command line, so only loop once */
      break;
  }

  if (savehex)
    FreePickColor();

  if (status != SUCCESS)
  {
    BlockMove(&origpstable, pstable, sizeof(PSTable));
    SendLuts(env);
    ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
  }

  return status;
}

/************************************************************************/
/* updateps takes a color from pickcolor and updates the pseudocolor LUT.
 */
int updateps(env, color)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  ColorTriplet		*color;		/* The new color to use		*/
{
  int i;

  for (i=range[0]; i<=range[1]; i++)
  {
    pstable->red[i] = color->red;
    pstable->green[i] = color->green;
    pstable->blue[i] = color->blue;
  }

  SendLuts(env);

  return SUCCESS;
}


/************************************************************************/
/* GetJpseditParms gathers all the parameters for JPSEDIT.
 */

int GetJpseditParms(env, range, userange, color, usecolor, useimage, drawlabels,
			rregion, cregion)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		range[];	/* The DN range, if given		*/
  Boolean	*userange;	/* True iff range given			*/
  ColorTriplet	*color;		/* Color to use, if given		*/
  Boolean	*usecolor;	/* True iff color given			*/
  Boolean	*useimage;	/* True iff we should use image planes	*/
  Boolean	*drawlabels;	/* True iff draw continuous RGB labels	*/
  Region	**rregion;	/* Display region for range wedge	*/
  Region	**cregion;	/* Display region for color picker	*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int nRgns;			/* number of regions specified (1 or 0)	*/
  int i,j,status;		/* temporary increment, status variables*/
  int rgncnt;			/* number of display regions given	*/
  TAEVariable *GetVariable();

  v = GetVariable(env, "RANGE");
  if (v == NULL) return FAIL;
  if (v->v_count == 0)
    *userange = False;
  else
  {
    *userange = True;
    range[0] = IVAL(*v, 0);
    if (v->v_count == 1)		/* only one value given */
      range[1] = range[0];
    else
      range[1] = IVAL(*v, 1);
  }

  v = GetVariable(env, "RGB");		/* overrides COLOR keyword */
  if (v == NULL) return FAIL;
  if (v->v_count != 0)
  {
    *usecolor = True;
    color->red = IVAL(*v, 0);
    color->green = IVAL(*v, 1);
    color->blue = IVAL(*v, 2);
  }
  else
  {
    v = GetVariable(env, "COLOR");
    if (v == NULL) return FAIL;
    if (v->v_count != 0)
    {
      *usecolor = True;
      StringToPseudo(SVAL(*v, 0), color);
    }
    else
      *usecolor = False;
  }

  v = GetVariable(env, "IMAGE");
  if (v == NULL) return FAIL;
  *useimage = False;
  if (EQUAL(SVAL(*v, 0), "IMAGE"))
    *useimage = True;

  v = GetVariable(env, "LABELS");
  if (v == NULL) return FAIL;
  *drawlabels = False;
  if (EQUAL(SVAL(*v, 0), "LABELS"))
    *drawlabels = True;

  if (GetRegionList(env, rregion, &rgncnt, "RREGION", 1) != SUCCESS)
    return FAIL;

  if (GetRegionList(env, cregion, &rgncnt, "CREGION", 1) != SUCCESS)
    return FAIL;
}

/************************************************************************/
/* GetPseudoRange gets the range of pseudocolors to modify.
 */
int GetPseudoRange(env, range, rgn, useimage)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		range[];	/* The DN range				*/
  Region	*rgn;		/* Display region to use for wedge	*/
  Boolean	useimage;	/* True iff wedge should be drawn	*/
{
  int status, imp;
  int size;
  unsigned char *buf;			/* buf for saved image data */
  unsigned char dn;
  Point		start, end;		/* start end end points of range */
  Point		gstart, gend;		/* points in graph plane coords	*/
  int		*lut;			/* Look-up table for the plane */
  Rect		impRect;
  Region	*disprgn;
  Region	*NameToRegion(), *NextEmptyRegion();
  unsigned char *BigMalloc();
  int		*CurrentLut();

  imp = env->bwIMP;

  if (useimage)
  {
    if (rgn == NULL)			/* last-ditch default */
      rgn = NameToRegion(env, "FULLSCREEN", False);

    disprgn = NextEmptyRegion(env);
    if (disprgn == NULL)
      return FAIL;
    MarkRgnTemp(disprgn);

    status = RegionToLocal(env, imp, rgn, disprgn);
    if (status != SUCCESS)
      return status;

    SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

    if (! RectInRect(&disprgn->bounds, &impRect))
      ABORT(FAIL,
      "Display region crosses edge of graphics plane, so can't display wedge",
      "VIDS-RGNXIMP");

    /* Save and blank out display region */

    size = (disprgn->bounds.bottom - disprgn->bounds.top + 1) *
           (disprgn->bounds.right - disprgn->bounds.left + 1);
    buf = BigMalloc(size);
    if (buf == NULL)
      ABORT(FAIL, "Not enough memory to save the image", "VIDS-INSUFMEM");

    status = SaveImageRegion(env, imp, &disprgn->bounds, buf);
    if (status != SUCCESS) goto cleanup2;

    zddbatch(env->devUnit, True);
    status = DrawWedge(env, imp, &disprgn->bounds, 0, 255, 256);
    zddbatch(env->devUnit, False);
    if (status != SUCCESS) goto cleanup;
  }

  NotifyUser(Inform, "", "Select the range of pixel values to modify");
  NotifyUser(Inform, "", "by picking two points with the cursor.");

  status = GetPointButton(env, env->grafIMP, &gstart);
  if (status != SUCCESS) goto cleanup;	/* including REJECT and CANCEL */

  status = CursIMP2IMP(env, env->grafIMP, imp, gstart.h, gstart.v,
					     &start.h, &start.v);
  if (status != SUCCESS) goto cleanup;

  status = ReadPixDisplay(env, imp, start.h, start.v, &dn);
  if (status != SUCCESS) goto cleanup;

  lut = CurrentLut(env, imp);
  range[0] = lut[dn];

  status = RubberLine(env, env->grafIMP, NoOrientation, &gstart, &gend, NULL);

  if (status != SUCCESS) goto cleanup;	/* including REJECT and CANCEL */

  status = CursIMP2IMP(env, env->grafIMP, imp, gend.h, gend.v,
					     &end.h, &end.v);
  if (status != SUCCESS) goto cleanup;

  status = ReadPixDisplay(env, imp, end.h, end.v, &dn);
  if (status != SUCCESS) goto cleanup;

  if (lut[dn] < range[0])
  {
    range[1] = range[0];		/* make sure [0] <= [1] */
    range[0] = lut[dn];
  }
  else
    range[1] = lut[dn];

  status = SUCCESS;

cleanup:

  if (useimage)
    RestoreImageRegion(env, imp, &disprgn->bounds, buf);

cleanup2:

  if (useimage)
    BigFree(buf);

  return status;
}

/************************************************************************/
/* StringToPseudo converts a pseudocolor name (string) to an RGB triplet.
 * Returns SUCCESS or FAIL, but does NOT set the error message.
 */
int StringToPseudo(string, color)
  char		*string;			/* in: pseudocolor name */
  ColorTriplet	*color;				/* out: RGB triplet */
{
  int i;

  static struct colorpair
  {
    char *name;
    ColorTriplet color;
  } pairs[] =
  {
    {"RED", 	255,   0,   0},
    {"GREEN",	  0, 255,   0},
    {"BLUE",	  0,   0, 255},
    {"ORANGE",	255, 128,   0},
    {"YELLOW",	255, 255, 130},
    {"MAGENTA",	255,   0, 255},
    {"CYAN",	  0, 255, 255},
    {"PURPLE",	100,   0, 150},
    {"BROWN",	150, 100,  50},
    {"PINK",    255,  80, 120},
    {"LTGREEN",	120, 255, 120},
    {"LTBLUE",	100, 140, 255},
    {"VIOLET",	160,  80 ,255},
    {"GREY",	128, 128, 128},
    {"BLACK",	  0,   0,   0},
    {"WHITE",	255, 255, 255},
    {"AQUA",	  0, 255, 200},
    {"MELON",	255, 150, 120},
    {NULL,	  0,   0,   0}
  };

  for (i=0; pairs[i].name != NULL; i++)
  {
    if (EQUAL(string, pairs[i].name))
    {
      color->red = pairs[i].color.red;
      color->green = pairs[i].color.green;
      color->blue = pairs[i].color.blue;
      return SUCCESS;
    }
  }

  return FAIL;
}
