#include "VIDSdefs.h"

#define COLPERPIX	4	/* columns needed per pixel */

/* Makes sure x is in the range 1...max */
#define BOUNDS(x,max)	(((x)<1) ? (x+max) : (((x)>(max)) ? (x-max) : (x)))

/************************************************************************/
/* jlist_do allows the user to list the DN's in a box to the terminal screen.
 */
int jlist_do(env)
  VIDSEnvironment	*env;
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int status;
  int imp, planecnt;
  Boolean stretch;		/* True iff stretch should be used	*/
  Boolean centered;		/* True if pt centered, F if upper left	*/
  Point location;		/* center point in imp coords		*/
  Point grLocation;		/* center point in graphics coords	*/
  int termheight, termwidth;	/* number of lines/cols in terminal	*/
  int pixheight, pixwidth;	/* number of pixels that fit on screen	*/
  int grheight, grwidth;	/* height & width in graphics plane coords */
  Rect bounds;
  int x[5], y[5];
  TAEVariable	*GetVariable();

  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  imp = 1;

  GetTermSize(&termheight, &termwidth);		/* get size of terminal */

  pixheight = termheight - 4;	/* 4 lines reserved for labels, etc. */
  if (pixheight <= 0)
    pixheight = 1;
  pixwidth = termwidth / COLPERPIX;	/* several columns needed per pixel */
  if (pixwidth <= 0)
    pixwidth = 1;

  status = GetPlaneList(env, &imp, &planecnt, False);   /* only 1 plane allowed */
  if (status != SUCCESS)
    return status;

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  stretch = False;
  if (EQUAL(SVAL(*v,0), "STRETCHED"))
    stretch = True;

  v = GetVariable(env, "REFERENC");
  if (v == NULL) return FAIL;
  centered = False;
  if (EQUAL(SVAL(*v,0), "CENTER"))
    centered = True;

  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  if (v->v_count == 2)			/* user gave coordinates */
  {
    location.v = IVAL(*v, 0);		/* line coord */
    location.h = IVAL(*v, 1);		/* sample coord */
    if (centered)
    {
      location.h -= (pixwidth / 2);	/* move location to upper left */
      location.v -= (pixheight / 2);
    }

    status = ListData(env, imp, &location, pixheight, pixwidth, stretch);
    if (status != SUCCESS)
      return status;
  }
  else
  {
    NotifyUser(Inform, "", "Select area to list:");

    while (FOREVER)
    {
      /* Set up the box for rubber-banding */

      status = OffsetIMP2IMP(env, imp, env->grafIMP, pixwidth, pixheight,
			   &grwidth, &grheight);
      if (status != SUCCESS)
        return status;

      if (centered)
      {
        x[0] = - (grwidth / 2);		/* location is centered in box */
        y[0] = - (grheight / 2);
      }
      else
      {
        x[0] = 0;			/* location is upper left of box */
        y[0] = 0;
      }

      x[1] = x[0] + grwidth - 1;
      y[1] = y[0];

      x[2] = x[1];
      y[2] = y[1] + grheight - 1;

      x[3] = x[0];
      y[3] = y[2];

      x[4] = x[0];
      y[4] = y[0];

      bounds.left = x[0];	bounds.top = y[0];
      bounds.right = x[2];	bounds.bottom = y[2];

      status = RubberShape(env, env->grafIMP, 5, x, y, &grLocation, &bounds);
      if (status != SUCCESS)
        return status;

      /* convert graphics plane coords from RubberShape to imp coords */

      status = CursIMP2IMP(env, env->grafIMP, imp, grLocation.h, grLocation.v,
			 &location.h, &location.v);
      if (status != SUCCESS)
        return status;

      if (centered)
      {
        location.h -= (pixwidth / 2);	/* move location to upper left */
        location.v -= (pixheight / 2);
      }

      status = ListData(env, imp, &location, pixheight, pixwidth, stretch);
      if (status != SUCCESS)
        return status;
    }

  }

  return SUCCESS;

}

/************************************************************************/
/* ListData reads the pixel data from the display at a given location and
 * lists the DN's on the terminal screen.  It draws a box in the graphics
 * plane indicating where listed area is, and erases any old boxes that
 * are still around.
 */
int ListData(env, imp, loc, height, width, stretch)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int			imp;		/* image plane to use		*/
  Point			*loc;		/* location of center of area	*/
  int			height;		/* height in pixels of area	*/
  int			width;		/* width in pixels of area	*/
  Boolean		stretch;	/* True iff stretch should be used */
{
  int status;
  int grimp;				/* graphics plane imp */
  Point center;				/* center of box in graphics coords */
  int grwidth, grheight;		/* height & width in graphics coords */
  Rect impRect;				/* size of image plane */
  Rect area;				/* area to list in imp coords */

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  grimp = env->grafIMP;

  /* Start by erasing the old box, if any */

  if (env->listData.oldvalid)		/* don't worry about failing this */
    FrameRect(env->devUnit, grimp, env->listData.oldmask, 0,
						&env->listData.oldarea);
  env->listData.oldvalid = False;

  /* Now calculate the new bounding box to use */

  status = CursIMP2IMP(env, imp, grimp, loc->h, loc->v, &center.h, &center.v);
  if (status != SUCCESS)
    return status;
  status = OffsetIMP2IMP(env, imp, grimp, width, height, &grwidth, &grheight);
  if (status != SUCCESS)
    return status;

  env->listData.oldarea.top = center.v;
  env->listData.oldarea.bottom = env->listData.oldarea.top + grheight - 1;
  env->listData.oldarea.left = center.h;
  env->listData.oldarea.right = env->listData.oldarea.left + grwidth - 1;

  if (! RectInRect(&env->listData.oldarea, &impRect))
    NotifyUser(Inform, "",
        "Bounding box not displayed because it crosses edge of graphics plane");
  else
  {
    FrameRect(env->devUnit, grimp, GC(System), GC(System),
						&env->listData.oldarea);
    env->listData.oldvalid = True;
    env->listData.oldmask = GC(System);
  }

  /* Now, finally, prepare to list the region */

  area.top = loc->v;
  area.bottom = area.top + height - 1;
  area.left = loc->h;
  area.right = area.left + width - 1;

  status = ListDataArea(env, imp, &area, stretch);	/* do the actual list */
  if (status != SUCCESS)
    return status;

  return SUCCESS;
}


/************************************************************************/
/* ListDataArea takes a rectangle on the display and actually lists all the
 * values from it onto the terminal.
 */
int ListDataArea(env, imp, area, stretch)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int			imp;		/* image plane to use		*/
  Rect			*area;		/* area to be listed		*/
  Boolean		stretch;	/* True iff stretch should be used */
{
  int status;
  char buf[STRINGSIZ+1];
  char leftstr[20], midstr[20], rightstr[20], temp[20];
  unsigned char value;
  int line, samp;
  int l, s;
  int w;
  int *lut;
  Rect impRect;				/* size of image plane */
  int *CurrentLut();

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);

  if (stretch)
    lut = CurrentLut(env, imp);

  w = (area->right - area->left + 1) * COLPERPIX - 1;	/* width of terminal */

  if (! RectInRect(area, &impRect))
    NotifyUser(Inform, "",
        "The area wraps around an image plane edge for plane %d", imp);
  else
    NotifyUser(Inform, "", "List of data for plane %d", imp);

  BlockFill(' ', buf, w);		/* blank out line */
  buf[w] = '\0';

  /* Construct a header of the form "<- Sample 5      Line 4    Sample 20 ->" */
  /* with the three pieces left, center, and right justified in the line.     */

  sprintf(leftstr, "<- Sample %d", BOUNDS(area->left,env->nsMax));
  sprintf(rightstr, "Sample %d ->", BOUNDS(area->right,env->nsMax));
  sprintf(midstr, "Line %d", BOUNDS(area->top,env->nlMax));

  if (strlen(leftstr) + strlen(midstr) + strlen(rightstr) + 2 <= w)
  {						/* it all fits on the line */
    BlockMove(leftstr, buf, strlen(leftstr));
    strcpy(&buf[w-strlen(rightstr)], rightstr);
    BlockMove(midstr, &buf[(w-strlen(midstr)) / 2], strlen(midstr));
  }
  else
    sprintf(buf, "Samp %d  Line %d  Samp %d",	/* doesn't fit, so fake it */
		BOUNDS(area->left, env->nsMax),
		BOUNDS(area->top, env->nlMax),
		BOUNDS(area->right, env->nsMax));

  NotifyUser(Inform, "", buf);

  for (l = area->top; l <= area->bottom; l++)
  {
    line = BOUNDS(l, env->nlMax);	/* make sure it's in range */

    buf[0] = '\0';
    for (s = area->left; s <= area->right; s++)
    {
      samp = BOUNDS(s, env->nsMax);	/* make sure it's in range */

      status = zdipixelread(env->devUnit, imp, samp, line, &value);
      if (status != SUCCESS)
        ABORT(FAIL, "Can't read the pixel from the display", "VIDS-VRDIERR");

      if (stretch)
        value = lut[value];		/* pipe it through the LUT */

      sprintf(temp, "%3d", value);
      strcat(buf, temp);

      if (s != area->right)		/* Append the separator char */
      {					/* (but not after last char on line) */
        if (((l - area->top + 1) % 5) == 0)
        {
          if (((s - area->left + 1) % 5) == 0)
            strcat(buf, "+");
          else
            strcat(buf, "-");
        }
        else
        {
          if (((s - area->left + 1) % 5) == 0)
            strcat(buf, "|");
          else
            strcat(buf, " ");
        }
      }
    }	/* end samp loop */

    NotifyUser(Inform, "", buf);

  }	/* end line loop */

  /* Construct a trailer like the header above */

  BlockFill(' ', buf, w);		/* blank out line */
  buf[w] = '\0';

  sprintf(midstr, "Line %d", BOUNDS(area->bottom, env->nsMax));

  if (strlen(leftstr) + strlen(midstr) + strlen(rightstr) + 2 <= w)
  {						/* it all fits on the line */
    BlockMove(leftstr, buf, strlen(leftstr));
    strcpy(&buf[w-strlen(rightstr)], rightstr);
    BlockMove(midstr, &buf[(w-strlen(midstr)) / 2], strlen(midstr));
  }
  else
    sprintf(buf, "Samp %d  Line %d  Samp %d",	/* doesn't fit, so fake it */
		BOUNDS(area->left, env->nsMax),
		BOUNDS(area->bottom, env->nlMax),
		BOUNDS(area->right, env->nsMax));

  NotifyUser(Inform, "", buf);

  return SUCCESS;

}
