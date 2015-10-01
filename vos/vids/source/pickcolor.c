/* PickColor.c contains routines related to interactively picking a color.
 */
#include "VIDSdefs.h"
#include "verrdefs.h"

#define SIN60		0.866025403
#define TAN30		0.577350269

#define BRIGHT_HEIGHT	20	/* Height of brightness scale */
#define MIN_HEX_WIDTH	50	/* Minimum width of hexagon */
				/* Both the above are somewhat arbitrary */

#define CURRENTDNMIN	0	/* Min DN to use to indicate current color */
#define CURRENTDNMAX	0	/* Max DN to use to indicate current color */
#define MINBRIGHTDN	1	/* Minimum DN to use for brightness wedge */
#define NBRIGHTDN	126	/* Number of DN's - 1 to use for bright wedge */
#define MINCOLORDN	128	/* Minimum DN to use for color hexagon	*/
#define NCOLORDN	127	/* Number of DN's - 1 to use for color hex */

struct hexagon
{
  Rect bounds;
  int height, width;
  float halfh, halfw;
  Point cur;
};

struct brightscale
{
  Rect bounds;
  int height, width;
  int cur;
};

struct zoomdwsave
{
  int line, samp;
  int zoom;
};

/* Memory to save the hex image in if save == True */

static unsigned char *saverbuf = NULL, *savegbuf = NULL;
static unsigned char *savebbuf = NULL, *savegraphbuf = NULL;

/************************************************************************/
/* PickColor allows the user to interactively choose a color using the
 * cursor.  Returns the VRDI status, or CANCEL if the user presses cancel.
 * The initial color is given by rgbcolor, which is modified to be the
 * color picked.
 * If useimage == False, the image planes are not used, and only the
 * graphics plane is written to.  In addition, a caller-supplied function
 * is called every time the color changes, so the color can be updated
 * in the appropriate places on the screen.  It is called with two
 * arguments: the environment block and a pointer to a ColorTriplet.
 * It should return a status.  Note that this function is only called if
 * useimage == False.  A NULL is allowed, in which case no updates of the
 * current color are made.
 * If useimage == True, then the image planes are saved and a color cube
 * or hexagon is drawn.  The lower half of the look-up table is used
 * for the brightness scale, while the upper half is used for the color
 * hex.  As a result, the LUTs get very messed up and the image outside
 * of the display region looks terrible.  So, the update function is
 * not called.  Instead, the current color is reflected in the border
 * around the hex.  Everything is restored on exit, including the image
 * and the LUTs.
 * If savehex == True, then a copy of the image for the hex and wedge are
 * saved for later use.  If there already exists a saved version, it is used
 * instead of regenerating the image.  This speeds up operations considerably.
 * Note, however, that you MUST call FreePickColor() after you are done
 * with all calls to PickColor in order to free the memory and for future
 * calls to work correctly.
 */
int PickColor(env, rgbcolor, rgn, useimage, drawlabels, updatefunc, savehex)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  ColorTriplet		*rgbcolor;	/* in/out: color picked		*/
  Region		*rgn;		/* Display region to use	*/
  Boolean		useimage;	/* True if ok to write in image	*/
  Boolean		drawlabels;	/* True if draw continuous RGB labels */
  int		(*updatefunc)();  	/* Pointer to color update function */
					/*   args:  (env, rgbcolor)	*/
{
  int status;
  int unit, imp;
  GraphColor color;
  int vidlines, vidsamps;
  int textheight, textwidth, size;
  int height, width;
  int isBW, isPseudo;
  Point point, pt;
  int   xLast,yLast;
  int	curs;				/* cursor number		*/
  ButtonAction	action;			/* accept,cancel, or reject	*/
  Boolean	inhex;			/* True if in hex, else in bright */
  struct brightscale brightrect;
  struct hexagon colorhex;
  CLut lut;
  unsigned char *rbuf, *gbuf, *bbuf, *graphbuf; /* bufs for saved image data */
  struct zoomdwsave rzoomdw,gzoomdw,bzoomdw,grzoomdw;	/* saves zoom and dw */
  Rect		impRect;
  Region	*disprgn;
  Region *NameToRegion(), *NextEmptyRegion();
  unsigned char *BigMalloc();

  SaveImageZoomDW(env, env->redIMP, &rzoomdw);
  SaveImageZoomDW(env, env->greenIMP, &gzoomdw);
  SaveImageZoomDW(env, env->blueIMP, &bzoomdw);
  SaveImageZoomDW(env, env->grafIMP, &grzoomdw);

  unit = env->devUnit;
  imp = env->grafIMP;

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
    "Display region crosses edge of graphics plane, so can't get color",
    "VIDS-RGNXIMP");

  color = System;

  /* Set up the text drawing routines */

  status = SystemText(env);
  if (status != SUCCESS)
    return status;
  status = zdtcolor(GC(color), 0);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text color", "VIDS-VRDIERR");
  status = zdtmask(GC(color));
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");

  textheight = SystemTextHeight(env);	/* height in pixels to use for font */

  status = zdtlength(&textwidth, 1, "R");		/* assume C is the same */
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to get text length", "VIDS-VRDIERR");

  /* Determine the size of the various display elements */

  height = disprgn->bounds.bottom - disprgn->bounds.top + 1;
  width = disprgn->bounds.right - disprgn->bounds.left + 1;

  height -= 3 * (textheight+1);	/* leave room for labels: (G,Y),(B,M),(0,255) */
  height -= BRIGHT_HEIGHT;	/* leave room for intensity scale */
  width -= 2 * (textwidth+1);	/* leave room for labels: C, R */

  colorhex.width = width;
  colorhex.height = colorhex.width * SIN60 + 0.5;	/* height of hexagon */
  if (colorhex.height > height)		/* won't fit */
  {
    colorhex.height = height;
    colorhex.width = colorhex.height / SIN60 + 0.5;	/* what will? */
  }
  if (colorhex.width <= MIN_HEX_WIDTH)
    ABORT(FAIL, "Display region is too small", "VIDS-DISPRGNSMALL");

  colorhex.bounds.top = disprgn->bounds.top + textheight + 1;
  colorhex.bounds.bottom = colorhex.bounds.top + colorhex.height - 1;
  colorhex.bounds.left = disprgn->bounds.left + textwidth + 1 +
				(width - colorhex.width) / 2;	/* center it */
  colorhex.bounds.right = colorhex.bounds.left + colorhex.width - 1;
  colorhex.halfw = ((double)colorhex.bounds.right - colorhex.bounds.left) / 2.0;
  colorhex.halfh = ((double)colorhex.bounds.bottom - colorhex.bounds.top) / 2.0;

  brightrect.width = disprgn->bounds.right - disprgn->bounds.left + 1;
  brightrect.height = BRIGHT_HEIGHT;
  brightrect.bounds.bottom = disprgn->bounds.bottom - textheight - 1;
  brightrect.bounds.top = brightrect.bounds.bottom - brightrect.height + 1;
  brightrect.bounds.left = disprgn->bounds.left;
  brightrect.bounds.right = brightrect.bounds.left + brightrect.width - 1;

  /* Set initial color */

  RGBToPoint(rgbcolor, &colorhex.cur, &brightrect.cur, &colorhex, &brightrect);

  if (useimage)			/* Save and blank out display region */
  {
    size = (disprgn->bounds.bottom - disprgn->bounds.top + 1) *
           (disprgn->bounds.right - disprgn->bounds.left + 1);
    rbuf = BigMalloc(4 * size);
    gbuf = rbuf + size;
    bbuf = gbuf + size;
    graphbuf = bbuf + size;
    if (rbuf == NULL)
    {
      BigFree(rbuf);
      ABORT(FAIL, "Not enough memory to save the image", "VIDS-INSUFMEM");
    }
    status = SaveImageRegion(env, env->redIMP, &disprgn->bounds, rbuf);
    if (status != SUCCESS) goto cleanup2;
    status = SaveImageRegion(env, env->greenIMP, &disprgn->bounds, gbuf);
    if (status != SUCCESS) goto cleanup2;
    status = SaveImageRegion(env, env->blueIMP, &disprgn->bounds, bbuf);
    if (status != SUCCESS) goto cleanup2;
    status = SaveImageRegion(env, env->grafIMP, &disprgn->bounds, graphbuf);
    if (status != SUCCESS) goto cleanup2;

    isBW = env->isBW;			/* save the mode to restore later */
    isPseudo = env->isPseudo;
    SetColorMode(env);

    UpdateBrightLut(env, &lut, &brightrect, &colorhex, rgbcolor);
    UpdateColorLut(env, &lut, &brightrect, &colorhex, rgbcolor);

    if (savehex && saverbuf != NULL && savegbuf != NULL &&
		   savebbuf != NULL && savegbuf != NULL)
    {
      zddbatch(unit, True);
      HomeZoomDW(env, env->redIMP);
      RestoreImageRegion(env, env->redIMP, &disprgn->bounds, saverbuf);
      HomeZoomDW(env, env->greenIMP);
      RestoreImageRegion(env, env->greenIMP, &disprgn->bounds, savegbuf);
      HomeZoomDW(env, env->blueIMP);
      RestoreImageRegion(env, env->blueIMP, &disprgn->bounds, savebbuf);
      HomeZoomDW(env, env->grafIMP);
      RestoreImageRegion(env, env->grafIMP, &disprgn->bounds, savegraphbuf);
      zddbatch(unit, False);
    }
    else
    {
      status = ClearImageRegion(env, env->redIMP, disprgn);
      if (status != SUCCESS) goto cleanup;
      status = ClearImageRegion(env, env->greenIMP, disprgn);
      if (status != SUCCESS) goto cleanup;
      status = ClearImageRegion(env, env->blueIMP, disprgn);
      if (status != SUCCESS) goto cleanup;
      status = ClearImageRegion(env, env->grafIMP, disprgn);
      if (status != SUCCESS) goto cleanup;

      zddbatch(unit, True);

      status = DrawColorWedge(env, &colorhex);
      if (status != SUCCESS) goto cleanup;

      status = DrawWedge(env, env->redIMP, &brightrect.bounds,
			MINBRIGHTDN, MINBRIGHTDN + NBRIGHTDN, 256);
      if (status != SUCCESS) goto cleanup;
      status = DrawWedge(env, env->greenIMP, &brightrect.bounds,
			MINBRIGHTDN, MINBRIGHTDN + NBRIGHTDN, 256);
      if (status != SUCCESS) goto cleanup;
      status = DrawWedge(env, env->blueIMP, &brightrect.bounds,
			MINBRIGHTDN, MINBRIGHTDN + NBRIGHTDN, 256);
      if (status != SUCCESS) goto cleanup;

      zddbatch(unit, False);

      /* Save the color hex image if requested to and it's not already saved */

      if (savehex && (saverbuf == NULL || savegbuf == NULL ||
		      savebbuf == NULL || savegraphbuf == NULL))
      {
        if (saverbuf != NULL) BigFree(saverbuf);
        saverbuf = BigMalloc(4 * size);
        savegbuf = saverbuf + size;
        savebbuf = savegbuf + size;
        savegraphbuf = savebbuf + size;
        if (saverbuf == NULL)
        {
          BigFree(saverbuf);
          saverbuf = savegbuf = savebbuf = savegraphbuf = NULL;
        }
        else
        {
          status = SaveImageRegion(env, env->redIMP, &disprgn->bounds, saverbuf);
          if (status == SUCCESS)
            status = SaveImageRegion(env,env->greenIMP,&disprgn->bounds,savegbuf);
          if (status == SUCCESS)
            status = SaveImageRegion(env, env->blueIMP,&disprgn->bounds,savebbuf);
          if (status == SUCCESS)
            status = SaveImageRegion(env, env->grafIMP, &disprgn->bounds,
								savegraphbuf);
          if (status != SUCCESS)
          {
            BigFree(saverbuf);
            saverbuf = savegbuf = savebbuf = savegraphbuf = NULL;
          }
        }
      }
    }
  }
  else			/* Don't use image, so draw in graphics plane */
  {
    zddbatch(unit, True);
    status = DrawPickFrame(env, color, color, &brightrect, &colorhex, textheight);
    zddbatch(unit, False);
    if (status != SUCCESS)
      goto cleanup;
    GetCurrentColor(env, &brightrect, &colorhex, rgbcolor);
    if (updatefunc != NULL)
    {
      status = (*updatefunc)(env, rgbcolor);
      if (status != SUCCESS)
        goto cleanup;
    }
  }

  status = DrawPickLabels(env, color, color, &brightrect, &colorhex, textheight);
  if (status != SUCCESS)
    goto cleanup;

  curs = env->cursor.number;

  NotifyUser(Inform,"",
          "Pick a color with hue and saturation (top) and brightness (bottom)");
  NotifyUser(Inform,"","Use button 1 to switch between color and brightness");
  NotifyUser(Inform,"","Button 2 cancels, and button 3 accepts the color");

  inhex = True;			/* toggled to False right away */
  action = ACCEPT;
  point.h = colorhex.bounds.left + colorhex.cur.h;
  point.v = colorhex.bounds.top + colorhex.cur.v;

  while (action == ACCEPT)
  {
    if (inhex)
    {
      MarkPoint(unit, imp, GC(color), GC(color), &point); /* Mark pt in hex */
      inhex = False;				/* brightness */
      point.h = brightrect.bounds.left + brightrect.cur;
      point.v = brightrect.bounds.top + brightrect.height / 2;
      MarkPoint(unit, imp, GC(color), GC(NoColor), &point); /* Unmark rect pt */
    }
    else
    {
      MarkPoint(unit, imp, GC(color), GC(color), &point); /* Mark pt in rect */
      inhex = True;
      point.h = colorhex.bounds.left + colorhex.cur.h;
      point.v = colorhex.bounds.top + colorhex.cur.v;
      MarkPoint(unit, imp, GC(color), GC(NoColor), &point); /* Unmark hex pt */
    }

    zdcset(unit, curs, point.h, point.v);

    xLast = yLast = 0;
    do
    {
      status = zdclocation(unit, curs, &pt.h, &pt.v); /* get cursor pos */
      if (status != SUCCESS) break;
      if ((pt.h == xLast) && (pt.v == yLast)) continue;
      xLast = pt.h; yLast = pt.v;

      if (drawlabels)			/* Erase old labels */
      {
        status = DrawRGBHexLabels(env, rgbcolor, &colorhex, &disprgn->bounds,
				textheight, True);
        if (status != SUCCESS)
          goto cleanup;
      }

      if (inhex)
      {
        colorhex.cur.h = pt.h - colorhex.bounds.left;
        colorhex.cur.v = pt.v - colorhex.bounds.top;
        ClipPointsToHex(&colorhex, &colorhex.cur);
        point.h = colorhex.cur.h + colorhex.bounds.left;
        point.v = colorhex.cur.v + colorhex.bounds.top;
        if (useimage)
          UpdateBrightLut(env, &lut, &brightrect, &colorhex, rgbcolor);
      }
      else
      {
        brightrect.cur = pt.h - brightrect.bounds.left;
        brightrect.cur = MIN(brightrect.cur, brightrect.width - 1);
        brightrect.cur = MAX(brightrect.cur, 0);
        point.h = brightrect.cur + brightrect.bounds.left;
        point.v = brightrect.bounds.top + brightrect.height / 2;
        if (useimage)
          UpdateColorLut(env, &lut, &brightrect, &colorhex, rgbcolor);
      }

      if (!useimage)
      {
        GetCurrentColor(env, &brightrect, &colorhex, rgbcolor);
        if (updatefunc != NULL)
        {
          status = (*updatefunc)(env, rgbcolor);
          if (status != SUCCESS)
            goto cleanup;
        }
      }

      if (drawlabels)
      {
        status = DrawRGBHexLabels(env, rgbcolor, &colorhex, &disprgn->bounds,
				textheight, False);
        if (status != SUCCESS)
          goto cleanup;
      }
    } while (!ButtonDown(env, &action));
  }

  if (action == CANCEL)
    status = CANCEL;
  else
    status = SUCCESS;

cleanup:

  if (useimage)
  {
    ResetImageZoomDW(env, env->redIMP, &rzoomdw);
    ResetImageZoomDW(env, env->greenIMP, &gzoomdw);
    ResetImageZoomDW(env, env->blueIMP, &bzoomdw);
    ResetImageZoomDW(env, env->grafIMP, &grzoomdw);

    RestoreImageRegion(env, env->redIMP, &disprgn->bounds, rbuf);
    RestoreImageRegion(env, env->greenIMP, &disprgn->bounds, gbuf);
    RestoreImageRegion(env, env->blueIMP, &disprgn->bounds, bbuf);
    RestoreImageRegion(env, env->grafIMP, &disprgn->bounds, graphbuf);

    if (isBW)
      SetBWMode(env);
    if (isPseudo)
      SetPseudoMode(env);
  }
  else
  {
    FillRegion(env, env->grafIMP, disprgn, 0xFF, 0);
  }

cleanup2:

  if (useimage)
  {
    BigFree(rbuf);
  }
  zddbatch(unit, False);

  return status;
}


/************************************************************************/
/* FreePickColor frees the image memory used to save the image of the
 * color hex.  This routine must be called after the last PickColor
 * call if the savehex parameter was set True at any time.
 */
FreePickColor()
{
  if (saverbuf != NULL) BigFree(saverbuf);
  saverbuf = savegbuf = savebbuf = savegraphbuf = NULL;
}


/************************************************************************/
/* DrawPickFrame draws a frame in the graphics plane for
 * the brightness scale and color hexagon.  Returns VRDI status.
 */
int DrawPickFrame(env, mask, color, rect, hex)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  GraphColor		mask;		/* Mask to draw with		*/
  GraphColor		color;		/* Color to draw in		*/
  struct brightscale	*rect;		/* brightness scale		*/
  struct hexagon	*hex;		/* color hexagon		*/
{
  int unit, imp;
  int status;
  int x[7], y[7];

  unit = env->devUnit;
  imp = env->grafIMP;

  /* Draw the brightness rectangle */

  status = FrameRect(unit, imp, GC(mask), GC(color), &rect->bounds);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to draw brightness frame", "VIDS-VRDIERR");

  /* Now draw the color hexagon */

  x[0] = hex->bounds.left;	y[0] = hex->bounds.top + hex->halfh;
  x[1] = x[0] + hex->halfw / 2;	y[1] = hex->bounds.top;
  x[2] = x[1] + hex->halfw;	y[2] = y[1];
  x[3] = hex->bounds.right;	y[3] = y[0];
  x[4] = x[2];			y[4] = hex->bounds.bottom;
  x[5] = x[1];			y[5] = y[4];
  x[6] = x[0];			y[6] = y[0];

  status = zdimpolyline(unit, imp, GC(mask), GC(color), 7, x, y);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to draw color hexagon frame", "VIDS-VRDIERR");

  return SUCCESS;
}


/************************************************************************/
/* DrawPickLabels draws the text labels in the graphics plane for
 * the brightness scale and color hex.  Returns VRDI status.
 */
int DrawPickLabels(env, mask, color, rect, hex, textheight)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  GraphColor		mask;		/* Mask to draw with		*/
  GraphColor		color;		/* Color to draw in		*/
  struct brightscale	*rect;		/* brightness scale		*/
  struct hexagon	*hex;		/* color hexagon		*/
  int			textheight;	/* height of text to use	*/
{
  int unit, imp;
  int status;
  int len, x, y;

  unit = env->devUnit;
  imp = env->grafIMP;

  status = zdtcolor(GC(color), 0);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text color", "VIDS-VRDIERR");
  status = zdtmask(GC(mask));
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");

  /* First draw the G and Y labels on the top */

  x = hex->bounds.left + hex->halfw / 2;	/* 'G' goes at top left */
  y = hex->bounds.top - 1;
  zdttext(unit, imp, x, y, 1, 1, "G");		/* mode 1==left just */

  x += hex->halfw;				/* 'Y' goes at top right */
  zdttext(unit, imp, x, y, 3, 1, "Y");		/* mode 3==left just */

  /* Next draw the C and R labels on the sides */

  x = hex->bounds.left - 1;				/* 'C' goes at left */
  y = hex->bounds.top + hex->halfh + textheight/2.0;
  zdttext(unit, imp, x, y, 3, 1, "C");		/* mode 1==right just */

  x = hex->bounds.right + 1;			/* 'R' goes at right */
  zdttext(unit, imp, x, y, 1, 1, "R");		/* mode 1==left just */

  /* Next draw the B and M labels on the bottom */

  x = hex->bounds.left + hex->halfw / 2;	/* 'B' goes at bottom left */
  y = hex->bounds.bottom + textheight + 1;
  zdttext(unit, imp, x, y, 1, 1, "B");		/* mode 1==left just */

  x += hex->halfw;				/* 'M' goes at bottom right */
  zdttext(unit, imp, x, y, 3, 1, "M");		/* mode 3==left just */

  /* Last draw the "0" and "255" labels for the intensity scale */

  x = rect->bounds.left;		/* 0 goes at bottom left */
  y = rect->bounds.bottom + textheight + 1;
  zdttext(unit, imp, x, y, 1, 1, "0");		/* mode 1==left just */

  x = rect->bounds.right;		/* 255 goes at bottom right */
  y = rect->bounds.bottom + textheight + 1;
  zdttext(unit, imp, x, y, 3, 3, "255");	/* mode 3==right just */

  return SUCCESS;
}

/************************************************************************/
/* DrawRGBHexLabels draws or erases the RGB value text labels in the
 * graphics plane.  Left/right position comes from 'bounds', the bounding
 * rect of the entire display region.  Top/bottom position comes from 'hex'.
 * Returns VRDI status.
 */
int DrawRGBHexLabels(env, color, hex, bounds, textheight, erase)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  ColorTriplet		*color;		/* The RGB color to print	*/
  struct hexagon	*hex;		/* Position information		*/
  Rect			*bounds;	/* More position information	*/
  int			textheight;	/* height of text to use	*/
  Boolean		erase;		/* True==erase, False==draw	*/
{
  int unit, imp;
  int status;
  int len, x, y;
  char text[10];
  GraphColor fore, back;

  unit = env->devUnit;
  imp = env->grafIMP;

  status = zdtmask(0xFF);
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to set text mask", "VIDS-VRDIERR");

  back = Black;
  if (erase)
    back = NoColor;

  /* Draw the red */

  x = bounds->right - 1;		/* Red goes at top right */
  y = hex->bounds.top;
  sprintf(text, "%d", color->red);
  len = strlen(text);
  fore = Red;
  if (erase)
    fore = NoColor;
  status = DrawText(unit, imp, x, y, 3, len, text, GC(fore), GC(back));
  if (status != SUCCESS)					/* 3=right just */
    ABORT(FAIL, "Unable to write red text", "VIDS-VRDIERR");

  /* Draw the green */

  x = bounds->left + 1;			/* Green goes at top left */
  y = hex->bounds.top;
  sprintf(text, "%d", color->green);
  len = strlen(text);
  fore = Green;
  if (erase)
    fore = NoColor;
  status = DrawText(unit, imp, x, y, 1, len, text, GC(fore), GC(back));
  if (status != SUCCESS)					/* 1=left just */
    ABORT(FAIL, "Unable to write green text", "VIDS-VRDIERR");

  /* Draw the blue */

  x = bounds->left + 1;		/* Blue goes at bottom left */
  y = hex->bounds.bottom - textheight;
  sprintf(text, "%d", color->blue);
  len = strlen(text);
  fore = Blue;
  if (erase)
    fore = NoColor;
  status = DrawText(unit, imp, x, y, 1, len, text, GC(fore), GC(back));
  if (status != SUCCESS)					/* 1=left just */
    ABORT(FAIL, "Unable to write blue text", "VIDS-VRDIERR");

  return SUCCESS;
}


/************************************************************************/
/* ClearImageRegion resets the zoom and pan to 1, and blanks out the region.
 * Returns VRDI status.
 */
int ClearImageRegion(env, imp, rgn)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int		imp;			/* Image plane to clear		*/
  Region	*rgn;			/* Region to clear		*/
{
  int status;

  status = zdizoom(env->devUnit, imp, 1);
  if (status != SUCCESS && status != MUSTZOOM)
    ABORT(FAIL, "Can't set zoom factor", "VIDS-VRDIERR");
  status = zdidwset(env->devUnit, imp, 1, 1);
  if (status != SUCCESS && status != MUSTSETDW)
    ABORT(FAIL, "Can't set display window location", "VIDS-VRDIERR");

  status = FillRegion(env, imp, rgn, 0xFF, 0);
  if (status != SUCCESS)
    return status;

  return SUCCESS;
}

/************************************************************************/
/* SaveImageZoomDW saves the current zoom and pan in the structure provided.
 * ResetImageZoomDW() will restore them.  Returns VRDI status.
 */
int SaveImageZoomDW(env, imp, save)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			imp;		/* Image plane to use		*/
  struct zoomdwsave	*save;		/* Structure holding the zoom & dw */
{
  int status;

  save->zoom = zdszoom(env->devUnit, imp);
  save->samp = zdsdwsamp(env->devUnit, imp);
  save->line = zdsdwline(env->devUnit, imp);

  return SUCCESS;
}

/************************************************************************/
/* ResetImageZoomDW resets the zoom and pan to the values saved by
 * SaveImageZoomDW().  Returns VRDI status.
 */
int ResetImageZoomDW(env, imp, save)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			imp;		/* Image plane to use		*/
  struct zoomdwsave	*save;		/* Structure holding the zoom & dw */
{
  int status;

  status = zdizoom(env->devUnit, imp, save->zoom);
  if (status != SUCCESS && status != MUSTZOOM)
    ABORT(FAIL, "Can't reset zoom factor", "VIDS-VRDIERR");
  status = zdidwset(env->devUnit, imp, save->samp, save->line);
  if (status != SUCCESS && status != MUSTSETDW)
    ABORT(FAIL, "Can't reset display window location", "VIDS-VRDIERR");

  return SUCCESS;
}

/************************************************************************/
/* HomeZoomDW resets the zoom and pan to the home values (1,1,1).
 */
int HomeZoomDW(env, imp)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			imp;		/* Image plane to use		*/
{
  int status;

  status = zdizoom(env->devUnit, imp, 1);
  status = zdidwset(env->devUnit, imp, 1, 1);

  return SUCCESS;
}

/************************************************************************/
/* DrawColorWedge draws the color wedges in the color hexagon.  For
 * each wedge, maximum intensity is at the corner with the its label,
 * and minimum is at the opposite side.
 */
int DrawColorWedge(env, hex)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  struct hexagon	*hex;		/* Hexagon to draw in		*/
{
  int hi, wi, status;
  unsigned char *rbuf, *gbuf, *bbuf;
  int right, left, size;
  int x, y;
  int unit;
  int pix, green, blue;

  unit = env->devUnit;

  rbuf = malloc(hex->bounds.right - hex->bounds.left + 1);
  gbuf = malloc(hex->bounds.right - hex->bounds.left + 1);
  bbuf = malloc(hex->bounds.right - hex->bounds.left + 1);

  if (rbuf == NULL || gbuf == NULL || bbuf == NULL)
  {
    free(rbuf);		free(gbuf);	free(bbuf);
    ABORT(FAIL, "Insufficient memory to draw color wedge", "VIDS-INSUFMEM");
  }

  for (hi = 0; hi < hex->height; hi++)	/* from the top down */
  {
    if ((double)hi <= hex->halfh)
      left = (hex->halfh - hi) * TAN30;
    else
      left = (hi - hex->halfh) * TAN30;
    right = hex->width - left;
    size = right - left + 1;

    green = (hex->height - hi) * NCOLORDN / hex->halfh;
    green = MAX(green, 0);	/* green is constant on part of scan line */
    green = MIN(green, NCOLORDN);
    green += MINCOLORDN;

    blue = hi * NCOLORDN / hex->halfh;
    blue = MAX(blue, 0);	/* blue is constant on part of scan line */
    blue = MIN(blue, NCOLORDN);
    blue += MINCOLORDN;

    for (wi = left; wi <= right; wi++)
    {
      /* Red plane first */

      pix = (wi - left) * NCOLORDN / hex->halfw;
      pix = MAX(pix, 0);
      pix = MIN(pix, NCOLORDN);
      rbuf[wi] = pix + MINCOLORDN;

      /* Green plane next */

      if ((double)wi < hex->halfw + left)
        gbuf[wi] = green;			/* constant part */
      else
      {
        pix = (hex->width - wi - (hi-hex->halfh)*TAN30) * NCOLORDN / hex->halfw;
        pix = MAX(pix, 0);
        pix = MIN(pix, NCOLORDN);
        gbuf[wi] = pix + MINCOLORDN;
      }

      /* Blue plane last */

      if ((double)wi < hex->halfw + left)
        bbuf[wi] = blue;			/* constant part */
      else
      {
        pix = (hex->width - wi - (hex->halfh-hi)*TAN30) * NCOLORDN / hex->halfw;
        pix = MAX(pix, 0);
        pix = MIN(pix, NCOLORDN);
        bbuf[wi] = pix + MINCOLORDN;
      }
    }

    x = hex->bounds.left + left;
    y = hex->bounds.top + hi;
    status = zdilinewrite(unit, env->redIMP, x, y, size, &rbuf[left]);
    if (status == SUCCESS)
      status = zdilinewrite(unit, env->greenIMP, x, y, size, &gbuf[left]);
    if (status == SUCCESS)
      status = zdilinewrite(unit, env->blueIMP, x, y, size, &bbuf[left]);
    if (status != SUCCESS)
    {
      free(rbuf);	free(gbuf);	free(bbuf);
      ABORT(FAIL, "Unable to write color wedge to image plane", "VIDS-VRDIERR");
    }
  }

  return SUCCESS;
}


/************************************************************************/
/* UpdateColorLut updates the color portion of the LUT to reflect
 * the changes in the brightness scale.
 */
int UpdateColorLut(env, lut, rect, hex, color)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  CLut			*lut;		/* The LUT to update		*/
  struct brightscale	*rect;		/* Brightness rect structure	*/
  struct hexagon	*hex;		/* Color hexagon structure	*/
  ColorTriplet		*color;		/* Out: the current color	*/
{
  double bright;
  int b;
  int i;

  bright = (double)rect->cur / (double)(rect->width-1);

  b = (int)(bright * 255);
  LinearLutRange(lut->red,   MINCOLORDN, MINCOLORDN+NCOLORDN, 0, b);
  LinearLutRange(lut->green, MINCOLORDN, MINCOLORDN+NCOLORDN, 0, b);
  LinearLutRange(lut->blue,  MINCOLORDN, MINCOLORDN+NCOLORDN, 0, b);

  PointToRGB(&hex->cur, color, hex);		/* Set current color */
  color->red *= bright;
  color->green *= bright;
  color->blue *= bright;

  for (i=CURRENTDNMIN; i<=CURRENTDNMAX; i++)
  {
    lut->red[i] = color->red;
    lut->green[i] = color->green;
    lut->blue[i] = color->blue;
  }

  zdlwrite(env->devUnit, redLUT,   1, lut->red);
  zdlwrite(env->devUnit, greenLUT, 1, lut->green);
  zdlwrite(env->devUnit, blueLUT,  1, lut->blue);

  return SUCCESS;
}


/************************************************************************/
/* UpdateBrightLut updates the brightness portion of the LUT to reflect
 * the changes in the color hex.
 */
int UpdateBrightLut(env, lut, rect, hex, color)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  CLut			*lut;		/* The LUT to update		*/
  struct brightscale	*rect;		/* Brightness rect structure	*/
  struct hexagon	*hex;		/* Color hexagon structure	*/
  ColorTriplet		*color;		/* Out: the current color	*/
{
  double bright;
  int i;

  PointToRGB(&hex->cur, color, hex);

  LinearLutRange(lut->red,   MINBRIGHTDN, MINBRIGHTDN+NBRIGHTDN,0, color->red);
  LinearLutRange(lut->green, MINBRIGHTDN, MINBRIGHTDN+NBRIGHTDN,0,color->green);
  LinearLutRange(lut->blue,  MINBRIGHTDN, MINBRIGHTDN+NBRIGHTDN,0, color->blue);

  bright = (double)rect->cur / (double)(rect->width-1);	/* Set current color */

  color->red *= bright;
  color->green *= bright;
  color->blue *= bright;

  for (i=CURRENTDNMIN; i<=CURRENTDNMAX; i++)
  {
    lut->red[i] = color->red;
    lut->green[i] = color->green;
    lut->blue[i] = color->blue;
  }

  zdlwrite(env->devUnit, redLUT,   1, lut->red);
  zdlwrite(env->devUnit, greenLUT, 1, lut->green);
  zdlwrite(env->devUnit, blueLUT,  1, lut->blue);

  return SUCCESS;
}


/************************************************************************/
/* GetCurrentColor converts the current brightness and hex position
 * to an RGB color.
 */
int GetCurrentColor(env, rect, hex, color)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  struct brightscale	*rect;		/* Brightness rect structure	*/
  struct hexagon	*hex;		/* Color hexagon structure	*/
  ColorTriplet		*color;		/* Out: the current color	*/
{
  double bright;

  PointToRGB(&hex->cur, color, hex);

  bright = (double)rect->cur / (double)(rect->width-1);

  color->red *= bright;
  color->green *= bright;
  color->blue *= bright;

  return SUCCESS;
}


/************************************************************************/
/* PointToRGB takes an X,Y position and the hex structure and returns the
 * corresponding RGB values without regard to the brightness.
 */
PointToRGB(point, color, hex)
  Point			*point;			/* Input point		*/
  ColorTriplet		*color;			/* Output color		*/
  struct hexagon	*hex;			/* Color hex		*/
{
  int hi, wi;
  int left, right, pix;

  hi = point->v;
  wi = point->h;

  if ((double)hi <= hex->halfh)
    left = (hex->halfh - hi) * TAN30;
  else
    left = (hi - hex->halfh) * TAN30;
  right = hex->width - left;

  wi = MAX(wi, left);
  wi = MIN(wi, right);

  /* Red plane first */

  pix = (wi - left) * 255 / hex->halfw;
  pix = MAX(pix, 0);
  pix = MIN(pix, 255);
  color->red = pix;

  /* Green plane next */

  if ((double)wi < hex->halfw + left)
    pix = (hex->height - hi) * 255 / hex->halfh;
  else
    pix = (hex->width - wi - (hi-hex->halfh)*TAN30) * 255 / hex->halfw;
  pix = MAX(pix, 0);
  pix = MIN(pix, 255);
  color->green = pix;

  /* Blue plane last */

  if ((double)wi < hex->halfw + left)
    pix = hi * 255 / hex->halfh;
  else
    pix = (hex->width - wi - (hex->halfh-hi)*TAN30) * 255 / hex->halfw;
  pix = MAX(pix, 0);
  pix = MIN(pix, 255);
  color->blue = pix;

}

/************************************************************************/
/* RGBToPoint takes an RGB value and the hex and rect structures and
 * returns the corresponding X,Y point and intensity point (scaled correctly
 * for the current point in both structures)
 */
RGBToPoint(color, point, value, hex, rect)
  ColorTriplet		*color;			/* Input color		*/
  Point			*point;			/* Output point		*/
  int			*value;			/* Output value (bright)*/
  struct hexagon	*hex;			/* Color hex		*/
  struct brightscale	*rect;			/* Brightness rect	*/
{
  double r, g, b;
  int bright;
  int left, right;
  int wi, hi;

  r = color->red;
  g = color->green;
  b = color->blue;

  bright = MAX(MAX(r, g), b);
  *value = (double)bright * (double)(rect->width - 1) / 255.0;

  if (bright != 0) {		/* avoid divide by 0 */
    r /= (double)bright;		/* convert to 0..1 range */
    g /= (double)bright;
    b /= (double)bright;
  }

  if ((color->red) < bright)	/* Not-constant red, so use R */
  {
    if (color->green < bright)	/* Non-constant green */
    {
      hi = hex->height - g * hex->halfh;
      left = (hi - hex->halfh) * TAN30;
    }
    else					/* Non-constant blue */
    {
      hi = b * hex->halfh;
      left = (hex->halfh - hi) * TAN30;
    }
    wi = r * hex->halfw + left;
  }
  else				/* Constant red, so must use other two colors */
  {
    wi = hex->width - (g + b) * hex->halfw / 2.0;
    hi = hex->halfh + (b - g) * hex->halfh;
  }

  point->h = wi;
  point->v = hi;

}


/************************************************************************/
/* ClipPointsToHex takes a point and clips it so it is inside the hexagon.
 */
ClipPointsToHex(hex, point)
  struct hexagon	*hex;			/* Color hex		*/
  Point			*point;			/* Point to clip 	*/
{
  int hi;
  int left, right, pix;

  point->v = MAX(point->v, 0);
  point->v = MIN(point->v, hex->height);

  hi = point->v;

  if ((double)hi <= hex->halfh)
    left = (hex->halfh - hi) * TAN30;
  else
    left = (hi - hex->halfh) * TAN30;
  right = hex->width - left;

  point->h = MAX(point->h, left);
  point->h = MIN(point->h, right);

}
