/* Poke.c contains the code for the "poking" routines; the routines used 
 * to interactively get the user to poke points, drag regions, etc. on 
 * the display device.
 * If the color is defined "GraphColor", then it is one of the graphics
 * color indices.  If it is defined "int", it is the actual DN value to use.
 * Generally, if "env" is passed, then GraphColor is used.  If "env" is
 * not passed, the DN is used.
 */
#include "VIDSdefs.h"

/************************************************************************/
int PokeSquare(env, imp, color, points)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: diametrically opposite 2 points	*/
{
  NotifyUser(Inform,"","Position the cursor on one corner of the square.");
  if (GetPoint(env, imp, points) != SUCCESS) return FAIL;
  NotifyUser(Inform,"","Now position the cursor on the opposite corner.");
  if (ExpandRect(env,imp,True,&points[0],&points[1]) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
int PokeRect(env, imp, color, points)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: diametrically opposite 2 points	*/
{
  NotifyUser(Inform,"","Position the cursor on one corner of the rectangle.");
  if (GetPoint(env, imp, points) != SUCCESS) return FAIL;
  NotifyUser(Inform,"","Now position the cursor on the opposite corner.");
  if (ExpandRect(env,imp,False,&points[0],&points[1]) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
int PokeCircle(env, imp, color, points)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: diametrically opposite 2 points	*/
{
  NotifyUser(Inform,"","Position cursor on one corner of bounding square");
  if (GetPoint(env, imp, points) != SUCCESS) return FAIL;
  NotifyUser(Inform,"","Now position cursor on the opposite corner");
  if (ExpandOval(env, imp, True, &points[0], &points[1]) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
int PokeOval(env, imp, color, points)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: diametrically opposite 2 points	*/
{
  NotifyUser(Inform,"","Position cursor on one corner of bounding rectangle");
  if (GetPoint(env, imp, points) != SUCCESS) return FAIL;
  NotifyUser(Inform,"","Now position cursor on the opposite corner");
  if (ExpandOval(env, imp, False, &points[0], &points[1]) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
int PokePoly(env, imp, color, points, nPoints)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: list of points in polygon	*/
  int		*nPoints;	/* in/out: number of points in polygon	*/
{
  int		maxPts,nPts,unit,status;
  Point		thePt;
  ButtonAction	action;
  
  maxPts = *nPoints;		/* maximum number of points allowed	*/
  if (maxPts < 2) ABORT(FAIL, "Must have room for at least two points for a polygon",
  			"VIDS-TWOPOINTS");
  
  status = PokeLineSegs(env,imp,color,points,nPoints);

  return status;
}
/************************************************************************/
/* PokeLineSegs will allow the user to select a connected series of 
 * lines (a non-closed polygon).
 */
int PokeLineSegs(env, imp, color, points, nPoints)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: list of points in polygon	*/
  int		*nPoints;	/* in/out: number of points in polygon	*/
{
  int		maxPts,nPts,unit,status;
  Point		thePt;
  ButtonAction	action;

  unit = env->devUnit;
  maxPts = *nPoints;		/* maximum number of points allowed	*/
  if (maxPts < 2) ABORT(FAIL, "Must have room for at least two points for a line segment",
  			"VIDS-TWOPOINTS");

  NotifyUser(Inform,"","Select the first point:");
  if (GetPoint(env, imp, &thePt) != SUCCESS) return FAIL;
  points[0].h = thePt.h; points[0].v = thePt.v;

  do
  {
    NotifyUser(Inform,"","Select the next point to connect to");
    status = RubberLine(env,imp,NoOrientation,&points[0],&thePt,NULL);
    if (status == REJECT)
      NotifyUser(Inform,"","You must define at least one more point or cancel");
  } while (status == REJECT);
  if (status != SUCCESS) return FAIL;
  DrawLine(unit, imp, GC(color), GC(color), points, &thePt);
  points[1].h = thePt.h; points[1].v = thePt.v;
  
  NotifyUser(Inform,"","Select the next point to connect to");
  for (nPts = 2; nPts < maxPts;)
  {
    status = RubberLine(env, imp, NoOrientation, &points[nPts - 1], &thePt, NULL);
    if (status == REJECT) break;
    if (status != SUCCESS)
    {
      FramePoly(unit,imp,GC(color),GC(NoColor),points,nPts);	/* erase poly */
      return FAIL;
    }
    if (EqualPoint(&thePt, &points[nPts - 1])) continue;
    points[nPts].h = thePt.h; points[nPts].v = thePt.v;
    DrawLine(unit, imp, GC(color), GC(color),	/* Burn in line to prevent  */
             &points[nPts - 1], &thePt);	/* erasing with rubber band */
    nPts++;
  }
  if (status != REJECT)
    NotifyUser(Inform,"","Out of points; line terminated prematurely");
  *nPoints = nPts;
  return SUCCESS;
}
/************************************************************************/
/* PokeLine will allow the user to select a single line.
 */
int PokeLine(env, imp, color, points)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  GraphColor	color;		/* color in which to draw		*/
  Point		*points;	/* out: the two endpoints as an array	*/
{
  int		maxPts,nPts,unit,status;
  Point		thePt;
  ButtonAction	action;

  unit = env->devUnit;

  NotifyUser(Inform,"","Select the first point:");
  if (GetPoint(env, imp, &thePt) != SUCCESS) return FAIL;
  points[0].h = thePt.h; points[0].v = thePt.v;

  do
  {
    NotifyUser(Inform,"","Select the point to connect to:");
    status = RubberLine(env,imp,NoOrientation,&points[0],&thePt,NULL);
    if (status == REJECT)
      NotifyUser(Inform,"","You must define one more point or cancel");
  } while (status == REJECT);
  if (status != SUCCESS) return FAIL;
  points[1].h = thePt.h; points[1].v = thePt.v;
  
  return SUCCESS;
}
/************************************************************************/
/* GetPoint will get a location on the display device relative to the
 * image memory plane imp.  This routine is exactly like GetPointButton
 * except only two buttons (Accept and Cancel) are active, and Cancel
 * is returned as a status of FAIL.
 */
int GetPoint(env, imp, thePt)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  Point			*thePt;	/* out: point indicated by user		*/
{
  int status,hasAuto,x,y,unit;
  Point	rawPoint,impPoint;
  ButtonAction	action;

  unit = env->devUnit;
  zddinfo(unit, 51, 1, &hasAuto);
  if (hasAuto)
  {
    status = zdcautotrack(unit, 1, 1, TRUE);	/* just in case		*/
    if (status != SUCCESS) 
      ABORT(FAIL, "Unable to turn on auto-tracking of the cursor", 
            "VIDS-VRDIERR");
    ButtonMessage(env, True, True, False);
    status = WaitButton(env, True, True, False, &rawPoint, &action);
    if (status != SUCCESS)
      ABORT(FAIL, "Sorry, could not get that point", "VIDS-VRDIERR");
    if (action == CANCEL)
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
  }
  else
    ABORT(FAIL,"Currently only implemented for autotracking devices","VIDS-NOTRACK");

  CursRaw2IMP(env, imp, rawPoint.h, rawPoint.v,
              &impPoint.h, &impPoint.v);
  thePt->h = impPoint.h;
  thePt->v = impPoint.v;
  return SUCCESS;
}
/************************************************************************/
/* GetPointButton will get a location on the display device relative to the
 * image memory plane imp.  This routine is exactly like GetPoint, except
 * that three buttons (Accept, Cancel, Terminate) are active instead of
 * only two.
 */
int GetPointButton(env, imp, thePt)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int			imp;	/* The plane in which to poke		*/
  Point			*thePt;	/* out: point indicated by user		*/
{
  int status,hasAuto,x,y,unit;
  Point	rawPoint,impPoint;
  ButtonAction		action;
  
  unit = env->devUnit;
  zddinfo(unit, 51, 1, &hasAuto);
  if (hasAuto)
  {
    status = zdcautotrack(unit, 1, 1, TRUE);	/* just in case		*/
    if (status != SUCCESS) 
      ABORT(FAIL, "Unable to turn on auto-tracking of the cursor", 
            "VIDS-VRDIERR");
    ButtonMessage(env, True, True, True);
    status = WaitButton(env, True, True, True, &rawPoint, &action);
    if (status != SUCCESS)
      ABORT(FAIL, "Sorry, could not get that point", "VIDS-VRDIERR");
  }
  else
    ABORT(FAIL,"Currently only implemented for autotracking devices","VIDS-NOTRACK");

  CursRaw2IMP(env, imp, rawPoint.h, rawPoint.v,
              &impPoint.h, &impPoint.v);
  thePt->h = impPoint.h;
  thePt->v = impPoint.v;
  switch (action)
  {
    case CANCEL:
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
    case ACCEPT:
      return SUCCESS;
    case REJECT:
      return REJECT;
  }
  return SUCCESS;		/* shouldn't get called */
}
/************************************************************************/
/* WaitButton will wait for a button to be pressed, and return the point
 * at which the cursor lies, and the button which was pressed: ACCEPT,
 * CANCEL, or REJECT.  Returns SUCCESS or FAIL.
 */
int WaitButton(env, acceptFlag, cancelFlag, rejectFlag, thePt, action)
  VIDSEnvironment	*env;		/* the vids environment		*/
  Boolean		acceptFlag;	/* True if ACCEPT button ok	*/
  Boolean		cancelFlag;	/* True if CANCEL button ok	*/
  Boolean		rejectFlag;	/* True if REJECT button ok	*/
  Point			*thePt;		/* out: cursor position		*/
  ButtonAction		*action;	/* out: which button pressed	*/
{
  Boolean ButtonDown();

				/* prevent accidental infinite loop!	*/
  if (!acceptFlag && !cancelFlag && !rejectFlag)
           ABORT(FAIL, "WaitButton: Must have a button to wait for",
                 "VIDS-NOBUTTON");
  while (True)				/* loop until button pressed */
  {
    if (ButtonDown(env, action))
    {
      if ((int)acceptFlag && (*action == ACCEPT)) break;
      if ((int)cancelFlag && (*action == CANCEL)) break;
      if ((int)rejectFlag && (*action == REJECT)) break;
    }
  }
  
  if (zdclocation(env->devUnit, env->cursor.number,
                  &thePt->h, &thePt->v) != SUCCESS)
      ABORT(FAIL,"Unable to read the cursor location","VIDS-VRDIERR");
  return SUCCESS;
}
/************************************************************************/
/* RubberLine will "rubber-band" a line, having one end tied to the point
 * startPt, and the other end to the cursor, until a button is pressed.
 * Returns the position upon button press in endPt.  The line is erased
 * on exit.
 */
/* Solaris 8 apparently makes "restrict" a keyword, thus the wierd spelling */
int RubberLine(env, imp, restrictt, startPt, endPt, limits)
  VIDSEnvironment	*env;		/* in: VIDS environment		*/
  int			imp;		/* in: plane in which to animate*/
  Orientation		restrictt;	/* in: horiz, vert, or none	*/
  Point			*startPt;	/* in: start point of rectangle	*/
  Point			*endPt;		/* out: determined opposite pt	*/
  Rect			*limits;	/* in: bounding rect for lines	*/
					/*     if restrictt==Limit	*/
{
  int x[2], y[2], x0, y0, x1, y1, unit, curs, status;
  ButtonAction action;
  Boolean ButtonDown();

  unit = env->devUnit;
  curs = env->cursor.number;
  x[0] = startPt->h;
  y[0] = startPt->v;
  ButtonMessage(env, True, True, True);       /* tell user what to hit	*/

  status = zdclocation(unit, curs, &x0, &y0);
  if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
  CursRaw2IMP(env, imp, x0, y0, &x0, &y0);
  if (restrictt == Horizontal)
    y0 = startPt->v;
  else if (restrictt == Vertical)
    x0 = startPt->h;
  else if (restrictt == Limit)
  {
    x0 = MIN(x0, limits->right);
    x0 = MAX(x0, limits->left);
    y0 = MIN(y0, limits->bottom);
    y0 = MAX(y0, limits->top);
  }

/* Now draw the initial line before the loop begins */
  x[1] = x0; y[1] = y0;
  status = zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), 2, x, y);
  if (status != SUCCESS)
    ABORT(FAIL, "Could not update line to cursor", "VIDS-VRDIERR");
  while (!ButtonDown(env, &action))		/* loop until user	*/
  {						/* presses a button	*/
    status = zdclocation(unit, curs, &x1, &y1);
    if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
    CursRaw2IMP(env, imp, x1, y1, &x1, &y1);
    if (restrictt == Horizontal)
      y1 = startPt->v;
    else if (restrictt == Vertical)
      x1 = startPt->h;
    else if (restrictt == Limit)
    {
      x1 = MIN(x1, limits->right);
      x1 = MAX(x1, limits->left);
      y1 = MIN(y1, limits->bottom);
      y1 = MAX(y1, limits->top);
    }
    if ((x1 == x0) && (y1 == y0)) continue;
    zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), 2, x, y); /* blank old */
    x[1] = x0 = x1; y[1] = y0 = y1;
    zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), 2, x, y); /* draw new line*/
  }
  zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), 2, x, y); /* erase */
  switch (action)
  {
    case CANCEL :
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
    case ACCEPT :
      endPt->h = x0;
      endPt->v = y0;
      return SUCCESS;
    case REJECT :
      endPt->h = x0;
      endPt->v = y0;
      return REJECT;
  }
  return FAIL;	/* Impossible case */
}

/************************************************************************/
/* RubberShape will "rubber-band" a fixed-size shape that moves with the
 * cursor.  The number and list of points for the shape vertices are passed
 * in, with a coordinate of 0 being the cursor position.  The position is
 * limited so the bounding rectangle is always on the screen, unless
 * 'bounds' is NULL, in which case there is no restriction.
 * Returns the position upon button press in thePt.  The shape is erased
 * on exit.
 */
int RubberShape(env, imp, n, xin, yin, thePt, bounds)
  VIDSEnvironment	*env;		/* in: VIDS environment		*/
  int			imp;		/* in: plane in which to animate*/
  int			n;		/* in: number of vertices	*/
  int			*xin;		/* in: array of X-coordinates	*/
  int			*yin;		/* in: array of Y-coordinates	*/
  Point			*thePt;		/* out: cursor position at end	*/
  Rect			*bounds;	/* in: bounding rect or NULL	*/
{
  int unit, curs, status, i;
  int hasAuto;
  int x, y;			/* cursor position */
  int xold, yold;		/* old cursor position */
  int *xout, *yout;		/* x & y coords after translation */
  ButtonAction action;
  Boolean ButtonDown();

  unit = env->devUnit;
  curs = env->cursor.number;

  zddinfo(unit, 51, 1, &hasAuto);
  if (!hasAuto)
    ABORT(FAIL,"Currently only implemented for autotracking devices","VIDS-NOTRACK");

  status = zdcautotrack(unit, curs, 1, TRUE);	/* just in case */
  if (status != SUCCESS) 
    ABORT(FAIL, "Unable to turn on auto-tracking of the cursor", 
          "VIDS-VRDIERR");

  xout = malloc(n * sizeof(int));
  if (xout == NULL)
    ABORT(FAIL, "Insufficient memory for X in RubberShape().", "VIDS-INSUFMEM");
  yout = malloc(n * sizeof(int));
  if (yout == NULL)
  {
    free(xout);
    ABORT(FAIL, "Insufficient memory for Y in RubberShape().", "VIDS-INSUFMEM");
  }

  ButtonMessage(env, True, True, False);       /* tell user what to hit	*/

  status = zdclocation(unit, curs, &x, &y);
  if (status != SUCCESS)
  {
    free(xout); free(yout);
    ABORT(FAIL, "Unable to get cursor location", "VIDS-VRDIERR");
  }
  CursRaw2IMP(env, imp, x, y, &x, &y);
  if (bounds != NULL)
  {
    if (bounds->bottom + y > env->nlMax)
      y = env->nlMax - bounds->bottom;
    if (bounds->right + x > env->nsMax)
      x = env->nsMax - bounds->right;
    if (bounds->top + y <= 0)
      y = 1 - bounds->top;
    if (bounds->left + x <= 0)
      x = 1 - bounds->left;
  }
  xold = x;
  yold = y;

  for (i=0; i<n; i++)
  {
    xout[i] = xin[i] + x;
    yout[i] = yin[i] + y;
  }

/* Now draw the initial shape before the loop begins */
  status = zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), n, xout, yout);
  if (status != SUCCESS)
  {
    free(xout); free(yout);
    ABORT(FAIL, "Could not draw shape to animate", "VIDS-VRDIERR");
  }

  while (True)		/* loop until user presses a button	*/
  {
    if (ButtonDown(env, &action))			/* check for	*/
      if ((action == ACCEPT) || (action == CANCEL))	/* completion	*/
        break;
    status = zdclocation(unit, curs, &x, &y);
    if (status != SUCCESS)
    {
      free(xout); free(yout);
      ABORT(FAIL, "Unable to get cursor location", "VIDS-VRDIERR");
    }
    CursRaw2IMP(env, imp, x, y, &x, &y);
    if (bounds != NULL)
    {
      if (bounds->bottom + y > env->nlMax)
        y = env->nlMax - bounds->bottom;
      if (bounds->right + x > env->nsMax)
        x = env->nsMax - bounds->right;
      if (bounds->top + y <= 0)
        y = 1 - bounds->top;
      if (bounds->left + x <= 0)
        x = 1 - bounds->left;
    }

    if ((x == xold) && (y == yold))
      continue;

    zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), n, xout, yout);
    for (i=0; i<n; i++)						/* blank old */
    {
      xout[i] = xin[i] + x;
      yout[i] = yin[i] + y;
    }
    zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), n, xout, yout); /*draw new*/
    xold = x;
    yold = y;
  }
  zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), n, xout, yout); /* erase */

  free(xout);
  free(yout);

  switch (action)
  {
    case CANCEL :
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
    case ACCEPT :
      thePt->h = x;
      thePt->v = y;
      return SUCCESS;
  }
  ABORT(FAIL, "Unrecognized action in RubberShape()", "VIDS-INTERR");
}					/* shouldn't happen */

/************************************************************************/
/* ExpandRect will expand the outline of a rectangle from a point to
 * the current cursor position until a button is pressed.
 */
int ExpandRect(env, imp, isSquare, startPt, endPt)
  VIDSEnvironment	*env;		/* in: VIDS environment		*/
  int			imp;		/* in: plane in which to animate*/
  Boolean		isSquare;	/* in: True if must be square	*/
  Point			*startPt;	/* in: start point of rectangle	*/
  Point			*endPt;		/* out: determined opposite pt	*/
{
  int x[5], y[5], x0, y0, x1, y1, unit, curs, status;
  ButtonAction action;
  Boolean ButtonDown();

  unit = env->devUnit;
  curs = env->cursor.number;
  
  ButtonMessage(env, True, True, False);       /* tell user what to hit	*/

  x[0] = x[1] = x[4] = startPt->h;		/* set up initial rect	*/
  y[0] = y[3] = y[4] = startPt->v;
  status = zdclocation(unit, curs, &x0, &y0);
  if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
  CursRaw2IMP(env, imp, x0, y0, &x0, &y0);
  if (isSquare) SquarePoints(startPt->h, startPt->v, &x0, &y0);
  x[2] = x[3] = x0;
  y[1] = y[2] = y0;
  status = zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), 5, x, y);
  if (status != SUCCESS)
    ABORT(FAIL, "Could not update rectangle", "VIDS-VRDIERR");
  while (True)
  {
    if (ButtonDown(env, &action))			/* check for	*/
      if ((action == ACCEPT) || (action == CANCEL))	/* completion	*/
        break;
    status = zdclocation(unit, curs, &x1, &y1);
    if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
    CursRaw2IMP(env, imp, x1, y1, &x1, &y1);
    if (isSquare) SquarePoints(startPt->h, startPt->v, &x1, &y1);
    if ((x1 == x0) && (y1 == y0)) continue;
    zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), 5, x, y); /* blank old */
    x[2] = x[3] = x0 = x1;
    y[1] = y[2] = y0 = y1;
    zdimpolyline(unit, imp, GC(Rubber), GC(Rubber), 5, x, y); /* draw new rect*/
  }
  zdimpolyline(unit, imp, GC(Rubber), GC(NoColor), 5, x, y); /* blank out rect*/
  switch (action)
  {
    case CANCEL :
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
    case ACCEPT :
      endPt->h = x0;
      endPt->v = y0;
  }
  return SUCCESS;
}
/************************************************************************/
/* ExpandOval will expand the outline of an oval from a point to
 * the current cursor position until a button is pressed.
 */
int ExpandOval(env, imp, isCircle, startPt, endPt)
  VIDSEnvironment	*env;		/* in: VIDS environment		*/
  int			imp;		/* in: plane in which to animate*/
  Boolean		isCircle;	/* in: True if must be circle	*/
  Point			*startPt;	/* in: start point of rectangle	*/
  Point			*endPt;		/* out: determined opposite pt	*/
{
  int unit, curs, status;
  ButtonAction action;
  Boolean ButtonDown();
  Rect	rect0,rect1;
  Point pts[2];

  unit = env->devUnit;
  curs = env->cursor.number;
  
  ButtonMessage(env, True, True, False);       /* tell user what to hit	*/
  pts[0].h = startPt->h; pts[0].v = startPt->v;
  status = zdclocation(unit, curs, &pts[1].h, &pts[1].v);
  if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
  CursRaw2IMP(env, imp, pts[1].h, pts[1].v, &pts[1].h, &pts[1].v);
  if (isCircle) SquarePoints(startPt->h, startPt->v, &pts[1].h, &pts[1].v);
  PointsToRect(pts, 2, &rect0);
  status = FrameOval(unit, imp, GC(Rubber), GC(Rubber), &rect0);
  if (status != SUCCESS)
    ABORT(FAIL, "Could not update oval", "VIDS-VRDIERR");
  while (True)
  {
    if (ButtonDown(env, &action))			/* check for	*/
      if ((action == ACCEPT) || (action == CANCEL))	/* completion	*/
        break;
    status = zdclocation(unit, curs, &pts[1].h, &pts[1].v);
    if (status != SUCCESS) ABORT(FAIL, "Unable to get cursor location",
			    "VIDS-VRDIERR");
    CursRaw2IMP(env, imp, pts[1].h, pts[1].v, &pts[1].h, &pts[1].v);
    if (isCircle) SquarePoints(startPt->h, startPt->v, &pts[1].h, &pts[1].v);
    PointsToRect(pts, 2, &rect1);
    if (EqualRect(&rect1, &rect0)) continue;
    FrameOval(unit, imp, GC(Rubber), GC(NoColor), &rect0); /* blank old oval */
    FrameOval(unit, imp, GC(Rubber), GC(Rubber), &rect1); /* and draw new one */
    BlockMove(&rect1, &rect0, sizeof(Rect));	/* save for next iteration */
  }
  FrameOval(unit, imp, GC(Rubber), GC(NoColor), &rect0);
  switch (action)
  {
    case CANCEL :
      ABORT(FAIL, "Operation canceled", "VIDS-CANCELED");
    case ACCEPT :
      endPt->h = pts[1].h;
      endPt->v = pts[1].v;
  }
  return SUCCESS;
}
/************************************************************************/
/* ButtonMessage tells the user which buttons to hit.  The buttons are
 * always consistent in function for a given trackball/mouse.
 */
ButtonMessage(env, acceptFlag, cancelFlag, rejectFlag)
  VIDSEnvironment	*env;
  Boolean		acceptFlag;	/* True if ACCEPT button needed	*/
  Boolean		cancelFlag;	/* True if CANCEL button needed	*/
  Boolean		rejectFlag;	/* True if REJECT button needed	*/
{
  char msg[STRINGSIZ+1];
  int nButtons;
  
  nButtons = env->nButtons;
  strcpy(msg, "Push ");
  if (acceptFlag)
  {
    if (nButtons >= 1)
      strcat(msg, "button 1 ");
    else
      strcat(msg, "carriage return ");
    strcat(msg, "to accept point; ");
  }
  if (cancelFlag)
  {
    if (nButtons >= 2)
      strcat(msg, "button 2 ");
    else
      strcat(msg, "C ");
    strcat(msg, "to cancel; ");
  }
  if (rejectFlag)
  {
    if (nButtons >= 3)
      strcat(msg, "button 3 ");
    else
      strcat(msg, "R ");
    strcat(msg, "to terminate");
  }
  NotifyUser(Inform,"",msg);
  return;
}
/************************************************************************/
/* ButtonDown returns true if any buttons are down, and the action which
 * should be taken if one is down.
 */
Boolean ButtonDown(env, action)
  VIDSEnvironment	*env;
  int			*action; /* ACCEPT, CANCEL, or REJECT the point	*/
{
  int down;
  char PeekChar();

/* First check all the buttons on the trackball that are there, and 	*/
/* then check the keyboard.  This allows the keyboard keys to function	*/
/* even when only the trackball has been asked for, so there is another	*/
/* means of escape from this loop					*/
  if (env->nButtons >= 3)
  {
    if (zdxswitch(env->devUnit, 1, 3, &down) != SUCCESS)
    {
      *action = CANCEL;
      return True;
    }
    if (down)
    {
      *action = REJECT;
      return True;
    }
  }
  if (env->nButtons >= 2)
  {
    if (zdxswitch(env->devUnit, 1, 2, &down) != SUCCESS)
    {
      *action = CANCEL;
      return True;
    }
    if (down)
    {
      *action = CANCEL;
      return True;
    }
  }
  if (env->nButtons >= 1)
  {
    if (zdxswitch(env->devUnit, 1, 1, &down) != SUCCESS)
    {
      *action = CANCEL;
      return True;
    }
    if (down)
    {
      *action = ACCEPT;
      return True;
    }
  }
/* If we are here, then none of the trackball buttons were pushed, or	*/
/* else some or all of the trackball buttons are not there.		*/
  switch (PeekChar(False))
  {
    case 'R' :
    case 'r' :
    case 'T' :			/* for Terminate */
    case 't' :
      *action = REJECT;
      return True;
    case 'C' :
    case 'c' :
      *action = CANCEL;
      return True;
    case '\r' :
    case 'A' :
    case 'a' :
      *action = ACCEPT;
      return True;
  }
  return False;
}
/************************************************************************/
