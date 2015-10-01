#include "VIDSdefs.h"
TAEVariable *GetVariable();
/************************************************************************/
/* jdraw_do will allow the user to draw shapes in the graphics plane
 * either via parameters or interactively.
 */
int jdraw_do(env)
  VIDSEnvironment	*env;
{
  ObjectType		shape;		/* the shape of the object	*/
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  Boolean		fill;
  Point			ipoints[1000];
  Point			*points;
  int			nPoints;	/* number of points		*/
  Rect			bounds;
  int			unit, imp, status;
  int			temp;
  int			color;
  int			mask;
  Region		*locRgn;
  ObjectType		GetShapeParm();
  GraphColor		StringToColor();
  Region		*PointsToRegion();

  unit = env->devUnit;
  imp = env->grafIMP;

  ShowGraphics(env);
  InvalHist(&env->planes[imp]);

  shape = GetShapeParm(env, False);
  if (shape == (ObjectType)0) return FAIL;

  color = env->drawColor;
  env->graphColors[Special] = color;
  mask = env->drawMask;

  v = GetVariable(env, "FILL");
  if (v == NULL) return FAIL;
  fill = False;
  if (EQUAL(SVAL(*v,0), "FILL"))
    fill = True;

  v = GetVariable(env, "POINTS");
  if (v == NULL) return FAIL;
  nPoints = v->v_count;

  if (nPoints == 0)				/* if no points given,	*/
  {						/* use interactive curs	*/
    nPoints = 1000;
    status = GetGraphic(env, imp, shape, Special, ipoints, &nPoints);
    if (status != SUCCESS) return status;
    points = ipoints;		/* set pointer to interactive point list */
  }
  else
  {
    nPoints = nPoints / 2;
    if ((nPoints * 2) != v->v_count)
      ABORT(FAIL,"Points array must have an even number of integers",
            "VIDS-ODDPOINTS");
    if (nPoints < 2)
      ABORT(FAIL,"Not enough points to define a shape; at least 2 required",
            "VIDS-INSUFPTS");
    points = (Point *)v->v_cvp;	/* set pointer to parameter point list */
  }

  PointsToRect(points, nPoints, &bounds);

  if (shape == Square || shape == Circle) {	/* make sides equal */
    temp = MIN((bounds.bottom - bounds.top),
               (bounds.right - bounds.left));
    bounds.right = bounds.left + temp;
    bounds.bottom = bounds.top + temp;
  }

  switch (shape)
  {
    case Square:
    case Rectangle:
      status = FrameRect(unit, imp, mask, color, &bounds);
      break;
    case Circle:
    case Oval:
      status = FrameOval(unit, imp, mask, color, &bounds);
      break;
    case Polygon:
      status = FramePoly(unit, imp, mask, color, points, nPoints);
      break;
    case Segments:
    case Line:
      status = FrameLineSegs(unit, imp, mask, color, points, nPoints);
      break;
    case Arrow:
      status = FrameArrow(unit, imp, mask, color, &points[0], &points[1]);
      break;
    default:
      ABORT(FAIL, "Internal error: invalid shape in jdraw_do().","VIDS-INTERR");
  }
  if (status != SUCCESS)
    ABORT(FAIL, "Unable to draw the graphic", "VIDS-VRDIERR");

  if (fill)			/* fill in the graphic if requested */
  {
    if (shape == Segments || shape == Line || shape == Arrow)
      ABORT(FAIL,"Sorry, can't fill a shape that's not closed","VIDS-NOTCLOSED");

    locRgn = PointsToRegion(env, "DRAW$TEMP", shape, points, nPoints);
    if (locRgn == NULL)
      ABORT(FAIL,"Insufficient memory to fill graphic","VIDS-INSUFMEM");
    status = FillRegion(env, imp, locRgn, mask, color);
    DisposeRegion(env, locRgn);
    if (status != SUCCESS)
      ABORT(FAIL,"Unable to fill the shape", "VIDS-VRDIERR");
  }

  return SUCCESS;
}

/************************************************************************/
/* GetGraphic will interactively get a graphic shape from the user which
 * is restricted to the shape 'type'.  Returns the VRDI status.
 */
int GetGraphic(env, imp, type, color, points, npoints)
  VIDSEnvironment	*env;
  int			imp;		/* The plane in which to draw it*/
  ObjectType		type;		/* type of graphic shape	*/
  GraphColor		color;		/* color to use for shape	*/
  Point			points[];	/* out: list of points defined	*/
  int			*npoints;	/* in/out: number of points	*/
{
  int		status;

  if (type != Polygon && type != Segments)
    *npoints = 2;

  switch (type)
  {
    case Square :
      status = PokeSquare(env, imp, color, points); break;
    case Rectangle :
      status = PokeRect(env, imp, color, points); break;
    case Circle :
      status = PokeCircle(env, imp, color, points); break;
    case Oval :
      status = PokeOval(env, imp, color, points); break;
    case Polygon :
      status = PokePoly(env, imp, color, points, npoints); break;
    case Segments :
      status = PokeLineSegs(env, imp, color, points, npoints); break;
    case Line :
    case Arrow :
      status = PokeLine(env, imp, color, points); break;
    default:
      ABORT(FAIL, "Internal error: invalid shape in GetGraphic().","VIDS-INTERR");
  }
  return status;
}
