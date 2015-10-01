#include "VIDSdefs.h"
TAEVariable *GetVariable();
/************************************************************************/
/* jdef_region_do will allow the user to define a region on the display
 * device interactively.
 */
int jdef_region_do(env)
  VIDSEnvironment	*env;
{
  ObjectType		shape;		/* the shape of the object	*/
  Region		*theRgn;	/* pointer to new region	*/
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  char			*theName;	/* ptr to region name		*/
  int			nPoints;	/* number of points		*/
  ObjectType		GetShapeParm();
  Region		*GetRegion(), *NameToRegion(), *PointsToRegion();

  shape = GetShapeParm(env, True);
  if (shape == (ObjectType)0) return FAIL;

  v = GetVariable(env, "NAME");
  if (v == NULL) return FAIL;
  theName = SVAL(*v, 0);

  v = GetVariable(env, "POINTS");
  if (v == NULL) return FAIL;
  nPoints = v->v_count;
  ShowGraphics(env);
  InvalHist(&env->planes[env->grafIMP]);

  if (nPoints == 0)				/* if no points given,	*/
  {						/* use interactive curs	*/
    theRgn = GetRegion(env, env->grafIMP, theName, shape);
    if (theRgn == NULL) return FAIL;
  }
  else
  {
    nPoints = nPoints / 2;
    if ((nPoints * 2) != v->v_count)
      ABORT(FAIL,"Points array must have an even number of integers",
            "VIDS-ODDPOINTS");
    if (nPoints < 2)
      ABORT(FAIL,"Not enough points to define a region; at least 2 required",
            "VIDS-INSUFPTS");
    theRgn = NameToRegion(env,theName,True);
    if (theRgn != NULL)
    {
      FrameRgn(env->devUnit,env->grafIMP,GC(theRgn->color),GC(NoColor),theRgn);
      theRgn->color = NoColor;
    }
    theRgn = PointsToRegion(env, theName, shape, v->v_cvp, nPoints);
    if (theRgn == NULL) return FAIL;
  }
  if (FrameRgn(env->devUnit, env->grafIMP, GC(System), GC(System), theRgn)
						!= SUCCESS)
        ABORT(FAIL,"Unable to frame that region", "VIDS-VRDIERR");
  theRgn->color = System;
  return SUCCESS;
}
/************************************************************************/
int jdef_plane_do(env)
  VIDSEnvironment	*env;
{
  TAEVariable	*v;
  char		*name;
  int		plane;
  int		i;
  
  v = GetVariable(env, "NAME");
  if (v == NULL) return FAIL;
  name = SVAL(*v, 0);

  if (GetPlaneList(env, &plane, &i, False) != SUCCESS)
    return FAIL;
  if (NewName(env, plane, name) != SUCCESS)
    return FAIL;
  return SUCCESS;
}
/************************************************************************/
ObjectType GetShapeParm(env, limit)
  VIDSEnvironment	*env;		/* pointer to input parm block	*/
  Boolean		limit;		/* True if shapes limited to rgn sh */
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/

  v = GetVariable(env, "SHAPE");
  if (v == NULL) return (ObjectType)0;
  switch (*SVAL(*v, 0))
  {
    case 'R' : return Rectangle;
    case 'O' : return Oval;
    case 'P' : return Polygon;
    case 'C' : return Circle;
    case 'S' : if (*(SVAL(*v,0)+1) == 'Q') return Square;
  }
  if (!limit)			/* not limited to region shapes */
  {
    switch (*SVAL(*v, 0))
    {
      case 'L' : return Line;
      case 'S' : if (*(SVAL(*v,0)+1) == 'E') return Segments;
      case 'A' : return Arrow;
    }
  }
  return Rectangle;		/* default, just in case */
}
/************************************************************************/
