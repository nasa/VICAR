/* region.c -- utility subroutines for handling regions in VIDS.
 */

#include "VIDSdefs.h"

static Boolean tempexist;

/************************************************************************/
/* InitRegions initializes the RegionList array.
 */
InitRegions(env)
  VIDSEnvironment	*env;
{

  env->regionList = NULL;

  return SUCCESS;
}
/************************************************************************/
/* ClearRegion clears out the individual members of a Region structure.
 */
int ClearRegion(rgn)
  Region	*rgn;			/* region to clear		*/
{
  rgn->next = NULL;
  rgn->prev = NULL;
  rgn->name[0] = '\0';
  rgn->seed = 0;
  SetRect(&rgn->bounds, 0,0,0,0);
  rgn->type = (ObjectType)0;
  rgn->nPoints = 0;
  rgn->pointList = NULL;
  rgn->temporary = FALSE;
  return SUCCESS;
}
/************************************************************************/
/* NameToRegion finds a region with a given name and returns a pointer
 * to that region.  If the 'exact' argument is True, then the region name
 * must be an exact match.  Otherwise, it may also be
 * a unique left substring of the actual name.  If no match is found,
 * NULL is returned.
 */
Region *NameToRegion(env, name, exact)
  VIDSEnvironment	*env;
  char *name;			/* name of region being looked for	*/
  Boolean exact;		/* True if only exact match acceptable	*/
{
  Region *rgn, *match;
  int nfound;
  char localName[STRINGSIZ+1];

  nfound = 0;			/* number of matches found. */
  UpperCase(name, localName);
  for (rgn = env->regionList; rgn != NULL; rgn = rgn->next)
  {
    if (EQUAL(rgn->name, localName)) return rgn;
    if (!exact)
    {
      if (s_lseq(localName, rgn->name))
      {			/* if substring, save which region and		*/
        match = rgn;	/* increment the total number of matches found.	*/
        nfound++;
        NotifyUser(Verbose,"","%s translated to %s",
            localName,rgn->name);
      }
    }
  }
  if (nfound == 1) return match;
  if (nfound > 1)
    NotifyUser(Inform,"VIDS-AMBIG","Ambiguous region name '%s'",name);
  return NULL;
}
/************************************************************************/
/* SeedToRegion finds the region with the given seed and returns a 
 * pointer to it.  Returns NULL if not found.
 */
Region *SeedToRegion(env, seed)
  VIDSEnvironment	*env;
  int seed;
{
  Region *rgn;
  
  for (rgn = env->regionList; rgn != NULL; rgn = rgn->next)
    if (rgn->seed == seed) return (rgn);
  return NULL;
}
/************************************************************************/
Region *NextEmptyRegion(env)
  VIDSEnvironment	*env;
{
  Region *rgn, *newrgn;

  newrgn = malloc(sizeof(Region));
  if (newrgn == NULL)
    ABORT(NULL, "Insufficient memory to get a new region", "VIDS-INSUFMEM");
  ClearRegion(newrgn);

  if (env->regionList == NULL)		/* first one in list */
    env->regionList = newrgn;
  else
  {					/* append this to end of the list */
    for (rgn = env->regionList; rgn->next != NULL; rgn = rgn->next)
      ;
    rgn->next = newrgn;
    newrgn->prev = rgn;
  }

  return newrgn;
}
/************************************************************************/
/* GetRegionList will get a list of regions from the user command line.
 */
int GetRegionList(env, rgnList, nRgns, parmName, maxCount)
  VIDSEnvironment	*env;
  Region		*rgnList[];	/* out: list of regions given	*/
  int			*nRgns;		/* out: no. of regions on list	*/
  char			*parmName;	/* in: name of the parameter	*/
  int			maxCount;	/* in: max # of regions allowed	*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  char			*theStr;	/* temp string ptr		*/
  int			i;		/* temporary increment variable	*/
  int			count;		/* TCL variable count		*/
  TAEVariable		*GetVariable();
  
  v = GetVariable(env, parmName);
  if (v == NULL)
  {
    *nRgns = 0;
    return SUCCESS;
  }
  count = v->v_count;
  if (count <= 0)
  {
    *nRgns = 0;
    return SUCCESS;
  }
  count = MIN(count, maxCount);
  *nRgns = 0;
  for (i = 0; i < count; i++)
  {
    theStr = SVAL(*v, i);
    UpperCase(theStr, theStr);
    rgnList[i] = NameToRegion(env, theStr, False);
    if (rgnList[i] == NULL)
    {
      char msgbuf[STRINGSIZ+1];

      sprintf(msgbuf,"Unrecognized or ambiguous region name \"%s\"",theStr);
      ABORT(FAIL, msgbuf, "VIDS-BADREGION");
    }
    *nRgns += 1;
  }
  return SUCCESS;
}
/************************************************************************/
/* GetAllRegionNames will get a list of all the region names that exist
 * in the region list.
 */
int GetAllRegionNames(env, rgnList, nRgns, maxCount)
  VIDSEnvironment	*env;
  char			*rgnList[];	/* out: list of region names	*/
  int			*nRgns;		/* out: no. of regions in list	*/
  int			maxCount;	/* in: max # of regions allowed	*/
{
  Region *rgn;

  *nRgns = 0;  
  for (rgn = env->regionList; rgn != NULL; rgn = rgn->next)
  {
    if (*nRgns >= maxCount)			/* array is full */
      return SUCCESS;
    rgnList[(*nRgns)++] = rgn->name;
  }

  return SUCCESS;
}
/************************************************************************/
/* GetRegion will interactively get a region from the user which is 
 * restricted to the shape theType.  Returns NULL and appropriate 
 * messages to print if anything bad happens.
 */
Region *GetRegion(env, imp, name, type)
  VIDSEnvironment	*env;
  int			imp;		/* The plane in which to draw it*/
  char			*name;		/* name of the region to define */
  ObjectType		type;		/* type of region		*/
{
  Point		*points;
  int		nPoints, status;
  Boolean	erased;
  Region	*theRgn, *PointsToRegion();
  GraphColor	color;
  
  nPoints = 2;
  if (type == Polygon)		/* Try 1000 pts as first try for polygon */
    points = (Point *) malloc(1000 * sizeof(Point));
  else
    points = (Point *) malloc(2 * sizeof(Point));
  if (points == NULL)
    ABORT(NULL, "Insufficient memory to define the region", "VIDS-INSUFMEM");
  erased = False;
  if ((theRgn = NameToRegion(env,name,True)) != NULL)	/* if redefining, */
  {							/* erase old rgn  */
    color = theRgn->color;
    FrameRgn(env->devUnit, imp, GC(color), GC(NoColor), theRgn);
    theRgn->color = NoColor;
    erased = True;
  }
  switch (type)
  {
    case Square :
      status = PokeSquare(env, imp, System, points); break;
    case Rectangle :
      status = PokeRect(env, imp, System, points); break;
    case Circle :
      status = PokeCircle(env, imp, System, points); break;
    case Oval :
      status = PokeOval(env, imp, System, points); break;
    case Polygon :
      nPoints = 1000;
      status = PokePoly(env, imp, System, points, &nPoints); break;
  }
  if (status != SUCCESS)
  {
    if (erased)		/* if erased from screen, redraw it */
    {
      FrameRgn(env->devUnit, imp, GC(color), GC(color), theRgn);
      theRgn->color = color;
    }
    free(points);
    return NULL;
  }
  theRgn = PointsToRegion(env, name, type, points, nPoints);
  if (theRgn != NULL) theRgn->color = System;
  free(points);
  return theRgn;
}
/************************************************************************/
/* PointsToRegion will take a straight list of points, string them
 * together as a polygon, and return a pointer to the region.  Returns
 * NULL and appropriate error messages if anything bad happens.
 */
Region *PointsToRegion(env, name, type, points, nPoints)
  VIDSEnvironment	*env;
  char			*name;		/* name of the region to define */
  ObjectType		type;		/* type of region		*/
  Point			points[];	/* list of points for vectors	*/
  int			nPoints;	/* number of elements in points	*/
{
  int			*rgnPoints;	/* ptr to points		*/
  int			temp;		/* temp variables		*/
  Region		*rgn;		/* ptr to found region		*/

  rgn = NameToRegion(env,name,True);		/* First, try to find	*/
  if (rgn != NULL)				/* a region with the	*/
  {						/* requested name.	*/
    if (rgn->pointList != NULL)
      free(rgn->pointList);
  }
  else
  {
    rgn = NextEmptyRegion(env);
    if (rgn == NULL)		/* If no empty region was found...	*/
      return NULL;
  }
  
  UpperCase(name,rgn->name);		/* store name in upper case	*/
  rgn->seed = NextSeed();
  rgn->type = type;
  PointsToRect(points, nPoints, &rgn->bounds);	/* find the bounding rect */
  switch (type)
  {
    case Square :		/* for squares and circles, restrict	*/
    case Circle :		/* the size of the rect to a square	*/
      temp = MIN((rgn->bounds.bottom - rgn->bounds.top),
                 (rgn->bounds.right - rgn->bounds.left));
      rgn->bounds.right = rgn->bounds.left + temp;
      rgn->bounds.bottom = rgn->bounds.top + temp;
    case Rectangle :
    case Oval :
      rgn->nPoints = 2;
      rgn->pointList = NULL;
      break;
    case Polygon :
      rgn->nPoints = nPoints;
      if ((rgn->pointList = malloc(nPoints * sizeof(Point))) == NULL)
        ABORT(NULL,"Insufficient memory to save the region","VIDS-INSUFMEM");
      BlockMove(points, rgn->pointList, (nPoints * sizeof(Point)));
  }
  return (rgn);
}
/************************************************************************/
/* NextSeed returns an integer used to uniquely identify a region.
 */
int NextSeed()
{
  static int seed = 0;
  
  seed++;
  return seed;
}
/************************************************************************/
/* DisposeRegion will "dispose" of a region, freeing up allocated space,
 * etc.
 */
int DisposeRegion(env, rgn)
  VIDSEnvironment	*env;
  Region		*rgn;		/* in: region to dispose	*/
{
  if (rgn->pointList != NULL) free(rgn->pointList);
  if (rgn->prev == NULL)		/* beginning of list */
    env->regionList = rgn->next;
  else
    rgn->prev->next = rgn->next;

  if (rgn->next != NULL)		/* end of list */
    rgn->next->prev = rgn->prev;

  free(rgn);
  return SUCCESS;
}
/************************************************************************/
/* InitTempRgns will clear the temp region flag to indicate that none
 * currently exist.
 */
int InitTempRgns()
{
  tempexist = False;

  return SUCCESS;
}
/************************************************************************/
/* FreeTempRgns will dispose of all temporary regions currently in the
 * region list.
 */
int FreeTempRgns(env)
  VIDSEnvironment	*env;
{
  Region *rgn;

  if (tempexist)
    for (rgn = env->regionList; rgn != NULL; rgn = rgn->next)
    {
      if (rgn->temporary)
        DisposeRegion(env, rgn);
    }

  return SUCCESS;
}
/************************************************************************/
/* MarkRgnTemp will mark a region as temporary so it will be disposed of
 * automatically.
 */
int MarkRgnTemp(rgn)
Region *rgn;
{
  rgn->temporary = TRUE;  
  tempexist = True;
  return SUCCESS;
}
/************************************************************************/
/* RegionToLocal takes the bounds and points in the region rgn, and 
 * converts them to coordinates local to the image plane imp.  The top
 * left corner of the bounding rectangle will always be in the image plane,
 * but the other corner may not be.  Points may have coordinates larger
 * than the image plane.
 */
int RegionToLocal(env, imp, rgn, outRgn)
  VIDSEnvironment *env;		/* in: the display device environment	*/
  int		imp;		/* in: plane to whose coords to convert	*/
  Region	*rgn;		/* in: region to be converted		*/
  Region	*outRgn;	/* in/out: ptr to new region in local	*/
{
  PlaneInfo	*p;		/* ptr to plane information		*/
  int		i;		/* incr variable	*/
  Rect		*b;		/* Ptr to outRgn->bounds		*/
  int		xOff,yOff;	/* offset from corner in x,y directions	*/
  int		savetemp;	/* save temporary status of region	*/
  Region	*saven, *savep;	/* save pointers			*/

  p = &env->planes[imp];

  savetemp = outRgn->temporary;
  saven = outRgn->next;
  savep = outRgn->prev;
  if (outRgn->pointList != NULL) free(outRgn->pointList);
  BlockMove(rgn, outRgn, sizeof(Region));
  outRgn->prev = savep;
  outRgn->next = saven;
  outRgn->temporary = savetemp;

  b = &outRgn->bounds;
  
  xOff = b->right - b->left;		/* Convert coords of bounding rect */
  yOff = b->bottom - b->top;
  CursIMP2IMP(env, env->grafIMP, imp, b->left, b->top, &b->left, &b->top);
  OffsetIMP2IMP(env, env->grafIMP, imp, xOff, yOff, &xOff, &yOff);
  b->right = b->left + xOff;
  b->bottom = b->top + yOff;

  if (rgn->pointList == NULL)	/* If no pointlist, then we are done.	*/
    return SUCCESS;

  outRgn->pointList = malloc(outRgn->nPoints * sizeof(Point));
  if (outRgn->pointList == NULL)
    ABORT(FAIL,"Insufficient memory to convert region","VIDS-INSUFMEM");
  
  for (i = 0; i < rgn->nPoints; i++) /* Convert all points in polygon	*/
  {
    xOff = rgn->pointList[i].h - rgn->bounds.left;
    yOff = rgn->pointList[i].v - rgn->bounds.top;
    OffsetIMP2IMP(env, env->grafIMP, imp, xOff, yOff, &xOff, &yOff);
    outRgn->pointList[i].h = b->left + xOff;
    outRgn->pointList[i].v = b->top + yOff;
  }
  return SUCCESS;
}
/************************************************************************/
/* FreeAllRegions will dispose of all regions in the environment block.
 */
int FreeAllRegions(env)
  VIDSEnvironment	*env;
{
  Region *rgn, *next;

  for (rgn = env->regionList; rgn != NULL; rgn = next)
  {
    next = rgn->next;
    DisposeRegion(env, rgn);
  }

  return SUCCESS;
}
