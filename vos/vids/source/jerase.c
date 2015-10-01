#include "VIDSdefs.h"

/* JERASE will erase the image planes on the current device
 */
 
int jerase_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int unit;			/* device unit number			*/
  int nPlanes;			/* number of image planes to erase	*/
  int nRgns;			/* number of regions specified (1 or 0)	*/
  int i,j,status,planeStat;	/* temporary increment, status variables*/
  int planeList[MAXPLANES];	/* list of planes to erase		*/
  Region *rgns[MAXREGIONS];	/* list of regions to erase		*/
  Region *locRgn;		/* ptr to temp local region		*/
  Rect impRect;			/* rect defined to be size of imps	*/
  Point points[2];		/* for default region			*/
  Region *PointsToRegion(), *NextEmptyRegion();

/* GetPlaneList returns the list of planes; if an error occurs, such as	*/
/* an unrecognized plane name, that plane is simply not added to the	*/
/* list, so we can ignore any errors and do as much as possible.  On	*/
/* the other hand, if GetRegionList fails, we would erase things the	*/
/* user probably does not want to erase, so abort.			*/

  unit = env->devUnit;
  planeStat = GetPlaneList(env, planeList, &nPlanes, False);
  if (GetRegionList(env, rgns, &nRgns, "REGIONS", MAXREGIONS) != SUCCESS)
    return FAIL;

  if (nRgns != 0)
  {
    locRgn = NextEmptyRegion(env);
    if (locRgn == NULL)
      return FAIL;
  }

  SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);
  for (i = 0; i < nPlanes; i++)
  {
    if (nRgns == 0)			/* default to whole screen */
    {
      points[0].h = 1;			points[0].v = 1;
      points[1].h = env->nsMax;		points[1].v = env->nlMax;
      locRgn = PointsToRegion(env, "ERASE$TEMP", Rectangle, points, 2);
      if (locRgn == NULL)
      {
        status = FAIL;
        break;
      }
      status = FillRegion(env, planeList[i], locRgn, 255, 0);
      InvalHist(&env->planes[planeList[i]]);
      DisposeRegion(env, locRgn);
    }
    else
    {
      for (j = 0; j < nRgns; j++)
      {
        status = RegionToLocal(env, planeList[i], rgns[j], locRgn);
        if (status != SUCCESS) break;
	if (RectInRect(&locRgn->bounds, &impRect))
        {
          status = FillRegion(env, planeList[i], locRgn, 255, 0);
          InvalHist(&env->planes[planeList[i]]);
        }
	else
	{
	  NotifyUser(Inform,"",
"Region \"%s\" crosses the edge of plane %d, so it cannot be erased",
	      locRgn->name, planeList[i]);
	}
      }
    }
    if (status != SUCCESS) break;
  }
  if (nRgns != 0)
    DisposeRegion(env, locRgn);

  return status;
}
