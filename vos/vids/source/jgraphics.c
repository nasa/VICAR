/* JGRAPHICS.C -- subroutines for the JGRAPHICS command
 */
#include "VIDSdefs.h"
/************************************************************************/
/* jgraphics_do will put the specified plane into graphics mode; ie, 
 * attach the plane to the graphics lookup table if it exists.
 */
int jgraphics_do(env)
  VIDSEnvironment	*env;		/* display device environment	*/
{
  int		status;			/* status indicator		*/
  int		plane;			/* plane to set			*/
  int		nPlanes;		/* number of image planes	*/
  int		i;

  status = GetPlaneList(env, &plane, &nPlanes, False);
  if (status != SUCCESS) return status;
  
  status = NewName(env, plane, "GRAPHICS");
  if (status != SUCCESS)
    return status;

  status = zdgconnect(env->devUnit, plane, 1, FALSE);
  if (status != SUCCESS)
      ABORT(FAIL, "Unable to connect graphics lookup table to that plane",
          "VIDS-VRDIERR");
  ShowGraphics(env);

  if (env->grafIMP != plane)	/* invalidate all histogram locations */
  {				/* in case region defs changed */
    for (i=0; i<MAXPLANES; i++)
      InvalHistLoc(&env->planes[i]);
  }
  env->grafIMP = plane;
  return SUCCESS;
}
