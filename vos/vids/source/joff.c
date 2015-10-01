#include "VIDSdefs.h"
/************************************************************************/
/* joff_do -- code for the JOFF command to turn off all of the LUTs
 * associated with the named planes.
 */
int joff_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  int		unit;		/* the device unit number		*/
  int		aLUT[256];	/* a lut of all zeroes			*/
  int		status;		/* status holder			*/
  int		planes[10];	/* planes whose LUTs are to be turned on */
  int		i,nPlanes;	/* temp variable			*/

  unit = env->devUnit;
  status = GetPlaneList(env, planes, &nPlanes, False);
  if (status != SUCCESS) return status;
  BlockFill(0, aLUT, 1024); /* 256 longwords */
  for (i = 0; i < nPlanes; i++)
  {
    if (planes[i] == env->grafIMP)
    {
      HideGraphics(env);
    }
    if (!env->isColor)			/* if bw mode, RED,GREEN,BLUE     */
    {					/* appear to the user to be off   */
      if (planes[i] != env->bwIMP) continue;
      status = zdlwrite(unit, redLUT, 1, aLUT);
      status = zdlwrite(unit, greenLUT, 1, aLUT);
      status = zdlwrite(unit, blueLUT, 1, aLUT);
      env->isOn.bw = False;
    }
    else			/* if color mode, do the individual planes */
    {
      if (planes[i] == env->redIMP) 
      {
        status = zdlwrite(unit, redLUT, 1, aLUT);
        env->isOn.red = False;
      }
      if (planes[i] == env->greenIMP)
      {
        status = zdlwrite(unit, greenLUT, 1, aLUT);
        env->isOn.green = False;
      }
      if (planes[i] == env->blueIMP)
      {
        status = zdlwrite(unit, blueLUT, 1, aLUT);
        env->isOn.blue = False;
      }
    }
    if (status != SUCCESS)
      ABORT(FAIL, "Sorry, cannot turn that off.", "VIDS-VRDIERR");
  }
  return SUCCESS;
}
