/* JON.C -- subroutines for the JON  command to turn image planes on and off.
 */
#include "VIDSdefs.h"

/************************************************************************/
/* jon_do -- code for the JON command.
 * This command will turn on all of the lookup tables associated with
 * the named planes.  Accepts any plane name.
 */
int jon_do(env)
  VIDSEnvironment	*env;	/* the VIDS environment			*/
{
  int		unit;		/* the device unit number		*/
  int		status;		/* status holder			*/
  int		planes[10];	/* planes whose LUTs are to be turned on */
  int		i,nPlanes;	/* temp variable			*/
  int		*CurrentLut();

  unit = env->devUnit;

  status = GetPlaneList(env, planes, &nPlanes, False);
  if (status != SUCCESS) return status;
  for (i = 0; i < nPlanes; i++)
  {
    if (planes[i] == env->grafIMP)
    {
      ShowGraphics(env);
    }
    if (env->isBW)			/* if bw mode, RED,GREEN,BLUE     */
    {					/* appear to the user to be off   */
      if (planes[i] != env->bwIMP) continue;
      SetBWMode(env);
    }
    else if (env->isPseudo)		/* if pseudo mode, RED,GREEN,BLUE */
    {					/* appear to the user to be off   */
      if (planes[i] != env->bwIMP) continue;
      SetPseudoMode(env);
    }
    else if (env->isColor)	/* if color mode, do the individual planes */
    {
      if (planes[i] == env->redIMP)
      {
        status = zdlwrite(unit, redLUT, 1, CurrentLut(env, env->redIMP));      
        env->isOn.red = True;
      }
      if (planes[i] == env->greenIMP)
      {
        status = zdlwrite(unit, greenLUT, 1, CurrentLut(env, env->greenIMP));
        env->isOn.green = True;
      }
      if (planes[i] == env->blueIMP)
      {
        status = zdlwrite(unit, blueLUT, 1, CurrentLut(env, env->blueIMP));
        env->isOn.blue = True;
      }
      env->isOn.bw = False;
    }

    if (status != SUCCESS)
      ABORT(FAIL, "Sorry, cannot turn that on.", "VIDS-VRDIERR");
  }
  return SUCCESS;
}
