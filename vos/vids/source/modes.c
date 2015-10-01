#include "VIDSdefs.h"

/* jbw_do, jcolor_do, jpseudo_do  -- routines to set color, black and white,
 * or pseudocolor mode.
 */
int jbw_do(env)

  VIDSEnvironment	*env;		/* current display environment	*/
{
  int			plane;		/* new plane for black and white mode */
  int			status;		/* temp status variable		*/
  int			nPlanes;	/* temp variable		*/
  
  status = GetPlaneList(env, &plane, &nPlanes, False);
  if (status != SUCCESS) return status;		/* depend on COUNT=1 in pdf */
  
  env->bwIMP = plane;
  status = NewName(env, plane, "BW");
  if (status != SUCCESS)
    return status;

  SetBWMode(env);
  return SUCCESS;
}
/************************************************************************/
int jcolor_do(env)

  VIDSEnvironment	*env;		/* current display environment	*/
{
  int			planes[3];	/* new planes for color mode	*/
  int			status;		/* temp status variable		*/
  int			nPlanes;	/* temp variable		*/

  status = GetPlaneList(env, planes, &nPlanes, True);
  if (status != SUCCESS) return status;

  switch (nPlanes)
  {
    case 3 :
        env->blueIMP = planes[2];
        status = NewName(env, planes[2], "BLUE");
        if (status != SUCCESS)
          return FAIL;
    case 2 :
        env->greenIMP = planes[1];
        status = NewName(env, planes[1], "GREEN");
        if (status != SUCCESS)
          return FAIL;
    case 1 :
        env->redIMP = planes[0];
        status = NewName(env, planes[0], "RED");
        if (status != SUCCESS)
          return FAIL;
  }
  SetColorMode(env);
  return SUCCESS;
}
/************************************************************************/
int jpseudo_do(env)

  VIDSEnvironment	*env;		/* current display environment	*/
{
  int			plane;		/* new plane for pseudocolor mode */
  int			status;		/* temp status variable		*/
  int			nPlanes;	/* temp variable		*/
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int			tabno;		/* pseudocolor table number	*/
  PSTable		*pstable;
  PSTable		*NewPSTable();
  TAEVariable		*GetVariable();

  status = GetPlaneList(env, &plane, &nPlanes, False);
  if (status != SUCCESS) return status;		/* depend on COUNT=1 in pdf */

  v = GetVariable(env, "TABLE");
  if (v == NULL) return FAIL;
  if (v->v_count != 0)			/* TABLE specified, so use it */
  {
    pstable = NewPSTable(env, plane);
    if (pstable == NULL)
      return FAIL;
    tabno = IVAL(*v, 0);
    status = PresetPSTable(pstable, tabno);
    if (status != SUCCESS)
      ABORT(FAIL, "Preset pseudocolor table number is not valid", "VIDS-BADPSTBL");
  }

  env->bwIMP = plane;			/* pseudo uses the BW plane */
  status = NewName(env, plane, "BW");
  if (status != SUCCESS)
    return FAIL;

  SetPseudoMode(env);
  return SUCCESS;
}
