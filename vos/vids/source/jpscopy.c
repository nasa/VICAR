#include "VIDSdefs.h"

/* JPSCOPY will copy a pseudocolor table from one plane to another.
 */

int jpscopy_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int p1, p2;				/* src, dst planes		*/
  int i;				/* temp variable		*/
  PSTable *pstable;
  PSTable *NewPSTable(), *CurrentPSTable();

  if (GetJPSCopyParms(env, &p1, &p2) != SUCCESS) return FAIL;

  pstable = NewPSTable(env, p2);
  if (pstable == NULL) return FAIL;

  BlockMove(CurrentPSTable(env, p1), pstable, sizeof(PSTable));

  SendLuts(env);
  return SUCCESS;
}


/************************************************************************/
/* GetJPSCopyParms gets parms for JPSCopy.
 */
int GetJPSCopyParms(env, p1, p2)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int *p1, *p2;				/* src/dst planes		*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  TAEVariable	*GetVariable();
  
  v = GetVariable(env, "PLANE1");
  if (v == NULL) return FAIL;
  if (NameToImp(env, SVAL(*v, 0), p1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "PLANE2");
  if (v == NULL) return FAIL;
  if (NameToImp(env, SVAL(*v, 0), p2) != SUCCESS)
    return FAIL;

  if ((*p1 <= 0) || (*p2 <= 0))
      ABORT(FAIL, "Image plane numbers must be positive.", "VIDS-BADPLANE");
  if ((*p1 > env->nimps) || (*p2 > env->nimps))
    ABORT(FAIL, "Sorry, one of the image plane numbers is too large.",
          "VIDS-BADPLANE");
  return SUCCESS;
}
