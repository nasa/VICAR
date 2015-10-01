/* ImpNames.c -- This module contains the code to store and decode virtual
 * names for the image memory planes in VIDS.  Each device has an independent
 * plane name list, which is maintained in the environment block.
 */
/******************************************************************************/
#include "VIDSdefs.h"

/******************************************************************************/
/* InitNames initializes the linked list of logical plane names to the default
 * values.
 */
 
InitNames(env)

  VIDSEnvironment	*env;
{
  strcpy(env->planeNames.name, "");
  env->planeNames.imp = 0;
  env->planeNames.last = NULL;
  env->planeNames.next = NULL;
  return;
} 
/******************************************************************************/
/* NameToImp returns an image plane number for a logical plane name.
 */
 
int NameToImp(env, name, imp)

  VIDSEnvironment	*env;	/* in: environment block		*/
  char		*name;		/* in: logical plane name or no. as str	*/
  int		*imp;		/* out: plane number			*/
{
  char		localName[STRINGSIZ+1];	/* local string buffer		*/
  int		i;			/* increment			*/
  PlaneName	*pn;			/* ptr to plane name structure	*/
  
  UpperCase(name, localName);
  for (pn = &env->planeNames; pn != NULL; pn = pn->next)
  {
    if (s_lseq(localName, pn->name))		/* first, try to find the */
    {						/* name on the name list  */
      *imp = pn->imp;
      NotifyUser(Verbose, "", "%s translated to %s on plane %d",
                 localName, pn->name, pn->imp);
      return SUCCESS;				/* if found, we are done */
    }
  }
  i = atoi(localName);			/* here, if the name is not on	*/
  if (i > 0)				/* the list, we try to convert	*/
  {					/* it to an integer, on the	*/
    *imp = i;				/* assumption it is a literal	*/
    return SUCCESS;			/* value.			*/
  }

  sprintf(localName, "Unrecognized image plane \"%s\".", name);
  ABORT(FAIL, localName, "VIDS-BADPLANE");
}
/******************************************************************************/
/* NewName assigns the logical name given by 'name' to the memory plane
 * number given by 'imp'.  If the name is already in use, it is simply
 * overwritten.
 */
 
int NewName(env, imp, name)

  VIDSEnvironment	*env;		/* in: environment block	*/
  int		imp;			/* in: image memory plane number */
  char		*name;			/* in: logical name to assign	 */
{
  PlaneName	*curr, *next, *save;	/* ptr to plane name structure	 */
  char		localName[STRINGSIZ+1];	/* local string buffer		 */
  Boolean	found;

  UpperCase(name, localName);
  found = False;
  for (curr = &env->planeNames; curr != NULL; curr = curr->next)
  {
    if (strcmp(curr->name, localName) == 0) /* first, try to find the name */
      { found = True;    break;}	    /* on the name list		   */
    save = curr;
  }
  if (found)
    next = curr;
  else
  {
    curr = save;
    next = malloc(sizeof(PlaneName));
    if (next == NULL)
      ABORT(FAIL, "Insufficient memory to create plane name", "VIDS-INSUFMEM");
    curr->next = next;
    next->last = curr;
    next->next = NULL;
  }
  next->imp = imp;
  strcpy(next->name, localName);
  return SUCCESS;
}
/******************************************************************************/
/* FreeNames frees up all the logical plane names for this device.
 * Note that the first one in the list is constant in env, and is not
 * allocated.  Therefore, the freeing skips the first node.
 */
 
int FreeNames(env)

  VIDSEnvironment	*env;		/* in: environment block	*/
{
  PlaneName	*curr, *next;		/* ptr to plane name structure	 */

  for (curr = env->planeNames.next; curr != NULL; curr = next)
  {
    next = curr->next;
    free(curr);
  }

  InitNames(env);			/* clear out the head of the list */

  return SUCCESS;
}
/******************************************************************************/
