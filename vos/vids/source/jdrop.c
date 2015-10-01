#include "VIDSdefs.h"

/* JDROP -- drop (allocate) a display device, freeing the current environment
 * structure.  This is the opposite of JGRAB.
 * The empty env block is searched for and freed, and the current env block
 * is re-initialized to be the new empty one.  This makes the code in vids.c
 * much simpler (the current env is now the empty one).
 * If the special device name "ALL" is given, then all active devices are
 * dropped.  This is normally only called by jstop.  In this case, the env
 * passed in is the empty one (since there would have been no device matches).
 */

jdrop_do(env)

  VIDSEnvironment		*env;
  
{
  VIDSEnvironment		*empty_env;	/* temp ptr for empty slot */
  VIDSEnvironment		*e, *next;	/* temp ptrs for loop */
  TAEVariable	*v, *GetVariable();
  char dev_name[MAXDEVNAMESIZE+1];

  v = GetVariable(env, "DEVICE");
  if (v == NULL) return FAIL;
  if (v->v_count == 1) {		/* device name given */
    UpperCase(SVAL(*v,0), dev_name);
    if (EQUAL(dev_name, "ALL")) {	/* drop all devices */
      e = env->next;			/* env is the empty one */
      while (e != env) {
        next = e->next;
        drop_device(e);
        free(e);
        e = next;
      }
      env->next = env;
      env->prev = env;

      return SUCCESS;
    }
  }

  if (env->devUnit == noUnit)
    ABORT(FAIL,"You must grab a display device first with JGRAB.","VIDS-NODEV");

  drop_device(env);			/* drop the one device only */

  empty_env = env;
  do
  {
    if (env->devUnit == noUnit)
      break;				/* found the empty one */
    empty_env = empty_env->next;
  } while (empty_env != env);

  if (empty_env != env)	  /* if current is empty, then whoops!  don't free it */
  {
    empty_env->next->prev = empty_env->prev;	/* unlink from chain */
    empty_env->prev->next = empty_env->next;

    free(empty_env);				/* and trash it */
  }
  InitEnvironment(env);		/* reinitialize old env as the new empty one */

  return SUCCESS;
}

/************************************************************************/
/* Drop a single device.  Should be called only by jdrop_do().
 */
drop_device(env)
VIDSEnvironment *env;
{

  zddclose(env->devUnit);		/* close the display device */
					/* (ignore any errors here) */

  FreeAllRegions(env);			/* deallocate all regions */

  FreeNames(env);			/* and all plane names */

  if (env->buffer != NULL)		/* and the image plane buffer */
    BigFree(env->buffer);

  return SUCCESS;
}
