#include "VIDSdefs.h"
#include "verrdefs.h"	/* VRDI error code definitions	*/

/* JGRAB -- try to grab (allocate) a display device, and initialize the info
 * about it in the current environment structure.  Those things which can have
 * a default value are assumed to have already been set to what we want.
 */

Region *PointsToRegion();

jgrab_do(env)

  VIDSEnvironment		*env;
  
{
  VIDSEnvironment		*new_env;	/* temp ptr for new free slot */
  TAEVariable			*v;		/* temp VARIABLE pointer      */
  char				*devName;	/* name of the current device */
  int				status;		/* general status indicator   */
  int				unit;		/* local device unit holder   */
  long				i;		/* temporary variable	      */
  static int		   config[4]={0,0,0,0};	/* default configuration      */
  int				len;
  TAEVariable *GetVariable();
  unsigned char *BigMalloc();

  if (env->devUnit != noUnit)
    return SUCCESS;			/* no-op if device already active */

  new_env = malloc(sizeof(VIDSEnvironment));	/* allocate new empty env */
  if (new_env == NULL)
      ABORT(FAIL,"Insufficient memory to allocate new environment",
		"VIDS-INSUFMEM");
  InitEnvironment(new_env);

  new_env->next = env->next;		/* link it into the circular chain */
  new_env->prev = env;
  env->next->prev = new_env;
  env->next = new_env;

  v = GetVariable(env, "DEVICE");
  if (v == NULL) return FAIL;
  if (v->v_count == 1)
  {
    devName = SVAL(*v, 0);
    UpperCase(devName, devName);
  }
  else
    devName = NULL;

  zdeaction(1, 1, 1);	/* make sure ZD routines just return status */
  if (devName == NULL)
    status = zddunit(&unit);
  else
    status = zddnamedunit(&unit, devName);

  if (status != SUCCESS)
  {
    if (devName == NULL)
      ABORT(FAIL,"Sorry, please specify a device name or allocate a device with USE",
            "VIDS-NODEV");
    status = zddallocate(devName);
    if ((status != DEVALLOC) && (status != SUCCESS))
    { 
      zdesignal(status);
      ABORT(status, "Sorry, could not allocate device", "VIDS-VRDIERR");
    }
    status = zddnamedunit(&unit, devName);
    if (status != SUCCESS)
    {
      zdesignal(status);
      ABORT(status, "Sorry, unable to obtain unit number","VIDS-VRDIERR");
    }
  }
  zdeaction(2, 2, 2);
  status = zddopen(unit);
  if (status != SUCCESS && status != DEVOPEN)
    ABORT(status, "Sorry, unable to open device.","VIDS-VRDIERR");

  status = zddactivate(unit, TRUE);
  if (status != SUCCESS && status != DEVACTIVE)
    ABORT(status, "Sorry, unable to activate device.","VIDS-VRDIERR");

  status = zddconfigure(unit, config);
  if (status != SUCCESS)
    ABORT(status, "Sorry, unable to configure device.","VIDS-VRDIERR");

  zddname(unit, 1, env->devName, MAXDEVNAMESIZE, &len); /* get device name */
  env->devName[len] = '\0';			/* null terminate the string */
  UpperCase(env->devName, env->devName);

  env->devUnit = unit;				/* we have the device; now set*/
  zddinfo (unit, 4, 1, &env->nimps);		/* up all the saved constants */
  zddinfo (unit, 5, 1, &env->nlMax);		/* load up the max number of  */
  zddinfo (unit, 6, 1, &env->nsMax);		/* IMPs, max nl, max ns	      */
  zddinfo (unit, 30, 1, &i);			/* graphics overlay available?*/
  if (i == 1)						/* yes */
    zddinfo (unit, 34, 1, &env->grafIMP);	/* ask vrdi which is graphics */
  else							/* no */
    env->grafIMP = 1;				/* use plane 1 for graphics   */
  zddinfo(unit, 64, 1, &env->nButtons);		/* Number of mouse buttons    */
  zddinfo(unit, 10, 1, &i);			/* Color, PS, or BW?	      */
  if (i == 1)					/* Color */
  {
    env->isColor = TRUE;
    env->isBW = FALSE;
    env->isPseudo = FALSE;
  }
  else if (i == 2)					/* Pseudocolor */
  {
    env->isColor = FALSE;
    env->isBW = FALSE;
    env->isPseudo = TRUE;
  }
  else if (i == 3)					/* BW */
  {
    env->isColor = FALSE;
    env->isBW = TRUE;
    env->isPseudo = FALSE;
  }

  if (env->nimps < 3)				/* For devices with less than	*/
    env->blueIMP = 1;				/* 3 image planes, put blue and	*/
  if (env->nimps < 2)				/* possibly green on plane 1	*/
    env->greenIMP = 1;
  InitLuts();
  InitGLuts(env);
  InitPSTable();

  if (NewName(env, env->redIMP, "RED") != SUCCESS)
    return FAIL;
  if (NewName(env, env->greenIMP, "GREEN") != SUCCESS)
    return FAIL;
  if (NewName(env, env->blueIMP, "BLUE") != SUCCESS)
    return FAIL;
  if (NewName(env, env->bwIMP, "BW") != SUCCESS)
    return FAIL;
  if (NewName(env, env->grafIMP, "GRAPHICS") != SUCCESS)
    return FAIL;

  status = SetDefaultRegions(env);
  if (status != SUCCESS) return status;

  for (i=0; i<MAXPLANES; i++)
    InvalHist(&env->planes[i]);

  if (env->buffer != NULL)				/* just in case ...	*/
      BigFree(env->buffer);

  i = env->nlMax * env->nsMax;
  env->buffer = BigMalloc(i);
  if (env->buffer == NULL)
      ABORT(FAIL, "Insufficient memory for image display buffer.",
                  "VIDS-INSUFMEM");
  env->bufSize = i;
  return SUCCESS;
}



/* SetDefaultRegions -- set up the default system display regions, based
 * on the size of the video.
 */

SetDefaultRegions(env)
  VIDSEnvironment		*env;
{
  Point				points[2];	/* for default region def     */

  points[0].h = 1;		points[0].v = 1;
  points[1].h = env->nsMax;	points[1].v = env->nlMax;
  if (PointsToRegion(env, "FULLSCREEN", Rectangle, points, 2) == NULL)
      ABORT(FAIL, "Unable to define default region; consult system manager",
                  "VIDS-INSUFMEM");

  if (VideoSamps(env) < 1024 || VideoLines(env) < 1024)		/* Lo-res */
  {
    /* Define default regions for histograms */

    points[0].v = 40;		points[1].v = 214;
    points[0].h = 10;		points[1].h = 159;
    if (PointsToRegion(env, "HIST1", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 181;		points[1].h = 330;
    if (PointsToRegion(env, "HIST2", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 352;		points[1].h = 501;
    if (PointsToRegion(env, "HIST3", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    points[0].v = 296;		points[1].v = 470;
    points[0].h = 10;		points[1].h = 159;
    if (PointsToRegion(env, "HIST4", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 181;		points[1].h = 330;
    if (PointsToRegion(env, "HIST5", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 352;		points[1].h = 501;
    if (PointsToRegion(env, "HIST6", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default regions for profile */

    points[0].v = 28;		points[1].v = 228;
    points[0].h = 6;		points[1].h = 506;
    if (PointsToRegion(env, "PRO1", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].v = 284;		points[1].v = 484;
    points[0].h = 6;		points[1].h = 506;
    if (PointsToRegion(env, "PRO2", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Display Transfer Function */

    points[0].v = 116;		points[1].v = 394;
    points[0].h = 106;		points[1].h = 405;
    if (PointsToRegion(env, "DTF", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Color Picker */

    points[0].v = 121;		points[1].v = 391;
    points[0].h = 121;		points[1].h = 391;
    if (PointsToRegion(env, "COLORPICK", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Wedge */

    points[0].v = VideoLines(env)-50;	points[1].v = VideoLines(env);
    points[0].h = 1;			points[1].h = VideoSamps(env);
    if (PointsToRegion(env, "WEDGE", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
  }
  else							/* hi-res */
  {
    /* Define default regions for histograms */

    points[0].v = 80;		points[1].v = 428;
    points[0].h = 20;		points[1].h = 318;
    if (PointsToRegion(env, "HIST1", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 362;		points[1].h = 660;
    if (PointsToRegion(env, "HIST2", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 704;		points[1].h = 1002;
    if (PointsToRegion(env, "HIST3", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    points[0].v = 592;		points[1].v = 940;
    points[0].h = 20;		points[1].h = 318;
    if (PointsToRegion(env, "HIST4", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 362;		points[1].h = 660;
    if (PointsToRegion(env, "HIST5", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].h = 704;		points[1].h = 1002;
    if (PointsToRegion(env, "HIST6", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default regions for profile */

    points[0].v = 56;		points[1].v = 456;
    points[0].h = 12;		points[1].h = 1012;
    if (PointsToRegion(env, "PRO1", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
    points[0].v = 568;		points[1].v = 968;
    points[0].h = 12;		points[1].h = 1012;
    if (PointsToRegion(env, "PRO2", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Display Transfer Function */

    points[0].v = 232;		points[1].v = 788;
    points[0].h = 212;		points[1].h = 810;
    if (PointsToRegion(env, "DTF", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Color Picker */

    points[0].v = 242;		points[1].v = 782;
    points[0].h = 242;		points[1].h = 782;
    if (PointsToRegion(env, "COLORPICK", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");

    /* Define default region for Wedge */

    points[0].v = VideoLines(env)-100;	points[1].v = VideoLines(env);
    points[0].h = 1;			points[1].h = VideoSamps(env);
    if (PointsToRegion(env, "WEDGE", Rectangle, points, 2) == NULL)
        ABORT(FAIL, "Unable to define default region; consult system manager",
                    "VIDS-INSUFMEM");
  }
  return SUCCESS;

}
