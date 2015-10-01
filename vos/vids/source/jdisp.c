/* JDISP - VIDS quick display command
 *
 */
#include "VIDSdefs.h"
#define MAXDISP	3	/* maximum number of images allowed on one disp */

/************************************************************************/
int jdisp_do(env)
     
  VIDSEnvironment	*env;		/* the VIDS environment		*/
    
{
  int			nFiles;		/* number of input files	*/
  char			**fileNames;	/* Array of file names		*/
  char			*scale;		/* NONE or FIT for scaling pict.*/
  int			location[2];	/* sl, ss on plane for disp	*/
  int			zoom;		/* zoom factor to apply		*/
  SizeField		fWindow;	/* area of image to display	*/
  int			planes[3];	/* list of planes		*/
  int			nPlanes;	/* number of planes on list	*/
  int			status;		/* status holder		*/

  status = GetJdispParms(env, &fileNames, &nFiles, &fWindow,
  		       &scale, location, &zoom);
  if (status != SUCCESS) return FAIL;
  status = OpenJdispFiles(env, fileNames, nFiles, planes, &nPlanes);
  if (status != SUCCESS) return FAIL;
  if (nPlanes == 1) {
    if (env->isPseudo)		/* if already in pseudo mode, leave it there */
      SetPseudoMode(env);
    else
      SetBWMode(env);
  }
  else
    SetColorMode(env);
  SetLocations(env, location, planes, nPlanes);
  ZoomAll(env, &fWindow, scale, zoom, planes, nPlanes);
  status = Redraw(env);
  switch (nFiles)
  {
    case 3 : CloseFile(env->planes[env->blueIMP].file);
    case 2 : CloseFile(env->planes[env->greenIMP].file);
    case 1 :
      if (nPlanes == 1)
        CloseFile(env->planes[env->bwIMP].file);
      else
        CloseFile(env->planes[env->redIMP].file);
  }
  return status;
}
/************************************************************************/
int GetJdispParms(env, fileNames, nFiles, fWindow, scale, location, zoom)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  char			***fileNames;	/* Array of file names		*/
  int			*nFiles;	/* number of input files	*/
  SizeField		*fWindow;	/* area of image to display	*/
  char			**scale;	/* NONE or FIT for scaling pict.*/
  int			location[2];	/* sl, ss on plane for disp	*/
  int			*zoom;		/* zoom factor to apply		*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  TAEVariable		*GetVariable();

  v = GetVariable (env, "INP");
  if (v == NULL) return FAIL;
  *nFiles = v->v_count;
  *fileNames = (char **)v->v_cvp;

  v = GetVariable (env, "SL");
  if (v == NULL) return FAIL;
  fWindow->sl = IVAL(*v, 0);
  if ((*nFiles == 0) && (v->v_default == 1)) fWindow->sl = 0;

  v = GetVariable (env, "SS");
  if (v == NULL) return FAIL;
  fWindow->ss = IVAL(*v, 0);
  if ((*nFiles == 0) && (v->v_default == 1)) fWindow->ss = 0;

  v = GetVariable (env, "NL");
  if (v == NULL) return FAIL;
  if (v->v_count == 1)
    fWindow->nl = IVAL(*v, 0);
  else
    fWindow->nl = 0;

  v = GetVariable (env, "NS");
  if (v == NULL) return FAIL;
  if (v->v_count == 1)
    fWindow->ns = IVAL(*v, 0);
  else
    fWindow->ns = 0;

  v = GetVariable (env, "SCALE");
  if (v == NULL) return FAIL;
  *scale = SVAL(*v, 0);
  if ((*nFiles == 0) && (v->v_default == 1)) *scale = NULL;

  v = GetVariable (env, "LOCATION");
  if (v == NULL) return FAIL;
  if ((*nFiles == 0) && (v->v_default == 1))
    location[0] = location[1] = 0;
  else
  {
    location[0] = IVAL(*v, 0);
    location[1] = IVAL(*v, 1);
  }

  v = GetVariable (env, "ZOOM");
  if (v == NULL) return FAIL;
  *zoom = (v->v_count == 1) ? IVAL(*v, 0) : 0;

  return SUCCESS;
}
/************************************************************************/
int OpenJdispFiles(env, fileNames, nFiles, planeList, nPlanes)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  char			*fileNames[];	/* in: Array of file names	*/
  int			nFiles;		/* in: number of input files	*/
  int			planeList[];	/* out: list of planes to use	*/
  int			*nPlanes;	/* out: number of planes onlist	*/
{
  FileInfo	*RFile,*GFile,*BFile;	/* file information structure	*/
  PlaneInfo *rPlane,*gPlane,*bPlane,*bwPlane; /* ptrs to plane info	*/
  static char		*nomem=		/* insuff mem message		*/
                    "Insufficient memory to prepare file information";
  static char		*opfail=	/* open failed message		*/
                    "Sorry, could not open the input file";
  FileInfo 		*findFileSlot();

  rPlane = &env->planes[env->redIMP];
  gPlane = &env->planes[env->greenIMP];
  bPlane = &env->planes[env->blueIMP];
  bwPlane = &env->planes[env->bwIMP];
  *nPlanes = 0;
  switch (nFiles)
  {
    case 3 :
        BFile = findFileSlot(env,fileNames[2]);
        if (BFile == NULL) ABORT(FAIL, nomem, "VIDS-NOMEM");
        if (OpenFile(BFile) != SUCCESS) ABORT(FAIL, opfail, "VIDS-OPENFAIL");
        TieImp(bPlane, BFile, 1);
        planeList[(*nPlanes)++] = env->blueIMP;
    case 2 :
        GFile = findFileSlot(env,fileNames[1]);
        if (GFile == NULL) ABORT(FAIL, nomem, "VIDS-NOMEM");
        if (OpenFile(GFile) != SUCCESS) ABORT(FAIL, opfail, "VIDS-OPENFAIL");
        TieImp(gPlane, GFile, 1);
        planeList[(*nPlanes)++] = env->greenIMP;
        RFile = findFileSlot(env,fileNames[0]);
        if (RFile == NULL) ABORT(FAIL, nomem, "VIDS-NOMEM");
        if (OpenFile(RFile) != SUCCESS) ABORT(FAIL, opfail, "VIDS-OPENFAIL");
        TieImp(rPlane, RFile, 1);
        planeList[(*nPlanes)++] = env->redIMP;
        break;
    case 1 :
        RFile = findFileSlot(env,fileNames[0]);
        if (RFile == NULL) ABORT(FAIL, nomem, "VIDS-NOMEM");
        if (OpenFile(RFile) != SUCCESS) ABORT(FAIL, opfail, "VIDS-OPENFAIL");
        if (RFile->nb >= 3)
        {
          TieImp(bPlane,  RFile, 3);
          planeList[(*nPlanes)++] = env->blueIMP;
        }
        if (RFile->nb >= 2)
        {
          TieImp(gPlane, RFile, 2);
          planeList[(*nPlanes)++] = env->greenIMP;
          TieImp(rPlane, RFile, 1);
          planeList[(*nPlanes)++] = env->redIMP;
        }
        else 		/* only one band...	*/
        {
          TieImp(bwPlane, RFile, 1);
          planeList[(*nPlanes)++] = env->bwIMP;
        }
        break;
    case 0 :
        if (env->isColor)
        {
          planeList[0] = env->redIMP;
          planeList[1] = env->greenIMP;
          planeList[2] = env->blueIMP;
          *nPlanes = 3;
        }
        else
        {
          planeList[0] = env->bwIMP;
          *nPlanes = 1;
        }
  }
  return SUCCESS;
}
/************************************************************************/
int SetLocations(env, location, planes, nPlanes)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int			location[2];	/* sl, ss on plane for disp	*/
  int			planes[];	/* array of plane numbers	*/
  int			nPlanes;	/* number of planes on list	*/
{
  int i;
  PlaneInfo	*p;

  if (location[0] < 0 || location[0] > env->nlMax)
  {
    NotifyUser(Inform, "VIDS-INVLOC",
		"Invalid line in LOCATION parameter; default used.");
    location[0] = 0;
  }
  if (location[1] < 0 || location[1] > env->nsMax)
  {
    NotifyUser(Inform, "VIDS-INVLOC",
		"Invalid sample in LOCATION parameter; default used.");
    location[1] = 0;
  }

  for (i = 0; i < nPlanes; i++)
  {
    p = &env->planes[planes[i]];
    if (location[0] != 0) p->accessWindow.sl = location[0];
    if (location[1] != 0) p->accessWindow.ss = location[1];
    p->accessWindow.nl = p->accessWindow.ns = 0;
  }
  return SUCCESS;
}
/************************************************************************/
int ZoomAll(env, size, scale, zoom, planes, nPlanes)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  SizeField		*size;		/* image area of interest	*/
  char			*scale;		/* NONE or FIT for scaling pict.*/
  int			zoom;		/* zoom factor to apply		*/
  int			planes[];	/* array of planes to zoom	*/
  int			nPlanes;	/* number of planes to zoom	*/
{
  int		i;
  PlaneInfo	*p;

  for (i = 0; i < nPlanes; i++)
  {
    p = &env->planes[planes[i]];
    if (size->sl != 0) p->imageWindow.sl = size->sl;
    if (size->ss != 0) p->imageWindow.ss = size->ss;
    if (size->nl != -1) p->imageWindow.nl = size->nl;
    if (size->ns != -1) p->imageWindow.ns = size->ns;
  }
  if ((zoom == 0) && (scale != NULL))	/* if zoom parm not given, check for zoom to fit	*/
  {
    if (EQUAL(scale, "FIT"))	    /* find the minimum zoom required (ie, */
    {				    /* the zoom for the largest plane) and */
      zoom = 32767;		    /* set all planes to the same zoom to  */
      for (i = 0; i < nPlanes; i++) /* keep the color images registered.   */
      {
        p = &env->planes[planes[i]];
        if (size->sl == 0) p->imageWindow.sl = 1;
        if (size->ss == 0) p->imageWindow.ss = 1;
        if (size->nl == -1) p->imageWindow.nl = 0;
        if (size->ns == -1) p->imageWindow.ns = 0;
        zoom = MIN(ZoomToFit(env, planes[i]), zoom);
      }
    }
    else if (EQUAL(scale, "NONE"))	/* If no fitting needed, so set	*/
    {					/* the zoom to one.		*/
      zoom = 1;
    }
  }

  for (i = 0; i < nPlanes; i++)
  {
    p = &env->planes[planes[i]];
    if (zoom != 0) p->softZoom = zoom;
    p->subPixel.left = p->subPixel.top = 0;
  }
  return SUCCESS;
}

