#include "VIDSdefs.h"
#include "verrdefs.h"		/* vrdi error definitions		*/
#define DELAY 0.5		/* seconds to delay before update	*/
/* *************  Global variables **************************************/
/* These variables are needed to keep track of updates from image files	*/
struct updateInfo
{
  Rect		loaded;		/* bounding rect in IMP of image data	*/
  Rect		file;		/* Rect in imp of entire file (even if	*/
  				/* not loaded)				*/
} *updates;
int	     nlVideo,nsVideo;	/* lines/samps of video available	*/
SizeField    *savedAW,*savedIW;	/* saved access/image windows		*/
/************************************************************************/
/* jpan_do -- code for jpan command.  Jpan (pronounced "japan" ?) will
 * pan through an image, either on the display device or hitting the
 * file if necessary.
 */
int jpan_do(env)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int		nPlanes;	/* number of planes to pan around in	*/
  int	     planes[MAXPLANES];	/* list of planes in which to pan	*/
  int		unit,status;	/* device unit number, status holder	*/
  int		curs;		/* cursor number			*/
  int		i;		/* increment variable			*/
  int		x,y,line,samp;	/* x,y locations of cursor/window, etc	*/
  int		lastx, lasty;	/* saved x,y locations			*/
  int		zoom;		/* current software zoom		*/
  ButtonAction	action;		/* which button was pushed		*/
  Boolean	independent;	/* True if planes can pan independently	*/
  Boolean	hitFile;	/* True if we should update from disk	*/
  Point		*savedLocs;	/* ptr to array of saved initial locs	*/
  float		xslope,xoffs;	/* slope/offset to apply to cursor pos	*/
  float		yslope,yoffs;	/* ditto for y direction		*/
  PlaneInfo	*p;		/* ptr to a plane information block	*/
  FileInfo	*f;		/* ptr to a file information block	*/
  Boolean	ButtonDown();	/* Function indicating a button is down	*/
  Boolean	pannedGraf;	/* True if graphics plane is panned	*/
    
  unit = env->devUnit;
  curs = env->cursor.number;
  if (GetPanParms(env, &hitFile, planes, &nPlanes, &line, &samp) != SUCCESS)
      return FAIL;

/* find out if each plane has a separate display window	(can	*/
/* pan independently).  If not, notify the user and turn off	*/
/* vrdi warnings.						*/

  status = zddinfo(unit, 20, 1, &independent);
  if (! independent)
  {
    NotifyUser(Inform,"","**** All planes must be panned together.");
    zdeaction(1, 2, 2);
  }
/*		*********************					*/
/* If user gave	either line or samp, set the window for each plane	*/
/* to that location, and return						*/
  if ((line != 0) || (samp != 0))
  {
    if (!hitFile)
      zddbatch(unit, True);
    for (i = 0; i < nPlanes; i++)
    {
      p = &env->planes[planes[i]];
      if (hitFile)		/* if updating from file, move	speci-	*/
      {				/* fied loc in file to top left of disp	*/
        if (samp > 0) p->imageWindow.ss = samp;
        if (line > 0) p->imageWindow.sl = line;
        p->imageWindow.nl = Zoom(env->nlMax, -(p->softZoom));
        p->imageWindow.ns = Zoom(env->nsMax, -(p->softZoom));
        p->accessWindow.sl = p->accessWindow.ss = 1;
        p->accessWindow.nl = p->accessWindow.ns = 0;
        p->subPixel.top = p->subPixel.left = 0;
        LoadIMP(env, planes[i]);
      }
      else
      {
        x = (samp <= 0) ? zdsdwsamp(unit,planes[i]) : samp;
        y = (line <= 0) ? zdsdwline(unit,planes[i]) : line;
        status = zdidwset(unit, planes[i], x, y);
        if ((status != SUCCESS) && (status != MUSTSETDW))
        {
          zddbatch(unit, False);
          zdeaction(2, 2, 2);		/* reset vrdi error messages	*/
          ABORT(FAIL, "Sorry, unable to pan image plane", "VIDS-VRDIERR");
        }
      }
    }
    zddbatch(unit, False);
    zdeaction(2,2,2);		/* reset vrdi error messages	*/
    return SUCCESS;
  }
/*		*********************					*/
/* Allocate an array to hold the initial positions in case the pan is	*/
/* canceled.  There is no need to check for a memory allocation failure	*/
/* here, because we want to pan anyway, and the NULL pointer serves	*/
/* as a flag that it can't be canceled.					*/
  savedLocs = (Point *) malloc(nPlanes * sizeof(Point));
  if (savedLocs != NULL)
  {
    for (i = 0; i < nPlanes; i++)			/* save starting */
    {							/* positions	 */
      savedLocs[i].h = zdsdwsamp(unit,planes[i]);
      savedLocs[i].v = zdsdwline(unit,planes[i]);
    }
  }
/* Now calculate a slope/offset to apply to the cursor position so	*/
/* that the start and end points of the pan are where we want them.	*/
/* The pan intentionally wraps around to keep away ugly jumps and to	*/
/* allow panning (for color registration, etc.) even when the IMP size	*/
/* is the same as the video size, except when updating from a file, in	*/
/* which case the pan is restricted to go only to the end of file.	*/

  nsVideo = VideoSamps(env);		/* fill in global video size 	*/
  nlVideo = VideoLines(env);		/* for use in updates below.	*/
  
  x = y = 0;
  zoom = 1;
  if (hitFile)		/* if we are hitting a file, use the size of 	*/
  {			/* file instead of the imps for pan range	*/
    for (i = 0; i < nPlanes; i++)
    {
      p = &env->planes[planes[i]];
      if ((f = p->file) != NULL)
      {
        zoom = p->softZoom;
        x = Zoom(f->ns, zoom); y = Zoom(f->nl, zoom);
        break;
      }
    }
    if (f == NULL)
    {
      NotifyUser(Inform,"","No files are active; panning locally to display");
      hitFile = False;
    }
  }
  xoffs = (float) MAX(x,env->nsMax);	/* use xoffs,yoffs temp. to	*/
  yoffs = (float) MAX(y,env->nlMax);	/* hold nl,ns maximum.		*/

  if (hitFile)	/* different calcs per above comment if update from file */
  {
    xslope = ((float)Zoom(nsVideo,-zdszoom(unit,p->imp)) - xoffs) /
							((float)nsVideo - 1.0);
    xoffs =  1 - xslope * (float)nsVideo;
    yslope = ((float)Zoom(nlVideo,-zdszoom(unit,p->imp)) - yoffs) /
							((float)nlVideo - 1.0);
    yoffs =  1 - yslope * (float)nlVideo;
  }
  else
  {
    xslope = (1.0 - xoffs) / ((float)nsVideo - 1.0);
    xoffs =  xoffs - xslope;
    yslope = (1.0 - yoffs) / ((float)nlVideo - 1.0);
    yoffs = yoffs - yslope;
  }
  zdcset(unit, curs, nsVideo, nlVideo);     /* pos cursor at max spot,	*/
  					    /* so pan is at upper left.	*/

/* Now start the panning loop, until the button is pressed.		*/
  lastx = lasty = 0;
  NotifyUser(Inform,"","Press any button to terminate panning");
  ButtonMessage(env, True, True, False);
  if (hitFile) StartUpdate(env, planes, nPlanes);
  StartTimer();			/* Use delay before updating,		*/
  				/* to allow smoother cursor movement	*/
  while (!ButtonDown(env, &action))
  {
    status = zdclocation(unit, curs, &samp, &line); /* get cursor pos.	*/
    if (status != SUCCESS)
    {
      if (hitFile)			/* clean up update structures	*/
        EndUpdate(env, planes, nPlanes, CANCEL);
      if (savedLocs != NULL) free(savedLocs);
      zdeaction(2, 2, 2);		/* reset vrdi error messages	*/
      ABORT(FAIL, "Sorry, could not get the cursor location", "VIDS-VRDIERR");
    }
    x = (int) (xslope * samp + xoffs);		/* scale the cursor	*/
    y = (int) (yslope * line + yoffs);		/* for zd routines	*/
    if (hitFile)
    {
      x = (((x + zoom - 1) / zoom ) * zoom) - zoom + 1; /* prevent half	*/
      y = (((y + zoom - 1) / zoom ) * zoom) - zoom + 1; /* pixels	*/
    }
    if ((x != lastx) || (y != lasty))		/* Don't pan if cursor	*/
    {						/* didn't move		*/
      zddbatch(unit, True);
      for (i = 0; i < nPlanes; i++)
      {
        line = IntToRange(y, 1, env->nlMax);
        samp = IntToRange(x, 1, env->nsMax);
        status = zdidwset(unit, planes[i], samp, line);
        if ((status != SUCCESS) && (status != MUSTSETDW))
        {
          if (hitFile) EndUpdate(env, planes, nPlanes, CANCEL);
          zddbatch(unit, False);
          if (savedLocs != NULL) free(savedLocs);
          zdeaction(2, 2, 2);		/* reset vrdi error messages	*/
          ABORT(FAIL, "Sorry, unable to pan image plane", "VIDS-VRDIERR");
        }
      }
      zddbatch(unit, False);
      StartTimer();			/* reset timer because updated	*/
    }
    else if (TimeElapsed(DELAY) && (int)hitFile)
    {
      DoUpdate(env, planes, nPlanes, x, y);
      zdcset(unit, curs, samp, line);
    }
    lastx = x; lasty = y;
  }
  if (hitFile)				/* clean up update structures	*/
    EndUpdate(env, planes, nPlanes, action);
    
  if (!independent)			/* If image planes cannot pan	*/
  {					/* independently, then all the	*/
    pannedGraf = True;			/* histograms need to be bashed	*/
  }					/* as if graph plane was panned	*/
  else
  {
    for (i = 0; i < nPlanes; i++)	/* See if the graphics plane	*/
    {					/* was panned, and if so, we	*/
      if (planes[i] == env->grafIMP)	/* may have to bash the histo.	*/
      {
        pannedGraf = True;
        break;
      }
    }
  }
  if (action == CANCEL)
  {
    if (savedLocs == NULL)
    {
      NotifyUser(Inform,"VIDS-INSUFMEM", "Sorry, not enough memory to cancel");
      BashHists(env, planes, nPlanes, pannedGraf);
    }
    else
    {
      zddbatch(unit, True);
      for (i = 0; i < nPlanes; i++)
        zdidwset(unit, planes[i], savedLocs[i].h, savedLocs[i].v);
      zddbatch(unit, False);
    }
  }
  else	/* Not canceled, so all histogram locations are invalid	*/
  {
    BashHists(env, planes, nPlanes, pannedGraf);
  }

  if (savedLocs != NULL) free(savedLocs);
  zdeaction(2, 2, 2);			/* reset vrdi error messages	*/
  for (i = 0; i < nPlanes; i++)			/* make sure saved locs	*/
  {						/* are correct.		*/
    p = &env->planes[planes[i]];		/* Ignore errors and try to */
    zdidwlocation(unit, planes[i], &x, &y);	/* do as many as possible.  */
    if (hitFile)
    {
      NotifyUser(Inform,"","Top left of plane %d (line,samp) = (%d, %d)",
      		 planes[i], p->imageWindow.sl, p->imageWindow.ss);
    }
    else if (independent)		/* Print out final positions.	*/
      NotifyUser(Inform,"","Image plane %d panned to (line,samp) = (%d,%d)",
      		 planes[i],y,x);
  }
  if (!independent)			/* Print out final positions.	*/
    NotifyUser(Inform,"","Image planes panned to (%d,%d)",y,x);
  return SUCCESS;
}
/************************************************************************/
/* StartUpdate sets up the info needed to update the plane from disk
 * files.
 */
int StartUpdate(env,imps,nimps)
  VIDSEnvironment	*env;
  int			imps[];
  int			nimps;
{
  PlaneInfo *p;
  FileInfo *f;
  int i,zoom,dh,dv;
  Rect r;

  updates = malloc(nimps * sizeof(struct updateInfo));
  if (updates == NULL)
    ABORT(FAIL,"Not enough memory to perform update from disk","VIDS-INSUFMEM");
  savedAW = malloc(nimps * sizeof(SizeField));
  savedIW = malloc(nimps * sizeof(SizeField));
  for (i = 0; i < nimps; i++)
  {
    p = &env->planes[imps[i]];
    f = p->file;
    if (f == NULL)
    {
      SetRect(&updates[i].file, 0, 0, -1, -1);
    }
    else
    {
      if (savedAW != NULL)
        BlockMove(&p->accessWindow, &savedAW[i], sizeof(SizeField));
      if (savedIW != NULL)
        BlockMove(&p->imageWindow, &savedIW[i], sizeof(SizeField));
      if (OpenFile(f) != SUCCESS)
      {
        NotifyUser(Inform,"","Sorry, %s could not be opened.",f->filename);
        SetRect(&updates[i].file, 0, 0, -1, -1);
        continue;
      }
      zoom = p->softZoom;
      SetRect(&updates[i].file, 1, 1, Zoom(f->nl, zoom), Zoom(f->ns, zoom));
      SizeToRect(&p->accessWindow, &updates[i].loaded);
      OffsetRect(&updates[i].loaded,
          Zoom((p->imageWindow.sl - 1), zoom) - p->accessWindow.sl + 1,
	  Zoom((p->imageWindow.ss - 1), zoom) - p->accessWindow.ss + 1);
      zdidwset(env->devUnit, imps[i], 1, 1);
    }
  }
  DoUpdate(env, imps, nimps, 1, 1);
  return SUCCESS;
}
/************************************************************************/
/* DoUpdate calculates the regions on the display which need to be
 * updated from disk, and causes them to be read in.  This routine
 * stores the area displayed on the plane, so it could wrap around
 * the plane (eg, if the plane is 512x512, the window rectangle could
 * still be something like (500,500,1012,1012)).  The upper left corner
 * is always within the plane, however.
 */
int DoUpdate(env, imps, nimps, x, y)
  VIDSEnvironment	*env;
  int			imps[];		/* list of imps to update	*/
  int			nimps;		/* number of imps in array	*/
  int			x,y;		/* new x,y location 		*/
{
  int i,imp;
  int		left,right,top,bot;	/* temp rect size holders	*/
  int		hMid,vMid;		/* horiz/vertic middle 		*/
  FileInfo	*f;
  PlaneInfo	*p;
  Rect		r,curView,*fileRect;
  Boolean	updated;

  updated = False;
  for (i = 0; i < nimps; i++)
  {
    imp = imps[i];
    if (env->isColor)
    {
      if ((imp != env->redIMP) && (imp != env->greenIMP) &&
          (imp != env->blueIMP) && (imp != env->grafIMP)) continue;
    }
    else
    {
      if ((imp != env->bwIMP) &&  (imp != env->grafIMP)) continue;
    }
    p = &env->planes[imp];
    if ((f = p->file) == NULL) continue;     /* if no file, do nothing	*/
/* Check the intersection of what is visible with what is loaded, and	*/
/* if the intersection is equal to either of these, then no update is	*/
/* needed.								*/
    fileRect = &updates[i].file;
    bot =   y + Zoom(nlVideo, -zdszoom(env->devUnit,imp)) - 1;
    right = x + Zoom(nsVideo, -zdszoom(env->devUnit,imp)) - 1;
    SetRect(&curView, y, x, bot, right);		/* visible area	*/
    SectRect(fileRect, &curView, &r);	/* vis area intersected w/file	*/
    if (RectInRect(&r, &updates[i].loaded))
        continue;			 /* if already visible, do nothing*/
/* Now we know we need to do updates.  There are four rectangles to	*/
/* be updated, the upper right, lower right, lower left, and the	*/
/* upper left. (They cannot be combined because of image plane wrap.)	*/
    NotifyUser(Inform,"","Updating plane %d from the file . . .",imp);
    updated = True;
    left = ((curView.left + curView.right - env->nsMax) / 2) + 1;
    right = left + env->nsMax - 1;
    top = ((curView.top + curView.bottom - env->nlMax) / 2) + 1;
    bot = top + env->nlMax - 1;
    hMid = left + env->nsMax - IntToRange(left, 1, env->nsMax);
    vMid = top + env->nlMax - IntToRange(top, 1, env->nlMax);

    SetRect(&r, top, left, vMid, hMid);			/* top left	*/
    UpdateRect(env, imp, &r, f, fileRect);
    SetRect(&r, top, hMid + 1, vMid, right);		/* top right	*/
    UpdateRect(env, imp, &r, f, fileRect);
    SetRect(&r, vMid + 1, left, bot, hMid);		/* low left	*/
    UpdateRect(env, imp, &r, f, fileRect);
    SetRect(&r, vMid + 1, hMid + 1, bot, right);	/* low right	*/
    UpdateRect(env, imp, &r, f, fileRect);

/* Now everything should be redrawn, so update the global updates	*/
/* structure.								*/
    SetRect(&updates[i].loaded, top, left, bot, right);
    SectRect(&updates[i].loaded, fileRect, &updates[i].loaded);
  }
  if (updated)
  {
    NotifyUser(Inform,"","Finished updating.");
    ButtonMessage(env, True, True, False);
  }
  return SUCCESS;
}
/************************************************************************/
/* UpdateRect takes a given rect on the image plane, intersects it with
 * the actual image file data, and loads whatever is necessary into the
 * image plane rect.
 */
UpdateRect(env, imp, impRect,theFile,fileRect)
  VIDSEnvironment	*env;	/* environment block			*/
  int			imp;	/* imp to update			*/
  Rect		*impRect;	/* location on imp to be updated	*/
  FileInfo	*theFile;	/* ptr to file information		*/
  Rect		*fileRect;	/* rectangle of image file data on imp	*/
{
  PlaneInfo	*p;
  Rect r,r2;
  int  zoom,dif;
  MessageType messages,GetMessage();	/* saved value of message level	*/

  SectRect(impRect, fileRect, &r);	/* intersection of area to be 	*/
  if (EmptyRect(&r)) return;		/* updated and image file	*/
  p = &env->planes[imp];
  HomeRect(&r,&r2,env->nlMax,env->nsMax); /* shift to actual imp coords	*/
  RectToSize(&r2, &p->accessWindow);

  zoom = p->softZoom;				/* If zooming up, check	*/
  OffsetRect(&r, 1 - fileRect->top,		/* for fractional 	*/
                 1 - fileRect->left);		/* pixels and store the	*/
  if (zoom > 1)					/* info.		*/
  {
    p->subPixel.left = zoom - ((r.left - 1) % zoom);
    p->subPixel.top  = zoom - ((r.top  - 1) % zoom);
  }
  else
  {
    p->subPixel.left = p->subPixel.top = 0;
  }
  ZoomRect(&r, -zoom);
  RectToSize(&r, &p->imageWindow);

  messages = GetMessage();
  if (messages != Verbose)		/* If not Verbose messages,	*/
    SetMessage(Silent);			/* make the updates silent.	*/
  LoadIMP(env, imp);
  SetMessage(messages);
  return;
}
/************************************************************************/
/* EndUpdate performs necessary cleanup for a software pan, such as
 * freeing up any allocated structures, and reloads the contents of
 * the plane to be contiguous, and resets the plane display window
 * to provide accepted view.
 */
EndUpdate(env, imps, nPlanes, action)
  VIDSEnvironment	*env;
  int			imps[];
  int			nPlanes;
  ButtonAction		action;		/* ACCEPT or CANCEL		*/
{
  int i,x,y;
  FileInfo	*f;
  PlaneInfo	*p;
  Rect		*l;		/* ptr to updates[i].loaded		*/

  if (updates == NULL) return;		/* If no saved info, do nothing	*/
  for (i = 0; i < nPlanes; i++)
  {
    p = &env->planes[imps[i]];
    if ((f = p->file) != NULL) CloseFile(f);	/* Close the file	*/
    x = y = 1;				/* New display window location.	*/
    if (action == CANCEL)		/* If canceled, restore the 	*/
    {					/* previous view.		*/
      if (savedAW != NULL)
        BlockMove(&savedAW[i], &p->accessWindow, sizeof(SizeField));
      if (savedIW != NULL)
        BlockMove(&savedIW[i], &p->imageWindow,  sizeof(SizeField));
    }
    else
    {
      l = &updates[i].loaded;		/* accepted, so	save this view	*/
      if (zdszoom(env->devUnit,imps[i]) > 1)
      {
        if (zdidwlocation(env->devUnit, imps[i], &x, &y) == SUCCESS)
        {
          x = IntToRange(x, l->left, l->left + env->nsMax - 1) - l->left + 1;
          y = IntToRange(y, l->top, l->top + env->nlMax - 1) - l->top + 1;
        }
      }
      p->accessWindow.sl = p->accessWindow.ss = 1;
      p->accessWindow.nl = p->accessWindow.ns = 0;
      ZoomRect(l, -(p->softZoom));
      RectToSize(l, &p->imageWindow);
    }
    zdidwset(env->devUnit, imps[i], x, y);
    p->subPixel.left = p->subPixel.top = 0;
    LoadIMP(env, imps[i]);
  }
  free(updates);
  if (savedAW != NULL) free(savedAW);
  if (savedIW != NULL) free(savedIW);

  return;
}
/************************************************************************/
/* GetPanParms processes the parameters for the jpan command.
 */
int GetPanParms(env, hitFile, planes, nPlanes, line, samp)
  VIDSEnvironment	*env;
  Boolean	*hitFile;	/* True if we should update from disk	*/
  int		*nPlanes;	/* number of planes to pan around in	*/
  int		*planes;	/* list of planes in which to pan	*/
  int		*line,*samp;	/* requested line and sample for pan	*/
{
  FileInfo	*theFile;
  char		*fileName;
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  float		low, high;	/* the low and high values for the file	*/
  TAEVariable *GetVariable();
  
  *line = *samp = 0;		/* set up defaults		*/
  *hitFile = False;

  if (GetPlaneList(env, planes, nPlanes, False) != SUCCESS) return FAIL;

  v = GetVariable(env, "SOURCE");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v, 0), "FILE"))
    *hitFile = True;
  
  v = GetVariable(env, "LOCATION");
  if (v == NULL) return FAIL;
  if (v->v_count == 2)
  {
    *line = IVAL(*v, 0);
    *samp = IVAL(*v, 1);
  }
  return SUCCESS;
}
/************************************************************************/
/* BashHists invalidates all the histograms in all the planes on our
 * list.  If pannedGraf is True, then all histograms must be bashed,
 * not just those on the list.
 */
int BashHists(env, planes, nPlanes, pannedGraf)
  VIDSEnvironment	*env;
  int			planes[];
  int			nPlanes;
  Boolean		pannedGraf;
{
  int i;

  if (pannedGraf)
  {
    for (i = 0; i < MAXPLANES; i++)
      InvalHistLoc(&env->planes[i]);
  }
  else
  {
    for (i = 0; i < nPlanes; i++)
      InvalHistLoc(&env->planes[planes[i]]);
  }
  return SUCCESS;
}
