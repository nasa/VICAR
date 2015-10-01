#include "VIDSdefs.h"
TAEVariable	*GetVariable();
#define GRAB(x) {v = GetVariable(env,x); if (v == NULL) return FAIL;}
/************************************************************************/
/* jmovie_set_do sets up the frame size and the number of frames
 * for VIDS movies.
 */
int jmovie_set_do(env)
  VIDSEnvironment		*env;	/* The VIDS environment		*/
{
  TAEVariable	*v;			/* temp variable ptr		*/
  register int	nl,nf;			/* local frame size,nframes	*/
  int		nlVideo,nsVideo;	/* size of screen		*/

  nlVideo = VideoLines(env);
  nsVideo = VideoSamps(env);

  GRAB("NLFRAME");
  if (v->v_count == 0)
    nl = nlVideo;
  else
    nl = MIN(nlVideo, IVAL(*v, 0));

  GRAB("NFRAMES");
  nf = (v->v_count == 0) ? 0 : IVAL(*v, 0);

  if (nl <= 0)
    ABORT(FAIL,"Sorry, NLFRAME (lines per frame) must be greater than zero",
          "VIDS-BADPARM");

  env->movie.movieZoom = nlVideo / nl;
  env->movie.nlFrame = nlVideo / env->movie.movieZoom;
  env->movie.nsFrame = nsVideo / env->movie.movieZoom;
  nl = MIN(env->nlMax / env->movie.nlFrame, env->nsMax / env->movie.nsFrame);
  env->movie.bwFrames = env->nimps * nl * nl;
  env->movie.nFrames = (nf == 0) ? env->movie.bwFrames :
				   MIN(nf, env->movie.bwFrames);
  env->movie.colorFrames = (env->nimps > 3) ? (env->movie.bwFrames / 4) :
					      (env->movie.bwFrames / 3);
  GiveFrameSizes(env);
  
  GRAB("REVERSE");
  env->movie.reverseKey = *SVAL(*v, 0);

  GRAB("FASTER");
  env->movie.fastKey = *SVAL(*v, 0);

  GRAB("SLOWER");
  env->movie.slowKey = *SVAL(*v, 0);

  GRAB("PAUSE");
  env->movie.pauseKey = *SVAL(*v, 0);

  return SUCCESS;
}
/************************************************************************/
/* jmovie_load_do loads up frames for a VIDS movie.
 */
int jmovie_load_do(env)
  VIDSEnvironment		*env;	/* The VIDS environment		*/
{
  int		i,j;			/* increment variables		*/
  int		plane,lastPlane;	/* first plane for a given frame*/
  int		frame;			/* frame to load		*/
  int		zoom;			/* zoom factor to apply		*/
  int		area[4];		/* image area to be displayed	*/
  Point		topLeft;		/* top left of each frame	*/
  FileInfo	*files[3];		/* current file of interest	*/
  int		nfiles;			/* number of input files	*/
  int		bands[3];		/* current band of interest	*/
  PlaneInfo	*p;			/* temp plane ptr		*/
  MessageType	messages,GetMessage();	/* saved value of message level	*/

  if (GetMovieLoadParms(env, &frame, area, &zoom) != SUCCESS) return FAIL;
  CheckSettings(env);
  i = env->isColor ? env->movie.colorFrames : env->movie.bwFrames;
  if (frame > i)
  {
    NotifyUser(Inform, "", "Sorry, can not load frame %d, highest frame is %d",
               frame, i);
    NotifyUser(Inform,"","Note sizes, and use JMOVIE-SET to modify them:");
    GiveFrameSizes(env);
    ABORT(FAIL,"Unable to load frame","VIDS-BADPARM");
  }
  FrameToPlane(env, frame, &plane, &topLeft);
  if (GetFileBand(env, files, bands, &nfiles) != SUCCESS) return FAIL;
  if (env->isColor)
  {
    lastPlane = plane + 2;
    env->redIMP = plane;
    env->greenIMP = plane + 1;
    env->blueIMP = plane + 2;
  }
  else
  {
    lastPlane = plane;
    env->bwIMP = plane;
  }
  SendLuts(env);
  lastPlane = MIN(lastPlane, env->nimps);
  NotifyUser(Inform,"","Loading frame number %d",frame);
  for (j = plane, i = 0; j <= lastPlane; j++, i++)
  {
    if ((i >= nfiles) && (bands[i] > (files[i])->nb)) break;
    p = &env->planes[j];
    TieImp(p, files[i], bands[i]);
    SetFileWindow(p, area[0], area[1], area[2], area[3]);
    SetMLoadZoom(env, p, files[i], zoom);
    p->accessWindow.sl = topLeft.v;
    p->accessWindow.ss = topLeft.h;
    p->accessWindow.nl = env->movie.nlFrame;
    p->accessWindow.ns = env->movie.nsFrame;
    p->subPixel.left = p->subPixel.top = 0;
    messages = GetMessage();
    if (messages != Verbose)		/* If not Verbose messages,	*/
      SetMessage(Silent);		/* make the updates silent.	*/
    LoadIMP(env, j);
    SetMessage(messages);
  }
  i = SUCCESS;
  switch (nfiles)
  {
    case 3 : CloseFile(files[2]);
    case 2 : CloseFile(files[1]);
    case 1 : i = CloseFile(files[0]);
  }
  if (i != SUCCESS)
    ABORT(FAIL,"Could not close all files, proceed with caution",
          "VIDS-CLOSFAIL");
  return SUCCESS;
}
/************************************************************************/
/* jmovie_run_do runs a vids movie.
 */
int jmovie_run_do(env)
  VIDSEnvironment		*env;	/* The VIDS environment		*/
{
  int		start;			/* starting frame of animation	*/
  int		frames;			/* number of frames to show	*/
  Boolean	forward;		/* direction: reverse if False.	*/
  Boolean	reverse;		/* True to reverse direction.	*/
  Boolean	terminate;		/* Flag to end looping		*/
  float		rate;			/* rate at which to run		*/
  int		nskip;			/* number of frames to skip	*/
  int		loops;			/* times to repeat animation	*/
  int		zoom;			/* hardwar zoom factor to apply	*/
  int		i,j;			/* increment variables		*/
  int		curPlane,lastPlane;	/* current,last planes		*/
  Point		curPt,lastPt;		/* upper left corner of plane	*/
  int		curFrame;		/* current frame		*/
  int		end;			/* ending frame of animation	*/
  int		nf;			/* nFrames limited by # available */
  Boolean	EqualPoint();

  i = GetRunParms(env, &start, &frames, &forward, &rate, &nskip,
    &loops,&zoom);
  if (i != SUCCESS) return FAIL;
  CheckSettings(env);		/* make sure sizes are good		*/
  nf = MIN(env->movie.nFrames, (env->isColor ? env->movie.colorFrames :
					       env->movie.bwFrames));
  if (frames <= 0)
    frames = nf;
  frames = MIN(nf, frames);

  if (loops <= 0) loops = 32767;
  if (nskip < 0)
    ABORT(FAIL,"Sorry, NSKIP may not be less than zero","VIDS-BADPARM");
  if (rate <= 0.0)
    ABORT(FAIL,"Sorry, RATE must be greater than zero","VIDS-BADPARM");
  if (! env->movie.indPan) zdeaction(1, 2, 2);
  FrameToPlane(env, start, &curPlane, &curPt);
  if (zoom <= 0) zoom = env->movie.movieZoom;
  for (i = 1; i <= env->nimps; i++)
  {
    zdidwset(env->devUnit, i, curPt.h, curPt.v);
    zdizoom(env->devUnit, i, zoom);
  }

  rate = 1.0 / rate;		/* convert from frames/second to seconds*/
  nskip += 1;			/* convert nskip to an increment var.	*/
  
  if (frames > ((nf - start) / nskip) + 1)
    frames = ((nf - start) / nskip) + 1;
  end = start + ((frames-1) * nskip);
  if (!forward) nskip = -nskip;

  NotifyUser(Inform,"","Control keys are as follows:");
  NotifyUser(Inform,"","\t%c to reverse direction",env->movie.reverseKey);
  NotifyUser(Inform,"","\t%c to increase speed",env->movie.fastKey);
  NotifyUser(Inform,"","\t%c to decrease speed",env->movie.slowKey);
  NotifyUser(Inform,"","\t%c to pause",env->movie.pauseKey);
  NotifyUser(Inform,"","Any other key will stop the movie.");
  lastPlane = 0;
  terminate = False;
  reverse = False;	/* means we changed, not absolute reverse direction */
  StartTimer();
  for (i = 0; i < loops; i++)	/* for each loop of the movie...	*/
  {
    if (nskip >= 0)
      curFrame = start;			/* forward */
    else
      curFrame = end;			/* reverse */

    while (curFrame >= start && curFrame <= end)	/* for each plane */
    {
      CheckButtons(env, &rate, &reverse, &terminate);
      if (reverse)
        nskip = -nskip;
      if (terminate) break;
      FrameToPlane(env, curFrame, &curPlane, &curPt);
      WaitElapsed(rate);
      if (curPlane == lastPlane)
      {
        if (!EqualPoint(&curPt, &lastPt))
	  SetCorner(env, curPlane, &curPt);
      }
      else
      {
	SetCorner(env, curPlane, &curPt);
	SetPlane(env, curPlane);
      }
      StartTimer();
      lastPlane = curPlane;
      lastPt.h = curPt.h; lastPt.v = curPt.v;
      curFrame += nskip;
    }
    if (terminate) break;
  }
  FixPlanes(env);
  if (! env->movie.indPan) zdeaction(2, 2, 2);
  return SUCCESS;
}
/************************************************************************/
/* jmovie_show_do will display the given frame of an already loaded movie.
 */
int jmovie_show_do(env)
  VIDSEnvironment		*env;	/* The VIDS environment		*/
{
  int		theFrame;		/* the frame to show		*/
  int		thePlane;		/* image plane of the frame	*/
  int		nf;			/* local max number of frames	*/
  Point		loc;			/* upper left corner of frame	*/
  TAEVariable	*v;
  
  GRAB("FRAME");
  theFrame = IVAL(*v, 0);

  CheckSettings(env);

  if (theFrame <= 0)
    ABORT(FAIL,"Sorry, FRAME must be greater than zero","VIDS-BADPARM");
  nf = env->isColor ? env->movie.colorFrames : env->movie.bwFrames;
  if (theFrame > nf)
  {
    if (theFrame <= nf)
      NotifyUser(Inform,"","Frame is outside of movie loop, showing anyway");
    else
      ABORT(FAIL,"Sorry, FRAME must be less than the number of frames",
	"VIDS-BADPARM");
  }

  FrameToPlane(env, theFrame, &thePlane, &loc);
  if (!env->movie.indPan) zdeaction(1, 2, 2);
  zdizoom(env->devUnit, thePlane, env->movie.movieZoom);
  SetCorner(env, thePlane, &loc);
  if (!env->movie.indPan) zdeaction(2, 2, 2);
  SetPlane(env, thePlane);
  FixPlanes(env);
  return SUCCESS;
}
/************************************************************************/
int GetMovieLoadParms(env, frame, area, zoom)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			*frame;		/* out: frame to load		*/
  int			area[];		/* out: area of image to load	*/
  int			*zoom;		/* out: zoom factor		*/
{
  TAEVariable	*v;

  env->movie.prevFrame++;
  if (env->movie.prevFrame > env->movie.nFrames) env->movie.prevFrame = 1;
  GRAB("FRAME");
  if (v->v_count == 1)
    env->movie.prevFrame = IVAL(*v, 0);
  *frame = env->movie.prevFrame;

  GRAB("AREA");
  if (v->v_count == 4)
  {
    area[0] = IVAL(*v, 0);	area[1] = IVAL(*v, 1);
    area[2] = IVAL(*v, 2);	area[3] = IVAL(*v, 3);
  }
  else
  {
    area[0] = area[1] = 1;	area[2] = area[3] = 0;
  }

  *zoom = 0;
  GRAB("ZOOM");
  if (v->v_count == 1)
    *zoom = IVAL(*v, 0);
  else
  {
    GRAB("SCALE");
    if (EQUAL(SVAL(*v, 0), "NONE")) *zoom = 1;
  }
  return SUCCESS;
}
/************************************************************************/
int GetRunParms(env, start, frames, forward, rate, nskip, loops, zoom)
  VIDSEnvironment	*env;		/* The VIDS environment		*/
  int			*start;		/* starting frame of animation	*/
  int			*frames;	/* number of frames to show	*/
  Boolean		*forward;	/* direction: reverse if False.	*/
  float			*rate;		/* rate at which to run		*/
  int			*nskip;		/* number of frames to skip	*/
  int			*loops;		/* times to repeat animation	*/
  int			*zoom;		/* zoom factor to use		*/
{
  TAEVariable	*v;
  
  GRAB("START");
  *start = IVAL(*v, 0);
  GRAB("NFRAMES");
  *frames = (v->v_count == 0) ? 0 : IVAL(*v, 0);
  GRAB("DIRECTN");
  *forward = (*SVAL(*v, 0) == 'R') ? False : True;
  GRAB("RATE");
  *rate = RVAL(*v, 0);
  GRAB("NSKIP");
  *nskip = IVAL(*v, 0);
  GRAB("LOOPS");
  *loops = (v->v_count == 0) ? 0 : IVAL(*v, 0);
  GRAB("ZOOM");
  *zoom = (v->v_count == 0) ? 0 : IVAL(*v, 0);
  return SUCCESS;
}
/************************************************************************/
/* GetFileBand looks at the INP parameter and fills in three file/band
 * pairs if color, 1 pair if bw into the files and the bands arguments.
 */
int GetFileBand(env, files, bands, nfiles)
  VIDSEnvironment	*env;
  FileInfo		*files[3];	/* out: files to be loaded	*/
  int			bands[3];	/* out: respective file bands	*/
  int			*nfiles;	/* number of input files	*/
{
  char		*names[3];		/* file names			*/
  char		*memMsg="Sorry, insufficient memory to open any more files";
  char		*opnfail="Sorry, could not open input file";
  int		i;			/* increment variable		*/
  TAEVariable	*v;			/* TAE variable structure	*/
  FileInfo	*findFileSlot();

  names[0] = names[1] = names[2] = NULL;
  GRAB("INP");
  *nfiles = v->v_count;
  for (i = 0; i < *nfiles; i++)
    names[i] = SVAL(*v, i);
  
  bands[0] = 1;
  switch (*nfiles)
  {
    case 3 :
      bands[1] = bands[2] = 1;
      files[2] = findFileSlot(env, names[2]);
      if (files[2] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[2]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      files[1] = findFileSlot(env, names[1]);
      if (files[1] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[1]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      files[0] = findFileSlot(env, names[0]);
      if (files[0] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[0]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      break;
    case 2 : 
      bands[1] = 1; bands[2] = 2;
      files[1] = findFileSlot(env, names[1]);
      if (files[1] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[1]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      files[0] = findFileSlot(env, names[0]);
      if (files[0] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[0]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      files[2] = files[1];
      break;
    case 1 :
      bands[1] = 2; bands[2] = 3;
      files[0] = findFileSlot(env, names[0]);
      if (files[0] == NULL) ABORT(FAIL, memMsg, "VIDS-INSUFMEM");
      if (OpenFile(files[0]) != SUCCESS) ABORT(FAIL, opnfail, "VIDS-OPENFAIL");
      files[2] = files[1] = files[0];
      break;
  }
  GRAB("BANDS");
  for (i = 0; i < v->v_count; i++)
    bands[i] = IVAL(*v, i);

  return SUCCESS;
}
/************************************************************************/
int SetMLoadZoom(env, p, file, zoom)
  VIDSEnvironment	*env;
  PlaneInfo	*p;
  FileInfo	*file;
  int		zoom;
{
  int	i;
  float	x,y;

  if (zoom != 0)
  {
    p->softZoom = zoom;
    return SUCCESS;
  }
  if (file == NULL)	/* just in case ... */
  {
    p->softZoom = 1;
    return SUCCESS;
  }
  i = p->imageWindow.nl;
  if (i == 0) i = file->nl - p->imageWindow.sl + 1;
  y = (float) (env->movie.nlFrame) / (float) (i);

  i = p->imageWindow.ns;
  if (i == 0) i = file->ns - p->imageWindow.ss + 1;
  x = (float) (env->movie.nsFrame) / (float) (i);
    
  x = MIN(x,y);
  p->softZoom = (int) ((x < 1.0) ? -(1.0/x + 0.999) : x);
  return SUCCESS;
}
/************************************************************************/
/* converts a frame number to a plane number and a display window
 * coordinate.
 */
int FrameToPlane(env, frame, plane, topLeft)
  VIDSEnvironment	*env;
  int			frame;
  int			*plane;
  Point			*topLeft;
{
  int frameZoom;
  
  frameZoom = MIN(env->nlMax / env->movie.nlFrame,
		  env->nsMax / env->movie.nsFrame);
  frame--;
  *plane = (frame / (frameZoom * frameZoom)) + 1;

  topLeft->v = (frame % frameZoom) * env->movie.nlFrame + 1;
  topLeft->h = ((frame % (frameZoom * frameZoom)) / frameZoom) *
							env->movie.nsFrame + 1;
  
  if ((env->movie.indPan == False) && (*plane % 2) == 0)
  {
    topLeft->v = env->nlMax - topLeft->v - env->movie.nlFrame + 2;
    topLeft->h = env->nsMax - topLeft->h - env->movie.nsFrame + 2;
  }
  if (env->isColor)
    *plane = (*plane - 1) * 4 + 1;		/* 4 for Adage 3000	*/
  return SUCCESS;
}
/************************************************************************/
int SetPlane(env, plane)
  VIDSEnvironment	*env;
  int			plane;
{
  if (env->isColor)
  {
    env->redIMP = plane;
    env->greenIMP = plane + 1;
    env->blueIMP = plane + 2;
  }
  else
  {
    env->bwIMP = plane;
  }
  SendLuts(env);
  return SUCCESS;
}
/************************************************************************/
int SetCorner(env, plane, topLeft)
  VIDSEnvironment	*env;
  int			plane;
  Point			*topLeft;
{
  if (env->isColor)
  {
    zdidwset(env->devUnit, plane, topLeft->h, topLeft->v);
    plane++;
    zdidwset(env->devUnit, plane, topLeft->h, topLeft->v);
    plane++;
    zdidwset(env->devUnit, plane, topLeft->h, topLeft->v);
  }
  else
    zdidwset(env->devUnit, plane, topLeft->h, topLeft->v);
  return SUCCESS;
}
/************************************************************************/
/* CheckSettings checks the frame sizes and other globals for movies
 * and ensures that everything is valid.
 */
int CheckSettings(env)
  VIDSEnvironment	*env;
{
  static int		maxZoom;	/* maximum zoom supported	*/
  int 			unit;		/* device unit number		*/
  
  unit = env->devUnit;
  zddinfo(unit, 20, 1, &env->movie.indPan);
  zddinfo(unit, 38, 1, &maxZoom);
  if (env->movie.movieZoom <= 0) env->movie.movieZoom = 1;
  else if (env->movie.movieZoom > maxZoom) env->movie.movieZoom = maxZoom;
  env->movie.nlFrame = VideoLines(env) / env->movie.movieZoom;
  env->movie.nsFrame = VideoSamps(env) / env->movie.movieZoom;
  
  maxZoom = MIN(env->nlMax / env->movie.nlFrame,
		env->nsMax / env->movie.nsFrame);
  if (env->movie.bwFrames == 0)
    env->movie.bwFrames = env->nimps * maxZoom * maxZoom;
  env->movie.colorFrames = (env->nimps > 3) ? (env->movie.bwFrames / 4) :
					      (env->movie.bwFrames / 3);
  if (env->movie.nFrames == 0)
    env->movie.nFrames = env->isColor ? env->movie.colorFrames :
					env->movie.bwFrames;

  env->movie.reverseKey = toupper(env->movie.reverseKey);
  env->movie.fastKey = toupper(env->movie.fastKey);
  env->movie.slowKey = toupper(env->movie.slowKey);
  env->movie.pauseKey = toupper(env->movie.pauseKey);
  env->movie.stepKey = toupper(env->movie.stepKey);

  return SUCCESS;
}
/************************************************************************/
int FixPlanes(env)
  VIDSEnvironment	*env;
{
  int i,x,y;
  PlaneInfo *p;
  int	unit;

  unit = env->devUnit;
  for (i = 1; i <= env->nimps; i++)		/* make sure saved locs	*/
  {						/* are correct.		*/
    p = &env->planes[i];			/* Ignore errors and try to */
    zdidwlocation(unit, i, &x, &y);		/* do as many as possible.  */
  }
  return SUCCESS;
}
/************************************************************************/
#define CHANGERATE 0.5
int CheckButtons(env, rate, reverse, terminate)
  VIDSEnvironment	*env;
  float			*rate;
  Boolean		*reverse;
  Boolean		*terminate;
{
  char PeekChar(),c;

  *terminate = False;
  *reverse = False;
  c = PeekChar(False);
  if (c == '\0') return SUCCESS;
  c = toupper(c);
  if (c == env->movie.fastKey)
    *rate *= CHANGERATE;
  else if (c == env->movie.slowKey)
    *rate /= CHANGERATE;
  else if (c == env->movie.reverseKey)
    *reverse = True;
  else if (c == env->movie.pauseKey)
    while (PeekChar(False) == '\0');
/*  else if (c == env->movie.stepKey)*/
  else
    *terminate = True;
  if (*rate > 2.0) *rate = 2.0;
  return SUCCESS;
}
/************************************************************************/
int GiveFrameSizes(env)
  VIDSEnvironment	*env;
{
  int cFrames;

  NotifyUser(Inform, "", "\tAvailable frame size is %d lines by %d samples",
             env->movie.nlFrame, env->movie.nsFrame);
  NotifyUser(Inform, "", "\t%d frames are available in black and white,",
             env->movie.bwFrames);
  NotifyUser(Inform, "", "\t%d frames are available in color.",
	     env->movie.colorFrames);
  cFrames = MIN(env->movie.colorFrames, env->movie.nFrames);
  NotifyUser(Inform,"","\t%d b&w (%d color) frames are currently in use",
             env->movie.nFrames,cFrames);
  return SUCCESS;
}
/************************************************************************/
/* Initialize the movie state variables in the environment structure.
 */
InitMovie(env)
  VIDSEnvironment	*env;
{
  env->movie.nlFrame = 0;
  env->movie.nsFrame = 0;
  env->movie.nFrames = 0;
  env->movie.bwFrames = 0;
  env->movie.colorFrames = 0;
  env->movie.movieZoom = 0;
  env->movie.prevFrame = 0;

  env->movie.reverseKey = 'r';
  env->movie.fastKey = '2';
  env->movie.slowKey = '1';
  env->movie.pauseKey = 'p';
  env->movie.stepKey = 's';
}
/************************************************************************/
