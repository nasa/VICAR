#include "VIDSdefs.h"

typedef enum {NoBurn, BWGraph, ColorGraph} Burn;

/************************************************************************/
/* JSAVE-STRETCH saves a set of stretch lookup tables into a file.
 */
int jsave_stretch_do(env)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
{
  char *out;				/* output file name		*/
  int planes[MAXPLANES];		/* image planes to save		*/
  int nplanes;				/* number of image planes	*/
  int status;
  TAEVariable *v;
  TAEVariable *GetVariable();

  v = GetVariable (env, "OUT");
  if (v == NULL) return FAIL;
  out = SVAL(*v, 0);

  status = GetPlaneList(env, planes, &nplanes, True);
  if (status != SUCCESS) return status;

  status = WriteStretch(env, planes, nplanes, out);
  if (status != SUCCESS) return status;

  return SUCCESS;
}

/************************************************************************/
/* JSAVE-PSEUDO saves a pseudocolor table into a file.
 */
int jsave_pseudo_do(env)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
{
  char *out;				/* output file name		*/
  int plane;				/* image plane to save		*/
  int nplanes;				/* # of planes (must be 1)	*/
  Boolean stretch;			/* True iff pstbl goes thru LUT	*/
  int *lut;				/* lut to use if stretch==True	*/
  int ramp[256];
  int status;
  TAEVariable *v;
  TAEVariable *GetVariable();
  int *CurrentLut();

  v = GetVariable (env, "OUT");
  if (v == NULL) return FAIL;
  out = SVAL(*v, 0);

  status = GetPlaneList(env, &plane, &nplanes, True);
  if (status != SUCCESS) return status;

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  if (EQUAL(SVAL(*v,0), "STRETCH"))
    lut = CurrentLut(env, plane);		/* use stretch */
  else
  {
    RampLut(ramp);				/* don't use stretch */
    lut = ramp;
  }

  status = WritePseudo(env, plane, out, lut);
  if (status != SUCCESS) return status;

  return SUCCESS;
}

/************************************************************************/
/* JSAVE-IMAGE saves a set of image planes into one or more image files.
 */
int jsave_image_do(env)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
{
  char **out;				/* output file names		*/
  int nfiles;				/* number of output files	*/
  int planes[MAXPLANES];		/* image planes to save		*/
  int nplanes;				/* number of image planes	*/
  Region *rgn;				/* region to save		*/
  Boolean stretch;			/* True iff stretch is applied	*/
  Boolean pseudo;			/* True iff pseudocolor applied	*/
  Burn burn;				/* how to burn graphics plane	*/
  Region *locrgn[MAXPLANES];		/* imp coord rgn for each plane	*/
  Point size;				/* size of files		*/
  int np;				/* # of planes going to files	*/
					/* (may be > nplanes)		*/
  FileInfo *theFile;
  int i, status, imp;
  int band, index;
  FileInfo *findFileSlot();

  status = GetJsaveImageParms(env, &out, &nfiles, planes, &nplanes,
			    &rgn, &stretch, &pseudo, &burn);
  if (status != SUCCESS) return status;

  if (pseudo == True && nplanes != 1)
    ABORT(FAIL,"Sorry, you can only save one image plane in pseudocolor mode",
	"VIDS-ONLYONE");

  status = GetOutFileSize(env, planes, nplanes, rgn, locrgn, &size);

  np = nplanes;			/* number of output planes */
  if (pseudo == True || burn == ColorGraph)
    np = MAX(np, 3);		/* make sure there's room to save color */

  if (nfiles > np)		/* ignore any extra files */
    nfiles = np;

  /* Now process each file in turn */

  /* If we're saving pseudocolor or color graphics, the number of planes */
  /* saved may be more than the number of image planes used, since color */
  /* takes three planes.  Index counts image planes used, while imp counts */
  /* image planes saved to the files.					 */

  index = 0;			/* index into planes array */
  imp = 0;			/* (0,1,2) == (red,green,blue) */
  for (i=0; i<nfiles; i++)
  {
    theFile = findFileSlot(env, out[i]);
    if (theFile == NULL) ABORT(FAIL,
        "Insufficient memory to prepare file information", "VIDS-INSUFMEM");
    theFile->nl = size.v;
    theFile->ns = size.h;
    if (i == nfiles-1)		/* last file, might need several bands */
      theFile->nb = np - nfiles + 1;
    else
      theFile->nb = 1;
    theFile->org = BSQ;
    theFile->format = ByteFormat;

    status = OpenOutFile(theFile);
    if (status != SUCCESS)
      ABORT(FAIL, "Unable to open the output file", "VIDS-OPENFAIL");

    /* Process all planes for this file */

    for (band = 1; band <= theFile->nb; band++)
    {
      NotifyUser(Inform, "", "Saving plane %d as band %d of file '%s'.",
		 planes[index], band, theFile->filename);
      status = SaveIMP(env, planes[index], imp, locrgn[index], theFile, band,
			stretch, pseudo, burn);
      if (status != SUCCESS)
      {
        CloseFile(theFile);
        return status;
      }
      imp++;
      index++;				/* next plane */
      if (index >= nplanes)
        index = nplanes - 1;		/* dup planes for pseudo, etc. */
    }

    CloseFile(theFile);
  }

  return SUCCESS;
}
/************************************************************************/
int GetJsaveImageParms(env,out,nfiles,planes,nplanes,rgn,stretch,pseudo,burn)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  char			***out;		/* output file names		*/
  int			*nfiles;	/* number of files		*/
  int			planes[];	/* planes to save		*/
  int			*nplanes;	/* number of planes to save	*/
  Region		**rgn;		/* region to save		*/
  Boolean		*stretch;	/* True iff stretch is applied	*/
  Boolean		*pseudo;	/* True iff pseudocolor applied	*/
  Burn			*burn;		/* burn graphics into image	*/
{
  TAEVariable		*v;		/* temp VARIABLE pointer	*/
  int			status;		/* temp status variable		*/
  int			i;		/* temp variable		*/
  TAEVariable		*GetVariable();

  v = GetVariable (env, "OUT");
  if (v == NULL) return FAIL;
  *nfiles = v->v_count;
  *out = (char **)v->v_cvp;

  status = GetPlaneList(env, planes, nplanes, True);
  if (status != SUCCESS) return status;

  status = GetRegionList(env, rgn, &i, "REGION", 1);
  if (status != SUCCESS) return status;
  if (i == 0)				/* region was defaulted */
    *rgn = NULL;

  v = GetVariable(env, "STRETCH");
  if (v == NULL) return FAIL;
  *stretch = False;
  if (EQUAL(SVAL(*v,0), "STRETCH"))
    *stretch = True;

  v = GetVariable(env, "PSEUDO");
  if (v == NULL) return FAIL;
  *pseudo = False;
  if (EQUAL(SVAL(*v,0), "PSEUDO"))
    *pseudo = True;
  if (EQUAL(SVAL(*v,0), "AUTO"))      /* on if in pseudo mode, off otherwise */
    *pseudo = env->isPseudo;

  v = GetVariable(env, "BURN");
  if (v == NULL) return FAIL;
  *burn = NoBurn;
  if (EQUAL(SVAL(*v,0), "BWGRAPH"))
    *burn = BWGraph;
  if (EQUAL(SVAL(*v,0), "COLORGRAPH"))
    *burn = ColorGraph;

  return SUCCESS;
}

/************************************************************************/
/* GetOutFileSize gets the size of an output image file using the bounding
 * rectangle of the region (which may be different on each plane).
 */
int GetOutFileSize(env, planes, nplanes, rgn, locrgn, size)
  VIDSEnvironment	*env;		/* the VIDS environment	*/
  int planes[];				/* image planes to save		*/
  int nplanes;				/* number of image planes	*/
  Region *rgn;				/* region to save		*/
  Region *locrgn[];			/* imp coord rgn for each plane	*/
  Point *size;				/* returned size of out files	*/
{
  int i, status;
  char text[STRINGSIZ+1];
  Rect impRect;
  Region *PointsToRegion(), *NextEmptyRegion();

  if (rgn == NULL)			/* no region given, so use entire imp */
  {
    size->h = env->nsMax;
    size->v = env->nlMax;
    for (i=0; i<nplanes; i++)
      locrgn[i] = NULL;
  }
  else				/* region given, so convert all to imp coords */
  {				/* and get the size of the biggest region     */
    SetRect(&impRect, 1, 1, env->nlMax, env->nsMax);
    size->h = 0;
    size->v = 0;
    for (i=0; i<nplanes; i++)
    {
      locrgn[i] = NextEmptyRegion(env);
      if (locrgn[i] == NULL)
        return FAIL;
      MarkRgnTemp(locrgn[i]);

      status = RegionToLocal(env, planes[i], rgn, locrgn[i]);
      if (status != SUCCESS)
        return status;

      if (! RectInRect(&locrgn[i]->bounds, &impRect))
      {
        sprintf(text,
		"Region \"%s\" crosses the edge of plane %d, so cannot save it",
		locrgn[i]->name, planes[i]);
        ABORT(FAIL, text, "VIDS-RGNXIMP");
      }

      size->h = MAX(size->h, locrgn[i]->bounds.right-locrgn[i]->bounds.left+1);
      size->v = MAX(size->v, locrgn[i]->bounds.bottom-locrgn[i]->bounds.top+1);
    }
  }
  return SUCCESS;
}

/************************************************************************/
/* SaveIMP saves a single image plane to a band of a file.  The plane may
 * be processed by first sending it through the stretch LUT, the pseudo
 * table, or burning the graphics into it.  The color parameter indicates
 * whether to use the red (0), green (1), or blue (2) pseudo table or
 * graphics colors (the first plane is always red, the second green, etc.).
 */
int SaveIMP(env, imp, color, rgn, theFile, band, stretch, pseudo, burn)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int imp;				/* image plane to use		*/
  int color;				/* see comments above		*/
  Region *rgn;				/* region in imp coordinates	*/
  FileInfo *theFile;			/* file to save in		*/
  int band;				/* band number in file to use	*/
  Boolean stretch;			/* True iff stretch is applied	*/
  Boolean pseudo;			/* True iff pseudocolor applied	*/
  Burn burn;				/* how to burn graphics into image */
{
  Rect imprect;
  Point size;
  int i, status;
  int line, samp;
  int y, x1, x2, npix;
  unsigned char *lp, *buf;
  int lut[256];
  int pseudolut[256];
  int graphlut[256];
  PSTable *pstable;
  CLut clut;
  int *CurrentLut();
  PSTable *CurrentPSTable();
  RegionState rgnstate;		/* saves state info for NextRegionLine() */
  Boolean NextRegionLine();

  if (rgn == NULL)			/* Read the image into memory */
  {
    imprect.top = 1;
    imprect.left = 1;
    imprect.bottom = env->nlMax;
    imprect.right = env->nsMax;
  }
  else
  {
    BlockMove(&rgn->bounds, &imprect, sizeof(Rect));
  }
  status = SaveImageRegion(env, imp, &imprect, env->buffer);
  if (status != SUCCESS)
    return status;

  /* Calculate the LUTs to use */

  RampLut(lut);
  if (stretch)					/* use the stretch LUT */
    BlockMove(CurrentLut(env,imp), lut, sizeof(lut));

  RampLut(pseudolut);
  if (pseudo)					/* use the pseudocolor table */
  {
    pstable = CurrentPSTable(env, imp);
    if (color == 0)
      BlockMove(pstable->red, pseudolut, sizeof(pseudolut));
    if (color == 1)
      BlockMove(pstable->green, pseudolut, sizeof(pseudolut));
    if (color == 2)
      BlockMove(pstable->blue, pseudolut, sizeof(pseudolut));
  }

  if (burn == BWGraph)			/* bw only, use only white */
  {
    for (i=0; i<256; i++)
      graphlut[i] = 255;
  }
  if (burn == ColorGraph)		/* use the graphics LUT */
  {
    GetGLuts(env, &clut);
    if (color == 0)
      BlockMove(clut.red, graphlut, sizeof(graphlut));
    if (color == 1)
      BlockMove(clut.green, graphlut, sizeof(graphlut));
    if (color == 2)
      BlockMove(clut.blue, graphlut, sizeof(graphlut));
  }
  
  size.h = imprect.right - imprect.left + 1;
  size.v = imprect.bottom - imprect.top + 1;

  /* Do all the image transformations (except region masking) */

  if (burn != NoBurn || stretch == True || pseudo == True)
  {
    lp = env->buffer;			/* ptr to start of line */
    for (line = 0; line < size.v; line++)	/* loop through each line */
    {
      for (samp = 0; samp < size.h; samp++)	/* apply LUTs */
        *(lp+samp) = pseudolut[lut[*(lp+samp)]];

      if (burn != NoBurn)
      {
        status = BurnGraphLine(env, imp, &imprect, lp, line, graphlut);
        if (status != SUCCESS)
          return status;
      }
      lp += size.h;
    }
  }

  /* If we have a non-rectangular region, or the image does not fill	*/
  /* up the entire file, then first fill the entire file with 0's so	*/
  /* we can randomly write the other parts later without worrying about	*/
  /* filling in the missing parts.					*/

  if ((rgn != NULL && rgn->type != Rectangle && rgn->type != Square) ||
      (size.h != theFile->ns || size.v != theFile->nl))
  {
    buf = malloc(size.h);
    if (buf == NULL)
      ABORT(FAIL, "Not enough memory to clear the file", "VIDS-INSUFMEM");
    for (samp=0; samp<size.h; samp++)
      buf[samp] = 0;
    for (line=1; line<=size.v; line++)
    {
      status = zvwrit(theFile->fileUnit, buf, "LINE", line,
                 "BAND", band, "NSAMPS", size.h, 0);
      if (status != SUCCESS)
      {
        free(buf);
        ABORT(FAIL, "Error writing to the file", "VIDS-EXECERR");
      }
    }
    free(buf);
  }

  /* Write the image out to the file */
  /* If it's a rectangular region, do it the easy way */

  if (rgn == NULL || rgn->type == Rectangle || rgn->type == Square)
  {
    lp = env->buffer;			/* ptr to start of line */
    for (line = 1; line <= size.v; line++)	/* loop through each line */
    {
      status = zvwrit(theFile->fileUnit, lp, "LINE", line,
                 "BAND", band, "NSAMPS", size.h, 0);
      if (status != SUCCESS)
        ABORT(FAIL, "Error writing to the file", "VIDS-EXECERR");
      lp += size.h;
    }
  }
  else		/* non-rectangular, so do the masking by writing */
  {		/* only those sections inside the region	 */

    status = StartRegionLine(rgn, &rgnstate);	/* rgn is non-NULL */
    if (status != SUCCESS)
      ABORT(FAIL, "Insufficient memory to save the image", "VIDS-INSUFMEM");
    while (NextRegionLine(rgn, &rgnstate, &y, &x1, &x2))
    {
      npix = x2 - x1 + 1;
      line = y - imprect.top + 1;
      samp = x1 - imprect.left + 1;
      lp = env->buffer + ((line-1) * size.h) + (samp-1);
      status = zvwrit(theFile->fileUnit, lp, "LINE", line, "SAMP", samp,
               "BAND", band, "NSAMPS", npix, 0);
      if (status != SUCCESS)
        ABORT(FAIL, "Error writing to the file", "VIDS-EXECERR");
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* BurnGraphLine takes a single line of an image and burns the graphics
 * plane into it.  The graphics plane line is read from the device.
 */
int BurnGraphLine(env, imp, imprect, lp, line, graphlut)
  VIDSEnvironment	*env;		/* the VIDS environment		*/
  int		imp;			/* image plane we're on		*/
  Rect		*imprect;		/* bounding rect in imp coords	*/
  unsigned char	*lp;			/* pointer to line of image	*/
  int		line;			/* line # within bounding rect	*/
  int		graphlut[256];		/* graphics LUT for this plane	*/
{
  int i, status, imgsize, grsize, size;
  Point graphstart, graphstop;
  unsigned char *gr;
  double pixstep, grpix;

  /* Get line in graphics plane coordinates */

  CursIMP2IMP(env, imp, env->grafIMP, imprect->left, imprect->top+line,
		&graphstart.h, &graphstart.v);
  CursIMP2IMP(env, imp, env->grafIMP, imprect->right, imprect->top+line,
		&graphstop.h,  &graphstop.v);

  /* Now allocate some memory, and read it in from the display */

  imgsize = imprect->right - imprect->left + 1;
  grsize = graphstop.h - graphstart.h + 1;
  if (imgsize <= 0)
    return SUCCESS;

  if (grsize <= 0)		/* wrap around the graphics plane edge */
  {
    grsize += env->nsMax;
    gr = malloc(grsize);
    if (gr == NULL)
      ABORT(FAIL, "Not enough memory to burn graphics plane.", "VIDS-INSUFMEM");
    size = env->nsMax - graphstart.h + 1;
    status = zdilineread(env->devUnit, env->grafIMP, graphstart.h, graphstart.v,
			size, gr);
    if (status == SUCCESS)
      status = zdilineread(env->devUnit, env->grafIMP, 1, graphstart.v,
			graphstop.h, gr+size);
  }
  else				/* no wrap, read it in one shot */
  {
    gr = malloc(grsize);
    if (gr == NULL)
      ABORT(FAIL, "Not enough memory to burn graphics plane.", "VIDS-INSUFMEM");
    status = zdilineread(env->devUnit, env->grafIMP, graphstart.h, graphstart.v,
			grsize, gr);
  }

  if (status != SUCCESS)
  {
    free(gr);
    ABORT(FAIL, "Error reading graphics data from display device", "VIDS-VRDIERR");
  }

  /* Figure out the scaling factor to use for the graphics data */
  /* (in case there are different zooms on the image & graphics planes) */

  pixstep = (double)grsize / (double)imgsize;

  /* Finally, apply the graphics data */

  grpix = 0.0;
  for (i = 0; i < imgsize; i++)
  {
    if (*(gr+(int)grpix) != 0)
      *(lp+i) = graphlut[*(gr+(int)grpix)];
    grpix += pixstep;
  }

  free(gr);

  return SUCCESS;
}
