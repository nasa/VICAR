#include "VIDSdefs.h"

extern struct PARBLK OutParblk;

/* JGET gets various values from the environment block and the display,
 * returns them in TCL variables, and optionally prints them.
 *
 * Note: JGET-CURSOR is in it's own file, JCURSOR.C
 */

#define LUTSIZE		256

/************************************************************************/
/* JGET-DEVICE returns some general device information in TCL variables.*/
/************************************************************************/

int jget_device_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  TAEVariable *v;
  Boolean print;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  char devname[21];		/* size matches string size in PDF + 1	*/
  char fullname[21];		/* size matches string size in PDF + 1	*/
  char string[80];
  int nimps, nl, ns, vnl, vns, mode;
  char *s;
  int unit;
  int len;
  TAEVariable *GetVariable();

  unit = env->devUnit;
  oparb = &OutParblk;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  status = zddname(unit, 1, devname, 20, &len);	/* get device name */
  if (status != SUCCESS)
    ABORT(FAIL, "Error getting device name from VRDI", "VIDS-VRDIERR");
  devname[len] = '\0';			/* null terminate the string */
  s = devname;				/* to pass **string */
  status = q_string(oparb, "PDEVNAME", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending DEVNAME parameter back to TAE", "VIDS-TAEERR");

  status = zddname(unit, 2, fullname, 20, &len);	/* get device name */
  if (status != SUCCESS)
    ABORT(FAIL, "Error getting full name from VRDI", "VIDS-VRDIERR");
  fullname[len] = '\0';			/* null terminate the string */
  s = fullname;				/* to pass **string */
  status = q_string(oparb, "PFULLNAM", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending FULLNAME parameter back to TAE", "VIDS-TAEERR");

  nimps = env->nimps;
  nl = env->nlMax;
  ns = env->nsMax;
  vnl = VideoLines(env);
  vns = VideoSamps(env);
  mode = zdsmode(unit);

  status = q_intg(oparb, "PNIMPS", 1, &nimps, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NIMPS parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PNL", 1, &nl, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NL parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PNS", 1, &ns, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NS parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PVNL", 1, &vnl, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending VNL parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PVNS", 1, &vns, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending VNS parameter back to TAE", "VIDS-TAEERR");

  if (mode == 1)
    s = "COLOR";
  else if (mode == 2)
    s = "PSEUDO";
  else
    s = "BW";
  status = q_string(oparb, "PMODE", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending MODE parameter back to TAE", "VIDS-TAEERR");


  if (print)
  {
    NotifyUser(Inform, "", "Device %s : %s", devname, fullname);
    if (mode == 1)
      NotifyUser(Inform, "", "Currently in Color mode");
    else if (mode == 2)
      NotifyUser(Inform, "", "Currently in Pseudocolor mode");
    else
      NotifyUser(Inform, "", "Currently in Black & White mode");
    NotifyUser(Inform, "", "Number of image planes = %d", nimps);
    NotifyUser(Inform, "",
	"Number of lines = %d, Number of samples = %d", nl, ns);
    NotifyUser(Inform, "",
	"Number of video lines = %d, Number of video samples = %d", vnl, vns);
  }

  return SUCCESS;
}
/************************************************************************/
/* JGET-DEVLIST returns a list of all device names currently in use.	*/
/* The current device is always returned as the first device.		*/
/************************************************************************/
#define MAXDEV 100	/* max # returned by this routine; must match pdf */

int jget_devlist_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  VIDSEnvironment	*tempenv;
  int i, status;
  TAEVariable *v;
  Boolean print;
  char *devList[MAXDEV];
  int ndevs;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  ndevs = 0;
  tempenv = env;
  do			/* Loop through environment list, getting all names */
  {
    if (tempenv->devUnit != noUnit)
    {
      if (ndevs >= MAXDEV)		/* array is full */
        break;
      devList[ndevs++] = tempenv->devName;
    }
    tempenv = tempenv->next;
  } while (tempenv != env);

  status = q_string(oparb, "PODEV", ndevs, devList, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending ODEV parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PNDEV", 1, &ndevs, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NDEV parameter back to TAE", "VIDS-TAEERR");

  if (print)
  {
    NotifyUser(Inform, "", "Number of devices = %d", ndevs);
    if (ndevs != 0)
    {
      NotifyUser(Inform, "", "Device names are:");
      for (i=0; i<ndevs; i++)
        NotifyUser(Inform, "", "  %s", devList[i]);
    }
  }

  return SUCCESS;
}
/************************************************************************/
/* JGET-PLANES takes a PLANES parameter and returns the associated	*/
/* plane numbers in a TCL integer variable.				*/
/************************************************************************/

int jget_planes_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  TAEVariable *v;
  Boolean dup, print;
  int planeList[MAXPLANES];
  int nPlanes;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  v = GetVariable(env, "DUP");
  if (v == NULL) return FAIL;
  dup = False;
  if (EQUAL(SVAL(*v, 0), "DUP"))
    dup = True;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  if (GetPlaneList(env, planeList, &nPlanes, dup) != SUCCESS)
    return FAIL;

  status = q_intg(oparb, "POPLANES", nPlanes, planeList, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending OPLANES parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PNPLANES", 1, &nPlanes, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NPLANES parameter back to TAE", "VIDS-TAEERR");

  if (print)
  {
    NotifyUser(Inform, "", "Number of planes = %d", nPlanes);
    if (nPlanes != 0)
    {
      NotifyUser(Inform, "", "Plane numbers are:");
      for (i=0; i<nPlanes; i++)
        NotifyUser(Inform, "", "  %d", planeList[i]);
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* JGET-PLINFO takes a single PLANE parameter and returns information	*/
/* about that plane in TCL variables.					*/
/* Note that Filename and Band are *NOT* printed.  That is taken care	*/
/* of by the JSHOW-PLANES command.					*/
/************************************************************************/

int jget_plinfo_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  TAEVariable *v;
  Boolean header, print;
  int imp;
  int nPlanes;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  PlaneInfo *plane;
  SizeField file;
  int pan[2], zoom;
  char *s;
  char tempstr[STRINGSIZ+1];
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  if (GetPlaneList(env, &imp, &nPlanes, False) != SUCCESS)  /* better be 1 */
    return FAIL;

  v = GetVariable(env, "HEADER");
  if (v == NULL) return FAIL;
  header = False;
  if (EQUAL(SVAL(*v, 0), "HEADER"))
    header = True;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  plane = &env->planes[imp];

  file.sl = plane->imageWindow.sl;	file.ss = plane->imageWindow.ss;
  file.nl = plane->imageWindow.nl;	file.ns = plane->imageWindow.ns;
  if (plane->file != NULL)
  {
    if (file.nl == 0)
      file.nl = plane->file->nl;
    if (file.ns == 0)
      file.ns = plane->file->ns;
  }

  /* Note: the following depends on SizeField being four integers in	*/
  /* the order SL, SS, NL, NS!!						*/

  status = q_intg(oparb, "PFILE", 4, &file, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending FILE parameter back to TAE", "VIDS-TAEERR");

  /* Note: the following depends on accessWindow being a SizeField of at  */
  /* least two integers in the order SL, SS!!				  */

  status = q_intg(oparb, "PLOCTION", 2, &plane->accessWindow, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending LOCATION parameter back to TAE", "VIDS-TAEERR");

  pan[0] = zdsdwline(env->devUnit, imp);
  pan[1] = zdsdwsamp(env->devUnit, imp);
  status = q_intg(oparb, "PPAN", 2, pan, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PAN parameter back to TAE", "VIDS-TAEERR");

  zoom = zdszoom(env->devUnit, imp);
  status = q_intg(oparb, "PDSPZOOM", 1, &zoom, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending DISPZOOM parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PFILZOOM", 1, &plane->softZoom, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending FILEZOOM parameter back to TAE", "VIDS-TAEERR");

  status = q_intg(oparb, "PBAND", 1, &plane->band, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending BAND parameter back to TAE", "VIDS-TAEERR");

  if (plane->file == NULL)
    s = "";
  else
    s = plane->file->filename;			/* to pass **string */
  status = q_string(oparb, "PFILENAM", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending FILENAME parameter back to TAE", "VIDS-TAEERR");

  if (print)
  {
    if (header)
    {
      NotifyUser(Inform, "",
     "Image      File Coordinates      Location    Device Pan     Zoom Factor");
      NotifyUser(Inform, "",
     "plane     sl   ss   nl   ns       sl   ss       sl   ss   display  file");
    }
    sprintf(tempstr, " %3d   %5d%5d%5d%5d    %5d%5d    %5d%5d       %3d  %4d",
	imp, file.sl, file.ss, file.nl, file.ns,
	plane->accessWindow.sl, plane->accessWindow.ss,
	pan[0], pan[1], zoom, plane->softZoom);
    NotifyUser(Inform, "", tempstr);
  }

  return SUCCESS;
}

/************************************************************************/
/* JGET-REGION takes a single REGION parameter and returns information	*/
/* about that region in TCL variables.					*/
/************************************************************************/

int jget_region_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  Boolean prtpts, print;
  Region *rgn;
  int nrgns;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  char *s, *ps;
  char tempstr[STRINGSIZ+1], tempstr2[20];
  TAEVariable *v;
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  if (GetRegionList(env, &rgn, &nrgns, "REGION", 1) != SUCCESS)
    return FAIL;

  v = GetVariable(env, "PRTPTS");
  if (v == NULL) return FAIL;
  prtpts = False;
  if (EQUAL(SVAL(*v, 0), "PRTPTS"))
    prtpts = True;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  s = rgn->name;			/* to pass **string */
  status = q_string(oparb, "PNAME", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NAME parameter back to TAE", "VIDS-TAEERR");

  switch (rgn->type)
  {
    case Rectangle:
      s = "RECT";
      ps = "Rectangle";
      break;
    case Square:
      s = "SQUARE";
      ps = "Square";
      break;
    case Oval:
      s = "OVAL";
      ps = "Oval";
      break;
    case Circle:
      s = "CIRCLE";
      ps = "Circle";
      break;
    case Polygon:
      s = "POLYGON";
      ps = "Polygon";
      break;
    default:
      ABORT(FAIL, "Unknown region shape!  Internal error; notify VIDS programmer", "VIDS-INTERR");
  }
  status = q_string(oparb, "PSHAPE", 1, &s, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending SHAPE parameter back to TAE", "VIDS-TAEERR");

  /* Note: the following depends on bounds being a Rect of four integers */
  /* in the order (top,left,bottom,right)!!				 */

  status = q_intg(oparb, "PBOUNDS", 4, &rgn->bounds, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending BOUNDS parameter back to TAE", "VIDS-TAEERR");

  /* Note: the following depends on pointList being an array of Points	*/
  /* with each point in the order (vert, horiz)!!			*/

  if (rgn->type == Polygon)
    status = q_intg(oparb, "PPOINTS", rgn->nPoints*2, rgn->pointList, P_ADD);
  else
    status = q_intg(oparb, "PPOINTS", 0, rgn->pointList, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending POINTS parameter back to TAE", "VIDS-TAEERR");

  if (print)
  {
    NotifyUser(Inform, "", "Region %s: %s, Bounds = (%d,%d,%d,%d)",
		rgn->name, ps, rgn->bounds.top, rgn->bounds.left,
		rgn->bounds.bottom, rgn->bounds.right);

    if (rgn->type == Polygon && prtpts == True)
    {
      strcpy(tempstr, "    Pointlist = (");
      for (i=0; i<rgn->nPoints; i++)
      {
        sprintf(tempstr2, " (%d,%d) ", rgn->pointList[i].v,rgn->pointList[i].h);
        strcat(tempstr, tempstr2);
        if (strlen(tempstr) > 65)
        {
          NotifyUser(Inform, "", tempstr);
          strcpy(tempstr, "        ");
        }
      }
      strcat(tempstr, ")");
      NotifyUser(Inform, "", tempstr);
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* JGET-RGNLIST returns a list of all region names currently in use.	*/
/************************************************************************/
#define MAXRGN 100	/* max # returned by this routine; must match pdf */

int jget_rgnlist_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  TAEVariable *v;
  Boolean print;
  char *rgnList[MAXRGN];
  int nrgns;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  status = GetAllRegionNames(env, rgnList, &nrgns, MAXRGN);
  if (status != SUCCESS)
    return status;

  status = q_string(oparb, "PORGN", nrgns, rgnList, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending ORGN parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PNRGN", 1, &nrgns, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending NRGN parameter back to TAE", "VIDS-TAEERR");

  if (print)
  {
    NotifyUser(Inform, "", "Number of regions = %d", nrgns);
    if (nrgns != 0)
    {
      NotifyUser(Inform, "", "Region names are:");
      for (i=0; i<nrgns; i++)
        NotifyUser(Inform, "", "  %s", rgnList[i]);
    }
  }

  return SUCCESS;
}

/************************************************************************/
/* JGET-TRANSLATE translates coordinates from one coordinate system to	*/
/* another.  The three systems are: Raw (screen coordinates), IMP	*/
/* (image plane coordinates), and File (file coordinates).		*/
/************************************************************************/

int jget_translate_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i, status;
  TAEVariable *v;
  Boolean print;
  char *trans;
  int imp, imp2;
  int nimp;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  int input[2];			/* input coordinates (line,samp) */
  int output[2];		/* output coordinates (line,samp) */
  TAEVariable *GetVariable();

  oparb = &OutParblk;

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    print = True;

  v = GetVariable(env, "TRANS");
  if (v == NULL) return FAIL;
  trans = SVAL(*v, 0);

  v = GetVariable(env, "IN");
  if (v == NULL) return FAIL;
  input[0] = IVAL(*v, 0);		/* depend on count=2 in PDF */
  input[1] = IVAL(*v, 1);

  if (GetPlaneList(env, &imp, &nimp, False) != SUCCESS)  /* better be 1 */
    return FAIL;

  if (EQUAL(trans, "IMP2IMP"))
  {
    v = GetVariable(env, "PLANE2");
    if (v == NULL) return FAIL;
    if (v->v_count == 0)
      ABORT(FAIL, "You must specify PLANE2 with a translation of IMP2IMP",
		"VIDS-BADPLANE");
    if (NameToImp(env, SVAL(*v, 0), &imp2) != SUCCESS)
      return FAIL;

    if (imp2 <= 0)
      ABORT(FAIL, "Image plane numbers must be positive.", "VIDS-BADPLANE");
    if (imp2 > env->nimps)
      ABORT(FAIL, "Sorry, the image plane number is too large.", "VIDS-BADPLANE");
  }
  else
    imp2 = 0;

  if (EQUAL(trans, "RAW2IMP"))
    CursRaw2IMP(env, imp, input[1], input[0], &output[1], &output[0]);
  else if (EQUAL(trans, "IMP2RAW"))
    CursIMP2Raw(env, imp, input[1], input[0], &output[1], &output[0]);
  else if (EQUAL(trans, "IMP2FILE"))
    CursIMP2File(&env->planes[imp], input[1], input[0], &output[1], &output[0]);
  else if (EQUAL(trans, "FILE2IMP"))
    CursFile2IMP(env, imp, input[1], input[0], &output[1], &output[0]);
  else if (EQUAL(trans, "IMP2IMP"))
    CursIMP2IMP(env, imp, imp2, input[1], input[0], &output[1],&output[0]);
  else
    ABORT(FAIL, "Invalid value for TRANS keyword", "VIDS-BADTRANS");

  status = q_intg(oparb, "POUT", 2, output, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending OUT parameter back to TAE", "VIDS-TAEERR");

  if (print)
    NotifyUser(Inform, "", "Output line = %d, sample = %d",output[0],output[1]);

  return SUCCESS;
}

/************************************************************************/
/* JGET-STAT acquires and optionally prints statistics for a region on	*/
/* single image plane.  Stats are returned in TCL variables.		*/
/************************************************************************/

int jget_stat_do(env)

  VIDSEnvironment	*env;	/* The VIDS environment			*/
{
  int i,j,status;		/* temporary increment, status variables*/
  int plane;			/* plane to use (only 1 allowed)	*/
  Region *rgn;			/* region to collect stats over		*/
  Boolean print;		/* True iff results are printed		*/
  int minpix, maxpix, npix, median;
  double mean, stddev;
  struct PARBLK *oparb;		/* pointer to output parm block		*/
  long *hist;			/* Histogram of region			*/
  long *CurrentHist();
  char msg[80];

  oparb = &OutParblk;
  
  status = GetJstatParms(env, &plane, &rgn, &print);
  if (status != SUCCESS)
    return status;

  hist = CurrentHist(env, plane, rgn);
  if (hist == NULL)
    return FAIL;

  GatherStats(hist, &minpix, &maxpix, &npix, &median, &mean, &stddev);

  /* Send the values back to TAE */

  status = q_intg(oparb, "PMIN", 1, &minpix, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PMIN parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PMAX", 1, &maxpix, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PMAX parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PNPIX", 1, &npix, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PNPIX parameter back to TAE", "VIDS-TAEERR");
  status = q_intg(oparb, "PMEDIAN", 1, &median, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PMEDIAN parameter back to TAE", "VIDS-TAEERR");

  status = q_real(oparb, "PMEAN", 1, &mean, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PMEAN parameter back to TAE", "VIDS-TAEERR");
  status = q_real(oparb, "PSTDDEV", 1, &stddev, P_ADD);
  if (status != SUCCESS)
    ABORT(status, "Error sending PSTDDEV parameter back to TAE", "VIDS-TAEERR");

  /* Now print the values if requested */

  if (print)
  {
    NotifyUser(Inform,"", "Statistics for plane %d:", plane);
    NotifyUser(Inform,"", "  Minimum pixel = %d\t Maximum pixel = %d",
				minpix, maxpix);
    NotifyUser(Inform,"", "  Total number of pixels = %d", npix);
    sprintf(msg, "  Mean = %f\t Standard Deviation = %f\t Median = %d",
				mean, stddev, median);
    NotifyUser(Inform,"", msg);		/* Can't send doubles to NotifyUser */
  }

  return SUCCESS;
}

/************************************************************************/
/* GetJstatParms gathers all the parameters for JSTAT.
 */

int GetJstatParms(env, planeList, rgn, print)
  VIDSEnvironment	*env;	/* The VIDS environment			*/
  int		planeList[];	/* list of planes to use (only 1 allowed) */
  Region	**rgn;		/* region to collect stats over		*/
  Boolean	*print;		/* True iff results are printed		*/
{
  TAEVariable	*v;		/* temp VARIABLE pointer		*/
  int i,j,status;		/* temporary increment, status variables*/
  int planecnt, rgncnt;		/* number of planes and regions given	*/
  TAEVariable *GetVariable();

  if (GetPlaneList(env, planeList, &planecnt, False) != SUCCESS)
    return FAIL;

  if (GetRegionList(env, rgn, &rgncnt, "REGION", 1) != SUCCESS)
    return FAIL;
  if (rgncnt == 0)
    *rgn = NULL;			/* region is defaulted */

  v = GetVariable(env, "PRINT");
  if (v == NULL) return FAIL;
  *print = False;
  if (EQUAL(SVAL(*v, 0), "PRINT"))
    *print = True;

  return SUCCESS;
}

/************************************************************************/
/* GatherStats gathers lots of statistics for a given histogram.
 * Similar to GatherHistStats, but more statistics are gathered.
 */

GatherStats(hist, minpix, maxpix, npix, median, mean, stddev)
  long			*hist;		/* the histogram to use		*/
  int			*minpix;	/* minimum pixel value		*/
  int			*maxpix;	/* maximum pixel value		*/
  int			*npix;		/* number of pixels in region	*/
  int			*median;	/* median DN of the region	*/
  double		*mean;		/* returned mean of data	*/
  double		*stddev;	/* returned sigma of data	*/
{
  int i;
  double rsum0, rsum1, rsum2, r;
  double variance, med;
  double sqrt();

  *minpix = 255;
  *maxpix = 0;

  rsum0 = 0;
  rsum1 = 0;
  rsum2 = 0;

  for (i=0; i<LUTSIZE; i++)
  {
    if (hist[i] != 0)			/* check for min & max if non-0 */
    {
      *minpix = MIN(i, *minpix);
      *maxpix = MAX(i, *maxpix);
    }
    r = (double) hist[i];
    rsum0 += r;
    rsum1 += r * i;
    rsum2 += r * i * i;
  }

  *npix = (int) rsum0;

  if (rsum0 == 0.0)
  {
    *mean = 0.0;
    *stddev = 0.0;
  }
  else
  {
    *mean = rsum1 / rsum0;
    variance = (rsum2 / rsum0) - (*mean * *mean);
    if (variance < 0.0)
      variance = 0.0;
    *stddev = sqrt(variance);
  }

  med = rsum0 / 2.0;			/* calculate the median */
  rsum0 = 0.0;
  *median = 255;
  for (i=0; i<LUTSIZE; i++)
  {
    r = (double) hist[i];
    rsum0 += r;
    if (rsum0 >= med)
    {
      *median = i;
      break;
    }
  }
}
